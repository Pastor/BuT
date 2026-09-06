//! Контекст печати выражений цели `sv` (выделено ).
//!
//! Граница - **ответственность**: `sv_expr` печатает выражения, а `Scope` отвечает на
//! вопросы о среде печати (какие сигналы регистровые, в теле какой функции мы
//! находимся, какие есть перечисления и структуры) и приводит значение к типу
//! приёмника.

use super::sv_expr::print_expression;
use super::sv_names::sv_enum_variant_name;
use super::sv_type::sv_enum_type_name;
use crate::diagnostics::Diagnostic;
use crate::semantic::ExpressionNode;
use std::collections::{BTreeMap, BTreeSet};

/// Контекст печати выражения.
pub(crate) struct Scope<'a> {
    /// Сигналы, имеющие регистровую пару `<имя>_next`.
    ///
    /// Это переменные модели и выходные порты: в `always_comb` **чтение** идёт из
    /// регистра (`name`), а **запись** - в комбинационную пару (`name_next`), которую
    /// `always_ff` защёлкивает по фронту. Разделение и делает такт тактом. Ключи -
    /// **имена сигналов** (уже с преисправлением уровня), а не имена Takt.
    pub(crate) registered: &'a BTreeSet<String>,
    /// Имена двунаправленных портов.
    ///
    /// У такого порта две стороны: чтение идёт с входа `<имя>_i`, который ведёт плата,
    /// а запись - в `<имя>_o` (плюс строб `<имя>_we`, его печатает оператор). Обычный
    /// порт остаётся одним сигналом, поэтому список, а не правило по имени.
    pub(crate) inouts: &'a BTreeSet<String>,
    /// Имя объемлющей функции, если печатается её тело.
    ///
    /// Нужно для `return`: в SystemVerilog функция возвращает значение
    /// **присваиванием собственному имени** (`f = t;`). Ключевое слово `return`
    /// синтаксически существует, но **yosys его не принимает** - проба
    /// 2026-07-16: `return t;` -> `ERROR: syntax error, unexpected TOK_ID`, тогда
    /// как `verilator --lint-only -Wall` тот же модуль **принял чисто**.
    ///
    /// То есть это ещё один случай, где один Verilator пропустил бы несинтезируемую
    /// конструкцию, - ровно как с `real`. Форма выбрана не по вкусу, а по тому, что
    /// принимают **оба** инструмента. Печатать ли охранные формулы (`--guard-enable`).
    pub(crate) guard_enable: bool,
    pub(crate) function: Option<&'a str>,
    /// Тип возврата функции, чьё тело печатается.
    ///
    /// Возврат здесь - присваивание имени функции, то есть позиция приёмника с
    /// известным типом: разряд `x.N` даёт один бит, и без приведения verilator отвечает
    /// `WIDTHEXPAND`, а проверка цели считает предупреждение ошибкой.
    pub(crate) function_ret: Option<&'a crate::semantic::type_node::TypeNode>,
    /// Варианты перечислений модели: `имя перечисления -> [(вариант, значение)]`.
    ///
    /// Нужны для **восстановления варианта по значению**. `command := Up` в АСД
    /// выглядит как `Assign(Variable, Number(2))`: узла варианта перечисления
    /// `ExpressionNode` не имеет вовсе (та же ловушка описана для цели `rust` в
    /// `CLAUDE.md`). А перечисления SystemVerilog **строго типизированы** - проба
    /// 2026-07-16: `command_next = 2;` даёт `%Error-ENUMVALUE: Implicit conversion to
    /// enum`.
    ///
    /// Приведение (`command_e'(2)`) оба инструмента принимают, но читать его инженеру
    /// хуже, чем `COMMAND_UP`, - а RTL читают. Поэтому значение восстанавливается в имя
    /// варианта, и приведение остаётся запасным путём для значения, которому варианта
    /// нет. Имена, локальные для печатаемой функции: параметры и её `var`.
    ///
    /// Без этого списка локальная переменная, чьё имя совпало с переменной модели,
    /// печаталась сигналом модели: признак "локальная" строился как "имя не объявлено в
    /// модели", а при совпадении он ложен.
    pub(crate) locals: &'a BTreeSet<String>,
    pub(crate) enums: &'a BTreeMap<String, Vec<(String, i128)>>,
    /// Поля структур модели: `имя структуры -> [(поле, тип)]`.
    ///
    /// Нужны присваиванию агрегата: место записи у структуры - **имя поля**, а не
    /// индекс. Снимок, а не `ModelNode`, потому что печатник цели работает с картой
    /// уровней, а не с деревом.
    pub(crate) structs: &'a BTreeMap<String, Vec<(String, crate::semantic::type_node::TypeNode)>>,
    /// Приёмник предупреждений генератора.
    ///
    /// `print_expression` берётся по `&Scope`, но `SV-009` (переменный делитель)
    /// рождается именно здесь - в единственной точке трансляции всех выражений (тела,
    /// условия, функции). Интерьерная мутабельность позволяет дописать диагностику, не
    /// протаскивая `&mut` сквозь 32 места вызова печатника. Владелец ячейки -
    /// [`super::sv_fsm::Fsm`], доставку делает `generate_program`.
    pub(crate) warnings: &'a std::cell::RefCell<Vec<Diagnostic>>,
}

impl Scope<'_> {
    /// Имя сигнала для **чтения**: рабочая копия `_next`, если она есть.
    ///
    /// **Читается `_next`, а не регистр - это не оптимизация, а семантика.**
    /// Тело состояния Takt императивно: `v := 1; w := v;` обязано дать `w = 1`
    /// (так в симуляторе; в C - `write(V,1)` затем `read(V)` возвращает
    /// только что записанное). В `always_comb` рабочая копия - `v_next`: она
    /// инициализируется значением регистра умолчанием в начале блока, а затем
    /// накапливает записи такта. Чтение регистра `v` дало бы значение
    /// **предыдущего** такта, то есть `w = 0`, - молча иная модель.
    ///
    /// Ровно этот дефект был внесён и пойман при разработке (2026-07-16): вывод печатал
    /// `w_next = v;`.
    ///
    /// Единственное место, где читается сам регистр, - умолчания в начале `always_comb`
    /// (`v_next = v;`) и ветвь сброса; оба печатаются напрямую, минуя этот метод.
    pub(crate) fn read(&self, signal: &str) -> String {
        // Двунаправленный порт читается со стороны входа: `_next` у него нет - это вход
        // модуля, а не регистр.
        if self.inouts.contains(signal) {
            return crate::generator::sv::sv_module::inout_in(signal);
        }
        if self.registered.contains(signal) {
            format!("{}_next", signal)
        } else {
            signal.to_string()
        }
    }

    /// Печатает значение в позиции присваивания элементу типа `ty`.
    ///
    /// Обычные типы печатаются как есть; для перечисления число восстанавливается в имя
    /// варианта (см. поле [`enums`](Scope::enums)).
    pub(crate) fn coerce(
        &self,
        ty: &crate::semantic::type_node::TypeNode,
        value: &ExpressionNode,
    ) -> Result<String, Diagnostic> {
        let crate::semantic::type_node::TypeNode::Enum(enum_name) = ty else {
            // Широкий литерал печатается размерной формой по ширине приёмника:
            // нетипизированная десятичная константа в SV знаковая и не уже 32 бит,
            // поэтому значение больше `i32::MAX` даёт verilator `WIDTHEXPAND`, а проверка
            // цели считает предупреждение ошибкой.
            if let ExpressionNode::Number(n) = value
                && let Some(sized) = super::sv_type::sized_literal(*n, ty)
            {
                return Ok(sized);
            }
            // Разряд `x.N` в позиции значения шире одного бита: `sel` даёт 1 бит, и
            // verilator отвечает `WIDTHEXPAND` - а проверка цели считает предупреждение
            // ошибкой. Размерная форма `W'(...)` - та же, какой печатается широкий
            // литерал.
            if matches!(
                value,
                ExpressionNode::BitAccess(_, crate::parser::ast::Member::Number(_))
            ) && let Some(width) = super::sv_type::scalar_width(ty)
                && width > 1
            {
                return Ok(format!("{width}'({})", print_expression(value, self)?));
            }
            // Арифметика печатается В ширине приёмника: `r := a + b;` при `a, b: u8` и
            // `r: u16` давало verilator `WIDTHEXPAND` - а проверка цели считает
            // предупреждение ошибкой. Эталон и цель `c` вход считают верно.
            //
            // Расширяются операнды, а не результат: сложение в восьми битах обернулось
            // бы **до** расширения, и 300 стало бы 44.
            if let Some(text) = super::sv_arith::in_target(value, ty, self)? {
                return Ok(text);
            }
            // Именованное значение иной ширины приводится к приёмнику: иначе verilator
            // отвечает `WIDTHEXPAND`.
            if let Some(text) = super::sv_arith::value_in_target(value, ty, self)? {
                return Ok(text);
            }
            return print_expression(value, self);
        };
        let printed = print_expression(value, self)?;
        let ExpressionNode::Number(n) = value else {
            // Значение уже имеет тип перечисления (переменная, вариант) - приводить
            // нечего.
            return Ok(printed);
        };
        if let Some(variants) = self.enums.get(enum_name)
            && let Some((variant, _)) = variants.iter().find(|(_, v)| v == n)
        {
            return Ok(sv_enum_variant_name(enum_name, variant));
        }
        // Варианта с таким значением нет. Приведение - единственный способ напечатать
        // это валидно; молча оставить число нельзя (ENUMVALUE).
        Ok(format!("{}'({})", sv_enum_type_name(enum_name), printed))
    }

    /// Имя сигнала для **записи**: комбинационная пара, если она есть.
    ///
    /// У локальной переменной функции и у константы пары нет: первая живёт внутри
    /// одного вычисления, вторая вообще не регистр.
    pub(crate) fn write(&self, signal: &str) -> String {
        // Запись двунаправленного порта идёт в сторону выхода; строб `_we` печатает
        // оператор присваивания.
        if self.inouts.contains(signal) {
            let out = crate::generator::sv::sv_module::inout_out(signal);
            return format!("{}_next", out);
        }
        if self.registered.contains(signal) {
            format!("{}_next", signal)
        } else {
            signal.to_string()
        }
    }
}

/// Пустой набор имён: контекст вне тела функции и модуль без двунаправленных портов.
///
/// Статический, а не временный: `&Default::default()` в литерале `Scope` живёт до конца
/// выражения, и компилятор такую ссылку не выпускает наружу.
pub(crate) fn no_locals() -> &'static BTreeSet<String> {
    static EMPTY: std::sync::OnceLock<BTreeSet<String>> = std::sync::OnceLock::new();
    EMPTY.get_or_init(BTreeSet::new)
}

/// Пустой набор двунаправленных портов - тот же статик под своим именем.
///
/// Только для тестов: в рабочем пути список приходит из `Fsm`, и своего "пустого"
/// вызова там нет - под `-D warnings` он был бы мёртвым кодом.
#[cfg(test)]
pub(crate) fn no_inouts() -> &'static BTreeSet<String> {
    no_locals()
}

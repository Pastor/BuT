//! Отображение типов Takt в типы SystemVerilog.
//!
//! ## `Result`, а не `Option` - ключевое отличие от `get_c_type`
//!
//! На непокрытом узле - **ошибка**, а не молчание. Прямое следствие уроков
//! 0025/0028/0029: в цели `c` неотображаемый тип возвращает `None`, и отличить
//! "отфильтровали как неиспользуемое" от "не смогли перевести" снаружи невозможно.
//!
//! ## Ширина перечисления считается по диапазону значений, а не по числу вариантов
//!
//! (таблица T11) предписывал `W = ⌈log₂(вариантов)⌉, минимум 1`. **Проба 2026-07-16 это
//! опровергла на реальном примере корпуса:** `enum Action { Idle = 670, ... }`
//! (`elevator.takt:121`) - два варианта, то есть по формуле `logic [0:0]`, - а значение
//! 670 требует десяти бит:
//!
//! ```text
//! %Error-ENUMITEMWIDTH: Enum value exceeds width of enum type (IEEE 1800-2023 6.19)
//!     typedef enum logic [0:0] { IDLE = 670, UP = 671 } action_e;
//! ```
//!
//! Это **тот же капкан**, в который уже попали два соседних генератора: ST (`CLAUDE.md`:
//! "Разрядность перечисления считается по диапазону вариантов, а не берётся `USINT`,
//! как предполагал ") и Rust (`#[repr(u8)]` не принял `Idle = 670`). Здесь он учтён
//! **до** написания кода.
//!
//! Формула [`enum_width`] - по диапазону - покрывает **оба** случая одной ветвью: у
//! перечисления состояний, где значения назначает сам генератор (`0..n-1`), она даёт
//! ровно `⌈log₂(n)⌉`, то есть совпадает с формулой; у пользовательского перечисления с
//! явными значениями - верную ширину. Отдельного правила для состояний заводить не
//! требуется.

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::enum_facts;
use crate::semantic::naming::normalize_lowercase_snakecase;
use crate::semantic::type_node::TypeNode;

/// Строит диагностику `SV-002` о неотображаемом типе.
///
/// Шаблон **общий** (`sv_expr::sv002`): своя копия давала бы одному коду два разных
/// вида сообщения - /0193/0195.
fn sv002_type(what: &str, ty: &TypeNode) -> Diagnostic {
    crate::generator::sv::sv_expr::sv002(&format!(
        "{what}: тип '{ty}' — внутренний либо неразрешённый, представления в \
         порождаемом RTL он не имеет"
    ))
}

/// Строит диагностику `SV-003` - вещественного типа в синтезируемом RTL нет.
fn sv003(what: &str) -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        format!(
            "{}: вещественный тип (float) не существует в синтезируемом RTL и \
             целью 'sv' не поддерживается. Тип 'real' языка SystemVerilog \
             пригоден только для симуляции — синтезатор его отвергает. \
             Используйте целочисленный тип либо цель 'c'/'rust', где float \
             отображается",
            what
        ),
    )
    .with_code("SV-003")
}

/// Строит диагностику `SV-004` - форма типа недопустима.
fn sv004(what: &str, why: &str) -> Diagnostic {
    // Позицию даёт носитель: оператор, объявление либо ничего.
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        format!("{}: {}", what, why),
    )
    .with_code("SV-004")
}

/// Объявление типа в SystemVerilog: часть до имени и распакованная размерность после
/// него.
///
/// Одной строкой тип не выражается: синтаксис объявления SV **разрывен вокруг имени** -
/// `logic [7:0] data [0:3];`, где `logic [7:0]` - упакованная часть (ширина элемента),
/// а `[0:3]` - распакованная (число элементов). Склеить их в одну строку значило бы
/// получить `logic [7:0] [0:3] data`, то есть **двумерный упакованный вектор** - другой
/// тип с другой семантикой.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SvType {
    /// Часть до имени: `logic`, `logic [7:0]`, `logic signed [15:0]`, `action_e`.
    pub(crate) prefix: String,
    /// Распакованная размерность после имени: пусто либо `[0:3]`.
    pub(crate) suffix: String,
}

impl SvType {
    /// Печатает объявление `<prefix> <name> <suffix>` - без завершающей `;`.
    ///
    /// Пробел перед распакованной размерностью - форма (`<T> name [0:N-1]`) и
    /// общепринятый стиль SV: он отделяет её от имени, тогда как упакованная часть
    /// прижата к типу. Читателю видно, какая размерность к чему относится.
    pub(crate) fn declare(&self, name: &str) -> String {
        if self.suffix.is_empty() {
            format!("{} {}", self.prefix, name)
        } else {
            format!("{} {} {}", self.prefix, name, self.suffix)
        }
    }

    /// Скалярный тип без распакованной размерности.
    fn scalar(prefix: impl Into<String>) -> Self {
        Self {
            prefix: prefix.into(),
            suffix: String::new(),
        }
    }
}

/// Имя типа перечисления в SV: `action` -> `action_e`.
pub(crate) fn sv_enum_type_name(raw: &str) -> String {
    format!("{}_e", normalize_lowercase_snakecase(raw.to_string()))
}

/// Имя типа структуры в SV: `point` -> `point_t`.
pub(crate) fn sv_struct_type_name(raw: &str) -> String {
    format!("{}_t", normalize_lowercase_snakecase(raw.to_string()))
}

/// Печатает `typedef struct packed` для каждой структуры дерева.
///
/// **Зачем.** `sv_type` отображает `TypeNode::Struct` в имя `<имя>_t`, но самого
/// объявления цель не эмитила - вывод ссылался на несуществующий тип, и
/// `verilator` отвечал `Can't find typedef/interface: 'gains_t'` при **нулевом**
/// коде возврата `taktc`.
///
/// **`packed` обязателен, а не украшение:** непакованная структура в SV не
/// синтезируется (yosys её не примет), не сравнивается как целое и не годится на роль
/// регистра - а именно регистром становится переменная модели.
///
/// Поле печатается тем же `sv_type`, что и переменная: разъехавшись, они дали бы полю и
/// переменной одного типа разную ширину.
///
/// Порядок полей - **объявленный** (`Vec`, не карта): в упакованной структуре он
/// определяет разряды, и перестановка молча изменила бы значение.
pub(crate) fn emit_structs(
    p: &mut crate::generator::indent::Printer,
    blocks: &[crate::generator::sv::sv_fsm::Block],
) -> Result<(), Diagnostic> {
    let mut seen: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for (_, model_rc) in blocks {
        let model = model_rc.borrow();
        // Порядок - по зависимостям: verilator отвечает "Reference to 'point_t' before
        // declaration", если вмещающая структура объявлена раньше вложенной.
        for def in crate::generator::struct_order::sorted(&model.structs) {
            if !seen.insert(def.name.clone()) {
                continue;
            }
            let def = &def;
            // Комментарий автора перед объявлением типа.
            for line in crate::generator::comments::leading(
                def.loc,
                crate::generator::header::CommentStyle::Slashes,
            ) {
                p.ident(&line).nl();
            }
            p.ident("typedef struct packed {").nl();
            p.up();
            for (field, ty) in &def.fields {
                let name = crate::semantic::naming::normalize_lowercase_snakecase(field.clone());
                // Поле-Массив печатается упакованным: внутри `struct packed`
                // распакованная размерность запрещена стандартом (IEEE 1800-2023
                // 7.2.1), и `verilator` отвечает "Unpacked data type ... in packed
                // struct/union" при нулевом коде возврата `taktc`. Обычная
                // переменная-массив остаётся распакованной - там форма выбрана прогоном
                // обоих инструментов, и менять её незачем.
                if let Some(decl) = packed_array_field(ty, &name, &def.name, field)? {
                    p.ident(&decl).nl();
                    continue;
                }
                let sv = sv_type(ty, &format!("поле '{}' структуры '{}'", field, def.name))?;
                let decl = format!("{} {}{};", sv.prefix.trim(), name, sv.suffix);
                p.ident(decl.trim_start()).nl();
            }
            p.down();
            p.ident(&format!("}} {};", sv_struct_type_name(&def.name)))
                .nl()
                .nl();
        }
    }
    Ok(())
}

/// Объявление поля-массива в `struct packed` - упакованной формой.
///
/// `None` - поле массивом не является (или это упакованный бит-вектор,: он печатается
/// скаляром).
fn packed_array_field(
    ty: &TypeNode,
    name: &str,
    struct_name: &str,
    field: &str,
) -> Result<Option<String>, Diagnostic> {
    let TypeNode::Array(size, elem) = ty else {
        return Ok(None);
    };
    if crate::semantic::bit_vector::is_bit_vector(ty).is_some() {
        return Ok(None);
    }
    let inner = sv_type(
        elem,
        &format!("элемент поля '{field}' структуры '{struct_name}'"),
    )?;
    // Размерность массива идёт перед размерностью элемента: `logic [N-1:0][W-1:0]` даёт
    // `data[i]` шириной элемента, а обратный порядок (`logic [W-1:0][N-1:0]`) - шириной
    // **числа элементов**; `verilator` тогда отвечает `WIDTHEXPAND`, что проверка цели
    // считает ошибкой.
    let prefix = inner.prefix.trim();
    let dim = format!("[{}:0]", i64::from(*size) - 1);
    let decl = match prefix.find('[') {
        Some(at) => format!("{}{}{}", &prefix[..at], dim, &prefix[at..]),
        // У элемента-структуры разрядов в префиксе нет - размерность приписывается к
        // имени типа.
        None => format!("{prefix} {dim}"),
    };
    Ok(Some(format!("{decl} {name};")))
}

/// Отображает тип Takt в объявление SystemVerilog.
///
/// `what` - что именно объявляется (`переменная 'x'`, `порт 'p'`), чтобы диагностика
/// указывала на место, а не на абстрактный тип.
///
/// # Ошибки
///
/// - [`SV-002`](sv002) - узел не покрыт (внутренний либо неразрешённый тип);
/// - [`SV-003`](sv003) - `Rational`: в синтезируемом RTL FP не существует;
/// - [`SV-004`](sv004) - форма типа недопустима (`Array(0, _)`, `Integer{0}`).
pub(crate) fn sv_type(ty: &TypeNode, what: &str) -> Result<SvType, Diagnostic> {
    match ty {
        // Идеальное соответствие: один провод / один триггер. В цели `c` - `int`, то
        // есть 32 бита на бит (дефект 0029, Д2). Тип `duration`: беззнаковый вектор в
        // **миллисекундах** - та же единица, что у остальных целей, поэтому потактовая
        // сверка сравнивает одинаковые числа. Ширина - `duration::VALUE_BITS`.
        TypeNode::Duration => Ok(SvType::scalar(format!(
            "logic [{}:0]",
            crate::semantic::duration::VALUE_BITS - 1
        ))),
        TypeNode::Bit | TypeNode::Bool => Ok(SvType::scalar("logic")),
        // Отдельного `real` нет и быть не может: см. шапку модуля.
        TypeNode::Rational => Err(sv003(what)),
        // Fixed-point q(m, n): знаковое целое ширины W = m + n. В отличие от `float`,
        // синтезируется - ради этого фича и делалась. Арифметика (масштабирование при
        // `*`/`/`) -.
        TypeNode::Fixed { m, n, .. } => {
            Ok(SvType::scalar(format!("logic signed [{}:0]", (m + n) - 1)))
        }
        TypeNode::Integer { bits, signed } => {
            if *bits == 0 {
                return Err(sv004(
                    what,
                    "нулевая разрядность целого: `logic [-1:0]` не является \
                     допустимым диапазоном",
                ));
            }
            // Разрядность буквальна и произвольна: в RTL машинного слова нет, `logic
            // [11:0]` так же нормален, как `logic [7:0]`. Округления до 8/16/32/64, как
            // в C, здесь не требуется.
            //
            // `signed` обязателен, а не косметика: он меняет семантику сравнений и
            // арифметического сдвига вправо.
            let sign = if *signed { "signed " } else { "" };
            Ok(SvType::scalar(format!("logic {}[{}:0]", sign, bits - 1)))
        }
        // Бит-вектор `[bit;N]`: SV умеет вектор произвольной ширины нативно, поэтому
        // упаковка - просто `logic [N-1:0]` при любом N (массив слов, как в C/rust/st,
        // ему не нужен). Так `[bit;8]` == `u8` (тоже `logic [7:0]`).
        TypeNode::Array(n, elem) if crate::semantic::bit_vector::is_bit_vector(ty).is_some() => {
            let _ = elem;
            if *n == 0 {
                return Err(sv004(
                    what,
                    "бит-вектор нулевой ширины: `logic [-1:0]` не является \
                     допустимым диапазоном",
                ));
            }
            Ok(SvType::scalar(format!("logic [{}:0]", n - 1)))
        }
        // Настоящий (распакованный) массив скаляров.
        TypeNode::Array(n, elem) => {
            if *n == 0 {
                return Err(sv004(
                    what,
                    "массив нулевого размера: `[0:-1]` не является допустимым \
                     диапазоном",
                ));
            }
            let inner = sv_type(elem, what)?;
            // Размерности накапливаются слева направо: `[u8; 2]` элементов `[u8; 4]`
            // даёт `logic [7:0] a [0:1][0:3]` - внешняя размерность первая, как и в
            // исходнике.
            Ok(SvType {
                prefix: inner.prefix,
                suffix: format!("[0:{}]{}", n - 1, inner.suffix),
            })
        }
        // Имена вариантов видны в осциллограмме - для отладки RTL это не косметика:
        // иначе на волнах числа вместо имён состояний.
        TypeNode::Enum(name) => Ok(SvType::scalar(sv_enum_type_name(name))),
        TypeNode::Struct(name) => Ok(SvType::scalar(sv_struct_type_name(name))),
        // Пустой тип осмыслен ровно в одной позиции - `function automatic void`.
        TypeNode::Unit => Ok(SvType::scalar("void")),
        // Ниже - то, что представления не имеет. Ветки `_` нет намеренно: `TypeNode`
        // помечен `#[non_exhaustive]`, но внутри крейта-объявителя атрибут не
        // действует, поэтому исчерпывающий разбор здесь возможен - и обязателен.
        // Добавление варианта в `TypeNode` обязано валить сборку, а не тихо
        // проваливаться в чужую ветку.
        //
        // План задачи требовал ветку `_ =>`, возвращающую `SV-002`, полагая
        // `#[non_exhaustive]` действующим здесь. Это неверно: атрибут не действует
        // внутри объявляющего крейта, и ветка `_` была бы не тестом, а его
        // отключением - она проглотила бы новый вариант молча.
        TypeNode::Address(_, _)
        | TypeNode::Inference
        | TypeNode::Unsupported
        | TypeNode::BuiltinString
        | TypeNode::BuiltinModel
        | TypeNode::BuiltinState
        | TypeNode::BuiltinNumeric => Err(sv002_type(what, ty)),
    }
}

/// Считает ширину и знаковость перечисления **по диапазону его значений**.
///
/// Возвращает `(ширина в битах, знаковое ли)`.
///
/// Формула (`⌈log₂(вариантов)⌉`) **неверна** и опровергнута пробой: см. шапку модуля.
/// Здесь ширина считается по значениям, что покрывает и перечисление состояний
/// (значения `0..n-1` назначает генератор -> формула вырождается в `⌈log₂(n)⌉`), и
/// пользовательское с явными значениями.
///
/// # Ошибки
/// [`SV-004`](sv004), если вариантов нет: ширина перечисления не определена.
pub(crate) fn enum_width(
    variants: &[(String, i128)],
    what: &str,
) -> Result<(u32, bool), Diagnostic> {
    // Аппаратная ширина точна, поэтому `sv` берёт `min_bits` факта напрямую (без
    // округления до машинной, в отличие от `c`/`st`/`rust`). Тонкости, которые раньше
    // жили здесь, теперь в факте: знаковое - минимум 2 бита (однобитного знакового не
    // бывает), `max == 0` -> 1 (ширины 0 не бывает). Пустое перечисление -> `SV-004`
    // (сегодняшнее поведение цели).
    match enum_facts(variants) {
        Some(f) => Ok((f.min_bits, f.signed)),
        None => Err(sv004(
            what,
            "перечисление без вариантов: ширина типа не определена",
        )),
    }
}

/// Литерал `value` в размерной форме `<W>'d<value>`, если она нужна.
///
/// `None` - печатать как есть. Так происходит для всего, что укладывается в 32-битное
/// **знаковое** целое (то есть для подавляющего большинства литералов корпуса: их вывод
/// обязан остаться байт-в-байт прежним), а также для отрицательных значений любой
/// величины - проба показала, что их verilator принимает без предупреждений, и знаковое
/// расширение работает верно.
///
/// `None` и тогда, когда у приёмника нет одной скалярной ширины: угадывать её нельзя -
/// форма `32'd...` в 64-битном регистре даёт то же `WIDTHEXPAND`.
pub(crate) fn sized_literal(value: i128, ty: &TypeNode) -> Option<String> {
    if value <= i128::from(i32::MAX) {
        return None;
    }
    let width = scalar_width(ty)?;
    Some(format!("{width}'d{value}"))
}

/// Ширина **скалярного** типа в битах, если она у него есть.
///
/// Нужна печати литерала: нетипизированная десятичная константа в SystemVerilog имеет
/// ширину не меньше 32 бит и **знаковая**, поэтому значение больше `i32::MAX` verilator
/// встречает предупреждением `WIDTHEXPAND` - а проверка цели идёт под `-Wall` и считает его
/// ошибкой. Лечится размерной формой `<W>'d<v>`, где `W` - ширина **приёмника**:
/// собственной ширины литерала недостаточно (проба: `32'd4294967295` в 64-битный
/// регистр - тот же `WIDTHEXPAND`).
///
/// `None` - у типа нет одной скалярной ширины (массив, структура, `real`): печатать
/// размерную форму не по чему, и литерал остаётся как есть.
pub(crate) fn scalar_width(ty: &TypeNode) -> Option<u32> {
    match ty {
        TypeNode::Bit | TypeNode::Bool => Some(1),
        TypeNode::Integer { bits, .. } => Some(u32::from(*bits)),
        TypeNode::Fixed { m, n, .. } => Some(u32::from(*m) + u32::from(*n)),
        TypeNode::Duration => Some(u32::from(crate::semantic::duration::VALUE_BITS)),
        // Бит-вектор `[bit;N]` - упакованный вектор ширины N; настоящий массив скаляров
        // одной ширины не имеет.
        TypeNode::Array(n, _) if crate::semantic::bit_vector::is_bit_vector(ty).is_some() => {
            Some(u32::from(*n))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn u(bits: u8) -> TypeNode {
        TypeNode::Integer {
            bits,
            signed: false,
        }
    }

    /// **T1/A7:** `bit` -> `logic`, а **не** `int`.
    ///
    /// Тест против дефекта 0029 (Д2): в C один провод занимает 32 бита.
    #[test]
    fn bit_maps_to_logic_not_int() {
        let ty = sv_type(&TypeNode::Bit, "тест").unwrap();
        assert_eq!(ty.prefix, "logic");
        assert_eq!(ty.suffix, "");
        assert_ne!(
            ty.prefix, "int",
            "повторён дефект 0029: 32 бита на один провод"
        );
    }

    /// **T2:** `bool` -> `logic`. Для RTL `bit` и `bool` - один провод.
    #[test]
    fn bool_maps_to_logic() {
        assert_eq!(sv_type(&TypeNode::Bool, "тест").unwrap().prefix, "logic");
    }

    /// **T3-T6:** разрядность целого буквальна.
    #[test]
    fn unsigned_integers_map_literally() {
        for (bits, expected) in [
            (8u8, "logic [7:0]"),
            (16, "logic [15:0]"),
            (32, "logic [31:0]"),
            (64, "logic [63:0]"),
        ] {
            assert_eq!(sv_type(&u(bits), "тест").unwrap().prefix, expected);
        }
    }

    /// **T7:** знаковое целое получает `signed` - это не косметика.
    ///
    /// `signed` меняет семантику сравнений и арифметического сдвига вправо; без него
    /// `-1 < 0` на `logic [7:0]` было бы ложным.
    #[test]
    fn signed_integers_carry_signed_keyword() {
        let ty = sv_type(
            &TypeNode::Integer {
                bits: 16,
                signed: true,
            },
            "тест",
        )
        .unwrap();
        assert_eq!(ty.prefix, "logic signed [15:0]");
    }

    /// **T8/T15:** разрядность, не кратная машинному слову, отображается даром.
    ///
    /// Проверка T15 тест-плана (`var x: u12;` -> `logic [11:0] x;`) в виде исходника
    /// `.takt` **невыполнима**: проба 2026-07-16 показала, что `u12` языком не
    /// принимается вовсе (`SE-034: Локальный тип 'u12' не найден`) - конструктор типов
    /// строит только 8/16/32/64. Утверждение "произвольная разрядность бесплатна" верно
    /// применительно к **отображению**, что этот тест и проверяет напрямую на
    /// `TypeNode`; появится ли `u12` в языке - вопрос к языку, а не к цели.
    #[test]
    fn arbitrary_width_integer_maps_without_rounding() {
        assert_eq!(sv_type(&u(12), "тест").unwrap().prefix, "logic [11:0]");
        assert_eq!(sv_type(&u(1), "тест").unwrap().prefix, "logic [0:0]");
        assert_eq!(sv_type(&u(3), "тест").unwrap().prefix, "logic [2:0]");
    }

    /// **T14/A6:** `[u8; 4]` -> `logic [7:0] data [0:3]`, а **не** `uint4_t`.
    ///
    /// Тест против дефекта 0029 (Д1): в C `Array(size, elem)` даёт `uint{size}_t`,
    /// где `size` - число элементов, то есть несуществующий тип.
    #[test]
    fn array_maps_to_unpacked_array_not_uint4_t() {
        let ty = sv_type(&TypeNode::Array(4, Box::new(u(8))), "тест").unwrap();
        assert_eq!(ty.prefix, "logic [7:0]");
        assert_eq!(ty.suffix, "[0:3]");
        assert_eq!(ty.declare("data"), "logic [7:0] data [0:3]");
        assert!(
            !ty.declare("data").contains("uint4_t"),
            "повторён дефект 0029"
        );
    }

    /// Вложенный массив даёт многомерную распакованную форму.
    ///
    /// В ST это невозможно (`ARRAY OF ARRAY` отвергается MatIEC - нужна многомерная
    /// форма `ARRAY [0..2, 0..1] OF T`); в SV - бесплатно. Размерности идут снаружи
    /// внутрь, как в исходнике.
    #[test]
    fn nested_array_maps_to_multidimensional_unpacked() {
        let inner = TypeNode::Array(4, Box::new(u(8)));
        let outer = TypeNode::Array(2, Box::new(inner));
        let ty = sv_type(&outer, "тест").unwrap();
        assert_eq!(ty.declare("a"), "logic [7:0] a [0:1][0:3]");
    }

    /// Бит-вектор `[bit;N]` - нативный упакованный вектор `logic [N-1:0]` любой ширины
    /// (SV умеет вектор произвольной ширины); так `[bit;8]` == `u8`.
    #[test]
    fn array_of_bit_maps_to_packed_logic_vector() {
        let ty = sv_type(&TypeNode::Array(8, Box::new(TypeNode::Bit)), "тест").unwrap();
        assert_eq!(ty.declare("flags"), "logic [7:0] flags");
        // Произвольная ширина, в т.ч. > 64, - нативно, без массива слов.
        let ty100 = sv_type(&TypeNode::Array(100, Box::new(TypeNode::Bit)), "тест").unwrap();
        assert_eq!(ty100.declare("wide"), "logic [99:0] wide");
    }

    /// **T11:** перечисление отображается в именованный тип с суффиксом `_e`.
    #[test]
    fn enum_maps_to_named_type() {
        let ty = sv_type(&TypeNode::Enum("Action".to_string()), "тест").unwrap();
        assert_eq!(ty.prefix, "action_e");
    }

    /// **T12:** структура отображается в именованный тип с суффиксом `_t`.
    #[test]
    fn struct_maps_to_named_type() {
        let ty = sv_type(&TypeNode::Struct("Point".to_string()), "тест").unwrap();
        assert_eq!(ty.prefix, "point_t");
    }

    /// **T13:** пустой тип - `void` (осмыслен только как тип возврата функции).
    #[test]
    fn unit_maps_to_void() {
        assert_eq!(sv_type(&TypeNode::Unit, "тест").unwrap().prefix, "void");
    }

    /// **T9/A5: контрпример.** `float` -> `SV-003`, а не `real`.
    ///
    /// Молчаливая подстановка `real` дала бы модуль, который **симулируется, но не
    /// синтезируется**, - и `verilator --lint-only` пропустил бы это молча. То
    /// есть цена ошибки здесь - необнаруживаемый линтером дефект.
    #[test]
    fn rational_is_sv003_not_real() {
        let err = sv_type(&TypeNode::Rational, "переменная 'x'").unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SV-003"));
        assert!(
            err.message.contains("переменная 'x'"),
            "диагностика должна указывать на место: {}",
            err.message
        );
    }

    /// **T16/A5: контрпример.** Непокрытый узел даёт `SV-002`, а не молчание.
    ///
    /// В цели `c` `get_c_type` возвращает здесь `None`, и "не смогли перевести" снаружи
    /// неотличимо от "отфильтровали как неиспользуемое" (наследие ).
    #[test]
    fn uncovered_nodes_are_sv002() {
        for ty in [
            TypeNode::Inference,
            TypeNode::Unsupported,
            TypeNode::Address(0x10, None),
            TypeNode::BuiltinString,
            TypeNode::BuiltinModel,
            TypeNode::BuiltinState,
            TypeNode::BuiltinNumeric,
        ] {
            let err = sv_type(&ty, "переменная 'x'").unwrap_err();
            assert_eq!(err.code.as_deref(), Some("SV-002"), "тип {:?}", ty);
        }
    }

    /// **A5: контрпример.** Массив нулевого размера -> `SV-004`.
    #[test]
    fn zero_sized_array_is_sv004() {
        let err = sv_type(&TypeNode::Array(0, Box::new(u(8))), "переменная 'x'").unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SV-004"));
    }

    /// **A5: контрпример.** Целое нулевой разрядности -> `SV-004`.
    #[test]
    fn zero_width_integer_is_sv004() {
        let err = sv_type(&u(0), "переменная 'x'").unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SV-004"));
    }

    /// Ошибка типа элемента всплывает наружу из массива, а не теряется.
    #[test]
    fn array_of_float_propagates_sv003() {
        let err = sv_type(
            &TypeNode::Array(4, Box::new(TypeNode::Rational)),
            "переменная 'x'",
        )
        .unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SV-003"));
    }

    /// **Ключевой тест.** Ширина перечисления - по диапазону значений.
    ///
    /// Реальный случай корпуса - `elevator.takt:121` (`Idle = 670`). По формуле
    /// (`⌈log₂(вариантов)⌉`) два варианта дали бы `logic [0:0]`, и проба 2026-07-16
    /// показала, что Verilator отвергает это с `%Error-ENUMITEMWIDTH: Enum value
    /// exceeds width of enum type`. Тот же капкан ранее поймал генераторы ST и Rust.
    #[test]
    fn enum_width_670_needs_ten_bits_not_one() {
        let variants = vec![("Idle".to_string(), 670), ("Up".to_string(), 671)];
        let (width, signed) = enum_width(&variants, "тест").unwrap();
        assert_eq!(
            width, 10,
            "ширина обязана считаться по диапазону значений, а не по числу вариантов"
        );
        assert!(!signed);
    }

    /// Для перечисления состояний формула совпадает с формулой.
    ///
    /// Значения `0..n-1` назначает сам генератор, поэтому "по диапазону" здесь
    /// вырождается в `⌈log₂(n)⌉` - отдельной ветви для состояний не требуется.
    #[test]
    fn enum_width_of_sequential_states_matches_log2() {
        let seq = |n: i128| -> u32 {
            let variants: Vec<(String, i128)> = (0..n).map(|i| (format!("S{}", i), i)).collect();
            enum_width(&variants, "тест").unwrap().0
        };
        assert_eq!(seq(1), 1, "одно состояние — ширины 0 в SV не бывает");
        assert_eq!(seq(2), 1);
        assert_eq!(seq(3), 2);
        assert_eq!(seq(4), 2);
        assert_eq!(seq(5), 3);
        assert_eq!(seq(8), 3);
        assert_eq!(seq(9), 4);
    }

    /// Границы беззнаковой ширины.
    #[test]
    fn enum_width_unsigned_boundaries() {
        let at = |v: i128| enum_width(&[("V".to_string(), v)], "тест").unwrap();
        assert_eq!(at(0), (1, false));
        assert_eq!(at(1), (1, false));
        assert_eq!(at(2), (2, false));
        assert_eq!(at(255), (8, false));
        assert_eq!(at(256), (9, false));
    }

    /// Отрицательный вариант даёт знаковое перечисление нужной ширины.
    #[test]
    fn enum_width_negative_is_signed() {
        assert_eq!(
            enum_width(&[("V".to_string(), -1)], "тест").unwrap(),
            (2, true)
        );
        assert_eq!(
            enum_width(&[("A".to_string(), -128), ("B".to_string(), 127)], "тест").unwrap(),
            (8, true)
        );
        assert_eq!(
            enum_width(&[("A".to_string(), -129)], "тест").unwrap(),
            (9, true)
        );
    }

    /// **Контрпример.** Перечисление без вариантов -> `SV-004`.
    #[test]
    fn empty_enum_is_sv004() {
        let err = enum_width(&[], "перечисление 'E'").unwrap_err();
        assert_eq!(err.code.as_deref(), Some("SV-004"));
    }
}

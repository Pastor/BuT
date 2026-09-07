//! Трансляция выражений и условий Takt в Rust.
//!
//! ## Чем Rust проще ST
//!
//! У цели `st` числа и биты - непересекающиеся миры (`n & m` требует обёрток
//! `BYTE_TO_USINT`, `CLAUDE.md`); в Rust побитовые операции на целых нативны.
//!
//! ## Ветки `_` здесь нет
//!
//! Каждый непереводимый вариант назван явно и возвращает `Err`: ровно `_ => None`
//! позволил вычислителям симулятора молча разойтись. Новый вариант `ExpressionNode`
//! обязан **валить сборку**, а не тихо проходить мимо.

// Приведение к типу приёмника живёт своим модулем; имя остаётся доступным отсюда -
// потребителей у него семь.
pub(crate) use crate::generator::rust::rust_coerce::{coerce_to, enum_variant_literal};
pub(crate) use crate::generator::rust::rust_text::unwrap_outer;

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::rust::rust_fixed::{self, FixedOp};
use crate::generator::rust::rust_name::{rust_type_name, rust_value_name};
use crate::generator::rust::rust_needs::function_needs;
use crate::generator::rust::rust_port::port_class;
use crate::generator::rust::rust_shift::Direction;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{
    ExpressionNode, FunctionDefinitionNode, ModelNode, PortDirection, VariableNode,
};

// Печатник условий вынесен в `rust_cond`. Реэкспорт держит путь
// `rust_expr::condition_as_bool` для потребителей - импорт в `rust_model.rs` не
// меняется.
pub(crate) use crate::generator::rust::rust_bit::{bit_mask, member_index};
pub(crate) use crate::generator::rust::rust_cond::{condition_as_bool, print_as_bool};

/// Строит диагностику `RS-011` - конструкция не транслируется в Rust.
///
/// Координата - у оператора, который печатается сейчас: своей позиции у выражения нет,
/// а `Location::Codegen` печатается вовсе без префикса, и автору пришлось бы искать
/// место самому.
pub(crate) fn unsupported(what: &str) -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        format!("Не транслируется в Rust: {}", what),
    )
    .with_code("RS-011")
}

/// Контекст трансляции: как печатать обращение к имени.
///
/// Обращение к переменной зависит от того, **где** мы печатаем:
///
/// Общая переменная корня печатается `self.shared.busy` у владельца-корня и
/// `shared.busy` у под-модели (параметр `&mut Shared`); своя - всегда `self.x`; в теле
/// свободной `fn` - голое имя. Это аналог `VAR_IN_OUT` цели `st`: в C под-модель берёт
/// указатель `main`, а в Rust `self.cabin.tick(&mut self)` заимствовался бы дважды,
/// поэтому общие переменные свёрнуты в `Shared` и идут одним параметром `&mut Shared`.
#[derive(Clone)]
pub(crate) struct Scope<'a> {
    /// Модель, в контексте которой печатается выражение.
    ///
    /// Нужна не для имён, а для **обратного** поиска: `command := Up` приходит сюда как
    /// `Assign(Variable(command), Number(2))` - вариант перечисления уже свёрнут в своё
    /// числовое значение. В C это неотличимо от присваивания числа и работает
    /// (перечисление там - целое), а в Rust `Command` и `2` - разные типы. Чтобы
    /// восстановить `Command::Stop`, нужен доступ к таблице перечислений модели.
    pub(crate) model: &'a ModelNode,
    /// Общие переменные корня - в структуре `Shared`; печать зависит от
    /// [`shared_via_self`](Self::shared_via_self).
    pub(crate) shared: Vec<String>,
    /// Владеет ли модель полем `self.shared` (корень) - иначе получает параметром.
    pub(crate) shared_via_self: bool,
    /// Локальные имена (параметры `fn`, `var` в теле) -> печатаются голым именем.
    pub(crate) locals: Vec<String>,
    /// Пришедшие по ссылке: возврат по значению даёт `E0308`.
    pub(crate) by_ref: Vec<String>,
    /// Имена, которым в теле присваивают - для выбора `let` против `let mut`.
    ///
    /// В Takt изменяемость не объявляется (`var` изменяем всегда), в Rust лишний `mut` -
    /// это `unused_mut`, то есть отказ проверки. Заполняется обходом тела до печати
    /// (`rust_assigned::collect_assigned`).
    pub(crate) assigned: std::collections::BTreeSet<String>,
    /// Выражение доступа к HAL: `self.hal` в корне, `hal` в под-модели.
    pub(crate) hal: String,
    /// Доступен ли `self` (в теле свободной `fn` - нет).
    pub(crate) has_self: bool,
    /// Является ли [`hal`](Self::hal) уже ссылкой `&mut H`.
    ///
    /// У корня `hal` - Поле типа `H`, и передать его дальше можно только как `&mut
    /// self.hal`. У под-модели и функции это уже `&mut H`, и `&mut hal` дало бы `&mut
    /// &mut H` - а `Hal` для `&mut H` не реализован. Нужно перезаимствование `&mut
    /// *hal`.
    pub(crate) hal_is_ref: bool,
    /// Экземпляры под-моделей текущей модели: уникальное имя -> имя поля.
    ///
    /// Нужны ровно для одной конструкции - `S(Модель) = Состояние` (и её формы `Модель
    /// != Состояние`): чтобы сравнить состояние под-модели, надо знать, в каком поле
    /// она лежит. Цель `c` строит путь от `model->`/`main->`; здесь поля плоские,
    /// поэтому достаточно карты "модель -> поле".
    pub(crate) instances: Vec<(String, String)>,
    /// Профиль времени: нужен печатнику выдержки `after` в `rust_cond` (счётчик тактов
    /// vs метка `now_ms`). Берётся из [`RustMap::time_profile`].
    pub(crate) time_profile: crate::semantic::duration::TimeProfile,
    /// Тип возврата функции, чьё тело печатается.
    ///
    /// `return` - позиция приёмника с известным типом, и форма значения в Rust от него
    /// зависит: `return 1;` при `-> bit` обязано печататься `true`. Вне тела функции
    /// возврата нет - там `None`.
    pub(crate) return_type: Option<TypeNode>,
    /// Тип приёмника для печати степени.
    ///
    /// Вывод типов Rust не проходит сквозь вызов метода, поэтому у литеральной базы
    /// `wrapping_pow` типа нет: `v := 2 ** 8;` давало `(2).wrapping_pow(8)` -
    /// **`E0689`** при нулевом коде возврата `taktc`.
    ///
    /// Поле, а не параметр печати: степень бывает слагаемым (`v := (2 ** 2) + x;`), и
    /// приёмник обязан доехать до неё сквозь арифметику. Перехватывать арифметику своим
    /// печатником нельзя - она печатается **обёрткой** (`wrapping_add`), и второй
    /// печатник разошёлся бы с первым.
    ///
    /// Роль та же, что у `return_type`, - подсказка о приёмнике.
    pub(crate) power_target: Option<TypeNode>,
    /// Печатать ли охранные формулы (`--guard-enable`).
    ///
    /// Формула-оператор печатается тем же носителем, что формула-элемент, и флаг обязан
    /// дойти до печатника тел: спроси его только карта, тела о флаге не узнают.
    pub(crate) guard_enable: bool,
}

impl Scope<'_> {
    /// Копия контекста с известным типом приёмника степени.
    pub(crate) fn with_power_target(&self, ty: &TypeNode) -> Scope<'_> {
        let mut copy = self.clone();
        copy.power_target = Some(ty.clone());
        copy
    }

    /// Печатает HAL в позиции **аргумента** (`&mut ...`).
    ///
    /// Отличается от [`hal_receiver`](Self::hal_receiver): получателю метода
    /// (`hal.read_bit(...)`) ссылка берётся автоматически, а аргументу - нет, и её
    /// форма зависит от того, поле перед нами или уже ссылка.
    pub(crate) fn hal_argument(&self, what: &str) -> Result<String, Diagnostic> {
        let hal = self.hal_receiver(what)?;
        if self.hal_is_ref {
            Ok(format!("&mut *{}", hal))
        } else {
            Ok(format!("&mut {}", hal))
        }
    }

    /// Печатает получатель HAL-вызова.
    ///
    /// # Ошибки
    /// [`RS-022`], если HAL в этой области недоступен. Без проверки получатель
    /// напечатался бы пустым, и вызов уехал бы в никуда: `.log_temp(x)` -
    /// синтаксическая ошибка, а не "почти работает". Это тест против
    /// рассинхрона с предикатом `needs_hal`, решающим, давать ли модели `hal`.
    pub(crate) fn hal_receiver(&self, what: &str) -> Result<&str, Diagnostic> {
        if self.hal.is_empty() {
            return Err(Diagnostic::error(
                Location::Codegen,
                format!(
                    "{} требует доступа к HAL, но он в этой области недоступен",
                    what
                ),
            )
            .with_code("RS-022"));
        }
        Ok(&self.hal)
    }

    /// Печатает обращение к переменной модели по её имени.
    fn field(&self, raw: &str, loc: Location) -> Result<String, Diagnostic> {
        let name = rust_value_name(raw, loc)?;
        if self.locals.iter().any(|l| l == raw) {
            return Ok(name);
        }
        if self.shared.iter().any(|s| s == raw) {
            // Общая переменная - в `Shared`; владелец-корень vs параметр.
            let base = if self.shared_via_self {
                "self.shared"
            } else {
                "shared"
            };
            return Ok(format!("{}.{}", base, name));
        }
        if !self.has_self {
            return Err(Diagnostic::error(
                loc,
                format!(
                    "Обращение к переменной '{}' модели из тела функции не \
                     транслируется в Rust: функция порождается свободной и \
                     состояния модели не видит. Передайте значение параметром",
                    raw
                ),
            )
            .with_code("RS-017"));
        }
        Ok(format!("self.{}", name))
    }
}

/// Печатает чтение порта: `hal.read_bit(InBitPort::Name)`.
fn read_port(
    name: &str,
    ty: &TypeNode,
    direction: PortDirection,
    scope: &Scope,
    loc: Location,
) -> Result<String, Diagnostic> {
    if direction == PortDirection::Out {
        return Err(Diagnostic::error(
            loc,
            format!(
                "Чтение выходного порта '{}' не транслируется в Rust: \
                 HAL-трейт даёт выходному порту только запись",
                name
            ),
        )
        .with_code("RS-018"));
    }
    let class = port_class(ty, name, loc, scope.model)?;
    let read = format!(
        "{}.{}({}::{})",
        scope.hal_receiver(&format!("чтение порта '{}'", name))?,
        class.read_fn(),
        class.in_enum(),
        rust_type_name(name, loc)?
    );
    // Порт перечислимого типа приходит с HAL целым, а приёмник - вариант: значение
    // восстанавливается функцией перечисления. Она печатается по нужде - только при
    // наличии входного порта этого типа.
    if let TypeNode::Enum(enum_name) = ty {
        return Ok(format!(
            "{}::{}({read})",
            rust_type_name(enum_name, loc)?,
            crate::generator::rust::rust_decl::FROM_REPR
        ));
    }
    Ok(read)
}

/// Печатает запись в порт: `hal.write_bit(OutBitPort::Name, value)`.
pub(crate) fn write_port(
    name: &str,
    ty: &TypeNode,
    direction: PortDirection,
    value: &str,
    scope: &Scope,
    loc: Location,
) -> Result<String, Diagnostic> {
    if direction == PortDirection::In {
        return Err(Diagnostic::error(
            loc,
            format!(
                "Запись во входной порт '{}' не транслируется в Rust: \
                 HAL-трейт даёт входному порту только чтение",
                name
            ),
        )
        .with_code("RS-018"));
    }
    let class = port_class(ty, name, loc, scope.model)?;
    // Значение перечислимого типа уходит в HAL целым: в структуре модели оно хранится
    // вариантом, а метод трейта принимает число. `#[repr(...)]` у перечисления цель
    // печатает всегда, поэтому `as` законен.
    let value = match ty {
        TypeNode::Enum(_) => format!("{value} as {}", class.value_type()),
        _ => value.to_string(),
    };
    let receiver = scope.hal_receiver(&format!("запись в порт '{}'", name))?;
    let call = format!(
        "{}.{}({}::{}, {})",
        receiver,
        class.write_fn(),
        class.out_enum(),
        rust_type_name(name, loc)?,
        value
    );
    // Значение, читающее порт, поднимается во временную: методы HAL-трейта берут `&mut
    // self`, и `hal.write_u8(p, hal.read_u8(q))` - это два изменяемых заимствования
    // сразу, `E0499`. Прочие сочетания законны, и это замер, а не осторожность: два
    // чтения подряд (`raw1 + raw2`) и чтение в аргументе функции, которой сам `hal`
    // передаётся, `rustc` принимает - их разводит two-phase borrow. Поднимать нечего и
    // там, где значение порта не читает, поэтому форма печатается по нужде.
    //
    // Признак берётся у напечатанного текста: чтение приходит и из условия, и из
    // арифметики, и из аргумента вызова - обходить дерево значило бы перечислять формы,
    // а печатнику важно одно, есть ли обращение к HAL.
    if value.contains(&format!("{receiver}.")) {
        return Ok(format!(
            "{{ let {TEMP} = {value}; {call_temp} }}",
            call_temp = call.replace(&value, TEMP)
        ));
    }
    Ok(call)
}

/// Имя временной под значение, читающее порт.
///
/// Имя занято компилятором и с авторским столкнуться не может: `takt_` запрещён каноном
/// именования, а `RS-026` проверяет поля структуры модели.
const TEMP: &str = "takt_value";

/// Печатает обращение к переменной/константе/порту.
pub(crate) fn variable(var: &VariableNode, scope: &Scope) -> Result<String, Diagnostic> {
    match var {
        VariableNode::Simple { name, loc, .. } => scope.field(name, *loc),
        // Константы живут на уровне модуля (`const MAX: u8 = 10;`) - обращение по имени
        // без `self`.
        VariableNode::Const {
            upper, name, loc, ..
        } => Ok(const_ident(upper.as_ref(), name, *loc)?),
        VariableNode::Port {
            name,
            ty,
            direction,
            loc,
            ..
        } => read_port(name, ty, *direction, scope, *loc),
        VariableNode::Unresolved => Err(unsupported("неразрешённая переменная")),
    }
}

/// Имя константы уровня модуля: `UPPER_SNAKE_CASE`.
pub(crate) fn const_name(raw: &str, loc: Location) -> Result<String, Diagnostic> {
    Ok(rust_value_name(raw, loc)?
        .trim_start_matches("r#")
        .to_uppercase())
}

/// Имя объявления константы - **с префиксом владельца** (; форма
/// заведена для констант-параметров).
///
/// Модуль в цели `rust` один на всю программу, и `const` в нём - **общее**
/// пространство имён: две модели с одноимённой константой разных значений
/// давали одно объявление, и вторая молча получала значение первой (проба:
/// `model A { const K := 2; } model B { const K := 3; }` -> единственный
/// `const K: u8 = 2`, то есть `y` считался по чужому значению). Поэтому
/// квалифицируются **все** константы, а не только выведенные из параметра
/// модели: имя обязано быть свойством **объявления**, а не программы - иначе
/// добавление модели переименовывало бы константу в другой, уже написанной
///
///
/// Ровно то же правило и в цели `sv` (`sv_expr::const_signal`); цель `c`
/// квалифицирует константы с самого начала, цель `st` держит их полями
/// `FUNCTION_BLOCK` - там коллизии нет по устройству.
///
/// Ключ дедупликации объявлений и ключ "константа используется"
/// ([`crate::semantic::unused::const_key`]) обязаны согласоваться с **этим**
/// именем: печать и фильтрация - одно правило, а не два похожих.
pub(crate) fn const_ident(
    upper: Option<&std::rc::Weak<std::cell::RefCell<crate::semantic::ModelNode>>>,
    name: &str,
    loc: Location,
) -> Result<String, Diagnostic> {
    let ident = const_name(name, loc)?;
    let Some(owner) = upper.and_then(|u| u.upgrade()) else {
        return Ok(ident);
    };
    let model: crate::semantic::minimap::Name = owner.into();
    Ok(format!("{}_{}", model.unique_uppercase_snakecase(), ident))
}

/// Печатает вещественный литерал так, чтобы он был литералом `f64`.
///
/// `Rational` хранит **текст** (`"1"`, `"1.5"`), а `1` литералом `f64` в Rust не
/// является - без точки это целое, и `let x: f64 = 1;` не компилируется.
pub(crate) fn rational(text: &str, negative: bool) -> String {
    let sign = if negative { "-" } else { "" };
    if text.contains('.') || text.contains('e') || text.contains('E') {
        format!("{}{}", sign, text)
    } else {
        format!("{}{}.0", sign, text)
    }
}

/// Печатает бинарную операцию, **всегда** заключая её в скобки.
///
/// ## Скобки здесь - не стиль, а корректность
///
/// Приоритеты C и Rust **расходятся**, и расходятся молча. Проба 2026-07-16:
///
/// ```text
/// a = 2, b = 2, c = 1;
/// C: a == b | c -> 1 (то есть (a == b) | c)
/// Rust: a == b | c -> false (то есть a == (b | c) = 2 == 3)
/// ```
///
/// В C `|` слабее `==`, в Rust - **сильнее**. Один и тот же текст означает
/// разное. Печатать выражения "как в C" значило бы получать код, который
/// собирается и делает не то, - то есть ровно тот дефект, ради отсутствия
/// которого заведена цель (`elevator_mini` поймал это лишь потому, что операнд
/// оказался перечислением и дал ошибку типа; на целых всё бы "работало").
///
/// Структурная расстановка скобок снимает вопрос приоритетов целиком: печатается
/// **дерево**, а не текст. Лишние внешние скобки снимает [`unwrap_outer`] в
/// позиции условия - единственном месте, где `unused_parens` на них ругается.
/// Печать сдвига: сперва правило насыщения, иначе оператор.
fn shift(
    direction: Direction,
    a: &ExpressionNode,
    b: &ExpressionNode,
    op: &str,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    match crate::generator::rust::rust_shift::guarded(direction, a, b, scope)? {
        Some(printed) => Ok(printed),
        None => binary(a, op, b, scope),
    }
}

fn binary(
    a: &ExpressionNode,
    op: &str,
    b: &ExpressionNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    // Бит-вектор шире 64 бит хранится массивом слов, и операции над
    // массивом в Rust не существует: `self.w + 1` давало `E0369` при нулевом
    // коде возврата `taktc`. Той же операции не поддерживает и
    // эталон (`SIM-005` в такте), поэтому отказ приходит свой, с причиной.
    if crate::generator::rust::rust_bit::words_of(a).is_some()
        || crate::generator::rust::rust_bit::words_of(b).is_some()
    {
        return Err(unsupported(&format!(
            "операция '{op}' над бит-вектором шире 64 бит: он представлен массивом \
             слов, и такой операции над словами не существует — её не поддерживает \
             и эталон (SIM-005); работайте с отдельными разрядами"
        )));
    }
    Ok(format!(
        "({} {} {})",
        print_expression(a, scope)?,
        op,
        print_expression(b, scope)?
    ))
}

/// Q-путь бинарной операции: `Some` тогда и только тогда, когда `expr`
/// имеет тип `q(m, n)` - иначе вызывающий печатает обычную арифметику.
fn fixed_binary(
    expr: &ExpressionNode,
    op: FixedOp,
    a: &ExpressionNode,
    b: &ExpressionNode,
    scope: &Scope,
) -> Option<Result<String, Diagnostic>> {
    rust_fixed::fixed_format_in(expr, scope.model)
        .map(|(m, n, sat)| rust_fixed::binary(op, a, b, scope, m, n, sat))
}

/// Печатает сравнение, приводя операнды друг к другу по типу.
///
/// Нужно ровно там же, где и [`coerce_to`]: вариант перечисления приходит из
/// семантики **числом** (`ExpressionNode` варианта не имеет), и `c == 0` при
/// `c : Constant` - ошибка типа. Цель `c` этого не замечает: перечисление там
/// целое.
fn comparison(
    a: &ExpressionNode,
    op: &str,
    b: &ExpressionNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    // Тип берётся у той стороны, где он известен; вторая к нему приводится.
    let left = match expression_type(b) {
        Some(ty) if expression_type(a).is_none() => coerce_to(a, &ty, scope)?,
        _ => print_expression(a, scope)?,
    };
    let right = match expression_type(a) {
        Some(ty) => coerce_to(b, &ty, scope)?,
        None => print_expression(b, scope)?,
    };
    // `x % k = 0` при беззнаковом `x` clippy требует записывать методом
    // (`manual_is_multiple_of`). Проверка идёт по узлам, а не по напечатанному
    // тексту: делитель обязан быть ненулевым литералом.
    if let Some(simplified) = super::rust_modulo::multiple_of(a, op, b, scope)? {
        return Ok(simplified);
    }
    // `x = 0` при булевом `x` даёт после приведения `x == false`, а clippy
    // требует отрицания (`bool_comparison`: "equality checks against false can
    // be replaced by a negation"). Форма `x.2 = 0` в корпусе обычна
    // (`extend_complex.takt`), поэтому случай не теоретический.
    if let Some(simplified) = bool_comparison(&left, op, &right) {
        return Ok(simplified);
    }
    Ok(format!("({} {} {})", left, op, right))
}

/// Упрощает сравнение с булевым литералом: `x == false` -> `(!x)`.
fn bool_comparison(left: &str, op: &str, right: &str) -> Option<String> {
    let (value, other) = match (left, right) {
        ("true" | "false", _) => (left, right),
        (_, "true" | "false") => (right, left),
        _ => return None,
    };
    // Отрицание нужно, когда сравнение с `false` на равенство либо с `true` на
    // неравенство.
    let negate = match (op, value) {
        ("==", "false") | ("!=", "true") => true,
        ("==", "true") | ("!=", "false") => false,
        _ => return None,
    };
    Some(if negate {
        format!("(!{})", other)
    } else {
        other.to_string()
    })
}

/// Транслирует выражение Takt в выражение Rust.
///
/// # Ошибки
/// [`RS-011`] на непереводимой конструкции - **не** тихий пропуск.
pub(crate) fn print_expression(expr: &ExpressionNode, scope: &Scope) -> Result<String, Diagnostic> {
    match expr {
        // Длительность печатается **миллисекундами**; пересчёт зовёт
        // общий слой - своей арифметики времени генератор не заводит.
        ExpressionNode::Duration(nanos) => Ok(crate::semantic::duration::value_millis(
            *nanos,
            Location::Codegen,
            "литерал длительности",
        )?
        .to_string()),
        ExpressionNode::Number(n) => Ok(n.to_string()),
        ExpressionNode::Rational(text, negative) => Ok(rational(text, *negative)),
        ExpressionNode::Bool(b) => Ok(b.to_string()),
        ExpressionNode::Variable(var) => variable(&var.borrow(), scope),
        // Скобки автора структурно уже учтены: печатник расставляет свои вокруг
        // каждого узла. Печатать ещё одни значило бы получать `((x))` - а это
        // `unused_parens`.
        ExpressionNode::Parenthesis(inner) => print_expression(inner, scope),

        // Унарные.
        ExpressionNode::Not(a) => Ok(format!("(!{})", print_expression(a, scope)?)),
        // В Rust `!` - и логическое, и побитовое отрицание (в C - `~`).
        ExpressionNode::BitwiseNot(a) => Ok(format!("(!{})", print_expression(a, scope)?)),
        ExpressionNode::UnaryPlus(a) => print_expression(a, scope),
        ExpressionNode::Negate(a) => match rust_fixed::fixed_format_in(expr, scope.model) {
            Some((m, n, sat)) => rust_fixed::negate(a, scope, m, n, sat),
            None => Ok(format!("(-{})", print_expression(a, scope)?)),
        },

        // Арифметика. Над q(m, n) - масштабирующая Q-арифметика.
        ExpressionNode::Multiply(a, b) => fixed_binary(expr, FixedOp::Multiply, a, b, scope)
            .unwrap_or_else(|| wrapping_or_plain(a, "*", "wrapping_mul", b, scope)),
        ExpressionNode::Divide(a, b) => fixed_binary(expr, FixedOp::Divide, a, b, scope)
            .unwrap_or_else(|| binary(a, "/", b, scope)),
        ExpressionNode::Modulo(a, b) => binary(a, "%", b, scope),
        ExpressionNode::Add(a, b) => fixed_binary(expr, FixedOp::Add, a, b, scope)
            .unwrap_or_else(|| wrapping_or_plain(a, "+", "wrapping_add", b, scope)),
        ExpressionNode::Subtract(a, b) => fixed_binary(expr, FixedOp::Subtract, a, b, scope)
            .unwrap_or_else(|| wrapping_or_plain(a, "-", "wrapping_sub", b, scope)),

        // Побитовые - нативны (в ST требовали бы BYTE_TO_USINT(...)).
        //
        // Сдвиг на величину, не меньшую ширины типа, в Rust либо не
        // собирается, либо считает не то - правило и его
        // повод в заголовке `rust_shift`.
        ExpressionNode::ShiftLeft(a, b) => shift(Direction::Left, a, b, "<<", scope),
        ExpressionNode::ShiftRight(a, b) => shift(Direction::Right, a, b, ">>", scope),
        ExpressionNode::BitwiseAnd(a, b) => binary(a, "&", b, scope),
        ExpressionNode::BitwiseXor(a, b) => binary(a, "^", b, scope),
        ExpressionNode::BitwiseOr(a, b) => binary(a, "|", b, scope),

        // Сравнения. Операнды приводятся друг к другу по типу: `c = X` при
        // `c : Constant` приходит как `Equal(Variable(c), Number(0))` -
        // семантика уже свернула вариант в число (см. `coerce_to`).
        ExpressionNode::Less(a, b) => expr_compare(a, "<", b, scope),
        ExpressionNode::More(a, b) => expr_compare(a, ">", b, scope),
        ExpressionNode::LessEqual(a, b) => expr_compare(a, "<=", b, scope),
        ExpressionNode::MoreEqual(a, b) => expr_compare(a, ">=", b, scope),
        ExpressionNode::Equal(a, b) => expr_compare(a, "==", b, scope),
        ExpressionNode::NotEqual(a, b) => expr_compare(a, "!=", b, scope),

        // Логические.
        ExpressionNode::And(a, b) => binary(a, "&&", b, scope),
        ExpressionNode::Or(a, b) => binary(a, "||", b, scope),

        // `=` в выражении - Присваивание, а не сравнение.
        // Запись в порт - не присваивание, а вызов метода HAL.
        ExpressionNode::Assign(target, value) => {
            crate::generator::rust::rust_assign::assign(target, value, scope)
        }

        ExpressionNode::ConditionalOperator(cond, then_, else_) => Ok(format!(
            "if {} {{ {} }} else {{ {} }}",
            print_expression(cond, scope)?,
            print_expression(then_, scope)?,
            print_expression(else_, scope)?
        )),

        // `x.0` -> маска. В MatIEC битового доступа нет вовсе; здесь - обычная
        // арифметика, но она типозависима: у `bool` бита 0 нет.
        ExpressionNode::BitAccess(inner, member) => {
            crate::generator::rust::rust_bit::bit_access(inner, member, scope)
        }

        // База - выражение: печатается тем же печатником.
        ExpressionNode::ArraySubscript(base, index) => Ok(subscript(
            &print_expression(base, scope)?,
            &print_expression(index, scope)?,
            matches!(index.as_ref(), ExpressionNode::Number(_)),
        )),

        ExpressionNode::Function(def, args) => call(def, args, scope),

        ExpressionNode::Cast(inner, ty) => {
            // Fixed-point: масштабирующее приведение, когда источник либо
            // цель - q(m, n); иначе обычный `as`.
            if matches!(ty, TypeNode::Fixed { .. })
                || rust_fixed::fixed_format_in(inner, scope.model).is_some()
            {
                rust_fixed::cast(inner, ty, scope)
            } else if crate::generator::rust::rust_type::same_printed_type(inner, ty) {
                // Приведение к тому же типу опускается:
                // `r as u16` при `r: u16` - это `clippy::unnecessary_cast`, то
                // есть отказ сборки под `-D warnings`, а проверка цели гоняет ровно
                // эти флаги. Запись законна и осмысленна (автор подчёркивает
                // тип), поэтому отвергать её нельзя - её печать обязана быть
                // валидной.
                print_expression(inner, scope)
            } else {
                let target = crate::generator::rust::rust_type::rust_type(ty, "приведение типа")?;
                Ok(format!(
                    "({} as {})",
                    print_expression(inner, scope)?,
                    target
                ))
            }
        }

        ExpressionNode::Array(items) | ExpressionNode::Initializer(items) => {
            let printed = items
                .iter()
                .map(|item| print_expression(item, scope))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(format!("[{}]", printed.join(", ")))
        }

        // Ниже - непереводимое. Ветки `_` нет намеренно: добавление
        // варианта в `ExpressionNode` обязано валить сборку.
        ExpressionNode::None => Err(unsupported("пустое выражение")),
        ExpressionNode::Unresolved(_) => Err(unsupported("неразрешённое выражение")),
        ExpressionNode::ArraySlice(_, _, _) => Err(unsupported(
            "срез массива: в Takt он не имеет типа-владельца, а в no_std нет alloc",
        )),
        ExpressionNode::CodeBlock(_, _) => Err(unsupported("блок кода в позиции выражения")),
        ExpressionNode::NamedFunctionBox(_, _) => {
            Err(unsupported("вызов с именованными аргументами"))
        }
        // Целая степень - `wrapping_pow`; довод - в заголовке
        // `rust_shift`.
        ExpressionNode::Power(base, exp) => {
            // Тип приёмника здесь неизвестен: его знает только
            // `rust_coerce`, и оттуда идёт вызов с `Some(ty)`.
            crate::generator::rust::rust_shift::power(base, exp, scope, scope.power_target.as_ref())
        }
        ExpressionNode::String(_) => Err(unsupported(
            "строковый литерал вне вызова debug: в no_std нет владеющей строки",
        )),
        ExpressionNode::Type(_) => Err(unsupported("тип в позиции выражения")),
        ExpressionNode::Address(_, _) => Err(unsupported(
            "адресный литерал: цель rust карту адресов не потребляет \
             (порты идут через HAL-трейт)",
        )),
        // Анонимное обращение: у цели `rust` порт - метод
        // HAL-трейта, адреса она не знает.
        ExpressionNode::AnonPort(_) => Err(unsupported(
            "обращение к ячейке по адресу ('#0x…'): цель rust адресов не знает — \
             доступ по адресу дают цели 'c-hal', 'st-at' и 'sv-mmio'",
        )),
        ExpressionNode::Model(_) => Err(unsupported("модель в позиции выражения")),
        // Именованное условие печатается печатником условий;
        // `condition_as_bool` тут не годится - довод в.
        ExpressionNode::Condition(cond) => {
            crate::generator::rust::rust_cond::print_condition(&cond.borrow().value, scope)
        }
        ExpressionNode::List(_) => Err(unsupported("список параметров в позиции выражения")),
    }
}

fn wrapping_or_plain(
    a: &ExpressionNode,
    op: &str,
    wrapping: &str,
    b: &ExpressionNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    if is_unsigned_int(a) || is_unsigned_int(b) {
        let left = wrapping_receiver(a, b, scope)?;
        let right = print_expression(b, scope)?;
        return Ok(format!("{left}.{wrapping}({})", unwrap_outer(&right)));
    }
    binary(a, op, b, scope)
}

/// Левый операнд обёрточной арифметики - Получатель метода, и литералу здесь
/// нужен тип.
///
/// Вывод типа в Rust **не проходит сквозь вызов метода**: `10.wrapping_add(x)`
/// не компилируется вовсе (`E0689`: "can't call method on ambiguous numeric
/// type"), тогда как `x.wrapping_add(10)` печатается верно. Замер 2026-08-31:
/// `probe := 10 + n;` переводили все восемь целей, но вывод `rust` отвергал
/// `rustc` при нулевом коде возврата `taktc`.
///
/// Тип берётся у второго операнда, а не у приёмника: у обёрточной арифметики
/// оба операнда одного типа по построению (`SE-059` запрещает смешение), и
/// печатнику доступен именно он. Приём - суффикс, а не приведение: `10 as u8`
/// это `clippy::unnecessary_cast`, то есть отказ проверки цели.
///
/// Отрицательный литерал под правило не подпадает: `-1u8` невыразим, а
/// значение вне диапазона типа отвергает семантика (`SE-089`) - печатнику
/// такого входа не приходит.
fn wrapping_receiver(
    a: &ExpressionNode,
    b: &ExpressionNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    let printed = print_expression(a, scope)?;
    if crate::generator::shift_width::type_of(a).is_some() {
        return Ok(printed);
    }
    let Some(literal) = crate::generator::shift_width::literal(a) else {
        return Ok(printed);
    };
    if literal < 0 {
        return Ok(printed);
    }
    match rust_fixed::expression_type(b) {
        Some(ty @ TypeNode::Integer { .. }) => {
            let name = crate::generator::rust::rust_type::rust_type(&ty, "операнд арифметики")?;
            Ok(format!("{literal}{name}"))
        }
        _ => Ok(printed),
    }
}

/// Беззнаковое ли целое у выражения (тип известен и не знаковый).
fn is_unsigned_int(expr: &ExpressionNode) -> bool {
    matches!(
        rust_fixed::expression_type(expr),
        Some(TypeNode::Integer { signed: false, .. })
    )
}

/// Арифметический узел, который печатается обёрткой (см. [`wrapping_or_plain`]).
pub(crate) fn is_wrapping_arith(expr: &ExpressionNode) -> bool {
    match expr {
        ExpressionNode::Add(a, b)
        | ExpressionNode::Subtract(a, b)
        | ExpressionNode::Multiply(a, b) => is_unsigned_int(a) || is_unsigned_int(b),
        _ => false,
    }
}

/// Индексация массива - одна функция на цель.
///
/// Приведение к `usize` печатается по нужде: индекс-переменная в нём нуждается (типы
/// Takt - от `u8` до `i64`), а литерал уже выводится как `usize` по контексту, и лишнее
/// `1 as usize` даёт `clippy::unnecessary_cast`, то есть отказ под `-D warnings` -
/// теми же флагами, что стоят в проверке предкоммита.
///
/// Отрицательный литерал сюда не доходит - его отсекает `SE-028` в семантике; иначе
/// `self.arr[-1]` не скомпилировалось бы вовсе.
///
/// Носитель один на обоих печатников (выражения и условия): две копии этого правила
/// разъехались бы молча.
pub(crate) fn subscript(base: &str, index: &str, index_is_literal: bool) -> String {
    if index_is_literal {
        format!("{base}[{index}]")
    } else {
        format!("{base}[{index} as usize]")
    }
}

/// Печатает вызов функции: встроенной, локальной либо внешней.
fn call(
    def: &std::rc::Rc<std::cell::RefCell<FunctionDefinitionNode>>,
    args: &[ExpressionNode],
    scope: &Scope,
) -> Result<String, Diagnostic> {
    let borrowed = def.borrow();
    // Аргумент - позиция приёмника с известным типом: у литерала `bit`, варианта
    // перечисления и разряда форма в Rust зависит от типа параметра, и печать "как есть"
    // даёт `E0308` при нулевом коде возврата `taktc`.
    //
    // Внешняя функция сюда тоже входит: у `External` есть то же поле `params`, что у
    // `Local`. Пропусти её - и `sink(2 ** 3)` отвергается `RS-011`, тогда как эталон и
    // семь остальных целей запись исполняют.
    let param_types: Vec<Option<TypeNode>> = match &*borrowed {
        FunctionDefinitionNode::Local { params, .. }
        | FunctionDefinitionNode::External { params, .. } => {
            params.iter().map(|(_, ty)| Some(ty.clone())).collect()
        }
        _ => Vec::new(),
    };
    let printed = args
        .iter()
        .enumerate()
        .map(|(i, a)| match param_types.get(i).and_then(Option::as_ref) {
            // Массив передаётся по ссылке: по значению это копия
            // на каждый вызов, тогда как цель `c` передаёт указатель, а `st` -
            // `VAR_IN_OUT`. Наблюдаемого расхождения нет - это цена, и платить
            // её незачем.
            Some(ty) if crate::generator::rust::rust_byref::is_array_by_reference(ty) => {
                Ok(format!("&{}", print_expression(a, scope)?))
            }
            Some(ty) => coerce_to(a, ty, scope),
            None => print_expression(a, scope),
        })
        .collect::<Result<Vec<_>, _>>()?;
    match &*borrowed {
        FunctionDefinitionNode::Builtin(name, _, _) => builtin(name, &printed, args, scope),
        local @ FunctionDefinitionNode::Local { name, loc, .. } => Ok(format!(
            "{}({})",
            rust_value_name(name, *loc)?,
            call_arguments(local, &printed, scope)?.join(", ")
        )),
        // Внешняя функция отображается в метод HAL. Форма `extern "C" { fn ... }`
        // отвергнута: она потребовала бы `unsafe` в порождаемом коде, а его в выводе
        // цели `rust` нет по построению.
        FunctionDefinitionNode::External { name, loc, .. } => Ok(format!(
            "{}.{}({})",
            scope.hal_receiver(&format!("вызов внешней функции '{}'", name))?,
            rust_value_name(name, *loc)?,
            printed.join(", ")
        )),
        FunctionDefinitionNode::None => Err(unsupported("пустое определение функции")),
        FunctionDefinitionNode::Unresolved(_) => Err(unsupported("неразрешённая функция")),
    }
}

/// Строит полный список аргументов вызова локальной функции.
///
/// Аргументы обязаны совпадать с параметрами, которые печатает
/// [`rust_func`](crate::generator::rust::rust_func) для той же функции: и то и
/// другое считает **один** предикат [`function_needs`]. Разойдись они -
/// порождённый код не собрался бы (а в худшем случае связал бы не те значения).
///
/// Порядок - тот же, что в сигнатуре: `hal`, объявленные параметры, переменные
/// модели (в порядке `BTreeMap`).
pub(crate) fn call_arguments(
    def: &FunctionDefinitionNode,
    printed: &[String],
    scope: &Scope,
) -> Result<Vec<String>, Diagnostic> {
    let needs = function_needs(def, scope.model, &mut std::collections::BTreeSet::new())?;
    let mut args = Vec::new();
    // Аргумент - позиция, где внешние скобки лишние: `f((*y))` даёт
    // `unnecessary parentheses around function argument`.
    args.extend(printed.iter().map(|a| unwrap_outer(a).to_string()));
    // Переменная модели печатается так, как видна вызывающему: `self.x` в
    // корне, `(*x)` в под-модели, `x` в теле другой функции. Именно поэтому
    // `shared_variables` обязана включать переменные вызываемых функций -
    // иначе под-модели нечего было бы передать.
    for (vname, vty) in &needs.vars {
        let text = scope.field(vname, Location::Codegen)?;
        // Массив передаётся по ссылке - и неявный аргумент тоже
        // сигнатура печатает `&[u8; 4]`, а место вызова отдавало
        // значение, и `rustc` отвечал `E0308` при нулевом коде возврата
        // `taktc`. Печатников признака стало три, и все спрашивают один
        // носитель - разойдись они, вывод не собрался бы.
        let text = if crate::generator::rust::rust_byref::is_array_by_reference(vty) {
            format!("&{}", unwrap_outer(&text))
        } else {
            unwrap_outer(&text).to_string()
        };
        args.push(text);
    }
    // HAL - Последним аргументом, зеркально сигнатуре: иначе вызов вида
    // `f(&mut hal, hal.read_u8(...))` взял бы `hal` изменяемо дважды (E0499).
    if needs.hal {
        let name = match def {
            FunctionDefinitionNode::Local { name, .. } => name.clone(),
            _ => String::new(),
        };
        args.push(scope.hal_argument(&format!("вызов функции '{}'", name))?);
    }
    Ok(args)
}

/// Печатает вызов встроенной функции.
///
/// Проба 2026-07-16: `min`/`max`/`abs`/`clamp` доступны на `u8`, `i32` и `f64`
/// **без `libm`** - цена профиля `no_std` для встроенных функций нулевая.
fn builtin(
    name: &str,
    printed: &[String],
    args: &[ExpressionNode],
    scope: &Scope,
) -> Result<String, Diagnostic> {
    match (name, printed.len()) {
        ("min", 2) => Ok(format!("{}.min({})", printed[0], printed[1])),
        ("max", 2) => Ok(format!("{}.max({})", printed[0], printed[1])),
        ("abs", 1) => Ok(format!("{}.abs()", printed[0])),
        ("clamp", 3) => Ok(format!(
            "{}.clamp({}, {})",
            printed[0], printed[1], printed[2]
        )),
        // `debug` отображается в метод HAL. В `no_std` нет `printf`, но профиль
        // `no_std` означает не "без вывода", а "вывод решает пользователь". Тихо
        // отбросить нельзя: конструкция автора не вправе исчезать из вывода молча.
        ("debug", 1) => {
            let ExpressionNode::String(parts) = &args[0] else {
                return Err(unsupported(
                    "debug с нестроковым аргументом: в no_std форматирования нет, \
                     HAL-метод принимает готовую строку",
                ));
            };
            Ok(format!(
                "{}.debug(\"{}\")",
                scope.hal_receiver("встроенная функция 'debug'")?,
                escape(&parts.join(""))
            ))
        }
        ("S", 1) => Err(unsupported(
            "встроенная функция S вне условия 'S(Модель) = Состояние'",
        )),
        (other, n) => Err(unsupported(&format!(
            "встроенная функция '{}' с {} аргументами",
            other, n
        ))),
    }
}

/// Экранирует строковый литерал Rust.
fn escape(text: &str) -> String {
    text.replace('\\', "\\\\").replace('"', "\\\"")
}

// `expression_type` вынесен в `rust_fixed`: вывод типа тематически рядом
// с детектором Q-формата. (`function_return` уехал вместе с печатником условий в
// `rust_cond`,.)
pub(crate) use crate::generator::rust::rust_fixed::expression_type;

/// Сравнение операндов разной знаковости в выражении.
///
/// Правило одно с печатником условий (`rust_cond::cond_compare`); здесь - путь тела
/// (`if s < u { ... }`), где условие приходит выражением. Без него вывод не собирается
/// вовсе: `E0308`.
fn expr_compare(
    a: &ExpressionNode,
    op: &str,
    b: &ExpressionNode,
    scope: &Scope,
) -> Result<String, Diagnostic> {
    match crate::generator::mixed_sign::plan(
        crate::generator::mixed_sign::operand_type_expr(a).as_ref(),
        crate::generator::mixed_sign::operand_type_expr(b).as_ref(),
    ) {
        crate::generator::mixed_sign::Plan::AsIs => comparison(a, op, b, scope),
        crate::generator::mixed_sign::Plan::Widen { bits } => Ok(format!(
            "(({} as i{bits}) {op} ({} as i{bits}))",
            print_expression(a, scope)?,
            print_expression(b, scope)?
        )),
        crate::generator::mixed_sign::Plan::SignGuard { signed_is_left } => {
            let (lt, rt) = (print_expression(a, scope)?, print_expression(b, scope)?);
            let (signed, unsigned) = if signed_is_left {
                (lt.as_str(), rt.as_str())
            } else {
                (rt.as_str(), lt.as_str())
            };
            let neg = format!("({signed} < 0)");
            let same = if signed_is_left {
                format!("(({signed} as u64) {op} {unsigned})")
            } else {
                format!("({unsigned} {op} ({signed} as u64))")
            };
            let negative_wins = crate::generator::mixed_sign::negative_wins(op, signed_is_left);
            Ok(if negative_wins {
                format!("({neg} || {same})")
            } else {
                format!("(!{neg} && {same})")
            })
        }
    }
}

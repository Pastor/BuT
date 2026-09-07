//! Q-арифметика fixed-point `q(m, n)` для цели ST.
//!
//! **В IEC 61131-3 сдвигов над числами нет** (`SHL`/`SHR` определены только
//! на битовых строках, арифметика над ними запрещена, `<<` не существует -
//! `CLAUDE.md`). Поэтому floor к −∞ у `*` и приведения `q -> int` выражаются
//! **floor-делением** через эмитируемую `FUNCTION TAKT_Q_FLOORDIV` (её MatIEC
//! принимает - проба 2026-07-19). Промежуток арифметики - `LINT` (64 бита):
//! операнды приводятся `{S}_TO_LINT`, результат сужается `LINT_TO_{S}`
//! (усечение битов = wraparound к W).
//!
//! Ограничение: точное произведение шириной `2W` обязано влезть в `LINT`, т. Для `q(32,
//! 32)` (`W = 64`) `*`/`/` дают честную `ST-013`, а не молчаливое переполнение.
//!
//! Нормативные правила совпадают с эталоном симулятора (`eval::fixed`) и целями
//! `c`/`rust` - сверка идёт побитово через вещественный порт (`... as float`).

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::st::st_expr::{inner_expr_type_in, print_expression};
use crate::semantic::type_node::TypeNode;
use crate::semantic::type_node::type_fixed::fixed_storage_bits;
use crate::semantic::{ExpressionNode, ModelNode};

/// Определение `FUNCTION TAKT_Q_FLOORDIV` - floor-деление целых `LINT`.
///
/// `MOD` и сравнение `BOOL <> BOOL` (XOR знаков) MatIEC принимает. Обход ловушки C11 в
/// ST не нужен (сдвигов нет вовсе), но floor к −∞ у `/` в IEC отсутствует (деление
/// усекает к нулю), поэтому floor строится явно.
pub(crate) const TAKT_Q_FLOORDIV: &str = "\
FUNCTION TAKT_Q_FLOORDIV : LINT
VAR_INPUT
    x : LINT;
    d : LINT;
END_VAR
VAR
    q : LINT;
END_VAR
    q := x / d;
    IF (x MOD d <> 0) AND ((x < 0) <> (d < 0)) THEN
        TAKT_Q_FLOORDIV := q - 1;
    ELSE
        TAKT_Q_FLOORDIV := q;
    END_IF;
END_FUNCTION

";

/// Определение `FUNCTION TAKT_Q_WRAP` - перенос к **W** битам.
///
/// Сужение `LINT_TO_{S}` переносит к ширине **хранения**, а не к `W = m + n`: при `W =
/// 12` это 16 бит против 12 - другая граница. Совпадают они лишь при `W ∈ {8, 16, 32,
/// 64}`, каков весь корпус, - поэтому расхождение с эталоном дожило от 0061
/// незамеченным.
///
/// Модуль `2^W` передаётся **аргументом**, а не считается: сдвигов над числами в IEC
/// нет вовсе (ловушка A-4 ), а `SHL` определён лишь над битовыми строками. `MOD` в IEC
/// даёт остаток со знаком делимого - отсюда две поправки.
pub(crate) const TAKT_Q_WRAP: &str = "\
FUNCTION TAKT_Q_WRAP : LINT
VAR_INPUT
    x : LINT;
    m : LINT;
END_VAR
VAR
    r : LINT;
END_VAR
    r := x MOD m;
    IF r < 0 THEN
        r := r + m;
    END_IF;
    IF r >= m / 2 THEN
        r := r - m;
    END_IF;
    TAKT_Q_WRAP := r;
END_FUNCTION

";

/// Определение `FUNCTION TAKT_Q_SAT` - прижатие к границам представления.
///
/// Границы передаются **аргументами**, а не считаются: степеней двойки в арифметике IEC
/// нет (сдвигов над числами тоже - ловушка A-4 ), и вычислять `2^(W−1)` в ST значило бы
/// городить второй способ узнать то, что компилятор уже знает.
pub(crate) const TAKT_Q_SAT: &str = "\
FUNCTION TAKT_Q_SAT : LINT
VAR_INPUT
    x : LINT;
    lo : LINT;
    hi : LINT;
END_VAR
    IF x > hi THEN
        TAKT_Q_SAT := hi;
    ELSIF x < lo THEN
        TAKT_Q_SAT := lo;
    ELSE
        TAKT_Q_SAT := x;
    END_IF;
END_FUNCTION

";

/// Истина, если `W = m + n` уже равна ширине хранения (перенос не нужен).
fn width_is_storage(m: u8, n: u8) -> bool {
    m + n == fixed_storage_bits(m + n)
}

/// Оборачивает выражение-`LINT` переносом к `W`, если `W` != ширины хранения.
///
/// При `W = S` возвращает выражение **как есть**: вывод для корпуса обязан остаться
/// байт-в-байт прежним.
fn wrap_lint(expr: String, m: u8, n: u8, sat: bool) -> Result<String, Diagnostic> {
    // Насыщение прижимает в LINT - До сужения `LINT_TO_{S}`: сужение сработало бы
    // раньше и вернуло обёрнутое значение (капкан ). Границы печатаются литералами:
    // арифметики над ними в IEC не требуется.
    if sat {
        let w = m + n;
        let max = (1i64 << (w - 1)) - 1;
        let min = -(1i64 << (w - 1));
        return Ok(format!("TAKT_Q_SAT({expr}, {min}, {max})"));
    }
    if width_is_storage(m, n) {
        return Ok(expr);
    }
    let w = m + n;
    // 2^W обязан быть представим в LINT (знаковое 64): при W = 63 модуль равен 2^63 и в
    // LINT не влезает. Отказ называет причину - молча считать по неверному модулю
    // значило бы дать иной результат, чем у эталона.
    if w >= 63 {
        return Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            format!(
                "перенос к {w} битам в цели st требует модуля 2^{w}, непредставимого \
                 в LINT (знаковое 64 бита); выберите q(m, n) с m + n ≤ 62 либо \
                 ширину, кратную 8"
            ),
        )
        .with_code("ST-021"));
    }
    Ok(format!("TAKT_Q_WRAP({expr}, {})", 1u64 << w))
}

/// Арифметическая операция над `q(m, n)`.
#[derive(Clone, Copy)]
pub(crate) enum FixedOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// Формат `q(m, n)` выражения, если его тип - `Fixed` (рекурсивно по арифметике;
/// `SE-059` гарантирует единый формат операндов).
pub(crate) fn fixed_format(expr: &ExpressionNode, model: &ModelNode) -> Option<(u8, u8, bool)> {
    // Признак насыщения едет вместе с разрядностями.
    //
    // Тип операнда спрашивается С оглядкой на модель: поле структуры знает только
    // `inner_expr_type_in`, и без него `g.kp as u8` печаталось как `INT_TO_USINT(g.kp)` -
    // без масштабирования, то есть **128** против `1` у эталона, молча.
    if let Some(TypeNode::Fixed { m, n, sat }) = inner_expr_type_in(expr, model) {
        return Some((m, n, sat));
    }
    match expr {
        ExpressionNode::Add(a, b)
        | ExpressionNode::Subtract(a, b)
        | ExpressionNode::Multiply(a, b)
        | ExpressionNode::Divide(a, b) => fixed_format(a, model).or_else(|| fixed_format(b, model)),
        ExpressionNode::Negate(a) | ExpressionNode::Parenthesis(a) => fixed_format(a, model),
        _ => None,
    }
}

/// Знаковый целый тип IEC для разрядности хранения (`m >= 1` -> всегда знаковый).
fn iec_signed(bits: u8) -> &'static str {
    match bits {
        0..=8 => "SINT",
        9..=16 => "INT",
        17..=32 => "DINT",
        _ => "LINT",
    }
}

/// Целый тип IEC произвольной знаковости (для приведений `q ↔ int`).
fn iec_int(bits: u8, signed: bool) -> &'static str {
    match (bits, signed) {
        (8, false) => "USINT",
        (16, false) => "UINT",
        (32, false) => "UDINT",
        (64, false) => "ULINT",
        (8, true) => "SINT",
        (16, true) => "INT",
        (32, true) => "DINT",
        _ => "LINT",
    }
}

/// `LINT`-представление операнда `q` (тип хранения `S`): `{S}_TO_LINT(expr)`.
fn to_lint(printed: &str, s: &str) -> String {
    format!("{s}_TO_LINT({printed})")
}

/// Печатает бинарную q-операцию. Результат сужается `LINT_TO_{S}` (wraparound).
pub(crate) fn binary(
    op: FixedOp,
    a: &ExpressionNode,
    b: &ExpressionNode,
    model: &ModelNode,
    m: u8,
    n: u8,
    sat: bool,
) -> Result<String, Diagnostic> {
    let bits = fixed_storage_bits(m + n);
    let s = iec_signed(bits);
    let (la, lb) = (print_expression(a, model)?, print_expression(b, model)?);
    let (la, lb) = (to_lint(&la, s), to_lint(&lb, s));
    let pow = 1u64 << n;
    let inner = match op {
        FixedOp::Add => format!("{la} + {lb}"),
        FixedOp::Subtract => format!("{la} - {lb}"),
        FixedOp::Multiply | FixedOp::Divide if bits == 64 => return Err(too_wide(m, n)),
        // Точное произведение 2W -> floor к −∞ (TAKT_Q_FLOORDIV).
        FixedOp::Multiply => format!("TAKT_Q_FLOORDIV({la} * {lb}, {pow})"),
        // Делимое <- n влево (умножением), деление IEC усекает к нулю (как сим).
        FixedOp::Divide => format!("({la} * {pow}) / {lb}"),
    };
    Ok(format!("LINT_TO_{s}({})", wrap_lint(inner, m, n, sat)?))
}

/// Печатает унарный минус над `q(m, n)`: `−repr` с wraparound к W.
pub(crate) fn negate(
    inner: &ExpressionNode,
    model: &ModelNode,
    m: u8,
    n: u8,
    sat: bool,
) -> Result<String, Diagnostic> {
    let s = iec_signed(fixed_storage_bits(m + n));
    let li = to_lint(&print_expression(inner, model)?, s);
    Ok(format!(
        "LINT_TO_{s}({})",
        wrap_lint(format!("-{li}"), m, n, sat)?
    ))
}

/// Печатает приведение `expr as T`, когда источник **или** цель - `q(m, n)`.
pub(crate) fn cast(
    inner: &ExpressionNode,
    target: &TypeNode,
    model: &ModelNode,
) -> Result<String, Diagnostic> {
    let src = fixed_format(inner, model);
    let printed = print_expression(inner, model)?;
    match (src, target) {
        // q -> q: пересчёт дробных разрядов.
        (Some((_, from_n, _)), TypeNode::Fixed { m: tm, n: tn, sat }) => {
            let li = to_lint(&printed, iec_signed(storage_of(inner, model)));
            rescale(&li, from_n, *tn, *tm, *sat)
        }
        // q -> float: repr / 2^n (точно представимо в LREAL).
        (Some((_, from_n, _)), TypeNode::Rational) => {
            let s = iec_signed(storage_of(inner, model));
            Ok(format!("({s}_TO_LREAL({printed}) / {}.0)", 1u64 << from_n))
        }
        // q -> целое/бит: floor(repr / 2^n) = целая часть.
        (Some((_, from_n, _)), _) => {
            let s = iec_signed(storage_of(inner, model));
            let tgt = int_name_of_target(target)?;
            let li = to_lint(&printed, s);
            Ok(format!(
                "LINT_TO_{tgt}(TAKT_Q_FLOORDIV({li}, {}))",
                1u64 << from_n
            ))
        }
        // Литерал -> q: значение известно при компиляции.
        //
        // Это и обещал текст соседнего отказа ("литеральный float понижается на этапе
        // компиляции"), но в теле не делал никто: замер 2026-08-22 дал `ST-011` -
        // причём с текстом "тип источника не выводится статически", хотя источник есть
        // литерал. Счёт - у общего носителя (`const_eval::fixed_literal`), поэтому
        // значение совпадает с эталоном и с прочими целями по построению.
        (None, TypeNode::Fixed { .. })
            if let Some(repr) =
                crate::semantic::const_eval::fixed_literal::cast_repr(inner, target) =>
        {
            Ok(format!("{repr}"))
        }
        // float -> q: floor(f · 2^n) - LREAL_TO_INT в IEC округляет, не floor.
        (None, TypeNode::Fixed { .. })
            if matches!(inner_expr_type_in(inner, model), Some(TypeNode::Rational)) =>
        {
            Err(Diagnostic::error(
                crate::generator::site::at(Location::Codegen),
                "приведение float → q в цели st: LREAL_TO_INT округляет к ближайшему, \
                 а q требует floor; литеральный float понижается на этапе компиляции"
                    .to_string(),
            )
            .with_code("ST-014"))
        }
        // целое/бит -> q: repr = v · 2^n с переносом либо насыщением к W.
        (None, TypeNode::Fixed { m: tm, n: tn, sat }) => {
            let ts = iec_signed(fixed_storage_bits(tm + tn));
            let src_ty = inner_expr_type_in(inner, model).ok_or_else(untyped_source)?;
            let src_name = match src_ty {
                TypeNode::Integer { bits, signed } => iec_int(bits, signed),
                TypeNode::Bit | TypeNode::Bool => "BOOL",
                _ => return Err(untyped_source()),
            };
            let li = if src_name == "BOOL" {
                format!("BOOL_TO_LINT({printed})")
            } else {
                format!("{src_name}_TO_LINT({printed})")
            };
            Ok(format!(
                "LINT_TO_{ts}({})",
                wrap_lint(format!("{li} * {}", 1u64 << tn), *tm, *tn, *sat)?
            ))
        }
        (None, _) => Ok(printed),
    }
}

/// Тип хранения (в битах) выражения-`q` - по его выведенному формату.
fn storage_of(expr: &ExpressionNode, model: &ModelNode) -> u8 {
    match fixed_format(expr, model) {
        // Ширина хранения от признака насыщения не зависит: `sat` меняет поведение при
        // переполнении, а не размер поля.
        Some((m, n, _)) => fixed_storage_bits(m + n),
        None => 64,
    }
}

/// Пересчёт представления `q` между дробными разрядностями с сужением к `S2`.
fn rescale(li: &str, from_n: u8, to_n: u8, to_m: u8, sat: bool) -> Result<String, Diagnostic> {
    let s2 = iec_signed(fixed_storage_bits(to_m + to_n));
    let inner = if to_n >= from_n {
        format!("{li} * {}", 1u64 << (to_n - from_n))
    } else {
        format!("TAKT_Q_FLOORDIV({li}, {})", 1u64 << (from_n - to_n))
    };
    Ok(format!(
        "LINT_TO_{s2}({})",
        wrap_lint(inner, to_m, to_n, sat)?
    ))
}

/// Имя целого IEC-типа цели приведения `q -> int`.
fn int_name_of_target(target: &TypeNode) -> Result<&'static str, Diagnostic> {
    match target {
        TypeNode::Integer { bits, signed } => Ok(iec_int(*bits, *signed)),
        TypeNode::Bit | TypeNode::Bool => Ok("BOOL"),
        _ => Err(Diagnostic::error(
            crate::generator::site::at(Location::Codegen),
            "приведение q → нецелого типа не поддержано".to_string(),
        )
        .with_code("ST-011")),
    }
}

/// `ST-013` - `q` шире 32 бит: промежуток `2W` не влезает в `LINT`.
fn too_wide(m: u8, n: u8) -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        format!(
            "q({m}, {n}): W = {} > 32 — точное произведение шириной 2W не влезает в LINT",
            m + n
        ),
    )
    .with_code("ST-013")
}

/// `ST-011` - тип источника приведения в `q` не выводится.
fn untyped_source() -> Diagnostic {
    Diagnostic::error(
        crate::generator::site::at(Location::Codegen),
        "приведение в q: тип источника не выводится статически".to_string(),
    )
    .with_code("ST-011")
}

/// Вставляет `FUNCTION TAKT_Q_FLOORDIV` перед первым POU, если она вызвана в `program`.
/// Эмитится по факту вызова (без лишней POU); корпус без `q` неизменен.
/// Опережающие ссылки в ST - расширение `iec2c -p`, которым цель уже пользуется,
/// поэтому позиция "перед первым FUNCTION_BLOCK" безопасна.
pub(crate) fn insert_helper(program: String) -> String {
    let mut helpers = String::new();
    // Порядок значим: `TAKT_Q_WRAP` зовёт только себя, `TAKT_Q_FLOORDIV` - тоже, но
    // объявление обязано стоять до использования, а вставляются они разом перед первым
    // POU.
    if program.contains("TAKT_Q_SAT(") {
        helpers.push_str(TAKT_Q_SAT);
    }
    if program.contains("TAKT_Q_WRAP(") {
        helpers.push_str(TAKT_Q_WRAP);
    }
    if program.contains("TAKT_Q_FLOORDIV(") {
        helpers.push_str(TAKT_Q_FLOORDIV);
    }
    if helpers.is_empty() {
        return program;
    }
    // **Перед первым POU, а не перед `FUNCTION_BLOCK`**. В IEC 61131-3 опережающих
    // ссылок нет, а пользовательские `FUNCTION` печатаются до блока: хелпер,
    // вставленный перед `FUNCTION_BLOCK`, оказывался объявлен позже своего вызова, и
    // `iec2c` отвечал "')' missing at the end of function invocation" - диагностикой о
    // синтаксисе в строке вызова, по которой причину не опознать.
    match first_pou(&program) {
        Some(i) => {
            let mut s = program;
            s.insert_str(i, &helpers);
            s
        }
        None => format!("{helpers}{program}"),
    }
}

/// Смещение первого POU - строки, начинающейся с `FUNCTION` (в том числе
/// `FUNCTION_BLOCK`).
///
/// Ищется **начало строки**: слово `FUNCTION` встречается и в комментариях, и внутри
/// тел, а вставка обязана попасть между разделом `TYPE` и первым POU.
fn first_pou(program: &str) -> Option<usize> {
    if program.starts_with("FUNCTION") {
        return Some(0);
    }
    program.find("\nFUNCTION").map(|i| i + 1)
}

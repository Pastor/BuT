//! Унарные и бинарные операции над [`Value`] - **единственное** место, где живёт
//! семантика операций симулятора.
//!
//! Здесь реализована только **тип-независимая** часть таблицы `S` анализа: то, что не
//! требует знания объявленного типа переменной. Правила, зависящие от
//! типа назначения, живут в [`crate::eval::coerce_to_type`] и
//! применяются на месте присваивания.
//!
//! Арифметика ведётся в `i64` (или `f64` при смешении, S5); усечение до объявленной
//! разрядности выполняется при записи. Это повторяет модель C, где операнды
//! продвигаются до `int`, а сужение происходит при присваивании.

use crate::eval::error::{EvalError, value_kind};
use crate::eval::value::Value;

/// Бинарная операция.
///
/// Логические (`LogicalAnd`/`LogicalOr`) и побитовые (`BitwiseAnd`/`BitwiseOr`)
/// разделены намеренно: `ConditionNode::And`/`Or` документированы как побитовые, но
/// исторически вычислялись логически. Выбор делает адаптер, ядро предоставляет обе
/// операции и ни одну не навязывает.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    ShiftLeft,
    ShiftRight,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalAnd,
    LogicalOr,
    Less,
    More,
    LessEqual,
    MoreEqual,
    Equal,
    NotEqual,
}

/// Унарная операция.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnOp {
    /// Логическое не (`!`).
    Not,
    /// Побитовое не (`~`).
    BitwiseNot,
    /// Унарный минус (`-`).
    Negate,
    /// Унарный плюс (`+`).
    UnaryPlus,
}

impl BinOp {
    /// Сравнение ли это (включая равенство).
    ///
    /// Нужно правилу "целое рядом с q есть представление": оно действует **только** в
    /// сравнении, потому что в арифметике смешение отвергает `SE-059` - вход до эталона
    /// не доходит.
    pub(crate) fn is_comparison(self) -> bool {
        matches!(
            self,
            BinOp::Less
                | BinOp::More
                | BinOp::LessEqual
                | BinOp::MoreEqual
                | BinOp::Equal
                | BinOp::NotEqual
        )
    }

    /// Символ операции - для текстов диагностик.
    pub(crate) fn symbol(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Subtract => "-",
            BinOp::Multiply => "*",
            BinOp::Divide => "/",
            BinOp::Modulo => "%",
            BinOp::Power => "**",
            BinOp::ShiftLeft => "<<",
            BinOp::ShiftRight => ">>",
            BinOp::BitwiseAnd => "&",
            BinOp::BitwiseOr => "|",
            BinOp::BitwiseXor => "^",
            BinOp::LogicalAnd => "&&",
            BinOp::LogicalOr => "||",
            BinOp::Less => "<",
            BinOp::More => ">",
            BinOp::LessEqual => "<=",
            BinOp::MoreEqual => ">=",
            BinOp::Equal => "=",
            BinOp::NotEqual => "!=",
        }
    }
}

impl UnOp {
    fn symbol(self) -> &'static str {
        match self {
            UnOp::Not => "!",
            UnOp::BitwiseNot => "~",
            UnOp::Negate => "-",
            UnOp::UnaryPlus => "+",
        }
    }
}

/// Приводит значение к логическому.
///
/// Целое трактуется как `n != 0` (как в C). Вещественное и массив к логическому не
/// приводятся: неявное приведение скрыло бы ошибку типов.
pub(crate) fn to_bool(value: &Value) -> Result<bool, EvalError> {
    match value {
        Value::Boolean(b) => Ok(*b),
        Value::Number(n) => Ok(*n != 0),
        Value::Real(_)
        | Value::Array(_)
        | Value::Fixed { .. }
        | Value::Struct { .. }
        | Value::Duration(_) => Err(EvalError::TypeMismatch {
            op: "логическое условие",
            lhs: value_kind(value),
            rhs: None,
        }),
    }
}

/// Числовой операнд после приведения.
enum Num {
    Int(i128),
    Real(f64),
}

/// Извлекает числовой операнд; `bool` числом **не** считается.
fn as_num(value: &Value, op: BinOp, other: Option<&Value>) -> Result<Num, EvalError> {
    match value {
        Value::Number(n) => Ok(Num::Int(*n)),
        Value::Real(f) => Ok(Num::Real(*f)),
        // q(m, n) сюда не доходит: `apply_binary` перехватывает Fixed раньше.
        Value::Boolean(_)
        | Value::Array(_)
        | Value::Fixed { .. }
        | Value::Struct { .. }
        | Value::Duration(_) => Err(EvalError::TypeMismatch {
            op: op.symbol(),
            lhs: value_kind(value),
            rhs: other.map(value_kind),
        }),
    }
}

/// Пара операндов, приведённая по S5: если хоть один вещественный - оба к `f64`.
enum Pair {
    Ints(i128, i128),
    Reals(f64, f64),
}

fn coerce_pair(lhs: &Value, rhs: &Value, op: BinOp) -> Result<Pair, EvalError> {
    let left = as_num(lhs, op, Some(rhs))?;
    let right = as_num(rhs, op, Some(lhs))?;
    Ok(match (left, right) {
        (Num::Int(a), Num::Int(b)) => Pair::Ints(a, b),
        // S5: смешение int/real - оба к f64 (обычные арифметические преобразования C).
        // Именно этот случай ронял `flat` (дефект Д6).
        (Num::Int(a), Num::Real(b)) => Pair::Reals(a as f64, b),
        (Num::Real(a), Num::Int(b)) => Pair::Reals(a, b as f64),
        (Num::Real(a), Num::Real(b)) => Pair::Reals(a, b),
    })
}

/// Целочисленные операнды: операция определена только для целых.
fn as_ints(lhs: &Value, rhs: &Value, op: BinOp) -> Result<(i128, i128), EvalError> {
    match coerce_pair(lhs, rhs, op)? {
        Pair::Ints(a, b) => Ok((a, b)),
        Pair::Reals(_, _) => Err(EvalError::TypeMismatch {
            op: op.symbol(),
            lhs: value_kind(lhs),
            rhs: Some(value_kind(rhs)),
        }),
    }
}

/// Применяет бинарную операцию.
///
/// Паники недостижимы: любое неопределённое сочетание - [`EvalError`].
pub(crate) fn apply_binary(op: BinOp, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    // q(m, n): fixed-point самоописателен - Q-семантика (сдвиг на n у `*`/`/`) живёт в
    // `fixed`, а не размазана по обычной целочисленной ветке.
    if matches!(lhs, Value::Fixed { .. }) || matches!(rhs, Value::Fixed { .. }) {
        return crate::eval::fixed::binary(op, lhs, rhs);
    }
    // Длительность перехватывается так же: значение самоописательно, семантика времени
    // живёт в своём модуле, а не в числовой ветке.
    if matches!(lhs, Value::Duration(_)) || matches!(rhs, Value::Duration(_)) {
        return crate::eval::duration::binary(op, lhs, rhs);
    }
    match op {
        BinOp::Add => arith(op, lhs, rhs, i128::checked_add, |a, b| a + b),
        BinOp::Subtract => arith(op, lhs, rhs, i128::checked_sub, |a, b| a - b),
        BinOp::Multiply => arith(op, lhs, rhs, i128::checked_mul, |a, b| a * b),
        BinOp::Divide => divide(op, lhs, rhs),
        BinOp::Modulo => modulo(op, lhs, rhs),
        BinOp::Power => power(op, lhs, rhs),
        BinOp::ShiftLeft | BinOp::ShiftRight => shift(op, lhs, rhs),
        BinOp::BitwiseAnd | BinOp::BitwiseOr | BinOp::BitwiseXor => bitwise(op, lhs, rhs),
        BinOp::LogicalAnd => Ok(Value::Boolean(to_bool(lhs)? && to_bool(rhs)?)),
        BinOp::LogicalOr => Ok(Value::Boolean(to_bool(lhs)? || to_bool(rhs)?)),
        BinOp::Less | BinOp::More | BinOp::LessEqual | BinOp::MoreEqual => compare(op, lhs, rhs),
        BinOp::Equal => equality(op, lhs, rhs, false),
        BinOp::NotEqual => equality(op, lhs, rhs, true),
    }
}

fn arith(
    op: BinOp,
    lhs: &Value,
    rhs: &Value,
    int_op: fn(i128, i128) -> Option<i128>,
    real_op: fn(f64, f64) -> f64,
) -> Result<Value, EvalError> {
    match coerce_pair(lhs, rhs, op)? {
        Pair::Ints(a, b) => int_op(a, b)
            .map(Value::Number)
            .ok_or(EvalError::ArithmeticOverflow { op: op.symbol() }),
        Pair::Reals(a, b) => Ok(Value::Real(real_op(a, b))),
    }
}

// Проверка делителя `if b == 0.0` не сводится к паттерну `0.0`: буквальный float в
// паттерне даёт `illegal_floating_point_literal_pattern` (future-incompat), то есть
// лечение линта породило бы худшее предупреждение.
#[allow(clippy::redundant_guards)]
fn divide(op: BinOp, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    match coerce_pair(lhs, rhs, op)? {
        // S3: деление на ноль - UB в C; выдаём ошибку, а не паникуем и не врём.
        Pair::Ints(_, 0) => Err(EvalError::DivisionByZero),
        Pair::Ints(a, b) => a
            .checked_div(b)
            .map(Value::Number)
            .ok_or(EvalError::ArithmeticOverflow { op: op.symbol() }),
        Pair::Reals(_, b) if b == 0.0 => Err(EvalError::DivisionByZero),
        Pair::Reals(a, b) => Ok(Value::Real(a / b)),
    }
}

#[allow(clippy::redundant_guards)] // см. `divide`: паттерн `0.0` - future-incompat
fn modulo(op: BinOp, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    match coerce_pair(lhs, rhs, op)? {
        Pair::Ints(_, 0) => Err(EvalError::DivisionByZero),
        Pair::Ints(a, b) => a
            .checked_rem(b)
            .map(Value::Number)
            .ok_or(EvalError::ArithmeticOverflow { op: op.symbol() }),
        Pair::Reals(_, b) if b == 0.0 => Err(EvalError::DivisionByZero),
        Pair::Reals(a, b) => Ok(Value::Real(a % b)),
    }
}

fn power(op: BinOp, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    match coerce_pair(lhs, rhs, op)? {
        Pair::Ints(a, b) => {
            let exp = u32::try_from(b).map_err(|_| EvalError::ArithmeticOverflow { op: "**" })?;
            a.checked_pow(exp)
                .map(Value::Number)
                .ok_or(EvalError::ArithmeticOverflow { op: op.symbol() })
        }
        Pair::Reals(a, b) => Ok(Value::Real(a.powf(b))),
    }
}

/// Сдвиги.
///
/// S4: `u8: x << 8` - в C **определено** (операнд продвигается до `int`), поэтому
/// вычисляем в `i64`, а усечение делает [`crate::eval::coerce_to_type`] при записи.
/// S4a: сдвиг на отрицательное или >= 64 - UB в C, отдаём ошибку.
///
/// Известное ограничение: точная граница C - ширина **продвинутого** типа (32 для типов
/// уже `int`), а не 64. Уточнение требует типа операнда, которого у ядра нет;
/// переносится в адаптеры, где тип известен.
fn shift(op: BinOp, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    let (a, b) = as_ints(lhs, rhs, op)?;
    if !(0..64).contains(&b) {
        return Err(EvalError::ShiftOutOfRange { by: b });
    }
    let by = b as u32;
    let result = match op {
        BinOp::ShiftLeft => a
            .checked_shl(by)
            .ok_or(EvalError::ShiftOutOfRange { by: b })?,
        BinOp::ShiftRight => a
            .checked_shr(by)
            .ok_or(EvalError::ShiftOutOfRange { by: b })?,
        BinOp::Add
        | BinOp::Subtract
        | BinOp::Multiply
        | BinOp::Divide
        | BinOp::Modulo
        | BinOp::Power
        | BinOp::BitwiseAnd
        | BinOp::BitwiseOr
        | BinOp::BitwiseXor
        | BinOp::LogicalAnd
        | BinOp::LogicalOr
        | BinOp::Less
        | BinOp::More
        | BinOp::LessEqual
        | BinOp::MoreEqual
        | BinOp::Equal
        | BinOp::NotEqual => unreachable!("shift вызывается только для ShiftLeft/ShiftRight"),
    };
    Ok(Value::Number(result))
}

fn bitwise(op: BinOp, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    let (a, b) = as_ints(lhs, rhs, op)?;
    let result = match op {
        BinOp::BitwiseAnd => a & b,
        BinOp::BitwiseOr => a | b,
        BinOp::BitwiseXor => a ^ b,
        BinOp::Add
        | BinOp::Subtract
        | BinOp::Multiply
        | BinOp::Divide
        | BinOp::Modulo
        | BinOp::Power
        | BinOp::ShiftLeft
        | BinOp::ShiftRight
        | BinOp::LogicalAnd
        | BinOp::LogicalOr
        | BinOp::Less
        | BinOp::More
        | BinOp::LessEqual
        | BinOp::MoreEqual
        | BinOp::Equal
        | BinOp::NotEqual => unreachable!("bitwise вызывается только для побитовых операций"),
    };
    Ok(Value::Number(result))
}

fn compare(op: BinOp, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    let ordering = match coerce_pair(lhs, rhs, op)? {
        Pair::Ints(a, b) => a.partial_cmp(&b),
        Pair::Reals(a, b) => a.partial_cmp(&b),
    };
    // NaN несравним - не выдумываем порядок, сообщаем об ошибке.
    let Some(ordering) = ordering else {
        return Err(EvalError::TypeMismatch {
            op: op.symbol(),
            lhs: value_kind(lhs),
            rhs: Some(value_kind(rhs)),
        });
    };
    let result = match op {
        BinOp::Less => ordering.is_lt(),
        BinOp::More => ordering.is_gt(),
        BinOp::LessEqual => ordering.is_le(),
        BinOp::MoreEqual => ordering.is_ge(),
        BinOp::Add
        | BinOp::Subtract
        | BinOp::Multiply
        | BinOp::Divide
        | BinOp::Modulo
        | BinOp::Power
        | BinOp::ShiftLeft
        | BinOp::ShiftRight
        | BinOp::BitwiseAnd
        | BinOp::BitwiseOr
        | BinOp::BitwiseXor
        | BinOp::LogicalAnd
        | BinOp::LogicalOr
        | BinOp::Equal
        | BinOp::NotEqual => unreachable!("compare вызывается только для операций сравнения"),
    };
    Ok(Value::Boolean(result))
}

/// Равенство: определено для чисел (со смешением по S5) и для логических.
fn equality(op: BinOp, lhs: &Value, rhs: &Value, negate: bool) -> Result<Value, EvalError> {
    let equal = match (lhs, rhs) {
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Number(_) | Value::Real(_), Value::Number(_) | Value::Real(_)) => {
            match coerce_pair(lhs, rhs, op)? {
                Pair::Ints(a, b) => a == b,
                Pair::Reals(a, b) => a == b,
            }
        }
        // Массивы и смешение bool с числом  - ошибка, а не тихое `false`: тихое
        // `false` неотличимо от честного неравенства (дефект Д8). q(m, n) сюда не
        // доходит: `apply_binary` перехватывает Fixed раньше. Сравнение структур (`p =
        // q`) - тоже TypeMismatch, осознанно: C запрещает `==` на структурах, и
        // симулятор, "умеющий больше" эталона, дал бы ложную уверенность.
        (Value::Array(_), _)
        | (_, Value::Array(_))
        // Длительность сюда не доходит: `apply_binary` перехватывает её раньше, как и
        // Fixed. Ветка нужна разбору, а не вычислению.
        | (Value::Duration(_), _)
        | (_, Value::Duration(_))
        | (Value::Fixed { .. }, _)
        | (_, Value::Fixed { .. })
        | (Value::Struct { .. }, _)
        | (_, Value::Struct { .. })
        | (Value::Boolean(_), Value::Number(_) | Value::Real(_))
        | (Value::Number(_) | Value::Real(_), Value::Boolean(_)) => {
            return Err(EvalError::TypeMismatch {
                op: op.symbol(),
                lhs: value_kind(lhs),
                rhs: Some(value_kind(rhs)),
            });
        }
    };
    Ok(Value::Boolean(equal != negate))
}

/// Применяет унарную операцию.
pub(crate) fn apply_unary(op: UnOp, value: &Value) -> Result<Value, EvalError> {
    match op {
        UnOp::Not => Ok(Value::Boolean(!to_bool(value)?)),
        UnOp::BitwiseNot => match value {
            Value::Number(n) => Ok(Value::Number(!n)),
            Value::Real(_)
            | Value::Boolean(_)
            | Value::Array(_)
            | Value::Fixed { .. }
            | Value::Struct { .. }
            | Value::Duration(_) => Err(EvalError::TypeMismatch {
                op: op.symbol(),
                lhs: value_kind(value),
                rhs: None,
            }),
        },
        UnOp::Negate => match value {
            Value::Number(n) => n
                .checked_neg()
                .map(Value::Number)
                .ok_or(EvalError::ArithmeticOverflow { op: op.symbol() }),
            Value::Real(f) => Ok(Value::Real(-f)),
            // q(m, n): унарный минус над представлением - перенос либо насыщение, по
            // признаку формата.
            Value::Fixed { repr, m, n, sat } => Ok(crate::eval::fixed::negate(*repr, *m, *n, *sat)),
            Value::Boolean(_) | Value::Array(_) | Value::Struct { .. } | Value::Duration(_) => {
                Err(EvalError::TypeMismatch {
                    op: op.symbol(),
                    lhs: value_kind(value),
                    rhs: None,
                })
            }
        },
        UnOp::UnaryPlus => match value {
            Value::Number(n) => Ok(Value::Number(*n)),
            Value::Real(f) => Ok(Value::Real(*f)),
            Value::Fixed { repr, m, n, sat } => Ok(Value::Fixed {
                repr: *repr,
                m: *m,
                n: *n,
                sat: *sat,
            }),
            Value::Boolean(_) | Value::Array(_) | Value::Struct { .. } | Value::Duration(_) => {
                Err(EvalError::TypeMismatch {
                    op: op.symbol(),
                    lhs: value_kind(value),
                    rhs: None,
                })
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn int(n: i128) -> Value {
        Value::Number(n)
    }

    fn real(f: f64) -> Value {
        Value::Real(f)
    }

    // -- Арифметика ------------------------------------------------------------

    #[test]
    fn add_integers() {
        // Дефект Д1: именно этого не умел eval_expression_rt.
        assert_eq!(apply_binary(BinOp::Add, &int(5), &int(1)), Ok(int(6)));
    }

    #[test]
    fn subtract_multiply_divide_modulo_integers() {
        assert_eq!(apply_binary(BinOp::Subtract, &int(5), &int(3)), Ok(int(2)));
        assert_eq!(apply_binary(BinOp::Multiply, &int(5), &int(3)), Ok(int(15)));
        assert_eq!(apply_binary(BinOp::Divide, &int(7), &int(2)), Ok(int(3)));
        assert_eq!(apply_binary(BinOp::Modulo, &int(7), &int(2)), Ok(int(1)));
    }

    #[test]
    fn power_integers() {
        assert_eq!(apply_binary(BinOp::Power, &int(2), &int(10)), Ok(int(1024)));
    }

    // -- S3: деление на ноль ---------------------------------------------------

    #[test]
    fn s3_division_by_zero_is_error_not_panic() {
        assert_eq!(
            apply_binary(BinOp::Divide, &int(10), &int(0)),
            Err(EvalError::DivisionByZero)
        );
    }

    #[test]
    fn s3_modulo_by_zero_is_error() {
        assert_eq!(
            apply_binary(BinOp::Modulo, &int(10), &int(0)),
            Err(EvalError::DivisionByZero)
        );
    }

    #[test]
    fn s3_real_division_by_zero_is_error_not_infinity() {
        // Контрпример: без явной проверки f64 дал бы inf и "успешно" продолжил.
        assert_eq!(
            apply_binary(BinOp::Divide, &real(1.0), &real(0.0)),
            Err(EvalError::DivisionByZero)
        );
    }

    // -- S5: смешение int/real -------------------------------------------------

    #[test]
    fn s5_mixed_int_real_add_yields_real() {
        // Дефект Д6: этот случай ронял flat через unwrap() на None.
        assert_eq!(apply_binary(BinOp::Add, &int(1), &real(2.5)), Ok(real(3.5)));
    }

    #[test]
    fn s5_mixed_int_real_compare_does_not_panic() {
        let sum = apply_binary(BinOp::Add, &int(1), &real(2.5)).unwrap();
        assert_eq!(
            apply_binary(BinOp::More, &sum, &int(3)),
            Ok(Value::Boolean(true))
        );
    }

    // -- S4a: сдвиги -----------------------------------------------------------

    #[test]
    fn s4_shift_left_within_i64_is_computed() {
        // S4: для u8 `x << 8` в C определено (продвижение до int) -> 256, усечение до
        // u8 произойдёт при записи (coerce_to_type).
        assert_eq!(
            apply_binary(BinOp::ShiftLeft, &int(1), &int(8)),
            Ok(int(256))
        );
    }

    #[test]
    fn s4a_negative_shift_is_error() {
        assert_eq!(
            apply_binary(BinOp::ShiftLeft, &int(1), &int(-1)),
            Err(EvalError::ShiftOutOfRange { by: -1 })
        );
    }

    #[test]
    fn s4a_shift_by_64_is_error() {
        assert_eq!(
            apply_binary(BinOp::ShiftRight, &int(1), &int(64)),
            Err(EvalError::ShiftOutOfRange { by: 64 })
        );
    }

    #[test]
    fn shift_right_works() {
        assert_eq!(
            apply_binary(BinOp::ShiftRight, &int(256), &int(8)),
            Ok(int(1))
        );
    }

    // -- Побитовые -------------------------------------------------------------

    #[test]
    fn bitwise_operations() {
        assert_eq!(
            apply_binary(BinOp::BitwiseAnd, &int(6), &int(3)),
            Ok(int(2))
        );
        assert_eq!(apply_binary(BinOp::BitwiseOr, &int(6), &int(3)), Ok(int(7)));
        assert_eq!(
            apply_binary(BinOp::BitwiseXor, &int(6), &int(3)),
            Ok(int(5))
        );
    }

    #[test]
    fn bitwise_on_real_is_error() {
        assert!(matches!(
            apply_binary(BinOp::BitwiseAnd, &real(1.0), &int(3)),
            Err(EvalError::TypeMismatch { .. })
        ));
    }

    // -- Логические ------------------------------------------------------------

    #[test]
    fn logical_and_or() {
        let t = Value::Boolean(true);
        let f = Value::Boolean(false);
        assert_eq!(apply_binary(BinOp::LogicalAnd, &t, &f), Ok(f.clone()));
        assert_eq!(apply_binary(BinOp::LogicalOr, &t, &f), Ok(t.clone()));
    }

    #[test]
    fn logical_treats_nonzero_int_as_true() {
        assert_eq!(
            apply_binary(BinOp::LogicalAnd, &int(2), &int(1)),
            Ok(Value::Boolean(true))
        );
    }

    // -- Сравнения -------------------------------------------------------------

    #[test]
    fn comparisons_on_integers() {
        assert_eq!(
            apply_binary(BinOp::Less, &int(1), &int(2)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_binary(BinOp::More, &int(6), &int(2)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_binary(BinOp::LessEqual, &int(2), &int(2)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_binary(BinOp::MoreEqual, &int(1), &int(2)),
            Ok(Value::Boolean(false))
        );
    }

    #[test]
    fn equality_on_numbers_and_bools() {
        assert_eq!(
            apply_binary(BinOp::Equal, &int(2), &int(2)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_binary(BinOp::NotEqual, &int(2), &int(3)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            apply_binary(BinOp::Equal, &Value::Boolean(true), &Value::Boolean(true)),
            Ok(Value::Boolean(true))
        );
    }

    #[test]
    fn equality_int_and_real_coerces() {
        assert_eq!(
            apply_binary(BinOp::Equal, &int(2), &real(2.0)),
            Ok(Value::Boolean(true))
        );
    }

    // -- S8: bool не смешивается с числом молча --------------------------------

    #[test]
    fn s8_bool_plus_int_is_error_not_silent_coercion() {
        assert!(matches!(
            apply_binary(BinOp::Add, &Value::Boolean(true), &int(1)),
            Err(EvalError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn s8_bool_equal_int_is_error_not_false() {
        // Контрпример к дефекту Д8: ошибка типов не должна выглядеть как "не равно".
        assert!(matches!(
            apply_binary(BinOp::Equal, &Value::Boolean(true), &int(1)),
            Err(EvalError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn array_comparison_is_error_not_false() {
        assert!(matches!(
            apply_binary(BinOp::Equal, &Value::Array(vec![]), &int(1)),
            Err(EvalError::TypeMismatch { .. })
        ));
    }

    // -- Переполнение носителя -------------------------------------------------

    #[test]
    fn carrier_overflow_is_error_not_wrap() {
        assert!(matches!(
            apply_binary(BinOp::Add, &int(i128::MAX), &int(1)),
            Err(EvalError::ArithmeticOverflow { .. })
        ));
    }

    // -- Унарные ---------------------------------------------------------------

    #[test]
    fn unary_negate_and_plus() {
        assert_eq!(apply_unary(UnOp::Negate, &int(5)), Ok(int(-5)));
        assert_eq!(apply_unary(UnOp::Negate, &real(0.5)), Ok(real(-0.5)));
        assert_eq!(apply_unary(UnOp::UnaryPlus, &int(5)), Ok(int(5)));
    }

    #[test]
    fn unary_not_and_bitwise_not() {
        assert_eq!(
            apply_unary(UnOp::Not, &Value::Boolean(true)),
            Ok(Value::Boolean(false))
        );
        assert_eq!(apply_unary(UnOp::Not, &int(0)), Ok(Value::Boolean(true)));
        assert_eq!(apply_unary(UnOp::BitwiseNot, &int(0)), Ok(int(-1)));
    }

    #[test]
    fn unary_bitwise_not_on_real_is_error() {
        assert!(matches!(
            apply_unary(UnOp::BitwiseNot, &real(1.0)),
            Err(EvalError::TypeMismatch { .. })
        ));
    }

    // -- to_bool ---------------------------------------------------------------

    #[test]
    fn to_bool_rules() {
        assert_eq!(to_bool(&Value::Boolean(true)), Ok(true));
        assert_eq!(to_bool(&int(0)), Ok(false));
        assert_eq!(to_bool(&int(7)), Ok(true));
    }

    #[test]
    fn to_bool_on_real_is_error() {
        assert!(matches!(
            to_bool(&real(1.0)),
            Err(EvalError::TypeMismatch { .. })
        ));
    }
}

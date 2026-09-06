//! Арифметика длительности в симуляторе.
//!
//! Устроено по образцу [`fixed`](super::fixed): значение самоописательно
//! (`Value::Duration` несёт наносекунды), поэтому семантика времени живёт
//! **здесь**, а не размазана по общей числовой ветке
//! [`apply_binary`](super::ops::apply_binary).
//!
//! ## Правила
//!
//! - `duration ± duration` -> `duration`;
//! - сравнения `duration` с `duration` -> логическое;
//! - **всё остальное с участием длительности - ошибка типов.**
//!
//! Последнее - не строгость ради строгости: компилятор запрещает смешение (`SE-065`), и
//! если бы симулятор считал `t + 1`, эталон оказался бы "умнее" языка, а расхождение
//! вылезло бы на целях. Умножение длительности на целое языком разрешено, но правило
//! вывода типа для него введёт та задача, которая его реализует; ошибиться в сторону
//! запрета безопасно, в сторону разрешения - нет.
//!
//! Эталон меряет время **наносекундами**, а не единицами профиля: профиль
//! ("часы"/"такты") - свойство генерации, ему нужен целый счётчик, а модели - нет.

use crate::eval::error::{EvalError, value_kind};
use crate::eval::ops::BinOp;
use crate::eval::value::Value;

/// Обрабатывает бинарную операцию, если хоть один операнд - длительность.
///
/// Вызывается из [`apply_binary`](super::ops::apply_binary) **до** числовой ветки - так
/// же, как перехватывается `Fixed`.
pub(crate) fn binary(op: BinOp, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    let mismatch = || EvalError::TypeMismatch {
        op: op.symbol(),
        lhs: value_kind(lhs),
        rhs: Some(value_kind(rhs)),
    };
    let (Value::Duration(a), Value::Duration(b)) = (lhs, rhs) else {
        // Длительность с чем угодно другим не сочетается - включая число.
        return Err(mismatch());
    };
    match op {
        BinOp::Add => a
            .checked_add(*b)
            .map(Value::Duration)
            .ok_or(EvalError::ArithmeticOverflow { op: op.symbol() }),
        BinOp::Subtract => a
            .checked_sub(*b)
            .map(Value::Duration)
            .ok_or(EvalError::ArithmeticOverflow { op: op.symbol() }),
        BinOp::Less => Ok(Value::Boolean(a < b)),
        BinOp::More => Ok(Value::Boolean(a > b)),
        BinOp::LessEqual => Ok(Value::Boolean(a <= b)),
        BinOp::MoreEqual => Ok(Value::Boolean(a >= b)),
        BinOp::Equal => Ok(Value::Boolean(a == b)),
        BinOp::NotEqual => Ok(Value::Boolean(a != b)),
        // Умножение, деление, остаток, степень, сдвиги, битовые и логические: над
        // длительностью не определены (см. шапку модуля).
        BinOp::Multiply
        | BinOp::Divide
        | BinOp::Modulo
        | BinOp::Power
        | BinOp::ShiftLeft
        | BinOp::ShiftRight
        | BinOp::BitwiseAnd
        | BinOp::BitwiseOr
        | BinOp::BitwiseXor
        | BinOp::LogicalAnd
        | BinOp::LogicalOr => Err(mismatch()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SECOND: i64 = 1_000_000_000;

    #[test]
    fn duration_arithmetic_stays_duration() {
        assert_eq!(
            binary(BinOp::Add, &Value::Duration(SECOND), &Value::Duration(500)),
            Ok(Value::Duration(SECOND + 500))
        );
        assert_eq!(
            binary(
                BinOp::Subtract,
                &Value::Duration(SECOND),
                &Value::Duration(1)
            ),
            Ok(Value::Duration(SECOND - 1))
        );
    }

    #[test]
    fn comparisons_yield_boolean() {
        assert_eq!(
            binary(BinOp::Less, &Value::Duration(1), &Value::Duration(2)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            binary(BinOp::MoreEqual, &Value::Duration(2), &Value::Duration(2)),
            Ok(Value::Boolean(true))
        );
        assert_eq!(
            binary(BinOp::Equal, &Value::Duration(1), &Value::Duration(2)),
            Ok(Value::Boolean(false))
        );
    }

    #[test]
    fn mixing_with_number_is_type_error() {
        // Эталон не должен быть "умнее" языка: компилятор это запрещает (SE-065).
        let error = binary(BinOp::Add, &Value::Duration(SECOND), &Value::Number(1))
            .expect_err("смешение обязано быть ошибкой");
        assert!(matches!(error, EvalError::TypeMismatch { .. }), "{error:?}");
        let error = binary(
            BinOp::Multiply,
            &Value::Duration(SECOND),
            &Value::Duration(2),
        )
        .expect_err("умножение длительностей не определено");
        assert!(matches!(error, EvalError::TypeMismatch { .. }), "{error:?}");
    }

    #[test]
    fn overflow_is_reported_not_wrapped() {
        let error = binary(BinOp::Add, &Value::Duration(i64::MAX), &Value::Duration(1))
            .expect_err("переполнение обязано быть ошибкой");
        assert!(
            matches!(error, EvalError::ArithmeticOverflow { .. }),
            "{error:?}"
        );
    }
}

//! Встроенные функции языка в эталоне.
//!
//! Цели переводят `min`/`max`/`abs`/`clamp` тернарником над **теми же** операциями, что
//! и операторы языка: цель `c` печатает `(((a) < (b)) ? (a) : (b))`, `sv` - то же,
//! `rust` - `a.min(b)`. Эталон до этой фичи не считал их вовсе (`SIM-020`), то есть
//! модель со встроенной функцией нельзя было сверить с целью ни на одном такте.
//!
//! **Своей арифметики здесь нет и быть не должно.** Выбор идёт через
//! [`ops::apply_binary`] и [`ops::apply_unary`] - тот же слой, что исполняет `<` и
//! унарный минус в теле модели. Второй экземпляр правил сравнения разъехался бы с
//! первым: этот класс стоил проекту исправлений (перенос q считался по ширине хранения) и
//! (сравнение `bit` в цели `rust`). Отсюда же бесплатно берутся q-формат, насыщение и
//! обёртка: `abs` над `q(m, n) sat` прижимает край ровно так же, как унарный минус.
//!
//! `debug` сюда не входит: его аргумент - строковый литерал, а `Value` строк не
//! представляет, поэтому он перехватывается **до** вычисления аргументов
//! (`crate::expression`). `S(Модель)` в позиции выражения не переводит ни одна цель -
//! эталон тоже отказывает.

use crate::eval::error::EvalError;
use crate::eval::ops::{self, BinOp, UnOp};
use crate::eval::value::Value;
use takt_lang::diagnostics::lang::keys;

/// Вычисляет встроенную функцию по имени и уже вычисленным аргументам.
///
/// `Ok(None)` - имя не из вычислимых (`debug`, `S`): решение принимает вызывающий,
/// потому что у них не значение, а побочный эффект либо отказ.
pub(crate) fn apply(name: &str, args: &[Value]) -> Result<Option<Value>, EvalError> {
    let value = match (name, args) {
        ("min", [a, b]) => pick_smaller(a, b)?,
        ("max", [a, b]) => pick_larger(a, b)?,
        ("abs", [x]) => absolute(x)?,
        // Порядок границ - как у цели `c`: сперва нижняя, потом верхняя. При `lo > hi`
        // это даёт `lo`, и эталон обязан вести себя так же, как прошивка, а не
        // "правильнее" её.
        ("clamp", [x, lo, hi]) => {
            let low = pick_larger(x, lo)?;
            pick_smaller(&low, hi)?
        }
        _ => return Ok(None),
    };
    Ok(Some(value))
}

/// Меньшее из двух: `a < b ? a : b` - форма цели `c`, включая поведение при равенстве
/// (берётся первый).
fn pick_smaller(a: &Value, b: &Value) -> Result<Value, EvalError> {
    Ok(if is_less(a, b)? { a.clone() } else { b.clone() })
}

/// Большее из двух в той же форме: `a < b ? b : a`.
fn pick_larger(a: &Value, b: &Value) -> Result<Value, EvalError> {
    Ok(if is_less(a, b)? { b.clone() } else { a.clone() })
}

/// `x < 0 ? -x : x` - ровно то, что печатают цели `c` и `sv`.
fn absolute(x: &Value) -> Result<Value, EvalError> {
    let zero = zero_like(x);
    if is_less(x, &zero)? {
        ops::apply_unary(UnOp::Negate, x)
    } else {
        Ok(x.clone())
    }
}

/// Ноль в формате самого значения: у `q(m, n)` нулём должно быть `Fixed` того же
/// формата, иначе сравнение уйдёт в смешение типов.
fn zero_like(value: &Value) -> Value {
    match value {
        Value::Fixed { m, n, sat, .. } => Value::Fixed {
            repr: 0,
            m: *m,
            n: *n,
            sat: *sat,
        },
        Value::Real(_) => Value::Real(0.0),
        Value::Duration(_) => Value::Duration(0),
        Value::Number(_) | Value::Boolean(_) | Value::Array(_) | Value::Struct { .. } => {
            Value::Number(0)
        }
    }
}

fn is_less(a: &Value, b: &Value) -> Result<bool, EvalError> {
    match ops::apply_binary(BinOp::Less, a, b)? {
        Value::Boolean(flag) => Ok(flag),
        Value::Number(_)
        | Value::Real(_)
        | Value::Fixed { .. }
        | Value::Duration(_)
        | Value::Array(_)
        | Value::Struct { .. } => Err(EvalError::TypeMismatch {
            op: "<",
            lhs: keys::SIM_KIND_VALUE,
            rhs: None,
        }),
    }
}

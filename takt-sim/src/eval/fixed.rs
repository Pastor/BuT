//! Эталонная Q-арифметика fixed-point `q(m, n)`.
//!
//! Нормативные правила - обязаны совпасть **побитово** с целями (`c`/`rust`/`st`/`sv`),
//! иначе сверка проваливается by design:
//!
//! - `+`/`−` - сложение представлений, **wraparound** к `W = m + n` бит (как у
//!   целых языка; насыщение - вне объёма, кандидат);
//! - `*` - точное произведение шириной `2W` (в `i128`), затем **арифметический**
//!   сдвиг вправо на `n` - округление **floor к −∞** (; самый дешёвый в
//!   аппаратуре и однозначный). Проверять на **отрицательных**: на
//!   положительных floor и trunc совпадают и дефект был бы невидим.
//! - `/` - делимое расширяется до `2W`, сдвигается влево на `n`, затем
//!   **целочисленное** деление (усечение к нулю, как в C - );
//! - приведения только явные (): `int`/`float` ↔ `q` масштабируют на 2ⁿ.
//!
//! Значение самоописательно ([`Value::Fixed`] несёт `m`/`n`), поэтому тип в операцию
//! протаскивать не нужно - семантика живёт **здесь**, а не в адаптерах.

use crate::eval::error::{EvalError, value_kind};
use crate::eval::ops::BinOp;
use crate::eval::value::Value;

/// Приводит `v` к знаковому `intW` в дополнительном коде (wraparound).
///
/// Само правило живёт в **общем носителе**
/// (`takt_lang::semantic::const_eval::fixed_repr`): компилятор сворачивает дробное
/// приведение теми же формулами, и вторая реализация разошлась бы с этой значениями.
pub(crate) fn wrap(v: i128, w: u8) -> i64 {
    takt_lang::semantic::const_eval::fixed_repr::wrap(v, w)
}

/// Пересчитывает представление между дробными разрядностями: влево - точно, вправо -
/// **арифметический** сдвиг (floor к −∞).
fn rescale(repr: i128, from_n: u8, to_n: u8) -> i128 {
    if to_n >= from_n {
        repr << (to_n - from_n)
    } else {
        repr >> (from_n - to_n)
    }
}

/// Прижимает значение к границам представления `intW`.
///
/// Границы - представления, а не "удобные" числа: `[−2^(W−1), 2^(W−1) − 1]`. Проверять
/// обязательно **обе**: на положительных прижатие и перенос дают разные значения, но
/// ошибиться знаком легко, а край `−(−2^(W−1))` вне тестов не встречается вовсе.
pub(crate) fn saturate(v: i128, w: u8) -> i64 {
    takt_lang::semantic::const_eval::fixed_repr::saturate(v, w)
}

/// Значение `Value::Fixed` из "сырого" (возможно, переполненного) представления.
///
/// Здесь - **единственная** точка, где решается судьба переполнения: перенос или
/// насыщение. Все операции идут через неё, поэтому разойтись между собой они не могут.
fn make(repr: i128, m: u8, n: u8, sat: bool) -> Value {
    Value::Fixed {
        repr: takt_lang::semantic::const_eval::fixed_repr::normalize(repr, m, n, sat),
        m,
        n,
        sat,
    }
}

/// Ошибка "операция не определена для этих операндов" (не тихий неверный ответ).
fn mismatch(op: BinOp, lhs: &Value, rhs: &Value) -> EvalError {
    EvalError::TypeMismatch {
        op: op.symbol(),
        lhs: value_kind(lhs),
        rhs: Some(value_kind(rhs)),
    }
}

/// Q-арифметика бинарной операции, когда хотя бы один операнд - [`Value::Fixed`].
///
/// Оба операнда обязаны быть `q` **одного формата** (гарантирует страж `SE-059`); иначе -
/// [`EvalError::TypeMismatch`], а не молчаливо неверный результат.
pub(crate) fn binary(op: BinOp, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    // **Целое рядом с q в сравнении - это представление**. Семантика понижает литерал в
    // представление везде, где известен тип приёмника, и `gain > 1.0` доезжает сюда как
    // `Fixed > Number(256)`. Цели печатают ровно это (`model->gain > 256`), и отказ
    // здесь развёл бы эталон с ними на записи, которую они исполняют.
    //
    // Только сравнение: в арифметике смешение q и целого отвергает `SE-059` - вход до
    // эталона не доходит, и послабление здесь означало бы "эталон умнее компилятора".
    let (lhs, rhs) = match (lhs, rhs, op.is_comparison()) {
        (Value::Fixed { m, n, sat, .. }, Value::Number(k), true) => (
            lhs.clone(),
            Value::Fixed {
                repr: *k as i64,
                m: *m,
                n: *n,
                sat: *sat,
            },
        ),
        (Value::Number(k), Value::Fixed { m, n, sat, .. }, true) => (
            Value::Fixed {
                repr: *k as i64,
                m: *m,
                n: *n,
                sat: *sat,
            },
            rhs.clone(),
        ),
        _ => (lhs.clone(), rhs.clone()),
    };
    let (lhs, rhs) = (&lhs, &rhs);
    let (
        Value::Fixed {
            repr: ra,
            m,
            n,
            sat,
        },
        Value::Fixed {
            repr: rb,
            m: m2,
            n: n2,
            sat: sat2,
        },
    ) = (lhs, rhs)
    else {
        return Err(mismatch(op, lhs, rhs));
    };
    // Признак насыщения входит в формат: `q(8,8)` и `q(8,8) sat` - Разные типы.
    // Совпадения требует компилятор (`SE-059`), а здесь - отказ вместо молчаливо
    // неверного результата: иначе выражение унаследовало бы семантику переполнения
    // случайного операнда.
    if m != m2 || n != n2 || sat != sat2 {
        return Err(mismatch(op, lhs, rhs));
    }
    let (m, n, sat) = (*m, *n, *sat);
    let (a, b) = (*ra as i128, *rb as i128);
    match op {
        BinOp::Add => Ok(make(a + b, m, n, sat)),
        BinOp::Subtract => Ok(make(a - b, m, n, sat)),
        // Точное произведение 2W -> сдвиг на n вправо, floor к −∞ (арифм. сдвиг).
        BinOp::Multiply => Ok(make((a * b) >> n, m, n, sat)),
        // Делимое <- n влево, затем целочисленное деление (усечение к нулю).
        BinOp::Divide => {
            if b == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(make((a << n) / b, m, n, sat))
        }
        BinOp::Less => Ok(Value::Boolean(a < b)),
        BinOp::More => Ok(Value::Boolean(a > b)),
        BinOp::LessEqual => Ok(Value::Boolean(a <= b)),
        BinOp::MoreEqual => Ok(Value::Boolean(a >= b)),
        BinOp::Equal => Ok(Value::Boolean(a == b)),
        BinOp::NotEqual => Ok(Value::Boolean(a != b)),
        // Не определены для q: остаток, степень, сдвиги, битовые, логические.
        BinOp::Modulo
        | BinOp::Power
        | BinOp::ShiftLeft
        | BinOp::ShiftRight
        | BinOp::BitwiseAnd
        | BinOp::BitwiseOr
        | BinOp::BitwiseXor
        | BinOp::LogicalAnd
        | BinOp::LogicalOr => Err(mismatch(op, lhs, rhs)),
    }
}

/// Унарный минус: `−(repr)` с переносом либо насыщением (значение остаётся `q`).
///
/// Край `−(−2^(W−1))` - единственное место, где перенос и насыщение расходятся на
/// унарной операции: перенос вернёт **то же** самое отрицательное значение, насыщение -
/// максимум. На "обычных" числах разницы не видно.
pub(crate) fn negate(repr: i64, m: u8, n: u8, sat: bool) -> Value {
    make(-(repr as i128), m, n, sat)
}

/// `expr as q(m, n)` (узел `Cast`): **масштабирует** значение к представлению ().
/// `int`/`bool` умножаются на 2ⁿ (сдвиг влево), `float` - floor от `f · 2ⁿ`, `q(m₂,
/// n₂)` - пересчитывается на разницу дробных разрядов.
pub(crate) fn cast_to_fixed(value: &Value, m: u8, n: u8, sat: bool) -> Result<Value, EvalError> {
    let repr: i128 = match value {
        Value::Number(i) => *i << n,
        Value::Boolean(b) => (i64::from(*b) as i128) << n,
        Value::Real(f) => (f * pow2(n)).floor() as i128,
        Value::Fixed { repr, n: n2, .. } => rescale(*repr as i128, *n2, n),
        Value::Array(_) | Value::Struct { .. } | Value::Duration(_) => {
            return Err(EvalError::NotCoercible {
                value: value_kind(value),
                ty: format!("q({m}, {n})"),
            });
        }
    };
    Ok(make(repr, m, n, sat))
}

/// Целая часть `q(m, n)` (floor): `repr >> n` - для `q as int`/`q as bit`.
pub(crate) fn to_integer_part(repr: i64, n: u8) -> i64 {
    repr >> n
}

/// Вещественное значение `q(m, n)`: `repr / 2ⁿ` - для `q as float`.
pub(crate) fn to_real(repr: i64, n: u8) -> f64 {
    repr as f64 / pow2(n)
}

/// `2ⁿ` как `f64` (n <= 63 по построению типа).
fn pow2(n: u8) -> f64 {
    (1u64 << n) as f64
}

#[cfg(test)]
mod tests {
    use super::*;

    fn q(repr: i64, m: u8, n: u8) -> Value {
        Value::Fixed {
            repr,
            m,
            n,
            sat: false,
        }
    }

    /// Насыщающий формат `q(m, n) sat`.
    fn qs(repr: i64, m: u8, n: u8) -> Value {
        Value::Fixed {
            repr,
            m,
            n,
            sat: true,
        }
    }

    /// T4: 1.5 в q(8, 8) - представление 384; сложение представлений.
    #[test]
    fn add_representations() {
        // 1.5 + 0.5 = 2.0 -> 384 + 128 = 512
        assert_eq!(
            binary(BinOp::Add, &q(384, 8, 8), &q(128, 8, 8)),
            Ok(q(512, 8, 8))
        );
    }

    /// T9 (ключевой): −1.5 · 2.0 в q(8, 8) -> −3.0 (−768). Округление floor к −∞
    /// проверяется именно на **отрицательном** - на положительных floor и trunc
    /// совпадают, и дефект был бы невидим.
    #[test]
    fn multiply_negative_is_floor() {
        // −1.5 -> −384, 2.0 -> 512; (−384·512) >> 8 = −196608 >> 8 = −768 = −3.0
        assert_eq!(
            binary(BinOp::Multiply, &q(-384, 8, 8), &q(512, 8, 8)),
            Ok(q(-768, 8, 8))
        );
    }

    /// Произведение, дающее дробный хвост, округляется к −∞ (не к нулю).
    #[test]
    fn multiply_floor_rounds_down_for_negative() {
        // 0.5 · (−0.00390625) = −0.001953...; репрезентации 128 · (−1) = −128;
        // −128 >> 8 = −1 (floor), тогда как усечение к нулю дало бы 0.
        assert_eq!(
            binary(BinOp::Multiply, &q(128, 8, 8), &q(-1, 8, 8)),
            Ok(q(-1, 8, 8))
        );
    }

    /// T19: переполнение `+` - wraparound (перенос), а не насыщение.
    #[test]
    fn add_overflow_wraps() {
        // q(8, 8): max repr = 32767 (127.996...). 32767 + 1 = 32768 -> −32768.
        assert_eq!(
            binary(BinOp::Add, &q(32767, 8, 8), &q(1, 8, 8)),
            Ok(q(-32768, 8, 8))
        );
    }

    /// Деление: 3.0 / 2.0 в q(8, 8) -> 1.5 (384). (768 << 8) / 512 = 384.
    #[test]
    fn divide_scales() {
        assert_eq!(
            binary(BinOp::Divide, &q(768, 8, 8), &q(512, 8, 8)),
            Ok(q(384, 8, 8))
        );
    }

    /// Деление на ноль - ошибка, а не паника.
    #[test]
    fn divide_by_zero_is_error() {
        assert_eq!(
            binary(BinOp::Divide, &q(384, 8, 8), &q(0, 8, 8)),
            Err(EvalError::DivisionByZero)
        );
    }

    /// Сравнение q идёт по представлениям (тот же формат).
    #[test]
    fn compare_representations() {
        assert_eq!(
            binary(BinOp::Less, &q(384, 8, 8), &q(512, 8, 8)),
            Ok(Value::Boolean(true))
        );
    }

    /// Смешение форматов на уровне значений - ошибка (страж на случай, если SE-059
    /// обошли).
    #[test]
    fn mixed_formats_is_error() {
        assert!(binary(BinOp::Add, &q(384, 8, 8), &q(384, 4, 4)).is_err());
    }

    /// Приведение int -> q(8, 8) масштабирует: 3 -> 3.0 = 768.
    #[test]
    fn cast_int_scales() {
        assert_eq!(
            cast_to_fixed(&Value::Number(3), 8, 8, false),
            Ok(q(768, 8, 8))
        );
    }

    /// Приведение q -> целая часть (floor): 1.5 (384) -> 1.
    #[test]
    fn cast_fixed_to_integer_part_floors() {
        assert_eq!(to_integer_part(384, 8), 1);
        assert_eq!(to_integer_part(-384, 8), -2); // −1.5 -> floor = −2
    }

    /// Приведение q -> float: 1.5 (384) -> 1.5.
    #[test]
    fn cast_fixed_to_real() {
        assert!((to_real(384, 8) - 1.5).abs() < 1e-9);
    }

    /// wrap на границе W=64 не паникует и даёт корректный i64.
    #[test]
    fn wrap_width_64() {
        assert_eq!(wrap(i64::MAX as i128, 64), i64::MAX);
        assert_eq!(wrap(i64::MIN as i128, 64), i64::MIN);
    }

    // -- Насыщение q(m, n) sat -------------------------------------
    //
    // Каждая операция проверяется на обеих границах: на положительных прижатие и
    // перенос дают разные значения, но ошибиться знаком легко.

    /// Верхняя граница `+`: перенос переворачивает, насыщение прижимает.
    #[test]
    fn saturating_add_clamps_at_upper_bound() {
        // q(4, 4): W = 8, repr ∈ [−128, 127]. 127 + 16 = 143 > 127.
        assert_eq!(
            binary(BinOp::Add, &qs(127, 4, 4), &qs(16, 4, 4)),
            Ok(qs(127, 4, 4)),
            "насыщение обязано прижать к максимуму"
        );
        assert_eq!(
            binary(BinOp::Add, &q(127, 4, 4), &q(16, 4, 4)),
            Ok(q(-113, 4, 4)),
            "перенос обязан остаться прежним (умолчание не меняется)"
        );
    }

    /// Нижняя граница `−`: та же проверка с другого края.
    #[test]
    fn saturating_sub_clamps_at_lower_bound() {
        assert_eq!(
            binary(BinOp::Subtract, &qs(-128, 4, 4), &qs(16, 4, 4)),
            Ok(qs(-128, 4, 4))
        );
        assert_eq!(
            binary(BinOp::Subtract, &q(-128, 4, 4), &q(16, 4, 4)),
            Ok(q(112, 4, 4))
        );
    }

    /// `*` насыщается после сдвига: округление не меняется.
    #[test]
    fn saturating_mul_clamps_after_shift() {
        // 4.0 · 4.0 = 16.0, вне q(4, 4) (максимум ≈ 7.94) -> прижатие.
        assert_eq!(
            binary(BinOp::Multiply, &qs(64, 4, 4), &qs(64, 4, 4)),
            Ok(qs(127, 4, 4))
        );
        // Отрицательное произведение прижимается к нижней границе.
        assert_eq!(
            binary(BinOp::Multiply, &qs(-64, 4, 4), &qs(64, 4, 4)),
            Ok(qs(-128, 4, 4))
        );
    }

    /// Ключевой край: `−(−2^(W−1))`. Перенос возвращает то же значение, насыщение -
    /// максимум. На "обычных" числах разницы не видно вовсе.
    #[test]
    fn saturating_negate_of_minimum_is_maximum() {
        assert_eq!(negate(-128, 4, 4, true), qs(127, 4, 4));
        assert_eq!(negate(-128, 4, 4, false), q(-128, 4, 4));
    }

    /// Приведение `int -> q sat` тоже прижимает: масштабирование выводит из диапазона
    /// так же легко, как арифметика.
    #[test]
    fn saturating_cast_from_int_clamps() {
        // 100 · 2⁴ = 1600 ≫ 127.
        assert_eq!(
            cast_to_fixed(&Value::Number(100), 4, 4, true),
            Ok(qs(127, 4, 4))
        );
        assert_eq!(
            cast_to_fixed(&Value::Number(-100), 4, 4, true),
            Ok(qs(-128, 4, 4))
        );
    }

    /// Смешение форматов по признаку - отказ, а не молчаливый выбор семантики.
    #[test]
    fn mixing_saturating_and_wrapping_is_type_mismatch() {
        assert!(
            binary(BinOp::Add, &qs(1, 4, 4), &q(1, 4, 4)).is_err(),
            "sat и не-sat — разные форматы; их смешение обязано отвергаться"
        );
    }

    /// В диапазоне насыщение не меняет ничего: прижатие срабатывает только на выходе за
    /// границы.
    #[test]
    fn saturating_is_transparent_inside_range() {
        assert_eq!(
            binary(BinOp::Add, &qs(16, 4, 4), &qs(16, 4, 4)),
            Ok(qs(32, 4, 4))
        );
    }
}

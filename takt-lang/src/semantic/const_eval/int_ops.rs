//! Целочисленные операции константного вычисления - **одна** таблица.
//!
//! # Что здесь есть и чего здесь нет
//!
//! Здесь - **только** таблица операций над целыми: результат либо число, либо булево.
//! Здесь **нет** диагностик: их текст и код принадлежат вызывающему (у адреса -
//! `SE-055` со своей формулировкой, у общего вычислителя - `SE-083`), и сводить их к
//! одному значило бы менять наблюдаемое поведение ради внутренней опрятности.
//!
//! Здесь **нет** и носителя: вычисление идёт в `i128`, а сужение к своему типу делает
//! вызывающий. Для адреса это `as i64` - то же самое, что прежняя обёртка `wrapping_*`
//! по 64 битам (`i64::MAX + 1` в `i128` даёт `2⁶³`, а `as i64` - `i64::MIN`).
//!
//! **Третьего дубля нет.** Вычислитель константной выдержки
//! (`semantic/condition/after_const.rs`) считает **наносекунды**
//! `checked_add`/`checked_sub` и отвечает `Cause::Overflow` - у него другая семантика
//! (проверяемая, а не обёртка) и другой домен (длительность). Загонять его сюда значило
//! бы навязать длительности обёртку целого.

/// Результат операции: целое либо булево (сравнения и логика).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IntOutcome {
    /// Числовой результат.
    Int(i128),
    /// Результат сравнения или логической операции.
    Bool(bool),
}

/// Почему операция не выполнена. Текст и код диагностики - за вызывающим.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IntOpError {
    /// Деление на ноль.
    DivisionByZero,
    /// Остаток от деления на ноль.
    RemainderByZero,
    /// Сдвиг на отрицательное число либо на 64 бита и более.
    ///
    /// В Rust такой сдвиг - **паника**, поэтому граница проверяется, а не обрезается: у
    /// автора это всегда опечатка.
    ShiftOutOfRange,
    /// Показатель степени отрицателен либо шире `u32`.
    ///
    /// Отрицательная степень даёт **дробное** значение (у цели `rust` она уже
    /// отвергается `RS-011`), а `wrapping_pow` принимает `u32`. Обрезать показатель
    /// нельзя: это молча дало бы другое число.
    ExponentOutOfRange,
    /// Операции с таким знаком в константном вычислении нет.
    UnsupportedOperator,
}

/// Применяет бинарную операцию к целым операндам.
///
/// Переполнение - обёртка (`wrapping_*`): такова норма для беззнаковых величин. Сужение
/// результата к своему носителю - задача вызывающего.
pub(crate) fn int_binary(op: &str, a: i128, b: i128) -> Result<IntOutcome, IntOpError> {
    use IntOutcome as O;
    let value = match op {
        "+" => O::Int(a.wrapping_add(b)),
        "-" => O::Int(a.wrapping_sub(b)),
        "*" => O::Int(a.wrapping_mul(b)),
        "/" => {
            if b == 0 {
                return Err(IntOpError::DivisionByZero);
            }
            O::Int(a.wrapping_div(b))
        }
        "%" => {
            if b == 0 {
                return Err(IntOpError::RemainderByZero);
            }
            O::Int(a.wrapping_rem(b))
        }
        "<<" | ">>" => {
            if !(0..64).contains(&b) {
                return Err(IntOpError::ShiftOutOfRange);
            }
            if op == "<<" {
                O::Int(a << b)
            } else {
                O::Int(a >> b)
            }
        }
        // Целая степень: счёт целыми, обёртка - как у всей таблицы. `wrapping_pow` даёт
        // **ровно** то, что печатает цель `rust` и считает хелпер `takt_ipow` цели `c`.
        "**" => {
            let Ok(exp) = u32::try_from(b) else {
                return Err(IntOpError::ExponentOutOfRange);
            };
            O::Int(a.wrapping_pow(exp))
        }
        "&" => O::Int(a & b),
        "|" => O::Int(a | b),
        "^" => O::Int(a ^ b),
        "=" => O::Bool(a == b),
        "!=" => O::Bool(a != b),
        "<" => O::Bool(a < b),
        "<=" => O::Bool(a <= b),
        ">" => O::Bool(a > b),
        ">=" => O::Bool(a >= b),
        "&&" => O::Bool(a != 0 && b != 0),
        "||" => O::Bool(a != 0 || b != 0),
        _ => return Err(IntOpError::UnsupportedOperator),
    };
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Обёртка при переполнении - та же норма, что у арифметики языка.
    #[test]
    fn overflow_wraps() {
        assert_eq!(
            int_binary("+", i128::MAX, 1),
            Ok(IntOutcome::Int(i128::MIN))
        );
    }

    /// Сужение к `i64` после вычисления в `i128` даёт ту же обёртку, что `wrapping_add`
    /// по 64 битам, - на этом стоит замена в выражении адреса.
    #[test]
    fn narrowing_to_i64_matches_64bit_wrapping() {
        let Ok(IntOutcome::Int(wide)) = int_binary("+", i128::from(i64::MAX), 1) else {
            panic!("сложение обязано дать число");
        };
        assert_eq!(wide as i64, i64::MAX.wrapping_add(1));
    }

    /// Деление и остаток на ноль - отказ, а не паника.
    #[test]
    fn division_by_zero_is_refused() {
        assert_eq!(int_binary("/", 1, 0), Err(IntOpError::DivisionByZero));
        assert_eq!(int_binary("%", 1, 0), Err(IntOpError::RemainderByZero));
    }

    /// Граница сдвига проверяется с обеих сторон.
    #[test]
    fn shift_range_is_checked() {
        assert_eq!(int_binary("<<", 1, -1), Err(IntOpError::ShiftOutOfRange));
        assert_eq!(int_binary("<<", 1, 64), Err(IntOpError::ShiftOutOfRange));
        assert_eq!(int_binary("<<", 1, 63), Ok(IntOutcome::Int(1i128 << 63)));
    }

    /// Сравнение даёт булево - по этому признаку выражение адреса отличает операции,
    /// которых у него нет.
    #[test]
    fn comparison_yields_bool() {
        assert_eq!(int_binary("<", 1, 2), Ok(IntOutcome::Bool(true)));
    }

    /// Неизвестный знак - отказ, а не молчаливый ноль.
    #[test]
    fn unknown_operator_is_refused() {
        assert_eq!(int_binary("@", 2, 3), Err(IntOpError::UnsupportedOperator));
    }

    /// Целая степень считается таблицей: знака, которого в ней нет, свёртка не считает,
    /// и `const SPAN: u16 := 2 ** 8;` теряет значение - молча у эталона и цели `st`,
    /// отказом у `c` и `sv`.
    #[test]
    fn integer_power_is_computed() {
        assert_eq!(int_binary("**", 2, 8), Ok(IntOutcome::Int(256)));
        assert_eq!(int_binary("**", 3, 0), Ok(IntOutcome::Int(1)));
        assert_eq!(int_binary("**", -2, 3), Ok(IntOutcome::Int(-8)));
    }

    /// Показатель вне `u32` - отказ, а не обрезание: обрезав, получили бы другое число
    /// молча.
    #[test]
    fn exponent_out_of_range_is_refused() {
        assert_eq!(int_binary("**", 2, -1), Err(IntOpError::ExponentOutOfRange));
        assert_eq!(
            int_binary("**", 2, i128::from(u32::MAX) + 1),
            Err(IntOpError::ExponentOutOfRange)
        );
    }

    /// Переполнение носителя - обёртка, как у прочей арифметики таблицы.
    #[test]
    fn power_overflow_wraps() {
        assert_eq!(int_binary("**", 2, 127), Ok(IntOutcome::Int(i128::MIN)));
    }
}

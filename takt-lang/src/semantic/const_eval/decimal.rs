//! Точная десятичная арифметика над дробными литералами.
//!
//! # Представление
//!
//! Пара "мантисса `i128` + число знаков после запятой". `12.34` -> `(1234, 2)`.
//! Переполнение - отказ, а не обёртка: свёрнутое значение обязано быть
//! **тем же**, что вычислил бы эталон, а обёртка сделала бы его другим.

/// Точное десятичное число: `mantissa * 10^-scale`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct Decimal {
    mantissa: i128,
    scale: u32,
}

/// Предел масштаба: столько знаков после запятой ещё умещается в `i128` вместе с целой
/// частью реальных моделей. Выше - отказ, а не потеря точности.
const MAX_SCALE: u32 = 30;

impl Decimal {
    /// Разбирает текст литерала (`"12.34"`) со знаком, заданным отдельно.
    ///
    /// Текст приходит из АСД в том виде, в каком его написал автор, - лексер уже
    /// проверил форму, поэтому неожиданные символы означают дефект разбора, а не ошибку
    /// пользователя, и дают `None`.
    pub(super) fn parse(text: &str, negative: bool) -> Option<Self> {
        let (int_part, frac_part) = match text.split_once('.') {
            Some((i, f)) => (i, f),
            None => (text, ""),
        };
        if !int_part.bytes().all(|b| b.is_ascii_digit())
            || !frac_part.bytes().all(|b| b.is_ascii_digit())
        {
            return None;
        }
        let scale = u32::try_from(frac_part.len()).ok()?;
        if scale > MAX_SCALE {
            return None;
        }
        let digits: String = format!("{int_part}{frac_part}");
        let mantissa: i128 = digits.parse().ok()?;
        Some(Self {
            mantissa: if negative { -mantissa } else { mantissa },
            scale,
        })
    }

    /// Целое как точное десятичное: масштаб нулевой.
    ///
    /// Нужно для смешанной записи `1 + 3.14`: она точна ровно так же, как `1.0 + 3.14`,
    /// и отвергать её значило бы наказывать за форму записи.
    pub(super) fn from_int(value: i128) -> Self {
        Self {
            mantissa: value,
            scale: 0,
        }
    }

    /// Мантисса и масштаб: значение есть `mantissa · 10⁻ˢᶜᵃˡᵉ`.
    ///
    /// Нужны приведению к `q(m, n)`: масштабирование считается
    /// **точно**, в `i128`, а не через `f64` - иначе в записи автора появилась
    /// бы ошибка, которой в ней нет.
    pub(super) fn parts(self) -> (i128, u32) {
        (self.mantissa, self.scale)
    }

    /// Печатает обратно в текст и знак - в том виде, в каком их принимает АСД.
    ///
    /// Дробная часть сохраняется **всегда**, даже нулевая: литерал без точки есть
    /// целое, а подмена вида числа изменила бы вывод типов.
    pub(super) fn to_text(self) -> (String, bool) {
        let negative = self.mantissa < 0;
        let digits = self.mantissa.unsigned_abs().to_string();
        let scale = self.scale as usize;
        let text = if scale == 0 {
            format!("{digits}.0")
        } else if digits.len() > scale {
            let (int_part, frac_part) = digits.split_at(digits.len() - scale);
            format!("{int_part}.{frac_part}")
        } else {
            format!("0.{}{digits}", "0".repeat(scale - digits.len()))
        };
        (text, negative)
    }

    /// Приводит два числа к общему масштабу.
    fn align(self, other: Self) -> Option<(i128, i128, u32)> {
        let scale = self.scale.max(other.scale);
        if scale > MAX_SCALE {
            return None;
        }
        let lift = |v: Self| -> Option<i128> {
            let factor = 10_i128.checked_pow(scale - v.scale)?;
            v.mantissa.checked_mul(factor)
        };
        Some((lift(self)?, lift(other)?, scale))
    }

    pub(super) fn add(self, other: Self) -> Option<Self> {
        let (a, b, scale) = self.align(other)?;
        Some(Self {
            mantissa: a.checked_add(b)?,
            scale,
        })
    }

    pub(super) fn sub(self, other: Self) -> Option<Self> {
        let (a, b, scale) = self.align(other)?;
        Some(Self {
            mantissa: a.checked_sub(b)?,
            scale,
        })
    }

    pub(super) fn mul(self, other: Self) -> Option<Self> {
        let scale = self.scale.checked_add(other.scale)?;
        if scale > MAX_SCALE {
            return None;
        }
        Some(Self {
            mantissa: self.mantissa.checked_mul(other.mantissa)?,
            scale,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn text(t: &str, neg: bool) -> Decimal {
        Decimal::parse(t, neg).expect("литерал разбирается")
    }

    #[test]
    fn one_plus_two_is_exactly_three() {
        let sum = text("1.0", false).add(text("2.0", false)).expect("сумма");
        assert_eq!(sum.to_text(), ("3.0".to_string(), false));
    }

    /// Разные масштабы выравниваются, а не теряются.
    #[test]
    fn scales_are_aligned() {
        let sum = text("1.5", false).add(text("0.25", false)).expect("сумма");
        assert_eq!(sum.to_text(), ("1.75".to_string(), false));
    }

    /// Умножение точное: масштабы складываются.
    #[test]
    fn multiplication_is_exact() {
        let product = text("1.5", false)
            .mul(text("1.5", false))
            .expect("произведение");
        assert_eq!(product.to_text(), ("2.25".to_string(), false));
    }

    /// Знак переносится, а результат ниже нуля печатается со знаком отдельно.
    #[test]
    fn subtraction_crosses_zero() {
        let diff = text("1.0", false)
            .sub(text("2.5", false))
            .expect("разность");
        assert_eq!(diff.to_text(), ("1.5".to_string(), true));
    }

    /// Смешанная запись точна так же, как чисто дробная.
    #[test]
    fn integer_operand_is_exact_too() {
        let sum = Decimal::from_int(1)
            .add(text("3.14", false))
            .expect("сумма");
        assert_eq!(sum.to_text(), ("4.14".to_string(), false));
    }

    /// Значение меньше единицы печатается с ведущими нулями.
    #[test]
    fn small_values_keep_leading_zeros() {
        let product = text("0.1", false)
            .mul(text("0.1", false))
            .expect("произведение");
        assert_eq!(product.to_text(), ("0.01".to_string(), false));
    }

    /// Целая запись без точки остаётся дробной на выходе: подмена вида числа изменила
    /// бы вывод типов.
    #[test]
    fn result_stays_fractional() {
        let sum = text("1.5", false).add(text("1.5", false)).expect("сумма");
        assert_eq!(sum.to_text(), ("3.0".to_string(), false));
    }

    /// Переполнение - отказ, а не обёртка: свёрнутое значение обязано совпасть с тем,
    /// что вычислит эталон.
    #[test]
    fn overflow_refuses() {
        let huge = text("170141183460469231731687303715.0", false);
        assert!(
            huge.mul(huge).is_none(),
            "переполнение обязано быть отказом"
        );
    }

    /// Масштаб за пределом - отказ, а не молчаливое округление.
    #[test]
    fn excessive_scale_refuses() {
        assert!(Decimal::parse(&format!("0.{}", "1".repeat(31)), false).is_none());
    }
}

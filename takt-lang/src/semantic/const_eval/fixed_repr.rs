//! Представление `q(m, n)`: нормализация и масштабирование - общий носитель.

/// Приводит значение к знаковому `intW` в дополнительном коде (перенос).
pub fn wrap(value: i128, w: u8) -> i64 {
    let modulo = 1i128 << w; // 2^W (W <= 64 гарантирован построением типа)
    let masked = value.rem_euclid(modulo); // 0..2^W
    let signed = if masked >= (modulo >> 1) {
        masked - modulo
    } else {
        masked
    };
    signed as i64
}

/// Прижимает значение к границам представления `intW`.
///
/// Границы - представления, а не "удобные" числа: `[−2^(W−1), 2^(W−1) − 1]`. Проверять
/// обязательно **обе**: на положительных прижатие и перенос дают разные значения, а
/// край `−(−2^(W−1))` вне тестов не встречается вовсе.
pub fn saturate(value: i128, w: u8) -> i64 {
    let max = (1i128 << (w - 1)) - 1;
    let min = -(1i128 << (w - 1));
    value.clamp(min, max) as i64
}

/// Значение представления из "сырого" (возможно, переполненного).
///
/// Здесь - **единственная** точка, где решается судьба переполнения: перенос или
/// насыщение. Все операции идут через неё, поэтому разойтись между собой они не могут.
pub fn normalize(repr: i128, m: u8, n: u8, sat: bool) -> i64 {
    let w = m + n;
    if sat {
        saturate(repr, w)
    } else {
        wrap(repr, w)
    }
}

/// Представление целого в формате `q(m, n)`: сдвиг влево на `n`.
pub fn from_int(value: i128, n: u8) -> i128 {
    value << n
}

/// Представление точной десятичной дроби `mantissa · 10⁻ˢ` в формате `q(m, n)` -
/// **floor к −∞**, как у эталона.
///
/// Округление именно floor, а не "к ближайшему": выбрано потому, что floor - самый
/// дешёвый в аппаратуре и однозначный. Разойтись с ним значит разойтись со всеми пятью
/// целями сразу.
///
/// Считается **точно** (в `i128`), а не через `f64`: у эталона на входе `Value::Real`,
/// здесь - текст литерала, и приводить его к двоичной плавающей точке значило бы
/// вносить ошибку, которой в исходной записи нет.
///
/// `None` - если промежуточное произведение не умещается в `i128`.
pub fn from_decimal(mantissa: i128, scale: u32, n: u8) -> Option<i128> {
    let numerator = mantissa.checked_mul(1i128 << n)?;
    let denominator = 10i128.checked_pow(scale)?;
    // `div_euclid` округляет к −∞ для положительного делителя - это и есть floor.
    Some(numerator.div_euclid(denominator))
}

/// Представление литерала, записанного текстом десятичной дроби.
///
/// Узкий вход для **целей**: печатник видит `ExpressionNode::Rational(текст, знак)` и
/// не имеет доступа к разбору десятичной записи, который живёт внутри константного
/// вычислителя. Счёт - тот же [`from_decimal`], поэтому второго знания о floor и
/// масштабе 2ⁿ не заводится.
pub fn from_decimal_text(text: &str, negative: bool, n: u8) -> Option<i128> {
    let decimal = super::decimal::Decimal::parse(text, negative)?;
    let (mantissa, scale) = decimal.parts();
    from_decimal(mantissa, scale, n)
}

/// Текст десятичной дроби `repr · 2⁻ⁿ` и знак - точный, без потерь.
///
/// Знаменатель - степень двойки, поэтому дробь **конечна** в десятичной записи: `repr ·
/// 5ⁿ / 10ⁿ`. Нужен свёртке: результат приведения возвращается литералом и понижается
/// штатным путём, а не отдельной веткой.
pub fn to_decimal_text(repr: i64, n: u8) -> (String, bool) {
    let negative = repr < 0;
    let magnitude = i128::from(repr).unsigned_abs();
    let scaled = magnitude * 5u128.pow(u32::from(n)); // repr·5ⁿ
    let text = format!("{scaled}");
    let n = usize::from(n);
    let padded = if text.len() <= n {
        format!("{}{}", "0".repeat(n - text.len() + 1), text)
    } else {
        text
    };
    let (int_part, frac_part) = padded.split_at(padded.len() - n);
    let frac_part = if frac_part.is_empty() { "0" } else { frac_part };
    (format!("{int_part}.{frac_part}"), negative)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Перенос и насыщение расходятся ровно там, где значение не влезает.
    #[test]
    fn wrap_and_saturate_differ_on_overflow() {
        assert_eq!(wrap(200, 8), -56);
        assert_eq!(saturate(200, 8), 127);
        assert_eq!(wrap(100, 8), 100);
        assert_eq!(saturate(100, 8), 100);
        // Край, который вне тестов не встречается.
        assert_eq!(saturate(128, 8), 127);
        assert_eq!(saturate(-129, 8), -128);
    }

    /// Целое масштабируется сдвигом: `3 as q(4, 4)` - это 48.
    #[test]
    fn integer_scales_by_shift() {
        assert_eq!(from_int(3, 4), 48);
        assert_eq!(from_int(-3, 4), -48);
    }

    /// Дробь округляется **floor к −∞**, и на отрицательных это видно.
    ///
    /// На положительных floor и усечение совпадают - дефект был бы невидим.
    #[test]
    fn decimal_floors_towards_minus_infinity() {
        assert_eq!(from_decimal(15, 1, 4), Some(24)); // 1.5 -> 24
        assert_eq!(from_decimal(11, 1, 4), Some(17)); // 1.1 -> floor(17.6) = 17
        assert_eq!(from_decimal(-11, 1, 4), Some(-18)); // −1.1 -> floor(−17.6) = −18
    }

    /// Обратный текст точен: знаменатель - степень двойки.
    #[test]
    fn text_is_exact() {
        assert_eq!(to_decimal_text(24, 4), (String::from("1.5000"), false));
        assert_eq!(to_decimal_text(17, 4), (String::from("1.0625"), false));
        assert_eq!(to_decimal_text(-18, 4), (String::from("1.1250"), true));
        assert_eq!(to_decimal_text(0, 4), (String::from("0.0000"), false));
    }
}

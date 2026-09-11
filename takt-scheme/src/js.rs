//! Числа так, как их считает и печатает холст страницы.
//!
//! Геометрия схемы существует в двух языках: холст на JS и чертёж здесь. Картинка
//! обязана совпадать с холстом, а правила округления у языков разные: `Math.round`
//! округляет половину вверх, `f64::round` - от нуля; `String(-0)` печатает `0`, а
//! `format!` - `-0`. Отличия собраны здесь, и вся геометрия зовёт только их.

/// `Math.round`: ближайшее целое, половина - вверх.
pub fn round(value: f64) -> f64 {
    (value + 0.5).floor()
}

/// Координата с точностью до десятой, без минус-нуля.
pub fn tenth(value: f64) -> f64 {
    round(value * 10.0) / 10.0 + 0.0
}

/// Округление до миллионной - так холст сравнивает направления.
pub fn millionth(value: f64) -> f64 {
    round(value * 1e6) / 1e6
}

/// `Math.sign`: у нуля - ноль.
pub fn sign(value: f64) -> f64 {
    if value > 0.0 {
        1.0
    } else if value < 0.0 {
        -1.0
    } else {
        0.0
    }
}

/// Число в записи пути SVG - как `String(число)` у холста: кратчайшая запись,
/// целое без дробной части, минус-ноль - ноль.
pub fn num(value: f64) -> String {
    if value == 0.0 {
        return "0".to_string();
    }
    format!("{value}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rounding_is_that_of_the_page() {
        assert_eq!(round(2.5), 3.0);
        assert_eq!(round(-2.5), -2.0, "половина - вверх, а не от нуля");
        assert_eq!(tenth(-0.04), 0.0);
        assert!(tenth(-0.04).is_sign_positive(), "минус-ноль не рождается");
        assert_eq!(sign(0.0), 0.0);
    }

    #[test]
    fn numbers_are_printed_as_the_page_prints_them() {
        assert_eq!(num(12.0), "12");
        assert_eq!(num(12.5), "12.5");
        assert_eq!(num(-0.0), "0");
        assert_eq!(num(0.1 + 0.2), "0.30000000000000004");
    }
}

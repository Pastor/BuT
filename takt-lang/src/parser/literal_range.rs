//! Границы числового литерала и восстановление после выхода за них.
//!
//! Тема отделена от лексера намеренно: **диапазон литерала - свойство системы типов
//! языка, а не техника сканирования**. Приём задан объединением самого широкого
//! знакового (`i64`) и самого широкого беззнакового (`u64`) типов; носитель токена
//! (`i128`) шире него, чтобы константное вычисление имело запас над границей приёма.
//! Возьми носитель границей - и маска `0xFFFFFFFFFFFFFFFF` для типа `[bit;64]` стала бы
//! невыразимой.

use crate::diagnostics::Location;
use crate::parser::lexer::{LexicalError, Spanned};
use crate::parser::token::Token;

/// Наибольшее значение числового литерала: `u64::MAX`.
///
/// Потолок задан **типами языка**: `u64` - самый широкий беззнаковый тип, и записать
/// больше просто некуда.
pub(crate) const LITERAL_MAX: i128 = u64::MAX as i128;

/// Наименьшее значение числового литерала: `i64::MIN`.
///
/// Дно задаёт самый широкий знаковый тип языка - `i64`. Расширение потолка до
/// `u64::MAX` дна **не опускает**: у отрицательного значения беззнаковых типов нет.
pub(crate) const LITERAL_MIN: i128 = i64::MIN as i128;

/// `LE-009` для литерала `text`, занимающего `[start, end]` в файле `file_no`.
pub(crate) fn out_of_range(file_no: u64, start: usize, end: usize, text: &str) -> LexicalError {
    LexicalError::NumberOutOfRange(Location::source(file_no, start, end + 1), text.to_string())
}

/// Восстановление после литерала вне диапазона: токен-заглушка.
///
/// `LE-009` уже записан в список ошибок, компиляция всё равно провалится - но
/// **исчезнувший** токен превращал одну причину в две: парсер спотыкался о
/// следующий символ и добавлял `SY-002` ("нераспознанный токен ';'"), уводя
/// автора от настоящей ошибки. Заглушка держит форму разбора, чтобы дальше
/// нашлись **другие** ошибки, а не эхо этой.
///
/// Значение заглушки (`0`) до вывода дойти не может: наличие лексической ошибки
/// прерывает конвейер до генерации.
pub(crate) fn recover_number<'input>(error: &LexicalError) -> Option<Spanned<'input>> {
    match error {
        LexicalError::NumberOutOfRange(Location::Source(_, start, end), _) => {
            Some((*start as usize, Token::Number(0), *end as usize))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Границы приёма - это границы **типов**, а не носителя.
    #[test]
    fn bounds_are_language_types_not_carrier() {
        assert_eq!(LITERAL_MAX, i128::from(u64::MAX));
        assert_eq!(LITERAL_MIN, i128::from(i64::MIN));
        const {
            assert!(LITERAL_MAX < i128::MAX, "носитель обязан быть шире приёма")
        };
    }

    /// Заглушка выдаётся ровно на `LE-009` и сохраняет позиции литерала.
    #[test]
    fn recovery_only_for_out_of_range() {
        let loc = Location::Source(0, 7, 12);
        let err = LexicalError::NumberOutOfRange(loc, "1e30".to_string());
        assert_eq!(recover_number(&err), Some((7, Token::Number(0), 12)));

        let other = LexicalError::MissingExponent(loc);
        assert_eq!(recover_number(&other), None);
    }
}

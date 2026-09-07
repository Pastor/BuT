//! Ошибки, которые парсер отдаёт как `ParseError::User`: тип [`LexicalError`], их
//! позиции и коды.
//!
//! Отделены от сканера ([`crate::parser::lexer`]) по границе ответственности: перечень
//! того, **что может пойти не так**, читают диагностика, LSP и тесты - им сканер не
//! нужен, а модуль лексера и без того у предела размера.
//!
//! Имя типа говорит о лексике, а содержимое шире: сюда же попадают **правила языка,
//! проверяемые действием грамматики**. Причина техническая: `ParseError::User`
//! параметризован **одним** типом ошибки, и завести второй, не переписав сигнатуры всех
//! точек разбора, нельзя. Код диагностики при этом честный - `SY-`, а не `LE-`.

use crate::diagnostics::Location;
use thiserror::Error;

/// Ошибка лексического анализатора.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[allow(missing_docs)]
pub enum LexicalError {
    /// Неожиданный конец файла внутри блочного комментария.
    #[error("неожиданный конец файла внутри комментария")]
    EndOfFileInComment(Location),

    /// Неожиданный конец файла внутри строкового литерала.
    #[error("неожиданный конец файла внутри строкового литерала")]
    EndOfFileInString(Location),

    /// Неожиданный конец файла внутри шестнадцатеричного литерала.
    #[error("неожиданный конец файла внутри шестнадцатеричного литерала")]
    EndOfFileInHex(Location),

    /// Отсутствуют цифры после `0x`.
    #[error("отсутствует число после '0x'")]
    MissingNumber(Location),

    /// Недопустимый символ в шестнадцатеричном литерале.
    #[error("недопустимый символ '{1}' в шестнадцатеричном литерале")]
    InvalidCharacterInHexLiteral(Location, char),

    /// Неизвестный токен.
    #[error("нераспознанный токен '{1}'")]
    UnrecognisedToken(Location, String),

    /// Отсутствует показатель степени после `e`/`E`.
    #[error("отсутствует показатель степени")]
    MissingExponent(Location),

    /// Ожидалось ключевое слово `from`, но встретилось другое слово.
    #[error("ожидалось ключевое слово 'from', но найдено '{1}'")]
    ExpectedFrom(Location, String),

    /// Числовой литерал не помещается ни в один целочисленный тип языка.
    ///
    /// Приём и его обоснование - [`crate::parser::literal_range`]. Здесь важно одно:
    /// диапазон проверяется против **типов языка**, а влезает ли литерал в конкретный
    /// тип приёмника - это уже `SE-089` на семантике.
    #[error(
        "числовой литерал '{1}' вне диапазона [-9223372036854775808, 18446744073709551615]: \
         не помещается ни в один целочисленный тип языка"
    )]
    NumberOutOfRange(Location, String),

    /// Литерал длительности/частоты вне представимого диапазона.
    ///
    /// Длительность хранится в наносекундах (`i64`, ±292 года), частота - в герцах
    /// (`u64`). Молчаливой обёртки здесь быть не должно: выдержка, обернувшаяся при
    /// разборе, стала бы другой выдержкой.
    #[error("литерал времени '{1}' вне представимого диапазона")]
    TimeLiteralOutOfRange(Location, String),

    /// Оператор-выражение без эффекта: `x + 1;`, `#0x100.4;`.
    ///
    /// Оператором остаются **присваивание** и **вызов функции**: у них есть действие.
    /// Прочее выражение в позиции оператора - вычисление, результат которого некуда
    /// деть.
    ///
    /// Проверка стоит в **грамматике**, а не в семантике: этого требовал,
    /// и тот же приём уже применён к позиции присваивания.
    #[error(
        "выражение в позиции оператора не имеет эффекта: оператором может быть \
         присваивание ('цель := значение;') либо вызов функции ('f(x);'). \
         Вычисленное значение здесь некуда деть"
    )]
    StatementWithoutEffect(Location),

    /// Единица времени стоит после формы, которая её не допускает.
    ///
    /// Длительность записывается **целым** десятичным числом с единицей: `1.5s`,
    /// `1e3ms` и `0xFFms` отвергаются здесь, а не оставляются "числом и
    /// идентификатором" - иначе автор получил бы `SY-002` про неведомый токен вместо
    /// указания на настоящую причину. Дробная длительность выражается меньшей единицей
    /// (`1500ms`).
    #[error(
        "недопустимый литерал времени '{1}': единица допустима только у целого десятичного числа"
    )]
    InvalidTimeLiteral(Location, String),
}

impl LexicalError {
    /// Возвращает местоположение в исходном тексте, где возникла ошибка.
    pub fn loc(&self) -> Location {
        match self {
            LexicalError::EndOfFileInComment(loc) => *loc,
            LexicalError::EndOfFileInString(loc) => *loc,
            LexicalError::EndOfFileInHex(loc) => *loc,
            LexicalError::MissingNumber(loc) => *loc,
            LexicalError::InvalidCharacterInHexLiteral(loc, _) => *loc,
            LexicalError::UnrecognisedToken(loc, _) => *loc,
            LexicalError::MissingExponent(loc) => *loc,
            LexicalError::ExpectedFrom(loc, _) => *loc,
            LexicalError::NumberOutOfRange(loc, _) => *loc,
            LexicalError::TimeLiteralOutOfRange(loc, _) => *loc,
            LexicalError::InvalidTimeLiteral(loc, _) => *loc,
            LexicalError::StatementWithoutEffect(loc) => *loc,
        }
    }

    /// Возвращает код ошибки в формате `LE-NNN`.
    pub fn code(&self) -> &'static str {
        match self {
            LexicalError::EndOfFileInComment(_) => "LE-001",
            LexicalError::EndOfFileInString(_) => "LE-002",
            LexicalError::EndOfFileInHex(_) => "LE-003",
            LexicalError::MissingNumber(_) => "LE-004",
            LexicalError::InvalidCharacterInHexLiteral(_, _) => "LE-005",
            LexicalError::UnrecognisedToken(_, _) => "LE-006",
            LexicalError::MissingExponent(_) => "LE-007",
            LexicalError::ExpectedFrom(_, _) => "LE-008",
            LexicalError::NumberOutOfRange(_, _) => "LE-009",
            LexicalError::TimeLiteralOutOfRange(_, _) => "LE-010",
            LexicalError::InvalidTimeLiteral(_, _) => "LE-011",
            // Коды `SY-`: это правила синтаксиса, а не лексики (см. заголовок).
            LexicalError::StatementWithoutEffect(_) => "SY-007",
        }
    }
}

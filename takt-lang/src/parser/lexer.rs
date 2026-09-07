//! Лексический анализатор языка Takt.
//!
//! Этот модуль реализует ручной лексер для языка Takt на базе итератора [`CharIndices`]
//! с односимвольным предпросмотром через [`PeekNth`].
//!
//! ## Основные типы
//!
//! - [`Token`] - перечисление всех лексем языка (ключевые слова, операторы,
//!   литералы, идентификаторы и прагмы).
//! - [`Lexer`] - итератор, преобразующий исходный текст в поток [`Spanned`]-токенов.
//! - [`LexicalError`] - ошибки, обнаруживаемые на этапе лексического анализа.
//!
//! ## Особенности
//!
//! - Ключевые слова (`if`, `while`, `model`, `state`, ...) определены в статической
//!   таблице [`KEYWORDS`] и распознаются при сканировании идентификаторов.
//! - Числовые литералы поддерживают десятичную и шестнадцатеричную (`0x`) запись,
//!   а также числа с плавающей точкой и экспоненту.
//! - Строковые литералы заключаются в двойные кавычки `"..."` и допускают
//!   конкатенацию смежных строк.
//! - Комментарии: однострочные `//`, документационные `///` и блочные `/* */`.

use crate::parser::literal_range::{LITERAL_MAX, LITERAL_MIN, out_of_range, recover_number};
use std::str::CharIndices;
use std::str::FromStr;

use itertools::{PeekNth, peek_nth};
use phf::phf_map;
use unicode_xid::UnicodeXID;

use crate::ast::Comment;
use crate::diagnostics::Location;

// Сканирование литералов времени - дочерний модуль: он работает с приватными полями
// `Lexer`, а приватность распространяется на потомков.
#[path = "lexer_time.rs"]
mod lexer_time;

/// Тип "токен с позицией": `(начало, токен, конец)`.
pub type Spanned<'a> = (usize, Token<'a>, usize);

/// Специализированный `Result` для операций лексера.
pub type Result<'a, T = Spanned<'a>, E = LexicalError> = std::result::Result<T, E>;

// `Token` вынесен в `parser::token`. Реэкспорт держит путь `parser::lexer::Token` для
// lalrpop (`use super::parser::lexer::{Token, ...}`) и для потребителей/doc-тестов.
pub use crate::parser::token::Token;

/// Лексический анализатор языка Takt.
///
/// Преобразует строку исходного кода в последовательность токенов ([`Token`]). Для
/// опережающего просмотра используется [`PeekNth`].
///
/// Комментарии (строчные `//`, документационные `///` и блочные `/* */`) собираются
/// отдельно в вектор [`Comment`] и не включаются в поток токенов.
///
/// # Примеры
///
/// ```
/// use takt_lang::parser::lexer::{Lexer, Token};
///
/// let source = "var x := 42;";
/// let mut comments = Vec::new();
/// let mut errors = Vec::new();
/// let mut lexer = Lexer::new(source, 0, &mut comments, &mut errors);
///
/// let mut next_token = || lexer.next().map(|(_, token, _)| token);
/// assert_eq!(next_token(), Some(Token::Variable));
/// assert_eq!(next_token(), Some(Token::Identifier("x")));
/// assert_eq!(next_token(), Some(Token::ColonAssign)); // `:=` - присваивание
/// assert_eq!(next_token(), Some(Token::Number(42i128)));
/// assert_eq!(next_token(), Some(Token::Semicolon));
/// assert_eq!(next_token(), None);
/// assert!(errors.is_empty());
/// assert!(comments.is_empty());
/// ```
#[derive(Debug)]
pub struct Lexer<'input> {
    /// Полный исходный текст.
    input: &'input str,
    /// Итератор по символам с позициями (поддерживает опережающий просмотр).
    chars: PeekNth<CharIndices<'input>>,
    /// Вектор для сохранения найденных комментариев.
    comments: &'input mut Vec<Comment>,
    /// Номер файла (используется в [`Location`]).
    file_no: u64,
    /// Вектор лексических ошибок, обнаруженных в ходе анализа.
    pub errors: &'input mut Vec<LexicalError>,
}

/// Ошибки лексического анализа - [`crate::parser::lex_error`].
///
/// Реэкспорт держит публичный путь `parser::lexer::LexicalError` неизменным: адрес
/// типа для потребителей задаёт этот модуль.
pub use crate::parser::lex_error::LexicalError;

/// Возвращает `true`, если переданная строка является ключевым словом Takt.
pub fn is_keyword(word: &str) -> bool {
    KEYWORDS.contains_key(word)
}

/// Перечисляет все ключевые слова языка Takt - таблицу [`KEYWORDS`].
///
/// Порядок не определён (`phf::Map`): потребитель обязан собирать множество, а не
/// полагаться на последовательность.
///
/// Служит редакторскому слою: список автодополнения LSP обязан покрывать таблицу
/// лексера, и сверяется он настоящими данными, а не грепом исходника, - иначе в проекте
/// заводится второй разборщик `phf_map!`.
pub fn all_keywords() -> impl Iterator<Item = &'static str> {
    KEYWORDS.keys().copied()
}

/// Статическая таблица ключевых слов языка Takt.
///
/// Отображает строку ключевого слова на соответствующий [`Token`].
static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "break"    => Token::Break,
    "const"    => Token::Constant,
    "parameter" => Token::Parameter,
    "continue" => Token::Continue,
    "else"     => Token::Else,
    "false"    => Token::False,
    "for"      => Token::For,
    "fn"       => Token::Function,
    "if"       => Token::If,
    "match"    => Token::Match,
    "_"        => Token::Wildcard,
    "import"   => Token::Import,
    "loop"     => Token::Loop,
    "while"    => Token::While,
    "return"   => Token::Return,
    "true"     => Token::True,
    "type"     => Token::Type,
    "as"       => Token::As,
    "assembly" => Token::Assembly,
    "formula"  => Token::Formula,
    "in"       => Token::PortIn,
    "out"      => Token::PortOut,
    "inout"    => Token::PortInOut,
    "address"  => Token::Address,
    "at"       => Token::At,
    "clock"    => Token::Clock,
    "after"    => Token::After,
    "every"    => Token::Every,
    "model"    => Token::Model,
    "state"    => Token::State,
    "start"    => Token::Start,
    "ref"      => Token::Reference,
    "cond"     => Token::Condition,
    "invariant" => Token::Invariant,
    "var"      => Token::Variable,
    "next"     => Token::Next,
    "extern"   => Token::Extern,
    "enum"     => Token::Enum,
    "struct"   => Token::Struct,
    "from"     => Token::From,
    "X"        => Token::LtlNext,
    "F"        => Token::LtlFinally,
    "G"        => Token::LtlGlobally,
    "U"        => Token::LtlUntil,
    "R"        => Token::LtlRelease,
    "LTL"      => Token::TypeLtl,
    "Guard"    => Token::TypeGuard,
};

impl<'input> Lexer<'input> {
    /// Создаёт новый экземпляр лексического анализатора.
    ///
    /// # Параметры
    ///
    /// - `input` - строка исходного кода для анализа.
    /// - `file_no` - числовой идентификатор файла (используется в [`Location`]).
    /// - `comments` - вектор для сохранения найденных комментариев.
    /// - `errors` - вектор для накопления лексических ошибок.
    ///
    /// # Примеры
    ///
    /// ```
    /// use takt_lang::parser::lexer::Lexer;
    ///
    /// let source = "model M { start S; }";
    /// let mut comments = Vec::new();
    /// let mut errors = Vec::new();
    /// let _lexer = Lexer::new(source, 0, &mut comments, &mut errors);
    /// ```
    pub fn new(
        input: &'input str,
        file_no: u64,
        comments: &'input mut Vec<Comment>,
        errors: &'input mut Vec<LexicalError>,
    ) -> Self {
        Lexer {
            input,
            chars: peek_nth(input.char_indices()),
            comments,
            file_no,
            errors,
        }
    }

    /// Читает числовой литерал начиная с символа `ch` в позиции `start`.
    ///
    /// Поддерживает десятичные, шестнадцатеричные (`0x`), рациональные (`3.14`) и числа
    /// с показателем степени (`1e10`).
    fn parse_number(&mut self, mut start: usize, ch: char) -> Result<'input> {
        let mut is_rational = false;
        let mut is_minus = false;
        if ch == '0'
            && let Some((_, 'x')) = self.chars.peek()
        {
            // Шестнадцатеричный литерал: 0x...
            self.chars.next();

            let mut end = match self.chars.next() {
                Some((end, ch)) if ch.is_ascii_hexdigit() => end,
                Some((..)) => {
                    return Err(LexicalError::MissingNumber(Location::source(
                        self.file_no,
                        start,
                        start + 1,
                    )));
                }
                None => {
                    return Err(LexicalError::EndOfFileInHex(Location::source(
                        self.file_no,
                        start,
                        self.input.len(),
                    )));
                }
            };

            while let Some((i, ch)) = self.chars.peek() {
                if !ch.is_ascii_hexdigit() && *ch != '_' {
                    break;
                }
                end = *i;
                self.chars.next();
            }

            // Удаляем разделители `_` перед разбором hex-числа
            let hex_raw = &self.input[start + 2..=end];
            let hex: String = hex_raw.chars().filter(|&c| c != '_').collect();
            // Шестнадцатеричная запись беззнаковая, поэтому потолок ей - `u64`: маска
            // `0xFFFFFFFFFFFFFFFF` выразима.
            let hex_val = match i128::from_str_radix(&hex, 16) {
                Ok(value) if value <= LITERAL_MAX => value,
                _ => {
                    return Err(out_of_range(
                        self.file_no,
                        start,
                        end,
                        &self.input[start..=end],
                    ));
                }
            };

            // Проверяем, является ли это адресным литералом `0xNNNN:bit` (токен
            // AddressLiteral, чтобы избежать LR(1)-конфликта с тернарным `?:`)
            if matches!(self.chars.peek(), Some((_, ':')))
                && matches!(self.chars.peek_nth(1), Some((_, '0'..='9')))
            {
                self.chars.next(); // потребляем ':'
                let mut bit_end = end + 1;
                while let Some((i, ch)) = self.chars.peek() {
                    if ch.is_ascii_digit() {
                        bit_end = *i;
                        self.chars.next();
                    } else {
                        break;
                    }
                }
                return Ok((
                    start,
                    Token::AddressLiteral(&self.input[start..=bit_end]),
                    bit_end + 1,
                ));
            }

            // `0xFFms` - не длительность: единица допустима только у целого десятичного
            // числа.
            self.reject_time_suffix(start, end)?;
            return Ok((start, Token::Number(hex_val), end + 1));
        }

        if ch == '.' {
            // Начало дробной части (например, `.5`)
            is_rational = true;
            start -= 1;
        }
        if ch == '-' {
            is_minus = true;
        }

        let mut end = start;
        while let Some((i, ch)) = self.chars.peek() {
            if !ch.is_ascii_digit() && *ch != '_' {
                break;
            }
            end = *i;
            self.chars.next();
        }
        // Литерал длительности/частоты: единица примыкает к целому десятичному числу.
        // Проверка стоит здесь, до дробной части и экспоненты: тем формам единица не
        // положена, и они дают `LE-011` ниже.
        if !is_rational
            && let Some((token, consumed)) = self.scan_time_literal(start, end, is_minus)?
        {
            return Ok((start, token, end + 1 + consumed));
        }

        // Конец дробной части - граница текста литерала без показателя. Переменные
        // `end_before_rational`/`rational_start` (границы целой и дробной частей)
        // удалены вместе с `let _integer`/`_fraction`, которым они и служили: разбор
        // текста на части делает потребитель (`f64::from_str`, понижение в q), а
        // лексеру он не нужен.
        let mut rational_end = end;

        if let Some((_, '.')) = self.chars.peek()
            && let Some((i, ch)) = self.chars.peek_nth(1)
            && ch.is_ascii_digit()
            && !is_rational
        {
            // Дробная часть: 3.14
            rational_end = *i;
            is_rational = true;
            self.chars.next(); // пропускаем '.'
            while let Some((i, ch)) = self.chars.peek() {
                if !ch.is_ascii_digit() && *ch != '_' {
                    break;
                }
                rational_end = *i;
                end = *i;
                self.chars.next();
            }
        }

        let old_end = end;
        let mut exp_start = end + 1;
        // Показатель есть / он отрицательный.
        let mut has_exponent = false;
        let mut exp_negative = false;

        if let Some((i, 'e' | 'E')) = self.chars.peek() {
            // Показатель степени: 1e10, 2.5E-3
            exp_start = *i + 1;
            has_exponent = true;
            self.chars.next();
            // Опциональный знак минус перед показателем
            while matches!(self.chars.peek(), Some((_, '-'))) {
                exp_negative = true;
                exp_start += 1;
                self.chars.next();
            }
            while let Some((i, ch)) = self.chars.peek() {
                if !ch.is_ascii_digit() && *ch != '_' {
                    break;
                }
                end = *i;
                self.chars.next();
            }

            if exp_start > end {
                return Err(LexicalError::MissingExponent(Location::source(
                    self.file_no,
                    start,
                    self.input.len(),
                )));
            }
        }

        if is_rational {
            // `1.5s` - не длительность: дробная форма выражается меньшей единицей.
            self.reject_time_suffix(start, end)?;
            // Текст рационального литерала хранится как написан, включая показатель -
            // как `2.5` и как длительность `1m30s`: форматтер печатает авторскую форму.
            // Потребители текста готовы: `f64::from_str` понимает показатель, цели
            // `c`/`rust`/`sv` печатают форму как есть, MatIEC принимает строчную `e`
            //
            let text_end = if has_exponent { end } else { rational_end };
            return Ok((
                start,
                Token::RationalNumber(&self.input[start..=text_end], is_minus),
                end + 1,
            ));
        }

        // Отрицательный показатель делает литерал рациональным: `1e-3` - это 0.001,
        // целым числом оно не выражается. Текст отдаётся как написан; `f64::from_str`
        // его понимает.
        if has_exponent && exp_negative {
            return Ok((
                start,
                Token::RationalNumber(&self.input[start..=end], is_minus),
                end + 1,
            ));
        }

        // Удаляем разделители `_` перед разбором десятичного числа
        let n_raw = &self.input[start..=old_end];
        let n_clean: String = n_raw.chars().filter(|&c| c != '_').collect();

        // Текст здесь всегда без знака (минус - отдельный символ, учитывается ниже),
        // поэтому потолок беззнаковый; дно - после смены знака.
        let Ok(mut n) = i128::from_str(&n_clean).map_err(|_| ()).and_then(|value| {
            if value <= LITERAL_MAX {
                Ok(value)
            } else {
                Err(())
            }
        }) else {
            return Err(out_of_range(self.file_no, start, old_end, &n_clean));
        };

        // Показатель без минуса оставляет литерал целым и вычисляется: `1e20` не
        // влезает ни в один тип языка и даёт `LE-009`, а не тихую обёртку.
        if has_exponent {
            let exp_raw = &self.input[exp_start..=end];
            let exp_clean: String = exp_raw.chars().filter(|&c| c != '_').collect();
            let out_of_range = || out_of_range(self.file_no, start, end, &self.input[start..=end]);
            // Показатель шире u32 заведомо выходит за диапазон - считать незачем.
            let exp: u32 = exp_clean.parse().map_err(|_| out_of_range())?;
            for _ in 0..exp {
                n = n.checked_mul(10).ok_or_else(out_of_range)?;
                if n > LITERAL_MAX {
                    return Err(out_of_range());
                }
            }
        }

        if is_minus {
            n = -n;
            // Дно - `i64::MIN`: беззнаковых типов у отрицательного значения нет.
            if n < LITERAL_MIN {
                return Err(out_of_range(
                    self.file_no,
                    start,
                    end,
                    &self.input[start..=end],
                ));
            }
        }
        // Сюда доходит только форма с экспонентой (`1e3ms`): у простого целого единица
        // уже прочитана выше и вернула токен длительности.
        self.reject_time_suffix(start, end)?;
        Ok((start, Token::Number(n), end + 1))
    }

    /// Читает строковый литерал, заключённый в кавычки `quote_char`.
    ///
    /// - `unicode` - признак Unicode-строки (`unicode"..."`)
    /// - `token_start` - позиция начала токена (включая открывающую кавычку)
    /// - `string_start` - позиция первого символа содержимого строки
    fn string(
        &mut self,
        unicode: bool,
        token_start: usize,
        string_start: usize,
        quote_char: char,
    ) -> Result<'input> {
        let mut end;

        let mut last_was_escape = false;

        loop {
            if let Some((i, ch)) = self.chars.next() {
                end = i;
                if !last_was_escape {
                    if ch == quote_char {
                        break;
                    }
                    last_was_escape = ch == '\\';
                } else {
                    last_was_escape = false;
                }
            } else {
                return Err(LexicalError::EndOfFileInString(Location::source(
                    self.file_no,
                    token_start,
                    self.input.len(),
                )));
            }
        }

        Ok((
            token_start,
            Token::StringLiteral(unicode, &self.input[string_start..end]),
            end + 1,
        ))
    }

    /// Основной цикл токенизации.
    ///
    /// Возвращает следующий токен или `None` при достижении конца файла.
    ///
    /// Имя намеренно **не** `next`: собственный метод с таким именем затеняет метод
    /// трейта [`Iterator`], и вызов `self.next()` внутри `impl Iterator` читается как
    /// бесконечная рекурсия, хотя ею не является. Так этот код и выглядел, пока обёртка
    /// `Iterator::next` обслуживала недостижимый механизм `pragma`.
    fn next_token(&mut self) -> Option<Spanned<'input>> {
        loop {
            match self.chars.next() {
                // Идентификатор или ключевое слово
                Some((start, ch)) if ch == '_' || ch == '$' || UnicodeXID::is_xid_start(ch) => {
                    let (id, end) = self.match_identifier(start);

                    // Специальная обработка Unicode-строк: unicode"..."
                    if id == "unicode" {
                        match self.chars.peek() {
                            Some((_, quote_char @ '"')) | Some((_, quote_char @ '\'')) => {
                                let quote_char = *quote_char;

                                self.chars.next();
                                let str_res = self.string(true, start, start + 8, quote_char);
                                match str_res {
                                    Err(lex_err) => self.errors.push(lex_err),
                                    Ok(val) => return Some(val),
                                }
                            }
                            _ => (),
                        }
                    }

                    return if let Some(w) = KEYWORDS.get(id) {
                        Some((start, *w, end))
                    } else {
                        Some((start, Token::Identifier(id), end))
                    };
                }
                // Строковый литерал в одинарных или двойных кавычках
                Some((start, quote_char @ '"')) | Some((start, quote_char @ '\'')) => {
                    let str_res = self.string(false, start, start + 1, quote_char);
                    match str_res {
                        Err(lex_err) => self.errors.push(lex_err),
                        Ok(val) => return Some(val),
                    }
                }
                // Слэш: деление `/`, начало строчного `//` или блочного `/* */`
                // комментария
                Some((start, '/')) => {
                    match self.chars.peek() {
                        Some((_, '/')) => {
                            // Строчный комментарий
                            self.chars.next();

                            let mut newline = false;

                            let doc_comment = match self.chars.next() {
                                Some((_, '/')) => {
                                    // `////` и далее - не документационный, а обычный
                                    !matches!(self.chars.peek(), Some((_, '/')))
                                }
                                Some((_, ch)) if ch == '\n' || ch == '\r' => {
                                    newline = true;
                                    false
                                }
                                _ => false,
                            };

                            let mut last = start + 3;

                            if !newline {
                                loop {
                                    match self.chars.next() {
                                        None => {
                                            last = self.input.len();
                                            break;
                                        }
                                        Some((offset, '\n' | '\r')) => {
                                            last = offset;
                                            break;
                                        }
                                        Some(_) => (),
                                    }
                                }
                            }

                            if doc_comment {
                                // Документационный комментарий `///`
                                self.comments.push(Comment::DocLine(
                                    Location::source(self.file_no, start, last),
                                    self.input[start..last].to_owned(),
                                ));
                            } else {
                                // Обычный строчный комментарий `//`
                                self.comments.push(Comment::Line(
                                    Location::source(self.file_no, start, last),
                                    self.input[start..last].to_owned(),
                                ));
                            }
                        }
                        Some((_, '*')) => {
                            // Блочный комментарий `/* ... */`
                            self.chars.next(); // потребляем `*`

                            // Сканируем до закрывающей пары `*/`; возвращаем позицию за
                            // `*/`
                            let last = 'scan: {
                                loop {
                                    match self.chars.next() {
                                        None => {
                                            // Неожиданный конец файла внутри
                                            // комментария
                                            self.errors.push(LexicalError::EndOfFileInComment(
                                                Location::source(
                                                    self.file_no,
                                                    start,
                                                    self.input.len(),
                                                ),
                                            ));
                                            return None;
                                        }
                                        Some((offset, '*')) => {
                                            if matches!(self.chars.peek(), Some((_, '/'))) {
                                                self.chars.next(); // потребляем `/`
                                                break 'scan offset + 2;
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            };

                            // Блочный комментарий не производит токены, только
                            // собирается
                            self.comments.push(Comment::Block(
                                Location::source(self.file_no, start, last),
                                self.input[start..last].to_owned(),
                            ));
                        }
                        _ => {
                            return Some((start, Token::Divide, start + 1));
                        }
                    }
                }
                // Цифра - начало числового литерала
                Some((start, ch)) if ch.is_ascii_digit() => {
                    let parse_result = self.parse_number(start, ch);
                    match parse_result {
                        Err(lex_err) => {
                            self.errors.push(lex_err.clone());
                            if matches!(lex_err, LexicalError::EndOfFileInHex(_)) {
                                return None;
                            }
                            if let Some(recovered) = recover_number(&lex_err) {
                                return Some(recovered);
                            }
                        }
                        Ok(parse_result) => return Some(parse_result),
                    }
                }
                Some((i, '#')) => return Some((i, Token::Sharp, i + 1)),
                Some((i, ';')) => return Some((i, Token::Semicolon, i + 1)),
                Some((i, ',')) => return Some((i, Token::Comma, i + 1)),
                Some((i, '(')) => return Some((i, Token::OpenParenthesis, i + 1)),
                Some((i, ')')) => return Some((i, Token::CloseParenthesis, i + 1)),
                Some((i, '{')) => return Some((i, Token::OpenCurlyBrace, i + 1)),
                Some((i, '}')) => return Some((i, Token::CloseCurlyBrace, i + 1)),
                Some((i, '=')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::Equal, i + 2))
                        }
                        Some((_, '>')) => {
                            self.chars.next();
                            Some((i, Token::FatArrow, i + 2))
                        }
                        _ => Some((i, Token::Assign, i + 1)),
                    };
                }
                Some((i, '!')) => {
                    return if let Some((_, '=')) = self.chars.peek() {
                        self.chars.next();
                        Some((i, Token::NotEqual, i + 2))
                    } else {
                        Some((i, Token::Not, i + 1))
                    };
                }
                Some((i, '|')) => {
                    return match self.chars.peek() {
                        Some((_, '|')) => {
                            self.chars.next();
                            Some((i, Token::Or, i + 2))
                        }
                        _ => Some((i, Token::BitwiseOr, i + 1)),
                    };
                }
                Some((i, '^')) => {
                    return Some((i, Token::BitwiseXor, i + 1));
                }
                Some((i, '&')) => {
                    return match self.chars.peek() {
                        Some((_, '&')) => {
                            self.chars.next();
                            Some((i, Token::And, i + 2))
                        }
                        _ => Some((i, Token::BitwiseAnd, i + 1)),
                    };
                }
                Some((i, '+')) => {
                    return Some((i, Token::Add, i + 1));
                }
                Some((i, '-')) => {
                    return match self.chars.peek() {
                        Some((_, '>')) => {
                            self.chars.next();
                            Some((i, Token::Arrow, i + 2))
                        }
                        Some((_, '-')) => {
                            if matches!(self.chars.peek_nth(1), Some((_, '>'))) {
                                self.chars.next(); // потребляем второй `-`
                                self.chars.next(); // потребляем `>`
                                Some((i, Token::PeirceArrow, i + 3))
                            } else {
                                Some((i, Token::Subtract, i + 1))
                            }
                        }
                        Some((_, other)) if other.is_ascii_digit() => {
                            // Отрицательный числовой литерал
                            return match self.parse_number(i + 1, '-') {
                                Err(lex_error) => {
                                    let recovered = recover_number(&lex_error);
                                    self.errors.push(lex_error);
                                    recovered
                                }
                                Ok(parse_result) => Some(parse_result),
                            };
                        }
                        _ => Some((i, Token::Subtract, i + 1)),
                    };
                }
                Some((i, '*')) => {
                    return match self.chars.peek() {
                        Some((_, '*')) => {
                            // Оператор возведения в степень `**`
                            self.chars.next();
                            Some((i, Token::Power, i + 2))
                        }
                        _ => Some((i, Token::Mul, i + 1)),
                    };
                }
                Some((i, '%')) => {
                    return Some((i, Token::Modulo, i + 1));
                }
                Some((i, '<')) => {
                    return match self.chars.peek() {
                        Some((_, '<')) => {
                            self.chars.next();
                            Some((i, Token::ShiftLeft, i + 2))
                        }
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::LessEqual, i + 2))
                        }
                        _ => Some((i, Token::Less, i + 1)),
                    };
                }
                Some((i, '>')) => {
                    return match self.chars.peek() {
                        Some((_, '>')) => {
                            self.chars.next();
                            Some((i, Token::ShiftRight, i + 2))
                        }
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::MoreEqual, i + 2))
                        }
                        _ => Some((i, Token::More, i + 1)),
                    };
                }
                Some((i, '.')) => {
                    return Some((i, Token::Member, i + 1));
                }
                Some((i, '[')) => return Some((i, Token::OpenBracket, i + 1)),
                Some((i, ']')) => return Some((i, Token::CloseBracket, i + 1)),
                Some((i, ':')) => {
                    // `:=` - оператор присваивания (Option B). `::` остаётся двумя
                    // токенами `Colon` (маx. munch не затрагивает, т.к. здесь
                    // склеивается только `:` + `=`).
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::ColonAssign, i + 2))
                        }
                        _ => Some((i, Token::Colon, i + 1)),
                    };
                }
                Some((i, '?')) => return Some((i, Token::Question, i + 1)),
                Some((i, '~')) => return Some((i, Token::BitwiseNot, i + 1)),
                // Пробельные символы игнорируются
                Some((_, ch)) if ch.is_whitespace() => (),
                // Неизвестный символ - лексическая ошибка
                Some((start, _)) => {
                    let mut end;

                    loop {
                        if let Some((i, ch)) = self.chars.next() {
                            end = i;

                            if ch.is_whitespace() {
                                break;
                            }
                        } else {
                            end = self.input.len();
                            break;
                        }
                    }

                    self.errors.push(LexicalError::UnrecognisedToken(
                        Location::source(self.file_no, start, end),
                        self.input[start..end].to_owned(),
                    ));
                }
                None => return None, // Конец файла
            }
        }
    }

    /// Читает идентификатор начиная с позиции `start`.
    ///
    /// Возвращает срез строки идентификатора и позицию его конца.
    fn match_identifier(&mut self, start: usize) -> (&'input str, usize) {
        let end;
        loop {
            if let Some((i, ch)) = self.chars.peek() {
                if !UnicodeXID::is_xid_continue(*ch) && *ch != '$' {
                    end = *i;
                    break;
                }
                self.chars.next();
            } else {
                end = self.input.len();
                break;
            }
        }

        (&self.input[start..end], end)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

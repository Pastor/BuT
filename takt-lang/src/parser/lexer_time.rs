//! Сканирование литералов времени в лексере.
//!
//! Здесь - только выборка текста, потребление символов и диагностика; разбор единиц и
//! составной формы живёт в [`time_literal`](super::time_literal).
//!
//! Модуль **дочерний** по отношению к [`lexer`](super) не по вкусу, а по необходимости:
//! методы работают с приватными полями `Lexer` (`input`, `chars`, `file_no`), а
//! приватность в Rust распространяется на потомков. Вынос вызван лимитом размера модуля
//! (`scripts/check-module-size.sh`): вместе с этим кодом `lexer.rs` давал 1008 строк
//! при лимите 1000 - проверка отказал, и это его работа.

use super::{Lexer, LexicalError, Result, Token};
use crate::diagnostics::Location;
use crate::parser::time_literal;
use std::str::FromStr;

impl<'input> Lexer<'input> {
    /// Читает единицу времени за целым числом `input[start..=end]`.
    ///
    /// `Ok(None)` - единицы нет, литерал числовой (поведение до фичи сохранено). Разбор
    /// единиц и составной формы - в [`time_literal`](super::time_literal); здесь только
    /// выборка текста, потребление символов и диагностика.
    pub(super) fn scan_time_literal(
        &mut self,
        start: usize,
        end: usize,
        is_minus: bool,
    ) -> Result<'input, Option<(Token<'input>, usize)>> {
        let digits: String = self.input[start..=end]
            .chars()
            .filter(char::is_ascii_digit)
            .collect();
        let Ok(value) = i64::from_str(&digits) else {
            return Ok(None);
        };
        let Some(scan) = time_literal::scan_literal(value, &self.input[end + 1..]) else {
            return Ok(None);
        };
        // Текст для диагностики - весь литерал вместе с единицами (`1s30m`), а не одно
        // число: сообщение о "литерале времени '1'" не даёт автору понять, на что
        // именно ругается компилятор.
        let tail_len = self.input[end + 1..]
            .bytes()
            .take_while(u8::is_ascii_alphanumeric)
            .count();
        let text = self.input[start..end + 1 + tail_len].to_string();
        // Отрицательной длительности в языке нет: `-3s` - ошибка, а не "минус три
        // секунды". Знак у выдержки не имеет смысла, а молчаливое отбрасывание дало бы
        // другую выдержку.
        if is_minus {
            return Err(LexicalError::InvalidTimeLiteral(
                Location::source(self.file_no, start, end + 1 + tail_len),
                text,
            ));
        }
        let (literal, consumed) = match scan {
            Ok(parsed) => parsed,
            Err(time_literal::ScanError::OutOfRange) => {
                return Err(LexicalError::TimeLiteralOutOfRange(
                    Location::source(self.file_no, start, end + 1 + tail_len),
                    text,
                ));
            }
            Err(time_literal::ScanError::Order) => {
                return Err(LexicalError::InvalidTimeLiteral(
                    Location::source(self.file_no, start, end + 1 + tail_len),
                    text,
                ));
            }
        };
        for _ in 0..consumed {
            self.chars.next();
        }
        let text = &self.input[start..end + 1 + consumed];
        Ok(Some(match literal {
            time_literal::Literal::Duration(ns) => (Token::Duration(ns, text), consumed),
            time_literal::Literal::Frequency(hz) => (Token::Frequency(hz, text), consumed),
            time_literal::Literal::Ticks(ticks) => (Token::Ticks(ticks, text), consumed),
        }))
    }

    /// Отвергает единицу времени за формой, которая её не допускает (`LE-011`).
    ///
    /// Дробная (`1.5s`), экспоненциальная (`1e3ms`) и шестнадцатеричная (`0xFFms`)
    /// записи единицы не принимают. Без этой проверки автор получил бы `SY-002` про
    /// "нераспознанный токен `ms`" - сообщение о следствии вместо причины.
    pub(super) fn reject_time_suffix(&self, start: usize, end: usize) -> Result<'input, ()> {
        let tail = &self.input[end + 1..];
        if time_literal::scan_suffix(tail).is_some() {
            let len = time_literal::alpha_run(tail);
            return Err(LexicalError::InvalidTimeLiteral(
                Location::source(self.file_no, start, end + 1 + len),
                self.input[start..end + 1 + len].to_string(),
            ));
        }
        Ok(())
    }
}

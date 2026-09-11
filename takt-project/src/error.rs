//! Отказ крейта: что не так и какого рода нарушение.

use std::fmt;

/// Отказ чтения либо проверки проекта.
///
/// Род нужен вызывающему: сервер отвечает на нарушение формы одним кодом, а на
/// превышение предела - другим, и текст сообщения для этого разбирать нельзя.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// Нарушено правило формы: имя файла, версия формата, состав архива.
    Invalid(String),
    /// Превышен предел: сообщение называет и число предела, и факт.
    Limit(String),
    /// Файловая система отказала: путь и причина.
    Io(String),
}

impl Error {
    /// Отказ предела: и число, и факт.
    ///
    /// Оба обязательны. "Слишком большой файл" не говорит, насколько ужиматься, а
    /// "предел 65 536" - было ли превышение на байт или вдесятеро.
    pub fn exceeded(what: &str, limit: impl fmt::Display, fact: impl fmt::Display) -> Self {
        Self::Limit(format!("{what}: предел {limit}, получено {fact}"))
    }

    /// Текст отказа.
    pub fn message(&self) -> &str {
        match self {
            Self::Invalid(text) | Self::Limit(text) | Self::Io(text) => text,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.message())
    }
}

impl std::error::Error for Error {}

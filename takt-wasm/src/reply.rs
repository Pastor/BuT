//! Форма ответа моста: JSON, у которого всегда есть поле `ok`.

use serde::Serialize;
use takt_lang::diagnostics::{Diagnostic, Level, Location};

/// Диагностика в форме, пригодной для страницы.
///
/// Своя структура, а не `Diagnostic` через `serde`: `Diagnostic` - тип компилятора, его
/// поля меняются вместе с ним, и превращать их в контракт страницы значило бы связать
/// вёрстку с внутренним представлением.
#[derive(Debug, Clone, Serialize)]
pub struct DiagnosticJson {
    /// Код (`SE-034`, `CC-023`, ...); `null` - код не проставлен.
    pub code: Option<String>,
    /// Уровень: `"error"` либо `"warning"`.
    pub level: &'static str,
    /// Текст сообщения на языке проекта.
    pub message: String,
    /// Путь файла, если диагностика его знает.
    pub file: Option<String>,
    /// Строка (с единицы) - там, где позиция известна.
    pub line: Option<usize>,
    /// Колонка в символах (в `.takt` есть кириллица), с единицы.
    pub column: Option<usize>,
}

impl DiagnosticJson {
    /// Переводит диагностику компилятора в форму страницы.
    pub fn of(diagnostic: &Diagnostic, source: &str) -> Self {
        let (line, column) = position_of(diagnostic, source);
        Self {
            code: diagnostic.code.clone(),
            level: match diagnostic.level {
                Level::Error => "error",
                Level::Warning => "warning",
                // Отладочные и информационные до пользователя не доходят и в
                // инструментах (их не печатает ни `taktc`, ни `takt-sim`); страница
                // показывает их наравне с информацией.
                Level::Info | Level::Debug => "information",
            },
            message: diagnostic.message.clone(),
            file: diagnostic.file.clone(),
            line,
            column,
        }
    }
}

/// Строка и колонка диагностики в исходнике.
///
/// Считает не мост: правило "строка с единицы, колонка в символах" живёт у диагностики
/// (`takt_lang::diagnostics::line_column`). Своя копия разошлась бы с `taktc` на первой
/// же кириллической строке - и подчёркивание в браузере уехало бы относительно позиции,
/// которую печатает компилятор.
///
/// Текст берётся из памяти, а не с диска: `position_prefix` читает файл по пути, а в
/// браузере файла нет вовсе.
fn position_of(diagnostic: &Diagnostic, source: &str) -> (Option<usize>, Option<usize>) {
    let Location::Source(_, start, _) = diagnostic.loc else {
        return (None, None);
    };
    let (line, column) = takt_lang::diagnostics::line_column(source, start as usize);
    (Some(line), Some(column))
}

/// Успешный ответ с полезной нагрузкой.
pub fn ok<T: Serialize>(payload: T) -> String {
    #[derive(Serialize)]
    struct Ok<T> {
        ok: bool,
        #[serde(flatten)]
        payload: T,
    }
    to_json(&Ok { ok: true, payload })
}

/// Отказ с диагностикой компилятора.
pub fn failed(diagnostic: &Diagnostic, source: &str) -> String {
    #[derive(Serialize)]
    struct Failed<'a> {
        ok: bool,
        error: &'a DiagnosticJson,
    }
    to_json(&Failed {
        ok: false,
        error: &DiagnosticJson::of(diagnostic, source),
    })
}

/// Отказ **вызова**: имя цели не из списка, битые ключи сборки, неизвестный прогон.
///
/// Кода диагностики здесь нет намеренно: это ошибка того, кто зовёт мост, а не
/// программы на Takt. Придумать ей код значило бы поселить в реестре диагностик запись,
/// которую автор модели не может ни вызвать, ни исправить.
pub fn refused(message: impl Into<String>) -> String {
    #[derive(Serialize)]
    struct Refused {
        ok: bool,
        error: RefusedError,
    }
    #[derive(Serialize)]
    struct RefusedError {
        code: Option<String>,
        level: &'static str,
        message: String,
    }
    to_json(&Refused {
        ok: false,
        error: RefusedError {
            code: None,
            level: "error",
            message: message.into(),
        },
    })
}

/// Сериализует ответ.
///
/// Отказ сериализации сюда не доходит по устройству (все поля - строки, числа и
/// списки), но `unwrap` здесь стал бы паникой, а паника в модуле - `abort`: страница
/// теряет модуль целиком. Поэтому запасной ответ - тоже JSON.
fn to_json<T: Serialize>(value: &T) -> String {
    serde_json::to_string(value).unwrap_or_else(|e| {
        format!(
            r#"{{"ok":false,"error":{{"code":null,"level":"error","message":"ответ не сериализуется: {}"}}}}"#,
            e.to_string().replace('"', "'")
        )
    })
}

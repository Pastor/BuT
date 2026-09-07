//! Пределы хранилища проектов.
//!
//! # Откуда числа
//!
//! Не "на глаз", а замером корпуса:
//! крупнейший файл проекта - 21 506 Б, и предел файла втрое больше. Он же
//! совпадает с пределом черновика в браузере и с начальным буфером моста -
//! **один предел на всё**: второй разошёлся бы с первым молча.
//!
//! # Единица - Байты UTF-8
//!
//! Не символы: в `.takt` есть кириллица, и "64 тысячи символов" дали бы вдвое больший
//! файл. Считается то, что ляжет в базу.
//!
//! Превышение - **отказ с названным числом и фактом**, а не усечение. Усечённый
//! исходник выглядит целым и перестаёт компилироваться в месте, которого автор не
//! писал.

use crate::error::ApiError;

/// Наибольший размер одного файла.
pub const FILE_BYTES: usize = 64 * 1024;

/// Наибольшее число файлов в проекте.
pub const FILES_PER_PROJECT: i64 = 32;

/// Наибольший суммарный размер проекта.
pub const PROJECT_BYTES: i64 = 512 * 1024;

/// Наибольшее число проектов у одного владельца.
pub const PROJECTS_PER_USER: i64 = 100;

/// Наибольшая длина имени проекта и имени файла, символов.
pub const NAME_CHARS: usize = 64;

/// Наибольшая длина описания, символов.
pub const DESCRIPTION_CHARS: usize = 512;

/// Наибольшая длина строки ключей сборки, символов.
///
/// Предел нужен раньше разбора: строка уходит в модуль, а разбор чужого ввода без
/// границы - это работа, объём которой задаёт отправитель. Число взято с запасом от
/// самой длинной осмысленной строки ключей (замер m - 11 ключей вместе короче 200
/// символов).
pub const BUILD_ARGS_CHARS: usize = 512;

/// Строит отказ предела: и число, и факт.
///
/// Оба обязательны. "Слишком большой файл" не говорит, насколько ужиматься, а "предел
/// 65 536" не говорит, было ли превышение на байт или вдесятеро.
pub fn exceeded(
    what: &str,
    limit: impl std::fmt::Display,
    fact: impl std::fmt::Display,
) -> ApiError {
    ApiError::LimitExceeded {
        message: format!("{what}: предел {limit}, получено {fact}"),
    }
}

/// Проверяет размер файла.
pub fn check_file(text: &str) -> Result<(), ApiError> {
    let size = text.len();
    if size > FILE_BYTES {
        return Err(exceeded("размер файла в байтах", FILE_BYTES, size));
    }
    Ok(())
}

/// Проверяет имя проекта.
pub fn check_project_name(name: &str) -> Result<(), ApiError> {
    let length = name.chars().count();
    if name.trim().is_empty() {
        return Err(ApiError::BadRequest("имя проекта: пустое".to_string()));
    }
    if length > NAME_CHARS {
        return Err(exceeded(
            "длина имени проекта в символах",
            NAME_CHARS,
            length,
        ));
    }
    Ok(())
}

/// Проверяет описание.
pub fn check_description(text: &str) -> Result<(), ApiError> {
    let length = text.chars().count();
    if length > DESCRIPTION_CHARS {
        return Err(exceeded(
            "длина описания в символах",
            DESCRIPTION_CHARS,
            length,
        ));
    }
    Ok(())
}

/// Проверяет длину строки ключей сборки.
pub fn check_build_args(text: &str) -> Result<(), ApiError> {
    let length = text.chars().count();
    if length > BUILD_ARGS_CHARS {
        return Err(exceeded(
            "длина строки ключей сборки в символах",
            BUILD_ARGS_CHARS,
            length,
        ));
    }
    Ok(())
}

/// Вид файла по расширению.
///
/// Вид выводится из расширения при записи и хранится колонкой: правило одно, и второго
/// носителя у него нет. Переименования файла в сервисе не бывает вовсе (имя - это и
/// есть личность файла), поэтому вид не может разойтись с расширением молча (замер
/// 2026-09-05 - он опровергает опасение проработки).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    /// Модель на Takt.
    Takt,
    /// Сценарий входов И проверок - та же форма, что у файла `-s` эталона (шаги с
    /// `in_ports`, `time_ms`, `extern` и `guard`). Своего формата сервис не заводит:
    /// проверки в нём уже есть.
    Scenario,
    /// Пояснение к проекту на Markdown.
    Markdown,
}

impl Kind {
    /// Имя вида в базе.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Takt => "takt",
            Self::Scenario => "scenario",
            Self::Markdown => "markdown",
        }
    }
}

/// Проверяет имя файла и определяет его вид.
///
/// Алфавит узкий: имя файла становится **именем корневой модели**, а оно попадает в
/// порождённый код - `concat.takt` даёт отказ `iec2c`, пробел или кириллица не пройдут
/// дальше первой цели. Отказать здесь дешевле, чем объяснять потом отказ чужого
/// инструмента.
pub fn check_file_name(name: &str) -> Result<Kind, ApiError> {
    let length = name.chars().count();
    if length > NAME_CHARS {
        return Err(exceeded("длина имени файла в символах", NAME_CHARS, length));
    }
    let kind = if let Some(stem) = name.strip_suffix(".takt") {
        check_stem(stem)?;
        Kind::Takt
    } else if let Some(stem) = name.strip_suffix(".json") {
        check_stem(stem)?;
        Kind::Scenario
    } else if let Some(stem) = name.strip_suffix(".md") {
        // Алфавит тот же, хотя именем модели такой файл не станет: имя попадает в путь
        // архива и в адрес ручки, и второе правило имени означало бы, что автор должен
        // помнить, какое из них где.
        check_stem(stem)?;
        Kind::Markdown
    } else {
        return Err(ApiError::BadRequest(
            "имя файла: расширение '.takt', '.json' либо '.md'".to_string(),
        ));
    };
    Ok(kind)
}

fn check_stem(stem: &str) -> Result<(), ApiError> {
    if stem.is_empty() {
        return Err(ApiError::BadRequest("имя файла: пустое".to_string()));
    }
    if !stem
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
    {
        return Err(ApiError::BadRequest(
            "имя файла: латинские буквы, цифры, '_' и '-'".to_string(),
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_limit_is_counted_in_bytes_not_characters() {
        // "64 тысячи символов" дали бы вдвое больший файл: в `.takt` есть кириллица, и
        // считать надо то, что ляжет в базу.
        let cyrillic = "я".repeat(FILE_BYTES / 2);
        assert_eq!(cyrillic.chars().count(), FILE_BYTES / 2);
        assert_eq!(cyrillic.len(), FILE_BYTES);
        assert!(check_file(&cyrillic).is_ok(), "ровно предел — можно");
        assert!(
            check_file(&format!("{cyrillic}я")).is_err(),
            "на два байта больше"
        );
    }

    #[test]
    fn refusal_names_both_the_limit_and_the_fact() {
        // Без числа предела автор не знает, насколько ужиматься; без факта - было ли
        // превышение на байт или вдесятеро.
        let error = check_file(&"x".repeat(FILE_BYTES + 5)).expect_err("предел");
        let text = error.to_string();
        assert!(text.contains(&FILE_BYTES.to_string()), "{text}");
        assert!(text.contains(&(FILE_BYTES + 5).to_string()), "{text}");
    }

    #[test]
    fn file_name_becomes_a_model_name_and_is_checked_as_one() {
        assert_eq!(check_file_name("heater.takt").expect("годно"), Kind::Takt);
        assert_eq!(
            check_file_name("run-1.json").expect("годно"),
            Kind::Scenario
        );
        assert!(check_file_name("модель.takt").is_err(), "кириллица");
        assert!(check_file_name("два слова.takt").is_err(), "пробел");
        assert!(check_file_name("heater.c").is_err(), "чужое расширение");
        assert!(check_file_name(".takt").is_err(), "пустое имя");
        assert!(check_file_name("a/b.takt").is_err(), "путь, а не имя");
        assert!(
            check_file_name(&format!("{}.takt", "x".repeat(NAME_CHARS))).is_err(),
            "длинное"
        );
    }

    #[test]
    fn project_name_and_description_are_measured_in_characters() {
        // Имя показывается человеку, а не хранится в порождённом коде: считать его
        // байтами значило бы дать кириллическому имени вдвое меньше места.
        assert!(check_project_name(&"я".repeat(NAME_CHARS)).is_ok());
        assert!(check_project_name(&"я".repeat(NAME_CHARS + 1)).is_err());
        assert!(check_project_name("   ").is_err(), "пустое имя");
        assert!(check_description(&"я".repeat(DESCRIPTION_CHARS)).is_ok());
        assert!(check_description(&"я".repeat(DESCRIPTION_CHARS + 1)).is_err());
    }

    #[test]
    fn limits_agree_with_each_other() {
        // Предел проекта обязан вмещать хотя бы несколько файлов предела: иначе один
        // законный файл делает проект невозможным.
        assert!(PROJECT_BYTES >= FILE_BYTES as i64 * 4, "проект тесен файлу");
        assert!(
            FILES_PER_PROJECT * FILE_BYTES as i64 > PROJECT_BYTES,
            "предел числа файлов недостижим — он ничего не ограничивает"
        );
    }
}

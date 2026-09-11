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

use std::collections::BTreeMap;

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

/// Наибольшая задержка между тактами прогона, секунд.
///
/// Минута - уже не темп показа, а остановка: дольше автор ждать не станет, а
/// опечатка в тысячу секунд выглядела бы зависшим прогоном.
pub const RUN_DELAY_SECONDS: f64 = 60.0;

/// Наибольшее число сценариев с задержкой у проекта - по числу файлов.
pub const RUN_DELAYS: usize = FILES_PER_PROJECT as usize;

/// Проверяет задержки прогона и отдаёт их в хранимом виде.
///
/// Задержка - число секунд от нуля до [`RUN_DELAY_SECONDS`], дробное; хранится с
/// точностью до миллисекунды. Ноль означает "без задержки" и не хранится: запись о
/// нуле ничего не несёт, а список рос бы от каждого сценария, который открывали.
/// Что ключ - сценарий проекта, судит вызывающий: состав знает база.
pub fn check_run_delays(delays: &BTreeMap<String, f64>) -> Result<BTreeMap<String, f64>, ApiError> {
    let mut kept = BTreeMap::new();
    for (name, &seconds) in delays {
        if !seconds.is_finite() || seconds < 0.0 {
            return Err(ApiError::BadRequest(format!(
                "задержка прогона у '{name}': число секунд от 0 до {RUN_DELAY_SECONDS}"
            )));
        }
        if seconds > RUN_DELAY_SECONDS {
            return Err(exceeded(
                &format!("задержка прогона у '{name}' в секундах"),
                RUN_DELAY_SECONDS,
                seconds,
            ));
        }
        let rounded = (seconds * 1000.0).round() / 1000.0;
        if rounded > 0.0 {
            kept.insert(name.clone(), rounded);
        }
    }
    if kept.len() > RUN_DELAYS {
        return Err(exceeded(
            "число сценариев с задержкой",
            RUN_DELAYS,
            kept.len(),
        ));
    }
    Ok(kept)
}

/// Род файла проекта - тип крейта проекта: правило одно у сервера и командной
/// строки.
pub use takt_project::Kind;

/// Проверяет имя файла и определяет его род.
///
/// Правило живёт в крейте проекта (`takt_project::check_file_name`): имя файла
/// становится именем корневой модели, и алфавит у него узкий. Здесь - только
/// перевод отказа в ответ сервиса.
pub fn check_file_name(name: &str) -> Result<Kind, ApiError> {
    takt_project::check_file_name(name).map_err(ApiError::from)
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
    fn a_bad_file_name_is_a_bad_request_and_a_long_one_is_a_limit() {
        // Правило имени проверяется в крейте проекта; здесь - что его отказы
        // доезжают до ответа своим кодом.
        assert_eq!(
            check_file_name("heater.takt-ui").expect("годно"),
            Kind::Layout
        );
        let (status, _) = check_file_name("модель.takt")
            .expect_err("кириллица")
            .status_and_code();
        assert_eq!(status, axum::http::StatusCode::BAD_REQUEST);
        let long = format!("{}.takt", "x".repeat(NAME_CHARS));
        let (status, _) = check_file_name(&long)
            .expect_err("длинное")
            .status_and_code();
        assert_eq!(status, axum::http::StatusCode::PAYLOAD_TOO_LARGE);
    }

    /// Расширения проекта в кавычках - признак второго списка родов.
    const EXTENSIONS: [&str; 5] = [
        "\".takt\"",
        "\".takt-ui\"",
        "\".takt-map\"",
        "\".json\"",
        "\".md\"",
    ];

    /// Строки кода файла (без тестов и комментариев), где стоит расширение проекта.
    fn extension_lines(source: &str) -> Vec<String> {
        let code = source.split("#[cfg(test)]").next().unwrap_or_default();
        code.lines()
            .filter(|line| !line.trim_start().starts_with("//"))
            .filter(|line| EXTENSIONS.iter().any(|ext| line.contains(ext)))
            .map(|line| line.trim().to_string())
            .collect()
    }

    #[test]
    fn the_server_has_no_second_list_of_extensions() {
        // Род файла по расширению знает крейт проекта; список у сервера разошёлся бы
        // с ним молча - так уже было с родом, которого не знала страница.
        assert_eq!(
            extension_lines("fn kind(n: &str) { n.strip_suffix(\".takt-map\") }"),
            ["fn kind(n: &str) { n.strip_suffix(\".takt-map\") }"],
            "контроль ловит второй список"
        );
        let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
        let mut found = Vec::new();
        let mut read = 0;
        for entry in std::fs::read_dir(&dir).expect("каталог исходников") {
            let path = entry.expect("запись").path();
            if path.extension().is_some_and(|e| e == "rs") {
                read += 1;
                let text = std::fs::read_to_string(&path).expect("исходник");
                for line in extension_lines(&text) {
                    found.push(format!("{}: {line}", path.display()));
                }
            }
        }
        assert!(read >= 10, "выборка пуста: прочитано {read} файлов");
        assert!(
            found.is_empty(),
            "список расширений у сервера:\n{}",
            found.join("\n")
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

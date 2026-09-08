//! Архив проекта: выгрузка и загрузка.
//!
//! # Круговой рейс, а не выгрузка
//!
//! архив несёт **файл метаданных**, и главная цель - **загрузка проекта из архива**.
//! Значит, архив - не снимок для человека, а форма обмена, и требования к нему другие:
//!
//! - **метаданные обязательны**: без них загрузка не восстановит ни имени, ни
//!   активного файла, ни **версии модуля**  - проект открылся бы
//!   чужим компилятором, и вывод целей молча поехал бы;
//! - **версия формата обязательна**: читатель, встретивший незнакомую,
//!   отказывает словами - половина восстановленного проекта хуже отказа;
//! - **исходники и вывод разделены** (`src/` и `generated/`): вывод
//!   воспроизводим и в проекте не хранится, поэтому загрузка его
//!   **игнорирует**. Свали их рядом - и в проекте появились бы файлы, которых
//!   компилятор не писал.
//!
//! Видимость, права, число копий и владелец в архив **не идут**: это свойства места, а
//! не проекта. Восстановив их у себя, автор получил бы чужие права на своей стороне.
//!
//! # Пределы
//!
//! Те же, что у ручек (файл 64 КиБ, файлов 32, проект 512 КиБ, проектов 100), и
//! проверяет их **сервер**: архив приходит извне, и доверять ему нельзя. Имя файла
//! судится тем же правилом, что при записи.

use std::io::{Cursor, Read as _, Write as _};

use serde::{Deserialize, Serialize};

use crate::db;
use crate::error::ApiError;
use crate::limits;
use crate::projects::ProjectJson;

/// Имя файла метаданных внутри архива.
pub const MANIFEST: &str = "takt-project.json";

/// Каталог исходников внутри архива.
pub const SOURCES: &str = "src/";

/// Каталог порождённого вывода внутри архива.
pub const GENERATED: &str = "generated/";

/// Версия формата архива.
///
/// Растёт вместе с формой записи. Читатель, встретивший бо́льшую версию,
/// **отказывает**: разобрать наполовину значит отдать автору проект, про
/// который он думает, что тот целый.
/// p подняла версию с `1` до `2`: манифест несёт цель и ключи
/// сборки. n подняла до `3`: появился активный сценарий и **новый род
/// файла** (`markdown`). Архив прежней версии по-прежнему читается - новые
/// поля приходят пустыми (`serde(default)`), и проект получает умолчания.
/// Отвергается только версия старше известной: там могут быть поля и роды, без
/// которых проект восстановится наполовину. Род поднимает версию наравне с
/// полем: прежний сервис отверг бы `.md` как негодное имя файла, и причина
/// ("расширение '.takt' либо '.json'") не назвала бы настоящую - устаревший
/// сервис.
pub const FORMAT: u32 = 3;

/// Метаданные проекта в архиве.
#[derive(Debug, Serialize, Deserialize)]
pub struct Manifest {
    /// Версия формата архива.
    pub format: u32,
    pub name: String,
    #[serde(default)]
    pub description: String,
    /// Версия модуля, которой открывается проект.
    pub takt_lang: String,
    #[serde(default)]
    pub language_version: String,
    /// Активный файл; `null` - не назначен.
    #[serde(default)]
    pub main_file: Option<String>,
    /// Активный сценарий прогона; `null` - не назначен.
    #[serde(default)]
    pub main_scenario: Option<String>,
    /// Состав исходников: имя и вид.
    #[serde(default)]
    pub files: Vec<ManifestFile>,
    /// Когда выгружен, Unix-секунды.
    #[serde(default)]
    pub exported_at: i64,
    /// Какой целью собран `generated/`; `null` - вывода в архиве нет.
    ///
    /// Это не выбор автора: поле отвечает на вопрос "чем собран каталог `generated/`",
    /// а выбор живёт в [`Manifest::build_target`]. Поля стоят рядом и легко путаются -
    /// оттого смысл каждого назван здесь.
    #[serde(default)]
    pub generated_target: Option<String>,
    /// Цель сборки, выбранная автором; пусто - архив прежней версии формата.
    #[serde(default)]
    pub build_target: String,
    /// Ключи сборки, выбранные автором; пусто - умолчания либо архив прежней версии
    /// формата.
    #[serde(default)]
    pub build_args: String,
}

/// Запись состава.
#[derive(Debug, Serialize, Deserialize)]
pub struct ManifestFile {
    pub name: String,
    pub kind: String,
}

/// Один файл проекта для укладки в архив.
#[derive(Debug)]
pub struct SourceFile {
    pub name: String,
    pub kind: String,
    pub text: String,
}

/// Что положить в архив.
pub struct Export {
    pub manifest: Manifest,
    pub sources: Vec<SourceFile>,
    /// Вывод цели: пары "имя, текст". Пусто - выгрузка без генерации.
    pub generated: Vec<(String, String)>,
    /// Отказ цели с причиной; `None` - цель не звали либо она не отказала.
    ///
    /// Отказ цели - **нормальный ответ**, а не ошибка сервиса (вопрос задачи закрыт
    /// так): он записывается в архив словами, потому что молча пропущенный вывод
    /// неотличим от "цель ничего не печатает".
    pub refusal: Option<String>,
}

/// Складывает архив.
///
/// # Ошибки
/// Отказ записи в память (практически недостижим) либо неразбираемые
/// метаданные.
pub fn pack(export: &Export) -> anyhow::Result<Vec<u8>> {
    let mut buffer = Cursor::new(Vec::new());
    {
        let mut zip = zip::ZipWriter::new(&mut buffer);
        let options: zip::write::FileOptions<'_, ()> =
            zip::write::FileOptions::default().compression_method(zip::CompressionMethod::Deflated);
        zip.start_file(MANIFEST, options)?;
        zip.write_all(serde_json::to_string_pretty(&export.manifest)?.as_bytes())?;
        for file in &export.sources {
            zip.start_file(format!("{SOURCES}{}", file.name), options)?;
            zip.write_all(file.text.as_bytes())?;
        }
        for (name, text) in &export.generated {
            zip.start_file(format!("{GENERATED}{name}"), options)?;
            zip.write_all(text.as_bytes())?;
        }
        if let Some(reason) = &export.refusal {
            zip.start_file(format!("{GENERATED}REFUSAL.txt"), options)?;
            zip.write_all(reason.as_bytes())?;
        }
        zip.finish()?;
    }
    Ok(buffer.into_inner())
}

/// Что прочитано из архива.
#[derive(Debug)]
pub struct Import {
    pub manifest: Manifest,
    pub sources: Vec<SourceFile>,
}

/// Разбирает архив и судит его пределами хранилища.
///
/// # Ошибки
/// Не архив, нет метаданных, чужая версия формата, нарушен предел, негодное имя
/// файла.
pub fn unpack(bytes: &[u8]) -> Result<Import, ApiError> {
    let mut zip = zip::ZipArchive::new(Cursor::new(bytes))
        .map_err(|error| ApiError::BadRequest(format!("это не архив: {error}")))?;

    let manifest: Manifest = {
        let mut entry = zip.by_name(MANIFEST).map_err(|_| {
            ApiError::BadRequest(format!(
                "в архиве нет '{MANIFEST}': без метаданных проект не восстановить"
            ))
        })?;
        let mut text = String::new();
        entry
            .read_to_string(&mut text)
            .map_err(|error| ApiError::BadRequest(format!("'{MANIFEST}' не читается: {error}")))?;
        serde_json::from_str(&text).map_err(|error| {
            ApiError::BadRequest(format!("'{MANIFEST}' не разбирается: {error}"))
        })?
    };
    if manifest.format > FORMAT {
        // Отказ, а не "прочитаем что сможем": половина восстановленного проекта хуже
        // отказа - автор будет думать, что он целый.
        return Err(ApiError::BadRequest(format!(
            "архив версии формата {}, а сервис знает {FORMAT}",
            manifest.format
        )));
    }
    limits::check_project_name(&manifest.name)?;
    limits::check_description(&manifest.description)?;

    let mut sources = Vec::new();
    let mut total: i64 = 0;
    for index in 0..zip.len() {
        let mut entry = zip
            .by_index(index)
            .map_err(|error| ApiError::BadRequest(format!("архив повреждён: {error}")))?;
        let path = entry.name().to_string();
        // Вывод целей игнорируется: он воспроизводим и в проекте не хранится. Прими его
        // загрузка за исходник - в проекте появились бы файлы, которых компилятор не
        // писал.
        let Some(name) = path.strip_prefix(SOURCES) else {
            continue;
        };
        if name.is_empty() || name.ends_with('/') {
            continue;
        }
        let kind = limits::check_file_name(name)?;
        let mut text = String::new();
        entry.read_to_string(&mut text).map_err(|error| {
            ApiError::BadRequest(format!("файл '{name}' не читается как текст: {error}"))
        })?;
        limits::check_file(&text)?;
        total += text.len() as i64;
        if sources.len() as i64 >= limits::FILES_PER_PROJECT {
            return Err(limits::exceeded(
                "число файлов в проекте",
                limits::FILES_PER_PROJECT,
                sources.len() as i64 + 1,
            ));
        }
        if total > limits::PROJECT_BYTES {
            return Err(limits::exceeded(
                "размер проекта в байтах",
                limits::PROJECT_BYTES,
                total,
            ));
        }
        sources.push(SourceFile {
            name: name.to_string(),
            kind: kind.as_str().to_string(),
            text,
        });
    }
    if sources.is_empty() {
        return Err(ApiError::BadRequest(format!(
            "в архиве нет исходников: их место — каталог '{SOURCES}'"
        )));
    }
    // Имена внутри архива могут повторяться - форма это допускает, а проект нет:
    // `PRIMARY KEY (project_id, name)` принял бы последний молча.
    let mut seen = std::collections::BTreeSet::new();
    for file in &sources {
        if !seen.insert(file.name.clone()) {
            return Err(ApiError::BadRequest(format!(
                "файл '{}' в архиве дважды",
                file.name
            )));
        }
    }
    Ok(Import { manifest, sources })
}

/// Собирает метаданные выгрузки.
///
/// Проект берётся целиком, а не разбирается на семь параметров: манифест повторяет
/// метаданные проекта, и список параметров рос бы вместе с ними - у p он и упёрся бы в
/// порог `clippy::too_many_arguments`.
pub fn manifest_of(
    project: &ProjectJson,
    files: &[SourceFile],
    generated_target: Option<String>,
) -> Manifest {
    Manifest {
        format: FORMAT,
        name: project.name.clone(),
        description: project.description.clone(),
        takt_lang: project.takt_lang.clone(),
        language_version: project.language_version.clone(),
        main_file: project.main_file.clone(),
        main_scenario: project.main_scenario.clone(),
        files: files
            .iter()
            .map(|file| ManifestFile {
                name: file.name.clone(),
                kind: file.kind.clone(),
            })
            .collect(),
        exported_at: db::now(),
        generated_target,
        build_target: project.build_target.clone(),
        build_args: project.build_args.clone(),
    }
}

/// Имя файла архива: по нему его узнают в каталоге загрузок.
///
/// Строится из имени проекта, но чистится: имя бывает кириллическим и с пробелами, а
/// заголовок `Content-Disposition` их не переносит.
pub fn file_name(project: &str) -> String {
    let mut out = String::new();
    for ch in project.chars() {
        if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
            out.push(ch);
        } else if !out.ends_with('-') && !out.is_empty() {
            out.push('-');
        }
    }
    let trimmed = out.trim_matches('-');
    if trimmed.is_empty() {
        "takt-project.zip".to_string()
    } else {
        format!("{trimmed}.zip")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Проект для проб: те же поля, что отдаёт ручка чтения.
    fn project() -> ProjectJson {
        ProjectJson {
            id: "ид".into(),
            name: "Термореле".into(),
            description: "проба".into(),
            visibility: "private".into(),
            owner: "автор".into(),
            takt_lang: "0.58.0".into(),
            language_version: "0.17.0".into(),
            main_file: Some("model.takt".to_string()),
            main_scenario: Some("run.json".to_string()),
            build_target: "sv-mmio".into(),
            build_args: "--bus=apb".into(),
            revision: 1,
            size_bytes: 10,
            forked_from: None,
            created_at: 0,
            updated_at: 0,
        }
    }

    fn sample() -> Export {
        Export {
            manifest: manifest_of(
                &project(),
                &[SourceFile {
                    name: "model.takt".into(),
                    kind: "takt".into(),
                    text: "model A {}".into(),
                }],
                Some("c".to_string()),
            ),
            sources: vec![
                SourceFile {
                    name: "model.takt".into(),
                    kind: "takt".into(),
                    text: "model A {}".into(),
                },
                SourceFile {
                    name: "model.takt-ui".into(),
                    kind: "layout".into(),
                    text: "{\"format\": 1, \"sheets\": {}}\n".into(),
                },
            ],
            generated: vec![("playground.h".into(), "#ifndef X".into())],
            refusal: None,
        }
    }

    #[test]
    fn the_archive_makes_a_round_trip() {
        // Круговой рейс - и есть предмет задачи: "архив собрался" не доказывает ничего,
        // пока он не прочитан обратно.
        let bytes = pack(&sample()).expect("архив");
        let back = unpack(&bytes).expect("разбор");
        assert_eq!(back.manifest.name, "Термореле");
        assert_eq!(
            back.manifest.takt_lang, "0.58.0",
            "версия модуля пережила рейс"
        );
        assert_eq!(back.manifest.main_file.as_deref(), Some("model.takt"));
        assert_eq!(back.sources.len(), 2, "вывод цели исходником не считается");
        assert_eq!(back.sources[0].name, "model.takt");
        assert_eq!(back.sources[0].text, "model A {}");
        // Раскладка схемы - исходник проекта: едет в архиве и возвращается своим родом.
        assert_eq!(back.sources[1].name, "model.takt-ui");
        assert_eq!(back.sources[1].kind, "layout");
        assert_eq!(back.sources[1].text, "{\"format\": 1, \"sheets\": {}}\n");
        // p: выбор автора едет вместе с проектом. Пара берётся непустой и не
        // умолчанием: на `c` без ключей потеря поля неотличима от подстановки
        // умолчания.
        assert_eq!(back.manifest.build_target, "sv-mmio", "цель пережила рейс");
        assert_eq!(back.manifest.build_args, "--bus=apb", "ключи пережили рейс");
        // Поля про цель два, и они значат разное: `generated_target` - чем собран
        // `generated/`, `build_target` - что выбрал автор.
        assert_eq!(back.manifest.generated_target.as_deref(), Some("c"));
    }

    #[test]
    fn an_archive_of_the_previous_format_is_still_read() {
        // Версия формата поднята p, и прежний архив обязан читаться: иначе подъём поля
        // стоил бы автору выгруженной работы. Пара приходит пустой, и умолчание
        // подставляет уже загрузка.
        let mut export = sample();
        export.manifest.format = 1;
        export.manifest.build_target = String::new();
        export.manifest.build_args = String::new();
        let bytes = pack(&export).expect("архив");
        let back = unpack(&bytes).expect("прежний формат читается");
        assert_eq!(back.manifest.name, "Термореле");
        assert!(back.manifest.build_target.is_empty());
    }

    #[test]
    fn an_archive_without_metadata_is_refused() {
        let mut buffer = Cursor::new(Vec::new());
        {
            let mut zip = zip::ZipWriter::new(&mut buffer);
            let options: zip::write::FileOptions<'_, ()> = zip::write::FileOptions::default();
            zip.start_file("src/model.takt", options).expect("файл");
            zip.write_all(b"model A {}").expect("запись");
            zip.finish().expect("конец");
        }
        let error = unpack(&buffer.into_inner()).expect_err("должен отказать");
        let (status, code) = error.status_and_code();
        assert_eq!(status, axum::http::StatusCode::BAD_REQUEST);
        assert_eq!(code, "bad_request");
        assert!(error.to_string().contains(MANIFEST), "{error}");
    }

    #[test]
    fn a_future_format_is_refused_by_words() {
        let mut export = sample();
        export.manifest.format = FORMAT + 1;
        let bytes = pack(&export).expect("архив");
        let error = unpack(&bytes).expect_err("должен отказать");
        // Оба числа названы: автор обязан понять, что обновлять - архив или сервис.
        assert!(
            error.to_string().contains(&(FORMAT + 1).to_string()),
            "{error}"
        );
        assert!(error.to_string().contains(&FORMAT.to_string()), "{error}");
    }

    #[test]
    fn the_limits_of_the_storage_apply_to_what_came_from_outside() {
        // Архив приходит извне, и доверять ему нельзя: пределы те же, что у ручек, и
        // проверяет их сервер.
        let mut export = sample();
        export.sources[0].text = "x".repeat(limits::FILE_BYTES + 1);
        let bytes = pack(&export).expect("архив");
        let error = unpack(&bytes).expect_err("должен отказать");
        let (status, _) = error.status_and_code();
        assert_eq!(status, axum::http::StatusCode::PAYLOAD_TOO_LARGE);

        // Имя файла судится как имя модели.
        let mut export = sample();
        export.sources[0].name = "модель.takt".to_string();
        let bytes = pack(&export).expect("архив");
        assert!(unpack(&bytes).is_err(), "кириллица в имени файла");
    }

    #[test]
    fn the_same_name_twice_does_not_get_into_an_archive() {
        // Форма zip повторы допускает, а проект - нет: `PRIMARY KEY` принял бы
        // последний молча. Замер: повтор отвергают оба конца - и запись (проверяется
        // здесь), и чтение. Проверка в `unpack` оставлена защитой в глубину и названа:
        // архив приходит извне, и полагаться на чужую библиотеку в вопросе целостности
        // данных нельзя.
        let mut export = sample();
        export.sources.push(SourceFile {
            name: "model.takt".into(),
            kind: "takt".into(),
            text: "model B {}".into(),
        });
        let error = pack(&export).expect_err("повтор не должен записаться");
        assert!(error.to_string().contains("Duplicate"), "{error}");
    }

    #[test]
    fn the_file_name_survives_a_cyrillic_project_name() {
        // Заголовок `Content-Disposition` не переносит ни кириллицы, ни пробелов, а имя
        // проекта бывает и тем и другим.
        assert_eq!(file_name("counter"), "counter.zip");
        assert_eq!(file_name("Термореле 2"), "2.zip");
        assert_eq!(file_name("Термореле"), "takt-project.zip");
        assert_eq!(file_name(""), "takt-project.zip");
    }
}

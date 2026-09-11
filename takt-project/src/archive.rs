//! Архив проекта: укладка и разбор.
//!
//! # Круговой рейс, а не выгрузка
//!
//! Архив несёт **манифест**, и главная цель - загрузка проекта обратно. Значит,
//! архив - не снимок для человека, а форма обмена:
//!
//! - **манифест обязателен**: без него не восстановить ни имени, ни активного файла,
//!   ни версии модуля;
//! - **версия формата обязательна**: незнакомая версия - отказ словами;
//! - **исходники и вывод разделены** (`src/` и `generated/`): вывод воспроизводим, и
//!   разбор его **игнорирует** - иначе в проекте появились бы файлы, которых
//!   компилятор не писал.
//!
//! # Пределы
//!
//! Задаёт вызывающий: у сервиса они свои (архив приходит извне, и доверять ему
//! нельзя), у командной строки, читающей архив автора с диска, их нет.

use std::collections::BTreeSet;
use std::io::{Cursor, Read as _, Write as _};

use crate::error::Error;
use crate::kind::check_file_name;
use crate::manifest::{FORMAT, GENERATED, MANIFEST, Manifest, SOURCES};

/// Один файл проекта: имя, род и текст.
#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// Отказ цели - нормальный ответ, а не ошибка: он записывается в архив словами,
    /// потому что молча пропущенный вывод неотличим от "цель ничего не печатает".
    pub refusal: Option<String>,
}

/// Что прочитано из архива.
#[derive(Debug)]
pub struct Import {
    pub manifest: Manifest,
    pub sources: Vec<SourceFile>,
}

/// Пределы разбора: размер файла и проекта в байтах, число файлов.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Limits {
    pub file_bytes: usize,
    pub files: usize,
    pub project_bytes: usize,
}

impl Limits {
    /// Без пределов: архив автора на его же диске.
    pub const NONE: Self = Self {
        file_bytes: usize::MAX,
        files: usize::MAX,
        project_bytes: usize::MAX,
    };
}

/// Складывает архив.
///
/// # Ошибки
/// Имя файла дважды (форма zip повторы допускает, проект - нет) либо отказ записи.
pub fn pack(export: &Export) -> Result<Vec<u8>, Error> {
    let fail = |error: &dyn std::fmt::Display| Error::Invalid(format!("архив не собран: {error}"));
    let mut buffer = Cursor::new(Vec::new());
    {
        let mut zip = zip::ZipWriter::new(&mut buffer);
        let options: zip::write::FileOptions<'_, ()> =
            zip::write::FileOptions::default().compression_method(zip::CompressionMethod::Deflated);
        let mut put = |name: &str, bytes: &[u8]| -> Result<(), Error> {
            zip.start_file(name, options).map_err(|e| fail(&e))?;
            zip.write_all(bytes).map_err(|e| fail(&e))
        };
        let manifest = serde_json::to_string_pretty(&export.manifest).map_err(|e| fail(&e))?;
        put(MANIFEST, manifest.as_bytes())?;
        for file in &export.sources {
            put(&format!("{SOURCES}{}", file.name), file.text.as_bytes())?;
        }
        for (name, text) in &export.generated {
            put(&format!("{GENERATED}{name}"), text.as_bytes())?;
        }
        if let Some(reason) = &export.refusal {
            put(&format!("{GENERATED}REFUSAL.txt"), reason.as_bytes())?;
        }
        zip.finish().map_err(|e| fail(&e))?;
    }
    Ok(buffer.into_inner())
}

/// Складывает файлы вывода в архив `.zip` как есть: без манифеста и без
/// `src/` - это не проект, а выгрузка для человека (картинки и видео экспорта).
/// Время файлов постоянное - тот же вывод даёт тот же архив байт в байт.
///
/// # Ошибки
/// Имя файла дважды либо отказ записи.
pub fn pack_files(files: &[(String, Vec<u8>)]) -> Result<Vec<u8>, Error> {
    let fail = |error: &dyn std::fmt::Display| Error::Invalid(format!("архив не собран: {error}"));
    let mut seen = BTreeSet::new();
    let mut buffer = Cursor::new(Vec::new());
    {
        let mut zip = zip::ZipWriter::new(&mut buffer);
        let options: zip::write::FileOptions<'_, ()> =
            zip::write::FileOptions::default().compression_method(zip::CompressionMethod::Deflated);
        for (name, bytes) in files {
            if !seen.insert(name.as_str()) {
                return Err(Error::Invalid(format!("в выгрузке дважды файл '{name}'")));
            }
            zip.start_file(name.as_str(), options)
                .map_err(|e| fail(&e))?;
            zip.write_all(bytes).map_err(|e| fail(&e))?;
        }
        zip.finish().map_err(|e| fail(&e))?;
    }
    Ok(buffer.into_inner())
}

/// Разбирает архив и судит его пределами.
///
/// # Ошибки
/// Не архив, нет манифеста, версия формата новее известной, негодное имя файла,
/// файл не текст, имя дважды, нет исходников, превышен предел.
pub fn unpack(bytes: &[u8], limits: Limits) -> Result<Import, Error> {
    let mut zip = zip::ZipArchive::new(Cursor::new(bytes))
        .map_err(|error| Error::Invalid(format!("это не архив: {error}")))?;

    let manifest: Manifest = {
        let mut entry = zip.by_name(MANIFEST).map_err(|_| {
            Error::Invalid(format!(
                "в архиве нет '{MANIFEST}': без метаданных проект не восстановить"
            ))
        })?;
        let mut text = String::new();
        entry
            .read_to_string(&mut text)
            .map_err(|error| Error::Invalid(format!("'{MANIFEST}' не читается: {error}")))?;
        parse_manifest(&text)?
    };

    let mut sources: Vec<SourceFile> = Vec::new();
    let mut total = 0usize;
    for index in 0..zip.len() {
        let mut entry = zip
            .by_index(index)
            .map_err(|error| Error::Invalid(format!("архив повреждён: {error}")))?;
        let path = entry.name().to_string();
        let Some(name) = path.strip_prefix(SOURCES) else {
            continue;
        };
        if name.is_empty() || name.ends_with('/') {
            continue;
        }
        let kind = check_file_name(name)?;
        let mut text = String::new();
        entry.read_to_string(&mut text).map_err(|error| {
            Error::Invalid(format!("файл '{name}' не читается как текст: {error}"))
        })?;
        if text.len() > limits.file_bytes {
            return Err(Error::exceeded(
                "размер файла в байтах",
                limits.file_bytes,
                text.len(),
            ));
        }
        total += text.len();
        if sources.len() >= limits.files {
            return Err(Error::exceeded(
                "число файлов в проекте",
                limits.files,
                sources.len() + 1,
            ));
        }
        if total > limits.project_bytes {
            return Err(Error::exceeded(
                "размер проекта в байтах",
                limits.project_bytes,
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
        return Err(Error::Invalid(format!(
            "в архиве нет исходников: их место — каталог '{SOURCES}'"
        )));
    }
    // Имена внутри архива могут повторяться - форма это допускает, а проект нет:
    // последний молча заменил бы первый.
    let mut seen = BTreeSet::new();
    for file in &sources {
        if !seen.insert(file.name.clone()) {
            return Err(Error::Invalid(format!(
                "файл '{}' в архиве дважды",
                file.name
            )));
        }
    }
    Ok(Import { manifest, sources })
}

/// Разбирает манифест и судит версию формата.
///
/// # Ошибки
/// Текст не манифест либо версия формата новее известной.
pub(crate) fn parse_manifest(text: &str) -> Result<Manifest, Error> {
    let manifest: Manifest = serde_json::from_str(text)
        .map_err(|error| Error::Invalid(format!("'{MANIFEST}' не разбирается: {error}")))?;
    if manifest.format > FORMAT {
        // Отказ, а не "прочитаем что сможем": половина восстановленного проекта хуже
        // отказа - автор будет думать, что он целый. Оба числа названы: понятно, что
        // обновлять - проект или инструмент.
        return Err(Error::Invalid(format!(
            "проект версии формата {}, а инструмент знает {FORMAT}",
            manifest.format
        )));
    }
    Ok(manifest)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::manifest::ManifestFile;

    fn sample() -> Export {
        Export {
            manifest: Manifest {
                format: FORMAT,
                name: "Термореле".into(),
                takt_lang: "0.61.0".into(),
                main_file: Some("model.takt".into()),
                files: vec![ManifestFile {
                    name: "model.takt".into(),
                    kind: "takt".into(),
                }],
                build_target: "sv-mmio".into(),
                build_args: "--bus=apb".into(),
                ..Manifest::default()
            },
            sources: vec![
                SourceFile {
                    name: "model.takt".into(),
                    kind: "takt".into(),
                    text: "model A {}".into(),
                },
                SourceFile {
                    name: "model.takt-ui".into(),
                    kind: "layout".into(),
                    text: "{\"format\": 1}\n".into(),
                },
            ],
            generated: vec![("playground.h".into(), "#ifndef X".into())],
            refusal: None,
        }
    }

    #[test]
    fn the_archive_makes_a_round_trip() {
        let back = unpack(&pack(&sample()).expect("архив"), Limits::NONE).expect("разбор");
        assert_eq!(back.manifest, sample().manifest, "манифест пережил рейс");
        assert_eq!(
            back.sources,
            sample().sources,
            "вывод цели исходником не считается"
        );
    }

    #[test]
    fn a_future_format_is_refused_by_words() {
        let mut export = sample();
        export.manifest.format = FORMAT + 1;
        let error = unpack(&pack(&export).expect("архив"), Limits::NONE).expect_err("отказ");
        assert!(
            error.message().contains(&(FORMAT + 1).to_string()),
            "{error}"
        );
        assert!(error.message().contains(&FORMAT.to_string()), "{error}");
    }

    #[test]
    fn limits_are_those_of_the_caller() {
        let small = Limits {
            file_bytes: 5,
            files: 32,
            project_bytes: 1000,
        };
        let error = unpack(&pack(&sample()).expect("архив"), small).expect_err("предел");
        assert!(matches!(error, Error::Limit(_)), "{error}");
        let one = Limits {
            file_bytes: 1000,
            files: 1,
            project_bytes: 1000,
        };
        assert!(matches!(
            unpack(&pack(&sample()).expect("архив"), one),
            Err(Error::Limit(_))
        ));
    }

    #[test]
    fn the_same_name_twice_does_not_get_into_an_archive() {
        let mut export = sample();
        export.sources.push(export.sources[0].clone());
        assert!(pack(&export).is_err(), "повтор не должен записаться");
    }

    #[test]
    fn an_archive_without_a_manifest_or_sources_is_refused() {
        let mut buffer = Cursor::new(Vec::new());
        {
            let mut zip = zip::ZipWriter::new(&mut buffer);
            let options: zip::write::FileOptions<'_, ()> = zip::write::FileOptions::default();
            zip.start_file("src/model.takt", options).expect("файл");
            zip.write_all(b"model A {}").expect("запись");
            zip.finish().expect("конец");
        }
        let error = unpack(&buffer.into_inner(), Limits::NONE).expect_err("отказ");
        assert!(error.message().contains(MANIFEST), "{error}");
        let mut export = sample();
        export.sources.clear();
        let error = unpack(&pack(&export).expect("архив"), Limits::NONE).expect_err("отказ");
        assert!(error.message().contains(SOURCES), "{error}");
    }
}

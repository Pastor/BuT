//! Поиск и чтение файлов импорта для Takt.
//!
//! Основная функция [`read_import_file`] ищет `.takt`-файл по списку директорий,
//! проверяет расширение и возвращает содержимое вместе с полным путём к файлу.

pub(in crate::semantic) mod adopt;
pub(in crate::semantic) mod build;
pub mod importers; // "кто подключает эту библиотеку" - подсказка SE-102
pub(in crate::semantic) mod select;

use crate::diagnostics::Diagnostic;
use crate::parser::ast::ImportPath;
use itertools::Itertools;
use std::fs::{exists, read_to_string};

/// Читает содержимое файла импорта, используя путь [`ImportPath`].
///
/// Перебирает `search_paths` в порядке объявления и возвращает первый найденный файл.
///
/// # Параметры
///
/// - `search_paths` - список директорий для поиска файлов (например, `["/src", "/lib"]`).
/// - `path` - путь импорта: строковый литерал (`"file.takt"`) или
///   идентификаторный путь (`a::b::c` -> `a/b/c.takt`).
///
/// # Возвращает
///
/// Пару `(содержимое_файла, полный_путь)` при успехе.
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`], если:
/// - файл не найден ни в одной из директорий поиска;
/// - файл имеет расширение, отличное от `.takt`;
/// - файл не удалось прочитать (нет прав, ошибка ввода-вывода и т.д.).
///
/// # Примеры
///
/// Использование через интеграционные тесты - см.
/// ```text
/// import "model.takt";            // Plain-импорт
/// import "engine.takt" as Motor;  // GlobalSymbol-импорт с алиасом
/// ```
/// Имя файла без каталога - для сообщений: полный путь в цепочке импорта только мешает
/// читать.
///
/// Живёт здесь, а не в `tree.rs`: короткое имя нужно **сообщениям об импорте**, и это
/// предмет данного модуля, а `tree.rs` пришпилен реестром размеров.
pub(crate) fn short_name(path: &str) -> String {
    std::path::Path::new(path)
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| path.to_string())
}

pub(crate) fn read_import_file(
    search_paths: &[String],
    path: &ImportPath,
) -> Result<(String, String), Diagnostic> {
    // Позиция директивы `import` - из самого пути.
    let loc = match path {
        ImportPath::Filename(literal) => literal.loc,
        ImportPath::Path(id_path) => id_path.loc,
    };

    // Формируем список кандидатов: cross-product (search_paths x path)
    let files: Vec<String> = match path {
        // Строковый литерал: `import "file.takt";`
        ImportPath::Filename(filename) => search_paths
            .iter()
            .map(|dir| format!("{}/{}", dir, filename.string))
            .collect(),

        // Идентификаторный путь: `import a::b::c;` -> ищем `a/b/c.takt`
        ImportPath::Path(id_path) => {
            let inner = id_path.identifiers.iter().map(|i| i.name.clone()).join("/");
            search_paths
                .iter()
                .map(|dir| format!("{}/{}.takt", dir, inner))
                .collect()
        }
    };

    // Оставляем только существующие файлы
    let found: Vec<String> = files
        .iter()
        .filter(|f| exists(f).is_ok_and(|r| r))
        .cloned()
        .collect();

    if found.is_empty() {
        let path_str = match path {
            ImportPath::Filename(sl) => sl.string.clone(),
            ImportPath::Path(id_path) => id_path
                .identifiers
                .iter()
                .map(|i| i.name.as_str())
                .collect::<Vec<_>>()
                .join("::"),
        };
        let paths_str = if search_paths.is_empty() {
            "(пути не заданы)".to_string()
        } else {
            search_paths.join(", ")
        };
        return Err(Diagnostic::error(
            loc,
            format!(
                "Файл импорта не найден: «{}». Пути поиска: {}",
                path_str, paths_str
            ),
        )
        .with_code("SE-013"));
    }

    let filename = found.first().unwrap();

    // V3: Защита от обхода каталогов (path traversal). Канонизируем найденный путь и
    // проверяем, что он находится внутри одного из разрешённых каталогов поиска. Это
    // предотвращает импорты вида `import "../../etc/passwd"`.
    {
        let canonical_file = std::fs::canonicalize(filename).map_err(|e| {
            Diagnostic::error(
                loc,
                format!("Не удалось канонизировать путь «{}»: {}", filename, e),
            )
            .with_code("SE-016")
        })?;
        let is_allowed = search_paths.iter().any(|dir| {
            // Канонизируем директорию поиска; если это не удаётся - пропускаем её.
            std::fs::canonicalize(dir)
                .map(|canon_dir| canonical_file.starts_with(&canon_dir))
                .unwrap_or(false)
        });
        if !is_allowed {
            return Err(Diagnostic::error(
                loc,
                format!(
                    "Путь импорта «{}» выходит за пределы разрешённых директорий поиска: {:?}",
                    filename, search_paths
                ),
            )
            .with_code("SE-016"));
        }
    }

    // Проверяем, что файл имеет расширение.takt
    if !filename.ends_with(".takt") {
        return Err(Diagnostic::error(
            loc,
            format!("Недопустимое расширение файла импорта: «{}»", filename),
        )
        .with_code("SE-014"));
    }

    let content = read_to_string(filename).map_err(|e| {
        Diagnostic::error(
            loc,
            format!("Ошибка чтения файла импорта «{}»: {}", filename, e),
        )
        .with_code("SE-015")
    })?;

    Ok((content, filename.clone()))
}

// --- Тесты --------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::Location;
    use crate::parser::ast::{IdentifierPath, ImportPath, StringLiteral};
    use std::fs;

    // --- Вспомогательные функции ----------------------------------------------

    /// Создаёт временный `.takt`-файл с заданным содержимым. Возвращает (директория,
    /// полный_путь_к_файлу).
    fn make_tmp_but(name: &str, content: &str) -> (tempfile::TempDir, String) {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(name);
        fs::write(&path, content).unwrap();
        (dir, path.to_string_lossy().into_owned())
    }

    /// Строит `ImportPath::Filename` из строки.
    fn filename_path(s: &str) -> ImportPath {
        ImportPath::Filename(StringLiteral {
            string: s.to_string(),
            unicode: false,
            loc: Location::default(),
        })
    }

    // --- Позитивные тесты -----------------------------------------------------

    /// Файл найден по строковому литералу: содержимое и путь возвращаются корректно.
    #[test]
    fn filename_found_returns_content_and_path() {
        let (dir, _) = make_tmp_but("model.takt", "start S;");
        let search = vec![dir.path().to_string_lossy().into_owned()];
        let path = filename_path("model.takt");

        let (content, fullpath) = read_import_file(&search, &path).unwrap();
        assert_eq!(content, "start S;");
        assert!(fullpath.ends_with("model.takt"));
    }

    /// Первый файл в нескольких путях поиска возвращается (порядок поиска).
    #[test]
    fn first_search_path_wins() {
        let dir1 = tempfile::tempdir().unwrap();
        let dir2 = tempfile::tempdir().unwrap();
        fs::write(dir1.path().join("x.takt"), "start A;").unwrap();
        fs::write(dir2.path().join("x.takt"), "start B;").unwrap();

        let search = vec![
            dir1.path().to_string_lossy().into_owned(),
            dir2.path().to_string_lossy().into_owned(),
        ];
        let (content, _) = read_import_file(&search, &filename_path("x.takt")).unwrap();
        assert_eq!(
            content, "start A;",
            "должна вернуться версия из первого пути"
        );
    }

    /// Идентификаторный путь `a::b` ищет файл `a/b.takt`.
    #[test]
    fn identifier_path_appends_but_extension() {
        use crate::parser::ast::Identifier;

        let dir = tempfile::tempdir().unwrap();
        let subdir = dir.path().join("a");
        fs::create_dir(&subdir).unwrap();
        fs::write(subdir.join("b.takt"), "start S;").unwrap();

        let search = vec![dir.path().to_string_lossy().into_owned()];
        let path = ImportPath::Path(IdentifierPath {
            identifiers: vec![
                Identifier {
                    name: "a".to_string(),
                    loc: Location::default(),
                },
                Identifier {
                    name: "b".to_string(),
                    loc: Location::default(),
                },
            ],
            loc: Location::default(),
        });

        let (content, fullpath) = read_import_file(&search, &path).unwrap();
        assert_eq!(content, "start S;");
        assert!(
            fullpath.ends_with("a/b.takt") || fullpath.ends_with("a\\b.takt"),
            "путь должен заканчиваться на a/b.takt"
        );
    }

    // --- Негативные тесты (контр-примеры) ------------------------------------

    /// Файл не найден -> ошибка с упоминанием путей поиска.
    #[test]
    fn missing_file_returns_error() {
        let err = read_import_file(
            &["/nonexistent_dir_xyz".to_string()],
            &filename_path("ghost.takt"),
        )
        .unwrap_err();
        assert!(
            err.message.contains("не найден"),
            "сообщение ошибки должно содержать «не найден»: {}",
            err.message
        );
    }

    /// Пустой список путей поиска -> ошибка.
    #[test]
    fn empty_search_paths_returns_error() {
        let err = read_import_file(&[], &filename_path("any.takt")).unwrap_err();
        assert!(err.message.contains("не найден"));
    }

    /// Файл существует, но не имеет расширения `.takt` -> ошибка.
    #[test]
    fn non_but_extension_is_error() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("model.txt"), "start S;").unwrap();
        let search = vec![dir.path().to_string_lossy().into_owned()];

        let err = read_import_file(&search, &filename_path("model.txt")).unwrap_err();
        assert!(
            err.message.contains("Недопустимое"),
            "ошибка должна сообщать о недопустимом расширении: {}",
            err.message
        );
    }

    /// Файл существует, но не имеет расширения вовсе -> ошибка.
    #[test]
    fn file_without_extension_is_error() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("model"), "start S;").unwrap();
        let search = vec![dir.path().to_string_lossy().into_owned()];

        let err = read_import_file(&search, &filename_path("model")).unwrap_err();
        assert!(err.message.contains("Недопустимое"));
    }

    // -- V3: Защита от обхода каталогов (path traversal) ----------------------

    /// V3: Путь вида `../../etc/passwd` должен возвращать ошибку, а не читать файлы за
    /// пределами разрешённых директорий.
    #[test]
    fn path_traversal_is_rejected() {
        // Создаём две временные директории: разрешённую (search dir) и "жертву".
        let allowed_dir = tempfile::tempdir().unwrap();
        let victim_dir = tempfile::tempdir().unwrap();

        // Кладём целевой файл в директорию "жертву" (не в search_paths).
        fs::write(victim_dir.path().join("secret.takt"), "start S;").unwrap();

        // Строим относительный путь, выходящий за пределы allowed_dir. Например:
        // "../<victim_basename>/secret.takt"
        let victim_dirname = victim_dir
            .path()
            .file_name()
            .unwrap()
            .to_string_lossy()
            .into_owned();
        let traversal = format!("../{}/secret.takt", victim_dirname);

        let search = vec![allowed_dir.path().to_string_lossy().into_owned()];
        let path = filename_path(&traversal);

        let result = read_import_file(&search, &path);
        // Ожидаем ошибку: файл либо не найден (файловая система не совпадёт), либо
        // отклонён проверкой обхода каталогов.
        match result {
            Err(e) => {
                // Допустимо: файл не найден или обход отклонён
                let _ = e;
            }
            Ok(_) => {
                panic!(
                    "path traversal должен быть отклонён, \
                     но файл был успешно прочитан"
                );
            }
        }
    }

    /// V3: Легитимный вложенный путь внутри search_dir разрешён.
    #[test]
    fn nested_path_within_search_dir_is_allowed() {
        let dir = tempfile::tempdir().unwrap();
        let subdir = dir.path().join("sub");
        fs::create_dir(&subdir).unwrap();
        fs::write(subdir.join("ok.takt"), "start S;").unwrap();

        let search = vec![dir.path().to_string_lossy().into_owned()];
        let path = filename_path("sub/ok.takt");

        // Должно успешно читаться - файл внутри search_dir.
        let (content, _) = read_import_file(&search, &path).unwrap();
        assert_eq!(content, "start S;");
    }

    /// Ошибка чтения файла (нет прав) возвращает осмысленный диагностик. Этот тест
    /// запускается только на Unix, где можно снять права на чтение.
    #[cfg(unix)]
    #[test]
    fn unreadable_file_returns_io_error() {
        use std::os::unix::fs::PermissionsExt;
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join("secret.takt");
        fs::write(&p, "start S;").unwrap();
        fs::set_permissions(&p, fs::Permissions::from_mode(0o000)).unwrap();

        let search = vec![dir.path().to_string_lossy().into_owned()];
        let err = read_import_file(&search, &filename_path("secret.takt")).unwrap_err();
        // Восстанавливаем права, чтобы tempdir мог удалить файл
        fs::set_permissions(&p, fs::Permissions::from_mode(0o644)).unwrap();
        assert!(
            !err.message.is_empty(),
            "сообщение об ошибке не должно быть пустым"
        );
    }
}

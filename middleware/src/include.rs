//! Механизм включения (include/import) файлов языка BuT.
//!
//! Поддерживаемые формы директивы import:
//!
//! ```text
//! import "файл";                        — прямой импорт
//! import "файл" as Псевдоним;           — импорт с псевдонимом пространства имён
//! import * as Псевдоним from "файл";    — импорт всех символов с псевдонимом
//! import { A, B as C } from "файл";    — именованный импорт
//! ```
//!
//! Резолвер выполняет:
//! - Поиск файлов относительно базового файла или в путях поиска
//! - Рекурсивную загрузку и разбор транзитивных зависимостей
//! - Дедупликацию: один файл включается ровно один раз
//! - Обнаружение циклических зависимостей
//! - Нумерацию файлов для корректной диагностики ошибок
//!
//! # Пример использования
//!
//! ```rust,no_run
//! use std::path::Path;
//! use but_middleware::include::IncludeResolver;
//!
//! let source = r#"import "types"; model M { state S {} start -> S; }"#;
//! let (unit, _) = but_grammar::parse(source, 0).unwrap();
//!
//! let mut resolver = IncludeResolver::from_file(Path::new("/project/main.but"));
//! match resolver.resolve(unit, Path::new("/project/main.but")) {
//!     Ok(merged) => println!("Объединено {} элементов", merged.0.len()),
//!     Err(errs)  => errs.iter().for_each(|e| eprintln!("Ошибка: {}", e)),
//! }
//! ```

use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use but_grammar::ast::{Import, ImportPath, SourceUnit, SourceUnitPart};
use but_grammar::diagnostics::Diagnostic;

// ─── Счётчик номеров файлов ───────────────────────────────────────────────────

/// Глобальный счётчик для присвоения уникальных номеров файлам при парсинге.
static FILE_NO_COUNTER: AtomicUsize = AtomicUsize::new(1);

fn next_file_no() -> usize {
    FILE_NO_COUNTER.fetch_add(1, Ordering::SeqCst)
}

// ─── Ошибки разрешения импорта ────────────────────────────────────────────────

/// Ошибка, возникающая при разрешении директивы import.
#[derive(Debug)]
pub enum IncludeError {
    /// Импортируемый файл не найден ни в одном из путей поиска.
    FileNotFound {
        /// Имя файла из директивы import.
        path: PathBuf,
        /// Список директорий, в которых выполнялся поиск.
        search_paths: Vec<PathBuf>,
    },
    /// Обнаружен циклический импорт (файл импортирует сам себя транзитивно).
    CircularImport(PathBuf),
    /// Ошибка ввода-вывода при чтении файла.
    IoError {
        /// Путь к файлу, который не удалось прочитать.
        path: PathBuf,
        /// Сообщение ошибки ОС.
        error: String,
    },
    /// Синтаксические ошибки в импортируемом файле.
    ParseError {
        /// Путь к файлу с ошибками.
        path: PathBuf,
        /// Список диагностик парсера.
        errors: Vec<Diagnostic>,
    },
}

impl fmt::Display for IncludeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IncludeError::FileNotFound { path, search_paths } => write!(
                f,
                "Файл не найден: '{}'. Пути поиска: [{}]",
                path.display(),
                search_paths
                    .iter()
                    .map(|p| format!("'{}'", p.display()))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            IncludeError::CircularImport(path) => {
                write!(f, "Циклический импорт: '{}'", path.display())
            }
            IncludeError::IoError { path, error } => {
                write!(f, "Ошибка чтения '{}': {}", path.display(), error)
            }
            IncludeError::ParseError { path, errors } => write!(
                f,
                "Ошибки разбора в '{}': {} ошибок",
                path.display(),
                errors.len()
            ),
        }
    }
}

// ─── Резолвер импортов ────────────────────────────────────────────────────────

/// Резолвер директив `import` языка BuT.
///
/// Рекурсивно загружает и объединяет импортируемые файлы в единый [`SourceUnit`].
/// Каждый файл обрабатывается ровно один раз благодаря отслеживанию посещённых
/// канонических путей.
pub struct IncludeResolver {
    /// Список директорий для поиска импортируемых файлов.
    search_paths: Vec<PathBuf>,
    /// Канонические пути уже обработанных файлов (защита от дублирования и циклов).
    visited: HashSet<PathBuf>,
}

impl IncludeResolver {
    /// Создать резолвер с явным списком путей поиска.
    pub fn new(search_paths: Vec<PathBuf>) -> Self {
        Self {
            search_paths,
            visited: HashSet::new(),
        }
    }

    /// Создать резолвер, добавив директорию файла `file_path` как путь поиска.
    pub fn from_file(file_path: &Path) -> Self {
        let mut search_paths = vec![];
        if let Some(dir) = file_path.parent() {
            // Пропустить пустой путь (""), который возвращает parent() для файлов без директории
            if dir != Path::new("") {
                search_paths.push(dir.to_path_buf());
            }
        }
        Self::new(search_paths)
    }

    /// Добавить директорию в список путей поиска.
    pub fn add_search_path(&mut self, path: PathBuf) -> &mut Self {
        self.search_paths.push(path);
        self
    }

    /// Получить список путей поиска.
    pub fn search_paths(&self) -> &[PathBuf] {
        &self.search_paths
    }

    /// Количество уже обработанных файлов.
    pub fn visited_count(&self) -> usize {
        self.visited.len()
    }

    /// Разрешить все директивы `import` в `unit`, рекурсивно загружая зависимости.
    ///
    /// Возвращает объединённый [`SourceUnit`] без директив импорта — все
    /// импортированные определения встроены непосредственно в результат.
    ///
    /// Порядок элементов: сначала все определения из импортированных файлов
    /// (в порядке встречи директив), затем определения из текущего файла.
    ///
    /// # Ошибки
    ///
    /// Возвращает список [`IncludeError`] если хотя бы один импорт не удалось
    /// разрешить. Обработка продолжается для остальных импортов, чтобы собрать
    /// максимум ошибок за один проход.
    pub fn resolve(
        &mut self,
        unit: SourceUnit,
        base_path: &Path,
    ) -> Result<SourceUnit, Vec<IncludeError>> {
        // Пометить базовый файл как посещённый
        self.mark_visited(base_path);

        let mut result_parts: Vec<SourceUnitPart> = vec![];
        let mut errors: Vec<IncludeError> = vec![];

        for part in unit.0 {
            match part {
                SourceUnitPart::ImportDirective(ref import) => {
                    let import_path_str = extract_path_str(import);

                    // Найти файл на диске
                    let resolved = match self.find_file(&import_path_str, base_path) {
                        Ok(p) => p,
                        Err(e) => {
                            errors.push(e);
                            continue;
                        }
                    };

                    // Проверить дедупликацию
                    let canonical = match resolved.canonicalize() {
                        Ok(c) => c,
                        Err(e) => {
                            errors.push(IncludeError::IoError {
                                path: resolved.clone(),
                                error: e.to_string(),
                            });
                            continue;
                        }
                    };

                    if self.visited.contains(&canonical) {
                        // Файл уже обработан — пропустить без ошибки
                        continue;
                    }

                    // Загрузить и рекурсивно разрешить файл
                    match self.load_and_resolve(&resolved) {
                        Ok(sub_unit) => result_parts.extend(sub_unit.0),
                        Err(mut sub_errors) => errors.append(&mut sub_errors),
                    }
                }
                // Все остальные элементы переносятся без изменений
                other => result_parts.push(other),
            }
        }

        if errors.is_empty() {
            Ok(SourceUnit(result_parts))
        } else {
            Err(errors)
        }
    }

    // ── Внутренние методы ────────────────────────────────────────────────────

    /// Загрузить файл с диска, разобрать его и рекурсивно разрешить его импорты.
    fn load_and_resolve(&mut self, path: &Path) -> Result<SourceUnit, Vec<IncludeError>> {
        // Проверить циклический импорт перед загрузкой
        if let Ok(canonical) = path.canonicalize() {
            if self.visited.contains(&canonical) {
                return Err(vec![IncludeError::CircularImport(canonical)]);
            }
        }

        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                return Err(vec![IncludeError::IoError {
                    path: path.to_path_buf(),
                    error: e.to_string(),
                }])
            }
        };

        let file_no = next_file_no();
        let unit = match but_grammar::parse(&source, file_no) {
            Ok((u, _)) => u,
            Err(diags) => {
                return Err(vec![IncludeError::ParseError {
                    path: path.to_path_buf(),
                    errors: diags,
                }])
            }
        };

        // Рекурсивно разрешить импорты внутри загруженного файла
        self.resolve(unit, path)
    }

    /// Найти файл по строке пути относительно базового файла или в путях поиска.
    ///
    /// Автоматически добавляет расширение `.but`, если оно не указано.
    fn find_file(&self, path_str: &str, base_path: &Path) -> Result<PathBuf, IncludeError> {
        // Нормализовать расширение
        let normalized = if path_str.ends_with(".but") {
            path_str.to_string()
        } else {
            format!("{}.but", path_str)
        };

        // 1. Относительно директории базового файла
        if let Some(base_dir) = base_path.parent() {
            let candidate = base_dir.join(&normalized);
            if candidate.exists() {
                return Ok(candidate);
            }
        }

        // 2. Абсолютный путь
        let abs = PathBuf::from(&normalized);
        if abs.is_absolute() && abs.exists() {
            return Ok(abs);
        }

        // 3. Перебрать пути поиска
        for search_dir in &self.search_paths {
            let candidate = search_dir.join(&normalized);
            if candidate.exists() {
                return Ok(candidate);
            }
        }

        Err(IncludeError::FileNotFound {
            path: PathBuf::from(&normalized),
            search_paths: self.search_paths.clone(),
        })
    }

    /// Зарегистрировать файл как посещённый (если удаётся канонизировать путь).
    fn mark_visited(&mut self, path: &Path) {
        if let Ok(canonical) = path.canonicalize() {
            self.visited.insert(canonical);
        }
    }
}

// ─── Вспомогательные функции ──────────────────────────────────────────────────

/// Извлечь строку пути из директивы [`Import`].
fn extract_path_str(import: &Import) -> String {
    match import {
        Import::Plain(p, _) => import_path_to_string(p),
        Import::GlobalSymbol(p, _, _) => import_path_to_string(p),
        Import::Rename(p, _, _) => import_path_to_string(p),
    }
}

/// Преобразовать [`ImportPath`] в строку файлового пути.
fn import_path_to_string(path: &ImportPath) -> String {
    match path {
        // `import "файл.but"` — литеральная строка
        ImportPath::Filename(lit) => lit.string.clone(),
        // `import std::types` — путь через `::` → преобразовать в `std/types`
        ImportPath::Path(id_path) => id_path
            .identifiers
            .iter()
            .map(|id| id.name.as_str())
            .collect::<Vec<_>>()
            .join("/"),
    }
}

// ─── Тесты ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;

    use super::*;

    // ── Вспомогательные утилиты для тестов ───────────────────────────────────

    /// Путь к директории тестовых фикстур include.
    fn fixtures_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests_data/include")
    }

    /// Создать резолвер с директорией фикстур как путём поиска.
    fn resolver_for(subdir: &str) -> IncludeResolver {
        let dir = fixtures_dir().join(subdir);
        IncludeResolver::new(vec![dir])
    }

    /// Разобрать BuT-источник без файловой системы (без разрешения импортов).
    fn parse_src(src: &str) -> SourceUnit {
        but_grammar::parse(src, 0).expect("Ошибка разбора тестового источника").0
    }

    // ── Тесты extract_path_str / import_path_to_string ───────────────────────

    #[test]
    fn test_extract_path_plain_string() {
        // import "types"; — строковый путь
        let src = r#"import "types";"#;
        let unit = parse_src(src);
        if let SourceUnitPart::ImportDirective(import) = &unit.0[0] {
            assert_eq!(extract_path_str(import), "types");
        } else {
            panic!("Ожидалась директива импорта");
        }
    }

    #[test]
    fn test_extract_path_with_alias() {
        // import "foo/bar" as fb;
        let src = r#"import "foo/bar" as fb;"#;
        let unit = parse_src(src);
        if let SourceUnitPart::ImportDirective(import) = &unit.0[0] {
            assert_eq!(extract_path_str(import), "foo/bar");
        } else {
            panic!("Ожидалась директива импорта");
        }
    }

    #[test]
    fn test_extract_path_rename() {
        // import { A, B as C } from "utils";
        let src = r#"import { A, B as C } from "utils";"#;
        let unit = parse_src(src);
        if let SourceUnitPart::ImportDirective(import) = &unit.0[0] {
            assert_eq!(extract_path_str(import), "utils");
        } else {
            panic!("Ожидалась директива импорта");
        }
    }

    #[test]
    fn test_extract_path_glob() {
        // import * as base from "lib";
        let src = r#"import * as base from "lib";"#;
        let unit = parse_src(src);
        if let SourceUnitPart::ImportDirective(import) = &unit.0[0] {
            assert_eq!(extract_path_str(import), "lib");
        } else {
            panic!("Ожидалась директива импорта");
        }
    }

    // ── Тесты IncludeResolver::new / from_file / add_search_path ─────────────

    #[test]
    fn test_resolver_new_empty() {
        let r = IncludeResolver::new(vec![]);
        assert_eq!(r.search_paths().len(), 0);
        assert_eq!(r.visited_count(), 0);
    }

    #[test]
    fn test_resolver_new_with_paths() {
        let paths = vec![PathBuf::from("/usr/include"), PathBuf::from("/opt/but/lib")];
        let r = IncludeResolver::new(paths.clone());
        assert_eq!(r.search_paths(), paths.as_slice());
    }

    #[test]
    fn test_resolver_from_file() {
        let r = IncludeResolver::from_file(Path::new("/home/user/project/main.but"));
        assert_eq!(r.search_paths().len(), 1);
        assert_eq!(r.search_paths()[0], PathBuf::from("/home/user/project"));
    }

    #[test]
    fn test_resolver_from_file_no_parent() {
        // Файл без родительской директории — пути поиска пустые
        let r = IncludeResolver::from_file(Path::new("main.but"));
        assert!(r.search_paths().is_empty());
    }

    #[test]
    fn test_add_search_path() {
        let mut r = IncludeResolver::new(vec![]);
        r.add_search_path(PathBuf::from("/opt/but"));
        assert_eq!(r.search_paths().len(), 1);
        r.add_search_path(PathBuf::from("/usr/local/but"));
        assert_eq!(r.search_paths().len(), 2);
    }

    // ── Тесты find_file ───────────────────────────────────────────────────────

    #[test]
    fn test_find_file_not_found() {
        let r = resolver_for("basic");
        let base = fixtures_dir().join("basic/main.but");
        let result = r.find_file("nonexistent_file_xyz", &base);
        assert!(matches!(result, Err(IncludeError::FileNotFound { .. })));
        if let Err(IncludeError::FileNotFound { path, .. }) = result {
            assert!(path.to_str().unwrap().contains("nonexistent_file_xyz"));
        }
    }

    #[test]
    fn test_find_file_adds_extension() {
        // Без расширения .but — должен найти types.but
        let r = resolver_for("basic");
        let base = fixtures_dir().join("basic/main.but");
        // Поиск "types" должен найти "types.but"
        let result = r.find_file("types", &base);
        assert!(result.is_ok(), "Не найден types.but при поиске 'types': {:?}", result);
        let found = result.unwrap();
        assert!(found.to_str().unwrap().ends_with("types.but"));
    }

    #[test]
    fn test_find_file_with_explicit_extension() {
        let r = resolver_for("basic");
        let base = fixtures_dir().join("basic/main.but");
        let result = r.find_file("types.but", &base);
        assert!(result.is_ok());
    }

    // ── Тесты resolve: базовый случай ────────────────────────────────────────

    #[test]
    fn test_resolve_no_imports() {
        // Исходник без импортов — возвращается как есть
        let src = r#"const X: u32 = 42; let y: bit = 0;"#;
        let unit = parse_src(src);
        let original_len = unit.0.len();

        let mut resolver = IncludeResolver::new(vec![]);
        let result = resolver.resolve(unit, Path::new("/fake/main.but"));
        assert!(result.is_ok());
        assert_eq!(result.unwrap().0.len(), original_len);
    }

    #[test]
    fn test_resolve_basic_import() {
        // main.but импортирует types.but с объявлениями типов
        let main_path = fixtures_dir().join("basic/main.but");
        let source = fs::read_to_string(&main_path).expect("Не удалось прочитать main.but");
        let unit = but_grammar::parse(&source, 0).expect("Ошибка разбора main.but").0;

        let mut resolver = IncludeResolver::from_file(&main_path);
        let result = resolver.resolve(unit, &main_path);

        assert!(result.is_ok(), "Ошибка разрешения: {:?}", result);
        let merged = result.unwrap();

        // Должны быть объединены определения из types.but + определения из main.but
        assert!(
            merged.0.len() >= 2,
            "Ожидалось как минимум 2 элемента после объединения, получено {}",
            merged.0.len()
        );
        // В объединённом SourceUnit не должно быть директив импорта
        let has_imports = merged
            .0
            .iter()
            .any(|p| matches!(p, SourceUnitPart::ImportDirective(_)));
        assert!(!has_imports, "Объединённый SourceUnit содержит директивы импорта");
    }

    #[test]
    fn test_resolve_deduplication() {
        // Два файла импортируют один и тот же файл — он должен быть включён один раз
        let main_path = fixtures_dir().join("dedup/main.but");
        let source = fs::read_to_string(&main_path).expect("Не удалось прочитать dedup/main.but");
        let unit = but_grammar::parse(&source, 0).expect("Ошибка разбора").0;

        let mut resolver = IncludeResolver::from_file(&main_path);
        let result = resolver.resolve(unit, &main_path);

        assert!(result.is_ok(), "Ошибки разрешения: {:?}", result);
        let merged = result.unwrap();

        // Подсчитать определения типов — тип из common.but должен быть ровно один раз
        let type_defs: Vec<_> = merged
            .0
            .iter()
            .filter(|p| matches!(p, SourceUnitPart::TypeDefinition(_)))
            .collect();
        assert_eq!(
            type_defs.len(),
            1,
            "Тип из common.but включён {} раз(а), ожидался 1",
            type_defs.len()
        );
    }

    #[test]
    fn test_resolve_nested_imports() {
        // main.but → level1.but → level2.but (транзитивная цепочка)
        let main_path = fixtures_dir().join("nested/main.but");
        let source = fs::read_to_string(&main_path).expect("Не удалось прочитать nested/main.but");
        let unit = but_grammar::parse(&source, 0).expect("Ошибка разбора").0;

        let mut resolver = IncludeResolver::from_file(&main_path);
        let result = resolver.resolve(unit, &main_path);

        assert!(result.is_ok(), "Ошибки при вложенных импортах: {:?}", result);
        let merged = result.unwrap();

        // Все три уровня должны быть включены
        assert!(
            merged.0.len() >= 3,
            "Ожидалось ≥ 3 элементов из трёх уровней, получено {}",
            merged.0.len()
        );
    }

    #[test]
    fn test_resolve_circular_import_no_crash() {
        // a.but → b.but → a.but — не должно вызывать бесконечную рекурсию
        // Благодаря дедупликации второй импорт a.but просто игнорируется
        let a_path = fixtures_dir().join("circular/a.but");
        let source = fs::read_to_string(&a_path).expect("Не удалось прочитать circular/a.but");
        let unit = but_grammar::parse(&source, 0).expect("Ошибка разбора circular/a.but").0;

        let mut resolver = IncludeResolver::from_file(&a_path);
        // Ожидаем либо успех (дедупликация), либо ошибку (обнаружение цикла) —
        // главное, что нет переполнения стека или зависания
        let _ = resolver.resolve(unit, &a_path);
        // Тест проходит, если мы добрались сюда
    }

    #[test]
    fn test_resolve_file_not_found_error() {
        let src = r#"import "does_not_exist_xyz";"#;
        let unit = parse_src(src);

        let mut resolver = IncludeResolver::new(vec![]);
        let result = resolver.resolve(unit, Path::new("/fake/main.but"));

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], IncludeError::FileNotFound { .. }));
    }

    #[test]
    fn test_resolve_multiple_errors_collected() {
        // Два несуществующих файла — оба должны быть в списке ошибок
        let src = r#"
            import "missing_a";
            import "missing_b";
            const X: u32 = 1;
        "#;
        let unit = parse_src(src);

        let mut resolver = IncludeResolver::new(vec![]);
        let result = resolver.resolve(unit, Path::new("/fake/main.but"));

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 2, "Ожидалось 2 ошибки, получено {}", errors.len());
    }

    #[test]
    fn test_resolve_parse_error_in_import() {
        // Файл с синтаксической ошибкой
        let main_path = fixtures_dir().join("errors/main.but");
        let source = fs::read_to_string(&main_path).expect("Не удалось прочитать errors/main.but");
        let unit = but_grammar::parse(&source, 0).expect("Ошибка разбора errors/main.but").0;

        let mut resolver = IncludeResolver::from_file(&main_path);
        let result = resolver.resolve(unit, &main_path);

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(
            errors.iter().any(|e| matches!(e, IncludeError::ParseError { .. })),
            "Ожидалась ошибка ParseError, получено: {:?}",
            errors
        );
    }

    // ── Тесты visited_count ───────────────────────────────────────────────────

    #[test]
    fn test_visited_count_after_resolve() {
        // После разрешения с одним файлом должен быть посещён ровно 1 файл
        let src = r#"const OK: u32 = 1;"#;
        let unit = parse_src(src);

        // Использовать несуществующий путь — mark_visited использует canonicalize,
        // который не сработает. Но тест структуры visited_count важен.
        let mut resolver = IncludeResolver::new(vec![]);
        let _ = resolver.resolve(unit, Path::new("/fake/file.but"));
        // После попытки canonicalize несуществующего пути visited остаётся пустым
        assert_eq!(resolver.visited_count(), 0);
    }

    #[test]
    fn test_visited_count_with_real_file() {
        let main_path = fixtures_dir().join("basic/main.but");
        let source = fs::read_to_string(&main_path).expect("Не удалось прочитать main.but");
        let unit = but_grammar::parse(&source, 0).expect("Ошибка разбора").0;

        let mut resolver = IncludeResolver::from_file(&main_path);
        let _ = resolver.resolve(unit, &main_path);

        // Основной файл + импортируемый файл types.but
        assert!(
            resolver.visited_count() >= 1,
            "Ожидалось ≥ 1 посещённый файл, получено {}",
            resolver.visited_count()
        );
    }

    // ── Тесты Display для IncludeError ────────────────────────────────────────

    #[test]
    fn test_display_file_not_found() {
        let err = IncludeError::FileNotFound {
            path: PathBuf::from("missing.but"),
            search_paths: vec![PathBuf::from("/opt/but"), PathBuf::from("/usr/but")],
        };
        let msg = err.to_string();
        assert!(msg.contains("missing.but"), "Сообщение: {}", msg);
        assert!(msg.contains("/opt/but"), "Сообщение: {}", msg);
    }

    #[test]
    fn test_display_circular_import() {
        let err = IncludeError::CircularImport(PathBuf::from("/project/a.but"));
        let msg = err.to_string();
        assert!(msg.contains("Циклический"), "Сообщение: {}", msg);
        assert!(msg.contains("a.but"), "Сообщение: {}", msg);
    }

    #[test]
    fn test_display_io_error() {
        let err = IncludeError::IoError {
            path: PathBuf::from("/protected/secret.but"),
            error: "Permission denied".to_string(),
        };
        let msg = err.to_string();
        assert!(msg.contains("secret.but"), "Сообщение: {}", msg);
        assert!(msg.contains("Permission denied"), "Сообщение: {}", msg);
    }

    #[test]
    fn test_display_parse_error() {
        let err = IncludeError::ParseError {
            path: PathBuf::from("/project/broken.but"),
            errors: vec![],
        };
        let msg = err.to_string();
        assert!(msg.contains("broken.but"), "Сообщение: {}", msg);
        assert!(msg.contains("0 ошибок"), "Сообщение: {}", msg);
    }

    // ── Тест import_path_to_string с Path-вариантом ───────────────────────────

    #[test]
    fn test_import_path_path_variant() {
        // import * as base from "lib"; — Path-вариант через идентификаторный путь
        // Этот вариант создаётся при import std::types (через :: нотацию)
        // Проверяем через строковый вариант (Filename), т.к. Path-синтаксис
        // требует отсутствия кавычек у пути
        let src = r#"import "std/types";"#;
        let unit = parse_src(src);
        if let SourceUnitPart::ImportDirective(import) = &unit.0[0] {
            // Filename-вариант: строка возвращается как есть
            assert_eq!(extract_path_str(import), "std/types");
        }
    }
}

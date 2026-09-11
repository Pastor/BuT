//! Разбор `initializationOptions` LSP: пути поиска импортов и язык сообщений.
//!
//! Без них ядро получает пустой список путей (`&[]`), и импорт из общей библиотеки вне
//! каталога документа в редакторе не находится, хотя `taktc -I lib` его собирает.
//! Здесь - чистое, тестируемое извлечение путей из штатного `initializationOptions`;
//! бинарник остаётся тонкой обвязкой.

use serde_json::Value;
use std::path::Path;

use crate::diagnostics::lang::{self, Lang};

/// Язык сообщений из `initializationOptions.lang`; `None` - клиент язык не назвал.
///
/// Неизвестный код - отказ словами с перечислением известных, а решает вызывающий:
/// сервер пишет его в журнал и остаётся на прежнем языке, а не падает на старте.
pub fn lang_from_options(options: Option<&Value>) -> Option<Result<Lang, String>> {
    let code = options?.get("lang")?.as_str()?;
    Some(lang::parse(code))
}

/// Извлекает пути поиска импортов (аналог `-I` у `taktc`) из `initializationOptions`
/// LSP.
///
/// Читает массив строк по ключу `searchPaths`. Относительный путь разрешается от корня
/// рабочей области `root` (обычно `InitializeParams.root_uri`, приведённый к пути);
/// абсолютный - как есть; при `root == None` относительный остаётся как есть (CWD
/// сервера, как `-I`). Порядок путей сохраняется: одноимённый файл из `searchPaths`
/// перекрывает локальный, как и в ядре. Битые записи (не массив, элемент не строка)
/// молча пропускаются: плохая настройка не должна ронять сервер.
pub fn search_paths_from_options(options: Option<&Value>, root: Option<&str>) -> Vec<String> {
    let Some(arr) = options
        .and_then(|o| o.get("searchPaths"))
        .and_then(Value::as_array)
    else {
        return Vec::new();
    };
    arr.iter()
        .filter_map(Value::as_str)
        .map(|s| resolve_path(s, root))
        .collect()
}

/// Абсолютный путь - как есть; относительный - от корня рабочей области, если он
/// известен, иначе как есть (CWD сервера, как `-I` у `taktc`).
fn resolve_path(path: &str, root: Option<&str>) -> String {
    let p = Path::new(path);
    if p.is_absolute() {
        return path.to_string();
    }
    match root {
        Some(r) => Path::new(r).join(p).to_string_lossy().into_owned(),
        None => path.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn reads_language() {
        let opts = json!({ "lang": "en" });
        let chosen = lang_from_options(Some(&opts)).expect("язык назван");
        assert_eq!(chosen.expect("язык известен").code(), "en");
    }

    #[test]
    fn language_absent_or_not_a_string_gives_none() {
        assert!(lang_from_options(None).is_none());
        assert!(lang_from_options(Some(&json!({}))).is_none());
        assert!(lang_from_options(Some(&json!({ "lang": 7 }))).is_none());
    }

    #[test]
    fn unknown_language_is_refused_with_the_known_ones() {
        let refused = lang_from_options(Some(&json!({ "lang": "xx" })))
            .expect("язык назван")
            .expect_err("язык неизвестен");
        assert!(
            refused.contains("en") && refused.contains("ru"),
            "{refused}"
        );
    }

    #[test]
    fn reads_search_paths_array() {
        let opts = json!({ "searchPaths": ["/abs/lib", "/abs/shared"] });
        assert_eq!(
            search_paths_from_options(Some(&opts), None),
            vec!["/abs/lib".to_string(), "/abs/shared".to_string()]
        );
    }

    #[test]
    fn relative_resolved_from_root_absolute_kept() {
        let opts = json!({ "searchPaths": ["lib", "/abs/shared"] });
        let paths = search_paths_from_options(Some(&opts), Some("/work/project"));
        assert_eq!(
            paths,
            vec![
                Path::new("/work/project")
                    .join("lib")
                    .to_string_lossy()
                    .into_owned(),
                "/abs/shared".to_string(),
            ]
        );
    }

    #[test]
    fn relative_without_root_kept_as_is() {
        let opts = json!({ "searchPaths": ["lib"] });
        assert_eq!(
            search_paths_from_options(Some(&opts), None),
            vec!["lib".to_string()]
        );
    }

    #[test]
    fn preserves_order() {
        let opts = json!({ "searchPaths": ["/z", "/a", "/m"] });
        assert_eq!(
            search_paths_from_options(Some(&opts), None),
            vec!["/z".to_string(), "/a".to_string(), "/m".to_string()]
        );
    }

    #[test]
    fn none_options_gives_empty() {
        assert!(search_paths_from_options(None, None).is_empty());
    }

    #[test]
    fn missing_key_gives_empty() {
        let opts = json!({ "other": 1 });
        assert!(search_paths_from_options(Some(&opts), None).is_empty());
    }

    #[test]
    fn non_array_gives_empty() {
        let opts = json!({ "searchPaths": "lib" });
        assert!(search_paths_from_options(Some(&opts), None).is_empty());
    }

    #[test]
    fn non_string_entries_skipped() {
        let opts = json!({ "searchPaths": ["/lib", 42, null, "/shared"] });
        assert_eq!(
            search_paths_from_options(Some(&opts), None),
            vec!["/lib".to_string(), "/shared".to_string()]
        );
    }
}

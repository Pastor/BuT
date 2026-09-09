//! Кто подключает эту библиотеку: поиск импортёров файла.
//!
//! # Границы (названы, а не забыты)
//!
//! - Ищем в **каталоге входного файла** и в каталогах `-I`. Импортёр,
//!   лежащий глубже по дереву или в чужом проекте, не найдётся - то же
//!   ограничение, что у рабочей области языкового сервера (: "полнота
//!   в пределах области или отказ").
//! - **Неразобранный сосед пропускается молча.** Он мог бы импортировать
//!   библиотеку, но сказать этого нельзя: АСД у него нет. Ошибка в сторону
//!   "подсказки нет", а не ложной подсказки.
//! - Сравнение идёт по **разрешённому пути**, а не по тексту директивы: одна и
//!   та же библиотека подключается и как `"helper.takt"`, и как `"./helper.takt"`,
//!   и через `-I`.

use crate::diagnostics::lang::keys;
use crate::msg;
use super::read_import_file;
use crate::parser::ast;
use crate::parser::ast::ImportPath;
use std::path::Path;

/// Сколько импортёров называть в подсказке.
///
/// Список - подсказка, а не отчёт: десять путей в тексте ошибки читаются хуже, чем три
/// и "и ещё N".
const MAX_LISTED: usize = 3;

/// Ищет файлы, подключающие `target`, в его каталоге и в каталогах поиска.
///
/// Возвращает пути в том виде, в каком их удобно показать автору. Пустой список значит
/// "не нашли" - и подсказки тогда не будет вовсе.
pub fn find_importers(target: &str, search_paths: &[String]) -> Vec<String> {
    let target_key = match normalize(target) {
        Some(key) => key,
        None => return Vec::new(),
    };

    let target_dir = match Path::new(target).parent() {
        Some(parent) if !parent.as_os_str().is_empty() => parent.to_string_lossy().to_string(),
        _ => ".".to_string(),
    };
    let mut dirs: Vec<String> = vec![target_dir.clone()];
    for path in search_paths {
        if !dirs.contains(path) {
            dirs.push(path.clone());
        }
    }

    let mut found: Vec<String> = Vec::new();
    for dir in &dirs {
        let Ok(entries) = std::fs::read_dir(dir) else {
            continue;
        };
        let mut paths: Vec<std::path::PathBuf> = entries
            .flatten()
            .map(|e| e.path())
            .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("takt"))
            .collect();
        // Порядок обхода каталога системно-зависим, а текст диагностики обязан быть
        // воспроизводимым.
        paths.sort();
        for path in paths {
            let candidate = path.to_string_lossy().to_string();
            if normalize(&candidate) == Some(target_key.clone()) {
                continue; // сам файл импортёром себе не бывает
            }
            if found.contains(&candidate) {
                continue;
            }
            if imports(&candidate, &target_key, search_paths, &target_dir) {
                found.push(candidate);
            }
        }
    }
    found
}

/// Подключает ли файл `candidate` библиотеку с ключом `target_key`.
///
/// Неразобранный файл отвечает "нет": директив импорта у него нет, потому что нет АСД
/// (та же граница, что у `unparsable_consumers` рабочей области).
fn imports(candidate: &str, target_key: &str, search_paths: &[String], target_dir: &str) -> bool {
    let Ok(source) = std::fs::read_to_string(candidate) else {
        return false;
    };
    let Ok((model, _)) = crate::parse(&source, 0) else {
        return false;
    };
    // Пути поиска импортёра: переданные `-I`, **каталог самой библиотеки** и
    // собственный каталог кандидата - последним, тем же правилом, что и при компиляции.
    //
    // Каталог библиотеки добавляется намеренно: импортёр мог собираться с другим `-I`,
    // и без этого допущения файл из каталога поиска, подключающий нашу библиотеку, не
    // нашёлся бы вовсе. Ошибка в сторону лишнего имени, а не молчания.
    let mut dirs: Vec<String> = search_paths.to_vec();
    if !dirs.iter().any(|d| d == target_dir) {
        dirs.push(target_dir.to_string());
    }
    if let Some(parent) = Path::new(candidate).parent() {
        let dir = parent.to_string_lossy().to_string();
        dirs.push(if dir.is_empty() { ".".to_string() } else { dir });
    }
    import_paths(&model)
        .iter()
        .filter_map(|path| read_import_file(&dirs, path).ok())
        .any(|(_, filename)| normalize(&filename).as_deref() == Some(target_key))
}

/// Пути директив `import` верхнего уровня.
fn import_paths(model: &ast::Model) -> Vec<ImportPath> {
    model
        .elements
        .iter()
        .filter_map(|element| match element {
            // Все три формы директивы несут путь первым полем: `import "p";`, `import
            // "p" as M;`, `import { a } from "p";`. Разбор исчерпывающий - новая форма
            // обязана заставить принять решение.
            ast::ModelElement::Import(ast::ImportDefine::Plain(path, _))
            | ast::ModelElement::Import(ast::ImportDefine::GlobalSymbol(path, _, _))
            | ast::ModelElement::Import(ast::ImportDefine::Rename(path, _, _)) => {
                Some(path.clone())
            }
            _ => None,
        })
        .collect()
}

/// Ключ сравнения путей - канонический путь, если файл существует.
fn normalize(path: &str) -> Option<String> {
    std::fs::canonicalize(path)
        .ok()
        .map(|p| p.to_string_lossy().to_string())
}

/// Заметка "библиотеку подключают такие-то файлы" либо `None`, если не нашли.
pub fn importers_note(target: &str, search_paths: &[String]) -> Option<String> {
    let found = find_importers(target, search_paths);
    if found.is_empty() {
        return None;
    }
    let listed: Vec<&str> = found.iter().take(MAX_LISTED).map(String::as_str).collect();
    let mut text = msg!(keys::IMPORT_IMPORTERS_NOTE, listed = listed.join(", "));
    if found.len() > MAX_LISTED {
        text.push_str(&msg!(keys::IMPORT_IMPORTERS_MORE, count = found.len() - MAX_LISTED));
    }
    Some(text)
}

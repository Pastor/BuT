extern crate lalrpop;

use std::collections::BTreeMap;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    build_message_catalogues()?;
    unsafe {
        std::env::set_var("LALRPOP_LANE_TABLE", "disabled");
    }
    lalrpop::Configuration::new()
        .always_use_colors()
        // .use_cargo_dir_conventions()
        .emit_rerun_directives(true)
        .process_current_dir()?;
    Ok(())
}

/// Базовый язык: его набор ключей задаёт константы, с ним сверяются остальные.
const BASE_LANG: &str = "ru";

/// Строит каталоги сообщений из `messages/*.txt`.
///
/// Языков **открытый список**: третий язык - это
/// третий файл в каталоге, без единой правки кода. Отсюда и сканирование
/// каталога вместо перечисления имён: список, повторённый в исходнике, разошёлся
/// бы с файлами молча.
///
/// Здесь же строятся **константы ключей**: ключ, написанный в коде строкой,
/// ошибался бы молча - сообщение просто не нашлось бы. Константа делает опечатку
/// отказом компиляции.
///
/// Паритет каталогов тут не проверяется - это предмет проверки
/// `scripts/check-messages.py`: сборка, падающая на неполном переводе, лишила бы
/// переводчика возможности собрать дерево в процессе работы.
fn build_message_catalogues() -> Result<(), Box<dyn std::error::Error>> {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("messages");
    println!("cargo:rerun-if-changed={}", dir.display());

    let mut catalogues: BTreeMap<String, BTreeMap<String, String>> = BTreeMap::new();
    for entry in std::fs::read_dir(&dir)? {
        let path = entry?.path();
        if path.extension().is_none_or(|e| e != "txt") {
            continue;
        }
        println!("cargo:rerun-if-changed={}", path.display());
        let lang = path
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| format!("нечитаемое имя каталога сообщений: {}", path.display()))?
            .to_string();
        catalogues.insert(lang, parse_catalogue(&path)?);
    }

    let base = catalogues
        .get(BASE_LANG)
        .ok_or_else(|| format!("нет базового каталога сообщений messages/{BASE_LANG}.txt"))?;

    let mut out =
        String::from("// Построено `build.rs` из `messages/*.txt` — не править руками.\n");

    out.push_str("/// Коды языков, для которых в дереве есть каталог сообщений.\n");
    out.push_str("pub const LANGS: &[&str] = &[\n");
    for lang in catalogues.keys() {
        out.push_str(&format!("    {},\n", quote(lang)));
    }
    out.push_str("];\n\n");

    out.push_str("/// Каталоги: язык → отсортированные пары «ключ, текст».\n");
    out.push_str("static CATALOGUES: &[(&str, &[(&str, &str)])] = &[\n");
    for (lang, entries) in &catalogues {
        out.push_str(&format!("    ({}, &[\n", quote(lang)));
        for (key, text) in entries {
            out.push_str(&format!("        ({}, {}),\n", quote(key), quote(text)));
        }
        out.push_str("    ]),\n");
    }
    out.push_str("];\n\n");

    out.push_str("/// Ключи сообщений: имя константы строится из ключа базового каталога.\n");
    out.push_str("pub mod keys {\n    use super::Key;\n");
    for key in base.keys() {
        out.push_str(&format!(
            "    /// `{key}`\n    pub const {}: Key = Key({});\n",
            const_name(key),
            quote(key)
        ));
    }
    out.push_str("}\n");

    let target = Path::new(&std::env::var("OUT_DIR")?).join("messages.rs");
    std::fs::write(target, out)?;
    Ok(())
}

/// Разбирает один каталог: `ключ = текст`, `#` - комментарий, пустые строки - прочь.
fn parse_catalogue(path: &Path) -> Result<BTreeMap<String, String>, Box<dyn std::error::Error>> {
    let text = std::fs::read_to_string(path)?;
    let mut entries = BTreeMap::new();
    for (no, line) in text.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let (key, value) = line
            .split_once('=')
            .ok_or_else(|| format!("{}:{}: строка без '='", path.display(), no + 1))?;
        let key = key.trim().to_string();
        if key.is_empty() {
            return Err(format!("{}:{}: пустой ключ", path.display(), no + 1).into());
        }
        if entries
            .insert(key.clone(), unescape(value.trim()))
            .is_some()
        {
            return Err(format!("{}:{}: ключ '{key}' повторён", path.display(), no + 1).into());
        }
    }
    Ok(entries)
}

/// Раскрывает экранирование текста каталога: `\n` - перевод строки, `\\` - слеш.
fn unescape(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => out.push('\n'),
            Some(other) => out.push(other),
            None => out.push('\\'),
        }
    }
    out
}

/// Имя константы из ключа: `se-034.local-type-not-found` -> `SE_034_LOCAL_TYPE_NOT_FOUND`.
fn const_name(key: &str) -> String {
    key.chars()
        .map(|c| match c {
            '-' | '.' => '_',
            other => other.to_ascii_uppercase(),
        })
        .collect()
}

/// Строковый литерал Rust: экранируются кавычка и обратный слеш.
fn quote(text: &str) -> String {
    let mut out = String::with_capacity(text.len() + 2);
    out.push('"');
    for c in text.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            other => out.push(other),
        }
    }
    out.push('"');
    out
}

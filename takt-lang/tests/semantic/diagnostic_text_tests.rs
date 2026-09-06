//! в тексте диагностики нет внутреннего представления.
//!
//!
//! ```text
//! Неразрешённое условие: Variable(Identifier { loc: Source(0, 51, 54), name: "qqq" })
//! ```
//!
//! Каждый раз дефект находили **глазами**. Этот тест ищет его машиной: он прогоняет
//! диагностики на исправлениетурах и проверяет их **тексты** на признаки внутреннего
//! представления.

use std::path::{Path, PathBuf};

/// Подстроки, выдающие внутреннее представление в тексте сообщения.
///
/// Список взят из **реальных** дампов, которые видел пользователь: `Source(0, 51, 54)`
/// (позиция), `Identifier { ... }` (узел АСД), `Integer { bits: 8, ... }` (тип).
/// Признак `loc:` ловит любой узел, несущий позицию, - их большинство.
const INTERNALS: &[&str] = &[
    "Source(",
    "loc:",
    "Identifier {",
    "Integer {",
    "RefCell",
    "ConditionNode::",
    "ExpressionNode::",
    "VariableNode::",
    "TypeNode::",
];

/// Каталог исправлениетур семантики: `valid/` и `invalid/` - оба.
fn fixtures() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/data");
    let mut files = Vec::new();
    collect(&root, &mut files);
    files.sort();
    files
}

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("каталог фикстур {} не читается: {e}", dir.display()));
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect(&path, out);
        } else if path.extension().is_some_and(|e| e == "takt") {
            out.push(path);
        }
    }
}

/// Пробы на диагностики, которых исправлениетуры не порождают.
///
/// Замер (2026-08-05): прогон по исправлениетурам даёт 98 сообщений, и `SE-025` среди них
/// **нет** - то есть один только обход исправлениетур не заметил бы возврата дампа в то самое
/// сообщение, с которого начался разбор. Тест без этого списка был бы декоративным.
const PROBES: &[(&str, &str)] = &[
    (
        "SE-025: неразрешённое условие",
        "model M {\n    var x: u8 := 0;\n    start S {\n        ref T: qqq > 0;\n    }\n    state T;\n}\nstart Main = M;\n",
    ),
    (
        "SE-003: имя не найдено в выражении",
        "model M {\n    var x: u8 := 0;\n    start S {\n        always {\n            x := nope + 1;\n        }\n    }\n}\nstart Main = M;\n",
    ),
    (
        "SE-034: тип не найден",
        "model M {\n    var v: NoSuchType := 1;\n    start S {\n        always {\n            v := 1;\n        }\n    }\n}\nstart Main = M;\n",
    ),
    (
        "SE-059/SE-065: смешение типов",
        "model M {\n    var q1: q(8, 8) := 1.5;\n    var i1: u8 := 2;\n    start S {\n        always {\n            q1 := q1 + i1;\n        }\n    }\n}\nstart Main = M;\n",
    ),
];

/// Проверяет один текст; возвращает найденный признак.
fn internal_in(message: &str) -> Option<&'static str> {
    INTERNALS.iter().copied().find(|m| message.contains(m))
}

/// **Главная проверка.** Ни одна диагностика, порождённая исправлениетурами, не
/// показывает внутреннее представление.
///
/// Прогон идёт через `collect_compile_diagnostics` - тот же вход, которым пользуются и
/// `taktc`, и языковой сервер, то есть проверяются тексты, которые действительно видит
/// человек.
#[test]
fn no_internal_representation_in_diagnostic_texts() {
    let files = fixtures();
    assert!(files.len() >= 50, "фикстуры не найдены: {}", files.len());

    let mut checked = 0usize;
    let mut offenders: Vec<String> = Vec::new();
    let search_paths = vec![
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/data/include")
            .display()
            .to_string(),
    ];

    let mut check = |label: &str, source: &str, checked: &mut usize| {
        for diagnostic in
            takt_lang::collect_compile_diagnostics(label, source, &search_paths, false)
        {
            *checked += 1;
            if let Some(marker) = internal_in(&diagnostic.message) {
                offenders.push(format!(
                    "{label}: [{}] содержит {marker:?}: {}",
                    diagnostic.code.as_deref().unwrap_or("?"),
                    diagnostic.message
                ));
            }
        }
        // Предупреждения - тот же канал, что у `taktc compile`: они тоже текст, который
        // читает человек.
        if let Ok((ast, _)) = takt_lang::parse(source, 0)
            && let Ok(model) = takt_lang::semantic::tree::construct_model(&ast, None, &search_paths)
        {
            for diagnostic in takt_lang::semantic::warnings::collect_model_warnings(&ast, &model) {
                *checked += 1;
                if let Some(marker) = internal_in(&diagnostic.message) {
                    offenders.push(format!(
                        "{label}: предупреждение [{}] содержит {marker:?}: {}",
                        diagnostic.code.as_deref().unwrap_or("?"),
                        diagnostic.message
                    ));
                }
            }
        }
    };

    for file in &files {
        let Ok(source) = std::fs::read_to_string(file) else {
            continue;
        };
        let name = file.display().to_string();
        check(&name, &source, &mut checked);
    }
    for (label, source) in PROBES {
        check(label, source, &mut checked);
    }

    eprintln!(
        "── Тексты диагностик: проверено {checked} сообщений на {} фикстурах ──",
        files.len()
    );
    assert!(
        checked >= 20,
        "фикстуры не породили диагностик — сторож ничего не проверил ({checked})"
    );
    assert!(
        offenders.is_empty(),
        "внутреннее представление в тексте диагностики:\n{}",
        offenders.join("\n")
    );
}

/// `SE-025` цитирует **запись автора**, а не дамп узла.
///
/// Адресная проверка того самого сообщения, с которого начался разбор: цитата
/// печатается форматтером, поэтому в ней стоит текст исходника.
#[test]
fn se025_quotes_the_source_text() {
    let source = "model M {\n    var x: u8 := 0;\n    start S {\n        ref T: qqq > 0;\n    }\n    state T;\n}\nstart Main = M;\n";
    let diagnostics = takt_lang::collect_compile_diagnostics("probe.takt", source, &[], false);

    let se025 = diagnostics
        .iter()
        .find(|d| d.code.as_deref() == Some("SE-025"))
        .unwrap_or_else(|| panic!("ожидалась SE-025: {diagnostics:?}"));

    assert!(
        se025.message.contains("'qqq'"),
        "сообщение обязано цитировать запись автора: {:?}",
        se025.message
    );
    assert_eq!(
        internal_in(&se025.message),
        None,
        "внутреннее представление в тексте: {:?}",
        se025.message
    );
    assert!(
        se025.message.contains("имя не найдено"),
        "текст обязан называть причину: {:?}",
        se025.message
    );
}

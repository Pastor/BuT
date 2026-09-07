//! Диагностика цели `c` несёт код - `CC-022` / `CC-023`.
//!
//! ## Что здесь ловится
//!
//! Таких мест в цели было **девятнадцать**.
//!
//! Соседи на том же входе отвечали кодом и причиной: `ST-011` ("в IEC 61131-3 нет
//! операции среза"), `RS-011` ("в no_std нет alloc"), `SV-002`, а эталон - `SIM-014`.
//! Отставала одна цель.

use takt_lang::GenerateOptions;

/// Каталог фикстур.
const DIR: &str = "tests/data/cdiag0212";

fn fixture(name: &str) -> (String, String) {
    let path = format!("{DIR}/{name}");
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("не прочитать {path}: {e}"));
    (path, source)
}

/// Каталог вывода, уникальный по имени потока (тесты идут параллельно, 0190). Не
/// создаётся: отказ приходит до записи файлов.
fn out_dir(tag: &str) -> String {
    let thread = std::thread::current()
        .name()
        .unwrap_or("unnamed")
        .replace(':', "_");
    format!(
        "{}/takt0212_{thread}_{tag}",
        std::env::temp_dir()
            .join(format!("takt_pid{}", std::process::id()))
            .display()
    )
}

/// Компилирует фикстуру целью `c` и возвращает ошибку.
fn refusal(name: &str) -> takt_lang::diagnostics::Diagnostic {
    let (path, source) = fixture(name);
    let search: [String; 0] = [];
    takt_lang::compile_to_c(
        &path,
        &source,
        &out_dir(name),
        &search,
        &GenerateOptions::default(),
    )
    .expect_err("ожидался отказ цели c")
}

// -- Достижимые входы отвечают CC-022 ----------------------------------

#[test]
fn model_in_expression_is_cc022() {
    let diagnostic = refusal("model_expr.takt");
    assert_eq!(
        diagnostic.code.as_deref(),
        Some("CC-022"),
        "получено: {diagnostic:?}"
    );
    assert!(
        diagnostic.message.contains("ссылка на модель"),
        "вид конструкции обязан быть назван по-русски: {}",
        diagnostic.message
    );
    assert!(
        diagnostic.message.contains("под-модел"),
        "причина обязана быть названа (образец ST-011/RS-011): {}",
        diagnostic.message
    );
}

#[test]
fn array_slice_is_cc022_with_position() {
    let diagnostic = refusal("array_slice.takt");
    assert_eq!(
        diagnostic.code.as_deref(),
        Some("CC-022"),
        "получено: {diagnostic:?}"
    );
    assert!(
        matches!(
            diagnostic.loc,
            takt_lang::diagnostics::Location::Source(_, _, _)
        ),
        "у среза позиция ЕСТЬ (её несёт переменная) и обязана доехать: {:?}",
        diagnostic.loc
    );
    assert!(
        diagnostic.file.is_some(),
        "путь файла обязан быть проставлен: без него позиция не печатается \
         (класс фикса 0228). Получено: {diagnostic:?}"
    );
}

// -- В текстах нет внутреннего представления -----------------

/// Имена вариантов АСД, печатавшиеся в сообщениях до фичи.
const AST_NAMES: [&str; 7] = [
    "ArraySlice",
    "CodeBlock",
    "NamedFunctionBox",
    "List",
    "Type",
    "Address",
    "Model",
];

#[test]
fn refusal_texts_do_not_leak_ast_variant_names() {
    let mut leaked = Vec::new();
    for fixture in ["model_expr.takt", "array_slice.takt"] {
        let message = refusal(fixture).message;
        for name in AST_NAMES {
            if message.contains(name) {
                leaked.push(format!("{fixture}: '{name}' в тексте — {message}"));
            }
        }
    }
    assert!(
        leaked.is_empty(),
        "имена вариантов АСД в сообщениях (класс 0231):\n{}",
        leaked.join("\n")
    );
}

// -- Машинный тест по исходнику генераторов -----------------------------

/// Файлы генераторов, кроме самих воронок (в их доке цитируется старая форма).
fn generator_sources() -> Vec<(String, String)> {
    fn walk(dir: &std::path::Path, out: &mut Vec<(String, String)>) {
        let entries = std::fs::read_dir(dir)
            .unwrap_or_else(|e| panic!("не прочитать каталог {}: {e}", dir.display()));
        for entry in entries {
            let path = entry.expect("запись каталога").path();
            if path.is_dir() {
                walk(&path, out);
            } else if path.extension().is_some_and(|e| e == "rs") {
                let name = path.to_string_lossy().to_string();
                // Воронки цитируют прежнюю форму в документации - это описание класса,
                // а не его источник.
                if name.ends_with("c_unsupported.rs") || name.ends_with("c_unresolved.rs") {
                    continue;
                }
                let text = std::fs::read_to_string(&path)
                    .unwrap_or_else(|e| panic!("не прочитать {name}: {e}"));
                out.push((name, text));
            }
        }
    }
    let mut out = Vec::new();
    walk(std::path::Path::new("src/generator"), &mut out);
    assert!(
        out.len() > 20,
        "обход генераторов нашёл {} файлов — это не похоже на правду; \
         сторож обязан падать при пропаже каталога (урок фичи 0230)",
        out.len()
    );
    out
}

#[test]
fn no_generator_builds_a_diagnostic_without_code() {
    let mut faceless = Vec::new();
    for (name, text) in generator_sources() {
        for (number, line) in text.lines().enumerate() {
            let trimmed = line.trim_start();
            // Строка документации класс не заводит.
            if trimmed.starts_with("//") {
                continue;
            }
            // `Err("...".into())` и `format!(...).as_str().into()` - обе формы
            // конверсии `From<&str> for Diagnostic`: код `None`, позиция `Source(0, 0,
            // 0)`.
            if trimmed.contains(".into())") || trimmed.starts_with("return Err(\"") {
                faceless.push(format!("{name}:{}: {}", number + 1, line.trim()));
            }
        }
    }
    assert!(
        faceless.is_empty(),
        "диагностики без кода в генераторах (заводите отказ через воронку \
         `c_unsupported::refuse` либо `c_unresolved::refuse`):\n{}",
        faceless.join("\n")
    );
}

// -- Поведение не изменилось ----------------------------------------------

#[test]
fn debug_builtin_is_still_skipped_silently() {
    let (path, source) = fixture("debug_call.takt");
    let search: [String; 0] = [];
    // Пропуск `debug`/`S` в позиции оператора - решение: эти
    // встроенные функции кода не порождают. меняет текст отказа, а не
    // судьбу оператора.
    takt_lang::compile_to_c(
        &path,
        &source,
        &out_dir("debug_call"),
        &search,
        &GenerateOptions::default(),
    )
    .expect("вызов debug в позиции оператора по-прежнему пропускается молча");
}

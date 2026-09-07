//! Возврат параметра-массива у цели `rust`.
//!
//! # Что проверяется
//!
//! Разыменование печатается в **обоих** путях возврата - хвостовом выражении и явном
//! `return`. Контроль: индексация того же имени звёздочки не получает - над ссылкой она
//! работает сама, а лишняя дала бы `(*a)[0]`, которую `clippy` отвергает.

use takt_lang::GenerateOptions;

/// Возврат параметра хвостом тела (единственный оператор).
const TAIL: &str = "model Probe {\n\
     \x20   var seen: [u8; 2] := {0, 0};\n\
     \x20   var src: [u8; 2] := {1, 2};\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out ticks_out: u8 at 0x300;\n\
     \x20   fn pick(a: [u8; 2]) -> [u8; 2] { return a; }\n\
     \x20   start Cycle {\n\
     \x20       always { ticks := ticks + 1; seen := pick(src); ticks_out := ticks; }\n\
     \x20       ref Cycle: ticks < 200;\n\
     \x20   }\n\
     }\n\
     start Main = Probe;\n";

/// Ранний возврат параметра: печатается вторым путём - явным `return`.
const EARLY: &str = "model Probe {\n\
     \x20   var seen: [u8; 2] := {0, 0};\n\
     \x20   var src: [u8; 2] := {1, 2};\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out ticks_out: u8 at 0x300;\n\
     \x20   fn pick(a: [u8; 2], flag: bit) -> [u8; 2] {\n\
     \x20       if flag = 1 { return a; }\n\
     \x20       return {7, 8};\n\
     \x20   }\n\
     \x20   start Cycle {\n\
     \x20       always { ticks := ticks + 1; seen := pick(src, 1); ticks_out := ticks; }\n\
     \x20       ref Cycle: ticks < 200;\n\
     \x20   }\n\
     }\n\
     start Main = Probe;\n";

/// Индексация параметра - контроль: разыменование здесь не нужно.
const INDEX: &str = "model Probe {\n\
     \x20   var seen: u8 := 0;\n\
     \x20   var src: [u8; 2] := {1, 2};\n\
     \x20   var ticks: u8 := 0;\n\
     \x20   out ticks_out: u8 at 0x300;\n\
     \x20   fn first(a: [u8; 2]) -> u8 { return a[0]; }\n\
     \x20   start Cycle {\n\
     \x20       always { ticks := ticks + 1; seen := first(src); ticks_out := ticks + seen; }\n\
     \x20       ref Cycle: ticks < 200;\n\
     \x20   }\n\
     }\n\
     start Main = Probe;\n";

fn emit_rust(source: &str, tag: &str) -> String {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0494_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    takt_lang::compile_to_rust(
        "probe",
        source,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("цель `rust` переводит");
    let text = std::fs::read_to_string(dir.join("probe.rs")).expect("вывод читается");
    let _ = std::fs::remove_dir_all(&dir);
    text
}

/// Хвостовой возврат параметра разыменован.
#[test]
fn tail_return_derefs_parameter() {
    let text = emit_rust(TAIL, "tail");
    assert!(
        text.contains(
            "    *a
"
        ),
        "возврат по значению обязан разыменовать ссылку:\n{text}"
    );
}

/// Явный `return` - второй путь печати, и правило действует в нём тоже.
#[test]
fn early_return_derefs_parameter() {
    let text = emit_rust(EARLY, "early");
    assert!(
        text.contains("return *a;"),
        "у явного возврата то же правило:\n{text}"
    );
}

/// **Контроль:** индексация параметра звёздочки не получает.
///
/// Над ссылкой `a[0]` работает само (автоматическое разыменование), а `(*a)[0]`
/// отвергает `clippy` под `-D warnings`.
#[test]
fn indexing_parameter_needs_no_deref() {
    let text = emit_rust(INDEX, "index");
    assert!(
        text.contains("a[0]") && !text.contains("(*a)[0]"),
        "индексация ссылки разыменования не требует:\n{text}"
    );
}

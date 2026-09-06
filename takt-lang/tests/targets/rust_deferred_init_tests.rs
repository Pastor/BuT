//! Отложенная инициализация локальной переменной у цели `rust`.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

const SRC: &str = "var o: u8 := 0;\nout probe: u8 at 0;\n\
     start Run {\n    always {\n        var t: u8;\n        t := 5;\n\
     \x20       o := t;\n        probe := o;\n    }\n    ref Run;\n}\n";

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0410_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

fn generate(tag: &str, src: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_rust(
        tag,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение вывода");
    (dir, text)
}

fn tool(name: &str) -> bool {
    Command::new(name)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Предмет: отложенное объявление сворачивается и `mut` не печатает.
#[test]
fn deferred_declaration_is_folded_without_mut() {
    let (_, text) = generate("rs0410", SRC);
    assert!(
        text.contains("let t: u8 = 5;"),
        "объявление обязано свернуться в инициализацию:\n{text}"
    );
    assert!(
        !text.contains("let mut t"),
        "первое присваивание — инициализация, `mut` ей не нужен:\n{text}"
    );
}

/// **Контроль:** переменная, которой присваивают дважды, `mut` сохраняет.
///
/// Без него правка читалась бы как "`mut` не печатается никогда", и вывод перестал бы
/// компилироваться на первой же изменяемой переменной.
#[test]
fn twice_assigned_variable_keeps_mut() {
    let src = "var o: u8 := 0;\nout probe: u8 at 0;\n\
         start Run {\n    always {\n        var t: u8;\n        t := 5;\n\
         \x20       t := t + 1;\n        o := t;\n        probe := o;\n    }\n    ref Run;\n}\n";
    let (_, text) = generate("rs0410m", src);
    assert!(
        text.contains("let mut t"),
        "второе присваивание — изменение, `mut` обязателен:\n{text}"
    );
}

/// **Контроль:** срез не сворачивается - он печатается поэлементно.
#[test]
fn slice_assignment_is_not_folded() {
    let src = "var src: [u8; 4] := {5, 6, 7, 8};\nvar o: u8 := 0;\nout probe: u8 at 0;\n\
         start Run {\n    always {\n        var part: [u8; 2] := {0, 0};\n\
         \x20       part := src[1:3];\n        o := part[0];\n        probe := o;\n    }\n\
         \x20   ref Run;\n}\n";
    let (_, text) = generate("rs0410s", src);
    assert!(
        text.contains("part[0]") && text.contains("part[1]"),
        "срез обязан печататься поэлементно:\n{text}"
    );
}

/// Порождённый Rust проходит `rustc` и `clippy` под флагами проверки цели.
#[test]
fn generated_rust_passes_the_gate_tools() {
    if !tool("rustc") || !tool("clippy-driver") {
        eprintln!("[ПРОПУСК] `rustc`/`clippy-driver` не найдены; текст вывода уже проверен");
        return;
    }
    let (dir, _) = generate("rs0410t", SRC);
    for exe in ["rustc", "clippy-driver"] {
        let out = Command::new(exe)
            .args(["--edition", "2021", "--crate-type", "lib", "-D", "warnings"])
            .arg(dir.join("rs0410t.rs"))
            .arg("--out-dir")
            .arg(&dir)
            .output()
            .unwrap_or_else(|e| panic!("запуск {exe}: {e}"));
        assert!(
            out.status.success(),
            "{exe} обязан принять вывод:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}

/// Отложенный **агрегат** получает умолчание.
///
/// Rust требует инициализировать массив целиком **до** записи по индексу: `let part:
/// [u8; 2]; part[0] = ...;` даёт `E0381` при нулевом коде возврата `taktc`, тогда как
/// эталон, `c`, `st` и `sv` вход исполняют.
#[test]
fn deferred_aggregate_gets_a_default() {
    let src = "var src: [u8; 4] := {5, 6, 7, 8};\nvar o: u8 := 0;\nout probe: u8 at 0;\n\
         start Run {\n    always {\n        var part: [u8; 2];\n\
         \x20       part := src[1:3];\n        o := part[0];\n        probe := o;\n    }\n\
         \x20   ref Run;\n}\n";
    let (dir, text) = generate("rs0411", src);
    assert!(
        text.contains("let mut part: [u8; 2] = [0; 2];"),
        "отложенный массив обязан получить умолчание:\n{text}"
    );
    if !tool("rustc") {
        eprintln!("[ПРОПУСК] `rustc` не найден; текст вывода уже проверен");
        return;
    }
    let out = std::process::Command::new("rustc")
        .args(["--edition", "2021", "--crate-type", "lib", "-D", "warnings"])
        .arg(dir.join("rs0411.rs"))
        .arg("--out-dir")
        .arg(&dir)
        .output()
        .expect("запуск rustc");
    assert!(
        out.status.success(),
        "rustc обязан принять вывод:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// **Контроль:** скаляру умолчание не печатается.
///
/// Без него правка читалась бы как "умолчание всегда", а лишнее значение - это
/// `unused_assignments`, отказ проверки.
#[test]
fn deferred_scalar_gets_no_default() {
    let (_, text) = generate("rs0411c", SRC);
    assert!(
        !text.contains("let mut t: u8 = 0;"),
        "скаляру умолчание не нужно — там законна отложенная форма:\n{text}"
    );
}

//! Библиотечный файл: законен для `import`, невозможен как вход -.
//!
//! # Что здесь ловится
//!
//! Единица компиляции без единого состояния - набор типов, функций и переменных для
//! подключения. Три утверждения, и каждое проверяется отдельно, потому что нарушаются
//! они независимо:
//!
//! 1. **как вход отвергается** - `SE-102`, с позицией и с названной причиной;
//! 2. **через `import` работает** - иначе исправление запретил бы то, ради чего заведён;
//! 3. **`SE-011` не подменён** - файл, где состояния есть, а стартового нет, -
//!    это забытая пометка, и ответ ему прежний.
//!
//! Третье утверждение - не формальность. Спутав случаи, инструмент ответил бы автору
//! библиотеки "пропущено начальное состояние", которого тот не писал, а автору автомата -
//! "файл библиотечный", хотя он писал автомат.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Библиотека: типы и функции, ни одного состояния.
const LIBRARY: &str = "\
struct Pid {
    kp: float,
    integral: float
}

fn pid_reset(p: Pid) -> Pid {
    var r: Pid := p;
    r.integral := 0.0;
    return r;
}
";

/// Применение: подключает тип библиотеки (импорт структуры работает).
const APPLICATION: &str = "\
import {Pid} from \"library.takt\";

model App {
    var p: Pid := {1.0, 0.0};
    start Run {
        always {
            p.integral := p.integral + 1.0;
        }
    }
}
start Main = App;
";

/// Состояния есть, стартового нет - забытая пометка, а не библиотека.
const NO_START: &str = "\
var v: u8 := 0;
state A {
    always {
        v := 1;
    }
}
state B;
";

fn taktc() -> Command {
    Command::new(env!("CARGO_BIN_EXE_taktc"))
}

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0182_02_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// stderr и код возврата `taktc compile -t c <файл>`.
fn compile(path: &Path, out: &Path) -> (String, i32) {
    let result = taktc()
        .arg("compile")
        .arg("-t")
        .arg("c")
        .arg(path)
        .arg("-o")
        .arg(out)
        .output()
        .expect("запуск taktc compile");
    (
        String::from_utf8_lossy(&result.stderr).into_owned(),
        result.status.code().unwrap_or(-1),
    )
}

#[test]
fn library_as_entry_is_rejected_with_reason() {
    let dir = work_dir("entry");
    let path = dir.join("library.takt");
    std::fs::write(&path, LIBRARY).expect("запись библиотеки");

    let (stderr, code) = compile(&path, &dir.join("out"));

    assert_ne!(code, 0, "библиотека входом быть не может: {stderr}");
    assert!(stderr.contains("[SE-102]"), "ожидался SE-102: {stderr}");
    // Причина названа, а не только факт: текст обязан сказать, что делать.
    assert!(
        stderr.contains("import"),
        "текст обязан назвать способ применить библиотеку: {stderr}"
    );
    // Позиция есть (прежний отказ приходил из генератора и шёл без неё).
    assert!(
        stderr.contains("library.takt:"),
        "диагностика обязана нести путь и позицию: {stderr}"
    );
}

#[test]
fn library_is_usable_through_import() {
    let dir = work_dir("import");
    std::fs::write(dir.join("library.takt"), LIBRARY).expect("запись библиотеки");
    let app = dir.join("app.takt");
    std::fs::write(&app, APPLICATION).expect("запись применения");

    let (stderr, code) = compile(&app, &dir.join("out"));

    assert_eq!(
        code, 0,
        "импорт библиотеки — то, ради чего фикс заведён: {stderr}"
    );
}

#[test]
fn missing_start_among_states_keeps_se011() {
    let dir = work_dir("no_start");
    let path = dir.join("no_start.takt");
    std::fs::write(&path, NO_START).expect("запись фикстуры");

    let (stderr, code) = compile(&path, &dir.join("out"));

    assert_ne!(code, 0, "{stderr}");
    assert!(
        stderr.contains("[SE-011]"),
        "состояния есть, стартового нет — это SE-011, а не SE-102: {stderr}"
    );
    assert!(
        !stderr.contains("[SE-102]"),
        "SE-102 не должен подменять SE-011: {stderr}"
    );
}

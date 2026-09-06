//! Инициализатор массива структур у целей `c` и `rust`.

use std::path::PathBuf;
use std::process::Command;

const FIXTURE: &str = "../takt-sim/tests/data/eval/conformance_array_init.takt";

fn source() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0343_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn available(tool: &str) -> bool {
    Command::new(tool)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Цель `c`: элементы кладутся по полям, `cc -Werror` принимает.
#[test]
fn c_array_of_structs_compiles() {
    let dir = build_dir("c");
    takt_lang::compile_to_c(
        "ai",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let text = std::fs::read_to_string(dir.join("ai.c")).expect("чтение");
    assert!(
        text.contains("model->cells[0].v = 3;") && text.contains("model->cells[1].w = 6;"),
        "элементы обязаны класться по полям:\n{text}"
    );
    assert!(
        text.contains("model->flat[0] = 1;"),
        "контроль: массив скаляров по-прежнему по индексу:\n{text}"
    );

    if !available("cc") {
        eprintln!("[ПРОПУСК] c_array_of_structs_compiles: cc не найден");
        return;
    }
    let build = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c", "-I"])
        .arg(&dir)
        .arg(dir.join("ai.c"))
        .arg("-o")
        .arg(dir.join("ai.o"))
        .output()
        .expect("запуск cc");
    assert!(
        build.status.success(),
        "порождённый C не собирается:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

/// Цель `rust`: элементы печатаются литералом структуры.
#[test]
fn rust_array_of_structs_compiles() {
    let dir = build_dir("rust");
    takt_lang::compile_to_rust(
        "ai",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join("ai.rs")).expect("чтение");
    assert!(
        text.contains("[Cell { v: 3, w: 4 }, Cell { v: 5, w: 6 }]"),
        "элементы обязаны печататься литералом структуры:\n{text}"
    );
    assert!(
        text.contains("[1, 2]"),
        "контроль: массив скаляров печатается как прежде:\n{text}"
    );

    if !available("rustc") {
        eprintln!("[ПРОПУСК] rust_array_of_structs_compiles: rustc не найден");
        return;
    }
    let out = dir.join("check");
    std::fs::create_dir_all(&out).expect("каталог");
    let build = Command::new("rustc")
        .args(["--edition", "2021", "--crate-type", "lib", "-D", "warnings"])
        .arg(dir.join("ai.rs"))
        .arg("--out-dir")
        .arg(&out)
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "порождённый Rust не собирается:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

/// Агрегат массива в аргументе параметра модели цель `st` по-прежнему отвергает.
///
/// Контроль границы: в объявлении переменной форма `:= [9, 8, 7, 6]` законна и `iec2c`
/// её принимает, а в инициализаторе экземпляра `FUNCTION_BLOCK` тот же массив даёт
/// "Initialization element identifier ... incompatible datatype". Первая редакция
/// правки сняла отказ вместе с потерей - и породила невалидный файл; поймал это тест.
#[test]
fn st_still_refuses_aggregate_parameter_argument() {
    let dir = build_dir("st_param");
    let err = takt_lang::compile_to_st(
        "agg",
        "model Memory {\n    parameter prog: [u8; 4] := {0, 0, 0, 0};\n\
         var out_v: u8 := 0;\n    start Run { always { out_v := prog[0]; } ref Run; }\n}\n\
         start Main = Memory(prog := {9, 8, 7, 6});\n",
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect_err("агрегат в аргументе параметра цель `st` не печатает");
    assert_eq!(err.code.as_deref(), Some("ST-017"));
}

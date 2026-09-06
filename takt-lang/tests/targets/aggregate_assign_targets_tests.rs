//! Присваивание агрегата в теле переводят все цели.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "../takt-sim/tests/data/eval/conformance_struct_assign.takt";

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
        .join(format!("takt_0340_{tag}_{thread}"));
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

/// Цель `c`: поэлементно, `cc -Werror` принимает.
#[test]
fn c_aggregate_assignment_compiles() {
    let dir = build_dir("c");
    takt_lang::compile_to_c(
        "agg",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let text = std::fs::read_to_string(dir.join("agg.c")).expect("чтение");
    assert!(
        text.contains("model->body.x = 3;") && text.contains("model->body.y = 4;"),
        "структура обязана адресоваться по имени поля:\n{text}"
    );
    assert!(
        !text.contains("= {3, 4}"),
        "агрегатной формы присваивания в C нет вовсе:\n{text}"
    );

    if !available("cc") {
        eprintln!("[ПРОПУСК] c_aggregate_assignment_compiles: cc не найден");
        return;
    }
    let build = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c", "-I"])
        .arg(&dir)
        .arg(dir.join("agg.c"))
        .arg("-o")
        .arg(dir.join("agg.o"))
        .output()
        .expect("запуск cc");
    assert!(
        build.status.success(),
        "порождённый C не собирается:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

/// Цель `st`: структура по имени поля, массив по индексу (контроль).
#[test]
fn st_aggregate_assignment_is_accepted() {
    let dir = build_dir("st");
    takt_lang::compile_to_st(
        "agg",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join("agg.st")).expect("чтение");
    assert!(
        text.contains("body.x := 3;"),
        "структура обязана адресоваться по имени поля:\n{text}"
    );
    assert!(
        text.contains("arr[0] := 5;"),
        "контроль: массив по-прежнему по индексу:\n{text}"
    );

    let Some(iec2c) = iec2c_path() else {
        eprintln!("[ПРОПУСК] st_aggregate_assignment_is_accepted: iec2c не найден");
        return;
    };
    let out = dir.join("st_out");
    std::fs::create_dir_all(&out).expect("каталог");
    let lib = iec2c
        .parent()
        .and_then(Path::parent)
        .map_or_else(|| PathBuf::from("/usr/local"), Path::to_path_buf)
        .join("share/matiec/lib");
    let run = Command::new(&iec2c)
        .args(["-I".as_ref(), lib.as_os_str()])
        .arg("-T")
        .arg(&out)
        .arg(dir.join("agg.st"))
        .output()
        .expect("запуск iec2c");
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        !stderr.contains("error"),
        "iec2c отверг порождённый ST:\n{stderr}"
    );
}

fn iec2c_path() -> Option<PathBuf> {
    let home = std::env::var("HOME").ok()?;
    let path = PathBuf::from(home).join(".local/bin/iec2c");
    path.is_file().then_some(path)
}

/// Цель `sv`: то же, линт чист.
#[test]
fn sv_aggregate_assignment_is_accepted() {
    let dir = build_dir("sv");
    takt_lang::compile_to_sv(
        "agg",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join("agg.sv")).expect("чтение");
    assert!(
        text.contains("agg_body_next.x = 3;"),
        "структура обязана адресоваться по имени поля:\n{text}"
    );
    assert!(
        text.contains("agg_arr_next[0] = 5;"),
        "контроль: массив по-прежнему по индексу:\n{text}"
    );

    if !available("verilator") {
        eprintln!("[ПРОПУСК] sv_aggregate_assignment_is_accepted: verilator не найден");
        return;
    }
    let lint = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("agg.sv"))
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "verilator не принял модуль:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
}

//! Константа структурного и массивного типа.

use std::path::PathBuf;
use std::process::Command;

const FIXTURE: &str = "../takt-sim/tests/data/eval/conformance_const_struct.takt";

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
        .join(format!("takt_0347_{tag}_{thread}"));
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

/// Цель `rust`: константа-структура печатается литералом структуры.
#[test]
fn rust_const_struct_compiles() {
    let dir = build_dir("rust");
    takt_lang::compile_to_rust(
        "ca",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join("ca.rs")).expect("чтение");
    assert!(
        text.contains("Cell { v: 3, w: 4 }"),
        "константа-структура обязана печататься литералом структуры:\n{text}"
    );
    assert!(
        text.contains("= 2;"),
        "контроль: константа-скаляр печатается как прежде:\n{text}"
    );

    if !available("rustc") {
        eprintln!("[ПРОПУСК] rust_const_struct_compiles: rustc не найден");
        return;
    }
    let out = dir.join("check");
    std::fs::create_dir_all(&out).expect("каталог");
    let build = Command::new("rustc")
        .args(["--edition", "2021", "--crate-type", "lib", "-D", "warnings"])
        .arg(dir.join("ca.rs"))
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

/// Цель `sv`: `localparam` печатается агрегатом, а типы объявлены раньше.
#[test]
fn sv_const_aggregate_is_accepted() {
    let dir = build_dir("sv");
    takt_lang::compile_to_sv(
        "ca",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join("ca.sv")).expect("чтение");
    let typedef = text.find("} cell_t;").expect("тип объявлен");
    let param = text
        .find("localparam cell_t")
        .expect("localparam напечатан");
    assert!(
        typedef < param,
        "тип обязан объявляться раньше константы, которая на него ссылается:\n{text}"
    );

    if !available("verilator") {
        eprintln!("[ПРОПУСК] sv_const_aggregate_is_accepted: verilator не найден");
        return;
    }
    let lint = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("ca.sv"))
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "verilator не принял модуль:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
}

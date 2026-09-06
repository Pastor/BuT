//! Вложенная структура переводится всеми целями.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "../takt-sim/tests/data/eval/conformance_nested_struct.takt";

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
        .join(format!("takt_0341_{tag}_{thread}"));
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

/// Цель `c`: зависимость объявлена раньше, `cc -Werror` принимает.
#[test]
fn c_nested_struct_compiles() {
    let dir = build_dir("c");
    takt_lang::compile_to_c(
        "nst",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let header = std::fs::read_to_string(dir.join("nst.h")).expect("чтение заголовка");
    let point = header.find("typedef struct Point").expect("Point объявлен");
    let line = header.find("typedef struct Line").expect("Line объявлен");
    assert!(
        point < line,
        "вложенная структура обязана быть объявлена раньше вмещающей:\n{header}"
    );

    if !available("cc") {
        eprintln!("[ПРОПУСК] c_nested_struct_compiles: cc не найден");
        return;
    }
    let build = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c", "-I"])
        .arg(&dir)
        .arg(dir.join("nst.c"))
        .arg("-o")
        .arg(dir.join("nst.o"))
        .output()
        .expect("запуск cc");
    assert!(
        build.status.success(),
        "порождённый C не собирается:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

/// Цель `st`: тот же порядок, `iec2c` принимает.
///
/// Алфавитной сортировки на выходе быть не должно: она разрушила бы порядок, собранный
/// по зависимостям. Детерминированность держит сам обход (`BTreeMap`).
#[test]
fn st_nested_struct_is_accepted() {
    let dir = build_dir("st");
    takt_lang::compile_to_st(
        "nst",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join("nst.st")).expect("чтение");
    let point = text.find("Point :").expect("Point объявлен");
    let line = text.find("Line :").expect("Line объявлен");
    assert!(point < line, "порядок обязан идти по зависимостям:\n{text}");

    let Some(iec2c) = iec2c_path() else {
        eprintln!("[ПРОПУСК] st_nested_struct_is_accepted: iec2c не найден");
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
        .arg(dir.join("nst.st"))
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

/// Цель `sv`: тот же порядок, линт чист.
#[test]
fn sv_nested_struct_is_accepted() {
    let dir = build_dir("sv");
    takt_lang::compile_to_sv(
        "nst",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join("nst.sv")).expect("чтение");
    let point = text.find("} point_t;").expect("point_t объявлен");
    let line = text.find("} line_t;").expect("line_t объявлен");
    assert!(point < line, "порядок обязан идти по зависимостям:\n{text}");

    if !available("verilator") {
        eprintln!("[ПРОПУСК] sv_nested_struct_is_accepted: verilator не найден");
        return;
    }
    let lint = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("nst.sv"))
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "verilator не принял модуль:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
}

/// Цель `rust`: доступ к полю не считается логическим.
///
/// Контроль на разряд обязателен: `x.7` **обязан** остаться логическим, иначе правка
/// читалась бы как "точка больше не даёт bool никогда".
#[test]
fn rust_nested_struct_compiles_and_bit_stays_boolean() {
    let dir = build_dir("rust");
    takt_lang::compile_to_rust(
        "nst",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join("nst.rs")).expect("чтение");
    assert!(
        text.contains("seg.a.x = 7;"),
        "поле структуры обязано печататься доступом через точку:\n{text}"
    );

    // Контроль: разряд по-прежнему логичен.
    let bit_dir = build_dir("rust_bit");
    takt_lang::compile_to_rust(
        "bitp",
        "var v: u8 := 200;\nvar f: bit := 0;\nout o: bit at 0;\n\
         start Run { always { f := v.7; o := f; } ref Run: f = 1; }\n",
        bit_dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let bit_text = std::fs::read_to_string(bit_dir.join("bitp.rs")).expect("чтение");
    assert!(
        bit_text.contains("self.f = ((self.v >> 7) & 1) != 0;"),
        "разряд обязан оставаться логическим:\n{bit_text}"
    );

    if !available("rustc") {
        eprintln!("[ПРОПУСК] rust_nested_struct_compiles_and_bit_stays_boolean: rustc не найден");
        return;
    }
    let out = dir.join("check");
    std::fs::create_dir_all(&out).expect("каталог");
    let build = Command::new("rustc")
        .args(["--edition", "2021", "--crate-type", "lib", "-D", "warnings"])
        .arg(dir.join("nst.rs"))
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

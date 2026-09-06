//! Длительность в поле структуры и в массиве.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "../takt-sim/tests/data/eval/conformance_duration_field.takt";

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
        .join(format!("takt_0349_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Цель `st`: приведение поля структуры печатается, `iec2c` принимает.
#[test]
fn st_duration_field_cast_is_translated() {
    let dir = build_dir("st");
    takt_lang::compile_to_st(
        "df",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join("df.st")).expect("чтение");
    assert!(
        text.contains("t : Timing := (hold := 5, gap := 10);"),
        "поля длительности объявляются миллисекундами:\n{text}"
    );

    let Some(iec2c) = iec2c_path() else {
        eprintln!("[ПРОПУСК] st_duration_field_cast_is_translated: iec2c не найден");
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
        .arg(dir.join("df.st"))
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

/// Цель `sv`: литерал длительности внутри агрегата печатается размерным.
///
/// Проверка цели считает предупреждение verilator **ошибкой**, поэтому `WIDTHCONCAT` здесь -
/// отказ сборки, а не косметика.
#[test]
fn sv_duration_field_literal_is_sized() {
    let dir = build_dir("sv");
    takt_lang::compile_to_sv(
        "df",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join("df.sv")).expect("чтение");
    assert!(
        text.contains("{32'd5, 32'd10}"),
        "литералы длительности внутри агрегата обязаны быть размерными:\n{text}"
    );

    let available = Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !available {
        eprintln!("[ПРОПУСК] sv_duration_field_literal_is_sized: verilator не найден");
        return;
    }
    let lint = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("df.sv"))
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "verilator не принял модуль:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
}

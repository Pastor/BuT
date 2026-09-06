//! Перечисление внутри функции: две цели порождали невалидный вывод.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "../takt-sim/tests/data/eval/conformance_enum_in_function.takt";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0338_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn source() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

/// Цель `sv`: инициализатор приводится к варианту, verilator принимает.
#[test]
fn sv_enum_local_is_variant_not_number() {
    let dir = build_dir("sv");
    takt_lang::compile_to_sv(
        "enumfn",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join("enumfn.sv")).expect("чтение");
    assert!(
        text.contains("res = MODE_IDLE;"),
        "инициализатор обязан печататься вариантом:\n{text}"
    );
    assert!(
        text.contains("acc = 1;"),
        "контроль: обычный инициализатор не изменился:\n{text}"
    );

    let available = Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !available {
        eprintln!("[ПРОПУСК] sv_enum_local_is_variant_not_number: verilator не найден");
        return;
    }
    let lint = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("enumfn.sv"))
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "verilator не принял модуль (прежде здесь был ENUMVALUE):\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
}

/// Цель `st`: константы перечисления дублируются внутрь функции.
///
/// Дублируются **упомянутые телом**, а не все: лишние объявления - шум в выводе,
/// который читает наладчик ПЛК.
#[test]
fn st_enum_constants_are_declared_inside_function() {
    let dir = build_dir("st");
    takt_lang::compile_to_st(
        "enumfn",
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join("enumfn.st")).expect("чтение");
    let classify = text
        .split("FUNCTION ")
        .find(|part| part.starts_with("Enumfn_classify"))
        .and_then(|part| part.split("END_FUNCTION").next())
        .expect("функция classify в выводе");
    assert!(
        classify.contains("Mode_Idle : USINT := 0;"),
        "константа перечисления обязана быть объявлена внутри функции:\n{classify}"
    );
    // Границу класть по `END_FUNCTION`: без неё срез дотягивается до `FUNCTION_BLOCK`,
    // где константы перечисления объявлены законно, и контрольная проверка "лишних
    // объявлений нет" падала бы всегда.
    let plain = text
        .split("FUNCTION ")
        .find(|part| part.starts_with("Enumfn_plain"))
        .and_then(|part| part.split("END_FUNCTION").next())
        .expect("функция plain в выводе");
    assert!(
        !plain.contains("Mode_"),
        "контроль: функция без перечисления лишних объявлений не получает:\n{plain}"
    );

    let Some(iec2c) = iec2c_path() else {
        eprintln!("[ПРОПУСК] st_enum_constants_are_declared_inside_function: iec2c не найден");
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
        .arg(dir.join("enumfn.st"))
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

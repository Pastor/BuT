//! Неиспользуемый параметр функции гасится заглушкой у трёх целей.
//!
//! # Формы
//!
//! | Цель | Заглушка |
//! |---|---|
//! | `c` | `(void)v;` |
//! | `rust` | `let _ = v;` |
//! | `sv` | `logic _unused_v; _unused_v = &{1'b0, v};` |
//!
//! Имя параметра в сигнатуре **не меняется**: форма `_v` тоже гасит предупреждение, но
//! сигнатура порождённого модуля читается человеком.

use std::path::PathBuf;
use std::process::Command;

/// Вход: `constant` параметром не пользуется, `echo` - пользуется (контроль).
const SRC: &str = "fn constant(v: u8) -> u8 { return 7; }\n\
     fn echo(w: u8) -> u8 { return w; }\n\
     var r: u8 := 0;\nvar e: u8 := 0;\n\
     out o: u8 at 0;\n\
     start Run {\n  always { r := constant(3); e := echo(5); o := r + e; }\n\
     ref Run: r = 7;\n}\n";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0337_{tag}_{thread}"));
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

/// Цель `c`: заглушка печатается, `cc -Werror` молчит.
#[test]
fn c_unused_parameter_is_guarded() {
    let dir = build_dir("c");
    takt_lang::compile_to_c(
        "unusedp",
        SRC,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let text = std::fs::read_to_string(dir.join("unusedp.c")).expect("чтение");
    assert!(text.contains("(void)v;"), "заглушка не напечатана:\n{text}");
    assert!(
        !text.contains("(void)w;"),
        "используемый параметр заглушки не требует:\n{text}"
    );

    if !available("cc") {
        eprintln!("[ПРОПУСК] c_unused_parameter_is_guarded: cc не найден");
        return;
    }
    let build = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c", "-I"])
        .arg(&dir)
        .arg(dir.join("unusedp.c"))
        .arg("-o")
        .arg(dir.join("unusedp.o"))
        .output()
        .expect("запуск cc");
    assert!(
        build.status.success(),
        "порождённый C не проходит -Werror:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

/// Цель `rust`: `let _ = v;`, вывод собирается под `-D warnings`.
#[test]
fn rust_unused_parameter_is_guarded() {
    let dir = build_dir("rust");
    takt_lang::compile_to_rust(
        "unusedp",
        SRC,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join("unusedp.rs")).expect("чтение");
    assert!(
        text.contains("let _ = v;"),
        "заглушка не напечатана:\n{text}"
    );
    assert!(
        !text.contains("let _ = w;"),
        "используемый параметр заглушки не требует:\n{text}"
    );
    assert!(
        text.contains("fn constant(v: u8)"),
        "имя параметра в сигнатуре обязано остаться прежним:\n{text}"
    );

    if !available("rustc") {
        eprintln!("[ПРОПУСК] rust_unused_parameter_is_guarded: rustc не найден");
        return;
    }
    let out = dir.join("check");
    std::fs::create_dir_all(&out).expect("каталог");
    let build = Command::new("rustc")
        .args(["--edition", "2021", "--crate-type", "lib", "-D", "warnings"])
        .arg(dir.join("unusedp.rs"))
        .arg("--out-dir")
        .arg(&out)
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "порождённый Rust не собирается под -D warnings:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

/// Цель `sv`: параметр поглощается редукцией, линт молчит.
///
/// `lint_off` запрещён правилом проекта - параметр здесь честно используется, а
/// константа `1'b0` в редукции даёт ноль, и синтезатор выбрасывает эту логику сам.
#[test]
fn sv_unused_parameter_is_absorbed() {
    let dir = build_dir("sv");
    takt_lang::compile_to_sv(
        "unusedp",
        SRC,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join("unusedp.sv")).expect("чтение");
    assert!(
        text.contains("_unused_v = &{1'b0, v};"),
        "заглушка не напечатана:\n{text}"
    );
    assert!(
        !text.contains("_unused_w"),
        "используемый параметр заглушки не требует:\n{text}"
    );
    assert!(
        !text.contains("lint_off"),
        "прагма линта запрещена правилом проекта:\n{text}"
    );

    if !available("verilator") {
        eprintln!("[ПРОПУСК] sv_unused_parameter_is_absorbed: verilator не найден");
        return;
    }
    let lint = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("unusedp.sv"))
        .output()
        .expect("запуск verilator");
    assert!(
        lint.status.success(),
        "verilator не принял модуль:\n{}",
        String::from_utf8_lossy(&lint.stderr)
    );
}

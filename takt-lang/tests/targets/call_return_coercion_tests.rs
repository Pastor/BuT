//! Приведение к типу приёмника в аргументе и возврате функции.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Вход: разряд в аргументе и в возврате, литерал `bit`, вариант перечисления.
const SRC: &str = "enum Mode { Idle, Busy }\n\
     fn take(v: u8) -> u8 { return v; }\n\
     fn give(x: u8) -> u8 { return x.7; }\n\
     fn flag(b: bit) -> u8 { if b { return 1; } return 0; }\n\
     fn ret_bit() -> bit { return 1; }\n\
     fn pick(m: Mode) -> u8 { if m = Busy { return 9; } return 8; }\n\
     var src: u8 := 200;\nvar arg: u8 := 0;\nvar ret: u8 := 0;\n\
     var lit: u8 := 0;\nvar rb: bit := 0;\nvar pk: u8 := 0;\nvar sum: u8 := 0;\n\
     out o: u8 at 0;\n\
     start Run {\n  always {\n    arg := take(src.7);\n    ret := give(src);\n\
     lit := flag(1);\n    rb := ret_bit();\n    pk := pick(Busy);\n    sum := arg + ret + lit + pk;\n    o := sum;\n  }\n\
     ref Run: sum > 0;\n}\n";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0336_{tag}_{thread}"));
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

/// Цель `rust`: обе позиции приведены, вывод собирается.
///
/// Проверка `unused_variables` **отключена флагом**: параметр `m` тела `pick`
/// используется в условии, но у входа побогаче нашлась бы функция, чей параметр не
/// нужен, - это соседний класс (кандидат), а не предмет фичи.
#[test]
fn rust_call_and_return_compile() {
    let dir = build_dir("rust");
    takt_lang::compile_to_rust(
        "callret",
        SRC,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join("callret.rs")).expect("чтение");
    assert!(
        text.contains("take(u8::from(((self.src >> 7) & 1) != 0))"),
        "аргумент обязан приводиться к типу параметра:\n{text}"
    );
    assert!(
        text.contains("flag(true)"),
        "литерал `bit` в аргументе обязан печататься `true`:\n{text}"
    );
    assert!(
        text.contains("pick(Mode::Busy)"),
        "вариант перечисления в аргументе обязан печататься именем:\n{text}"
    );

    if !available("rustc") {
        eprintln!("[ПРОПУСК] rust_call_and_return_compile: rustc не найден");
        return;
    }
    let out = dir.join("check");
    std::fs::create_dir_all(&out).expect("каталог сборки");
    let build = Command::new("rustc")
        .args(["--edition", "2021", "--crate-type", "lib"])
        .arg(dir.join("callret.rs"))
        .arg("--out-dir")
        .arg(&out)
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "порождённый Rust не собирается (прежде здесь были E0308):\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

/// Цель `st`: разряд в аргументе и возврате оборачивается преобразованием.
#[test]
fn st_call_and_return_accepted_by_iec2c() {
    let dir = build_dir("st");
    takt_lang::compile_to_st(
        "callret",
        SRC,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join("callret.st")).expect("чтение");
    assert!(
        text.contains("Callret_take(BOOL_TO_USINT("),
        "аргумент обязан приводиться:\n{text}"
    );
    assert!(
        text.contains("Callret_give := BOOL_TO_USINT("),
        "возврат обязан приводиться:\n{text}"
    );

    let Some(iec2c) = iec2c_path() else {
        eprintln!("[ПРОПУСК] st_call_and_return_accepted_by_iec2c: iec2c не найден");
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
        .arg(dir.join("callret.st"))
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

/// Цель `sv`: разряд в аргументе и возврате печатается размерной формой.
///
/// Вход здесь **уже**, чем у соседей: перечисление в параметре функции цель не
/// переводит вовсе (`SV-002`) - это соседний класс, вынесенный кандидатом.
#[test]
fn sv_call_and_return_have_no_width_warning() {
    let dir = build_dir("sv");
    let src = "fn take(v: u8) -> u8 { return v; }\n\
         fn give(x: u8) -> u8 { return x.7; }\n\
         var src: u8 := 200;\nvar arg: u8 := 0;\nvar ret: u8 := 0;\n\
         var sum: u8 := 0;\nout o: u8 at 0;\n\
         start Run { always { arg := take(src.7); ret := give(src); sum := arg + ret; o := sum; } \
         ref Run: sum > 0; }\n";
    takt_lang::compile_to_sv(
        "callret",
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join("callret.sv")).expect("чтение");
    assert!(
        text.contains("give = 8'(x[7]);"),
        "возврат обязан приводиться:\n{text}"
    );
    assert!(
        text.contains("take(8'(callret_src_next[7]))"),
        "аргумент обязан приводиться:\n{text}"
    );

    if !available("verilator") {
        eprintln!("[ПРОПУСК] sv_call_and_return_have_no_width_warning: verilator не найден");
        return;
    }
    let lint = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("callret.sv"))
        .output()
        .expect("запуск verilator");
    let stderr = String::from_utf8_lossy(&lint.stderr);
    assert!(
        !stderr.contains("WIDTHEXPAND"),
        "verilator предупреждает о ширине:\n{stderr}"
    );
}

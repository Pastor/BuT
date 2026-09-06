//! Разряд `x.N` в позиции числового значения переводят все цели.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Вход: разряд в числовую переменную (`high`) и в битовую (`flag` - контроль).
const SRC: &str = "var src: u8 := 200;\nvar high: u8 := 0;\nvar flag: bit := 0;\n\
     out o_high: u8 at 0;\nout o_flag: bit at 2;\n\
     start Run {\n  always { high := src.7; flag := src.3; o_high := high; o_flag := flag; }\n\
     ref Run: high = 1;\n}\n";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0335_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn available(tool: &str, arg: &str) -> bool {
    Command::new(tool)
        .arg(arg)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn generate(target: &str, dir: &Path) -> String {
    let options = takt_lang::generator::GenerateOptions::default();
    let path = dir.to_str().expect("путь в UTF-8");
    match target {
        "rust" => takt_lang::compile_to_rust("bitv", SRC, path, &[], &options),
        "st" => takt_lang::compile_to_st("bitv", SRC, path, &[], &options),
        "sv" => takt_lang::compile_to_sv("bitv", SRC, path, &[], &options),
        other => panic!("цель '{other}' не участвует"),
    }
    .unwrap_or_else(|d| panic!("цель '{target}' отказала: {d:?}"));
    let ext = if target == "rust" { "rs" } else { target };
    std::fs::read_to_string(dir.join(format!("bitv.{ext}"))).expect("чтение вывода")
}

/// Цель `rust`: разряд приводится к типу приёмника и **собирается**.
///
/// Контрольная проверка обязательна: битовый приёмник приведения **не** получает -
/// иначе правило читалось бы как "разряд всегда число", и `flag: bit` перестал бы
/// собираться в другую сторону.
#[test]
fn rust_bit_value_compiles() {
    let dir = build_dir("rust");
    let text = generate("rust", &dir);
    assert!(
        text.contains("self.high = u8::from(((self.src >> 7) & 1) != 0);"),
        "числовой приёмник обязан получить приведение:\n{text}"
    );
    assert!(
        text.contains("self.flag = ((self.src >> 3) & 1) != 0;"),
        "битовый приёмник приведения не требует:\n{text}"
    );

    if !available("rustc", "--version") {
        eprintln!("[ПРОПУСК] rust_bit_value_compiles: rustc не найден");
        return;
    }
    let out = dir.join("check");
    std::fs::create_dir_all(&out).expect("каталог сборки");
    let build = Command::new("rustc")
        .args(["--edition", "2021", "--crate-type", "lib", "-D", "warnings"])
        .arg(dir.join("bitv.rs"))
        .arg("--out-dir")
        .arg(&out)
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "порождённый Rust не собирается (прежде здесь был E0308):\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

/// Цель `st`: разряд оборачивается стандартным преобразованием IEC.
#[test]
fn st_bit_value_is_accepted_by_iec2c() {
    let dir = build_dir("st");
    let text = generate("st", &dir);
    assert!(
        text.contains("high := BOOL_TO_USINT("),
        "числовой приёмник обязан получить BOOL_TO_<тип>:\n{text}"
    );
    assert!(
        !text.contains("flag := BOOL_TO_"),
        "битовому приёмнику преобразование не нужно:\n{text}"
    );

    let iec2c = dirs_iec2c();
    let Some(iec2c) = iec2c else {
        eprintln!("[ПРОПУСК] st_bit_value_is_accepted_by_iec2c: iec2c не найден");
        return;
    };
    let out = dir.join("st_out");
    std::fs::create_dir_all(&out).expect("каталог");
    let run = Command::new(&iec2c)
        .arg("-I")
        .arg(iec2c.parent().and_then(Path::parent).map_or_else(
            || PathBuf::from("/usr/local/share/matiec/lib"),
            |base| base.join("share/matiec/lib"),
        ))
        .arg("-T")
        .arg(&out)
        .arg(dir.join("bitv.st"))
        .output()
        .expect("запуск iec2c");
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        !stderr.contains("error"),
        "iec2c отверг порождённый ST:\n{stderr}"
    );
}

/// Путь к `iec2c`, если он установлен (проверка ставит его в `~/.local/bin`).
fn dirs_iec2c() -> Option<PathBuf> {
    let home = std::env::var("HOME").ok()?;
    let path = PathBuf::from(home).join(".local/bin/iec2c");
    path.is_file().then_some(path)
}

/// Цель `sv`: разряд печатается размерной формой и линт молчит.
///
/// Проверка цели считает предупреждение verilator **ошибкой**, поэтому `WIDTHEXPAND` здесь -
/// не косметика, а отказ сборки.
#[test]
fn sv_bit_value_has_no_width_warning() {
    let dir = build_dir("sv");
    let text = generate("sv", &dir);
    assert!(
        text.contains("bitv_high_next = 8'(bitv_src_next[7]);"),
        "числовой приёмник обязан получить размерную форму:\n{text}"
    );
    assert!(
        text.contains("bitv_flag_next = bitv_src_next[3];"),
        "однобитный приёмник размерной формы не требует:\n{text}"
    );

    if !available("verilator", "--version") {
        eprintln!("[ПРОПУСК] sv_bit_value_has_no_width_warning: verilator не найден");
        return;
    }
    let lint = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("bitv.sv"))
        .output()
        .expect("запуск verilator");
    let stderr = String::from_utf8_lossy(&lint.stderr);
    assert!(
        lint.status.success() && !stderr.contains("WIDTHEXPAND"),
        "verilator предупреждает о ширине:\n{stderr}"
    );
}

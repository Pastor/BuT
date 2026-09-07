//! Биты `reg_wdata`, не занятые входными портами.
//!
//! # Что проверяется
//!
//! Поглотитель печатается **только на непокрытые** диапазоны и **только** при наличии
//! входного порта; когда вход занимает всё слово, его нет вовсе. Проверка текстовая
//! плюс прогон `verilator` - линт и есть предмет фичи.

use std::process::Command;
use takt_lang::GenerateOptions;

fn out_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt0486_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

/// Входной порт **уже** слова: занят один бит из восьми.
const NARROW_IN: &str = "in cmd: bit at 0x300:0;\n\
                         out level: u8 at 0x304;\n\
                         var seen: bit := 0;\n\
                         start Run { always { seen := cmd; level := 1; } ref Run: seen = 0; }\n";

/// Входной порт во всю ширину слова - гасить нечего.
const FULL_IN: &str = "in cmd: u8 at 0x300;\n\
                       out level: u8 at 0x304;\n\
                       var seen: u8 := 0;\n\
                       start Run { always { seen := cmd; level := seen; } ref Run: seen < 200; }\n";

/// Два входных порта в разных словах: занятое объединяется, дыра остаётся одна.
const TWO_IN: &str = "in low: bit at 0x300:0;\n\
                      in high: bit at 0x308:7;\n\
                      out level: u8 at 0x304;\n\
                      var seen: bit := 0;\n\
                      start Run { always { seen := low; level := 1; } ref Run: high = 0; }\n";

fn generate(unit: &str, source: &str, tag: &str) -> (std::path::PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_sv_mmio(
        unit,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[] as &[String],
        &[],
        &takt_lang::address_map::AddressEnv::default(),
        &GenerateOptions::default(),
    )
    .unwrap_or_else(|d| panic!("{unit}: цель обязана скомпилировать вход: {d:?}"));
    let text = std::fs::read_to_string(dir.join(format!("{unit}.sv"))).expect("вывод читается");
    (dir, text)
}

/// Непокрытые биты гасятся, и ровно тем срезом, который не занят.
#[test]
fn unused_wdata_bits_are_absorbed() {
    let (dir, text) = generate("narrow", NARROW_IN, "narrow");
    assert!(
        text.contains("wire _unused_wdata = &{1'b0, reg_wdata[7:1]};"),
        "непокрытые биты обязаны поглощаться редукцией:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Когда вход занимает всё слово, поглотителя нет.
///
/// Контроль обязателен: поглотитель "на всякий случай" гасил бы и настоящую ошибку -
/// неподключённый бит порта, ради которой `UNUSEDSIGNAL` и заведён.
#[test]
fn full_width_input_needs_no_guard() {
    let (dir, text) = generate("full", FULL_IN, "full");
    assert!(
        !text.contains("_unused_wdata"),
        "гасить нечего — все биты слова заняты входом:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Занятое объединяется по всем входам, даже из разных слов.
#[test]
fn coverage_unites_inputs_of_every_word() {
    let (dir, text) = generate("two", TWO_IN, "two");
    assert!(
        text.contains("wire _unused_wdata = &{1'b0, reg_wdata[6:1]};"),
        "биты 0 и 7 заняты входами разных слов — гасится середина:\n{text}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Инструмент цели принимает вывод - предмет фичи именно в этом.
#[test]
fn verilator_accepts_narrow_input() {
    if !Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        eprintln!("[ПРОПУСК] verilator_accepts_narrow_input: нет verilator");
        return;
    }
    for (unit, source, tag) in [
        ("narrow_lint", NARROW_IN, "narrow_lint"),
        ("two_lint", TWO_IN, "two_lint"),
    ] {
        let (dir, _) = generate(unit, source, tag);
        // Флаги - те же, что у проверки цели: без `-Wall` предупреждение не поднимается до
        // отказа, и дефект остаётся невидимым.
        let out = Command::new("verilator")
            .args(["--lint-only", "-Wall"])
            .arg(dir.join(format!("{unit}.sv")))
            .output()
            .expect("запуск verilator");
        assert!(
            out.status.success(),
            "{unit}: verilator отверг вывод:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
        let _ = std::fs::remove_dir_all(&dir);
    }
}

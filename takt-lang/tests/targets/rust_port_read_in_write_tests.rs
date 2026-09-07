//! Чтение порта в аргументе записи порта у цели `rust`.
//!
//! # Что проверяется
//!
//! - значение, читающее порт, поднимается во временную, и `rustc`/`clippy`
//!   принимают вывод под флагами проверки (`-D warnings`);
//! - подъём печатается **по нужде**: значение без чтения порта остаётся прямым
//!   аргументом;
//! - граница замерена, а не угадана: два чтения подряд и чтение в аргументе
//!   функции, которой передаётся сам `hal`, законны - их разводит two-phase
//!   borrow, и поднимать там нечего.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Запись в порт, чьё значение читает порт, - во всех трёх формах значения.
const READS_PORT: &str = "in raw: u8 at 0x200;\n\
     in flag: bit at 0x201:0;\n\
     out echo: u8 at 0x300;\n\
     out lamp: bit at 0x301:0;\n\
     var ticks: u8 := 0;\n\
     fn twice(k: u8) -> u8 { return k + k; }\n\
     start Cycle {\n\
     \x20   always {\n\
     \x20       ticks := ticks + 1;\n\
     \x20       echo := twice(raw) + ticks;\n\
     \x20       lamp := flag;\n\
     \x20   }\n\
     \x20   ref Cycle: ticks < 200;\n\
     }\n";

/// Контроль: значение порта порта не читает - подъёму взяться неоткуда.
const NO_PORT_READ: &str = "out echo: u8 at 0x300;\n\
     var ticks: u8 := 0;\n\
     start Cycle {\n\
     \x20   always { ticks := ticks + 1; echo := ticks + 1; }\n\
     \x20   ref Cycle: ticks < 200;\n\
     }\n";

fn out_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0499_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог вывода");
    dir
}

fn generate(tag: &str, src: &str) -> (PathBuf, String) {
    let dir = out_dir(tag);
    takt_lang::compile_to_rust(
        tag,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение вывода");
    (dir, text)
}

fn tool(name: &str) -> bool {
    Command::new(name)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Вывод принимают `rustc` и `clippy` под флагами проверки цели.
///
/// Это главный тест фичи: текстовая проверка формы сама по себе не доказывает
/// валидности, а класс был именно невалидным выводом при нулевом коде возврата.
#[test]
fn generated_rust_passes_the_gate_tools() {
    if !tool("rustc") || !tool("clippy-driver") {
        eprintln!("[ПРОПУСК] `rustc`/`clippy-driver` не найдены; форма вывода проверена отдельно");
        return;
    }
    let (dir, _) = generate("rs0499t", READS_PORT);
    for exe in ["rustc", "clippy-driver"] {
        let out = Command::new(exe)
            .args(["--edition", "2021", "--crate-type", "lib", "-D", "warnings"])
            .arg(dir.join("rs0499t.rs"))
            .arg("--out-dir")
            .arg(&dir)
            .output()
            .unwrap_or_else(|e| panic!("запуск {exe}: {e}"));
        assert!(
            out.status.success(),
            "{exe} обязан принять вывод:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}

/// Значение, читающее порт, поднимается во временную - в обеих формах записи.
#[test]
fn port_read_is_lifted_for_both_write_kinds() {
    let (_, text) = generate("rs0499f", READS_PORT);
    for call in ["hal.write_u8(", "hal.write_bit("] {
        let line = text
            .lines()
            .find(|l| l.contains(call))
            .unwrap_or_else(|| panic!("в выводе нет вызова {call}:\n{text}"));
        assert!(
            line.contains("let takt_value ="),
            "значение обязано быть поднято во временную:\n{line}"
        );
        let at_call = &line[line.find(call).expect("вызов найден")..];
        assert!(
            !at_call.contains("hal.read_"),
            "внутри вызова записи чтения порта остаться не должно:\n{line}"
        );
    }
}

/// **Контроль:** без чтения порта временной не появляется.
///
/// Без него правка читалась бы как "поднимать всегда", а лишняя временная - это шум в
/// выводе, который люди читают, и расхождение снимков `examples/`.
#[test]
fn value_without_port_read_stays_direct() {
    let (_, text) = generate("rs0499c", NO_PORT_READ);
    let line = text
        .lines()
        .find(|l| l.contains("hal.write_u8("))
        .expect("запись в порт напечатана");
    assert!(
        !line.contains("takt_value"),
        "подъём печатается по нужде:\n{line}"
    );
}

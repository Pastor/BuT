//! Потактовая сверка сдвига на величину, не меньшую ширины типа.

use std::path::Path;
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_var_shift.takt";
const UNIT: &str = "conformance_var_shift";

fn rustc_available() -> bool {
    Command::new("rustc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Значения `(sr, ss, sl, ctl)` у эталона после первого такта.
fn simulator_values() -> (i128, i128, i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let _ = unit.tick();
    let number = |name: &str| match unit.variable(name) {
        Some(takt_sim::Value::Number(v)) => v,
        other => panic!("порт '{name}' обязан быть числом, получено {other:?}"),
    };
    (
        number("o_sr"),
        number("o_ss"),
        number("o_sl"),
        number("o_ctl"),
    )
}

/// Те же значения у порождённого Rust.
fn generated_rust_values(dir: &Path) -> (i128, i128, i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_rust(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");

    let module = dir.join(format!("{UNIT}.rs"));
    let driver = format!(
        r#"#[path = "{module}"]
mod model;
use model::*;
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ sr: Rc<RefCell<i32>>, ss: Rc<RefCell<i32>>, sl: Rc<RefCell<i32>>, ctl: Rc<RefCell<i32>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        match port {{
            OutU8Port::OSr => *self.sr.borrow_mut() = i32::from(value),
            OutU8Port::OSl => *self.sl.borrow_mut() = i32::from(value),
            OutU8Port::OCtl => *self.ctl.borrow_mut() = i32::from(value),
        }}
    }}
    fn write_i8(&mut self, port: OutI8Port, value: i8) {{
        match port {{
            OutI8Port::OSs => *self.ss.borrow_mut() = i32::from(value),
        }}
    }}
}}

fn main() {{
    let sr = Rc::new(RefCell::new(0));
    let ss = Rc::new(RefCell::new(0));
    let sl = Rc::new(RefCell::new(0));
    let ctl = Rc::new(RefCell::new(0));
    let mut model = ConformanceVarShift::new(Probe {{
        sr: Rc::clone(&sr), ss: Rc::clone(&ss), sl: Rc::clone(&sl), ctl: Rc::clone(&ctl),
    }});
    model.init();
    model.tick();
    println!("sr={{}} ss={{}} sl={{}} ctl={{}}", sr.borrow(), ss.borrow(), sl.borrow(), ctl.borrow());
}}
"#,
        module = module.display(),
    );
    let driver_path = dir.join("driver.rs");
    std::fs::write(&driver_path, driver).expect("запись драйвера");

    let build = Command::new("rustc")
        .current_dir(dir)
        .args(["--edition", "2021", "driver.rs", "-o", "driver"])
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "rustc не собрал драйвер со сдвигами:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("driver"))
        .current_dir(dir)
        .output()
        .expect("запуск драйвера");
    assert!(
        run.status.success(),
        "драйвер упал (прежде здесь была паника переполнения сдвига):\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
    let out = String::from_utf8_lossy(&run.stdout);
    let value = |key: &str| -> i128 {
        out.split_whitespace()
            .find_map(|token| token.strip_prefix(key)?.parse::<i128>().ok())
            .unwrap_or_else(|| panic!("драйвер обязан печатать {key}: {out}"))
    };
    (value("sr="), value("ss="), value("sl="), value("ctl="))
}

/// Эталон и порождённый Rust совпадают, а ожидание записано **числами**.
///
/// `ctl = 25` - контрольный вход (`200 >> 3`, величина меньше ширины): он работал
/// всегда, и без него совпадение сторон значило бы лишь, что они одинаково исполняют
/// исправленное.
#[test]
fn shift_by_type_width_matches_simulator_and_generated_rust() {
    let reference = simulator_values();
    assert_eq!(
        reference,
        (0, -1, 0, 25),
        "эталон: беззнаковый уходит в 0, знаковый оставляет знак, влево 0"
    );

    if !rustc_available() {
        eprintln!(
            "[ПРОПУСК] shift_by_type_width_matches_simulator_and_generated_rust: rustc не найден"
        );
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0334_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let generated = generated_rust_values(&dir);
    assert_eq!(
        reference, generated,
        "трассы разошлись: эталон {reference:?}, цель rust {generated:?}"
    );
}

/// Приведение величины к `u32` печатается **по нужде**.
///
/// У величины типа `u32` запись `x as u32` - это `clippy::unnecessary_cast`, то есть
/// **отказ** сборки порождённого кода под `-D warnings`. Проверка текстовая: линт
/// корпуса этот класс не видит - сдвигов в `examples/` нет ни одного.
#[test]
fn shift_amount_cast_is_printed_only_when_needed() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0334_cast_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let source = "var v: u8 := 200;\nvar wide: u32 := 8;\nvar r: u8 := 0;\n\
                  out o: u8 at 0;\nstart Run {\n  always { r := v >> wide; o := r; }\n\
                  ref Run: r = 0;\n}\n";
    takt_lang::compile_to_rust(
        "castprobe",
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join("castprobe.rs")).expect("чтение модуля");
    assert!(
        text.contains("checked_shr(self.wide)"),
        "величина типа u32 приведения не требует:\n{text}"
    );
}

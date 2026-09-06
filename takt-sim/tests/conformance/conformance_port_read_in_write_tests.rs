//! Сверка: значение выходного порта читает порт.

use std::path::Path;
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_port_read_in_write.takt";
const UNIT: &str = "conformance_port_read_in_write";
const TICKS: usize = 3;

fn rustc_available() -> bool {
    Command::new("rustc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Значения `(o_sum, o_bit, o_plain)` у эталона после трёх тактов.
fn simulator_values() -> (i128, i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    for _ in 0..TICKS {
        let _ = unit.tick();
    }
    let number = |name: &str| match unit.variable(name) {
        Some(takt_sim::Value::Number(v)) => v,
        Some(takt_sim::Value::Boolean(b)) => i128::from(b),
        other => panic!("порт '{name}' обязан быть числом, получено {other:?}"),
    };
    (number("o_sum"), number("o_bit"), number("o_plain"))
}

/// Те же значения у порождённого Rust.
fn generated_rust_values(dir: &Path) -> (i128, i128, i128) {
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

struct Probe {{ v: Rc<RefCell<[i32; 4]>> }}

impl Hal for Probe {{
    // Двунаправленный порт: сторона чтения отдаёт то, что модель записала.
    fn read_u8(&mut self, port: InU8Port) -> u8 {{
        match port {{ InU8Port::Line => self.v.borrow()[3] as u8 }}
    }}
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        let idx = match port {{
            OutU8Port::OSum => 0,
            OutU8Port::OPlain => 2,
            OutU8Port::Line => 3,
        }};
        self.v.borrow_mut()[idx] = i32::from(value);
    }}
    fn write_bit(&mut self, port: OutBitPort, value: bool) {{
        match port {{ OutBitPort::OBit => self.v.borrow_mut()[1] = i32::from(value) }}
    }}
}}

fn main() {{
    let v = Rc::new(RefCell::new([0i32; 4]));
    let mut model = ConformancePortReadInWrite::new(Probe {{ v: Rc::clone(&v) }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
    }}
    let v = v.borrow();
    println!("{{}} {{}} {{}}", v[0], v[1], v[2]);
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
        "rustc не собрал драйвер (прежде здесь был E0499):\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("driver"))
        .current_dir(dir)
        .output()
        .expect("запуск драйвера");
    assert!(run.status.success(), "драйвер упал");
    let out = String::from_utf8_lossy(&run.stdout);
    let nums: Vec<i128> = out
        .split_whitespace()
        .filter_map(|t| t.parse().ok())
        .collect();
    assert_eq!(nums.len(), 3, "драйвер печатает три числа: {out}");
    (nums[0], nums[1], nums[2])
}

/// Значения совпадают, а ожидание записано числами.
#[test]
fn port_read_in_write_matches_simulator_and_generated_rust() {
    let reference = simulator_values();
    assert_eq!(
        reference,
        (21, 1, 3),
        "эталон: o_sum = 2·9 + 3, o_bit = (9 > 5), контрольный o_plain = 3"
    );

    if !rustc_available() {
        eprintln!("[ПРОПУСК] port_read_in_write_matches_simulator_and_generated_rust: нет rustc");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0499_{}",
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
    let _ = std::fs::remove_dir_all(&dir);
}

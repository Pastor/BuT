//! Потактовая сверка **умолчаний** структуры и `q(m, n)` цели `rust`.

use std::path::Path;
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_default_value.takt";
const TICKS: usize = 5;

fn rustc_available() -> bool {
    Command::new("rustc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Трасса `(acc, grew)` эталона по тактам.
fn simulator_trace() -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::with_capacity(TICKS);
    for _ in 0..TICKS {
        let _ = unit.tick();
        let number = |name: &str| match unit.variable(name) {
            Some(takt_sim::Value::Number(v)) => v,
            other => panic!("порт '{name}': ожидалось число, получено {other:?}"),
        };
        trace.push((number("acc"), number("grew")));
    }
    trace
}

/// Трасса `(acc, grew)` порождённого Rust по тактам.
///
/// Драйвер пишется **здесь**, а не порождается `taktc`: он принадлежность проверки, а
/// не продукта (то же решение, что у прочих сверок цели `rust`).
fn generated_rust_trace(dir: &Path) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_rust(
        "conformance_default_value",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");

    let module = dir.join("conformance_default_value.rs");
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{ConformanceDefaultValue, Hal, OutBitPort, OutU8Port}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ acc: Rc<RefCell<u8>>, grew: Rc<RefCell<bool>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        assert!(matches!(port, OutU8Port::Acc), "неожиданный числовой порт");
        *self.acc.borrow_mut() = value;
    }}
    fn write_bit(&mut self, port: OutBitPort, value: bool) {{
        assert!(matches!(port, OutBitPort::Grew), "неожиданный битовый порт");
        *self.grew.borrow_mut() = value;
    }}
}}

fn main() {{
    let acc = Rc::new(RefCell::new(0u8));
    let grew = Rc::new(RefCell::new(false));
    let mut model = ConformanceDefaultValue::new(Probe {{
        acc: Rc::clone(&acc),
        grew: Rc::clone(&grew),
    }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
        println!("{{}} {{}}", acc.borrow(), i32::from(*grew.borrow()));
    }}
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
        "rustc не собрал драйвер умолчаний:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("driver"))
        .current_dir(dir)
        .output()
        .expect("запуск драйвера");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let (acc, grew) = line.split_once(' ')?;
            Some((acc.trim().parse().ok()?, grew.trim().parse().ok()?))
        })
        .collect()
}

/// С1: трасса цели `rust` совпадает с эталоном на умолчаниях структуры и `q`.
#[test]
fn default_values_match_simulator_and_generated_rust() {
    if !rustc_available() {
        eprintln!("[ПРОПУСК] default_values_match_simulator_and_generated_rust: `rustc` не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0351_rust_default");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");

    let reference = simulator_trace();
    // Предусловие: трасса значима - значение растёт, а `grew` переключается. Без этого
    // сверка была бы зелена и на выводе, где ничего не считается.
    assert_eq!(
        reference,
        vec![(3, 0), (6, 0), (9, 1), (12, 1), (12, 1)],
        "предусловие сверки: эталон обязан дать накапливающую трассу с \
         переключением на третьем такте"
    );

    let generated = generated_rust_trace(&dir);
    assert_eq!(
        reference, generated,
        "умолчания обязаны совпадать: эталон {reference:?}, цель rust {generated:?}"
    );
}

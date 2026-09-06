//! Порт перечислимого типа: цель `rust` против эталона.
//!
//! ## Что доказывает набор
//!
//! Значение перечисления пересекает границу HAL **дважды**: уходит целым (`as u8`) и
//! возвращается вариантом (`Mode::from_repr`). Ошибись хотя бы одна сторона - вывод
//! остался бы валидным (`rustc` принял бы `as u8` и без обратного преобразования, будь
//! приёмник целым), а автомат считал бы другое. Поэтому сверяется **трасса значений**,
//! а не факт компиляции.

use std::path::Path;
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_enum_port.takt";
const UNIT: &str = "enumportconf";
const TICKS: usize = 3;
/// `(mode, step)` по тактам: `Idle -> Run -> Halt`, счётчик тикает каждый такт.
const EXPECTED: [(i128, i128); TICKS] = [(1, 1), (2, 2), (2, 3)];

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0485_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn source() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

fn simulator_trace() -> Vec<(i128, i128)> {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit: Unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        assert!(
            !matches!(unit.tick(), TickResult::Failed(_)),
            "прогон не обрывается"
        );
        let mode = match unit.variable("mode") {
            Some(Value::Number(v)) => v,
            other => panic!("порт 'mode': {other:?}"),
        };
        let step = match unit.variable("step") {
            Some(Value::Number(v)) => v,
            other => panic!("порт 'step': {other:?}"),
        };
        trace.push((mode, step));
    }
    trace
}

fn rust_trace(dir: &Path) -> Vec<(i128, i128)> {
    takt_lang::compile_to_rust(
        UNIT,
        &source(),
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let module = dir.join(format!("{UNIT}.rs"));
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{Enumportconf, Hal, OutU8Port}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ reg: Rc<RefCell<(u8, u8)>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        match port {{
            OutU8Port::Mode => self.reg.borrow_mut().0 = value,
            OutU8Port::Step => self.reg.borrow_mut().1 = value,
        }}
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new((0u8, 0u8)));
    let mut model = Enumportconf::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
        let r = reg.borrow();
        println!("{{}} {{}}", r.0, r.1);
    }}
}}
"#,
        module = module.display(),
    );
    std::fs::write(dir.join("driver.rs"), driver).expect("драйвер");
    let build = Command::new("rustc")
        .current_dir(dir)
        .args(["--edition", "2021", "driver.rs", "-o", "driver"])
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "rustc не собрал драйвер:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("driver"))
        .current_dir(dir)
        .output()
        .expect("запуск драйвера");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let mut it = line.split_whitespace();
            Some((
                it.next()?.parse::<i128>().ok()?,
                it.next()?.parse::<i128>().ok()?,
            ))
        })
        .collect()
}

/// Эталон даёт ожидаемую трассу - она же ожидается от цели.
#[test]
fn simulator_trace_matches_expectation() {
    assert_eq!(simulator_trace(), EXPECTED.to_vec());
}

/// Цель `rust` считает то же самое: значение перечисления переживает HAL.
#[test]
fn rust_target_matches_simulator() {
    if !Command::new("rustc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        eprintln!("[ПРОПУСК] rust_target_matches_simulator: нет rustc");
        return;
    }
    let dir = temp_dir("rust");
    let trace = rust_trace(&dir);
    assert_eq!(
        trace,
        simulator_trace(),
        "цель `rust` обязана считать то же, что эталон"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

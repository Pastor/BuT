//! Арифметика с литералом слева у цели `rust`.

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::generator::GenerateOptions;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_literal_left.takt";
const UNIT: &str = "conformance_literal_left";
const TICKS: usize = 5;
/// Наблюдаемое: `(имя порта, вариант перечисления цели `rust`)`.
const PORTS: [(&str, &str); 3] = [("sum", "Sum"), ("diff", "Diff"), ("prod", "Prod")];

fn tool(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Каталог сборки, уникальный по потоку И процессу.
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0442_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn source() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

/// Трасса эталона по трём портам.
fn simulator_trace() -> Vec<Vec<i128>> {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut reg = vec![0i128; PORTS.len()];
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "эталон не обрывает прогон: {result:?}"
        );
        for (slot, (port, _)) in reg.iter_mut().zip(PORTS) {
            if let Some(Value::Number(v)) = unit.variable(port) {
                *slot = v;
            }
        }
        trace.push(reg.clone());
    }
    trace
}

/// Трасса модуля Rust: тот же набор портов, те же такты.
fn rust_trace(dir: &Path) -> Vec<Vec<i128>> {
    takt_lang::compile_to_rust(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let module = dir.join(format!("{UNIT}.rs"));
    let arms = PORTS
        .iter()
        .enumerate()
        .map(|(index, (_, variant))| {
            format!("            OutU8Port::{variant} => self.reg.borrow_mut()[{index}] = value,")
        })
        .collect::<Vec<_>>()
        .join("\n");
    let width = PORTS.len();
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{ConformanceLiteralLeft, Hal, OutU8Port}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ reg: Rc<RefCell<Vec<u8>>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        match port {{
{arms}
        }}
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new(vec![0u8; {width}]));
    let mut model = ConformanceLiteralLeft::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
        let values = reg.borrow();
        let line: Vec<String> = values.iter().map(|v| v.to_string()).collect();
        println!("{{}}", line.join(" "));
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
        "rustc не собрал драйвер — вывод цели `rust` невалиден:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("driver"))
        .current_dir(dir)
        .output()
        .expect("запуск драйвера");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
            line.split_whitespace()
                .map(|v| v.parse().expect("число в трассе"))
                .collect()
        })
        .collect()
}

/// Значения цели `rust` совпадают с эталонными такт в такт.
#[test]
fn literal_left_arithmetic_matches_reference() {
    if !tool("rustc") {
        eprintln!("rustc недоступен — сверка пропущена");
        return;
    }
    let expected = simulator_trace();
    let actual = rust_trace(&build_dir("trace"));
    assert_eq!(actual, expected, "цель rust разошлась с эталоном");
    // Контроль: наблюдаемое меняется по тактам, иначе подмена операнда была бы
    // незаметна.
    assert!(
        expected.first() != expected.last(),
        "трасса постоянна и сверкой ничего не доказывает: {expected:?}"
    );
}

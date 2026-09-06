//! Потактовая сверка табличной формы автомата у цели `rust`.
//!
//! # Что доказывает набор
//!
//! Флаг `--fsm=table` меняет **форму** порождённого Rust, а не поведение: эталон,
//! модуль формы `match` и модуль формы `table` дают одну трассу такт в такт - и на
//! простом автомате, и на последовательной композиции.
//!
//! Проверка цели (`rustc` + `clippy -D warnings`) этого не видит: таблица с переставленными
//! строками или с потерянным блоком `exit` компилируется без замечаний - вывод валиден,
//! автомат другой. Форма без сверки была бы утверждением, а не фактом.

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::generator::{FsmForm, GenerateOptions};
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Value, build_unit};

const SIMPLE_FIXTURE: &str = "tests/data/eval/conformance_fsm_table.takt";
const SIMPLE_UNIT: &str = "conformance_fsm_table";
const SIMPLE_TICKS: usize = 8;

const CHAIN_FIXTURE: &str = "tests/data/eval/conformance_fsm_table_chain.takt";
const CHAIN_UNIT: &str = "conformance_fsm_table_chain";
const CHAIN_TICKS: usize = 9;

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
        .join(format!("takt_0440_rust_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn source(fixture: &str) -> String {
    std::fs::read_to_string(fixture).expect("фикстура читается")
}

/// Трасса эталона: значения наблюдаемых портов по тактам.
fn simulator_trace(fixture: &str, ports: &[&str], ticks: usize) -> Vec<Vec<i128>> {
    let (ast, _) = takt_lang::parse(&source(fixture), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut reg = vec![0i128; ports.len()];
    for _ in 0..ticks {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "эталон не обрывает прогон: {result:?}"
        );
        for (slot, port) in reg.iter_mut().zip(ports) {
            if let Some(Value::Number(v)) = unit.variable(port) {
                *slot = v;
            }
        }
        trace.push(reg.clone());
    }
    trace
}

/// Трасса модуля Rust заданной формы.
fn rust_trace(
    dir: &Path,
    fixture: &str,
    unit: &str,
    ticks: usize,
    ports: &[(&str, &str)],
    fsm: FsmForm,
) -> Vec<Vec<i128>> {
    let mut options = GenerateOptions::default();
    options.fsm = fsm;
    takt_lang::compile_to_rust(
        unit,
        &source(fixture),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options,
    )
    .expect("порождение Rust");
    let module = dir.join(format!("{unit}.rs"));
    let struct_name = camel(unit);
    let arms = ports
        .iter()
        .enumerate()
        .map(|(index, (_, variant))| {
            format!("            OutU8Port::{variant} => self.reg.borrow_mut()[{index}] = value,")
        })
        .collect::<Vec<_>>()
        .join("\n");
    let width = ports.len();
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{{struct_name}, Hal, OutU8Port}};
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
    let mut model = {struct_name}::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{ticks} {{
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
        "rustc не собрал драйвер:\n{}",
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

/// Имя структуры корня: `conformance_fsm_table` -> `ConformanceFsmTable`.
fn camel(unit: &str) -> String {
    unit.split('_')
        .map(|part| {
            let mut chars = part.chars();
            match chars.next() {
                Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                None => String::new(),
            }
        })
        .collect()
}

/// Простой автомат: условное ребро, `enter`/`exit`, два конкурирующих ребра.
#[test]
fn rust_table_form_matches_match_form_and_simulator() {
    if !tool("rustc") {
        eprintln!("rustc недоступен — сверка пропущена");
        return;
    }
    let ports = [("probe", "Probe")];
    let names: Vec<&str> = ports.iter().map(|(n, _)| *n).collect();
    let expected = simulator_trace(SIMPLE_FIXTURE, &names, SIMPLE_TICKS);
    let switch = rust_trace(
        &build_dir("simple_match"),
        SIMPLE_FIXTURE,
        SIMPLE_UNIT,
        SIMPLE_TICKS,
        &ports,
        FsmForm::Switch,
    );
    let table = rust_trace(
        &build_dir("simple_table"),
        SIMPLE_FIXTURE,
        SIMPLE_UNIT,
        SIMPLE_TICKS,
        &ports,
        FsmForm::Table,
    );
    assert_eq!(switch, expected, "форма match разошлась с эталоном");
    assert_eq!(table, expected, "форма table разошлась с эталоном");
    // Контроль: трасса наблюдает и вход (`enter` даёт 100), и выход (`exit` - 200).
    let flat: Vec<i128> = expected.iter().flatten().copied().collect();
    assert!(
        flat.contains(&100) && flat.contains(&200),
        "трасса не наблюдает блоки enter/exit: {expected:?}"
    );
}

/// Последовательная композиция: выход из состояния-цепочки - строка таблицы.
#[test]
fn rust_table_form_matches_match_form_on_chain() {
    if !tool("rustc") {
        eprintln!("rustc недоступен — сверка пропущена");
        return;
    }
    let ports = [
        ("first_probe", "FirstProbe"),
        ("second_probe", "SecondProbe"),
        ("line_probe", "LineProbe"),
    ];
    let names: Vec<&str> = ports.iter().map(|(n, _)| *n).collect();
    let expected = simulator_trace(CHAIN_FIXTURE, &names, CHAIN_TICKS);
    let switch = rust_trace(
        &build_dir("chain_match"),
        CHAIN_FIXTURE,
        CHAIN_UNIT,
        CHAIN_TICKS,
        &ports,
        FsmForm::Switch,
    );
    let table = rust_trace(
        &build_dir("chain_table"),
        CHAIN_FIXTURE,
        CHAIN_UNIT,
        CHAIN_TICKS,
        &ports,
        FsmForm::Table,
    );
    assert_eq!(switch, expected, "форма match разошлась с эталоном");
    assert_eq!(table, expected, "форма table разошлась с эталоном");
    // Контроль: видны оба шага цепочки и жизнь после неё.
    let flat: Vec<i128> = expected.iter().flatten().copied().collect();
    assert!(
        flat.contains(&12) && flat.contains(&22) && flat.contains(&91),
        "трасса не наблюдает шаги цепочки: {expected:?}"
    );
}

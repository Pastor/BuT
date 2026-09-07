//! Наблюдение состояния соседа: эталон == цели `rust` и `st`.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_state_observe.takt";
const UNIT: &str = "conformance_state_observe";
const TICKS: usize = 6;

fn tool(name: &str) -> bool {
    Command::new(name)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0397_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Трасса эталона: `seen` на каждом такте.
fn simulator_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let _ = unit.tick();
        trace.push(match unit.variable("probe") {
            Some(takt_sim::Value::Number(v)) => v,
            other => panic!("порт 'probe' обязан быть числом, получено {other:?}"),
        });
    }
    trace
}

/// Та же трасса у порождённого Rust.
fn rust_trace(dir: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_rust(
        UNIT,
        &source,
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let module = dir.join(format!("{UNIT}.rs"));
    // Наблюдение идёт через HAL: поля порождённых структур приватны.
    let driver = format!(
        r#"// Модуль цели — библиотека: в драйвере часть его API не зовётся, и
// `-D warnings` считал бы это ошибкой (`dead_code`).
#![allow(dead_code)]
#[path = "{module}"]
mod model;
use model::*;
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ seen: Rc<RefCell<u8>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        match port {{
            OutU8Port::Probe => *self.seen.borrow_mut() = value,
        }}
    }}
}}

fn main() {{
    let seen = Rc::new(RefCell::new(0));
    let mut m = ConformanceStateObserve::new(Probe {{ seen: Rc::clone(&seen) }});
    m.init();
    for _ in 0..{TICKS} {{
        m.tick();
        println!("{{}}", seen.borrow());
    }}
}}
"#,
        module = module.display()
    );
    let driver_path = dir.join("driver.rs");
    std::fs::write(&driver_path, driver).expect("запись драйвера");
    let bin = dir.join("driver_bin");
    let build = Command::new("rustc")
        .args(["--edition", "2021", "-D", "warnings", "-o"])
        .arg(&bin)
        .arg(&driver_path)
        .output()
        .expect("запуск rustc");
    assert!(
        build.status.success(),
        "rustc не собрал драйвер:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера");
    assert!(run.status.success(), "драйвер завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|l| l.trim().parse::<i128>().expect("число"))
        .collect()
}

/// Эталон и цель `rust` совпадают потактово.
#[test]
fn state_observation_matches_the_reference_in_rust() {
    // Ожидание считается независимо: сосед уходит в `End` на 3-м такте, наблюдатель
    // входит в `Done` тогда же и начинает считать со следующего.
    let expected = vec![0, 0, 0, 1, 2, 3];
    let sim = simulator_trace();
    assert_eq!(sim, expected, "эталон разошёлся с ожиданием");

    if !tool("rustc") {
        eprintln!("[ПРОПУСК] `rustc` не найден; трасса эталона уже сверена");
        return;
    }
    let dir = build_dir("rust");
    assert_eq!(rust_trace(&dir), sim, "цель rust разошлась с эталоном");
}

/// Цель `st` порождает вывод, который принимает `iec2c`.
///
/// Значения у неё сверяет прогон порождённого C в проверке цели; здесь - доказательство,
/// что отказ `ST-011` снят и вывод валиден.
#[test]
fn state_observation_is_translated_in_st() {
    let dir = build_dir("st");
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_st(
        UNIT,
        &source,
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("цель st обязана перевести форму");

    let home = std::env::var("HOME").expect("HOME");
    let tool_path = PathBuf::from(&home).join(".local/bin/iec2c");
    if !tool_path.exists() {
        eprintln!("[ПРОПУСК] `iec2c` не установлен; перевод уже состоялся");
        return;
    }
    let out = Command::new(&tool_path)
        .arg("-I")
        .arg(format!("{home}/.local/share/matiec/lib"))
        .arg("-T")
        .arg(&dir)
        .arg(dir.join(format!("{UNIT}.st")))
        .output()
        .expect("запуск iec2c");
    assert!(
        out.status.success(),
        "iec2c обязан принять вывод:\n{}\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
}

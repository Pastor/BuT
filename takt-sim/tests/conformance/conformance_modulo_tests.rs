//! Потактовая сверка остатка в сравнении у цели `rust`.
//!
//! # Что доказывает набор
//!
//! Цель печатает `x % k = 0` методом (`x.is_multiple_of(k)`) - формой, которой
//! требует `clippy` под флагами проверки самой цели. Замена обязана быть
//! **тождественной**: трасса модуля совпадает с эталонной такт в такт.
//!
//! Компиляция этого не доказывает: `x.is_multiple_of(k)` собирается и тогда, когда
//! правило применили не к тому операнду (например к знаковому, где смысл остатка
//! другой). Поэтому предмет проверки - **значения**, а вывод дополнительно прогоняется
//! через `clippy -D warnings`: без этого шага фича не доказывает того, ради чего
//! заведена.

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::generator::GenerateOptions;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_modulo.takt";
const UNIT: &str = "conformance_modulo";
const TICKS: usize = 10;

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
        .join(format!("takt_0448_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn source() -> String {
    std::fs::read_to_string(FIXTURE).expect("фикстура читается")
}

/// Трасса эталона по порту `probe`.
fn simulator_trace() -> Vec<i128> {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut probe = 0i128;
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "эталон не обрывает прогон: {result:?}"
        );
        if let Some(Value::Number(v)) = unit.variable("probe") {
            probe = v;
        }
        trace.push(probe);
    }
    trace
}

/// Порождает модуль Rust; отдаёт путь к нему.
fn emit(dir: &Path) -> PathBuf {
    takt_lang::compile_to_rust(
        UNIT,
        &source(),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    dir.join(format!("{UNIT}.rs"))
}

/// Трасса модуля Rust: тот же порт, те же такты.
fn rust_trace(dir: &Path) -> Vec<i128> {
    let module = emit(dir);
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{ConformanceModulo, Hal, OutU8Port}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ reg: Rc<RefCell<u8>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        match port {{
            OutU8Port::Probe => *self.reg.borrow_mut() = value,
        }}
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new(0u8));
    let mut model = ConformanceModulo::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
        println!("{{}}", reg.borrow());
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
        .filter_map(|l| l.trim().parse::<i128>().ok())
        .collect()
}

/// Значения совпадают с эталонными: замена формы поведения не меняет.
#[test]
fn modulo_comparison_matches_reference() {
    if !tool("rustc") {
        eprintln!("rustc недоступен — сверка пропущена");
        return;
    }
    let expected = simulator_trace();
    let actual = rust_trace(&build_dir("trace"));
    assert_eq!(actual, expected, "цель rust разошлась с эталоном");
    // Контроль: трасса проходит через разные сочетания условий - на постоянной подмена
    // операнда была бы незаметна.
    let distinct: std::collections::BTreeSet<i128> = expected.iter().copied().collect();
    assert!(
        distinct.len() >= 3,
        "трасса слишком бедна и сверкой ничего не доказывает: {expected:?}"
    );
}

/// Вывод принимает `clippy` под флагами проверки цели - то, ради чего фича.
#[test]
fn output_is_accepted_by_clippy() {
    if !tool("clippy-driver") {
        eprintln!("clippy-driver недоступен — шаг пропущен");
        return;
    }
    let dir = build_dir("clippy");
    let module = emit(&dir);
    let out = Command::new("clippy-driver")
        .current_dir(&dir)
        .args(["--edition", "2021", "--crate-type", "lib", "-D", "warnings"])
        .arg(&module)
        .output()
        .expect("запуск clippy-driver");
    assert!(
        out.status.success(),
        "clippy отверг вывод цели `rust`:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let text = std::fs::read_to_string(&module).expect("модуль читается");
    // Правило применено там, где тождественно...
    assert!(
        text.contains("self.n.is_multiple_of(2)") && text.contains("!self.n.is_multiple_of(3)"),
        "остаток с литеральным делителем печатается не методом:\n{text}"
    );
    // Литерал слева - та же запись для линтера, значит и для правила.
    assert!(
        text.contains("self.n.is_multiple_of(5)"),
        "форма `0 = x % k` осталась без правила:\n{text}"
    );
    // ...и не применено там, где замена изменила бы поведение: у переменного делителя
    // (`% 0` паникует, `is_multiple_of(0)` - нет) и у знакового операнда (метода в
    // стабильном Rust нет).
    assert!(
        text.contains("% (self.d as i32)") && !text.contains("self.d)"),
        "остаток с переменным делителем печатается не как прежде:\n{text}"
    );
    assert!(
        !text.contains("self.s.is_multiple_of"),
        "правило применено к знаковому операнду:\n{text}"
    );
}

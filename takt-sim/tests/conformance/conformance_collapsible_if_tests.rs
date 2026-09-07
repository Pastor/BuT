//! Потактовая сверка: слияние вложенного `if` не меняет автомат.

use std::path::Path;
use std::process::Command;

/// Фикстура: ветвь `1 => { if acc > 100 { ... } }` стоит перед `_`.
const FIXTURE: &str = "tests/data/eval/conformance_collapsible_if.takt";
/// Тактов: первый - ветвь с ложным вложенным условием, дальше - `_`.
const TICKS: usize = 4;

fn rustc_available() -> bool {
    Command::new("rustc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Потактовая трасса порта `probe` у эталона.
fn simulator_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::with_capacity(TICKS);
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, takt_sim::TickResult::Failed(_)),
            "эталон не должен падать: {result:?}"
        );
        match unit.variable("probe") {
            Some(takt_sim::Value::Number(v)) => trace.push(v),
            other => panic!("порт 'probe' обязан быть числом, получено {other:?}"),
        }
    }
    trace
}

/// Та же трасса у порождённой прошивки.
fn generated_rust_trace(dir: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_rust(
        "conformance_collapsible_if",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");

    let module = dir.join("conformance_collapsible_if.rs");
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{ConformanceCollapsibleIf, Hal, OutU8Port}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ reg: Rc<RefCell<u8>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        assert!(matches!(port, OutU8Port::Probe), "неожиданный порт");
        *self.reg.borrow_mut() = value;
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new(0u8));
    let mut model = ConformanceCollapsibleIf::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
        println!("TICK {{}}", reg.borrow());
    }}
}}
"#,
        module = module.display(),
    );
    std::fs::write(dir.join("driver.rs"), driver).expect("запись драйвера");

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
    assert!(run.status.success(), "прошивка обязана досчитать трассу");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| line.strip_prefix("TICK "))
        .map(|v| v.trim().parse::<i128>().expect("значение — целое"))
        .collect()
}

/// Предмет: ветвь перед `_` не сливается, и первый такт ничего не делает.
#[test]
fn arm_before_default_keeps_its_semantics() {
    let sim = simulator_trace();
    // Числа названы явно: "трассы совпали" ничего не стоит, если обе стороны ошиблись
    // одинаково. Ноль первого такта - весь предмет.
    assert_eq!(
        sim,
        vec![0, 10, 20, 30],
        "первый такт обязан не делать НИЧЕГО: слитая ветвь дала бы 10"
    );

    if !rustc_available() {
        eprintln!("[ПРОПУСК] rustc не найден — сверка с прошивкой не выполнена");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_conformance_collapsible_if_{}",
            std::thread::current()
                .name()
                .unwrap_or("t")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    let rs = generated_rust_trace(&dir);
    assert_eq!(
        sim, rs,
        "потактовые трассы эталона и порождённого Rust обязаны совпадать"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

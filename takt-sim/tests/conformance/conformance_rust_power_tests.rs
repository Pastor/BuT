//! Потактовая сверка значений степени цели `rust` с эталоном.

use std::path::Path;
use std::process::Command;

/// Фикстура: сумма четырёх форм записи степени, все операнды растут по тактам.
const FIXTURE: &str = "tests/data/eval/conformance_power_operands.takt";
/// Тактов в трассе - с запасом над её длиной (модель завершается на третьем).
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
    let mut trace = Vec::new();
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

/// Потактовая трасса того же порта у порождённой прошивки.
///
/// Драйвер пишется **здесь**, а не порождается `taktc`: он принадлежность проверки, а
/// не продукта (решение сверок `rust` и `sv`).
fn generated_rust_trace(dir: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_rust(
        "conformance_power_operands",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");

    let module = dir.join("conformance_power_operands.rs");
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{ConformancePowerOperands, Hal, OutU8Port}};
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
    let mut model = ConformancePowerOperands::new(Probe {{ reg: Rc::clone(&reg) }});
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
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| line.strip_prefix("TICK "))
        .map(|v| v.trim().parse::<i128>().expect("значение — целое"))
        .collect()
}

/// Предмет: трассы совпадают потактово, и значения растут.
#[test]
fn power_operands_match_the_reference() {
    let sim = simulator_trace();
    // Значения названы явно: молчаливое "трассы совпали" ничего не стоит, если обе
    // стороны считают одно и то же неверно.
    assert_eq!(
        sim,
        vec![12, 31, 148, 148],
        "эталон обязан давать 4+4+2+2, 4+9+9+9, 4+16+64+64 и удержание"
    );

    if !rustc_available() {
        eprintln!("[ПРОПУСК] rustc не найден — сверка с прошивкой не выполнена");
        return;
    }
    let dir = std::env::temp_dir().join(format!(
        "takt_conformance_rust_power_{}_{}",
        std::process::id(),
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

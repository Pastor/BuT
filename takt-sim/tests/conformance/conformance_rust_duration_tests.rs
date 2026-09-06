//! Потактовая сверка **значений** типа `duration` цели `rust` с эталоном.
//!
//! Отдельный файл по образцу `conformance_c_duration_tests.rs`: предмет - значение
//! длительности, а не выдержка. Представление эталона - наносекунды, представление цели -
//! **миллисекунды**; ошибка на этой границе была бы молчаливой.
//!
//! Проверка `rustc`/`clippy` доказывает лишь **компилируемость**: расхождение приоритетов C
//! и Rust собирается прекрасно, а неверная единица времени - тем более. Поэтому
//! сверяются числа, и числа названы явно.

use std::path::Path;
use std::process::Command;

/// Исправлениетура: `elapsed := pause + 750ms`, `ms := elapsed as u32`, `late := elapsed >
/// 500ms`.
const FIXTURE: &str = "tests/data/eval/conformance_duration_value.takt";

fn rustc_available() -> bool {
    Command::new("rustc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Значения `(ms, late)` у эталона после первого такта.
fn simulator_values() -> (i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let _ = unit.tick();
    let number = |name: &str| match unit.variable(name) {
        Some(takt_sim::Value::Number(v)) => v,
        other => panic!("порт '{name}' обязан быть числом, получено {other:?}"),
    };
    (number("ms"), number("late"))
}

/// Значения `(ms, late)` у порождённого Rust после первого такта.
///
/// Драйвер пишется **здесь**, а не порождается `taktc`: он принадлежность проверки, а
/// не продукта (то же решение, что у сверок `rust` и `sv`).
fn generated_rust_values(dir: &Path) -> (i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_rust(
        "conformance_duration_value",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");

    let module = dir.join("conformance_duration_value.rs");
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{ConformanceDurationValue, Hal, OutBitPort, OutU32Port}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ ms: Rc<RefCell<u32>>, late: Rc<RefCell<bool>> }}

impl Hal for Probe {{
    fn write_u32(&mut self, port: OutU32Port, value: u32) {{
        assert!(matches!(port, OutU32Port::Ms), "неожиданный числовой порт");
        *self.ms.borrow_mut() = value;
    }}
    fn write_bit(&mut self, port: OutBitPort, value: bool) {{
        assert!(matches!(port, OutBitPort::Late), "неожиданный битовый порт");
        *self.late.borrow_mut() = value;
    }}
}}

fn main() {{
    let ms = Rc::new(RefCell::new(0u32));
    let late = Rc::new(RefCell::new(false));
    let mut model = ConformanceDurationValue::new(Probe {{
        ms: Rc::clone(&ms),
        late: Rc::clone(&late),
    }});
    model.init();
    model.tick();
    println!("ms={{}} late={{}}", ms.borrow(), i32::from(*late.borrow()));
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
        "rustc не собрал драйвер со значениями duration:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(dir.join("driver"))
        .current_dir(dir)
        .output()
        .expect("запуск драйвера");
    let out = String::from_utf8_lossy(&run.stdout);
    let mut ms = None;
    let mut late = None;
    for token in out.split_whitespace() {
        if let Some(v) = token.strip_prefix("ms=") {
            ms = v.parse::<i128>().ok();
        }
        if let Some(v) = token.strip_prefix("late=") {
            late = v.parse::<i128>().ok();
        }
    }
    (
        ms.expect("драйвер печатает ms"),
        late.expect("драйвер печатает late"),
    )
}

#[test]
fn duration_values_match_simulator_and_generated_rust() {
    if !rustc_available() {
        eprintln!(
            "[ПРОПУСК] duration_values_match_simulator_and_generated_rust: `rustc` не найден"
        );
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0183_rust_duration");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let reference = simulator_values();
    let generated = generated_rust_values(&dir);
    assert_eq!(
        reference, generated,
        "значения обязаны совпадать: эталон {reference:?}, цель rust {generated:?}"
    );
    assert_eq!(
        reference,
        (1_750, 1),
        "ожидались ms=1750 и late=1, получено {reference:?}"
    );
}

/// Приведение `as` **не порождает арифметики** - та же проверка, что у цели `c`:
/// единица представления одна, поэтому пересчёту взяться неоткуда.
///
/// **Прежде здесь требовался текст `self.elapsed as u32`, и это утверждение перестало
/// быть верным**: такая печать есть `clippy::unnecessary_cast`, то есть отказ проверки
/// цели - `duration` отображается в `u32`. Предмет проверки не изменился.
#[test]
fn cast_between_duration_and_number_emits_no_arithmetic_in_rust() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0183_rust_duration_text");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_rust(
        "conformance_duration_value",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let code = std::fs::read_to_string(dir.join("conformance_duration_value.rs"))
        .expect("порождённый .rs");
    assert!(
        code.contains("self.elapsed"),
        "значение обязано читаться напрямую:\n{code}"
    );
    assert!(
        !code.contains("self.elapsed as u32"),
        "приведение к тому же напечатанному типу не печатается (фича 0374):\n{code}"
    );
    for forbidden in ["1000000", "/ 1000", "* 1000"] {
        assert!(
            !code.contains(forbidden),
            "в выводе не должно быть пересчёта единиц ('{forbidden}'):\n{code}"
        );
    }
}

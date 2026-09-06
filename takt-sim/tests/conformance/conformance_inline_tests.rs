//! Потактовая сверка подстановки тела функции.
//!
//! # Что доказывает набор
//!
//! Атрибут `[inline]` и флаг `--inline=auto` меняют **форму** порождённого кода, а не
//! поведение: эталон, прошивка без подстановки и прошивка с ней обязаны давать одну
//! трассу такт в такт - у цели `c` и у цели `rust`.
//!
//! **Эталон подстановку не делает, и это замысел.** Позови её обе стороны - сверка
//! перестала бы видеть дефект подстановки и говорила бы лишь, что стороны "ошибаются
//! одинаково".
//!
//! Проверки целей этого класса не видят по устройству: подстановка, потерявшая
//! переименование параметра, собирается тем же `cc -Werror` и тем же `clippy -D
//! warnings` без замечаний - вывод валиден, значения другие. Исправлениетура для того и держит
//! параметр, одноимённый переменной модели.

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::generator::{GenerateOptions, InlinePolicy};
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_inline.takt";
const UNIT: &str = "conformance_inline";
const TICKS: usize = 6;

/// Исправлениетура раннего возврата: подстановка идёт через признак выхода, и разойтись с
/// вызовом она может каждым порогом.
const EARLY_FIXTURE: &str = "tests/data/eval/conformance_inline_early.takt";
const EARLY_UNIT: &str = "conformance_inline_early";
const EARLY_TICKS: usize = 8;

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
        .join(format!("takt_0444_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn source(fixture: &str) -> String {
    std::fs::read_to_string(fixture).expect("фикстура читается")
}

/// Трасса эталона: значение порта `probe` по тактам.
fn simulator_trace(fixture: &str, ticks: usize) -> Vec<i128> {
    let (ast, _) = takt_lang::parse(&source(fixture), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut probe = 0i128;
    for _ in 0..ticks {
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

/// Трасса прошивки цели `c` при заданном режиме подстановки.
fn c_trace(dir: &Path, fixture: &str, unit: &str, ticks: usize, inline: InlinePolicy) -> Vec<i128> {
    let mut options = GenerateOptions::default();
    options.inline = inline;
    takt_lang::compile_to_c(
        unit,
        &source(fixture),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options,
    )
    .expect("порождение C");
    let camel = camel(unit);
    let harness = format!(
        r#"#include <stdio.h>
#include "{unit}.h"
static long long probe;
static void on_num({camel}_Out_NumericPort port, uint8_t index, int64_t value, void *userdata) {{
    (void)index;
    (void)userdata;
    if (port == {upper}_WORKER_PORT_PROBE) {{ probe = (long long)value; }}
}}
int main(void) {{
    {camel} m;
    {camel}_init(&m);
    m.write_numeric = on_num;
    m.userdata = 0;
    for (int i = 0; i < {ticks}; i++) {{
        {camel}_tick(&m);
        printf("%lld\n", probe);
    }}
    return 0;
}}
"#,
        upper = unit.to_uppercase(),
    );
    std::fs::write(dir.join("harness.c"), harness).expect("харнесс");
    let bin = dir.join("bin");
    let compile = Command::new("cc")
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-parameter",
            "-Werror",
            "-o",
        ])
        .arg(&bin)
        .arg(dir.join("harness.c"))
        .arg(dir.join(format!("{unit}.c")))
        .arg("-I")
        .arg(dir)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "cc не собрал харнесс флагами гейта цели:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск харнесса");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.trim().parse::<i128>().ok())
        .collect()
}

/// Трасса модуля Rust при заданном режиме подстановки.
fn rust_trace(
    dir: &Path,
    fixture: &str,
    unit: &str,
    ticks: usize,
    inline: InlinePolicy,
) -> Vec<i128> {
    let mut options = GenerateOptions::default();
    options.inline = inline;
    takt_lang::compile_to_rust(
        unit,
        &source(fixture),
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options,
    )
    .expect("порождение Rust");
    let module = dir.join(format!("{unit}.rs"));
    let camel = camel(unit);
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{{camel}, Hal, OutU8Port}};
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
    let mut model = {camel}::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{ticks} {{
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

/// Имя структуры корня: `conformance_inline` -> `ConformanceInline`.
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

/// Цель `c`: подстановка не меняет значений - ни атрибутом, ни эвристикой.
#[test]
fn inlined_c_firmware_matches_reference() {
    if !tool("cc") {
        eprintln!("cc недоступен — сверка пропущена");
        return;
    }
    let expected = simulator_trace(FIXTURE, TICKS);
    let plain = c_trace(&build_dir("c_off"), FIXTURE, UNIT, TICKS, InlinePolicy::Off);
    let auto = c_trace(
        &build_dir("c_auto"),
        FIXTURE,
        UNIT,
        TICKS,
        InlinePolicy::Auto,
    );
    assert_eq!(plain, expected, "прошивка с атрибутом разошлась с эталоном");
    assert_eq!(auto, expected, "прошивка с эвристикой разошлась с эталоном");
    // Контроль: трасса меняется по тактам - на постоянной подмена операнда была бы
    // незаметна.
    assert!(
        expected.first() != expected.last(),
        "трасса постоянна и сверкой ничего не доказывает: {expected:?}"
    );
}

/// Цель `rust`: то же самое у второго потребителя подстановки.
#[test]
fn inlined_rust_module_matches_reference() {
    if !tool("rustc") {
        eprintln!("rustc недоступен — сверка пропущена");
        return;
    }
    let expected = simulator_trace(FIXTURE, TICKS);
    let plain = rust_trace(
        &build_dir("rs_off"),
        FIXTURE,
        UNIT,
        TICKS,
        InlinePolicy::Off,
    );
    let auto = rust_trace(
        &build_dir("rs_auto"),
        FIXTURE,
        UNIT,
        TICKS,
        InlinePolicy::Auto,
    );
    assert_eq!(plain, expected, "модуль с атрибутом разошёлся с эталоном");
    assert_eq!(auto, expected, "модуль с эвристикой разошёлся с эталоном");
}

/// Ранний возврат: признак выхода считает ровно то же, что вызов.
///
/// Эталон вызов **исполняет**, а не подставляет, поэтому сверка проверяет именно
/// преобразование: пропущенная обёртка "выхода ещё не было" даёт валидный код,
/// считающий другое (последний порог перезаписал бы ранний).
#[test]
fn early_return_inlining_matches_reference() {
    let expected = simulator_trace(EARLY_FIXTURE, EARLY_TICKS);
    // Контроль осмысленности: трасса проходит через разные ветви - иначе ранние выходы
    // могли ни разу не сработать, и сверка ничего не значила бы.
    let distinct: std::collections::BTreeSet<i128> = expected.iter().copied().collect();
    assert!(
        distinct.len() >= 5,
        "трасса слишком бедна, ранние выходы могли не сработать: {expected:?}"
    );
    if tool("cc") {
        let actual = c_trace(
            &build_dir("early_c"),
            EARLY_FIXTURE,
            EARLY_UNIT,
            EARLY_TICKS,
            InlinePolicy::Off,
        );
        assert_eq!(
            actual, expected,
            "цель c разошлась с эталоном на раннем возврате"
        );
    }
    if tool("rustc") {
        let actual = rust_trace(
            &build_dir("early_rs"),
            EARLY_FIXTURE,
            EARLY_UNIT,
            EARLY_TICKS,
            InlinePolicy::Off,
        );
        assert_eq!(
            actual, expected,
            "цель rust разошлась с эталоном на раннем возврате"
        );
    }
}

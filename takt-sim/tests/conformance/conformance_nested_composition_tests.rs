//! Вложенная последовательная композиция внутри параллельной.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_nested_composition.takt";
const UNIT: &str = "conformance_nested_composition";
const TICKS: usize = 6;

fn tool(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn build_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "takt_0426_{tag}_{}_{}",
        std::process::id(),
        std::thread::current()
            .name()
            .unwrap_or("t")
            .replace(':', "_")
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Трасса эталона: `(oa, ob, oc)` по тактам.
fn simulator_trace() -> Vec<(i128, i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    // Наблюдение через регистр, а не через прямое чтение: `unit.variable` отдаёт порт
    // только у ветви, которая сейчас активна, а завершившаяся ветвь из ответа исчезает.
    let mut reg = [0i128; 3];
    for _ in 0..TICKS {
        let _ = unit.tick();
        for (idx, name) in ["oa", "ob", "oc"].iter().enumerate() {
            if let Some(Value::Number(v)) = unit.variable(name) {
                reg[idx] = v;
            }
        }
        trace.push((reg[0], reg[1], reg[2]));
    }
    trace
}

/// Трасса прошивки цели `c`.
fn generated_c_trace(dir: &Path) -> Vec<(i128, i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

static long long reg[3] = {{0, 0, 0}};

static void on_write(ConformanceNestedComposition_Out_NumericPort port, uint8_t index, int64_t value, void *ud) {{
    (void)index;
    (void)ud;
    reg[(int)port] = (long long)value;
}}

int main(void) {{
    ConformanceNestedComposition m;
    ConformanceNestedComposition_init(&m);
    m.write_numeric = on_write;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceNestedComposition_tick(&m);
        printf("%lld %lld %lld\n", reg[0], reg[1], reg[2]);
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), &harness).expect("запись харнесса");
    let bin = dir.join("nested_bin");
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
        .arg(dir.join(format!("{UNIT}.c")))
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
    assert!(run.status.success(), "харнесс завершился с ошибкой");
    parse_trace(&String::from_utf8_lossy(&run.stdout))
}

/// Трасса прошивки цели `rust`.
fn generated_rust_trace(dir: &Path) -> Vec<(i128, i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_rust(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение Rust");

    let module = dir.join(format!("{UNIT}.rs"));
    let driver = format!(
        r#"#[path = "{module}"]
mod generated;
use generated::{{ConformanceNestedComposition, Hal, OutU8Port}};
use std::cell::RefCell;
use std::rc::Rc;

struct Probe {{ reg: Rc<RefCell<[u8; 3]>> }}

impl Hal for Probe {{
    fn write_u8(&mut self, port: OutU8Port, value: u8) {{
        let idx = match port {{
            OutU8Port::Oa => 0,
            OutU8Port::Ob => 1,
            OutU8Port::Oc => 2,
        }};
        self.reg.borrow_mut()[idx] = value;
    }}
}}

fn main() {{
    let reg = Rc::new(RefCell::new([0u8; 3]));
    let mut model = ConformanceNestedComposition::new(Probe {{ reg: Rc::clone(&reg) }});
    model.init();
    for _ in 0..{TICKS} {{
        model.tick();
        let r = reg.borrow();
        println!("{{}} {{}} {{}}", r[0], r[1], r[2]);
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
    parse_trace(&String::from_utf8_lossy(&run.stdout))
}

fn parse_trace(text: &str) -> Vec<(i128, i128, i128)> {
    text.lines()
        .filter_map(|line| {
            let mut it = line.split_whitespace();
            Some((
                it.next()?.parse::<i128>().ok()?,
                it.next()?.parse::<i128>().ok()?,
                it.next()?.parse::<i128>().ok()?,
            ))
        })
        .collect()
}

/// Значения эталона названы числами: A идёт до B, C - параллельно.
#[test]
fn reference_values_are_named() {
    assert_eq!(
        simulator_trace(),
        vec![
            (1, 0, 3),
            (2, 0, 6),
            (2, 0, 9),
            (2, 2, 9),
            (2, 4, 9),
            (2, 4, 9)
        ],
        "A завершается на такте 2, B начинает считать с такта 4, C идёт параллельно"
    );
}

/// Прошивка цели `c` считает то же.
#[test]
fn nested_composition_matches_generated_c() {
    if !tool("cc") {
        eprintln!("[ПРОПУСК] nested_composition_matches_generated_c: cc не найден");
        return;
    }
    let dir = build_dir("c");
    let sim = simulator_trace();
    let firmware = generated_c_trace(&dir);
    assert_eq!(sim, firmware, "трассы эталона и прошивки обязаны совпадать");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Прошивка цели `rust` считает то же.
#[test]
fn nested_composition_matches_generated_rust() {
    if !tool("rustc") {
        eprintln!("[ПРОПУСК] nested_composition_matches_generated_rust: rustc не найден");
        return;
    }
    let dir = build_dir("rust");
    let sim = simulator_trace();
    let firmware = generated_rust_trace(&dir);
    assert_eq!(sim, firmware, "трассы эталона и прошивки обязаны совпадать");
    let _ = std::fs::remove_dir_all(&dir);
}

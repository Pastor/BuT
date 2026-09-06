//! Именованное условие с вызовом функции: цель `c` против эталона.
//!
//! ## Что доказывает набор
//!
//! Компиляция доказала бы лишь, что отказ снят. Наблюдаются две величины: `probe` -
//! значение самой функции, `phase` - сторона, выбранная условием. Перепривяжись вызов к
//! другой функции - разошлось бы первое; сработай условие не в тот такт - второе.

use std::path::Path;
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_cond_call.takt";
const UNIT: &str = "condcall";
const TICKS: usize = 4;
/// `(probe, phase)` по тактам: уход в `Hot` - на втором.
const EXPECTED: [(i128, i128); TICKS] = [(2, 1), (4, 1), (4, 2), (4, 2)];

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0503_{tag}_{}",
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
            "прогон не обрывается (прежде здесь был SIM-016)"
        );
        let number = |name: &str| match unit.variable(name) {
            Some(Value::Number(v)) => v,
            other => panic!("порт '{name}': {other:?}"),
        };
        trace.push((number("probe"), number("phase")));
    }
    trace
}

fn c_trace(dir: &Path) -> Vec<(i128, i128)> {
    takt_lang::compile_to_c(
        UNIT,
        &source(),
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"
static long long probe, phase;
static void on_write(Condcall_Out_NumericPort p, uint8_t index, int64_t v, void *u) {{
    (void)index;
    (void)u;
    if (p == CONDCALL_COND_CALL_PORT_PROBE) probe = (long long)v; else phase = (long long)v;
}}
int main(void) {{
    Condcall m;
    Condcall_init(&m);
    m.write_numeric = on_write;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        Condcall_tick(&m);
        printf("%lld %lld\n", probe, phase);
    }}
    return 0;
}}
"#
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

/// Цель `c` считает то же: вызов в именованном условии перепривязан.
#[test]
fn c_target_matches_simulator() {
    if !Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        eprintln!("[ПРОПУСК] c_target_matches_simulator: нет cc");
        return;
    }
    let dir = temp_dir("c");
    let trace = c_trace(&dir);
    assert_eq!(
        trace,
        EXPECTED.to_vec(),
        "условие обязано звать ТУ функцию и срабатывать в ТОТ такт"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

//! Узел-поддерево составного порта: цель `c` против эталона.
//!
//! ## Что доказывает набор
//!
//! Сборка доказывает, что вывод валиден; она **не** доказывает, что узел собран из тех
//! листьев и в том порядке. Переставь сборка `{a, b}` на `{b, a}` - вывод остался бы
//! валидным, а значения разошлись бы. Поэтому листья различаются по величине, а
//! наблюдаются оба: `tail.a`, `tail.b` - со стороны записи, `probe` (`seen.b - seen.a`) -
//! со стороны чтения.

use std::path::Path;
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_port_subtree.takt";
const UNIT: &str = "portsubtree";
const TICKS: usize = 3;
/// `(probe, cfg.tail.a, cfg.tail.b)` по тактам.
const EXPECTED: [(i128, i128, i128); TICKS] = [(10, 1, 11), (10, 2, 12), (10, 3, 13)];

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0501_{tag}_{}",
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

/// Поле структурного значения эталона по пути.
fn field(value: &Value, path: &[&str]) -> i128 {
    match (value, path.split_first()) {
        (Value::Number(v), None) => *v,
        (Value::Struct { fields, .. }, Some((name, rest))) => fields
            .iter()
            .find(|(f, _)| f == name)
            .map(|(_, v)| field(v, rest))
            .unwrap_or_else(|| panic!("поля '{name}' нет в значении")),
        other => panic!("неожиданное значение: {other:?}"),
    }
}

fn simulator_trace() -> Vec<(i128, i128, i128)> {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit: Unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        assert!(
            !matches!(unit.tick(), TickResult::Failed(_)),
            "прогон не обрывается"
        );
        let probe = match unit.variable("probe") {
            Some(Value::Number(v)) => v,
            other => panic!("порт 'probe': {other:?}"),
        };
        let cfg = unit.variable("cfg").expect("порт 'cfg'");
        trace.push((
            probe,
            field(&cfg, &["tail", "a"]),
            field(&cfg, &["tail", "b"]),
        ));
    }
    trace
}

fn c_trace(dir: &Path) -> Vec<(i128, i128, i128)> {
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
static long long probe, head, tail_a, tail_b;
static void on_write(Portsubtree_Out_NumericPort p, uint8_t index, int64_t v, void *u) {{
    (void)index;
    (void)u;
    switch (p) {{
        case PORTSUBTREE_PORT_SUBTREE_PORT_PROBE: probe = (long long)v; break;
        case PORTSUBTREE_PORT_SUBTREE_PORT_CFG_HEAD_OUT: head = (long long)v; break;
        case PORTSUBTREE_PORT_SUBTREE_PORT_CFG_TAIL_A_OUT: tail_a = (long long)v; break;
        case PORTSUBTREE_PORT_SUBTREE_PORT_CFG_TAIL_B_OUT: tail_b = (long long)v; break;
        default: break;
    }}
}}
static int64_t on_read(Portsubtree_In_NumericPort p, uint8_t index, void *u) {{
    (void)index;
    (void)u;
    switch (p) {{
        case PORTSUBTREE_PORT_SUBTREE_PORT_CFG_HEAD_IN: return (int64_t)head;
        case PORTSUBTREE_PORT_SUBTREE_PORT_CFG_TAIL_A_IN: return (int64_t)tail_a;
        case PORTSUBTREE_PORT_SUBTREE_PORT_CFG_TAIL_B_IN: return (int64_t)tail_b;
        default: return 0;
    }}
}}
int main(void) {{
    Portsubtree m;
    Portsubtree_init(&m);
    m.write_numeric = on_write;
    m.read_numeric = on_read;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        Portsubtree_tick(&m);
        printf("%lld %lld %lld\n", probe, tail_a, tail_b);
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
        "cc не собрал харнесс флагами гейта цели (прежде здесь было \
         'use of undeclared identifier'):\n{}",
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

/// Цель `c` собирает узел из тех листьев и в том порядке.
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
        "узел собирается из листьев — порядок и состав значимы"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

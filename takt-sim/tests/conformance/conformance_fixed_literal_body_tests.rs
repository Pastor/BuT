//! Сверка понижения q-литерала в телах и условиях.
//!
//! ## Что доказывает
//!
//! Дробный литерал понижался в представление `q(m, n)` только в **объявлении**.
//!
//! | Запись | эталон | цель `c` |
//! |---|---|---|
//! | `gain := 2.0;` | `2.0` | `model->gain = 2.0;` -> **2**, а не 512 |
//! | `whole(3.0)` при параметре `q(8, 8)` | `3.0` | `Qpos_whole(model, 3.0)` -> **3** |
//! | `ref Done: gain > 1.0;` при `gain = 0.5` | ребро не срабатывает | `128 > 1.0` -> **срабатывает** |
//!
//! Последняя строка - **другой автомат** при нулевом коде возврата `taktc`; у целей
//! `st` и `sv` тот же вход не транслировался вовсе.

use std::path::Path;
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_fixed_literal_body.takt";
const TICKS: usize = 3;
const OBSERVED: &[&str] = &["set_v", "arg_v", "loc_v", "guard_v"];

fn sim_value(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(v)) => v,
        other => panic!("порт '{name}': неожиданное значение {other:?}"),
    }
}

fn simulator_trace() -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция не должна падать: {result:?}"
        );
        trace.push(OBSERVED.iter().map(|name| sim_value(&unit, name)).collect());
    }
    trace
}

fn generated_c_trace(dir: &Path) -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_fixed_literal_body",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let prints = (0..OBSERVED.len())
        .map(|i| format!(r#"        printf("%d ", (int)seen[{i}]);"#))
        .collect::<Vec<_>>()
        .join("\n");
    let harness = format!(
        r#"#include <stdio.h>
#include "conformance_fixed_literal_body.h"

static int64_t seen[4];

static void write_numeric(ConformanceFixedLiteralBody_Out_NumericPort port, uint8_t index, int64_t value,
                          void *userdata) {{
    (void)index;
    (void)userdata;
    if (port == CONFORMANCE_FIXED_LITERAL_BODY_PORT_SET_V) {{ seen[0] = value; }}
    if (port == CONFORMANCE_FIXED_LITERAL_BODY_PORT_ARG_V) {{ seen[1] = value; }}
    if (port == CONFORMANCE_FIXED_LITERAL_BODY_PORT_LOC_V) {{ seen[2] = value; }}
    if (port == CONFORMANCE_FIXED_LITERAL_BODY_PORT_GUARD_V) {{ seen[3] = value; }}
}}

int main(void) {{
    ConformanceFixedLiteralBody model;
    ConformanceFixedLiteralBody_init(&model);
    model.write_numeric = write_numeric;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceFixedLiteralBody_tick(&model);
{prints}
        printf("\n");
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), harness).expect("драйвер");
    let bin = dir.join("fixed_literal_bin");
    let build = Command::new("cc")
        .args(["-std=c11", "-w", "-I"])
        .arg(dir)
        .arg(dir.join("harness.c"))
        .arg(dir.join("conformance_fixed_literal_body.c"))
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        build.status.success(),
        "порождённый C не собирается:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера");
    assert!(run.status.success(), "драйвер завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            l.split_whitespace()
                .map(|v| v.parse().expect("число в трассе"))
                .collect()
        })
        .collect()
}

/// Четыре позиции приёмника: значения эталона и цели `c` совпадают.
#[test]
fn fixed_literal_in_body_matches_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![vec![2, 3, 4, 1]; TICKS],
        "эталон: 2.5 → 2, аргумент 3.0 → 3, локальная 4.5 → 4, охрана сработала: {sim:?}"
    );

    let cc = Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !cc {
        eprintln!("[ПРОПУСК] fixed_literal_in_body_matches_generated_c: нет cc");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_fixed_literal_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let c = generated_c_trace(&dir);
    assert_eq!(sim, c, "трассы разошлись\nsim={sim:?}\nC={c:?}");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Условие ребра понижается так же: иначе автомат уходит по другому пути.
///
/// Ребро хранит сырой АСД (инвариант проекта), и цели печатают его, разрешая имена
/// против модели, - понижение обязано идти туда же.
#[test]
fn edge_condition_literal_is_lowered() {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_fixed_literal_edge_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_fixed_literal_body",
        &source,
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let c = std::fs::read_to_string(dir.join("conformance_fixed_literal_body.c"))
        .expect("порождённый .c");
    // 100.0 в q(8, 8) - это 25600; сырой литерал дал бы `> 100.0`.
    assert!(
        c.contains("> 25600"),
        "литерал условия ребра обязан быть понижен в представление:\n{c}"
    );
    assert!(
        !c.contains("100.0"),
        "сырого дробного литерала в выводе быть не должно:\n{c}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

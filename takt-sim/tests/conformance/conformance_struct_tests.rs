//! Сверка структур симулятора с эталоном C.
//!
//! Область сверки - как у `conformance_c_tests.rs` - сужена до
//! полей `Integer` (для `[T;N]`/`bit`/`float` эталон C дефектен).
//! Фикстура **без инициализатора структуры**: `var p: Point := {1,2}` порождает
//! `model->p = {1, 2};` - невалидный C11 (brace-инициализатор в присваивании);
//! поля заполняются в теле `always`. Обёрнута в `model` + `start Entry = Conf;`
//! (для одиночной модели C не печатает `typedef`). Сверяются
//! **финальные** значения, а не потактовая трасса (INIT-такты смещают трассы).

use std::path::Path;
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/struct_conformance.takt";
const MAX_TICKS: usize = 10;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn simulate() -> Unit {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("юнит");
    for _ in 0..MAX_TICKS {
        if !matches!(unit.tick(), TickResult::Processing) {
            break;
        }
    }
    unit
}

/// Поле структурной переменной `var.field` симулятора как целое.
fn sim_field(unit: &Unit, var: &str, field: &str) -> i128 {
    match unit.variable(var) {
        Some(Value::Struct { fields, .. }) => match fields.iter().find(|(f, _)| f == field) {
            Some((_, Value::Number(n))) => *n,
            other => panic!("{var}.{field}: ожидалось целое, получено {other:?}"),
        },
        other => panic!("{var}: ожидалась структура, получено {other:?}"),
    }
}

fn sim_scalar(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(n)) => n,
        other => panic!("{name}: ожидалось целое, получено {other:?}"),
    }
}

/// Порождает C, компилирует `cc`, запускает и возвращает финальные значения `p.x`,
/// `p.y`, `t` из эталона.
fn run_generated_c(dir: &Path) -> Vec<(String, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    takt_lang::compile_to_c(
        "struct_conformance",
        &source,
        dir.to_str().expect("путь UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    // Путь к полям: Conf вложена как поле `entry` корня StructConformance.
    let harness = format!(
        r#"#include <stdio.h>
#include "struct_conformance.h"

int main(void) {{
    StructConformance m;
    StructConformance_init(&m);
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        StructConformance_tick(&m);
        if (StructConformance_is_done(&m)) break;
    }}
    printf("p.x=%d\n", (int)m.entry.p.x);
    printf("p.y=%d\n", (int)m.entry.p.y);
    printf("t=%d\n", (int)m.entry.t);
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, harness).expect("харнесс");

    let bin = dir.join("bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-I"])
        .arg(dir)
        .arg(dir.join("struct_conformance.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&bin).output().expect("запуск C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let (name, value) = line.split_once('=')?;
            Some((name.to_string(), value.trim().parse().ok()?))
        })
        .collect()
}

/// Финальные значения полей структуры совпадают у симулятора и порождённого
/// C. Значения в диапазоне полей (`x: u8 = 7`, `y: u16 = 300`).
#[test]
fn struct_fields_match_generated_c() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] struct_fields_match_generated_c: `cc` не найден");
        return;
    }
    let unit = simulate();
    let sim = [
        ("p.x", sim_field(&unit, "p", "x")),
        ("p.y", sim_field(&unit, "p", "y")),
        ("t", sim_scalar(&unit, "t")),
    ];

    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_conformance_struct");
    std::fs::create_dir_all(&dir).expect("каталог");
    let c: std::collections::HashMap<String, i128> = run_generated_c(&dir).into_iter().collect();

    for (name, sim_val) in sim {
        let c_val = c
            .get(name)
            .unwrap_or_else(|| panic!("C не напечатал '{name}'"));
        assert_eq!(sim_val, *c_val, "{name}: симулятор {sim_val} ≠ C {c_val}");
    }
}

/// Ожидаемые значения закреплены (защита от "оба неверны одинаково"): значения - из
/// пробы порождённого C, а не догадок.
#[test]
fn struct_expected_values_are_pinned() {
    let unit = simulate();
    assert_eq!(sim_field(&unit, "p", "x"), 7);
    assert_eq!(sim_field(&unit, "p", "y"), 300);
    assert_eq!(sim_scalar(&unit, "t"), 1);
}

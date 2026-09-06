//! Сверка FIXED-POINT В параметре функции.
//!
//! ## Что доказывает
//!
//! `fn whole_part(v: q(8, 8)) -> u8` давала **`SE-119`** - "внутренний
//! инвариант семантики нарушен: параметр функции без типа". Это отказ,
//! объявляющий **дефект компилятора**, на записи, которая языку принадлежит:
//! тип параметра грамматика хранит выражением, и `q(8, 8)` приходит туда
//! **вызовом** (`q` - обычный идентификатор, решение 0061).
//!
//! Вывода не получал **никто**, включая эталон, - то есть класс виден и по коду
//! возврата. Контроль: `q` в **возврате** (`-> q(8, 8)`) работал и прежде.

use std::path::Path;
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_fixed_param.takt";
const TICKS: usize = 3;
const OBSERVED: &[&str] = &["whole", "doubled"];

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

/// Трасса порождённого C: драйвер печатает наблюдаемые каждый такт.
fn generated_c_trace(dir: &Path) -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_fixed_param",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let prints = OBSERVED
        .iter()
        .enumerate()
        .map(|(i, _)| format!(r#"        printf("%d ", (int)seen[{i}]);"#))
        .collect::<Vec<_>>()
        .join("\n");
    let harness = format!(
        r#"#include <stdio.h>
#include "conformance_fixed_param.h"

static int64_t seen[2];

static void write_numeric(ConformanceFixedParam_Out_NumericPort port, uint8_t index, int64_t value,
                          void *userdata) {{
    (void)index;
    (void)userdata;
    if (port == CONFORMANCE_FIXED_PARAM_PORT_WHOLE) {{ seen[0] = value; }}
    if (port == CONFORMANCE_FIXED_PARAM_PORT_DOUBLED) {{ seen[1] = value; }}
}}

int main(void) {{
    ConformanceFixedParam model;
    ConformanceFixedParam_init(&model);
    model.write_numeric = write_numeric;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceFixedParam_tick(&model);
{prints}
        printf("\n");
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), harness).expect("драйвер");
    let bin = dir.join("conformance_fixed_param_bin");
    let build = Command::new("cc")
        .args(["-std=c11", "-w", "-I"])
        .arg(dir)
        .arg(dir.join("harness.c"))
        .arg(dir.join("conformance_fixed_param.c"))
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

/// Параметр `q(m, n)` принимается, и значения совпадают с целью `c`.
#[test]
fn fixed_parameter_values_match_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![vec![1, 1], vec![1, 2], vec![2, 3]],
        "эталон: `whole` — до присваивания, `doubled` — после: {sim:?}"
    );

    let cc = Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !cc {
        eprintln!("[ПРОПУСК] fixed_parameter_values_match_generated_c: нет cc");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_fixed_param_{}",
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

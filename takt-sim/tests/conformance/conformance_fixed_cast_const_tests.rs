//! Сверка приведения имени константы к `q(m, n)`.
//!
//! ## Что доказывает
//!
//! научила цели считать `2.5 as q(8, 8)` при компиляции, но признак требовал
//! **литерала**. Имя константы под него не подпадало, и замер 2026-08-22 на `gain := K
//! as q(8, 8);` при `const K: float := 2.5;` дал:
//!
//! | Потребитель | Ответ |
//! |---|---|
//! | эталон, `c`, `c-hal` | исполняют (`set_v = 2`) |
//! | `st`, `st-at` | `ST-014` "литеральный float понижается на этапе компиляции" |
//! | `rust` | `RS-011` с тем же обещанием |
//! | `sv`, `sv-mmio` | `SV-003` - но **на объявлении** константы: вещественного типа в RTL нет |
//!
//! ## Второй слой
//!
//! Свернув приведение, цели перестают печатать **имя** константы - и вывод ломается
//! иначе: `rust` под `-D warnings` отвечает "constant is never used", а `sv`
//! по-прежнему падает на объявлении `float`. Поэтому константа, все обращения к которой
//! свёрнуты, в вывод не эмитится вовсе (правка сборщика
//! `semantic::unused::usage_from_expr`).
//!
//! Фикстура держит **вторую** константу (`BUMP`), используемую и в приведении, и
//! обычным путём: она обязана остаться в выводе - иначе правка читалась бы как
//! "константы под приведением выбрасываются всегда".
//!
//! Наблюдаемые `as u8` грубы, поэтому фикстура сверяет ещё и пути: `eq_v` сравнивает
//! значение из тела со значением того же литерала из объявления.

use std::path::Path;
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_fixed_cast_const.takt";
const TICKS: usize = 3;
const OBSERVED: &[&str] = &["set_v", "eq_v", "mix_v"];

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

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_fixed_cast_const_{tag}_{}",
            std::thread::current()
                .name()
                .unwrap_or("single")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn generate_c(dir: &Path) -> String {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_fixed_cast_const",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    std::fs::read_to_string(dir.join("conformance_fixed_cast_const.c")).expect("порождённый .c")
}

fn generated_c_trace(dir: &Path) -> Vec<Vec<i128>> {
    generate_c(dir);
    let prints = (0..OBSERVED.len())
        .map(|i| format!(r#"        printf("%d ", (int)seen[{i}]);"#))
        .collect::<Vec<_>>()
        .join("\n");
    let harness = format!(
        r#"#include <stdio.h>
#include "conformance_fixed_cast_const.h"

static int64_t seen[3];

static void write_numeric(ConformanceFixedCastConst_Out_NumericPort port, uint8_t index, int64_t value,
                          void *userdata) {{
    (void)index;
    (void)userdata;
    if (port == CONFORMANCE_FIXED_CAST_CONST_PORT_SET_V) {{ seen[0] = value; }}
    if (port == CONFORMANCE_FIXED_CAST_CONST_PORT_EQ_V) {{ seen[1] = value; }}
    if (port == CONFORMANCE_FIXED_CAST_CONST_PORT_MIX_V) {{ seen[2] = value; }}
}}

int main(void) {{
    ConformanceFixedCastConst model;
    ConformanceFixedCastConst_init(&model);
    model.write_numeric = write_numeric;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceFixedCastConst_tick(&model);
{prints}
        printf("\n");
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), harness).expect("драйвер");
    let bin = dir.join("fixed_cast_const_bin");
    let build = Command::new("cc")
        .args(["-std=c11", "-w", "-I"])
        .arg(dir)
        .arg(dir.join("harness.c"))
        .arg(dir.join("conformance_fixed_cast_const.c"))
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

/// Приведение константы: значения эталона и цели `c` совпадают.
#[test]
fn fixed_cast_const_matches_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![vec![2, 1, 4], vec![2, 1, 5], vec![2, 1, 6]],
        "эталон: K=2.5 → 2, пути тела и объявления дают одно представление, \
         вторая константа считается обычным путём: {sim:?}"
    );

    let cc = Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !cc {
        eprintln!("[ПРОПУСК] fixed_cast_const_matches_generated_c: нет cc");
        return;
    }
    let dir = temp_dir("trace");
    let c = generated_c_trace(&dir);
    assert_eq!(sim, c, "трассы разошлись\nsim={sim:?}\nC={c:?}");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Свёрнутая константа не печатается, а используемая - печатается.
///
/// Обе стороны обязательны: без первой правка не нужна (вывод не собирается у `rust` и
/// `sv`), без второй она читалась бы как "выбрасываем константы под приведением
/// всегда".
#[test]
fn folded_const_is_not_emitted_but_used_one_is() {
    let dir = temp_dir("text");
    let c = generate_c(&dir);
    assert!(
        !c.contains("floor("),
        "приведение константы обязано быть посчитано при компиляции:\n{c}"
    );
    assert!(
        c.contains("640"),
        "в выводе нет представления 2.5 в q(8, 8):\n{c}"
    );
    assert!(
        !c.contains("_K"),
        "имя свёрнутой константы в выводе не нужно — оно ломает сборку у rust и sv:\n{c}"
    );
    assert!(
        c.contains("BUMP"),
        "константа, используемая обычным путём, обязана остаться:\n{c}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

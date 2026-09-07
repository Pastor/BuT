//! Сверка приведения литерала к `q(m, n)` в теле.
//!
//! ## Что доказывает
//!
//! Значение `2.5 as q(8, 8)` известно при компиляции (`const_eval::fixed_repr`),
//! но носитель звался только из свёртки инициализаторов.
//!
//! | Потребитель | `gain := 2.5 as q(8, 8);` |
//! |---|---|
//! | эталон | исполняет |
//! | `c`, `c-hal` | `(int16_t)floor((2.5) * 256.0)` - вызов libm **в времени выполнения ради константы** |
//! | `st`, `st-at` | `ST-011` "тип источника не выводится статически" |
//! | `rust` | `RS-011` "нужен floor, которого нет в no_std без libm" |
//! | `sv`, `sv-mmio` | `SV-002` "вещественный литерал" |

use std::path::Path;
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_fixed_cast_literal.takt";
const TICKS: usize = 4;
const OBSERVED: &[&str] = &["set_v", "mul_v", "neg_v", "eq_v"];

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
            "takt_fixed_cast_lit_{tag}_{}",
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
        "conformance_fixed_cast_literal",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    std::fs::read_to_string(dir.join("conformance_fixed_cast_literal.c")).expect("порождённый .c")
}

fn generated_c_trace(dir: &Path) -> Vec<Vec<i128>> {
    generate_c(dir);
    let prints = (0..OBSERVED.len())
        .map(|i| format!(r#"        printf("%d ", (int)seen[{i}]);"#))
        .collect::<Vec<_>>()
        .join("\n");
    let harness = format!(
        r#"#include <stdio.h>
#include "conformance_fixed_cast_literal.h"

static int64_t seen[4];

static void write_numeric(ConformanceFixedCastLiteral_Out_NumericPort port, uint8_t index, int64_t value,
                          void *userdata) {{
    (void)index;
    (void)userdata;
    if (port == CONFORMANCE_FIXED_CAST_LITERAL_PORT_SET_V) {{ seen[0] = value; }}
    if (port == CONFORMANCE_FIXED_CAST_LITERAL_PORT_MUL_V) {{ seen[1] = value; }}
    if (port == CONFORMANCE_FIXED_CAST_LITERAL_PORT_NEG_V) {{ seen[2] = value; }}
    if (port == CONFORMANCE_FIXED_CAST_LITERAL_PORT_EQ_V) {{ seen[3] = value; }}
}}

int main(void) {{
    ConformanceFixedCastLiteral model;
    ConformanceFixedCastLiteral_init(&model);
    model.write_numeric = write_numeric;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceFixedCastLiteral_tick(&model);
{prints}
        printf("\n");
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), harness).expect("драйвер");
    let bin = dir.join("fixed_cast_lit_bin");
    let build = Command::new("cc")
        .args(["-std=c11", "-w", "-I"])
        .arg(dir)
        .arg(dir.join("harness.c"))
        .arg(dir.join("conformance_fixed_cast_literal.c"))
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

/// Три формы приведения: значения эталона и цели `c` совпадают.
#[test]
fn fixed_cast_literal_matches_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![
            vec![2, 2, 0, 1],
            vec![2, 4, 2, 1],
            vec![2, 8, 6, 1],
            vec![2, 16, 14, 1]
        ],
        "эталон: 2.5 → 2, work удваивается, отрицательный литерал вычитает 1.5, \
         пути тела и объявления дают одно представление: {sim:?}"
    );

    let cc = Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !cc {
        eprintln!("[ПРОПУСК] fixed_cast_literal_matches_generated_c: нет cc");
        return;
    }
    let dir = temp_dir("trace");
    let c = generated_c_trace(&dir);
    assert_eq!(sim, c, "трассы разошлись\nsim={sim:?}\nC={c:?}");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Вызова `floor()` в порождённом C больше нет: значение посчитано компилятором.
///
/// Это не косметика: `floor` тянет libm, а прошивка МК собирается без неё.
#[test]
fn literal_cast_needs_no_runtime_floor() {
    let dir = temp_dir("text");
    let c = code_only(&generate_c(&dir));
    assert!(
        !c.contains("floor("),
        "приведение литерала обязано быть посчитано при компиляции:\n{c}"
    );
    // 2.5 · 256 = 640, 2.0 · 256 = 512, floor(−1.5 · 256) = −384.
    for expected in ["640", "512", "-384"] {
        assert!(
            c.contains(expected),
            "в выводе нет представления '{expected}':\n{c}"
        );
    }
    let _ = std::fs::remove_dir_all(&dir);
}

/// Текст вывода без строк-комментариев.
///
/// Нужен с: комментарии автора модели переносятся в вывод как есть, и фикстура,
/// объясняющая словами "`floor(...)` компилируется и считает верно", приносит эти слова
/// в порождённый C. Проверка на вызов обязана смотреть на код - иначе она красна из-за
/// собственного пояснения (тот же класс, что ловля предмета по тексту: тест обязан
/// проверять обещание).
fn code_only(text: &str) -> String {
    text.lines()
        .filter(|l| !l.trim_start().starts_with("//"))
        .collect::<Vec<_>>()
        .join("\n")
}

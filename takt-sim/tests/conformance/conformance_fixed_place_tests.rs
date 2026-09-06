//! Сверка понижения q-литерала, когда приёмник - Поле или элемент.
//!
//! ## Что доказывает
//!
//! понижала литерал там, где приёмник назван **именем**. Поле структуры и элемент
//! массива остались вне правила, и замер 2026-08-22 дал расхождение у всех потребителей
//! сразу:
//!
//! | Запись | эталон | цель `c` | `st`, `rust`, `sv` |
//! |---|---|---|---|
//! | `g.kp := 2.0;` | `set_v = 2` | `model->g.kp = 2.0;` -> **0** | вывод отвергают их инструменты |
//! | `gains[0] := 3.0;` | верно | то же | то же |
//! | `if g.ki > 1.0` | `SIM-005` **в такте** | считает по представлению | то же |
//! | `ref Done: g.ki > 4.0;` | `SIM-005` **в такте** | **другой автомат** | то же |

use std::path::Path;
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_fixed_place.takt";
const TICKS: usize = 3;
const OBSERVED: &[&str] = &["set_v", "arr_v", "cmp_v", "edge_v"];

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
    // Каталог уникален по тесту; двоеточие имени потока вычищается - после слияния
    // целей оно есть в каждом имени.
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_fixed_place_{tag}_{}",
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
        "conformance_fixed_place",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    std::fs::read_to_string(dir.join("conformance_fixed_place.c")).expect("порождённый .c")
}

fn generated_c_trace(dir: &Path) -> Vec<Vec<i128>> {
    generate_c(dir);
    let prints = (0..OBSERVED.len())
        .map(|i| format!(r#"        printf("%d ", (int)seen[{i}]);"#))
        .collect::<Vec<_>>()
        .join("\n");
    let harness = format!(
        r#"#include <stdio.h>
#include "conformance_fixed_place.h"

static int64_t seen[4];

static void write_numeric(ConformanceFixedPlace_Out_NumericPort port, uint8_t index, int64_t value,
                          void *userdata) {{
    (void)index;
    (void)userdata;
    if (port == CONFORMANCE_FIXED_PLACE_PORT_SET_V) {{ seen[0] = value; }}
    if (port == CONFORMANCE_FIXED_PLACE_PORT_ARR_V) {{ seen[1] = value; }}
    if (port == CONFORMANCE_FIXED_PLACE_PORT_CMP_V) {{ seen[2] = value; }}
    if (port == CONFORMANCE_FIXED_PLACE_PORT_EDGE_V) {{ seen[3] = value; }}
}}

int main(void) {{
    ConformanceFixedPlace model;
    ConformanceFixedPlace_init(&model);
    model.write_numeric = write_numeric;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceFixedPlace_tick(&model);
{prints}
        printf("\n");
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), harness).expect("драйвер");
    let bin = dir.join("fixed_place_bin");
    let build = Command::new("cc")
        .args(["-std=c11", "-w", "-I"])
        .arg(dir)
        .arg(dir.join("harness.c"))
        .arg(dir.join("conformance_fixed_place.c"))
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

/// Поле, элемент, сравнение и ребро: эталон и цель `c` считают одно.
#[test]
fn fixed_literal_in_place_matches_generated_c() {
    let sim = simulator_trace();
    assert_eq!(
        sim,
        vec![vec![2, 3, 0, 0]; TICKS],
        "эталон: поле 2.0 → 2, элемент 3.0 → 3, сравнение ложно, ребро не сработало: {sim:?}"
    );

    let cc = Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !cc {
        eprintln!("[ПРОПУСК] fixed_literal_in_place_matches_generated_c: нет cc");
        return;
    }
    let dir = temp_dir("trace");
    let c = generated_c_trace(&dir);
    assert_eq!(sim, c, "трассы разошлись\nsim={sim:?}\nC={c:?}");
    let _ = std::fs::remove_dir_all(&dir);
}

/// Литерал понижён во всех четырёх позициях - проверяется по тексту вывода.
///
/// Позиции разные по устройству: присваивание полю и элементу идут через печатник
/// выражений, сравнение в теле - через него же, а ребро хранит сырой АСД (инвариант
/// проекта) и печатается отдельным путём. Текстовая проверка отделяет "понизили в одном
/// месте" от "понизили везде".
#[test]
fn every_place_lowers_its_literal() {
    let dir = temp_dir("text");
    let c = code_only(&generate_c(&dir));
    // 2.0 -> 512, 3.0 -> 768, 1.0 -> 256, 4.0 -> 1024 в q(8, 8).
    for expected in ["= 512", "= 768", "> 256", "> 1024"] {
        assert!(
            c.contains(expected),
            "в выводе нет понижённого литерала '{expected}':\n{c}"
        );
    }
    for raw in ["2.0", "3.0", "1.0", "4.0"] {
        assert!(
            !c.contains(raw),
            "сырого дробного литерала '{raw}' в выводе быть не должно:\n{c}"
        );
    }
    let _ = std::fs::remove_dir_all(&dir);
}

/// Текст вывода без строк-комментариев.
///
/// Нужен с: комментарии автора модели переносятся в вывод как есть, и исправлениетура,
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

//! Потактовая сверка применения аргументов инстанцирования -, (критерии A6 "локальная
//! копия" и A13 анализа).
//!
//! # Что доказывается
//!
//! 1. **Два экземпляра с разными значениями ведут себя по-разному** - и
//!    одинаково в симуляторе и в порождённом C: `Tuner(gain:= 100) |
//!    Tuner(gain:= 200)` даёт накопители 100·t и 200·t.
//! 2. **"Локальная копия"**: экземпляр меняет свой параметр в такте - сосед и
//!    объявление этого не видят.
//!
//! Сверка идёт **значениями**, а не фактом компиляции: молча неверная трансляция
//! компилируется тоже (правило CLAUDE.md о сверке целей).

use std::path::Path;
use std::process::Command;
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, build_unit};

/// Два экземпляра одной модели с разными настройками.
///
/// `gain`, а не `step`: `step` занят стандартной библиотекой IEC (`ST-014`), а исправлениетура
/// делит текст с тестами целей. Самопереход `ref Count;` обязателен: состояние без
/// исходящих `ref` цель `c` завершает после первого такта (контракт Ф8), и трасса
/// застыла бы - сверялось бы одно значение, а не накопление с обёрткой.
const SRC: &str = "model Tuner {\n\
                   \x20   parameter gain: u8 := 1;\n\
                   \x20   var acc: u8 := 0;\n\
                   \x20   start Count {\n\
                   \x20       always { acc := acc + gain; }\n\
                   \x20       ref Count;\n\
                   \x20   }\n\
                   }\n\
                   \n\
                   start Main = Tuner(gain := 100) | Tuner(gain := 200);\n";

/// Тактов сверки. Настройки 100 и 200 при `u8` дают обёртку уже на третьем такте (300
/// mod 256 = 44) - переполнение попадает в сверку намеренно.
const TICKS: usize = 4;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Юнит из исходника.
fn unit_of(src: &str) -> Unit {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    build_unit(model).expect("построение юнита")
}

/// Значения переменной `name` у всех экземпляров-детей, в порядке объявления.
///
/// Квалифицированная адресация (`Модель::имя`) здесь не годится: квалификатор - имя
/// модели, а оба экземпляра - `Tuner`. Снимок (`state_io`) различает экземпляры
/// структурно.
fn variables_named(unit: &Unit, name: &str) -> Vec<i128> {
    fn walk(snap: &takt_sim::state_io::UnitSnapshot, name: &str, out: &mut Vec<i128>) {
        match snap {
            takt_sim::state_io::UnitSnapshot::None => {}
            takt_sim::state_io::UnitSnapshot::Node { variables, .. } => {
                if let Some(value) = variables
                    .get(name)
                    .and_then(|v| v.as_number())
                    .and_then(serde_json::Number::as_i128)
                {
                    out.push(value);
                }
            }
            takt_sim::state_io::UnitSnapshot::Parallel { children }
            | takt_sim::state_io::UnitSnapshot::Sequential { children, .. } => {
                for child in children {
                    walk(child, name, out);
                }
            }
        }
    }
    let mut out = Vec::new();
    walk(&takt_sim::state_io::snapshot(unit), name, &mut out);
    out
}

// --- Симулятор: разные значения и "локальная копия" --------------------------

/// Два экземпляра накапливают с разной скоростью; обёртка `u8` - по 0127.
#[test]
fn two_instances_accumulate_at_different_rates() {
    let mut unit = unit_of(SRC);
    let mut expected_fast: i128 = 0;
    let mut expected_slow: i128 = 0;
    for tick in 1..=TICKS {
        assert!(!matches!(unit.tick(), TickResult::Failed(_)));
        expected_fast = (expected_fast + 100) % 256;
        expected_slow = (expected_slow + 200) % 256;
        // Порядок ветвей `|` - порядок объявления: первый экземпляр - gain=100.
        let values = variables_named(&unit, "acc");
        assert_eq!(
            values,
            vec![expected_fast, expected_slow],
            "такт {tick}: накопители обоих экземпляров"
        );
    }
}

/// "Локальная копия" (A6): экземпляр меняет свой параметр - сосед не видит.
///
/// У состояния есть самопереход (`ref Count;`), и это не украшение: без него состояние
/// терминально, автомат завершается на первом же такте, и накопление прекращается - так
/// делает прошивка цели `c` (замер 2026-08-23: `20 20 20`).
#[test]
fn changing_a_parameter_in_one_instance_is_invisible_to_the_other() {
    let src = "model Tuner {\n\
               \x20   parameter gain: u8 := 1;\n\
               \x20   var acc: u8 := 0;\n\
               \x20   var first: u8 := 1;\n\
               \x20   start Count {\n\
               \x20       always {\n\
               \x20           acc := acc + gain;\n\
               \x20           if first = 1 { gain := gain + 1; first := 0; }\n\
               \x20       }\n\
               \x20       ref Count;\n\
               \x20   }\n\
               }\n\
               \n\
               start Main = Tuner(gain := 10) | Tuner(gain := 20);\n";
    let mut unit = unit_of(src);
    // Такт 1: acc = 10 и 20; каждый экземпляр поднимает свой gain на 1.
    assert!(!matches!(unit.tick(), TickResult::Failed(_)));
    assert_eq!(variables_named(&unit, "acc"), vec![10, 20]);
    assert_eq!(variables_named(&unit, "gain"), vec![11, 21]);
    // Такт 2: приращения независимы - 10+11 и 20+21.
    assert!(!matches!(unit.tick(), TickResult::Failed(_)));
    assert_eq!(
        variables_named(&unit, "acc"),
        vec![21, 41],
        "изменение параметра в одном экземпляре не должно влиять на другой"
    );
}

// --- Потактовая сверка с целью `c` (A13) -------------------------------------

/// Трасса симулятора и порождённого C совпадают на каждом такте.
#[test]
fn per_tick_trace_matches_generated_c() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] per_tick_trace_matches_generated_c: компилятор `cc` не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt-0185-04-conformance");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");

    // Трасса симулятора: оба накопителя на каждом такте.
    let mut unit = unit_of(SRC);
    let mut sim_trace: Vec<(i128, i128)> = Vec::new();
    for _ in 0..TICKS {
        assert!(!matches!(unit.tick(), TickResult::Failed(_)));
        let values = variables_named(&unit, "acc");
        sim_trace.push((values[0], values[1]));
    }

    // Трасса порождённого C - теми же тактами.
    let c_trace = run_generated_c(&dir);
    assert_eq!(
        sim_trace, c_trace,
        "симулятор и цель c разошлись на трассе накопителей"
    );
}

/// Порождает C, собирает с харнессом и возвращает потактовую трассу обоих накопителей.
fn run_generated_c(dir: &Path) -> Vec<(i128, i128)> {
    takt_lang::compile_to_c(
        "param_conf",
        SRC,
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "param_conf.h"

int main(void) {{
    ParamConf m;
    ParamConf_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        ParamConf_tick(&m);
        printf("%d %d\n", (int)m.main.tuner0.acc, (int)m.main.tuner1.acc);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, &harness).expect("запись харнесса");

    let bin = dir.join("param_conf_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-o"])
        .arg(&bin)
        .arg(&harness_path)
        .arg(dir.join("param_conf.c"))
        .arg("-I")
        .arg(dir)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "cc не собрал харнесс:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&bin).output().expect("запуск харнесса");
    assert!(run.status.success(), "харнесс завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|line| {
            let mut parts = line.split_whitespace().map(|p| p.parse::<i128>().unwrap());
            (parts.next().unwrap(), parts.next().unwrap())
        })
        .collect()
}

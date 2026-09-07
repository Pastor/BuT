//! Проверка состояния под-модели: эталон == порождённый C -.
//!
//! # Что наблюдается
//!
//! `Feeder` считает такты и уходит `Idle -> Done` на третьем; `Watcher` следит
//! за ним и на том же такте пишет `seen := 1`. Наблюдаемая - `seen`, то есть
//! **момент**, когда наблюдатель увидел чужое состояние.
//!
//! # Мягкая деградация
//!
//! Нет `cc` - половина с прошивкой пропускается с сообщением (образец -
//! `conformance_c_unconditional_edge_tests`); трасса эталона проверяется всегда.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Тактов в трассе.
const TICKS: usize = 5;

/// Имя порождаемой единицы (оно же - имя корневой модели: `StateOf`).
const UNIT: &str = "state_of";

/// Полная форма записи: `S(Feeder) = Done`.
const SRC_FULL: &str = "model Feeder {\n\
                        \x20   var n: u8 := 0;\n\
                        \x20   start Idle { always { n := n + 1; } ref Done: n >= 3; }\n\
                        \x20   state Done { }\n\
                        }\n\
                        model Watcher {\n\
                        \x20   var seen: u8 := 0;\n\
                        \x20   start Wait { ref Report: S(Feeder) = Done; }\n\
                        \x20   state Report { enter { seen := 1; } }\n\
                        }\n\
                        start Main = Feeder | Watcher;\n";

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Каталог сборки, уникальный по имени потока (тесты идут параллельно, 0190; двоеточие
/// имени модуля вычищается - ).
fn build_dir() -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0245_conformance_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

/// Потактовая трасса переменной `seen` у эталона.
fn simulator_trace(src: &str) -> Vec<i128> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        if let takt_sim::TickResult::Failed(why) = unit.tick() {
            panic!("эталон остановился: {why}");
        }
        match unit.variable("seen") {
            Some(takt_sim::Value::Number(v)) => trace.push(v),
            other => panic!("переменная 'seen' обязана быть числом, получено {other:?}"),
        }
    }
    trace
}

/// Потактовая трасса переменной `seen` у порождённой прошивки.
fn generated_c_trace(dir: &Path, src: &str) -> Vec<i128> {
    takt_lang::compile_to_c(
        UNIT,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[] as &[String],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

int main(void) {{
    StateOf m = {{0}};
    StateOf_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        StateOf_tick(&m);
        printf("%ld\n", (long)m.main.watcher1.seen);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_state_of.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("state_of_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join(format!("{UNIT}.c")))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|l| l.trim().parse::<i128>().expect("число в строке вывода"))
        .collect()
}

/// **A3: наблюдатель видит чужое состояние на том же такте, что и C.**
#[test]
fn state_of_model_trace_matches_generated_c() {
    let sim = simulator_trace(SRC_FULL);
    assert_eq!(
        sim,
        vec![0, 0, 1, 1, 1],
        "эталон: Feeder уходит в Done на третьем такте, и наблюдатель видит это \
         тогда же — до фичи 0245 прогон вовсе останавливался SIM-013"
    );

    if !cc_available() {
        eprintln!("[ПРОПУСК] state_of_model_trace_matches_generated_c: `cc` не найден");
        return;
    }
    let dir = build_dir();
    let c = generated_c_trace(&dir, SRC_FULL);
    assert_eq!(
        sim, c,
        "потактовые трассы эталона и порождённого C обязаны совпадать:\nsim={sim:?}\nC={c:?}"
    );
}

/// **A1: краткая форма записи равносильна полной.**
///
/// Обе формы разбирает одна функция `takt-lang` - тест проверяет, что симулятор
/// пользуется именно ею, а не собственным разбором полной формы.
#[test]
fn short_form_matches_full_form() {
    let short = SRC_FULL.replace("S(Feeder) = Done", "Feeder = Done");
    assert_ne!(short, SRC_FULL, "замена обязана состояться");
    assert_eq!(
        simulator_trace(&short),
        simulator_trace(SRC_FULL),
        "краткая форма `Модель = Состояние` обязана значить то же, что `S(Модель) = Состояние`"
    );
}

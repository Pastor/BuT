//! Время в состоянии при самопереходе: эталон == цель `c` -.
//!
//! #
//!
//! Правы **цели**: самопереход отсчёт не сбрасывает. `enter`/`exit` при этом
//! исполняются по-прежнему - "вход" для них и для отсчёта времени понимается
//! по-разному, и разница названа в документе.
//!
//! Исправлениетура наблюдает **оба** следствия: счётчик `every` и такт перехода по выдержке.
//! На одном из них правка могла бы "почти получиться".

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_self_transition_time.takt";
const UNIT: &str = "conformance_self_transition_time";
const TICKS: usize = 8;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0393_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Трасса эталона: `(ticks, done)` на каждом такте.
fn simulator_trace() -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for step in 0..TICKS {
        // Модельное время ведёт тест: `clock 1kHz` задаёт период такта, но сам эталон
        // часы не двигает - их даёт прогон (образец сверки ).
        unit.set_time_ns(i64::try_from(step).expect("номер такта") * 1_000_000);
        let _ = unit.tick();
        let number = |name: &str| match unit.variable(name) {
            Some(takt_sim::Value::Number(v)) => v,
            other => panic!("переменная '{name}' обязана быть числом, получено {other:?}"),
        };
        trace.push((number("ticks"), number("done")));
    }
    trace
}

/// Та же трасса у порождённого C.
fn generated_c_trace(dir: &Path) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    // Частота такта берётся из объявления `clock` фикстуры - цель `c` требует
    // совпадающего `--tick-hz` (контракт 0134).
    let mut options = takt_lang::generator::GenerateOptions::default();
    options.tick_hz = Some(1000);
    takt_lang::compile_to_c(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options,
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

int main(void) {{
    ConformanceSelfTransitionTime m;
    ConformanceSelfTransitionTime_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceSelfTransitionTime_tick(&m);
        printf("%d %d\n", (int)m.ticks, (int)m.done);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, &harness).expect("запись харнесса");

    let bin = dir.join("self_bin");
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
        .arg(&harness_path)
        .arg(dir.join(format!("{UNIT}.c")))
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

/// Самопереход не сбрасывает отсчёт - эталон и цель `c` совпадают потактово.
#[test]
fn self_transition_keeps_the_dwell() {
    // Ожидание считается независимо от обоих исполнителей: при 1 кГц `every 3ms`
    // срабатывает на 4-м и 7-м тактах, `after 5ms` уводит в `Done` на 6-м. Самопереход,
    // срабатывающий каждый такт, на это не влияет.
    let expected = vec![
        (0, 0),
        (0, 0),
        (0, 0),
        (1, 0),
        (1, 0),
        (1, 1),
        (1, 1),
        (1, 1),
    ];
    let sim = simulator_trace();
    assert_eq!(sim, expected, "эталон разошёлся с ожиданием");

    if !cc_available() {
        eprintln!("[ПРОПУСК] компилятор `cc` не найден; трасса эталона уже сверена");
        return;
    }
    let dir = build_dir("trace");
    assert_eq!(generated_c_trace(&dir), sim, "цель c разошлась с эталоном");
}

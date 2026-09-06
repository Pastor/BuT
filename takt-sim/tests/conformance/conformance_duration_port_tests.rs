//! Порт типа `duration`: цель `c` против эталона.
//!
//! ## Что доказывает набор
//!
//! Единица на границе - **миллисекунда**, а эталон меряет наносекундами: контракт
//! держит `semantic::duration::value_millis`. Ошибись пересчёт - вывод остался бы
//! валидным (`cc` принимает любое целое), а прошивка ждала бы не тот интервал. Поэтому
//! сверяется трасса значений, а не факт компиляции.

use std::path::Path;
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/conformance_duration_port.takt";
const UNIT: &str = "durationport";
const TICKS: usize = 3;
/// `(sig в миллисекундах, probe)`: `2s`, затем `500ms`.
const EXPECTED: [(i128, i128); TICKS] = [(2000, 1), (500, 2), (500, 3)];

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_0487_{tag}_{}",
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

/// Трасса эталона, приведённая к миллисекундам границы.
///
/// Эталон меряет **наносекундами** - это его каноническое представление (профиль
/// времени принадлежит генерации, а не модели). Пересчёт здесь - часть проверяемого
/// контракта, а не удобство теста.
fn simulator_trace() -> Vec<(i128, i128)> {
    let (ast, _) = takt_lang::parse(&source(), 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit: Unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        assert!(
            !matches!(unit.tick(), TickResult::Failed(_)),
            "прогон не обрывается"
        );
        let sig = match unit.variable("sig") {
            Some(Value::Duration(ns)) => i128::from(ns) / 1_000_000,
            other => panic!("порт 'sig': {other:?}"),
        };
        let probe = match unit.variable("probe") {
            Some(Value::Number(v)) => v,
            other => panic!("порт 'probe': {other:?}"),
        };
        trace.push((sig, probe));
    }
    trace
}

fn c_trace(dir: &Path) -> Vec<(i128, i128)> {
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
static long long sig, probe;
static void on_num(Durationport_Out_NumericPort p, uint8_t index, int64_t v, void *u) {{
    (void)index;
    (void)u;
    if (p == DURATIONPORT_DURATION_PORT_PORT_SIG) sig = (long long)v; else probe = (long long)v;
}}
int main(void) {{
    Durationport m;
    Durationport_init(&m);
    m.write_numeric = on_num;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        Durationport_tick(&m);
        printf("%lld %lld\n", sig, probe);
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
        "cc не собрал харнесс флагами гейта цели:\n{}",
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
            ))
        })
        .collect()
}

/// Эталон даёт ожидаемую трассу - она же ожидается от цели.
#[test]
fn simulator_trace_matches_expectation() {
    assert_eq!(simulator_trace(), EXPECTED.to_vec());
}

/// Цель `c` печатает порт длительности числом миллисекунд.
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
        "цель `c` обязана считать то же, что эталон (в миллисекундах границы)"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

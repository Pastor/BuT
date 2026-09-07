//! Потактовая сверка периодического блока `every` цели `c` с эталоном. Профиль "часы":
//! период меряется модельным временем `now_ms`, тестбенч ведёт его 1 мс/такт - как
//! эталон-симулятор. Наблюдение - выходной порт `led` (счётчик срабатываний), через
//! колбэк `write_numeric`.

use std::path::Path;
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_every.takt";
const TICKS: usize = 10;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Трасса эталона: значение `led` после каждого такта при 1 мс/такт.
fn simulate_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for step in 0..TICKS {
        unit.set_time_ns(i64::try_from(step).unwrap() * 1_000_000);
        let r = unit.tick();
        assert!(
            !matches!(r, takt_sim::TickResult::Failed(_)),
            "падение: {r:?}"
        );
        match unit.variable("led") {
            Some(takt_sim::Value::Number(n)) => trace.push(n),
            other => panic!("led: {other:?}"),
        }
    }
    trace
}

/// Трасса порождённого C: `now_ms` возвращает модельное время (1 мс/такт, с нуля),
/// колбэк `write_numeric` перехватывает запись в `led`.
fn generated_c_trace(dir: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура");
    takt_lang::compile_to_c(
        "conformance_every",
        &source,
        dir.to_str().expect("путь"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "conformance_every.h"

static uint64_t fake_now = 0;
static uint64_t clk(void *ud) {{ (void)ud; return fake_now; }}
static int64_t led = 0;
static void wr(ConformanceEvery_Out_NumericPort port, uint8_t index, int64_t v, void *ud) {{
    (void)index;
    (void)port; (void)ud; led = v;
}}

int main(void) {{
    ConformanceEvery m = {{0}};
    m.now_ms = clk;
    m.write_numeric = wr;
    /* Вход стартового состояния - "до такта 1": метка латчится в _init. */
    fake_now = 0;
    ConformanceEvery_init(&m);
    for (int tick = 1; tick <= {TICKS}; tick++) {{
        fake_now = (uint64_t)(tick - 1); /* 1 мс на такт, начиная с нуля */
        ConformanceEvery_tick(&m);
        printf("TICK %lld\n", (long long)led);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness_every.c");
    std::fs::write(&harness_path, harness).expect("харнесс");

    let bin = dir.join("conformance_every_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join("conformance_every.c"))
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
    let run = Command::new(&bin).output().expect("запуск");
    assert!(run.status.success(), "собранный C упал");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.strip_prefix("TICK ")?.trim().parse::<i128>().ok())
        .collect()
}

/// `every 3ms` при 1 мс/такт срабатывает у симулятора и у порождённого C на одних
/// тактах (3, 6, 9). Мягкая деградация: нет `cc` -> пропуск.
#[test]
fn every_period_matches_generated_c() {
    let sim = simulate_trace();
    assert_eq!(
        sim,
        vec![0, 0, 0, 1, 1, 1, 2, 2, 2, 3],
        "эталон периода `every`: {sim:?}"
    );
    if !cc_available() {
        eprintln!("[ПРОПУСК] every_period_matches_generated_c: `cc` не найден");
        return;
    }
    let dir = tempfile::tempdir().expect("временный каталог");
    let c = generated_c_trace(dir.path());
    assert_eq!(
        sim, c,
        "трассы эталона и C обязаны совпадать\nэталон={sim:?}\nC={c:?}"
    );
}

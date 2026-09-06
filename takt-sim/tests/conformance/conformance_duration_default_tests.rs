//! Сверка эталона с целью `c`: `duration` без инициализатора.

use std::path::Path;
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_duration_default.takt";
const TICKS: usize = 5;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Трасса `acc` эталона по тактам.
fn simulator_trace() -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::with_capacity(TICKS);
    for _ in 0..TICKS {
        let _ = unit.tick();
        match unit.variable("ms") {
            Some(takt_sim::Value::Number(v)) => trace.push(v),
            other => panic!("порт 'ms': ожидалось число, получено {other:?}"),
        }
    }
    trace
}

/// Трасса `acc` порождённого C по тактам.
fn generated_c_trace(dir: &Path) -> Vec<i128> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_duration_default",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "conformance_duration_default.h"

static long last_ms;

static void write_numeric(ConformanceDurationDefault_Out_NumericPort port, uint8_t index, int64_t val, void *ud) {{
    (void)index;
    (void)port; (void)ud;
    last_ms = (long)val;
}}

/* Битовый порт `late` в трассу не идёт, но колбэк обязан быть привязан:
   без него запись из тела — вызов по нулевому указателю. */
static void write_bit(ConformanceDurationDefault_Out_BitPort port, uint8_t bit, bool val, void *ud) {{
    (void)bit;
    (void)port; (void)val; (void)ud;
}}

int main(void) {{
    ConformanceDurationDefault m = {{0}};
    ConformanceDurationDefault_init(&m);
    m.write_numeric = write_numeric;
    m.write_bit = write_bit;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceDurationDefault_tick(&m);
        printf("%ld\n", last_ms);
    }}
    return 0;
}}
"#
    );
    std::fs::write(dir.join("harness.c"), harness).expect("харнесс");

    let bin = dir.join("bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-I"])
        .arg(dir)
        .arg(dir.join("conformance_duration_default.c"))
        .arg(dir.join("harness.c"))
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
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| l.trim().parse().ok())
        .collect()
}

/// Трасса `ms` совпадает у эталона и цели `c` при `duration` без инициализатора.
#[test]
fn duration_default_traces_match() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] duration_default_traces_match: `cc` не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0354_duration_default");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");

    let reference = simulator_trace();
    // Предусловие: трасса накапливающая - иначе сверка зелена и там, где поле не
    // читается вовсе.
    assert_eq!(
        reference,
        vec![1000, 2000, 3000, 4000, 5000],
        "предусловие сверки: эталон обязан дать накапливающую трассу"
    );
    let generated = generated_c_trace(&dir);
    assert_eq!(
        reference, generated,
        "трассы обязаны совпадать: эталон {reference:?}, цель c {generated:?}"
    );
}

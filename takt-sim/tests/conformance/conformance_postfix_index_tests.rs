//! Сверка эталона с целью `c`: индексация поля структуры.

use std::path::Path;
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_postfix_index.takt";
const TICKS: usize = 5;

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Трасса `(lo, hi)` эталона по тактам.
fn simulator_trace() -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::with_capacity(TICKS);
    for _ in 0..TICKS {
        let _ = unit.tick();
        let number = |name: &str| match unit.variable(name) {
            Some(takt_sim::Value::Number(v)) => v,
            other => panic!("порт '{name}': ожидалось число, получено {other:?}"),
        };
        trace.push((number("lo"), number("hi")));
    }
    trace
}

/// Трасса `(lo, hi)` порождённого C по тактам.
fn generated_c_trace(dir: &Path) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_postfix_index",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "conformance_postfix_index.h"

static long last_lo, last_hi;

static void write_numeric(ConformancePostfixIndex_Out_NumericPort port, uint8_t index, int64_t val, void *ud) {{
    (void)index;
    (void)ud;
    if (port == CONFORMANCE_POSTFIX_INDEX_PORT_LO) last_lo = (long)val; else last_hi = (long)val;
}}

int main(void) {{
    ConformancePostfixIndex m = {{0}};
    ConformancePostfixIndex_init(&m);
    m.write_numeric = write_numeric;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformancePostfixIndex_tick(&m);
        printf("%ld %ld\n", last_lo, last_hi);
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
        .arg(dir.join("conformance_postfix_index.c"))
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
        .filter_map(|l| {
            let (lo, hi) = l.trim().split_once(' ')?;
            Some((lo.parse().ok()?, hi.parse().ok()?))
        })
        .collect()
}

/// Трасса совпадает у эталона и цели `c` при индексации поля структуры.
#[test]
fn postfix_index_traces_match() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] postfix_index_traces_match: `cc` не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0358_postfix");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");

    let reference = simulator_trace();
    // Предусловие: трасса накапливающая - иначе сверка зелена и там, где поле не
    // читается вовсе.
    assert_eq!(
        reference,
        vec![(8, 13), (9, 15), (10, 17), (11, 19), (11, 19)],
        "предусловие сверки: эталон обязан дать накапливающую трассу"
    );
    let generated = generated_c_trace(&dir);
    assert_eq!(
        reference, generated,
        "трассы обязаны совпадать: эталон {reference:?}, цель c {generated:?}"
    );
}

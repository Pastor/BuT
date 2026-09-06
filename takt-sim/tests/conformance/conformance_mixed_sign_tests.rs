//! Сверка эталона с целью `c`: сравнение знакового с беззнаковым.

use std::path::Path;
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_mixed_sign.takt";
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
        "conformance_mixed_sign",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "conformance_mixed_sign.h"

static long last_lo, last_hi;

static void write_numeric(ConformanceMixedSign_Out_NumericPort port, uint8_t index, int64_t val, void *ud) {{
    (void)index;
    (void)ud;
    if (port == CONFORMANCE_MIXED_SIGN_PORT_LO) last_lo = (long)val; else last_hi = (long)val;
}}

int main(void) {{
    ConformanceMixedSign m = {{0}};
    ConformanceMixedSign_init(&m);
    m.write_numeric = write_numeric;
    m.userdata = 0;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformanceMixedSign_tick(&m);
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
        .arg(dir.join("conformance_mixed_sign.c"))
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

/// Трасса совпадает у эталона и цели `c` при смешанном сравнении.
#[test]
fn mixed_sign_traces_match() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] mixed_sign_traces_match: `cc` не найден");
        return;
    }
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_0359_mixed");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");

    let reference = simulator_trace();
    // Предусловие: трасса накапливающая - иначе сверка зелена и там, где поле не
    // читается вовсе.
    assert_eq!(
        reference,
        vec![(1, 2), (1, 2), (2, 2), (2, 1), (2, 1)],
        "предусловие сверки: эталон обязан дать накапливающую трассу"
    );
    let generated = generated_c_trace(&dir);
    assert_eq!(
        reference, generated,
        "трассы обязаны совпадать: эталон {reference:?}, цель c {generated:?}"
    );
}

//! Порт составного типа: эталон == цель `c` - (Option C).

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_port_split.takt";
const UNIT: &str = "conformance_port_split";
const TICKS: usize = 3;

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
        .join(format!("takt_0390_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Трасса эталона: `(lo, hi)` порта на каждом такте.
fn simulator_trace() -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let _ = unit.tick();
        let value = unit.variable("po").expect("порт 'po'");
        let takt_sim::Value::Struct { fields, .. } = value else {
            panic!("порт обязан быть структурой, получено {value:?}");
        };
        let field = |name: &str| match fields.iter().find(|(f, _)| f == name) {
            Some((_, takt_sim::Value::Number(v))) => *v,
            other => panic!("поле '{name}': {other:?}"),
        };
        trace.push((field("lo"), field("hi")));
    }
    trace
}

/// Та же трасса у порождённого C - через колбэки HAL.
fn generated_c_trace(dir: &Path) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        UNIT,
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = format!(
        r#"#include <stdio.h>
#include "{UNIT}.h"

static long long lo = 0, hi = 0;

static void wr(ConformancePortSplit_Out_NumericPort port, uint8_t index, int64_t value, void *ud) {{
    (void)index;
    (void)ud;
    if (port == CONFORMANCE_PORT_SPLIT_PORT_PO_LO) lo = value;
    if (port == CONFORMANCE_PORT_SPLIT_PORT_PO_HI) hi = value;
}}

int main(void) {{
    ConformancePortSplit m;
    ConformancePortSplit_init(&m);
    m.write_numeric = wr;
    for (int i = 0; i < {TICKS}; i++) {{
        ConformancePortSplit_tick(&m);
        printf("%lld %lld\n", lo, hi);
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, &harness).expect("запись харнесса");

    let bin = dir.join("port_bin");
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

/// Эталон и цель `c` дают одни значения по каждому листу.
#[test]
fn composite_port_matches_the_reference() {
    // Ожидание считается независимо: `lo` - счётчик, `hi` - счётчик плюс 10.
    let expected = vec![(1, 11), (2, 12), (3, 13)];
    let sim = simulator_trace();
    assert_eq!(sim, expected, "эталон разошёлся с ожиданием");

    if !cc_available() {
        eprintln!("[ПРОПУСК] компилятор `cc` не найден; трасса эталона уже сверена");
        return;
    }
    let dir = build_dir("trace");
    assert_eq!(generated_c_trace(&dir), sim, "цель c разошлась с эталоном");
}

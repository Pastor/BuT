//! Срез массива в аргументе вызова: эталон == цель `c`.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_slice_argument.takt";
const UNIT: &str = "conformance_slice_argument";

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
        .join(format!("takt_0400_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Значения `(mid, tail, sum)` у эталона после такта.
fn simulator_values() -> (i128, i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let _ = unit.tick();
    let number = |name: &str| match unit.variable(name) {
        Some(takt_sim::Value::Number(v)) => v,
        other => panic!("переменная '{name}' обязана быть числом, получено {other:?}"),
    };
    (number("o_mid"), number("o_tail"), number("o_sum"))
}

/// Те же значения у порождённого C - прогоном харнесса.
fn generated_c_values(dir: &Path) -> (i128, i128, i128) {
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

int main(void) {{
    ConformanceSliceArgument m;
    ConformanceSliceArgument_init(&m);
    ConformanceSliceArgument_tick(&m);
    printf("%d %d %d\n", (int)m.o_mid, (int)m.o_tail, (int)m.o_sum);
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, &harness).expect("запись харнесса");

    let bin = dir.join("slice_bin");
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
        "cc не собрал харнесс флагами гейта цели:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&bin).output().expect("запуск харнесса");
    assert!(run.status.success(), "харнесс завершился с ошибкой");
    let stdout = String::from_utf8_lossy(&run.stdout);
    let mut parts = stdout
        .split_whitespace()
        .map(|p| p.parse::<i128>().expect("число в выводе харнесса"));
    (
        parts.next().expect("mid"),
        parts.next().expect("tail"),
        parts.next().expect("sum"),
    )
}

/// Эталон и цель `c` дают одни значения.
#[test]
fn slice_argument_matches_the_reference() {
    // Ожидание считается независимо от обоих исполнителей: `src = {5,6,7,8}`,
    // `first(src[1:3])` = 6, `first(src[2:4])` = 7, `total(src[0:2])` = 11.
    let expected = (6i128, 7i128, 11i128);
    assert_eq!(simulator_values(), expected, "эталон разошёлся с ожиданием");

    if !cc_available() {
        eprintln!("[ПРОПУСК] компилятор `cc` не найден; трасса эталона уже сверена");
        return;
    }
    let dir = build_dir("values");
    assert_eq!(
        generated_c_values(&dir),
        expected,
        "цель c разошлась с эталоном"
    );
}

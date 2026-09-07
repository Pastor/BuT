//! Умолчание переменной перечислимого типа: эталон == цель `c` -.
//!
//! #
//!
//! Умолчание - **первый по тексту** вариант: ноль может не принадлежать набору, и тогда
//! автомат стартует со значения, о котором не знает ни один `match`. Порядок объявления
//! в языке значим, поэтому "первый" - по тексту, а не по значению.
//!
//! Фикстура различает **оба** возможных прочтения: у `Order` первый по тексту (2) не
//! наименьший (1); контрольный `Plain` начинается с нуля - там прежнее поведение
//! обязано сохраниться.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_enum_default.takt";
const UNIT: &str = "conformance_enum_default";

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
        .join(format!("takt_0391_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Значения `(mode, order, plain)` у эталона после такта.
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
    (number("o_mode"), number("o_order"), number("o_plain"))
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
    ConformanceEnumDefault m;
    ConformanceEnumDefault_init(&m);
    ConformanceEnumDefault_tick(&m);
    printf("%d %d %d\n", (int)m.o_mode, (int)m.o_order, (int)m.o_plain);
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, &harness).expect("запись харнесса");

    let bin = dir.join("enum_bin");
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
    let stdout = String::from_utf8_lossy(&run.stdout);
    let mut parts = stdout
        .split_whitespace()
        .map(|p| p.parse::<i128>().expect("число в выводе харнесса"));
    (
        parts.next().expect("mode"),
        parts.next().expect("order"),
        parts.next().expect("plain"),
    )
}

/// Эталон и цель `c` дают умолчанием первый по тексту вариант.
#[test]
fn enum_default_is_the_first_variant() {
    // Ожидание считается независимо от обоих исполнителей: `Idle = 5` первый у `Mode`,
    // `Second = 2` первый по тексту у `Order` (наименьший там 1), `Zero = 0` первый у
    // `Plain`.
    let expected = (5i128, 2i128, 0i128);
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

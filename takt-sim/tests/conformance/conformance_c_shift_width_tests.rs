//! Потактовая сверка сдвига на величину, не меньшую ширины **продвинутого** типа, у
//! цели `c`.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_c_shift_width.takt";
const UNIT: &str = "conformance_c_shift_width";

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Каталог теста, уникальный по имени потока.
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0392_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Значения `(беззнаковый, знаковый, контрольный)` у эталона после такта.
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
    (number("o_unsigned"), number("o_signed"), number("o_narrow"))
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
    ConformanceCShiftWidth m;
    ConformanceCShiftWidth_init(&m);
    ConformanceCShiftWidth_tick(&m);
    printf("%lld %lld %lld\n", (long long)m.o_unsigned, (long long)m.o_signed,
           (long long)m.o_narrow);
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, &harness).expect("запись харнесса");

    let bin = dir.join("shift_bin");
    // Те же флаги, что у проверки цели: класс, ради которого фича заведена, - именно отказ
    // `cc -Werror` при рапорте об успехе.
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
        parts.next().expect("беззнаковый"),
        parts.next().expect("знаковый"),
        parts.next().expect("контрольный"),
    )
}

/// Эталон и цель `c` дают одни значения - и вывод собирается флагами проверки.
#[test]
fn shift_beyond_promoted_width_matches_the_reference() {
    // Ожидание считается независимо от обоих исполнителей: совпадение двух реализаций
    // между собой ещё не значит, что они правы.
    let expected = (0i128, -1i128, 0i128);
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

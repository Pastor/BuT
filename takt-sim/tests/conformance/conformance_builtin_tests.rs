//! Встроенные функции: эталон == цель `c` такт в такт -.
//!
//! # Что наблюдается
//!
//! Четыре переменные на каждом такте: `min`, `max`, `clamp` от растущего счётчика и
//! `abs` от отрицательного. Значения меняются по тактам, поэтому перепутанные местами
//! границы (`min` вместо `max`) видны сразу - первая редакция реализации именно так и
//! ошиблась, и поймала это проба, а не чтение.
//!
//! **Q-формат проверяется отдельным входом:** у `q(m, n)` сравнение идёт по
//! представлению, а унарный минус несёт нормировку переноса и насыщения (0061, 0127,
//! 0170). Если бы эталон считал встроенные своей арифметикой, а не общим слоем
//! `eval::ops`, разошёлся бы он именно здесь.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Тактов в трассе.
const TICKS: usize = 5;

/// Целочисленный вход: все четыре вычислимые встроенные сразу.
const INT_UNIT: &str = "builtin_int";
const INT_SRC: &str = "var n: u8 := 0;\n\
                       var mn: u8 := 0;\n\
                       var mx: u8 := 0;\n\
                       var cl: u8 := 0;\n\
                       var ab: i8 := 0;\n\
                       var neg: i8 := -5;\n\
                       start Idle {\n\
                           always {\n\
                               n := n + 1;\n\
                               mn := min(n, 3);\n\
                               mx := max(n, 3);\n\
                               cl := clamp(n, 2, 4);\n\
                               ab := abs(neg);\n\
                           }\n\
                           ref Idle;\n\
                       }\n";

/// Вход на `q(8, 8)`: сравнение идёт по представлению, а не по числу.
const FIXED_UNIT: &str = "builtin_fixed";
const FIXED_SRC: &str = "var t: q(8, 8) := 0.0;\n\
                         var step: q(8, 8) := 0.75;\n\
                         var lim: q(8, 8) := 2.0;\n\
                         var mn: q(8, 8) := 0.0;\n\
                         var ab: q(8, 8) := 0.0;\n\
                         var neg: q(8, 8) := -1.5;\n\
                         start Idle {\n\
                             always {\n\
                                 t := t + step;\n\
                                 mn := min(t, lim);\n\
                                 ab := abs(neg);\n\
                             }\n\
                             ref Idle;\n\
                         }\n";

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Каталог сборки, уникальный по имени потока (тесты идут параллельно, 0190).
///
/// `:` вычищается: имя теста после слияния целей несёт префикс модуля, и двоеточие
/// попало бы в путь.
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0248_conformance_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

/// Потактовая трасса эталона по перечисленным именам переменных.
fn simulator_trace(src: &str, names: &[&str]) -> Vec<Vec<i128>> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, takt_sim::TickResult::Failed(_)),
            "эталон обязан исполнять встроенные функции: {result:?}"
        );
        trace.push(names.iter().map(|name| raw_value(&unit, name)).collect());
    }
    trace
}

/// Наблюдаемое значение переменной "как лежит": у `q(m, n)` это представление, то же
/// самое, что печатает прошивка (целое `intW`).
fn raw_value(unit: &takt_sim::Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(takt_sim::Value::Number(v)) => v,
        Some(takt_sim::Value::Fixed { repr, .. }) => i128::from(repr),
        other => panic!("переменная '{name}': неожиданное значение {other:?}"),
    }
}

/// Потактовая трасса порождённой прошивки: харнесс печатает те же поля.
fn generated_c_trace(
    dir: &Path,
    unit_name: &str,
    src: &str,
    root: &str,
    fields: &[&str],
) -> Vec<Vec<i128>> {
    takt_lang::compile_to_c(
        unit_name,
        src,
        dir.to_str().expect("путь в UTF-8"),
        &[] as &[String],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let prints = fields
        .iter()
        .map(|f| format!("        printf(\"%ld \", (long)m.{f});"))
        .collect::<Vec<_>>()
        .join("\n");
    let harness = format!(
        r#"#include <stdio.h>
#include "{unit_name}.h"

int main(void) {{
    {root} m = {{0}};
    {root}_init(&m);
    for (int i = 0; i < {TICKS}; i++) {{
        {root}_tick(&m);
{prints}
        printf("\n");
    }}
    return 0;
}}
"#
    );
    let harness_path = dir.join("harness.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join(format!("{unit_name}.c")))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|token| token.parse::<i128>().expect("число в выводе"))
                .collect()
        })
        .collect()
}

/// **A1: `min`/`max`/`abs`/`clamp` над целыми - эталон == цель `c`.**
#[test]
fn integer_builtins_match_c() {
    let names = ["mn", "mx", "cl", "ab"];
    let expected = simulator_trace(INT_SRC, &names);
    // Значения обязаны меняться по тактам: постоянная трасса прошла бы и у реализации,
    // всегда возвращающей первый аргумент.
    assert!(
        expected.iter().any(|row| row != &expected[0]),
        "трасса обязана меняться по тактам, получено {expected:?}"
    );
    assert_eq!(
        expected[0],
        vec![1, 3, 2, 5],
        "такт 1: min(1,3)=1, max(1,3)=3, clamp(1,2,4)=2, abs(-5)=5"
    );

    if !cc_available() {
        eprintln!("cc недоступен — сверка с целью `c` пропущена");
        return;
    }
    let dir = build_dir("int");
    let actual = generated_c_trace(&dir, INT_UNIT, INT_SRC, "BuiltinInt", &names);
    assert_eq!(
        expected, actual,
        "встроенные функции обязаны давать одну трассу у эталона и цели `c`"
    );
}

/// **A2: те же встроенные над `q(8, 8)` - эталон == цель `c`.**
///
/// Сравнение q идёт по представлению, а `abs` - через унарный минус с нормировкой
/// формата; своя арифметика в эталоне разошлась бы здесь.
#[test]
fn fixed_point_builtins_match_c() {
    let names = ["mn", "ab"];
    let expected = simulator_trace(FIXED_SRC, &names);
    assert!(
        expected.iter().any(|row| row != &expected[0]),
        "трасса q обязана меняться по тактам, получено {expected:?}"
    );

    if !cc_available() {
        eprintln!("cc недоступен — сверка с целью `c` пропущена");
        return;
    }
    let dir = build_dir("fixed");
    let actual = generated_c_trace(&dir, FIXED_UNIT, FIXED_SRC, "BuiltinFixed", &names);
    assert_eq!(
        expected, actual,
        "встроенные над q(8, 8) обязаны давать одну трассу у эталона и цели `c`"
    );
}

/// **A3: `debug` не останавливает прогон** (до фичи он давал `SIM-020`).
#[test]
fn debug_does_not_stop_the_run() {
    const SRC: &str = "var n: u8 := 0;\n\
                       start Idle { always { debug(\"такт\"); n := n + 1; } ref Idle; }\n";
    let trace = simulator_trace(SRC, &["n"]);
    assert_eq!(
        trace.into_iter().flatten().collect::<Vec<_>>(),
        vec![1, 2, 3, 4, 5],
        "счётчик обязан расти: `debug` — побочный эффект, а не отказ"
    );
}

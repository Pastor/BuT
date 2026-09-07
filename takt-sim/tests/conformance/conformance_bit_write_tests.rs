//! Запись разряда `x.N := v`: эталон == цель `c` такт в такт.
//!
//! # Что наблюдается
//!
//! **Тело накапливающее**: разряды устанавливаются и
//! сбрасываются по счётчику, поэтому значение меняется от такта к такту. На
//! идемпотентном теле пропуск записи и двойная запись неразличимы.
//!
//! Проверяются разом четыре свойства, каждое ловится своей клеткой трассы:
//!
//! | Свойство | Как ловится |
//! |---|---|
//! | установка разряда | `b` растёт на маску |
//! | сброс разряда | `b` уменьшается на маску, прочие разряды целы |
//! | нулевой разряд | отдельная переменная: у него особая печать (`1` без сдвига) |
//! | **младший бит значения** | `b.5 := 2` обязано разряд **очистить**, а не установить |
//!
//! Правило младшего бита - главная клетка. Оно нигде не было записано и лишь выводилось
//! из того, что печатает цель `c` (`(rhs & 1u) << N`); реализуй эталон "ненулевое
//! значит единица" - трасса разошлась бы ровно здесь, а все прочие проверки остались бы
//! зелёными.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Тактов в трассе.
const TICKS: usize = 6;

/// Вход: установка, сброс, нулевой разряд и правило младшего бита.
const UNIT: &str = "bit_write";
const SRC: &str = "var b: u8 := 0;\n\
                   var z: u8 := 0;\n\
                   var packed: [bit;8] := 0;\n\
                   var n: u8 := 0;\n\
                   start Idle {\n\
                       always {\n\
                           n := n + 1;\n\
                           b.3 := 1;\n\
                           b.1 := n;\n\
                           b.5 := 2;\n\
                           z.0 := n;\n\
                           packed.6 := 1;\n\
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

/// Каталог сборки, уникальный по имени потока (тесты идут параллельно, 0190; `:`
/// вычищается - имя теста несёт префикс модуля, 0244).
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0250_conformance_{thread}_{tag}"));
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
            "эталон обязан исполнять запись разряда: {result:?}"
        );
        trace.push(
            names
                .iter()
                .map(|name| match unit.variable(name) {
                    Some(takt_sim::Value::Number(v)) => v,
                    other => panic!("переменная '{name}': неожиданное значение {other:?}"),
                })
                .collect(),
        );
    }
    trace
}

/// Потактовая трасса порождённой прошивки: харнесс печатает те же поля.
fn generated_c_trace(dir: &Path, src: &str, root: &str, fields: &[&str]) -> Vec<Vec<i128>> {
    takt_lang::compile_to_c(
        UNIT,
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
#include "{UNIT}.h"

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
        .arg(dir.join(format!("{UNIT}.c")))
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

/// **Запись разряда - эталон == цель `c`, включая правило младшего бита.**
#[test]
fn bit_write_matches_c() {
    let names = ["b", "z", "packed"];
    let expected = simulator_trace(SRC, &names);

    // Трасса обязана меняться по тактам: постоянная прошла бы и у реализации, которая
    // записи вовсе не делает (тело было бы идемпотентным).
    assert!(
        expected.iter().any(|row| row != &expected[0]),
        "трасса обязана меняться по тактам, получено {expected:?}"
    );

    // Такт 1: n = 1. `b.3 := 1` -> 8; `b.1 := 1` -> +2 = 10; `b.5 := 2` - младший бит
    // двойки нулевой, разряд очищается, значение не меняется.
    assert_eq!(
        expected[0],
        vec![10, 1, 64],
        "такт 1: b = 8|2 = 10 (разряд 5 не установлен: младший бит двойки — ноль)"
    );
    // Такт 2: n = 2. `b.1 := 2` - младший бит двойки нулевой, разряд 1 очищается: 10 ->
    // 8.
    assert_eq!(
        expected[1],
        vec![8, 0, 64],
        "такт 2: чётное значение очищает разряд — это и есть правило младшего бита"
    );

    if !cc_available() {
        eprintln!("cc недоступен — сверка с целью `c` пропущена");
        return;
    }
    let dir = build_dir("bits");
    let actual = generated_c_trace(&dir, SRC, "BitWrite", &names);
    assert_eq!(
        expected, actual,
        "запись разряда обязана давать одну трассу у эталона и цели `c`"
    );
}

/// **Запись разряда элемента массива - эталон == цель `c`.**
///
/// Отдельный вход: путь к месту здесь двухсегментный (индекс + разряд), и перепутанный
/// порядок сегментов дал бы верное значение у односегментных случаев и неверное здесь.
#[test]
fn bit_write_into_array_element_matches_c() {
    const ELEM_SRC: &str = "var arr: [u8; 3] := { 0, 0, 0 };\n\
                            var n: u8 := 0;\n\
                            var seen: u8 := 0;\n\
                            start Idle {\n\
                                always {\n\
                                    n := n + 1;\n\
                                    arr[1].2 := 1;\n\
                                    arr[1].0 := n;\n\
                                    seen := arr[1];\n\
                                }\n\
                                ref Idle;\n\
                            }\n";
    let names = ["seen"];
    let expected = simulator_trace(ELEM_SRC, &names);
    assert_eq!(expected[0], vec![5], "такт 1: разряды 2 и 0 → 5");
    assert_eq!(expected[1], vec![4], "такт 2: n чётно, разряд 0 очищен → 4");

    if !cc_available() {
        eprintln!("cc недоступен — сверка с целью `c` пропущена");
        return;
    }
    let dir = build_dir("elem");
    // Соседние элементы обязаны остаться нулями: точечность записи - свойство места, и
    // её ломает любая замена массива целиком.
    let actual = generated_c_trace(&dir, ELEM_SRC, "BitWrite", &names);
    assert_eq!(
        expected, actual,
        "запись разряда элемента обязана давать одну трассу"
    );
}

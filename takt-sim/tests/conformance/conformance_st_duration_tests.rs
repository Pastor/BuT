//! Потактовая сверка **значений** типа `duration` цели `st` с эталоном.
//!
//! Это слабее по существу: MatIEC принимает и молча неверный код - ровно на этом стояли
//! дефекты 0041, а у самой цели `st` потактовый сдвиг первого скана дожил до при
//! зелёном `iec2c`.
//!
//! Граница, ради которой сверка заведена, узкая и тихая: представление эталона -
//! **наносекунды**, представление цели - **миллисекунды**. Ошибка на
//! ней (не тот множитель, `250us` -> 0 мс) даёт валидный ST с другим поведением.
//!
//! Исправлениетура - **та же**, что у целей `c` и `rust` (`conformance_duration_value.takt`):
//! сопоставимость трёх целей важнее единообразия с `sv`, которая ту же запись не берёт
//! вовсе (`SV-002` на `as`, своя фикстура - `conformance_sv_duration_tests.rs`).
//!
//! Время выполненияа у ST нет: `iec2c` транслирует в C, драйвер вызывает `_body__` и читает поля
//! структуры (образец - `conformance_st_every_tests.rs`).
//!
//! Мягкая деградация: нет `iec2c`/`cc`/заголовков MatIEC -> пропуск, не отказ.

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURE: &str = "tests/data/eval/conformance_duration_value.takt";
/// Имя входа для порождения: корень ST берёт имя из имени файла.
const UNIT: &str = "stdurvalue.takt";
/// Корень фикстуры в C-символах `iec2c` (верхний регистр от `stdurvalue`).
const ROOT: &str = "STDURVALUE";
/// Путь к наблюдаемым полям: под-FB `TIMERS0`, поля `MS` и `LATE`.
const PORT_MS: &str = "TIMERS0.MS";
const PORT_LATE: &str = "TIMERS0.LATE";
/// Значение `elapsed` в миллисекундах - приватное поле того же под-FB.
const VAR_ELAPSED: &str = "TIMERS0.ELAPSED";
const SCANS: usize = 3;

fn iec2c_prefix() -> PathBuf {
    std::env::var_os("IEC2C_PREFIX")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(std::env::var_os("HOME").unwrap()).join(".local"))
}

fn iec2c_available() -> Option<(PathBuf, PathBuf)> {
    let prefix = iec2c_prefix();
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    (bin.is_file() && lib.join("ieclib.txt").is_file()).then_some((bin, lib))
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Значения `(ms, late, elapsed)` у эталона после первого такта.
fn simulator_values() -> (i128, i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let _ = unit.tick();
    let number = |name: &str| match unit.variable(name) {
        Some(takt_sim::Value::Number(v)) => v,
        // Длительность эталон держит наносекундами - приводим к миллисекундам здесь, а
        // не в цели: перевод и есть предмет сверки.
        Some(takt_sim::Value::Duration(ns)) => i128::from(ns / 1_000_000),
        other => panic!("значение '{name}' не число и не длительность: {other:?}"),
    };
    (number("ms"), number("late"), number("elapsed"))
}

/// Значения `(ms, late, elapsed)` у порождённого ST после каждого скана.
fn generated_st_values(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<(i128, i128, i128)> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        UNIT,
        &source,
        st_dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");

    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_dir.join("stdurvalue.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал ST длительностей:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    {ROOT}_data__ fb = {{0}};
    {ROOT}_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {SCANS}; i++) {{
        __CURRENT_TIME.tv_sec = 0;
        __CURRENT_TIME.tv_nsec = (long)i * 1000000L;
        {ROOT}_body__(&fb);
        printf("SCAN %u %u %u\n",
               (unsigned)fb.{PORT_MS}.value,
               (unsigned)fb.{PORT_LATE}.value,
               (unsigned)fb.{VAR_ELAPSED}.value);
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness_duration.c");
    std::fs::write(&harness_path, harness).expect("драйвер");
    let bin = work.join("st_duration_bin");
    let compile = Command::new("cc")
        .args(["-std=c99", "-w", "-I"])
        .arg(lib.join("C"))
        .arg("-I")
        .arg(&work)
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "ST длительностей (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера");
    assert!(run.status.success(), "драйвер ST длительностей упал");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|line| {
            let rest = line.strip_prefix("SCAN ")?;
            let mut it = rest.split_whitespace();
            let ms = it.next()?.parse::<i128>().ok()?;
            let late = it.next()?.parse::<i128>().ok()?;
            let elapsed = it.next()?.parse::<i128>().ok()?;
            Some((ms, late, elapsed))
        })
        .collect()
}

/// Каталог сборки уникален по тесту (; после слияния целей имя потока несёт `::`, и
/// двоеточие вычищается - ).
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

/// Значение длительности, результат сравнения и приведение к числу совпадают у эталона
/// и у порождённого ST.
///
/// Сверяются **значения**, а не факт трансляции: `iec2c` доказывает, что ST валиден, но
/// не что он считает то же.
#[test]
fn duration_values_match_generated_st() {
    let (ms, late, elapsed) = simulator_values();
    // Эталон: 1s + 750ms = 1750 мс, сравнение с 500ms истинно.
    assert_eq!(
        (ms, late, elapsed),
        (1750, 1, 1750),
        "эталон обязан давать 1750 мс и истинное сравнение"
    );

    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("[ПРОПУСК] duration_values_match_generated_st: iec2c не найден");
        return;
    };
    if !cc_available() {
        eprintln!("[ПРОПУСК] duration_values_match_generated_st: cc не найден");
        return;
    }

    let dir = build_dir("st_duration");
    let scans = generated_st_values(&dir, &iec2c, &lib);
    assert_eq!(scans.len(), SCANS, "драйвер обязан напечатать каждый скан");
    for (index, got) in scans.iter().enumerate() {
        assert_eq!(
            *got,
            (ms, late, elapsed),
            "скан {}: ST разошёлся с эталоном (ms, late, elapsed)",
            index + 1
        );
    }
}

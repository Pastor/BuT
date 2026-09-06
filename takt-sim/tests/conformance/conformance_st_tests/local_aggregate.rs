//! Агрегат в локальном объявлении тела - эталон == `st`.
//!
//! # Что ловится
//!
//! Цель отвечала `ST-011` на `var p: Point := {0, 0};` внутри функции - отказ на
//! записи, которую эталон, `c` и `rust` исполняют. Агрегатной формы значения в IEC
//! 61131-3 нет, но **поэлементная** есть, и место записи выбирает общий носитель.
//!
//! Сверяются **значения**: поэлементная запись, перепутавшая поля, ST компилирует
//! прекрасно.

use super::*;

const LOCAL_FIXTURE: &str = "tests/data/eval/conformance_local_aggregate.takt";

/// Трасса `(sum, ctl)` у эталона.
fn simulate_local_trace() -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(LOCAL_FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    for _ in 0..MAX_TICKS {
        match unit.tick() {
            TickResult::Failed(why) => panic!("эталон остановился: {why}"),
            TickResult::Terminated => break,
            TickResult::Processing => {}
        }
        trace.push((sim_value(&unit, "sum"), sim_value(&unit, "ctl")));
    }
    trace
}

/// Та же трасса у порождённого ST.
fn run_generated_st_local(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(LOCAL_FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "localagg.takt",
        &source,
        st_dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение ST");

    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог iec2c");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_dir.join("localagg.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не принял ST с локальным агрегатом:\n{}",
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
    LOCALAGG_data__ fb = {{0}};
    LOCALAGG_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        LOCALAGG_body__(&fb);
        printf("%d %d\n", (int)fb.SUM.value, (int)fb.CTL.value);
        if (fb.IS_DONE.value) break;
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness_local.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");
    let bin = work.join("localagg_bin");
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
        "ST с локальным агрегатом не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск драйвера ST");
    assert!(run.status.success(), "драйвер ST завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter_map(|l| {
            let mut it = l.split_whitespace();
            Some((it.next()?.parse().ok()?, it.next()?.parse().ok()?))
        })
        .collect()
}

/// Значения совпадают: функция со структурой даёт 7, контрольная - 3.
#[test]
fn local_aggregate_matches_generated_st() {
    let expected = simulate_local_trace();
    assert!(
        expected.first() == Some(&(7, 3)),
        "контроль: эталон обязан дать (sum, ctl) = (7, 3), получено {expected:?}"
    );
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c/заголовки MatIEC недоступны — сверка ST пропущена");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка ST пропущена");
        return;
    }
    let dir = std::env::temp_dir().join(format!("st_localagg_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("рабочий каталог");
    let actual = run_generated_st_local(&dir, &iec2c, &lib);
    let common: Vec<(i128, i128)> = actual.iter().take(expected.len()).copied().collect();
    assert_eq!(
        common, expected,
        "трасса ST разошлась с эталоном\nST     = {actual:?}\nэталон = {expected:?}"
    );
}

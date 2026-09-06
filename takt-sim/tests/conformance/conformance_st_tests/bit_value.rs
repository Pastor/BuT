//! Разряд в позиции числового значения - эталон == `st`.
//!
//! # Что ловится
//!
//! Цель печатала разряд **булевым** выражением над маской (битового доступа в MatIEC
//! нет вовсе), и `high := (...) <> 16#00;` при `high: USINT` отвергал `iec2c` - при
//! **нулевом** коде возврата `taktc`.
//!
//! Форма выбрана **пробой**: `BOOL_TO_USINT(...)` - стандартное преобразование,
//! принятое `iec2c` 2026-08-20.
//!
//! Сверяются **значения**, а не факт компиляции: обёртка, дающая не тот разряд,
//! компилируется прекрасно.

use super::*;

/// Исправлениетура: `high := src.7`, `low := src.0` при `src: u8 := 200`.
const BIT_FIXTURE: &str = "tests/data/eval/conformance_bit_value.takt";

/// Трасса `(high, low)` у эталона.
fn simulate_bit_trace() -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(BIT_FIXTURE).expect("фикстура читается");
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
        trace.push((sim_value(&unit, "high"), sim_value(&unit, "low")));
    }
    trace
}

/// Та же трасса у порождённого ST (через `iec2c` и `cc`).
fn run_generated_st_bit(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(BIT_FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "bitvalue.takt",
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
        .arg(st_dir.join("bitvalue.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не принял ST с разрядом в числовой переменной:\n{}",
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
    BITVALUE_data__ fb = {{0}};
    BITVALUE_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        BITVALUE_body__(&fb);
        printf("%d %d\n", (int)fb.HIGH.value, (int)fb.LOW.value);
        if (fb.IS_DONE.value) break;
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness_bit.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");
    let bin = work.join("bitvalue_bin");
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
        "ST с разрядом (через iec2c) не собирается:\n{}",
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

/// Значения разрядов совпадают: старший даёт 1, младший - 0.
///
/// Наблюдаются **два** разряда: обёртка, всегда дающая 1 (например, потерявшая маску),
/// прошла бы проверку по одному старшему.
#[test]
fn bit_value_matches_generated_st() {
    let expected = simulate_bit_trace();
    assert!(
        expected.first() == Some(&(1, 0)),
        "контроль: эталон обязан дать (high, low) = (1, 0), получено {expected:?}"
    );
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c/заголовки MatIEC недоступны — сверка ST пропущена");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка ST пропущена");
        return;
    }
    let dir = std::env::temp_dir().join(format!("st_bitvalue_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("рабочий каталог");
    let actual = run_generated_st_bit(&dir, &iec2c, &lib);
    let common: Vec<(i128, i128)> = actual.iter().take(expected.len()).copied().collect();
    assert_eq!(
        common, expected,
        "трасса ST разошлась с эталоном\nST     = {actual:?}\nэталон = {expected:?}"
    );
}

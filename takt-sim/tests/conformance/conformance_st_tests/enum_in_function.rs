//! Перечисление внутри функции - эталон == `st`.
//!
//! # Что ловится
//!
//! Константы перечисления живут в `VAR CONSTANT` функционального блока, а `FUNCTION` в
//! IEC 61131-3 - замкнутая единица: `iec2c` отвечал "Ambiguous enumerate value or
//! Variable not declared in this scope" при **нулевом** коде возврата `taktc`.
//!
//! Сверяются **значения**: дублированная константа с неверным значением компилируется
//! прекрасно, и вердикт даёт только число.

use super::*;

const ENUM_FIXTURE: &str = "tests/data/eval/conformance_enum_in_function.takt";

/// Трасса `(code, ctl)` у эталона.
fn simulate_enum_trace() -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(ENUM_FIXTURE).expect("фикстура читается");
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
        trace.push((sim_value(&unit, "code"), sim_value(&unit, "ctl")));
    }
    trace
}

/// Та же трасса у порождённого ST.
fn run_generated_st_enum(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<(i128, i128)> {
    let source = std::fs::read_to_string(ENUM_FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "enumfn.takt",
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
        .arg(st_dir.join("enumfn.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не принял ST с перечислением внутри функции:\n{}",
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
    ENUMFN_data__ fb = {{0}};
    ENUMFN_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {MAX_TICKS}; i++) {{
        ENUMFN_body__(&fb);
        printf("%d %d\n", (int)fb.CODE.value, (int)fb.CTL.value);
        if (fb.IS_DONE.value) break;
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness_enum.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");
    let bin = work.join("enumfn_bin");
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
        "ST с перечислением (через iec2c) не собирается:\n{}",
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

/// Значения совпадают: `code = 9` (вариант `Busy` различён), `ctl = 3`.
#[test]
fn enum_in_function_matches_generated_st() {
    let expected = simulate_enum_trace();
    assert!(
        expected.first() == Some(&(9, 3)),
        "контроль: эталон обязан дать (code, ctl) = (9, 3), получено {expected:?}"
    );
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c/заголовки MatIEC недоступны — сверка ST пропущена");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка ST пропущена");
        return;
    }
    let dir = std::env::temp_dir().join(format!("st_enumfn_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("рабочий каталог");
    let actual = run_generated_st_enum(&dir, &iec2c, &lib);
    let common: Vec<(i128, i128)> = actual.iter().take(expected.len()).copied().collect();
    assert_eq!(
        common, expected,
        "трасса ST разошлась с эталоном\nST     = {actual:?}\nэталон = {expected:?}"
    );
}

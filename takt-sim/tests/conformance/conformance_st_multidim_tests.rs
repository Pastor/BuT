//! Потактовая сверка многомерного массива у цели `st`.
//!
//! ## Что доказывает
//!
//! В IEC 61131-3 массивы **не вкладываются**: `st_type` печатает `[[u8; 2]; 2]`
//! многомерной формой `ARRAY [0..1, 0..1] OF USINT` (T12 ), а индексация печаталась
//! формой C - `cells[0][1]`. `iec2c` такой вывод отвергал ("Number of
//! subscripts/indexes does not match ... (array has 0 indexes)") при **нулевом** коде
//! возврата `taktc`, тогда как эталон, `rust` и `sv` тот же вход исполняют.

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/st_multidim.takt";

/// Имя корневой модели в C-символах `iec2c` (идентификаторы IEC регистронезависимы, и
/// `iec2c` печатает их в верхнем регистре).
const ROOT: &str = "STMULTIDIM";

/// Тактов в трассе - больше, чем нужно автомату: хвост после завершения тоже обязан
/// совпасть.
const TICKS: usize = 5;

/// Наблюдаемые: `(имя в симуляторе, путь поля в структуре POUS)`.
///
/// `got` растёт (2 -> 12 -> 22 -> 32), `other` стоит на месте (3): вместе они отличают
/// верную индексацию от зеркальной.
const OBSERVED: &[(&str, &str)] = &[
    ("n", "GRID0.N"),
    ("got", "GRID0.GOT"),
    ("other", "GRID0.OTHER"),
];

fn iec2c_prefix() -> PathBuf {
    if let Ok(p) = std::env::var("IEC2C_PREFIX") {
        return PathBuf::from(p);
    }
    PathBuf::from(std::env::var("HOME").unwrap_or_default()).join(".local")
}

/// `(бинарник iec2c, каталог lib MatIEC)` - если оба на месте.
fn iec2c_available() -> Option<(PathBuf, PathBuf)> {
    let prefix = iec2c_prefix();
    let bin = prefix.join("bin").join("iec2c");
    let lib = prefix.join("share").join("matiec").join("lib");
    (bin.is_file() && lib.join("C").is_dir()).then_some((bin, lib))
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Трасса эталона: значения наблюдаемых после каждого такта.
fn simulate_trace() -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция не должна падать: {result:?}"
        );
        trace.push(
            OBSERVED
                .iter()
                .map(|(name, _)| sim_value(&unit, name))
                .collect(),
        );
    }
    trace
}

fn sim_value(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(n)) => n,
        Some(Value::Boolean(b)) => i128::from(b),
        other => panic!("переменная '{name}': неожиданное значение {other:?}"),
    }
}

/// Трасса цели: тот же ST, оттранслированный `iec2c` и исполненный.
fn st_trace(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "st_multidim.takt",
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
        .arg(st_dir.join("st_multidim.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал порождённый ST:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let prints = OBSERVED
        .iter()
        .map(|(_, path)| format!(r#"        printf("%lld ", (long long)fb.{path}.value);"#))
        .collect::<Vec<_>>()
        .join("\n");
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
    for (int i = 0; i < {TICKS}; i++) {{
        {ROOT}_body__(&fb);
{prints}
        printf("\n");
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");

    let bin = work.join("st_multidim_bin");
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
        "порождённый ST (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&bin).output().expect("запуск драйвера ST");
    assert!(run.status.success(), "драйвер ST завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            l.split_whitespace()
                .map(|v| v.parse().expect("число в трассе"))
                .collect()
        })
        .collect()
}

/// Трассы эталона и цели `st` совпадают такт в такт.
///
/// До этот тест не мог даже дойти до сравнения: `iec2c` отвергал порождённый ST.
#[test]
fn st_multidim_trace_matches_reference_tick_by_tick() {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c не установлен — сверка пропущена (см. scripts/ensure-iec2c.sh)");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка пропущена");
        return;
    }

    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_st_multidim_{}",
            std::thread::current()
                .name()
                .unwrap_or("main")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("рабочий каталог");

    let reference = simulate_trace();
    let target = st_trace(&dir, &iec2c, &lib);

    assert_eq!(
        target.len(),
        reference.len(),
        "длина трасс:\nэталон {reference:?}\nst     {target:?}"
    );
    for (tick, (r, t)) in reference.iter().zip(target.iter()).enumerate() {
        assert_eq!(
            t,
            r,
            "такт {} разошёлся.\nнаблюдаемые: {:?}\nэталон: {reference:?}\nst:     {target:?}",
            tick + 1,
            OBSERVED.iter().map(|(n, _)| *n).collect::<Vec<_>>()
        );
    }

    let _ = std::fs::remove_dir_all(&dir);
}

// --: вложенный агрегат раскрывается до листьев --------------------

const AGG_FIXTURE: &str = "tests/data/eval/st_nested_aggregate.takt";
const AGG_ROOT: &str = "STNESTEDAGGREGATE";
const AGG_OBSERVED: &[(&str, &str)] = &[("n", "GRID0.N"), ("a", "GRID0.A"), ("b", "GRID0.B")];

/// Трасса эталона по исправлениетуре вложенного агрегата.
fn aggregate_simulate_trace() -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(AGG_FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut trace = Vec::new();
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция не должна падать: {result:?}"
        );
        trace.push(
            AGG_OBSERVED
                .iter()
                .map(|(name, _)| sim_value(&unit, name))
                .collect(),
        );
    }
    trace
}

/// Трасса цели `st` по той же исправлениетуре.
fn aggregate_st_trace(dir: &Path, iec2c: &Path, lib: &Path) -> Vec<Vec<i128>> {
    let source = std::fs::read_to_string(AGG_FIXTURE).expect("фикстура читается");
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        "st_nested_aggregate.takt",
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
        .arg(st_dir.join("st_nested_aggregate.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал порождённый ST:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let prints = AGG_OBSERVED
        .iter()
        .map(|(_, path)| format!(r#"        printf("%lld ", (long long)fb.{path}.value);"#))
        .collect::<Vec<_>>()
        .join("\n");
    let harness = format!(
        r#"#include <stdio.h>
#include "iec_std_lib.h"
TIME __CURRENT_TIME;
BOOL __DEBUG = 0;
#include "POUS.h"
#include "POUS.c"

int main(void) {{
    {AGG_ROOT}_data__ fb = {{0}};
    {AGG_ROOT}_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {TICKS}; i++) {{
        {AGG_ROOT}_body__(&fb);
{prints}
        printf("\n");
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");

    let bin = work.join("st_nested_aggregate_bin");
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
        "порождённый ST (через iec2c) не собирается:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&bin).output().expect("запуск драйвера ST");
    assert!(run.status.success(), "драйвер ST завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            l.split_whitespace()
                .map(|v| v.parse().expect("число в трассе"))
                .collect()
        })
        .collect()
}

/// Вложенный агрегат: значения совпадают такт в такт.
///
/// До цель отвергала обе записи (`ST-011`): раскрытие агрегата было одноуровневым.
/// Проверяются значения, а не факт компиляции: путь к листу, собранный неверно
/// (перепутанные поле и индекс), даёт валидный ST с другими числами.
#[test]
fn st_nested_aggregate_trace_matches_reference() {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c не установлен — сверка пропущена (см. scripts/ensure-iec2c.sh)");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка пропущена");
        return;
    }

    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!(
            "takt_st_nested_aggregate_{}",
            std::thread::current()
                .name()
                .unwrap_or("main")
                .replace(':', "_")
        ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("рабочий каталог");

    let reference = aggregate_simulate_trace();
    assert_eq!(
        reference[0],
        vec![1, 4, 7],
        "эталон первого такта: n = 1, a = pts[1].y = 4, b = cells[1][0] = 7"
    );
    let target = aggregate_st_trace(&dir, &iec2c, &lib);
    assert_eq!(
        target.len(),
        reference.len(),
        "длина трасс:\nэталон {reference:?}\nst     {target:?}"
    );
    for (tick, (r, t)) in reference.iter().zip(target.iter()).enumerate() {
        assert_eq!(
            t,
            r,
            "такт {} разошёлся.\nнаблюдаемые: {:?}\nэталон: {reference:?}\nst:     {target:?}",
            tick + 1,
            AGG_OBSERVED.iter().map(|(n, _)| *n).collect::<Vec<_>>()
        );
    }

    let _ = std::fs::remove_dir_all(&dir);
}

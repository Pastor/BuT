//! Потактовая сверка табличной формы автомата у цели `st`.
//!
//! # Что доказывает набор
//!
//! Форма `--fsm=table` печатает переходы данными - четырьмя константными массивами и
//! диспетчером `WHILE`. Поведение при этом обязано остаться тем же: трассы эталона,
//! формы `CASE` и формы `table` совпадают **скан в скан**.
//!
//! `iec2c` этого не видит: он проверяет валидность, а не верность. Устройство сверки то
//! же, что у `conformance_st_per_tick_tests`: `taktc -t st` -> `iec2c` -> драйвер
//! печатает наблюдаемое каждый скан.
//!
//! **Момент выхода из состояния-цепочки** - главное, что здесь ловится: обе формы
//! обязаны уводить состояние в том же скане, в котором завершился последний шаг, и обе
//! обязаны совпасть с эталоном. `iec2c` принимал и сдвинутый вывод: он проверяет
//! валидность, а не верность.

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::generator::{FsmForm, GenerateOptions};
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Value, build_unit};

const SIMPLE_FIXTURE: &str = "tests/data/eval/conformance_fsm_table.takt";
const SIMPLE_UNIT: &str = "conformance_fsm_table";
const SIMPLE_ROOT: &str = "CONFORMANCEFSMTABLE";
const SIMPLE_TICKS: usize = 8;
/// Наблюдаемое: `(имя порта в эталоне, путь поля в структуре POUS)`.
const SIMPLE_OBSERVED: &[(&str, &str)] = &[("probe", "WORKER0.PROBE")];

const CHAIN_FIXTURE: &str = "tests/data/eval/conformance_fsm_table_chain.takt";
const CHAIN_UNIT: &str = "conformance_fsm_table_chain";
const CHAIN_ROOT: &str = "CONFORMANCEFSMTABLECHAIN";
const CHAIN_TICKS: usize = 9;
const CHAIN_OBSERVED: &[(&str, &str)] = &[
    ("first_probe", "LINE0.CHAIN_FIRST0.FIRST_PROBE"),
    ("second_probe", "LINE0.CHAIN_SECOND1.SECOND_PROBE"),
    ("line_probe", "LINE0.LINE_PROBE"),
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

/// Каталог сборки, уникальный по потоку И процессу.
fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0440_st_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог сборки");
    dir
}

fn source(fixture: &str) -> String {
    std::fs::read_to_string(fixture).expect("фикстура читается")
}

/// Трасса эталона по наблюдаемым портам.
fn simulator_trace(fixture: &str, observed: &[(&str, &str)], ticks: usize) -> Vec<Vec<i128>> {
    let (ast, _) = takt_lang::parse(&source(fixture), 0).expect("разбор фикстуры");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение Unit");
    let mut trace = Vec::new();
    let mut reg = vec![0i128; observed.len()];
    for _ in 0..ticks {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "эталон не обрывает прогон: {result:?}"
        );
        for (slot, (port, _)) in reg.iter_mut().zip(observed) {
            match unit.variable(port) {
                Some(Value::Number(v)) => *slot = v,
                Some(Value::Boolean(b)) => *slot = i128::from(b),
                _ => {}
            }
        }
        trace.push(reg.clone());
    }
    trace
}

/// Трасса цели `st` заданной формы: `iec2c` -> `cc` -> прогон.
fn st_trace(
    dir: &Path,
    (iec2c, lib): (&Path, &Path),
    fixture: &str,
    (unit, root): (&str, &str),
    observed: &[(&str, &str)],
    ticks: usize,
    fsm: FsmForm,
) -> Vec<Vec<i128>> {
    let mut options = GenerateOptions::default();
    options.fsm = fsm;
    let st_dir = dir.join("st");
    std::fs::create_dir_all(&st_dir).expect("каталог ST");
    takt_lang::compile_to_st(
        unit,
        &source(fixture),
        st_dir.to_str().expect("путь в UTF-8"),
        &[],
        &options,
    )
    .expect("порождение ST");

    let work = dir.join("iec2c");
    std::fs::create_dir_all(&work).expect("рабочий каталог iec2c");
    let transpile = Command::new(iec2c)
        .arg("-I")
        .arg(lib)
        .arg(st_dir.join(format!("{unit}.st")))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал порождённый ST:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    let prints = observed
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
    {root}_data__ fb = {{0}};
    {root}_init__(&fb, __BOOL_LITERAL(FALSE));
    for (int i = 0; i < {ticks}; i++) {{
        {root}_body__(&fb);
{prints}
        printf("\n");
    }}
    return 0;
}}
"#
    );
    let harness_path = work.join("harness.c");
    std::fs::write(&harness_path, harness).expect("запись драйвера");

    let bin = work.join("st_table_bin");
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

/// Простой автомат: условное ребро, `enter`/`exit`, два конкурирующих ребра.
#[test]
fn st_table_form_matches_case_form_and_simulator() {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c недоступен — сверка пропущена");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка пропущена");
        return;
    }
    let expected = simulator_trace(SIMPLE_FIXTURE, SIMPLE_OBSERVED, SIMPLE_TICKS);
    let case = st_trace(
        &build_dir("simple_case"),
        (&iec2c, &lib),
        SIMPLE_FIXTURE,
        (SIMPLE_UNIT, SIMPLE_ROOT),
        SIMPLE_OBSERVED,
        SIMPLE_TICKS,
        FsmForm::Switch,
    );
    let table = st_trace(
        &build_dir("simple_table"),
        (&iec2c, &lib),
        SIMPLE_FIXTURE,
        (SIMPLE_UNIT, SIMPLE_ROOT),
        SIMPLE_OBSERVED,
        SIMPLE_TICKS,
        FsmForm::Table,
    );
    assert_eq!(case, expected, "форма CASE разошлась с эталоном");
    assert_eq!(table, expected, "форма table разошлась с эталоном");
    let flat: Vec<i128> = expected.iter().flatten().copied().collect();
    assert!(
        flat.contains(&100) && flat.contains(&200),
        "трасса не наблюдает блоки enter/exit: {expected:?}"
    );
}

/// Последовательная композиция: момент выхода из цепочки сохранён защёлкой.
#[test]
fn st_table_form_matches_case_form_on_chain() {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c недоступен — сверка пропущена");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка пропущена");
        return;
    }
    let expected = simulator_trace(CHAIN_FIXTURE, CHAIN_OBSERVED, CHAIN_TICKS);
    let case = st_trace(
        &build_dir("chain_case"),
        (&iec2c, &lib),
        CHAIN_FIXTURE,
        (CHAIN_UNIT, CHAIN_ROOT),
        CHAIN_OBSERVED,
        CHAIN_TICKS,
        FsmForm::Switch,
    );
    let table = st_trace(
        &build_dir("chain_table"),
        (&iec2c, &lib),
        CHAIN_FIXTURE,
        (CHAIN_UNIT, CHAIN_ROOT),
        CHAIN_OBSERVED,
        CHAIN_TICKS,
        FsmForm::Table,
    );
    assert_eq!(table, case, "формы CASE и table разошлись на цепочке");
    // И с эталоном тоже: до цель тратила лишний скан на выходе из цепочки (машина шагов
    // уходила наружу из отдельной ветви `CASE`, то есть скан спустя), и трасса ST
    // отличалась сдвигом. Теперь последний шаг уводит состояние в том же скане - как
    // эталон и цель `c`.
    assert_eq!(case, expected, "форма CASE разошлась с эталоном");
    let flat: Vec<i128> = case.iter().flatten().copied().collect();
    assert!(
        flat.contains(&12) && flat.contains(&22),
        "трасса не наблюдает оба шага цепочки: {case:?}"
    );
}

//! Потактовая сверка цели `st` с эталоном.
//!
//! ## Что доказывает фикстура
//!
//! `data/eval/st_per_tick.takt` устроена так, что оба расхождения проявляются трассой:
//!
//! - **вложенность >= 2** (`Main = Blinker`): пока `INIT` был ветвью `CASE`,
//!   трасса ST начиналась с двух нулей;
//! - **переход по `mask = 255`** над переменной типа `[bit;8]`: потеряй цель
//!   `st` инициализатор (а она теряла), условие не выполнится никогда и автомат
//!   застрянет - трасса разойдётся с эталоном.
//!
//! ## Устройство
//!
//! `taktc -t st` -> `iec2c` транслирует ST в C (`POUS.c`) -> драйвер печатает
//! наблюдаемое **каждый скан** -> трасса сверяется с трассой симулятора. Нет
//! `iec2c`/`cc`/заголовков MatIEC -> тест-пропуск, а не красный: инструмент пакетом не
//! поставляется (`scripts/ensure-iec2c.sh`).

use std::path::{Path, PathBuf};
use std::process::Command;

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

const FIXTURE: &str = "tests/data/eval/st_per_tick.takt";

/// Имя корневой модели в C-символах `iec2c`: идентификаторы IEC регистронезависимы, и
/// `iec2c` печатает их в верхнем регистре.
const ROOT: &str = "STPERTICK";

/// Тактов в трассе. Больше, чем нужно автомату (он завершается на пятом), - хвост тоже
/// обязан совпасть: расхождение любит прятаться после завершения.
const TICKS: usize = 6;

/// Наблюдаемые точки: `(имя в симуляторе, путь поля в структуре POUS)`.
///
/// Путь задан именами экземпляров MatIEC (`BLINKER0` - модель + порядковый номер) и
/// полями в верхнем регистре. Оба детерминированы (генерация 0048 + правила именования
/// `iec2c`).
const OBSERVED: &[(&str, &str)] = &[("n", "BLINKER0.N"), ("mask", "BLINKER0.MASK")];

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

/// Трасса эталона: значение наблюдаемых **после каждого такта**.
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
        // Завершение такт не обрывает: значения после него тоже сверяются - цель
        // обязана держать их так же, как эталон.
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
        "st_per_tick.takt",
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
        .arg(st_dir.join("st_per_tick.st"))
        .current_dir(&work)
        .output()
        .expect("запуск iec2c");
    assert!(
        transpile.status.success() && work.join("POUS.c").is_file(),
        "iec2c не оттранслировал порождённый ST:\n{}",
        String::from_utf8_lossy(&transpile.stderr)
    );

    // Печать идёт внутри цикла - в этом вся разница с прежней сверкой.
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

    let bin = work.join("st_per_tick_bin");
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

/// Трассы эталона и цели `st` совпадают **такт в такт**.
///
/// Именно потактовое сравнение, а не сверка установившихся значений: до установившиеся
/// значения совпадали, а трасса ST была сдвинута на два скана (замер: `0 0 8 8 8 ...`
/// против `8 ...`).
#[test]
fn st_trace_matches_reference_tick_by_tick() {
    let Some((iec2c, lib)) = iec2c_available() else {
        eprintln!("iec2c не установлен — сверка пропущена (см. scripts/ensure-iec2c.sh)");
        return;
    };
    if !cc_available() {
        eprintln!("cc недоступен — сверка пропущена");
        return;
    }

    let dir = std::env::temp_dir().join(format!("takt_st_per_tick_{}", std::process::id()));
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

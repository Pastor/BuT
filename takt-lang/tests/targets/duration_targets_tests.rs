//! Тип `duration` в целях `st` и `sv`.
//!
//! Проверяются **текст** вывода (единица представления видна только так) и
//! **валидность** у настоящего инструмента цели (`iec2c`, `verilator`). Потактовые
//! сверки значений живут в `takt-sim/tests/conformance_{c,rust}_duration_tests.rs`
//! для целей `c` и `rust`; для `st`/`sv` они заводятся отдельными подзадачами -
//! их механизм наблюдения (трансляция в C через MatIEC, тестбенч RTL) требует
//! своей обвязки.
//!
//! Валидность != верность: `iec2c` и `verilator` принимают и молча неверный код.
//! Поэтому текст проверяется на **конкретные числа миллисекунд**, а не на
//! "скомпилировалось".

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::GenerateOptions;

/// Фикстура сверки значений: `elapsed := pause + 750ms`, приведение и сравнение.
const FIXTURE: &str = "../takt-sim/tests/data/eval/conformance_duration_value.takt";

/// Фикстура для цели `sv`: без приведения `as` - цель его не транслирует вовсе
/// (`SV-002`, ограничение цели, к этой фиче отношения не имеющее). Значение наблюдается
/// **тремя сравнениями**: `>= 1750` и `< 1751` вместе означают ровно 1750 мс, то есть
/// бит-порты кодируют число не хуже, чем вывод целого.
const SV_SOURCE: &str = r#"
model Timers {
    var pause: duration := 1s;
    var elapsed: duration := 0s;

    out above500: bit;
    out atleast1750: bit;
    out atleast1751: bit;

    start Counting {
        always {
            elapsed := pause + 750ms;
            if (elapsed > 500ms) { above500 := 1; } else { above500 := 0; }
            if (elapsed >= 1750ms) { atleast1750 := 1; } else { atleast1750 := 0; }
            if (elapsed >= 1751ms) { atleast1751 := 1; } else { atleast1751 := 0; }
        }
        ref Counting: false;
    }
}

start Main = Timers;
"#;

fn tmp(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0183_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

fn tool_available(cmd: &str, probe: &str) -> bool {
    Command::new(cmd)
        .arg(probe)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Цель `st`: длительность - `UDINT` в миллисекундах, и `iec2c` это принимает.
#[test]
fn st_emits_duration_as_udint_milliseconds() {
    let dir = tmp("st_duration");
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_st(
        "duration_value",
        &source,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение ST");
    let code = std::fs::read_to_string(dir.join("duration_value.st")).expect("порождённый .st");

    // `UDINT` - беззнаковое 32-битное целое IEC. Тип `TIME` намеренно не выбран:
    // арифметика над ним в MatIEC ограничена, а приведение `duration ↔ число` обязано
    // быть бесплатным.
    assert!(
        code.contains("elapsed : UDINT := 0;") && code.contains("pause : UDINT := 1000;"),
        "длительность обязана быть UDINT в миллисекундах:\n{code}"
    );
    // Инициализатор длительности терялся **молча** (переменная объявлялась нулём,
    // эталон давал 1000 мс): ветвь `Duration` в `literal_init` отсутствовала, и ни
    // `iec2c`, ни проверка этого не видели. Тест - проверка на конкретное число.
    assert!(
        code.contains("elapsed := pause + (750);"),
        "750ms обязаны напечататься как 750:\n{code}"
    );
    // Приведение `as` бесплатно: слева тот же операнд, без умножений и делений.
    assert!(
        code.contains("ms := elapsed;"),
        "приведение обязано быть тождественным:\n{code}"
    );
    for forbidden in ["1000000", "/ 1000", "* 1000"] {
        assert!(
            !code.contains(forbidden),
            "в выводе нет места пересчёту единиц ('{forbidden}'):\n{code}"
        );
    }

    assert_st_valid(&dir, "duration_value");
}

/// Цель `sv`: длительность - беззнаковый вектор в миллисекундах, и `verilator` это
/// принимает.
#[test]
fn sv_emits_duration_as_unsigned_vector_milliseconds() {
    let dir = tmp("sv_duration");
    takt_lang::compile_to_sv(
        "duration_value",
        SV_SOURCE,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение SV");
    let code = std::fs::read_to_string(dir.join("duration_value.sv")).expect("порождённый .sv");

    assert!(
        code.contains("logic [31:0] duration_value_timers_elapsed"),
        "длительность обязана быть 32-битным беззнаковым вектором:\n{code}"
    );
    assert!(
        code.contains("1000"),
        "1s обязана дать 1000 мс в ветви сброса:\n{code}"
    );
    assert!(
        code.contains("+ 750)"),
        "750ms обязаны напечататься как 750:\n{code}"
    );
    for forbidden in ["1000000", "/ 1000", "* 1000"] {
        assert!(
            !code.contains(forbidden),
            "в выводе нет места пересчёту единиц ('{forbidden}'):\n{code}"
        );
    }

    if !tool_available("verilator", "--version") {
        eprintln!("[ПРОПУСК] verilator недоступен — SV не проверен линтом");
        return;
    }
    let out = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("duration_value.sv"))
        .output()
        .expect("запуск verilator");
    assert!(
        out.status.success(),
        "порождённый SV со значениями duration не проходит линт:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Прогоняет `iec2c` по порождённому ST (мягкий пропуск, если недоступен).
fn assert_st_valid(dir: &Path, name: &str) {
    let iec2c = std::env::var("HOME")
        .map(|h| PathBuf::from(h).join(".local/bin/iec2c"))
        .unwrap_or_else(|_| PathBuf::from("iec2c"));
    let lib = std::env::var("HOME")
        .map(|h| PathBuf::from(h).join(".local/share/matiec/lib"))
        .unwrap_or_default();
    if !iec2c.exists() && !tool_available("iec2c", "-h") {
        eprintln!("[ПРОПУСК] iec2c недоступен — ST не проверен арбитром");
        return;
    }
    let out = Command::new(&iec2c)
        .arg("-I")
        .arg(&lib)
        .arg("-T")
        .arg(dir)
        .arg(dir.join(format!("{name}.st")))
        .output()
        .expect("запуск iec2c");
    assert!(
        out.status.success(),
        "порождённый ST со значениями duration не принят iec2c:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

// -- Вычисляемая выдержка: границы ----------------

/// Модель с вычисляемой выдержкой (`after (base + 500ms)`).
const DYNAMIC: &str = r#"
model Timer {
    var base: duration := 2s;
    out done: bit;
    start Waiting { ref Ready: after (base + 500ms); }
    state Ready { enter { done := 1; } }
}
start Main = Timer;
"#;

/// Частота, не кратная 1000 Гц, отвергается `SE-073`: множитель `hertz / 1000` не цел,
/// и сравнение округлялось бы **молча**.
#[test]
fn dynamic_dwell_refuses_frequency_not_multiple_of_kilohertz() {
    let dir = tmp("dynamic_3hz");
    let mut options = GenerateOptions::default();
    options.tick_hz = Some(3);
    let error =
        takt_lang::compile_to_c("timer", DYNAMIC, dir.to_str().expect("путь"), &[], &options)
            .expect_err("частота 3 Гц обязана быть отвергнута");
    // Код приходит причиной внутри диагностики кодогена - проверяется текстом, потому
    // что цель оборачивает причину в свою `CC-018`.
    let text = format!("{error:?}");
    assert!(
        text.contains("SE-073") || error.notes.iter().any(|n| n.message.contains("SE-073")),
        "ожидался SE-073 о частоте, получено: {text}"
    );
}

/// Частота, кратная 1000 Гц, принимается, и множитель виден в выводе.
#[test]
fn dynamic_dwell_multiplies_milliseconds_by_ticks_per_milli() {
    let dir = tmp("dynamic_2khz");
    let mut options = GenerateOptions::default();
    options.tick_hz = Some(2_000);
    takt_lang::compile_to_c("timer", DYNAMIC, dir.to_str().expect("путь"), &[], &options)
        .expect("частота 2 кГц обязана приниматься");
    let code = std::fs::read_to_string(dir.join("timer.c")).expect("порождённый .c");
    assert!(
        code.contains("* 2"),
        "при 2 кГц миллисекунды обязаны умножаться на 2 такта:\n{code}"
    );
}

/// Цель `st` в профиле "часы" вычисляемую выдержку пока не поддерживает -
/// **громко** (`ST-016`), а не молча иной выдержкой: переменный `PT` таймера
/// `TON` требует своей обвязки.
#[test]
fn st_refuses_dynamic_dwell_in_clock_profile() {
    let dir = tmp("dynamic_st_clock");
    let error = takt_lang::compile_to_st(
        "timer",
        DYNAMIC,
        dir.to_str().expect("путь"),
        &[],
        &GenerateOptions::default(),
    )
    .expect_err("профиль «часы» обязан быть отвергнут");
    let text = format!("{error:?}");
    assert!(
        text.contains("ST-016") || error.notes.iter().any(|n| n.message.contains("ST-016")),
        "ожидался ST-016, получено: {text}"
    );
}

//! Потактовая сверка **значений** типа `duration` цели `c` с эталоном.
//!
//! Отдельный файл, а не дополнение `conformance_c_time_tests.rs`: тот уже 421 строка и
//! посвящён **выдержке** (номеру такта срабатывания), а здесь предмет другой - само
//! значение длительности. Граница честная: представление эталона - наносекунды,
//! представление целей - **миллисекунды**, и ошибка на этой границе была бы молчаливой
//! (`250us` -> 0 мс).
//!
//! Сверяются **значения**, а не факт компиляции: проверка `cc` доказывает, что код
//! собирается, но не что он считает то же.

use std::path::Path;
use std::process::Command;

/// Фикстура: `elapsed := pause + 750ms`, `ms := elapsed as u32`, `late := elapsed >
/// 500ms`.
const FIXTURE: &str = "tests/data/eval/conformance_duration_value.takt";

/// Доступен ли компилятор C (иначе сверка мягко пропускается).
fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Значения `(ms, late)` у эталона после первого такта.
fn simulator_values() -> (i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    let _ = unit.tick();
    let number = |name: &str| match unit.variable(name) {
        Some(takt_sim::Value::Number(v)) => v,
        other => panic!("порт '{name}' обязан быть числом, получено {other:?}"),
    };
    (number("ms"), number("late"))
}

/// Значения `(ms, late)` у порождённого C после первого такта.
fn generated_c_values(dir: &Path) -> (i128, i128) {
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_duration_value",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    // Харнесс печатает то, что модель записала в порты: значение длительности видно
    // снаружи только через них.
    let harness = r#"#include <stdio.h>
#include "conformance_duration_value.h"

static unsigned long ms_value = 0;
static int late_value = 0;

static void wr_num(ConformanceDurationValue_Out_NumericPort port, uint8_t index, int64_t v, void *ud) {
    (void)index;
    (void)port;
    (void)ud;
    ms_value = (unsigned long)v;
}

static void wr_bit(ConformanceDurationValue_Out_BitPort port, uint8_t bit, bool v, void *ud) {
    (void)bit;
    (void)port;
    (void)ud;
    late_value = v;
}

int main(void) {
    ConformanceDurationValue m = {0};
    m.write_numeric = wr_num;
    m.write_bit = wr_bit;
    ConformanceDurationValue_init(&m);
    ConformanceDurationValue_tick(&m);
    printf("ms=%lu late=%d\n", ms_value, late_value);
    return 0;
}
"#;
    let harness_path = dir.join("harness_duration.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("conformance_duration_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join("conformance_duration_value.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C со значениями duration не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    let out = String::from_utf8_lossy(&run.stdout);
    let mut ms = None;
    let mut late = None;
    for token in out.split_whitespace() {
        if let Some(v) = token.strip_prefix("ms=") {
            ms = v.parse::<i128>().ok();
        }
        if let Some(v) = token.strip_prefix("late=") {
            late = v.parse::<i128>().ok();
        }
    }
    (
        ms.expect("харнесс печатает ms"),
        late.expect("харнесс печатает late"),
    )
}

/// Арифметика, сравнение и приведение длительности дают у эталона и у цели `c`
/// **одни и те же** значения.
#[test]
fn duration_values_match_simulator_and_generated_c() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] duration_values_match_simulator_and_generated_c: `cc` не найден");
        return;
    }
    let dir = tempfile::tempdir().expect("временный каталог");
    let reference = simulator_values();
    let generated = generated_c_values(dir.path());
    assert_eq!(
        reference, generated,
        "значения обязаны совпадать: эталон {reference:?}, цель C {generated:?}"
    );
    // Числа из модели: 1s + 750ms = 1750 мс, и это больше 500 мс. Проверяются явно -
    // иначе сверка "ноль против нуля" была бы зелёной и бессмысленной.
    assert_eq!(
        reference,
        (1_750, 1),
        "ожидались ms=1750 и late=1, получено {reference:?}"
    );
}

/// Приведение `as` **не порождает арифметики**: единица представления та же
/// (миллисекунды), поэтому в выводе стоит одно имя переменной.
///
/// Проверяется текстом, а не поведением: деление на 1000000, вставленное "на всякий
/// случай", прошло бы сверку значений при `elapsed = 0` и провалилось бы на живой
/// модели.
///
/// **Прежде здесь требовался текст `(uint32_t)model->elapsed`, и это утверждение
/// перестало быть верным**: `duration` отображается в `uint32_t`, то есть приведение
/// совпадает с типом операнда после отображения и больше не печатается. У цели `rust`
/// та же печать была отказом проверки (`clippy::unnecessary_cast`). Предмет проверки -
/// отсутствие пересчёта единиц - остался прежним.
#[test]
fn cast_between_duration_and_number_emits_no_arithmetic() {
    let dir = tempfile::tempdir().expect("временный каталог");
    let source = std::fs::read_to_string(FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_duration_value",
        &source,
        dir.path().to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");
    let code = std::fs::read_to_string(dir.path().join("conformance_duration_value.c"))
        .expect("порождённый .c");
    assert!(
        code.contains("model->elapsed"),
        "значение обязано читаться напрямую:\n{code}"
    );
    assert!(
        !code.contains("(uint32_t)model->elapsed"),
        "приведение к тому же напечатанному типу не печатается (фича 0374):\n{code}"
    );
    for forbidden in ["1000000", "/ 1000", "* 1000"] {
        assert!(
            !code.contains(forbidden),
            "в выводе не должно быть пересчёта единиц ('{forbidden}'):\n{code}"
        );
    }
}

// -- Вычисляемая выдержка -------------------------

/// Фикстура: `after (base + 2ms)` при `base := 3ms` и 1 кГц - пять тактов.
const DYNAMIC_FIXTURE: &str = "tests/data/eval/conformance_dynamic_dwell.takt";

/// На каком такте `done` впервые стал единицей - у эталона.
fn simulator_dynamic_tick() -> usize {
    let source = std::fs::read_to_string(DYNAMIC_FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    for step in 0..12i32 {
        // 1 мс на такт - та же частота, что объявлена моделью.
        unit.set_time_ns(i64::from(step) * 1_000_000);
        let _ = unit.tick();
        if unit.variable("done") == Some(takt_sim::Value::Number(1)) {
            return usize::try_from(step + 1).expect("номер такта");
        }
    }
    panic!("эталон обязан снять вычисляемую выдержку за 12 тактов");
}

/// На каком такте `done` впервые стал единицей - у порождённого C.
fn generated_c_dynamic_tick(dir: &Path) -> usize {
    let source = std::fs::read_to_string(DYNAMIC_FIXTURE).expect("фикстура читается");
    let mut options = takt_lang::generator::GenerateOptions::default();
    options.tick_hz = Some(1_000);
    takt_lang::compile_to_c(
        "conformance_dynamic_dwell",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options,
    )
    .expect("порождение C");

    let harness = r#"#include <stdio.h>
#include "conformance_dynamic_dwell.h"

static int done = 0;
static void wr(ConformanceDynamicDwell_Out_BitPort port, uint8_t bit, bool v, void *ud) {
    (void)bit;
    (void)port;
    (void)ud;
    done = v;
}

int main(void) {
    ConformanceDynamicDwell m = {0};
    m.write_bit = wr;
    ConformanceDynamicDwell_init(&m);
    for (int tick = 1; tick <= 12; tick++) {
        ConformanceDynamicDwell_tick(&m);
        if (done) { printf("tick=%d\n", tick); return 0; }
    }
    printf("tick=0\n");
    return 0;
}
"#;
    let harness_path = dir.join("harness_dynamic.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("conformance_dynamic_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join("conformance_dynamic_dwell.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C с вычисляемой выдержкой не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .find_map(|line| line.strip_prefix("tick=")?.trim().parse::<usize>().ok())
        .expect("харнесс печатает номер такта")
}

/// Вычисляемая выдержка срабатывает на **одном и том же** такте у эталона и у
/// порождённого C.
///
/// Порог здесь не известен компилятору: он складывается из переменной и литерала уже во
/// время работы. Именно поэтому сверяется такт, а не текст: и пересчёт "миллисекунды ->
/// такты", и ширина счётчика могли бы разойтись молча.
#[test]
fn dynamic_dwell_fires_on_the_same_tick_in_simulator_and_generated_c() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] dynamic_dwell_fires_on_the_same_tick...: `cc` не найден");
        return;
    }
    let dir = tempfile::tempdir().expect("временный каталог");
    let reference = simulator_dynamic_tick();
    let generated = generated_c_dynamic_tick(dir.path());
    assert_eq!(
        reference, generated,
        "вычисляемая выдержка обязана срабатывать на одном такте: эталон {reference}, цель C {generated}"
    );
    // 3ms + 2ms = 5 мс при 1 кГц - пять тактов от входа; вход занимает такт 1 (время на
    // нём ноль), поэтому срабатывание - такт 6.
    assert_eq!(reference, 6, "ожидался такт 6, получено {reference}");
}

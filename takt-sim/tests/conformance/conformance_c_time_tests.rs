//! Потактовая сверка выдержки `after` цели `c` с эталоном.
//!
//! Отдельный файл, а не дополнение `conformance_c_tests.rs`: тот вместе с этой сверкой
//! давал 1072 строки при лимите 1000, и проверка размера отказал. Заодно граница честная -
//! время самостоятельная тема.
//!
//! Сверяется **номер такта** срабатывания, а не факт перехода: сдвиг на один такт
//! компилируется молча. Эталон и цель заданы одной частотой (1 кГц) - сверка идёт
//! **внутри** профиля времени.

use std::path::Path;
use std::process::Command;

/// Доступен ли компилятор C (иначе сверка мягко пропускается - как у ).
fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

// Сверяется **номер такта** срабатывания выдержки, а не факт перехода: сдвиг на
// один такт - тот класс дефекта, который компилируется молча.
// главного капкана цели `sv`). Модельное время эталона и счётчик тактов цели
// заданы одной частотой (1 кГц), поэтому сравнение осмысленно: сверка идёт
// **внутри** профиля.

/// Фикстура: выход из состояния через 5 мс при 1 кГц (5 тактов от входа).
const AFTER_FIXTURE: &str = "tests/data/eval/conformance_after.takt";

/// На каком такте выходной порт `done` впервые стал единицей - у эталона.
fn simulator_dwell_tick() -> usize {
    let source = std::fs::read_to_string(AFTER_FIXTURE).expect("фикстура читается");
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
    panic!("эталон обязан снять выдержку за 12 тактов");
}

/// На каком такте `done` впервые стал единицей - у порождённого C.
fn generated_c_dwell_tick(dir: &Path) -> usize {
    let source = std::fs::read_to_string(AFTER_FIXTURE).expect("фикстура читается");
    // Фикстура объявляет `clock 1kHz` -> контракт частоты требует подтверждающий
    // `--tick-hz`; здесь - совпадающая частота.
    let mut options = takt_lang::generator::GenerateOptions::default();
    options.tick_hz = Some(1_000);
    takt_lang::compile_to_c(
        "conformance_after",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &options,
    )
    .expect("порождение C");

    let harness = r#"#include <stdio.h>
#include "conformance_after.h"

static int done = 0;
static void wr(ConformanceAfter_Out_BitPort port, uint8_t bit, bool v, void *ud) {
    (void)bit;
    (void)port;
    (void)ud;
    done = v;
}

int main(void) {
    /* Имя корневой структуры берётся из имени файла: корневая модель анонимна. */
    ConformanceAfter m = {0};
    m.write_bit = wr;
    ConformanceAfter_init(&m);
    for (int tick = 1; tick <= 12; tick++) {
        ConformanceAfter_tick(&m);
        if (done) { printf("tick=%d\n", tick); return 0; }
    }
    printf("tick=0\n");
    return 0;
}
"#;
    let harness_path = dir.join("harness_after.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("conformance_after_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join("conformance_after.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C с выдержкой не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .find_map(|line| line.strip_prefix("tick=")?.trim().parse::<usize>().ok())
        .expect("харнесс печатает номер такта")
}

#[test]
fn after_fires_on_the_same_tick_in_simulator_and_generated_c() {
    if !cc_available() {
        eprintln!(
            "[ПРОПУСК] after_fires_on_the_same_tick_in_simulator_and_generated_c: \
             компилятор `cc` не найден"
        );
        return;
    }
    let dir = tempfile::tempdir().expect("временный каталог");
    let reference = simulator_dwell_tick();
    let generated = generated_c_dwell_tick(dir.path());
    assert_eq!(
        reference, generated,
        "выдержка обязана срабатывать на одном такте: эталон {reference}, цель C {generated}"
    );
    // Число из модели: 5 мс при 1 кГц - пять тактов от входа в стартовое состояние, то
    // есть такт 6 (вход занимает такт 1, время на нём - ноль).
    assert_eq!(reference, 6, "ожидался такт 6, получено {reference}");
}

/// Компилирует исходник целью `c` с частотой и возвращает текст заголовка.
fn header_with_tick_hz(source: &str, tick_hz: Option<u64>) -> String {
    let dir = tempfile::tempdir().expect("временный каталог");
    let mut options = takt_lang::generator::GenerateOptions::default();
    options.tick_hz = tick_hz;
    takt_lang::compile_to_c(
        "fp",
        source,
        dir.path().to_str().expect("путь в UTF-8"),
        &[],
        &options,
    )
    .expect("порождение C");
    std::fs::read_to_string(dir.path().join("fp.h")).expect("заголовок читается")
}

/// Отпечаток контракта частоты: модель с `clock` порождает статическое утверждение в
/// заголовке - несовпадение частоты ловится при сборке прошивки.
#[test]
fn generated_c_carries_clock_contract_static_assert() {
    let source = "model Fp {\n    clock 1kHz;\n    out done: bit;\n    \
                  start Waiting { ref Ready: after 5ms; }\n    state Ready { enter { done := 1; } }\n}\n\
                  start Main = Fp;\n";
    let header = header_with_tick_hz(source, Some(1_000));
    assert!(
        header.contains("#define TAKT_REQUIRED_CLOCK_HZ 1000u"),
        "заголовок обязан закрепить объявленную частоту:\n{header}"
    );
    assert!(
        header.contains("_Static_assert(TAKT_TICK_HZ == TAKT_REQUIRED_CLOCK_HZ,"),
        "заголовок обязан нести статическое утверждение частоты:\n{header}"
    );
}

/// Контрпример: без объявления `clock` (частоту задал лишь `--tick-hz`) отпечатка нет -
/// автор частоту не ограничивал, закреплять контракт нечего.
#[test]
fn tick_hz_without_clock_declaration_emits_no_contract() {
    let source = "model Fp {\n    out done: bit;\n    \
                  start Waiting { ref Ready: after 5t; }\n    state Ready { enter { done := 1; } }\n}\n\
                  start Main = Fp;\n";
    let header = header_with_tick_hz(source, Some(1_000));
    assert!(
        !header.contains("TAKT_REQUIRED_CLOCK_HZ"),
        "без объявления clock отпечатка контракта быть не должно:\n{header}"
    );
}

// -- Профиль "часы": внешний источник времени `now_ms` ---------

const CLOCK_FIXTURE: &str = "tests/data/eval/conformance_after_clock.takt";

/// Номер такта срабатывания у эталона в профиле "часы" (нет объявления `clock`).
fn simulator_dwell_tick_clock() -> usize {
    let source = std::fs::read_to_string(CLOCK_FIXTURE).expect("фикстура читается");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор фикстуры");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = takt_sim::build_unit(model).expect("построение Unit");
    for step in 0..12i32 {
        // 1 мс на такт - эталон меряет выдержку модельным временем.
        unit.set_time_ns(i64::from(step) * 1_000_000);
        let _ = unit.tick();
        if unit.variable("done") == Some(takt_sim::Value::Number(1)) {
            return usize::try_from(step + 1).expect("номер такта");
        }
    }
    panic!("эталон обязан снять выдержку за 12 тактов");
}

/// Номер такта срабатывания у порождённого C в профиле "часы".
///
/// Драйвер подставляет фиктивный `now_ms`, возвращающий модельное время (1 мс на такт), -
/// как эталон. Профиль "часы" получается тем, что модель не объявляет `clock`, а
/// `--tick-hz` не передаётся (`tick_hz: None`).
fn generated_c_dwell_tick_clock(dir: &Path) -> usize {
    let source = std::fs::read_to_string(CLOCK_FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_after_clock",
        &source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    let harness = r#"#include <stdio.h>
#include "conformance_after_clock.h"

static uint64_t fake_now = 0;
static uint64_t clk(void *ud) { (void)ud; return fake_now; }

static int done = 0;
static void wr(ConformanceAfterClock_Out_BitPort port, uint8_t bit, bool v, void *ud) {
    (void)bit;
    (void)port;
    (void)ud;
    done = v;
}

int main(void) {
    ConformanceAfterClock m = {0};
    m.write_bit = wr;
    m.now_ms = clk;
    /* Вход стартового состояния - "до такта 1": метка латчится в _init. */
    fake_now = 0;
    ConformanceAfterClock_init(&m);
    for (int tick = 1; tick <= 12; tick++) {
        fake_now = (uint64_t)(tick - 1); /* 1 мс на такт, начиная с нуля */
        ConformanceAfterClock_tick(&m);
        if (done) { printf("tick=%d\n", tick); return 0; }
    }
    printf("tick=0\n");
    return 0;
}
"#;
    let harness_path = dir.join("harness_clock.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");

    let bin = dir.join("conformance_after_clock_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(dir)
        .arg(dir.join("conformance_after_clock.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "порождённый C профиля «часы» не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск собранного C");
    assert!(run.status.success(), "собранный C завершился с ошибкой");
    String::from_utf8_lossy(&run.stdout)
        .lines()
        .find_map(|line| line.strip_prefix("tick=")?.trim().parse::<usize>().ok())
        .expect("харнесс печатает номер такта")
}

#[test]
fn after_clock_profile_fires_on_the_same_tick_in_simulator_and_generated_c() {
    if !cc_available() {
        eprintln!(
            "[ПРОПУСК] after_clock_profile_fires_on_the_same_tick_in_simulator_and_generated_c: \
             компилятор `cc` не найден"
        );
        return;
    }
    let dir = tempfile::tempdir().expect("временный каталог");
    let reference = simulator_dwell_tick_clock();
    let generated = generated_c_dwell_tick_clock(dir.path());
    assert_eq!(
        reference, generated,
        "выдержка «часы» обязана срабатывать на одном такте: эталон {reference}, цель C {generated}"
    );
    // 5 мс при 1 мс/такт - пять тактов от входа в стартовое состояние (такт 6: вход
    // занимает такт 1, время на нём - ноль).
    assert_eq!(reference, 6, "ожидался такт 6, получено {reference}");
}

/// Тест формулы разности: выдержка **через границу переполнения**
/// счётчика метки. Метка `uint8_t` (D_MS <= 5 -> 8 бит), поэтому `now_ms` у границы 2⁸
/// проверяет, что сравнение идёт разностью, а не `t0 + D`.
#[test]
fn after_clock_survives_counter_wraparound() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] after_clock_survives_counter_wraparound: `cc` не найден");
        return;
    }
    let dir = tempfile::tempdir().expect("временный каталог");
    let source = std::fs::read_to_string(CLOCK_FIXTURE).expect("фикстура читается");
    takt_lang::compile_to_c(
        "conformance_after_clock",
        &source,
        dir.path().to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение C");

    // Вход при now=254; метка uint8 = 254. Через 5 мс now проходит 255->0->...->3
    // (обёртка). Разность (uint8)(3 - 254) = 5 -> выдержка снимается ровно на 5-м мс,
    // несмотря на переполнение. Формула `t0 + D <= now` дала бы здесь ложь.
    let harness = r#"#include <stdio.h>
#include "conformance_after_clock.h"
static uint64_t fake_now = 0;
static uint64_t clk(void *ud) { (void)ud; return fake_now; }
static int done = 0;
static void wr(ConformanceAfterClock_Out_BitPort port, uint8_t bit, bool v, void *ud) {
    (void)bit;
    (void)port; (void)ud; done = v;
}
int main(void) {
    ConformanceAfterClock m = {0};
    m.write_bit = wr;
    m.now_ms = clk;
    fake_now = 254;
    ConformanceAfterClock_init(&m);
    for (int step = 0; step <= 12; step++) {
        fake_now = (uint64_t)(254 + step); /* обёртка uint8 на 256 */
        ConformanceAfterClock_tick(&m);
        if (done) { printf("fire=%d\n", step); return 0; }
    }
    printf("fire=-1\n");
    return 0;
}
"#;
    let harness_path = dir.path().join("harness_wrap.c");
    std::fs::write(&harness_path, harness).expect("запись харнесса");
    let bin = dir.path().join("wrap_bin");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Werror", "-I"])
        .arg(dir.path())
        .arg(dir.path().join("conformance_after_clock.c"))
        .arg(&harness_path)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "C не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&bin).output().expect("запуск C");
    let out = String::from_utf8_lossy(&run.stdout);
    let fire: i32 = out
        .lines()
        .find_map(|l| l.strip_prefix("fire=")?.trim().parse().ok())
        .expect("харнесс печатает шаг срабатывания");
    // Вход отмечается на шаге 0 (now=254), выдержка 5 мс снимается на шаге 5 - через
    // границу 256, доказывая корректность разностной формулы.
    assert_eq!(
        fire, 5,
        "выдержка обязана сняться на 5-м мс несмотря на обёртку"
    );
}

/// Цель `c-hal` в профиле "часы": умолчательный `now_ms` через `clock_gettime`
/// компилируется. Флаги - как у проверки c-hal (`cc -std=c11 -c`, без `-Werror`:
/// `bind_default_hal` статична и в самом TU не зовётся).
#[test]
fn c_hal_clock_profile_default_now_ms_compiles() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] c_hal_clock_profile_default_now_ms_compiles: `cc` не найден");
        return;
    }
    let dir = tempfile::tempdir().expect("временный каталог");
    // Порт с inline-адресом - иначе c-hal даёт SE-052 (used-порт без адреса).
    let source = "model HalDwell {\n    out done: bit at 0x40000000:0;\n    \
                  start Waiting { ref Ready: after 5ms; }\n    state Ready { enter { done := 1; } }\n}\n\
                  start Main = HalDwell;\n";
    takt_lang::compile_to_c_hal(
        "haldwell",
        source,
        dir.path().to_str().expect("путь в UTF-8"),
        &[],
        &[],
        &takt_lang::address_map::AddressEnv::default(),
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("порождение c-hal");

    let header = std::fs::read_to_string(dir.path().join("haldwell.h")).expect("заголовок");
    assert!(
        header.contains("clock_gettime(CLOCK_MONOTONIC")
            && header.contains("#define _POSIX_C_SOURCE"),
        "c-hal профиля «часы» обязан нести дефолтный now_ms через clock_gettime:\n{header}"
    );

    // Флаги проверки c-hal: без -Werror (static bind_default_hal не зовётся в TU).
    let compile = Command::new("cc")
        .args(["-std=c11", "-I"])
        .arg(dir.path())
        .arg("-c")
        .arg(dir.path().join("haldwell.c"))
        .arg("-o")
        .arg(dir.path().join("haldwell.o"))
        .output()
        .expect("запуск cc");
    assert!(
        compile.status.success(),
        "c-hal профиля «часы» не компилируется:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
}

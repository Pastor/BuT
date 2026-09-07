//! Тесты цели `sv-mmio` на уровне `compile_to_sv_mmio`.
//!
//! Проверяют карту регистров, направление по биту, порт без адреса, приём внешней карты
//! и диагностики-"не угадываем" (SE-060, SV-013, SV-014). Проверка (verilator + yosys) и
//! потактовая сверка - в `precheck.sh` и
//! `takt-sim/tests/conformance/conformance_sv_mmio_tests.rs`.

use std::path::PathBuf;
use takt_lang::generator::GenerateOptions;

/// Каталог сборки под тест (тесты идут однопоточно).
fn out_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_sv_mmio_test_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Компилирует исходник целью `sv-mmio` и возвращает текст модуля.
fn compile(tag: &str, source: &str) -> String {
    let dir = out_dir(tag);
    let env = takt_lang::parse_defines(&[]).expect("env");
    takt_lang::compile_to_sv_mmio(
        tag,
        source,
        dir.to_str().unwrap(),
        &[],
        &[],
        &env,
        &GenerateOptions::default(),
    )
    .unwrap_or_else(|d| panic!("sv-mmio не собрался: {:?}", d));
    std::fs::read_to_string(dir.join(format!("{tag}.sv"))).expect("вывод .sv")
}

/// Компилирует и возвращает ошибку (для негативных тестов).
fn compile_err(tag: &str, source: &str) -> takt_lang::diagnostics::Diagnostic {
    let dir = out_dir(tag);
    let env = takt_lang::parse_defines(&[]).expect("env");
    takt_lang::compile_to_sv_mmio(
        tag,
        source,
        dir.to_str().unwrap(),
        &[],
        &[],
        &env,
        &GenerateOptions::default(),
    )
    .expect_err("ожидалась ошибка")
}

/// **A1:** адресованные порты дают регистровый интерфейс и мультиплексор чтения
/// по их адресам.
#[test]
fn addressed_ports_form_register_file() {
    let sv = compile(
        "regmap",
        "out cmd_fork: bit at 0x500:0; in task_valid: bit at 0x100:0; \
         start S { always { cmd_fork := task_valid; } ref S; }",
    );
    // Регистровый интерфейс в заголовке.
    assert!(
        sv.contains("input  logic [10:0] reg_addr,"),
        "нет reg_addr:\n{sv}"
    );
    assert!(sv.contains("reg_wen,"), "нет reg_wen:\n{sv}");
    assert!(
        sv.contains("output logic [0:0] reg_rdata,"),
        "нет reg_rdata:\n{sv}"
    );
    // Адреса портов буквально - карта регистров.
    assert!(sv.contains("11'h500"), "нет адреса cmd_fork 0x500:\n{sv}");
    assert!(sv.contains("11'h100"), "нет адреса task_valid 0x100:\n{sv}");
    // Адресованные порты не порты модуля.
    assert!(
        !sv.contains("output logic cmd_fork"),
        "cmd_fork остался портом:\n{sv}"
    );
    assert!(
        !sv.contains("input  logic task_valid"),
        "task_valid остался портом:\n{sv}"
    );
}

/// **A7:** порт **без** адреса остаётся портом модуля; **с** адресом - бит регистра.
#[test]
fn port_without_address_stays_module_port() {
    // `plain_in` без размещения -> адреса нет; `reg_out at 0x200:0` - адрес. (после
    // адрес задаётся только `at`, а `:=` означает начальное значение, поэтому "порт без
    // адреса" - это порт без `at`.)
    let sv = compile(
        "mixport",
        "in plain_in: bit; out reg_out: bit at 0x200:0; \
         start S { always { reg_out := plain_in; } ref S; }",
    );
    // Неадресованный вход - порт модуля.
    assert!(
        sv.contains("input  logic plain_in,"),
        "plain_in не порт модуля:\n{sv}"
    );
    // Адресованный выход - не порт модуля, а бит регистра.
    assert!(
        !sv.contains("output logic reg_out"),
        "reg_out остался портом:\n{sv}"
    );
    assert!(sv.contains("'h200"), "нет адреса reg_out:\n{sv}");
}

/// **A5:** одно слово несёт биты обоих направлений - читаются все, пишется
/// только `in` (-5 ). `extend_complex` без `extern fn`.
#[test]
fn mixed_direction_word_reads_all_writes_only_in() {
    let sv = compile(
        "mixedword",
        "out flag_a: bit at 0x40:1; out flag_b: bit at 0x40:2; in gate: bit at 0x40:33; \
         start S { always { flag_a := gate; flag_b := 1; } ref S; }",
    );
    // Чтение собирает все три бита одного слова.
    assert!(
        sv.contains("reg_rdata[1 +: 1] = flag_a;"),
        "нет чтения flag_a:\n{sv}"
    );
    assert!(
        sv.contains("reg_rdata[2 +: 1] = flag_b;"),
        "нет чтения flag_b:\n{sv}"
    );
    assert!(
        sv.contains("reg_rdata[33 +: 1] = gate;"),
        "нет чтения gate:\n{sv}"
    );
    // Запись касается только in-бита gate; out-биты запись игнорируют.
    assert!(
        sv.contains("gate <= reg_wdata[33 +: 1];"),
        "нет записи gate:\n{sv}"
    );
    assert!(
        !sv.contains("flag_a <= reg_wdata"),
        "out-бит flag_a пишется шиной (R5 нарушен):\n{sv}"
    );
}

/// **T12 (не угадываем):** бит адреса вне `[0, 63]` -> `SE-060` (не молчаливый
/// выбор ширины слова).
#[test]
fn bit_out_of_range_is_se060_not_guessed() {
    let err = compile_err(
        "bit64",
        "out sig: bit at 0x1:64; start S { always { sig := 1; } ref S; }",
    );
    assert_eq!(
        err.code.as_deref(),
        Some("SE-060"),
        "ожидался SE-060: {err:?}"
    );
}

/// **SV-013 (не угадываем):** срез порта не помещается в 64-битный регистр.
#[test]
fn slice_over_64_is_sv013() {
    // u8 (8 бит) на бите 60 -> биты [60..67], выход за 64.
    let err = compile_err(
        "slice64",
        "out sig: u8 at 0x1:60; start S { always { sig := 1; } ref S; }",
    );
    assert_eq!(
        err.code.as_deref(),
        Some("SV-013"),
        "ожидался SV-013: {err:?}"
    );
}

/// **SV-014:** имя, совпавшее с сигналом регистрового интерфейса.
#[test]
fn reg_interface_name_collision_is_sv014() {
    let err = compile_err(
        "collide",
        "out reg_addr: bit at 0x1:0; start S { always { reg_addr := 1; } ref S; }",
    );
    assert_eq!(
        err.code.as_deref(),
        Some("SV-014"),
        "ожидался SV-014: {err:?}"
    );
}

/// **T9/A8:** внешняя карта адресов (`--address-map`) принимается целью `sv-mmio`
/// и переопределяет адрес модели (SE-050 - предупреждение наложения, не ошибка).
#[test]
fn external_address_map_is_accepted() {
    let dir = out_dir("extmap");
    let env = takt_lang::parse_defines(&[]).expect("env");
    // Карта переопределяет адрес sig с 0x1 на 0x2A.
    let external = takt_lang::parse_address_map("sig = 0x2A:0;", 0).expect("карта");
    let warnings = takt_lang::compile_to_sv_mmio(
        "extmap",
        "out sig: bit at 0x1:0; start S { always { sig := 1; } ref S; }",
        dir.to_str().unwrap(),
        &[],
        &external,
        &env,
        &GenerateOptions::default(),
    )
    .expect("sv-mmio с картой");
    let sv = std::fs::read_to_string(dir.join("extmap.sv")).expect("вывод");
    // Победил адрес карты (0x2A), а не модели (0x1).
    assert!(sv.contains("'h2a"), "адрес карты не применён:\n{sv}");
    // Наложение отмечен предупреждением SE-050 (карта поверх адреса модели).
    assert!(
        warnings.iter().any(|w| w.code.as_deref() == Some("SE-050")),
        "нет предупреждения оверлея SE-050: {warnings:?}"
    );
}

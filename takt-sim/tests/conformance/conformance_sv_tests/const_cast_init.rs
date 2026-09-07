//! Вычислимое приведение в инициализаторе - эталон == `sv`.
//!
//! # Что проверяется
//!
//! `var v := 5 as u16;` цель `sv` отвергала (`SV-002`), тогда как `var v: u16 := 5;` и
//! `var v: u16 := 2 + 3;` принимала: разницу делает свёртка инициализаторов, которая приведения
//! намеренно не берёт. Отказ шёл по **виду узла**, а не по вычислимости.

use super::*;

/// Инициализатор - приведение, значение помещается в целевой тип.
const SRC: &str = "model Probe {\n\
    \x20   var v := 5 as u16;\n\
    \x20   var n: u8 := 0;\n\n\
    \x20   start Run {\n\
    \x20       always { n := n + 1; v := v + 1; }\n\
    \x20       ref Done: n = 3;\n\
    \x20   }\n\n\
    \x20   state Done;\n\
    }\n\n\
    start Main = Probe;\n";

#[test]
fn computable_cast_initializer_matches_generated_sv() {
    if !verilator_available() {
        eprintln!("verilator недоступен — сверка `sv` пропущена");
        return;
    }
    let dir = build_dir("const_cast_init");
    let path = fixture(&dir, "castinit", SRC);

    let expected = simulate_trace(path.to_str().expect("путь в UTF-8"), &["Probe::v"]);
    let actual = sv_trace(
        &dir,
        path.to_str().expect("путь в UTF-8"),
        "castinit",
        &["castinit_probe_v"],
        4,
    );
    let common: Vec<Vec<i128>> = actual.iter().take(expected.len()).cloned().collect();
    assert_eq!(
        common, expected,
        "трасса SystemVerilog разошлась с эталоном:\nsv     = {actual:?}\nэталон = {expected:?}"
    );
    assert!(
        expected.first().is_some_and(|row| row[0] == 6),
        "контроль: на первом такте значение обязано быть 6 (5 из инициализатора + 1): {expected:?}"
    );
}

/// **Граница сдвинулась: приведение, меняющее значение, вычисляется.**
///
/// Правило целочисленного приведения (обёртка беззнакового, ошибка знакового - )
/// переехало в общий носитель `takt_lang::semantic::const_eval::int_cast`, и эталон
/// зовёт **его же**.
///
/// Проверяется **значение** в выводе, а не отсутствие отказа: "цель научилась" иначе
/// означало бы лишь "перестала отказывать".
#[test]
fn value_changing_cast_is_folded() {
    let src = SRC.replace("5 as u16", "300 as u8");
    let dir = build_dir("const_cast_wrap");
    takt_lang::compile_to_sv(
        "castinit",
        &src,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect("приведение с обёрткой вычисляется при компиляции");
    let text = std::fs::read_to_string(dir.join("castinit.sv")).expect("чтение модуля");
    assert!(
        text.contains("44"),
        "обёрнутое значение 300 mod 256 = 44 обязано доехать до ветви сброса:\n{text}"
    );
}

/// **Контроль:** знаковое переполнение приведения - ошибка, а не молчаливое
/// значение.
///
/// Знаковое переполнение есть ошибка программы, и в C это неопределённое поведение.
#[test]
fn signed_overflow_cast_is_refused() {
    let src = SRC.replace("5 as u16", "300 as i8");
    let err = takt_lang::compile_to_sv(
        "castinit",
        &src,
        std::env::temp_dir()
            .join(format!("takt_pid{}", std::process::id()))
            .to_str()
            .expect("путь в UTF-8"),
        &[],
        &takt_lang::generator::GenerateOptions::default(),
    )
    .expect_err("знаковое переполнение обязано отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-121"), "{}", err.message);
}

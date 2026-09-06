//! Сравнение операндов разной знаковости у четырёх целей.
//!
//! # Что тестым
//!
//! Форму вывода у четырёх целей на обоих случаях (расширение и раскрытие
//! проверкой знака), приём вывода настоящими инструментами и - главное -
//! **контрпример**: сравнение с литералом остаётся прежним.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Смешение на 8 битах: общий тип есть (`i16`).
const MIX8: &str = "var s: i8 := -1; var u: u8 := 200; var res: u8 := 0; out o: u8 at 0x100; \
                    start Run { always { if s < u { res := 1; } else { res := 2; } o := res; } \
                    next Done; } state Done { }";

/// Смешение на 64 битах: общего типа нет - раскрытие проверкой знака.
const MIX64: &str = "var s: i64 := -1; var u: u64 := 200; var res: u8 := 0; out o: u8 at 0x100; \
                     start Run { always { if s < u { res := 1; } else { res := 2; } o := res; } \
                     next Done; } state Done { }";

/// **Контрпример:** сравнение переменной с литералом правилом не затронуто.
const LITERAL: &str = "var n: u8 := 0; out o: u8 at 0x100; \
                       start Run { always { n := n + 1; o := n; } ref Done: n >= 3; } \
                       state Done { }";

fn dir_for(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0359_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

fn generate(tag: &str, target: &str, source: &str) -> (std::path::PathBuf, String) {
    let dir = dir_for(&format!("{tag}_{target}"));
    let path = dir.to_str().expect("путь");
    let opts = GenerateOptions::default();
    let name = "probe";
    match target {
        "c" => takt_lang::compile_to_c(name, source, path, &[], &opts).map(|_| ()),
        "st" => takt_lang::compile_to_st(name, source, path, &[], &opts).map(|_| ()),
        "rust" => takt_lang::compile_to_rust(name, source, path, &[], &opts).map(|_| ()),
        _ => takt_lang::compile_to_sv(name, source, path, &[], &opts).map(|_| ()),
    }
    .unwrap_or_else(|e| panic!("порождение для '{target}': {e:?}"));
    let ext = match target {
        "c" => "c",
        "st" => "st",
        "rust" => "rs",
        _ => "sv",
    };
    let text = std::fs::read_to_string(dir.join(format!("{name}.{ext}"))).expect("чтение");
    (dir, text)
}

// -- Форма вывода -------------------------------------------------------------

/// На 8 битах операнды расширяются до типа, вмещающего оба.
#[test]
fn eight_bit_mix_widens_in_rust_st_sv() {
    let (_d, rust) = generate("mix8", "rust", MIX8);
    assert!(
        rust.contains("as i16") && rust.contains("<"),
        "цель `rust` обязана расширить оба операнда: смешанное сравнение там \
         не компилируется вовсе (E0308).\n{rust}"
    );
    let (_d, st) = generate("mix8", "st", MIX8);
    assert!(
        st.contains("SINT_TO_INT") && st.contains("USINT_TO_INT"),
        "цель `st`: `iec2c` отвергает сравнение разных типов.\n{st}"
    );
    let (_d, sv) = generate("mix8", "sv", MIX8);
    assert!(
        sv.contains("$signed(16'("),
        "цель `sv`: смешанное сравнение приводит операнды к беззнаковым — \
         нужно явное расширение со знаком.\n{sv}"
    );
}

/// **`c` на 8 битах не трогается:** продвижение до `int` уже даёт верный ответ.
///
/// Без этой проверки правка изменила бы вывод всего корпуса без нужды.
#[test]
fn eight_bit_mix_is_untouched_in_c() {
    let (_d, c) = generate("mix8", "c", MIX8);
    assert!(
        c.contains("model->s < model->u"),
        "на 8 битах C сравнивает верно сам: лишнее приведение изменило бы вывод \
         корпуса.\n{c}"
    );
}

/// На 64 битах общего типа нет - раскрытие проверкой знака у всех четырёх.
#[test]
fn sixty_four_bit_mix_expands_to_sign_guard() {
    for (target, marker) in [
        ("c", "(uint64_t)"),
        ("rust", "as u64"),
        ("st", "LINT_TO_ULINT"),
        ("sv", "$unsigned"),
    ] {
        let (_d, text) = generate("mix64", target, MIX64);
        assert!(
            text.contains(marker) && text.contains("< 0"),
            "цель '{target}': знакового 65-битного типа нет, правило \
             раскрывается проверкой знака (ожидался маркер `{marker}`).\n{text}"
        );
    }
}

/// Равенство и неравенство покрыты тем же правилом.
///
/// Первая редакция фичи их **не покрыла**: `sv` считал `-1 == 255` истиной (проверено
/// прогоном verilator), `rust` и `st` не собирались.
#[test]
fn equality_is_covered_too() {
    const EQ: &str = "var s: i8 := -1; var u: u8 := 255; var res: u8 := 0; out o: u8 at 0x100; \
                      start Run { always { if s = u { res := 1; } else { res := 2; } o := res; } \
                      next Done; } state Done { }";
    let (_d, rust) = generate("eq", "rust", EQ);
    assert!(
        rust.contains("as i16"),
        "равенство обязано выравнивать знаковость так же, как `<`.\n{rust}"
    );
    let (_d, st) = generate("eq", "st", EQ);
    assert!(st.contains("SINT_TO_INT"), "то же у цели `st`.\n{st}");
    let (_d, sv) = generate("eq", "sv", EQ);
    assert!(sv.contains("$signed(16'("), "то же у цели `sv`.\n{sv}");
}

/// **Контрпример:** сравнение с литералом остаётся прежним.
///
/// У литерала знаковости нет - он подстраивается под приёмник. Приведение `3 as i32`
/// есть `clippy::unnecessary_cast`, то есть **отказ** проверки цели `rust`; класс поймал
/// чужой тест (`rust_default_impl_tests`).
#[test]
fn comparison_with_literal_is_untouched() {
    let (_d, rust) = generate("lit", "rust", LITERAL);
    assert!(
        !rust.contains("as i32") && !rust.contains("as i16"),
        "литерал знаковости не имеет — приведения быть не должно.\n{rust}"
    );
    let (_d, st) = generate("lit", "st", LITERAL);
    assert!(!st.contains("USINT_TO_"), "то же у цели `st`.\n{st}");
}

// -- Настоящие инструменты ----------------------------------------------------

fn tool_available(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Вывод цели `c` принимается `cc -Wall -Wextra -Werror` на обоих случаях.
///
/// На 64 битах прежний вывод давал `-Wsign-compare`, то есть проверка цели уже краснел бы -
/// если бы такой вход попал в корпус.
#[test]
fn c_output_passes_werror() {
    if !tool_available("cc") {
        eprintln!("[ПРОПУСК] c_output_passes_werror: `cc` не найден");
        return;
    }
    for (tag, source) in [("mix8", MIX8), ("mix64", MIX64)] {
        let (dir, _) = generate(tag, "c", source);
        let out = Command::new("cc")
            .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c"])
            .arg("-I")
            .arg(&dir)
            .arg(dir.join("probe.c"))
            .arg("-o")
            .arg(dir.join("probe.o"))
            .output()
            .expect("запуск cc");
        assert!(
            out.status.success(),
            "вывод цели `c` для '{tag}' обязан приниматься тем же гейтом, что в \
             precheck:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}

/// Вывод цели `rust` принимается `clippy -D warnings` на обоих случаях.
#[test]
fn rust_output_passes_clippy() {
    if !tool_available("clippy-driver") {
        eprintln!("[ПРОПУСК] rust_output_passes_clippy: `clippy-driver` не найден");
        return;
    }
    for (tag, source) in [("mix8", MIX8), ("mix64", MIX64)] {
        let (dir, _) = generate(tag, "rust", source);
        let out = Command::new("clippy-driver")
            .args(["--edition", "2021", "--crate-type=lib", "-D", "warnings"])
            .arg(dir.join("probe.rs"))
            .arg("--out-dir")
            .arg(dir.join("out"))
            .output()
            .expect("запуск clippy-driver");
        assert!(
            out.status.success(),
            "вывод цели `rust` для '{tag}' обязан собираться:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}

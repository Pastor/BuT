//! Печать широкого числового литерала целями `c` и `sv`.
//!
//! # Что доказывается
//!
//! Носитель значения расширен до `i128`, и в вывод впервые могут попасть числа больше
//! `i64::MAX`. Голое десятичное для них **невалидно** в двух целях, и обе ловушки тихие
//! ровно до проверки:
//!
//! - цель `c`: `uint64_t x = 18446744073709551615;` -> clang отвечает
//!   `-Wimplicitly-unsigned-literal`, а проверка идёт под `-Werror`;
//! - цель `sv`: нетипизированная десятичная константа знаковая и не уже 32 бит,
//!   поэтому значение больше `i32::MAX` даёт verilator `WIDTHEXPAND` -
//!   предупреждение, которое проверка считает ошибкой. Порог здесь **33 бита**, а
//!   не 64: дефект существовал и до 0157, корпус его просто не содержал.
//!
//! # Устройство теста
//!
//! Два слоя, как в тестах 0174: **эмиссия** (текст вывода - быстро и точно) и
//! **проверка** (`cc -Wall -Werror`, `verilator --lint-only -Wall` - доказывает, что
//! тест ловит настоящее правило инструмента, а не наше представление о нём).
//! Нет инструмента -> второй слой пропускается с сообщением.
//!
//! Отдельный тест держит **обратную совместимость**: всё, что укладывается в 32-битное
//! знаковое, обязано печататься как прежде, иначе поедут снапшоты
//! `examples/generated/`.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Широкое значение, читаемое в условии: годится для проверок `st` и `rust`.
///
/// Без чтения `rustc -D warnings` отверг бы вывод за "присваивание поля самому себе" -
/// то есть за конструкцию исправлениетуры, а не за литерал.
const WIDE_READ: &str = "model M { \
                         var top: u64 := 18446744073709551615; \
                         var seen: u8 := 0; \
                         start S { always { seen := 1; } ref Done: top > 0; } \
                         state Done; \
                         } start Root = M;";

/// Маска `[bit;64]` и максимум `u64` - значения, невыразимые до 0157.
const WIDE: &str = "model M { \
                    var mask: [bit;64] := 0xFFFFFFFFFFFFFFFF; \
                    var top: u64 := 18446744073709551615; \
                    start S { always { mask := mask; top := top; } } \
                    } start Root = M;";

/// 33-битное и 32-битное значения: порог `WIDTHEXPAND` у цели `sv`.
const MID: &str = "model M { \
                   var big: u64 := 8589934592; \
                   var full32: u32 := 4294967295; \
                   start S { always { big := big; full32 := full32; } } \
                   } start Root = M;";

/// Обычные значения: их печать обязана остаться прежней.
const NARROW: &str = "model M { \
                      var a: u8 := 42; \
                      var b: i32 := -7; \
                      start S { always { a := a; b := b; } } \
                      } start Root = M;";

fn build_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0157_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Порождает C и возвращает `(каталог, текст .c)`.
fn generate_c(tag: &str, source: &str) -> (std::path::PathBuf, String) {
    let dir = build_dir(&format!("c_{tag}"));
    takt_lang::compile_to_c(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.c"))).expect("чтение .c");
    (dir, text)
}

/// Порождает SystemVerilog и возвращает `(каталог, текст .sv)`.
fn generate_sv(tag: &str, source: &str) -> (std::path::PathBuf, String) {
    let dir = build_dir(&format!("sv_{tag}"));
    takt_lang::compile_to_sv(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение SystemVerilog");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.sv"))).expect("чтение .sv");
    (dir, text)
}

// -- Слой 1: эмиссия ----------------------------------------------------------

/// Цель `c`: значение выше `LLONG_MAX` печатается с суфисправлением `ULL`.
#[test]
fn c_prints_unsigned_suffix_beyond_signed_range() {
    let (_dir, text) = generate_c("wide", WIDE);
    assert!(
        text.contains("18446744073709551615ULL"),
        "значение выше LLONG_MAX обязано нести суффикс: без него clang отвечает \
         -Wimplicitly-unsigned-literal, а гейт идёт под -Werror.\n{text}"
    );
}

/// Цель `c`: обычные значения печатаются как раньше (обратная совместимость).
#[test]
fn c_keeps_narrow_literals_bare() {
    let (_dir, text) = generate_c("narrow", NARROW);
    assert!(
        text.contains("= 42;"),
        "узкое значение печатается голым\n{text}"
    );
    assert!(text.contains("= -7;"), "отрицательное — тоже\n{text}");
    assert!(
        !text.contains("ULL"),
        "суффикс появляется ТОЛЬКО там, где иначе вывод невалиден — иначе \
         поедут снапшоты examples/generated.\n{text}"
    );
}

/// Цель `sv`: широкое значение печатается размерной формой по ширине приёмника.
#[test]
fn sv_prints_sized_literal_by_target_width() {
    let (_dir, text) = generate_sv("wide", WIDE);
    assert!(
        text.contains("64'd18446744073709551615"),
        "маска [bit;64] обязана печататься размерной формой шириной приёмника\n{text}"
    );
}

/// Цель `sv`: ширина берётся у **приёмника**, а не у значения.
///
/// 33-битное значение в 64-битном регистре обязано дать `64'd...`: `32'd...` или
/// собственная ширина значения дали бы тот же `WIDTHEXPAND`.
#[test]
fn sv_sized_literal_width_follows_receiver_not_value() {
    let (_dir, text) = generate_sv("mid", MID);
    assert!(
        text.contains("64'd8589934592"),
        "33-битное значение в u64 → ширина 64\n{text}"
    );
    assert!(
        text.contains("32'd4294967295"),
        "32-битное значение в u32 → ширина 32\n{text}"
    );
}

/// Цель `sv`: узкие литералы печатаются как раньше.
#[test]
fn sv_keeps_narrow_literals_bare() {
    let (_dir, text) = generate_sv("narrow", NARROW);
    assert!(
        !text.contains("'d42"),
        "узкое значение печатается голым — размерная форма только по нужде\n{text}"
    );
}

// -- Слой 2: те же проверки, что в precheck.sh -----------------------------------

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn verilator_available() -> bool {
    Command::new("verilator")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Порождённый C принимается `cc -Wall -Werror` - как в проверке.
#[test]
fn generated_c_compiles_under_werror() {
    if !cc_available() {
        eprintln!("[ПРОПУСК] generated_c_compiles_under_werror: `cc` не найден");
        return;
    }
    let (dir, _) = generate_c("gate", WIDE);
    let out = Command::new("cc")
        .args(["-Wall", "-Werror", "-std=c11", "-c"])
        .arg(dir.join("gate.c"))
        .arg("-I")
        .arg(&dir)
        .arg("-o")
        .arg(dir.join("gate.o"))
        .output()
        .expect("запуск cc");
    assert!(
        out.status.success(),
        "порождённый C с широким литералом отвергнут гейтом:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Порождённый SystemVerilog принимается `verilator --lint-only -Wall`.
///
/// Проверяются **оба** порога: 64-битная маска и 33-битное значение - второй и был тем
/// дефектом, который существовал до 0157 незамеченным.
#[test]
fn generated_sv_passes_verilator_lint() {
    if !verilator_available() {
        eprintln!("[ПРОПУСК] generated_sv_passes_verilator_lint: `verilator` не найден");
        return;
    }
    for (tag, source) in [("gatewide", WIDE), ("gatemid", MID)] {
        let (dir, _) = generate_sv(tag, source);
        let out = Command::new("verilator")
            .args(["--lint-only", "-Wall"])
            .arg(dir.join(format!("{tag}.sv")))
            .output()
            .expect("запуск verilator");
        assert!(
            out.status.success(),
            "порождённый SV ({tag}) отвергнут линтом:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}

/// Порождает Structured Text и возвращает `(каталог, текст .st)`.
fn generate_st(tag: &str, source: &str) -> (std::path::PathBuf, String) {
    let dir = build_dir(&format!("st_{tag}"));
    takt_lang::compile_to_st(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение ST");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.st"))).expect("чтение .st");
    (dir, text)
}

/// Порождает Rust и возвращает `(каталог, текст .rs)`.
fn generate_rust(tag: &str, source: &str) -> (std::path::PathBuf, String) {
    let dir = build_dir(&format!("rust_{tag}"));
    takt_lang::compile_to_rust(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text = std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение .rs");
    (dir, text)
}

/// Цели `st` и `rust` печатают широкое значение **как есть** - и это верно.
///
/// Особой формы этим целям не нужно, и тест закрепляет именно это: "ничего не делаем" -
/// решение, а не пробел.
#[test]
fn st_and_rust_print_wide_literal_plainly() {
    let (_dir, st) = generate_st("plainst", WIDE_READ);
    assert!(
        st.contains("ULINT := 18446744073709551615"),
        "ST печатает значение как есть\n{st}"
    );
    let (_dir, rs) = generate_rust("plainrs", WIDE_READ);
    assert!(
        rs.contains("18446744073709551615"),
        "Rust печатает значение как есть\n{rs}"
    );
}

fn iec2c_path() -> Option<std::path::PathBuf> {
    let path = dirs_home()?.join(".local/bin/iec2c");
    path.exists().then_some(path)
}

fn dirs_home() -> Option<std::path::PathBuf> {
    std::env::var_os("HOME").map(std::path::PathBuf::from)
}

/// Порождённый ST принимается `iec2c` - как в проверке `precheck.sh`.
#[test]
fn generated_st_is_accepted_by_iec2c() {
    let Some(iec2c) = iec2c_path() else {
        eprintln!("[ПРОПУСК] generated_st_is_accepted_by_iec2c: `iec2c` не собран");
        return;
    };
    let home = dirs_home().expect("HOME");
    let (dir, _) = generate_st("gatest", WIDE_READ);
    let out_dir = dir.join("iec");
    std::fs::create_dir_all(&out_dir).expect("каталог вывода iec2c");
    let out = Command::new(iec2c)
        .arg("-I")
        .arg(home.join(".local/share/matiec/lib"))
        .arg("-T")
        .arg(&out_dir)
        .arg(dir.join("gatest.st"))
        .output()
        .expect("запуск iec2c");
    assert!(
        out.status.success(),
        "порождённый ST с широким литералом отвергнут iec2c:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

fn rustc_available() -> bool {
    Command::new("rustc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Порождённый Rust принимается `rustc -D warnings` - как в проверке.
#[test]
fn generated_rust_compiles_under_deny_warnings() {
    if !rustc_available() {
        eprintln!("[ПРОПУСК] generated_rust_compiles_under_deny_warnings: `rustc` не найден");
        return;
    }
    let (dir, text) = generate_rust("gaters", WIDE_READ);
    // Обёртка с `#![no_std]` в корне повторяет проверка `precheck.sh`: атрибут допустим
    // только в корне крейта.
    let wrapper = dir.join("lib.rs");
    std::fs::write(&wrapper, format!("#![no_std]\n{text}")).expect("запись обёртки");
    let out = Command::new("rustc")
        .args(["--crate-type=lib", "-D", "warnings"])
        .arg(&wrapper)
        .arg("--out-dir")
        .arg(&dir)
        .output()
        .expect("запуск rustc");
    assert!(
        out.status.success(),
        "порождённый Rust с широким литералом отвергнут гейтом:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

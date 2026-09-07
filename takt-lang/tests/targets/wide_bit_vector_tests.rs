//! Бит-вектор шире 64 бит в целях `c` и `rust`.
//!
//! # Что здесь проверяется
//!
//! `[bit;N]` при `N > 64` представлен массивом слов (`uint64_t[K]`, `[u64; K]`) -
//! правило единого слоя `semantic::bit_vector`. Цели звали его только при печати
//! **типа**, а операции печатали так, будто носитель скаляр.
//!
//! | Инструмент | Ответ до фичи |
//! |---|---|
//! | `cc -std=c11 -Wall -Wextra -Werror` | `array type 'uint64_t[2]' is not assignable`, `invalid operands` x2, `shift count >= width of type` |
//! | `rustc --edition 2021` | `E0308` x2, `E0368`, `E0369` |
//! | эталон, цель `sv` | исполняют верно |
//! | цель `st` | честный отказ `ST-011` |
//!
//! Код возврата `taktc` при этом был **нулевым**: об ошибке узнавал не автор модели, а
//! сборка прошивки.
//!
//! # Два слоя проверки
//!
//! 1. **эмиссия** - форма вывода (какое слово, какое смещение);
//! 2. **настоящий инструмент** - `cc` и `clippy-driver -D warnings`, те же, что
//!    в проверках предкоммита. Первый слой без второго доказывает лишь, что текст
//!    такой, как задумано; второй без первого - что "что-то собралось".
//!
//! **Контрольный вход обязателен.** Маска разряда печаталась литералом `1u` (32 бита),
//! и разряд >= 32 не собирался **и у обычного `u64`**, где никакого широкого вектора
//! нет. Этот вход стоит здесь наравне с широким.
//!
//! **Корпус класс не покрывает:** векторов шире 64 бит в `examples/` нет ни одного,
//! поэтому оба проверки молчали и молчали бы дальше.

use std::path::PathBuf;
use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Широкий вектор: инициализация, запись и чтение разряда в разных словах.
const WIDE: &str = "var w: [bit;96] := 0;\n\
                    var seen: u8 := 0;\n\
                    start Run {\n\
                        always {\n\
                            w.5 := 1;\n\
                            w.70 := 1;\n\
                            if w.70 { seen := 1; }\n\
                        }\n\
                        ref Run;\n\
                    }\n";

/// Копирование вектора в вектор - форма, которую эталон исполняет.
const COPY: &str = "var w: [bit;96] := 0;\n\
                    var x: [bit;96] := 0;\n\
                    var seen: u8 := 0;\n\
                    start Run {\n\
                        always {\n\
                            x.70 := 1;\n\
                            w := x;\n\
                            if w.70 { seen := 1; }\n\
                        }\n\
                        ref Run;\n\
                    }\n";

/// Контроль: разряд >= 32 у обычного `u64` - широкого вектора здесь нет.
const NARROW_HIGH_BIT: &str = "var v: u64 := 0;\n\
                               var seen: u8 := 0;\n\
                               start Run {\n\
                                   always {\n\
                                       v.35 := 1;\n\
                                       if v.35 { seen := 1; }\n\
                                   }\n\
                                   ref Run;\n\
                               }\n";

/// Арифметика над широким вектором: её не поддерживает и эталон (`SIM-005`).
const ARITH: &str = "var w: [bit;96] := 0;\n\
                     var seen: u8 := 0;\n\
                     start Run { always { seen := w + 1; } ref Run; }\n";

fn build_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("single")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0262_targets_{thread}_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

fn generate_c(
    tag: &str,
    source: &str,
) -> Result<(PathBuf, String), takt_lang::diagnostics::Diagnostic> {
    let dir = build_dir(tag);
    takt_lang::compile_to_c(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )?;
    let text = std::fs::read_to_string(dir.join(format!("{tag}.c"))).expect("чтение .c");
    Ok((dir, text))
}

fn generate_rust(
    tag: &str,
    source: &str,
) -> Result<(PathBuf, String), takt_lang::diagnostics::Diagnostic> {
    let dir = build_dir(tag);
    takt_lang::compile_to_rust(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )?;
    let text = std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение модуля");
    Ok((dir, text))
}

// -- Слой 1: эмиссия ----------------------------------------------------------

/// Цель `c`: разряд адресуется своим словом, маска - 64-битная.
#[test]
fn c_addresses_the_right_word() {
    let (_dir, text) = generate_c("wide_c", WIDE).expect("порождение C");
    assert!(
        text.contains("model->w[0] = (model->w[0] & ~(1ull << 5)) | ((1 & 1ull) << 5)"),
        "разряд 5 живёт в слове 0:\n{text}"
    );
    assert!(
        text.contains("model->w[1] = (model->w[1] & ~(1ull << 6)) | ((1 & 1ull) << 6)"),
        "разряд 70 живёт в слове 1 со смещением 6:\n{text}"
    );
    assert!(
        text.contains("((model->w[1] >> 6) & 1ull)"),
        "чтение обязано идти по тому же слову:\n{text}"
    );
}

/// Цель `c`: инициализация и копирование идут по словам.
#[test]
fn c_initialises_and_copies_word_by_word() {
    let (_dir, init) = generate_c("wide_c_init", WIDE).expect("порождение C");
    assert!(
        init.contains("(model->w[0] = (0), model->w[1] = 0ull);"),
        "массив в C не присваивается — обнуление идёт по словам:\n{init}"
    );
    let (_dir, copy) = generate_c("wide_c_copy", COPY).expect("порождение C");
    assert!(
        copy.contains("(model->w[0] = model->x[0], model->w[1] = model->x[1])"),
        "копирование вектора идёт по словам:\n{copy}"
    );
}

/// Цель `rust`: тип, умолчание и разряд согласованы между собой.
#[test]
fn rust_keeps_word_array_everywhere() {
    let (_dir, text) = generate_rust("wide_rs", WIDE).expect("порождение Rust");
    assert!(text.contains("w: [u64; 2],"), "тип поля:\n{text}");
    assert!(
        text.contains("w: [0u64; 2],") && text.contains("self.w = [0u64; 2];"),
        "умолчание и сброс обязаны совпадать с типом:\n{text}"
    );
    assert!(
        text.contains("self.w[1] |= 1 << 6;"),
        "запись разряда 70 — слово 1, смещение 6:\n{text}"
    );
    assert!(
        text.contains("((self.w[1] >> 6) & 1) != 0"),
        "чтение разряда 70 — то же слово:\n{text}"
    );
}

/// **Контроль: скалярный носитель не превратился в массив.**
///
/// `[bit;64]` и уже - по-прежнему скаляр; меняется только суффикс маски.
#[test]
fn scalar_carrier_is_untouched() {
    let (_dir, text) = generate_c("narrow_c", NARROW_HIGH_BIT).expect("порождение C");
    assert!(
        text.contains("model->v = (model->v & ~(1ull << 35)) | ((1 & 1ull) << 35)"),
        "скаляр пишется без индекса слова:\n{text}"
    );
    assert!(
        !text.contains("model->v["),
        "у скаляра индекса слова быть не должно:\n{text}"
    );
}

/// Операция над словами отвергается обеими целями - с причиной.
#[test]
fn arithmetic_over_words_is_refused_with_reason() {
    let c = generate_c("arith_c", ARITH).expect_err("цель `c` обязана отказать");
    assert_eq!(c.code.as_deref(), Some("CC-022"), "код отказа цели `c`");
    assert!(
        c.message.contains("бит-вектор") && c.message.contains("SIM-005"),
        "отказ обязан называть причину:\n{}",
        c.message
    );
    let r = generate_rust("arith_rs", ARITH).expect_err("цель `rust` обязана отказать");
    assert_eq!(r.code.as_deref(), Some("RS-011"), "код отказа цели `rust`");
    assert!(
        r.message.contains("бит-вектор"),
        "отказ обязан называть причину:\n{}",
        r.message
    );
}

/// Разряд за пределом вектора: `[bit;96]` и разряд 200.
const BEYOND: &str = "var w: [bit;96] := 0;\n\
                      var seen: u8 := 0;\n\
                      start Run { always { w.200 := 1; } ref Run; }\n";

/// Разряд за объявленной шириной отвергается **семантикой**.
///
/// Проверка отказов **целей** (`CC-022` у `c`, `RS-011` у `rust`) здесь неверна: с
/// вход отсекает `SE-125` - то есть **раньше** генерации, и одинаково для всех восьми
/// целей: до неё эталон и три цели вход исполняли, а `verilator` вывод `sv` отвергал
/// (`SELRANGE`) при нулевом коде возврата `taktc`.
///
/// Отказы целей при этом **не сняты**: они остаются защитой в глубину, а их
/// недостижимость держит другая фича - ровно как у `CC-023` и предиката безусловного
/// ребра.
#[test]
fn bit_beyond_vector_is_refused_by_semantics() {
    let c = generate_c("beyond_c", BEYOND).expect_err("вход обязан отвергаться до цели `c`");
    assert_eq!(
        c.code.as_deref(),
        Some("SE-125"),
        "разряд за объявленной шириной судит семантика, а не цель"
    );
    assert!(
        c.message.contains("за объявленной шириной"),
        "отказ обязан называть причину:\n{}",
        c.message
    );
    let r = generate_rust("beyond_rs", BEYOND).expect_err("вход обязан отвергаться до цели `rust`");
    assert_eq!(
        r.code.as_deref(),
        Some("SE-125"),
        "тот же код и у второй цели"
    );
}

// -- Слой 2: настоящие инструменты --------------------------------------------

fn tool_available(tool: &str) -> bool {
    Command::new(tool)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Порождённый C принимается `cc` под флагами проверки - все три формы.
#[test]
fn generated_c_compiles_under_gate_flags() {
    if !tool_available("cc") {
        eprintln!("[ПРОПУСК] generated_c_compiles_under_gate_flags: `cc` не найден");
        return;
    }
    for (tag, src) in [
        ("cc_wide", WIDE),
        ("cc_copy", COPY),
        ("cc_narrow", NARROW_HIGH_BIT),
    ] {
        let (dir, _) = generate_c(tag, src).expect("порождение C");
        let out = Command::new("cc")
            .args(["-std=c11", "-c", "-Wall", "-Wextra", "-Werror", "-I"])
            .arg(&dir)
            .arg(dir.join(format!("{tag}.c")))
            .arg("-o")
            .arg(dir.join("out.o"))
            .output()
            .expect("запуск cc");
        assert!(
            out.status.success(),
            "порождённый C ({tag}) обязан собираться под флагами гейта:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}

/// Порождённый Rust принимается `clippy -D warnings` - как в проверке.
#[test]
fn generated_rust_passes_clippy_gate() {
    if !tool_available("clippy-driver") {
        eprintln!("[ПРОПУСК] generated_rust_passes_clippy_gate: clippy-driver не найден");
        return;
    }
    for (tag, src) in [("rs_wide", WIDE), ("rs_copy", COPY)] {
        let (dir, _) = generate_rust(tag, src).expect("порождение Rust");
        let wrapper = dir.join("gate.rs");
        std::fs::write(
            &wrapper,
            format!(
                "#![no_std]\n#[path = \"{}\"]\npub mod generated;\n",
                dir.join(format!("{tag}.rs")).display()
            ),
        )
        .expect("запись обёртки");
        let out = Command::new("clippy-driver")
            .args(["--edition", "2021", "--crate-type=lib", "-D", "warnings"])
            .arg(&wrapper)
            .arg("--out-dir")
            .arg(dir.join("out"))
            .output()
            .expect("запуск clippy-driver");
        assert!(
            out.status.success(),
            "порождённый Rust ({tag}) обязан приниматься `clippy -D warnings`:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}

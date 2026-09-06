//! Model-level `always` (тело `always` вне состояния) эмитится всеми целями кода -.
//!
//! он тело синтетического состояния). Контракт 0083: блок исполняется каждый такт до
//! диспетчеризации состояния (эталон - шаг 2 `execution("always")` симулятора).
//!
//! Здесь - структурная проверка (тело присутствует и стоит до диспетчеризации);
//! потактовое поведение сверяет
//! `conformance_c_tests::model_level_always_matches_generated_c`.

use std::path::{Path, PathBuf};
use std::process::Command;
use takt_lang::GenerateOptions;
use takt_lang::{compile_to_c, compile_to_rust, compile_to_st, compile_to_sv};

/// Успешен ли запуск инструмента `cmd --version`/`--help` (для мягкого пропуска).
fn tool_available(cmd: &str, probe: &str) -> bool {
    Command::new(cmd)
        .arg(probe)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Простая модель `Counter` со **своими** состояниями и model-level `always`. Обёрнута
/// в композицию - так у цели `c` эмитится `typedef` корня.
const SRC: &str = r#"
model Counter {
    var n: u8 := 0;
    always { n := n + 1; }
    start Run { ref Done: n > 2; }
    state Done;
}
start Entry = Counter;
"#;

fn tmp(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0083_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог");
    dir
}

/// Тело `always` (инкремент `n`) обязано стоять **до** ключевого слова диспетчеризации
/// состояния `dispatch` - иначе оно исполнялось бы не каждый такт, а только внутри
/// ветви состояния.
fn assert_before(code: &str, incr: &str, dispatch: &str, target: &str) {
    let incr_at = code.find(incr).unwrap_or_else(|| {
        panic!("{target}: тело model-level `always` ('{incr}') не найдено:\n{code}")
    });
    let disp_at = code
        .find(dispatch)
        .unwrap_or_else(|| panic!("{target}: диспетчеризация '{dispatch}' не найдена:\n{code}"));
    assert!(
        incr_at < disp_at,
        "{target}: model-level `always` ('{incr}') обязан стоять ДО диспетчеризации \
         '{dispatch}' (каждый такт, а не в ветви состояния):\n{code}"
    );
}

#[test]
fn c_emits_model_level_always_before_switch() {
    let dir = tmp("c");
    compile_to_c(
        "ma",
        SRC,
        dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение C");
    let code = std::fs::read_to_string(dir.join("ma.c")).expect(".c");
    assert_before(
        &code,
        "model->n = model->n + 1;",
        "switch (model->state)",
        "c",
    );
}

#[test]
fn rust_emits_model_level_always_before_match() {
    let dir = tmp("rust");
    compile_to_rust(
        "ma",
        SRC,
        dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение rust");
    let path = dir.join("ma.rs");
    let code = std::fs::read_to_string(&path).expect(".rs");
    // `wrapping_add`, а не `+=`: беззнаковая арифметика печатается обёрткой. Свёртка в
    // `+=` вернула бы панику debug-профиля на переполнении - ровно то, что 0127
    // устранила.
    assert_before(
        &code,
        "self.n = self.n.wrapping_add(1);",
        "match self.state",
        "rust",
    );

    // В корпусе model-level `always` нет - проверка `rustc` его не компилирует. Проверяем
    // компиляцию тут же (rustc всегда доступен): касались расчёта мутабельности `let`
    // (`assigned`), это ловит `-D warnings`.
    let out = Command::new("rustc")
        .args(["--edition", "2021", "--crate-type", "lib", "-D", "warnings"])
        .arg(&path)
        .arg("-o")
        .arg(dir.join("ma.rlib"))
        .output()
        .expect("запуск rustc");
    assert!(
        out.status.success(),
        "порождённый Rust с model-level `always` не компилируется:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
fn st_emits_model_level_always_before_case() {
    let dir = tmp("st");
    compile_to_st(
        "ma",
        SRC,
        dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение st");
    let code = std::fs::read_to_string(dir.join("ma.st")).expect(".st");
    assert_before(&code, "n := n + 1;", "CASE state OF", "st");
    assert_st_valid(&dir, "ma");
}

#[test]
fn sv_emits_model_level_always_before_case() {
    let dir = tmp("sv");
    compile_to_sv(
        "ma",
        SRC,
        dir.to_str().unwrap(),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение sv");
    let code = std::fs::read_to_string(dir.join("ma.sv")).expect(".sv");
    // В always_comb инкремент идёт над `_next`; сигнал преисправленийан именем модуля.
    assert_before(
        &code,
        "ma_counter_n_next = (ma_counter_n_next + 1);",
        "unique case (ma_counter_state)",
        "sv",
    );

    // Проверка корпуса model-level `always` не покрывает - линтуем тут (мягкий пропуск,
    // если verilator недоступен).
    if !tool_available("verilator", "--version") {
        eprintln!("[ПРОПУСК] verilator недоступен — sv не проверен линтом");
        return;
    }
    let out = Command::new("verilator")
        .args(["--lint-only", "-Wall"])
        .arg(dir.join("ma.sv"))
        .output()
        .expect("запуск verilator");
    assert!(
        out.status.success(),
        "порождённый SV с model-level `always` не проходит линт:\n{}",
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
        eprintln!("[ПРОПУСК] iec2c недоступен — st не проверен арбитром");
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
        "порождённый ST с model-level `always` не принят iec2c:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

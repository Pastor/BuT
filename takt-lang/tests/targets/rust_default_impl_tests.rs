//! Цель `rust`: модель без портов обязана нести `impl Default`.
//!
//! # Устройство теста
//!
//! Проверка идёт в два слоя, и оба нужны:
//!
//! 1. **эмиссия** - быстрый тест на текст вывода: `impl Default` есть там, где
//!    конструктор без аргументов, и его нет там, где есть `hal` (иначе вывод
//!    просто не скомпилировался бы: значение `H` взять неоткуда);
//! 2. **проверка** - тот же `clippy -D warnings`, что и в `precheck.sh`, на
//!    фикстуре без портов. Первый слой ловит регресс дёшево и точно, второй
//!    доказывает, что ловим мы **настоящее** правило линта, а не своё
//!    представление о нём.
//!
//! Мягкая деградация: нет `clippy-driver` -> второй слой **пропускается с сообщением**
//! (образец - `conformance_rust_tests`). Первый работает всегда.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Модель без портов и без `extern fn`: конструктор без аргументов.
const NO_PORTS: &str = "var n: u8 := 0; \
                        model Counter { start Tick { always { n := n + 1; } ref Done: n >= 3; } state Done; } \
                        start Main = Counter;";

/// Та же модель С портом: конструктор принимает `hal`.
const WITH_PORT: &str = "out ready: bit; var n: u8 := 0; \
                         model Counter { start Tick { always { n := n + 1; ready := 1; } ref Done: n >= 3; } state Done; } \
                         start Main = Counter;";

fn build_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0174_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Порождает Rust для исходника и возвращает текст модуля.
fn generate(tag: &str, source: &str) -> (std::path::PathBuf, String) {
    let dir = build_dir(tag);
    takt_lang::compile_to_rust(
        tag,
        source,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let path = dir.join(format!("{tag}.rs"));
    let text = std::fs::read_to_string(&path).expect("чтение порождённого модуля");
    (dir, text)
}

// -- Слой 1: эмиссия ----------------------------------------------------------

/// Конструктор без аргументов обязан идти в паре с `impl Default`.
#[test]
fn model_without_ports_emits_default_impl() {
    let (_dir, text) = generate("noports", NO_PORTS);
    assert!(
        text.contains("pub fn new() -> Self"),
        "предусловие теста: у модели без портов конструктор без аргументов.\n{text}"
    );
    assert!(
        text.contains("impl Default for Noports"),
        "конструктор без аргументов обязан идти в паре с `impl Default`: иначе \
         `clippy::new_without_default` валит гейт цели `rust` под `-D warnings`.\n{text}"
    );
}

/// **Контрпример:** при `new(hal: H)` `impl Default` печатать нельзя.
///
/// Не "не нужно", а именно нельзя: значение `H` взять неоткуда, и такой вывод не
/// скомпилировался бы. Без этого теста правка, печатающая `Default` всегда, прошла
/// бы первый тест и сломала весь корпус.
#[test]
fn model_with_port_does_not_emit_default_impl() {
    let (_dir, text) = generate("withport", WITH_PORT);
    assert!(
        text.contains("pub fn new(hal: H) -> Self"),
        "предусловие теста: у модели с портом конструктор принимает `hal`.\n{text}"
    );
    assert!(
        !text.contains("impl Default for"),
        "при конструкторе с аргументом `impl Default` печатать нельзя — значение \
         `H` взять неоткуда, вывод не скомпилируется.\n{text}"
    );
}

// -- Слой 2: тот же проверка, что в precheck.sh -----------------------------------

fn clippy_available() -> bool {
    Command::new("clippy-driver")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Порождённый модуль принимается `clippy -D warnings` - как в проверке.
///
/// Обёртка с `#![no_std]` в корне повторяет проверка `precheck.sh` дословно: атрибут
/// допустим только в корне крейта, и модуль обязан проверяться так, как он будет
/// использоваться.
#[test]
fn generated_module_passes_clippy_gate() {
    if !clippy_available() {
        eprintln!("[ПРОПУСК] generated_module_passes_clippy_gate: clippy-driver не найден");
        return;
    }
    let (dir, _) = generate("noports_gate", NO_PORTS);
    let wrapper = dir.join("gate.rs");
    let module = dir.join("noports_gate.rs");
    std::fs::write(
        &wrapper,
        format!(
            "#![no_std]\n#[path = \"{}\"]\npub mod generated;\n",
            module.display()
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
        "порождённый Rust обязан приниматься `clippy -D warnings` — это тот же \
         гейт, что в `precheck.sh`:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

//! Цель `rust`: живость инициализатора локальной переменной.
//!
//! # Что тестытся
//!
//! `generator/rust/rust_live.rs` решает, печатать ли начальное значение локальной
//! переменной. Ошибка здесь тиха для `taktc` и громка для пользователя: `let mut ds: u8
//! = 0;` перед записью во все ветки - это `unused_assignments`, а отложенная форма `let
//! ds: u8;` перед `if/else` - `clippy::needless_late_init`. Обе под `-D warnings`
//! (политика цели, 0050) означают **отказ сборки** порождённого кода при нулевом коде
//! возврата компилятора.
//!
//! # Устройство: два слоя
//!
//! 1. **эмиссия** - проверка текста вывода: какая именно форма напечатана
//!    (`let x: T = ...`, `let mut x: T;`, `let mut x: T = ...`). Ловит регресс
//!    точно и называет место;
//! 2. **проверка** - тот же `clippy -D warnings`, что в `precheck.sh`, на всех
//!    исправлениетурах разом. Доказывает, что проверяется **настоящее** правило
//!    линта, а не представление о нём (образец - `rust_default_impl_tests.rs`).
//!
//! Мягкая деградация: нет `clippy-driver` -> второй слой пропускается.

use std::process::Command;
use takt_lang::generator::GenerateOptions;

/// Все формы живости в одном входе: так их печать сверяется одним прогоном, а проверка
/// линта видит их вместе - как увидел бы пользователь.
const FIXTURE: &str = r#"
fn both(a: u8, b: u8) -> u8 {
    var ds: u8 := 0;
    if a > b { ds := a - b; } else { ds := b - a; }
    return ds;
}

fn once(a: u8) -> u8 {
    var t: u8 := 0;
    t := a;
    return t;
}

fn twice(a: u8) -> u8 {
    var t: u8 := 0;
    t := a;
    t := t + 1;
    return t;
}

fn guarded(a: u8) -> u8 {
    var t: u8 := 0;
    if a > 1 { t := a; }
    return t;
}

fn looped(a: u8) -> u8 {
    var t: u8 := 0;
    while a > t { t := t + 1; }
    return t;
}

fn read_first(a: u8) -> u8 {
    var t: u8 := 0;
    var s: u8 := t;
    t := a;
    return s + t;
}

fn matched(a: u8) -> u8 {
    var t: u8 := 0;
    match a { 1 => { t := 5; } _ => { t := 7; } }
    return t;
}

fn matched_open(a: u8) -> u8 {
    var t: u8 := 0;
    match a { 1 => { t := 5; } 2 => { t := 6; } }
    return t;
}

fn stepped(a: u8) -> u8 {
    var t: u8 := 0;
    var i: u8 := 0;
    for i := 0; i < a; i := i + 1 { t := t + i; }
    return t;
}

fn two_vars(a: u8, b: u8) -> u8 {
    var x: u8 := 0;
    var y: u8 := 0;
    if a > b { x := a; y := b; } else { x := b; y := a; }
    return x + y;
}

var n: u8 := 0;

start Run {
    always {
        n := both(4, 2) + once(1) + twice(1) + guarded(2) + looped(3)
           + read_first(1) + matched(1) + matched_open(1) + stepped(2) + two_vars(4, 2);
    }
    ref Done: n > 0;
}
state Done;
"#;

fn build_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0216_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("создание каталога");
    dir
}

/// Порождает Rust и возвращает (каталог, текст модуля).
fn generate(tag: &str) -> (std::path::PathBuf, String) {
    let dir = build_dir(tag);
    takt_lang::compile_to_rust(
        tag,
        FIXTURE,
        dir.to_str().expect("путь в UTF-8"),
        &[],
        &GenerateOptions::default(),
    )
    .expect("порождение Rust");
    let text =
        std::fs::read_to_string(dir.join(format!("{tag}.rs"))).expect("чтение порождённого модуля");
    (dir, text)
}

/// Тело функции `name` из порождённого модуля.
fn body_of<'a>(text: &'a str, name: &str) -> &'a str {
    let head = format!("fn {name}(");
    let start = text
        .find(&head)
        .unwrap_or_else(|| panic!("в выводе нет функции `{name}`:\n{text}"));
    let end = text[start..]
        .find("\n}\n")
        .unwrap_or_else(|| panic!("не найден конец функции `{name}`"));
    &text[start..start + end]
}

// -- Слой 1: эмиссия ----------------------------------------------------------

/// Обе ветки `if/else` пишут переменную - объявление сворачивается в значение.
///
/// Ключевой случай корпуса (`travel_time` в `stacker.takt`).
#[test]
fn both_branches_fold_into_value() {
    let (_dir, text) = generate("fold_branches");
    let body = body_of(&text, "both");
    assert!(
        body.contains("let ds: u8 = if a > b {"),
        "объявление обязано свернуться в `let ds: u8 = if …`:\n{body}"
    );
    assert!(
        !body.contains("let mut ds"),
        "присваивание на каждом пути — это инициализация, `mut` лишний:\n{body}"
    );
}

/// Безусловная запись сворачивает объявление в значение.
#[test]
fn unconditional_assign_folds_into_value() {
    let (_dir, text) = generate("fold_value");
    assert!(
        body_of(&text, "once").contains("let t: u8 = a;"),
        "ожидалось `let t: u8 = a;`:\n{}",
        body_of(&text, "once")
    );
}

/// Второе присваивание на том же пути делает переменную изменяемой.
#[test]
fn second_assignment_requires_mut() {
    let (_dir, text) = generate("fold_mut");
    let body = body_of(&text, "twice");
    assert!(
        body.contains("let mut t: u8 = a;"),
        "после инициализации есть ещё запись — нужен `mut`:\n{body}"
    );
}

/// **Контрпример:** `if` без `else` перезаписи не гарантирует - значение живо.
#[test]
fn if_without_else_keeps_initializer() {
    let (_dir, text) = generate("keep_if");
    assert!(
        body_of(&text, "guarded").contains("let mut t: u8 = 0;"),
        "путь мимо `if` оставляет начальное значение живым:\n{}",
        body_of(&text, "guarded")
    );
}

/// **Контрпример:** тело цикла может не исполниться - значение живо.
#[test]
fn loop_keeps_initializer() {
    let (_dir, text) = generate("keep_loop");
    assert!(
        body_of(&text, "looped").contains("let mut t: u8 = 0;"),
        "цикл может не исполниться ни разу:\n{}",
        body_of(&text, "looped")
    );
}

/// **Контрпример:** чтение до перезаписи оставляет значение живым.
#[test]
fn read_before_assign_keeps_initializer() {
    let (_dir, text) = generate("keep_read");
    assert!(
        body_of(&text, "read_first").contains("let mut t: u8 = 0;"),
        "`t` прочитана до перезаписи:\n{}",
        body_of(&text, "read_first")
    );
}

/// `match` с ветвью `_`, пишущий переменную во всех ветвях, сворачивается в цепочку
/// сравнений.
///
/// Прежде здесь печаталось `let mut t: u8 = 0;` - мёртвое значение, то есть
/// `unused_assignments` и отказ проверки у пользователя.
#[test]
fn exhaustive_match_folds_into_chain() {
    let (_dir, text) = generate("fold_match");
    let body = body_of(&text, "matched");
    assert!(
        body.contains("let t: u8 = if a == 1 { 5 } else { 7 };"),
        "`match` с `_` обязан свернуться в значение:\n{body}"
    );
    assert!(
        !body.contains("let mut t: u8 = 0;"),
        "мёртвое начальное значение печатать нельзя — это `unused_assignments`:\n{body}"
    );
}

/// **Контрпример:** `match` **без** ветви `_` перезаписи не доказывает -
/// значение живо.
///
/// Без этой проверки правка читалась бы как "любой `match` затирает", и разбор перестал
/// бы быть консервативным.
#[test]
fn open_match_keeps_initializer() {
    let (_dir, text) = generate("keep_match");
    assert!(
        body_of(&text, "matched_open").contains("let mut t: u8 = 0;"),
        "путь мимо всех образцов оставляет значение живым:\n{}",
        body_of(&text, "matched_open")
    );
}

/// `init` цикла `for` исполняется всегда - начальное значение мертво.
///
/// Прежде печаталось `let mut i: u8 = 0;` перед `{ i = 0; while ... }` - то же
/// `unused_assignments`.
#[test]
fn for_init_kills_initializer() {
    let (_dir, text) = generate("fold_for");
    let body = body_of(&text, "stepped");
    assert!(
        body.contains("let mut i: u8;"),
        "`init` цикла затирает значение — печатается отложенная форма:\n{body}"
    );
    assert!(
        !body.contains("let mut i: u8 = 0;"),
        "мёртвое начальное значение печатать нельзя:\n{body}"
    );
}

/// Один оператор затирает две переменные - свернуть в одно `let` нельзя, печатается
/// отложенная форма обеих.
#[test]
fn two_variables_share_one_statement() {
    let (_dir, text) = generate("fold_two");
    let body = body_of(&text, "two_vars");
    assert!(
        body.contains("let x: u8;") && body.contains("let y: u8;"),
        "обе переменные обязаны получить отложенную форму:\n{body}"
    );
    assert!(
        !body.contains("let mut x") && !body.contains("let mut y"),
        "присваивание на каждом пути — инициализация, `mut` лишний:\n{body}"
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

/// Порождённый модуль со всеми формами живости принимается `clippy -D warnings`.
///
/// Этот слой и нашёл оба дефекта: проверка текста говорит, что напечатано, а линт -
/// примет ли это пользователь.
#[test]
fn generated_module_passes_clippy_gate() {
    if !clippy_available() {
        eprintln!("[ПРОПУСК] generated_module_passes_clippy_gate: clippy-driver не найден");
        return;
    }
    let (dir, _) = generate("live_gate");
    let wrapper = dir.join("gate.rs");
    let module = dir.join("live_gate.rs");
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

//! Стенд внешних функций из сценария.
//!
//! # Что здесь проверяется
//!
//! `extern fn mem_read(addr: u8) -> u8;` цель `c` принимает (колбэк HAL), а эталон
//! отвечал `SIM-019` и **останавливал прогон**: модель с внешним интерфейсом была
//! непроверяема эталоном, а примеры документа такой формы содержать не могли.
//!
//! значения приходят **из сценария** - оттуда же, откуда входы портов. Две формы: одно
//! значение на такт и таблица по первому аргументу ("память").
//!
//! **Умолчания нет.** Не задали подмену - прежний отказ. Ноль по умолчанию сделал бы
//! прогон зелёным там, где эталон и прошивка расходятся, - это тот самый класс, ради
//! которого в проекте заведены потактовые сверки.

use std::path::PathBuf;
use std::process::Command;

/// Модель читает "память" внешней функцией и складывает прочитанное.
const MODEL: &str = "\
extern fn mem_read(addr: u8) -> u8;

var v: u8 := 0;
var i: u8 := 0;
start Run {
    always {
        v := mem_read(i);
        i := i + 1;
    }
    ref Run;
}
";

/// Уникальный по тесту каталог.
fn work_dir(tag: &str) -> PathBuf {
    let thread = std::thread::current()
        .name()
        .unwrap_or("main")
        .replace(':', "_");
    let dir = std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_0209_ext_{tag}_{thread}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("каталог теста");
    dir
}

/// Прогоняет модель со сценарием (если задан) и возвращает вывод симулятора.
fn run(tag: &str, scenario: Option<&str>, steps: u32) -> (String, bool) {
    let dir = work_dir(tag);
    let model = dir.join("model.takt");
    std::fs::write(&model, MODEL).expect("запись модели");
    let mut command = Command::new(env!("CARGO_BIN_EXE_takt-sim"));
    command.arg(&model).arg("-n").arg(steps.to_string());
    if let Some(scenario) = scenario {
        let path = dir.join("scenario.json");
        std::fs::write(&path, scenario).expect("запись сценария");
        command.arg("-s").arg(path);
    }
    let out = command.output().expect("запуск takt-sim");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    (text, out.status.success())
}

/// **T1.** Таблица по первому аргументу - "память", из которой читает модель.
///
/// Такт 1 читает по адресу 0, такт 2 - по адресу 1: значения обязаны прийти
/// **разные**, иначе таблица не по аргументу.
#[test]
fn table_by_argument_feeds_reads() {
    let scenario = r#"[
      { "extern": { "mem_read": { "0": 9, "1": 8 } } },
      { "extern": { "mem_read": { "0": 9, "1": 8 } } }
    ]"#;
    let (text, ok) = run("table", Some(scenario), 2);
    assert!(ok, "прогон обязан удаться:\n{text}");
    assert!(text.contains("v=9"), "первый такт читает адрес 0:\n{text}");
    assert!(text.contains("v=8"), "второй такт читает адрес 1:\n{text}");
}

/// **T2.** Одно значение на такт - форма для вызовов без осмысленного аргумента.
#[test]
fn single_value_answers_any_call() {
    let scenario = r#"[ { "extern": { "mem_read": 42 } } ]"#;
    let (text, ok) = run("single", Some(scenario), 1);
    assert!(ok, "прогон обязан удаться:\n{text}");
    assert!(text.contains("v=42"), "значение стенда не доехало:\n{text}");
}

/// **T3. Контр-пример.** Без стенда - прежний отказ, а не ноль.
///
/// Это главная проверка фичи: молчаливый ноль здесь означал бы, что эталон считает
/// одно, прошивка другое, а сверка трасс зелена.
#[test]
fn without_stub_the_run_still_refuses() {
    let (text, ok) = run("none", None, 1);
    assert!(!ok, "прогон без стенда обязан остановиться:\n{text}");
    assert!(
        text.contains("SIM-019"),
        "отказ обязан остаться прежним:\n{text}"
    );
}

/// **T4. Контр-пример.** Стенд, заданный на один такт, не действует на другой.
///
/// Значения принадлежат **шагу**, а не прогону: иначе автор, задавший подмену в первом
/// такте, молча получал бы её и в десятом.
#[test]
fn stub_belongs_to_the_step() {
    let scenario = r#"[
      { "extern": { "mem_read": 42 } },
      { }
    ]"#;
    let (text, ok) = run("per_step", Some(scenario), 2);
    assert!(!ok, "второй такт без стенда обязан отказать:\n{text}");
    assert!(text.contains("v=42"), "первый такт обязан пройти:\n{text}");
    assert!(text.contains("SIM-019"), "второй такт: {text}");
}

/// **T5.** Ключ таблицы, не являющийся числом, - ошибка сценария.
///
/// Автор написал подмену, и она обязана сработать; молча пропустить её значило бы
/// вернуть прогон к отказу, оставив автора гадать.
#[test]
fn non_numeric_table_key_is_an_error() {
    let scenario = r#"[ { "extern": { "mem_read": { "первый": 9 } } } ]"#;
    let (text, ok) = run("badkey", Some(scenario), 1);
    assert!(
        !ok,
        "сценарий с нечисловым ключом обязан быть отвергнут:\n{text}"
    );
    assert!(
        text.contains("mem_read"),
        "диагностика обязана назвать функцию:\n{text}"
    );
}

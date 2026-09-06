//! Подкоманда `taktc version` и её синонимы.
//!
//! Тесты идут **сквозные, через бинарник**: предмет фичи - то, что видит пользователь в
//! терминале. Юнит-тест `version_text` проверяет содержание строки, а здесь
//! проверяется, что до пользователя она доходит - и по всем трём входам одинаково.

use std::process::Command;

fn run(args: &[&str]) -> (String, i32) {
    let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .args(args)
        .output()
        .expect("запуск taktc");
    (
        String::from_utf8_lossy(&out.stdout).to_string(),
        out.status.code().unwrap_or(-1),
    )
}

/// A1: подкоманда печатает обе версии и завершается успехом.
#[test]
fn version_subcommand_prints_both_versions() {
    let (stdout, code) = run(&["version"]);
    assert_eq!(code, 0, "версия — не ошибка: код {code}");
    assert!(
        stdout.contains(takt_lang::LANGUAGE_VERSION),
        "вывод обязан нести версию языка: {stdout:?}"
    );
    assert!(
        stdout.contains("taktc "),
        "вывод обязан нести версию компилятора: {stdout:?}"
    );
}

/// A2: три входа - один вывод.
///
/// Проверяется **равенство**, а не "каждый что-то печатает": синоним, разошедшийся с
/// подкомандой, - это две реализации одного ответа, и они расходятся молча.
#[test]
fn all_three_entries_agree() {
    let (subcommand, _) = run(&["version"]);
    let (long_flag, long_code) = run(&["--version"]);
    let (short_flag, short_code) = run(&["-V"]);
    assert_eq!(
        subcommand, long_flag,
        "`--version` обязан совпасть с `version`"
    );
    assert_eq!(subcommand, short_flag, "`-V` обязан совпасть с `version`");
    assert_eq!(long_code, 0);
    assert_eq!(short_code, 0);
}

/// A5: справка называет подкоманду - иначе о ней узнают только из документа.
#[test]
fn usage_mentions_version_subcommand() {
    let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .arg("--help")
        .output()
        .expect("запуск taktc");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        text.contains("taktc version"),
        "справка обязана назвать подкоманду: {text}"
    );
}

/// A6: текст "неизвестная команда" перечисляет доступные - и новую в том числе.
#[test]
fn unknown_command_message_lists_version() {
    let out = Command::new(env!("CARGO_BIN_EXE_taktc"))
        .arg("нетакой")
        .output()
        .expect("запуск taktc");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("'version'"),
        "перечень доступных команд обязан включать version: {stderr}"
    );
}

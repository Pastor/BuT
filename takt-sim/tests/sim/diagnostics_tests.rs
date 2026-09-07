//! Позиции в диагностиках симулятора.
//!
//! Тесты сквозные - гоняют сам бинарник: печать живёт в `bin/simulation.rs`, и
//! проверять надо ровно то, что увидит пользователь.

use std::process::Command;

const DIR: &str = "tests/data/diag54";

/// Запускает симулятор на фикстуре и возвращает stderr (фикстуры ошибочны).
fn stderr_of(fixture: &str) -> String {
    let out = Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .args([&format!("{DIR}/{fixture}"), "-I", DIR, "--steps", "3"])
        .output()
        .expect("запуск симулятора");
    assert!(!out.status.success(), "{fixture}: фикстура обязана падать");
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// A1: позиция и код печатаются.
#[test]
fn semantic_error_prints_position_and_code() {
    let err = stderr_of("lib_bad.takt");
    assert!(
        err.contains("lib_bad.takt:2:18:"),
        "ожидались путь:строка:колонка, получено: {err}"
    );
    assert!(err.contains("[SE-002]"), "код диагностики потерян: {err}");
}

/// A2: ошибка внутри импорта названа библиотекой, а не импортёром.
///
/// Это и есть суть фичи: прежде оба случая давали дословно одинаковый вывод.
#[test]
fn error_inside_import_names_the_library() {
    let err = stderr_of("importer.takt");
    assert!(
        err.contains("lib_bad.takt:"),
        "виновник — библиотека, а не импортёр: {err}"
    );
    assert!(
        !err.contains("importer.takt:"),
        "импортёр ошибок не содержит и назван быть не должен: {err}"
    );
}

/// A3: ошибки разбора печатаются все, каждая со своей позицией.
///
/// Симулятор показывает их все (в отличие от `taktc`, показывающего первую) - поведение
/// сохранено осознанно: каждая ошибка своя подсказка.
#[test]
fn all_parse_errors_are_printed_with_positions() {
    let err = stderr_of("syntax_bad.takt");
    let positioned = err
        .lines()
        .filter(|l| l.contains("syntax_bad.takt:"))
        .count();
    assert!(
        positioned >= 2,
        "ожидалось несколько ошибок разбора, каждая с позицией; получено {positioned}: {err}"
    );
}

/// A4: формат позиции совпадает с `taktc` - потому что функция одна и та же
/// (`takt_lang::diagnostics::position_prefix`).
///
/// Тест против расхождения печати: копия формата в каждом бинарнике разошлась бы
/// (доказанный класс дефекта - ).
#[test]
fn position_format_matches_the_shared_layer() {
    let err = stderr_of("lib_bad.takt");
    let source = std::fs::read_to_string(format!("{DIR}/lib_bad.takt")).expect("чтение");
    let offset = source.find("Nowhere").expect("ссылка есть");
    let (line, column) = takt_lang::diagnostics::line_column(&source, offset);
    assert!(
        err.contains(&format!("lib_bad.takt:{line}:{column}:")),
        "позиция обязана совпадать с расчётом общего слоя ({line}:{column}): {err}"
    );
}

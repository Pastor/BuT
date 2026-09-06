//! Тесты для модуля [`takt_lang::diagnostics`].
//!
//! Проверяют все фабричные методы создания [`Diagnostic`], форматирование уровней
//! серьёзности и сортировку типов ошибок.

use takt_lang::diagnostics::{Diagnostic, ErrorType, Level, Location, Note};

// ------------------------------------------------------------------- Вспомогательная
// константа -------------------------------------------------------------------

const LOC: Location = Location::Source(0, 0, 5);
const LOC2: Location = Location::Source(0, 10, 20);

// ------------------------------------------------------------------- Level
// -------------------------------------------------------------------

/// `Level::as_str()` возвращает ожидаемые строки.
#[test]
fn level_as_str() {
    assert_eq!(Level::Debug.as_str(), "debug");
    assert_eq!(Level::Info.as_str(), "info");
    assert_eq!(Level::Warning.as_str(), "warning");
    assert_eq!(Level::Error.as_str(), "error");
}

/// `Display` для `Level` совпадает с `as_str()`.
#[test]
fn level_display() {
    assert_eq!(format!("{}", Level::Debug), "debug");
    assert_eq!(format!("{}", Level::Info), "info");
    assert_eq!(format!("{}", Level::Warning), "warning");
    assert_eq!(format!("{}", Level::Error), "error");
}

/// Уровни упорядочены: Debug < Info < Warning < Error.
#[test]
fn level_ordering() {
    assert!(Level::Debug < Level::Info);
    assert!(Level::Info < Level::Warning);
    assert!(Level::Warning < Level::Error);
}

// ------------------------------------------------------------------- Фабричные методы
// Diagnostic -------------------------------------------------------------------

/// `Diagnostic::debug()` создаёт отладочное сообщение.
#[test]
fn diagnostic_debug_method() {
    let d = Diagnostic::debug(LOC, "отладка".to_string());
    assert_eq!(d.level, Level::Debug);
    assert_eq!(d.ty, ErrorType::None);
    assert_eq!(d.message, "отладка");
    assert!(d.notes.is_empty());
}

/// `Diagnostic::info()` создаёт информационное сообщение.
#[test]
fn diagnostic_info_method() {
    let d = Diagnostic::info(LOC, "информация".to_string());
    assert_eq!(d.level, Level::Info);
    assert_eq!(d.ty, ErrorType::None);
    assert!(d.notes.is_empty());
}

/// `Diagnostic::parser_error()` - ошибка парсера.
#[test]
fn diagnostic_parser_error_method() {
    let d = Diagnostic::parser_error(LOC, "неожиданный токен".to_string());
    assert_eq!(d.level, Level::Error);
    assert_eq!(d.ty, ErrorType::ParserError);
    assert_eq!(d.message, "неожиданный токен");
}

/// `Diagnostic::error()` - синтаксическая ошибка.
#[test]
fn diagnostic_error_method() {
    let d = Diagnostic::error(LOC, "синтаксическая ошибка".to_string());
    assert_eq!(d.level, Level::Error);
    assert_eq!(d.ty, ErrorType::SyntaxError);
}

/// `Diagnostic::declaration_error()` - ошибка объявления.
#[test]
fn diagnostic_declaration_error_method() {
    let d = Diagnostic::declaration_error(LOC, "неизвестный идентификатор".to_string());
    assert_eq!(d.level, Level::Error);
    assert_eq!(d.ty, ErrorType::DeclarationError);
}

/// `Diagnostic::cast_error()` - ошибка приведения типов.
#[test]
fn diagnostic_cast_error_method() {
    let d = Diagnostic::cast_error(LOC, "невозможно привести тип".to_string());
    assert_eq!(d.level, Level::Error);
    assert_eq!(d.ty, ErrorType::CastError);
    assert!(d.notes.is_empty());
}

/// `Diagnostic::cast_error_with_note()` - ошибка с заметкой.
#[test]
fn diagnostic_cast_error_with_note_method() {
    let d = Diagnostic::cast_error_with_note(
        LOC,
        "ошибка типа".to_string(),
        LOC2,
        "здесь объявлен тип".to_string(),
    );
    assert_eq!(d.level, Level::Error);
    assert_eq!(d.ty, ErrorType::CastError);
    assert_eq!(d.notes.len(), 1);
    assert_eq!(d.notes[0].loc, LOC2);
    assert_eq!(d.notes[0].message, "здесь объявлен тип");
}

/// `Diagnostic::type_error()` - ошибка типизации.
#[test]
fn diagnostic_type_error_method() {
    let d = Diagnostic::type_error(LOC, "несоответствие типов".to_string());
    assert_eq!(d.level, Level::Error);
    assert_eq!(d.ty, ErrorType::TypeError);
}

/// `Diagnostic::cast_warning()` - предупреждение о небезопасном приведении.
#[test]
fn diagnostic_cast_warning_method() {
    let d = Diagnostic::cast_warning(LOC, "потеря точности".to_string());
    assert_eq!(d.level, Level::Warning);
    assert_eq!(d.ty, ErrorType::CastError);
}

/// `Diagnostic::warning()` - предупреждение.
#[test]
fn diagnostic_warning_method() {
    let d = Diagnostic::warning(LOC, "неиспользуемая переменная".to_string());
    assert_eq!(d.level, Level::Warning);
    assert_eq!(d.ty, ErrorType::Warning);
    assert!(d.notes.is_empty());
}

/// `Diagnostic::warning_with_note()` - предупреждение с одной заметкой.
#[test]
fn diagnostic_warning_with_note_method() {
    let d = Diagnostic::warning_with_note(
        LOC,
        "предупреждение".to_string(),
        LOC2,
        "контекст".to_string(),
    );
    assert_eq!(d.level, Level::Warning);
    assert_eq!(d.notes.len(), 1);
    assert_eq!(d.notes[0].message, "контекст");
}

/// `Diagnostic::warning_with_notes()` - предупреждение с несколькими заметками.
#[test]
fn diagnostic_warning_with_notes_method() {
    let notes = vec![
        Note {
            loc: LOC,
            message: "первая заметка".to_string(),
        },
        Note {
            loc: LOC2,
            message: "вторая заметка".to_string(),
        },
    ];
    let d = Diagnostic::warning_with_notes(LOC, "предупреждение".to_string(), notes);
    assert_eq!(d.level, Level::Warning);
    assert_eq!(d.notes.len(), 2);
}

/// `Diagnostic::error_with_note()` - ошибка с одной заметкой.
#[test]
fn diagnostic_error_with_note_method() {
    let d = Diagnostic::error_with_note(
        LOC,
        "ошибка".to_string(),
        LOC2,
        "дополнительный контекст".to_string(),
    );
    assert_eq!(d.level, Level::Error);
    assert_eq!(d.ty, ErrorType::None);
    assert_eq!(d.notes.len(), 1);
}

/// `Diagnostic::error_with_notes()` - ошибка со списком заметок.
#[test]
fn diagnostic_error_with_notes_method() {
    let notes = vec![
        Note {
            loc: LOC,
            message: "заметка 1".to_string(),
        },
        Note {
            loc: LOC2,
            message: "заметка 2".to_string(),
        },
    ];
    let d = Diagnostic::error_with_notes(LOC, "ошибка".to_string(), notes);
    assert_eq!(d.level, Level::Error);
    assert_eq!(d.notes.len(), 2);
}

// ------------------------------------------------------------------- Поля Diagnostic
// -------------------------------------------------------------------

/// Местоположение сохраняется корректно.
#[test]
fn diagnostic_location_stored_correctly() {
    let d = Diagnostic::error(LOC, "test".to_string());
    assert_eq!(d.loc, LOC);
    assert_eq!(d.loc.start(), 0);
    assert_eq!(d.loc.end(), 5);
}

/// Debug-вывод Diagnostic не паникует.
#[test]
fn diagnostic_debug_format() {
    let d = Diagnostic::warning(LOC, "тест".to_string());
    let _ = format!("{:?}", d);
}

/// Clone Diagnostic работает корректно.
#[test]
fn diagnostic_clone() {
    let d = Diagnostic::parser_error(LOC, "клон".to_string());
    let cloned = d.clone();
    assert_eq!(d, cloned);
}

/// Сортировка Diagnostic: Debug < Info < Warning < Error.
#[test]
fn diagnostic_ordering() {
    let d_debug = Diagnostic::debug(LOC, "".to_string());
    let d_error = Diagnostic::error(LOC, "".to_string());
    assert!(d_debug < d_error);
}

// ------------------------------------------------------------------- Контр-примеры
// (некорректные случаи)
// -------------------------------------------------------------------

/// Отладочный диагностик не является ошибкой.
#[test]
fn debug_is_not_error_level() {
    let d = Diagnostic::debug(LOC, "".to_string());
    assert_ne!(d.level, Level::Error);
}

/// Предупреждение не является ошибкой.
#[test]
fn warning_is_not_error_level() {
    let d = Diagnostic::warning(LOC, "".to_string());
    assert_ne!(d.level, Level::Error);
}

/// `parser_error` не является предупреждением.
#[test]
fn parser_error_is_not_warning() {
    let d = Diagnostic::parser_error(LOC, "".to_string());
    assert_ne!(d.level, Level::Warning);
}

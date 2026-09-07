//! Отображение имён Takt в идентификаторы Rust.
//!
//! Правила:
//!
//! - типы (модели, состояния, перечисления, варианты) -> `CamelCase`;
//! - значения (переменные, поля, функции, методы HAL) -> `snake_case`;
//! - коллизия с ключевым словом снимается **raw-идентификатором** (`r#type`);
//! - `Self`/`self`/`crate`/`super` - **не снимаются ничем** -> диагностика
//!   [`RS-004`](rs004).
//!
//! ## Чего здесь нет: ловушки стандартной библиотеки
//!
//! У цели `st` имя модели `Concat` даёт `invalid function block name` - в IEC 61131-3
//! пространство имён POU плоское и общее со стандартной библиотекой (`CLAUDE.md`). В
//! Rust этой ловушки **нет**: `Box`/`Option`/`String` как имена принимаются - прелюдия
//! лишь затеняется, а не занимает имя.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::keywords;
use crate::semantic::naming::{normalize_camelcase_name, normalize_lowercase_snakecase};

/// Строит диагностику `RS-004` - имя непредставимо в Rust.
fn rs004(original: &str, produced: &str, loc: Location) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "Имя '{}' даёт идентификатор '{}', непредставимый в Rust: \
             это ключевое слово, и raw-идентификатор 'r#{}' запрещён отдельным \
             правилом языка. Переименуйте элемент в исходнике .takt",
            original, produced, produced
        ),
    )
    .with_code("RS-004")
}

/// Строит диагностику `RS-005` - два имени слиплись после приведения регистра.
///
/// Отдана наружу для полей структуры модели: там столкновение ловится **накопительно**,
/// по одному объявлению за раз, чтобы отказ нёс координату этого объявления, - а не
/// списком пар, как у портов и состояний.
pub(super) fn name_collision(
    first: &str,
    second: &str,
    produced: &str,
    kind: &str,
    loc: Location,
) -> Diagnostic {
    rs005(first, second, produced, kind, loc)
}

/// Строит диагностику `RS-005` - два имени слиплись после приведения регистра.
fn rs005(first: &str, second: &str, produced: &str, kind: &str, loc: Location) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "{}: имена '{}' и '{}' после приведения регистра дают один \
             идентификатор '{}'. Переименуйте одно из них в исходнике .takt",
            kind, first, second, produced
        ),
    )
    .with_code("RS-005")
}

/// Приводит имя к `CamelCase` - для типов: моделей, состояний, перечислений.
///
/// # Ошибки
/// [`RS-004`], если результат - ключевое слово, не спасаемое raw-идентификатором
/// (практически: исходное имя `Self` или `self`, оба дают `Self`).
pub(crate) fn rust_type_name(raw: &str, loc: Location) -> Result<String, Diagnostic> {
    let name = normalize_camelcase_name(raw);
    if keywords::RUST_NOT_RAW.contains(&name.as_str()) {
        return Err(rs004(raw, &name, loc));
    }
    // Все ключевые слова Rust, кроме `Self`, записаны строчными, поэтому CamelCase их и
    // так снимает (`type` -> `Type`). Проверка оставлена как тест на случай смены
    // правил регистра в `normalize_camelcase_name`.
    if keywords::RUST.contains(&name.as_str()) {
        return Ok(format!("r#{}", name));
    }
    Ok(name)
}

/// Приводит имя к `snake_case` - для значений: переменных, полей, функций.
///
/// # Ошибки
/// [`RS-004`], если результат - `self`/`crate`/`super`.
pub(crate) fn rust_value_name(raw: &str, loc: Location) -> Result<String, Diagnostic> {
    let name = normalize_lowercase_snakecase(raw.to_string());
    if keywords::RUST_NOT_RAW.contains(&name.as_str()) {
        return Err(rs004(raw, &name, loc));
    }
    if keywords::RUST.contains(&name.as_str()) {
        return Ok(format!("r#{}", name));
    }
    Ok(name)
}

/// Проверяет, что имена не слипаются после приведения регистра.
///
/// Вход - пары `(исходное имя, порождённый идентификатор)`. Молчаливое слипание
/// (`floor_sensor` и `FloorSensor` -> одно `FloorSensor`) дало бы либо ошибку сборки в
/// чужом месте, либо - хуже - связывание не тех элементов.
///
/// # Ошибки
/// [`RS-005`] на первой же коллизии.
pub(crate) fn check_name_collisions(
    names: &[(String, String)],
    kind: &str,
    loc: Location,
) -> Result<(), Diagnostic> {
    let mut seen: Vec<(&String, &String)> = Vec::new();
    for (original, produced) in names {
        if let Some((first, _)) = seen.iter().find(|(_, p)| *p == produced) {
            return Err(rs005(first, original, produced, kind, loc));
        }
        seen.push((original, produced));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> Location {
        Location::Codegen
    }

    /// Имя состояния приводится к CamelCase.
    #[test]
    fn type_name_is_camelcase() {
        assert_eq!(rust_type_name("at_floor", loc()).unwrap(), "AtFloor");
        assert_eq!(rust_type_name("Idle", loc()).unwrap(), "Idle");
    }

    /// Регистр снимает коллизию с ключевым словом: `type` -> `Type`.
    #[test]
    fn type_name_keyword_is_saved_by_case() {
        assert_eq!(rust_type_name("type", loc()).unwrap(), "Type");
        assert_eq!(rust_type_name("match", loc()).unwrap(), "Match");
        assert_eq!(rust_type_name("loop", loc()).unwrap(), "Loop");
    }

    /// Переменная-ключевое слово спасается raw-идентификатором.
    #[test]
    fn value_name_keyword_is_saved_by_raw() {
        assert_eq!(rust_value_name("type", loc()).unwrap(), "r#type");
        assert_eq!(rust_value_name("match", loc()).unwrap(), "r#match");
        assert_eq!(rust_value_name("fn", loc()).unwrap(), "r#fn");
    }

    /// Обычное имя переменной raw-идентификатором не оборачивается.
    #[test]
    fn value_name_plain_is_untouched() {
        assert_eq!(
            rust_value_name("CurrentFloor", loc()).unwrap(),
            "current_floor"
        );
        assert_eq!(rust_value_name("counter", loc()).unwrap(), "counter");
    }

    /// **Контрпример:** `Self` не спасается ничем -> `RS-004`.
    #[test]
    fn self_as_type_name_is_rs004() {
        let err = rust_type_name("Self", loc()).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("RS-004"));
    }

    /// `self` в позиции значения - тоже `RS-004`.
    #[test]
    fn self_as_value_name_is_rs004() {
        let err = rust_value_name("self", loc()).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("RS-004"));
    }

    /// Строчное `self` в позиции типа даёт `Self` - та же диагностика.
    ///
    /// CamelCase от `self` - это и есть `Self`, поэтому регистр здесь не лечение, а
    /// причина.
    #[test]
    fn lowercase_self_as_type_name_is_rs004() {
        let err = rust_type_name("self", loc()).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("RS-004"));
        assert!(
            err.message.contains("Self"),
            "сообщение должно называть порождённый идентификатор: {}",
            err.message
        );
    }

    /// `crate`/`super` в позиции значения raw-идентификатором не спасаются.
    #[test]
    fn crate_and_super_as_value_names_are_rs004() {
        assert_eq!(
            rust_value_name("crate", loc()).unwrap_err().code.as_deref(),
            Some("RS-004")
        );
        assert_eq!(
            rust_value_name("super", loc()).unwrap_err().code.as_deref(),
            Some("RS-004")
        );
    }

    /// Имена стандартной библиотеки Rust законны: `Box` и `Option` диагностики не дают.
    ///
    /// Ловушка имён есть у цели `st` - там модель `Concat` ломает вывод, - и переносить
    /// её сюда нельзя.
    #[test]
    fn prelude_names_are_not_an_error() {
        assert_eq!(rust_type_name("Box", loc()).unwrap(), "Box");
        assert_eq!(rust_type_name("Option", loc()).unwrap(), "Option");
        assert_eq!(rust_type_name("String", loc()).unwrap(), "String");
    }

    /// Различные имена коллизии не дают.
    #[test]
    fn distinct_names_do_not_collide() {
        let names = vec![
            ("a_b".to_string(), "AB".to_string()),
            ("c_d".to_string(), "CD".to_string()),
        ];
        assert!(check_name_collisions(&names, "состояния", loc()).is_ok());
    }

    /// **Контрпример:** имена, слипающиеся после приведения регистра -> `RS-005`.
    #[test]
    fn colliding_names_are_rs005() {
        let names = vec![
            ("floor_sensor".to_string(), "FloorSensor".to_string()),
            ("FloorSensor".to_string(), "FloorSensor".to_string()),
        ];
        let err = check_name_collisions(&names, "состояния", loc()).unwrap_err();
        assert_eq!(err.code.as_deref(), Some("RS-005"));
        assert!(
            err.message.contains("floor_sensor") && err.message.contains("FloorSensor"),
            "сообщение должно называть оба исходных имени: {}",
            err.message
        );
    }
}

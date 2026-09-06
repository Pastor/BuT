//! Заглушка неиспользуемого параметра порождённых функций.
//!
//! Функции цели `c` следуют единому протоколу вызова: под-модель получает указатель на
//! корень (`main`), пользовательская функция - указатель на состояние (`model`),
//! помощник HAL - `m`. Протокол единообразен намеренно: печать вызовов, предобъявлений
//! и определений идёт из четырёх модулей, и сигнатура, зависящая от тела, потребовала
//! бы согласовать их все (разбор -, Option A).
//!
//! Но тело **не всегда** пользуется параметром - и тогда `cc -Wall -Wextra` говорит
//! `-Wunused-parameter`.
//!
//! Решение: там, где тело параметром не пользуется, первой строкой печатается
//! `(void)параметр;` - идиома C, гасящая предупреждение и честно говорящая читателю
//! "протокол требует, тело не пользуется".

/// Печатается ли `(void)<param>;` перед телом функции.
///
/// `body` - уже напечатанное тело (без сигнатуры). Возвращает `true`, если параметр
/// телом **не** используется.
pub(in crate::generator::c) fn is_unused(body: &str, param: &str) -> bool {
    !body
        .lines()
        // Строка `assert(...)` исчезает под `-DNDEBUG` - упоминание в ней
        // использованием не является (см. шапку модуля).
        .filter(|line| !line.trim_start().starts_with("assert("))
        .any(|line| mentions(line, param))
}

/// Строка-заглушка для параметра.
pub(in crate::generator::c) fn unused_guard(param: &str) -> String {
    format!("(void){param};")
}

/// Встречается ли `ident` в строке **как отдельный идентификатор**.
fn mentions(line: &str, ident: &str) -> bool {
    let bytes = line.as_bytes();
    let mut from = 0;
    while let Some(pos) = line[from..].find(ident) {
        let start = from + pos;
        let end = start + ident.len();
        let before_ok = start == 0 || !is_ident_byte(bytes[start - 1]);
        let after_ok = end == bytes.len() || !is_ident_byte(bytes[end]);
        if before_ok && after_ok {
            return true;
        }
        from = end;
    }
    false
}

/// Байт, который может входить в идентификатор C.
fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_use_is_seen() {
        assert!(!is_unused("    model->state = 1;\n", "model"));
        assert!(!is_unused("    Child_tick(&model->c, main);\n", "main"));
    }

    #[test]
    fn absent_parameter_is_unused() {
        assert!(is_unused("    return model->state == END;\n", "main"));
        assert!(is_unused("", "model"));
    }

    /// Границы идентификатора: `domain` - не `main`, `model_state` - не `model`.
    #[test]
    fn substring_is_not_a_use() {
        assert!(is_unused("    uint8_t domain = 0;\n", "main"));
        assert!(is_unused("    x = MAIN_STATE;\n", "main"));
        assert!(is_unused("    int model_state = 0;\n", "model"));
    }

    /// `assert` исчезает под `-DNDEBUG` - использованием не считается.
    #[test]
    fn assert_is_not_a_use() {
        assert!(is_unused("    assert(0 != main);\n    return 1;\n", "main"));
        assert!(!is_unused(
            "    assert(0 != main);\n    main->x = 1;\n",
            "main"
        ));
    }

    #[test]
    fn guard_text() {
        assert_eq!(unused_guard("main"), "(void)main;");
    }
}

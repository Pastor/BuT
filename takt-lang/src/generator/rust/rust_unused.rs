//! Заглушка неиспользуемого параметра функции - цель `rust`.
//!
//! `fn constant(v: u8) -> u8 { return 7; }` даёт `unused_variables`, то есть под `-D
//! warnings` порождённый модуль **не собирается** - при **нулевом** коде возврата
//! `taktc`. У цели `c` тот же класс закрыт идиомой `(void)параметр;`; здесь идиома
//! другая - `let _ = v;`, - а признак "параметр использован" тот же: вопрос задаётся
//! **напечатанному тексту** тела.
//!
//! Имя параметра в сигнатуре остаётся прежним. Форма `_v` тоже гасит предупреждение, но
//! меняет **видимое** имя: сигнатура порождённого модуля читается человеком, и
//! параметр, названный автором `v`, должен остаться `v`.
//!
//! Признак смотрит на текст, а не на семантику - как и у цели `c`: это снимает
//! транзитивность (тело, зовущее другую функцию с этим параметром, упоминает его
//! текстом).

/// Печатается ли заглушка для параметра `param`.
///
/// `body` - уже напечатанное тело функции.
pub(crate) fn is_unused(body: &str, param: &str) -> bool {
    !body.lines().any(|line| mentions(line, param))
}

/// Строка-заглушка.
pub(crate) fn guard(param: &str) -> String {
    format!("let _ = {param};")
}

/// Встречается ли `ident` в строке **как отдельный идентификатор**.
///
/// Границы обязательны: параметр `v` содержится в `value`, и поиск подстрокой заглушил
/// бы заглушку там, где она нужна.
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

/// Байт, который может входить в идентификатор Rust.
fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_use_is_seen() {
        assert!(!is_unused("    return v;\n", "v"));
    }

    #[test]
    fn absent_parameter_is_unused() {
        assert!(is_unused("    return 7;\n", "v"));
    }

    /// Границы идентификатора: `value` - не `v`.
    #[test]
    fn substring_is_not_a_use() {
        assert!(is_unused("    return value;\n", "v"));
    }
}

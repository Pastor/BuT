//! Текст вывода цели без комментариев - общий помощник сверок.

/// Текст без комментариев целевого языка: `//`, `/* ... */` и `(* ... *)`.
///
/// Строковые литералы не разбираются: в порождённом коде их нет ни у одной цели, а
/// разбор ради несуществующего случая усложнил бы помощник.
pub fn code_only(text: &str) -> String {
    let without_blocks = strip_pairs(&strip_pairs(text, "/*", "*/"), "(*", "*)");
    without_blocks
        .lines()
        .filter(|l| !l.trim_start().starts_with("//"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Вырезает парные фрагменты; незакрытый - до конца текста.
fn strip_pairs(text: &str, open: &str, close: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(at) = rest.find(open) {
        out.push_str(&rest[..at]);
        let Some(end) = rest[at + open.len()..].find(close) else {
            return out;
        };
        rest = &rest[at + open.len() + end + close.len()..];
    }
    out.push_str(rest);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Комментарии уходят, код остаётся - обе формы.
    #[test]
    fn comments_go_and_code_stays() {
        let st = "(*\n * профиль «такты» не эмитит TON\n *)\nx : USINT;\n";
        assert!(!code_only(st).contains("TON"));
        assert!(code_only(st).contains("x : USINT;"));

        let c = "// значение считает floor(…)\nint x = 1;\n";
        assert!(!code_only(c).contains("floor("));
        assert!(code_only(c).contains("int x = 1;"));
    }

    /// Незакрытый комментарий не роняет помощник и не отдаёт свой текст.
    #[test]
    fn an_unclosed_comment_does_not_leak() {
        assert!(!code_only("(* хвост без закрытия TON").contains("TON"));
    }
}

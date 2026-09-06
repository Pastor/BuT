//! Признаки ветвей `match` - общий носитель (0509, 0514).
//!
//! Автор вправе написать ветвь без операторов: `_ => {}` ("прочие значения ничего не
//! делают") - запись из практики, она стоит в примерах документа. Эталон и цели
//! `c`/`sv` её принимают, а инструменты двух других - нет:
//!
//! | Цель | Ответ инструмента до 0509 |
//! |---|---|
//! | `rust` | `clippy`: "this `else` branch is empty" - отказ под `-D warnings` |
//! | `st`, `st-at` | `iec2c`: "no statement defined after 'ELSE'/'THEN'" |
//!
//! Код возврата `taktc` - **нулевой**. Пустого оператора в IEC 61131-3 нет
//! вовсе (`;` MatIEC отвергает), поэтому у цели `st` пустая ветвь
//! **опускается**, а не наполняется заглушкой.
//!
//! Второй признак - "образец повторяет более ранний": такая ветвь недостижима, `match`
//! берёт первое совпадение. Её не печатает ни одна цель, чей инструмент дубль
//! отвергает, и о ней предупреждает `SE-131`.

use crate::semantic::{MatchArmNode, MatchPatternNode};

/// Повторяется ли хотя бы один образец ветви `index` в предыдущих ветвях.
///
/// Такая ветвь недостижима: `match` берёт первое совпадение. Признак нужен диагностике
/// `SE-131` и печатникам целей `c` и `rust` - их инструменты дубль отвергают (`cc`:
/// "duplicate case value", `clippy`: "these `if` branches have the same condition") при
/// Нулевом коде возврата `taktc`.
pub(crate) fn pattern_repeats_above(arms: &[MatchArmNode], index: usize) -> bool {
    let Some(arm) = arms.get(index) else {
        return false;
    };
    arms.iter().take(index).any(|earlier| {
        earlier
            .patterns
            .iter()
            .any(|ep| arm.patterns.iter().any(|p| same_pattern(p, ep)))
    })
}

/// Совпадают ли образцы. `_` совпадает с `_`: вторая ветвь по умолчанию тоже
/// недостижима.
fn same_pattern(a: &MatchPatternNode, b: &MatchPatternNode) -> bool {
    match (a, b) {
        (MatchPatternNode::Value(x), MatchPatternNode::Value(y)) => x == y,
        (MatchPatternNode::Wildcard, MatchPatternNode::Wildcard) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::pattern_repeats_above;
    use crate::semantic::{ExpressionNode, MatchArmNode, MatchPatternNode, StatementNode};

    fn arm(value: i128) -> MatchArmNode {
        MatchArmNode {
            patterns: vec![MatchPatternNode::Value(Box::new(ExpressionNode::Number(
                value,
            )))],
            body: Box::new(StatementNode::Block(Vec::new())),
            loc: crate::diagnostics::Location::Builtin,
        }
    }

    #[test]
    fn distinct_patterns_do_not_repeat() {
        let arms = vec![arm(1), arm(2)];
        assert!(!pattern_repeats_above(&arms, 1));
    }

    /// Дубль выше - эта ветвь недостижима: `match` берёт первое совпадение.
    #[test]
    fn duplicate_above_is_seen() {
        let arms = vec![arm(1), arm(1)];
        assert!(pattern_repeats_above(&arms, 1));
    }

    /// Первая ветвь достижима всегда: выше неё ничего нет.
    #[test]
    fn first_arm_is_never_unreachable() {
        let arms = vec![arm(1), arm(1)];
        assert!(!pattern_repeats_above(&arms, 0));
    }
}

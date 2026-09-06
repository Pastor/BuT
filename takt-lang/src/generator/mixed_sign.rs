//! Сравнение операндов разной знаковости - общий носитель целей.
//!
//! # Правило
//!
//! Операнды приводятся к типу, вмещающему **оба** значения, - как продвижение в C,
//! которое уже даёт эталон. Если такого типа в целевом языке нет (беззнаковый 64 бита
//! против знакового), сравнение раскрывается **проверкой знака**: отрицательное меньше
//! любого беззнакового.
//!
//! Раскрытие печатает операнд **дважды**, и это безопасно: в условии Takt эффектов не
//! бывает - присваивание есть оператор, а не выражение.

use crate::semantic::type_node::TypeNode;
use crate::semantic::{ConditionNode, ExpressionNode, VariableNode};

/// Что делать с парой операндов сравнения.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Plan {
    /// Знаковости совпадают либо тип не выводится - печать как прежде.
    AsIs,
    /// Оба операнда приводятся к знаковому типу ширины `bits`.
    Widen { bits: u16 },
    /// Общего типа нет: сравнение раскрывается проверкой знака.
    ///
    /// `signed_is_left` - с какой стороны стоит знаковый операнд.
    SignGuard { signed_is_left: bool },
}

/// Ширина и знаковость целочисленного типа, если он целочисленный.
///
/// `[bit;N]` сюда **не входит**: он беззнаков по построению, но смешения с ним в замере
/// не наблюдалось, а лишняя ветвь означала бы правило без проверенного входа.
fn int_facts(ty: &TypeNode) -> Option<(u16, bool)> {
    match ty {
        TypeNode::Integer { bits, signed } => Some((u16::from(*bits), *signed)),
        _ => None,
    }
}

/// План печати сравнения по типам операндов.
///
/// `None` у типа означает "не выводится" - тогда печать прежняя: ложное приведение
/// опаснее пропуска (за пропуском стоит прежнее поведение, за приведением - молча
/// изменённое значение).
pub(crate) fn plan(lhs: Option<&TypeNode>, rhs: Option<&TypeNode>) -> Plan {
    let (Some(l), Some(r)) = (lhs.and_then(int_facts), rhs.and_then(int_facts)) else {
        return Plan::AsIs;
    };
    let ((sw, _), (uw, _), signed_is_left) = if l.1 && !r.1 {
        (l, r, true)
    } else if r.1 && !l.1 {
        (r, l, false)
    } else {
        return Plan::AsIs;
    };
    // Знаковый тип обязан вместить и беззнаковое значение: ему нужен лишний разряд под
    // знак.
    let need = sw.max(uw.saturating_add(1));
    match need {
        n if n <= 8 => Plan::Widen { bits: 8 },
        n if n <= 16 => Plan::Widen { bits: 16 },
        n if n <= 32 => Plan::Widen { bits: 32 },
        n if n <= 64 => Plan::Widen { bits: 64 },
        // `u64` против знакового: знакового 65-битного типа нет ни в одной цели, и
        // правило раскрывается проверкой знака.
        _ => Plan::SignGuard { signed_is_left },
    }
}

/// Истинно ли сравнение уже потому, что знаковый операнд **отрицателен**.
///
/// Правило разное по операторам и по стороне знакового операнда, и держать его копиями
/// в четырёх целях значило бы разойтись - что и случилось: первая редакция 0359 не
/// покрыла `=`/`!=` вовсе, и `sv` считал `-1 == 255` истиной (проверено прогоном
/// verilator), `rust` и `st` не собирались.
///
/// | Оператор | Знаковый слева | Знаковый справа |
/// |---|---|---|
/// | `<`, `<=` | да: отрицательное меньше любого беззнакового | нет |
/// | `>`, `>=` | нет | да |
/// | `!=` | да: отрицательное не равно ничему беззнаковому | да |
/// | `==` | нет | нет |
pub(crate) fn negative_wins(op: &str, signed_is_left: bool) -> bool {
    match op {
        "<" | "<=" => signed_is_left,
        ">" | ">=" => !signed_is_left,
        // Неравенство истинно при любом отрицательном знаковом.
        "!=" | "<>" => true,
        // Равенство при отрицательном знаковом ложно всегда.
        _ => false,
    }
}

/// Тип операнда-выражения для правила знаковости.
///
/// **Только именованное значение** - переменная, порт, константа (и скобки над ними).
/// Литерал сюда не входит **намеренно**: у него знаковости нет, он подстраивается под
/// приёмник, и приведение `3 as i32` - это `clippy::unnecessary_cast`, то есть отказ
/// проверки цели `rust`.
pub(crate) fn operand_type_expr(expr: &ExpressionNode) -> Option<TypeNode> {
    match expr {
        ExpressionNode::Variable(var) => named_type(&var.borrow()),
        ExpressionNode::Parenthesis(inner) => operand_type_expr(inner),
        _ => None,
    }
}

/// То же для условия: у условий своё дерево.
pub(crate) fn operand_type_cond(cond: &ConditionNode) -> Option<TypeNode> {
    match cond {
        ConditionNode::Variable(var, _) => named_type(&var.borrow()),
        ConditionNode::Parenthesis(inner) => operand_type_cond(inner),
        _ => None,
    }
}

fn named_type(var: &VariableNode) -> Option<TypeNode> {
    match var {
        VariableNode::Simple { ty, .. }
        | VariableNode::Port { ty, .. }
        | VariableNode::Const { ty, .. } => Some(ty.clone()),
        VariableNode::Unresolved => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{Plan, plan};
    use crate::semantic::type_node::TypeNode;

    fn int(bits: u8, signed: bool) -> TypeNode {
        TypeNode::Integer { bits, signed }
    }

    #[test]
    fn same_signedness_is_left_as_is() {
        assert_eq!(
            plan(Some(&int(8, false)), Some(&int(16, false))),
            Plan::AsIs,
            "оба беззнаковых — правило не при чём"
        );
        assert_eq!(
            plan(Some(&int(8, true)), Some(&int(64, true))),
            Plan::AsIs,
            "оба знаковых"
        );
    }

    #[test]
    fn mixed_widens_to_type_that_holds_both() {
        assert_eq!(
            plan(Some(&int(8, true)), Some(&int(8, false))),
            Plan::Widen { bits: 16 },
            "`u8` требует девяти разрядов со знаком — значит i16"
        );
        assert_eq!(
            plan(Some(&int(32, true)), Some(&int(16, false))),
            Plan::Widen { bits: 32 },
            "знаковый уже вмещает"
        );
    }

    /// `u64` не помещается ни в один знаковый тип целевых языков.
    #[test]
    fn u64_against_signed_needs_sign_guard() {
        assert_eq!(
            plan(Some(&int(8, true)), Some(&int(64, false))),
            Plan::SignGuard {
                signed_is_left: true
            }
        );
        assert_eq!(
            plan(Some(&int(64, false)), Some(&int(64, true))),
            Plan::SignGuard {
                signed_is_left: false
            },
            "сторона знакового операнда важна для раскрытия"
        );
    }

    /// Правило "отрицательное решает исход" - по оператору и стороне.
    #[test]
    fn negative_wins_covers_equality() {
        use super::negative_wins;
        assert!(
            negative_wins("<", true),
            "отрицательное меньше беззнакового"
        );
        assert!(
            !negative_wins("<", false),
            "беззнаковое не меньше отрицательного"
        );
        assert!(negative_wins(">", false));
        assert!(!negative_wins(">", true));
        assert!(
            negative_wins("!=", true) && negative_wins("!=", false),
            "неравенство истинно при отрицательном с любой стороны"
        );
        assert!(
            !negative_wins("==", true) && !negative_wins("==", false),
            "равенство при отрицательном знаковом ложно всегда"
        );
    }

    /// Невыводимый тип оставляет печать прежней.
    #[test]
    fn unknown_type_is_left_as_is() {
        assert_eq!(plan(None, Some(&int(8, false))), Plan::AsIs);
        assert_eq!(plan(Some(&int(8, true)), None), Plan::AsIs);
        assert_eq!(plan(Some(&TypeNode::Bit), Some(&int(8, true))), Plan::AsIs);
    }
}

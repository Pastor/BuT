//! Константные условия перехода (SE-047).
//!
//! Часть модуля `validate`.

use super::*;

/// SE-047: предупреждения об очевидно константных условиях переходов.
///
/// Обнаруживает сравнения двух числовых/булевых литералов, результат
/// которых известен в compile-time:
/// - `1 = 0` - всегда ложно (переход никогда не произойдёт);
/// - `1 = 1` - всегда истинно (переход безусловный);
/// - `x = 5 & x = 6` - второе сравнение с той же переменной делает
///   конъюнкцию всегда ложной.
pub fn check_constant_conditions(model: &Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    collect_constant_condition_warnings(model, &mut diags);
    diags
}

fn collect_constant_condition_warnings(model: &Rc<RefCell<ModelNode>>, out: &mut Vec<Diagnostic>) {
    let borrowed = model.borrow();

    for state in borrowed.states.values() {
        match state {
            StateNode::Simple { references, .. } => {
                for r in references {
                    eval_condition_const(&r.cond, r.location, out);
                }
            }
            StateNode::Implement {
                references, next, ..
            } => {
                for r in references {
                    eval_condition_const(&r.cond, r.location, out);
                }
                if let Some(nr) = next {
                    eval_condition_const(&nr.cond, nr.location, out);
                }
            }
            StateNode::Unresolved => {}
        }
    }

    let nested: Vec<Rc<RefCell<ModelNode>>> = borrowed.models.values().map(Rc::clone).collect();
    drop(borrowed);
    for m in nested {
        collect_constant_condition_warnings(&m, out);
    }
}

/// Вычисляет условие и, если оно константно, добавляет предупреждение.
fn eval_condition_const(cond: &ConditionNode, loc: Location, out: &mut Vec<Diagnostic>) {
    match eval_const_value(cond) {
        Some(true) => {
            out.push(
                Diagnostic::warning(
                    loc,
                    "условие перехода всегда истинно — переход безусловный".to_string(),
                )
                .with_code("SE-047"),
            );
        }
        Some(false) => {
            out.push(
                Diagnostic::warning(
                    loc,
                    "условие перехода всегда ложно — переход недостижим".to_string(),
                )
                .with_code("SE-047"),
            );
        }
        None => {}
    }
}

/// Пытается вычислить булево значение условия из одних литералов. Возвращает
/// `Some(true/false)` только если результат очевиден статически.
fn eval_const_value(cond: &ConditionNode) -> Option<bool> {
    match cond {
        ConditionNode::Bool(b) => Some(*b),
        ConditionNode::Number(n) => Some(*n != 0),

        ConditionNode::Equal(l, r) => match (eval_literal_int(l), eval_literal_int(r)) {
            (Some(a), Some(b)) => Some(a == b),
            _ => None,
        },
        ConditionNode::NotEqual(l, r) => match (eval_literal_int(l), eval_literal_int(r)) {
            (Some(a), Some(b)) => Some(a != b),
            _ => None,
        },
        ConditionNode::Less(l, r) => match (eval_literal_int(l), eval_literal_int(r)) {
            (Some(a), Some(b)) => Some(a < b),
            _ => None,
        },
        ConditionNode::More(l, r) => match (eval_literal_int(l), eval_literal_int(r)) {
            (Some(a), Some(b)) => Some(a > b),
            _ => None,
        },
        ConditionNode::LessEqual(l, r) => match (eval_literal_int(l), eval_literal_int(r)) {
            (Some(a), Some(b)) => Some(a <= b),
            _ => None,
        },
        ConditionNode::MoreEqual(l, r) => match (eval_literal_int(l), eval_literal_int(r)) {
            (Some(a), Some(b)) => Some(a >= b),
            _ => None,
        },

        // Конъюнкция: если хоть одна ветка константно ложна - всё ложно
        ConditionNode::And(l, r) => match (eval_const_value(l), eval_const_value(r)) {
            (Some(false), _) | (_, Some(false)) => Some(false),
            (Some(true), Some(true)) => Some(true),
            _ => None,
        },
        // Дизъюнкция: если хоть одна ветка константно истинна - всё истинно
        ConditionNode::Or(l, r) => match (eval_const_value(l), eval_const_value(r)) {
            (Some(true), _) | (_, Some(true)) => Some(true),
            (Some(false), Some(false)) => Some(false),
            _ => None,
        },

        ConditionNode::Not(inner) => eval_const_value(inner).map(|v| !v),
        ConditionNode::Parenthesis(inner) => eval_const_value(inner),

        _ => None,
    }
}

/// Возвращает числовое значение условия, если оно является числовым литералом.
fn eval_literal_int(cond: &ConditionNode) -> Option<i128> {
    match cond {
        ConditionNode::Number(n) => Some(*n),
        ConditionNode::Bool(b) => Some(if *b { 1 } else { 0 }),
        ConditionNode::Parenthesis(inner) => eval_literal_int(inner),
        _ => None,
    }
}

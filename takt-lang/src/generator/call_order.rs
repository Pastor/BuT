//! Порядок печати функций — по зависимостям вызова (фича 0344).
//!
//! В IEC 61131-3 опережающих ссылок нет: `FUNCTION`, вызывающая другую,
//! обязана стоять **после** неё. Порядок был алфавитным, и замер 2026-08-20
//! показал цену: `fn quad` вызывает `fn twice`, а `Loops_quad` печаталась
//! первой — `iec2c` отвечал «';' missing at the end of statement» (сообщение,
//! по которому исходную причину не опознать) при **нулевом** коде возврата
//! `taktc`.
//!
//! # Почему это возможно
//!
//! Граф вызовов **ацикличен**: рекурсию запрещает семантика (`SE-053`,
//! «Рекурсия функций запрещена: цепочка вызовов …»). Значит топологический
//! порядок существует всегда, и носителю не нужно решать, что делать с циклом,
//! — он лишь переживает его без дублей, как и порядок структур (0341).
//!
//! # Устойчивость
//!
//! Обход идёт по **алфавитному** списку имён, зависимости поднимаются выше:
//! порядок детерминирован (инвариант 0048).

use crate::semantic::{FunctionDefinitionNode, StatementNode};
use std::collections::{BTreeMap, BTreeSet};

/// Имена функций в порядке зависимостей: вызываемая раньше вызывающей.
pub(crate) fn sorted(defs: &BTreeMap<String, FunctionDefinitionNode>) -> Vec<String> {
    let mut out = Vec::new();
    let mut done = BTreeSet::new();
    let mut active = BTreeSet::new();
    for name in defs.keys() {
        visit(name, defs, &mut done, &mut active, &mut out);
    }
    out
}

/// Обход в глубину: сперва вызываемые, потом сама функция.
fn visit(
    name: &str,
    defs: &BTreeMap<String, FunctionDefinitionNode>,
    done: &mut BTreeSet<String>,
    active: &mut BTreeSet<String>,
    out: &mut Vec<String>,
) {
    if done.contains(name) || !active.insert(name.to_string()) {
        return;
    }
    if let Some(FunctionDefinitionNode::Local { body, .. }) = defs.get(name) {
        let mut called = BTreeSet::new();
        collect_calls(body, &mut called);
        for callee in called {
            if defs.contains_key(&callee) {
                visit(&callee, defs, done, active, out);
            }
        }
    }
    if defs.contains_key(name) && done.insert(name.to_string()) {
        out.push(name.to_string());
    }
    active.remove(name);
}

/// Имена функций, вызываемых телом.
fn collect_calls(stmt: &StatementNode, out: &mut BTreeSet<String>) {
    let mut expressions = Vec::new();
    walk_statement(stmt, &mut expressions);
    for expr in expressions {
        collect_calls_expr(&expr, out);
    }
}

/// Обход операторов: собирает выражения тела.
///
/// ⚠️ Разбор **не** исчерпывающий: пропущенная форма даёт прежний алфавитный
/// порядок, то есть прежнее поведение, а не молчаливую порчу вывода.
fn walk_statement(stmt: &StatementNode, exprs: &mut Vec<crate::semantic::ExpressionNode>) {
    match stmt {
        StatementNode::Block(items) => {
            for item in items {
                walk_statement(item, exprs);
            }
        }
        StatementNode::Expression(expr, _) => exprs.push((**expr).clone()),
        StatementNode::Return(Some(expr), _) => exprs.push((**expr).clone()),
        StatementNode::Variable(_, _, Some(expr), _) => exprs.push((**expr).clone()),
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            exprs.push((**cond).clone());
            walk_statement(then_, exprs);
            if let Some(other) = else_ {
                walk_statement(other, exprs);
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(cond) = cond {
                exprs.push((**cond).clone());
            }
            walk_statement(body, exprs);
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(init) = init {
                walk_statement(init, exprs);
            }
            if let Some(cond) = cond {
                exprs.push((**cond).clone());
            }
            if let Some(step) = step {
                exprs.push((**step).clone());
            }
            walk_statement(body, exprs);
        }
        StatementNode::Match { expr, arms, .. } => {
            exprs.push((**expr).clone());
            for arm in arms {
                walk_statement(&arm.body, exprs);
            }
        }
        _ => {}
    }
}

/// Имена функций, вызываемых выражением.
fn collect_calls_expr(expr: &crate::semantic::ExpressionNode, out: &mut BTreeSet<String>) {
    use crate::semantic::ExpressionNode as E;
    if let E::Function(def, args) = expr {
        if let FunctionDefinitionNode::Local { name, .. } = &*def.borrow() {
            out.insert(name.clone());
        }
        for arg in args {
            collect_calls_expr(arg, out);
        }
        return;
    }
    for child in children(expr) {
        collect_calls_expr(&child, out);
    }
}

/// Подвыражения узла (плоский разбор бинарных и унарных форм).
fn children(expr: &crate::semantic::ExpressionNode) -> Vec<crate::semantic::ExpressionNode> {
    use crate::semantic::ExpressionNode as E;
    match expr {
        E::Add(a, b)
        | E::Subtract(a, b)
        | E::Multiply(a, b)
        | E::Divide(a, b)
        | E::Modulo(a, b)
        | E::Power(a, b)
        | E::ShiftLeft(a, b)
        | E::ShiftRight(a, b)
        | E::BitwiseAnd(a, b)
        | E::BitwiseOr(a, b)
        | E::BitwiseXor(a, b)
        | E::And(a, b)
        | E::Or(a, b)
        | E::Equal(a, b)
        | E::NotEqual(a, b)
        | E::Less(a, b)
        | E::LessEqual(a, b)
        | E::More(a, b)
        | E::MoreEqual(a, b)
        | E::Assign(a, b) => vec![(**a).clone(), (**b).clone()],
        E::Not(a) | E::BitwiseNot(a) | E::Negate(a) | E::Parenthesis(a) | E::Cast(a, _) => {
            vec![(**a).clone()]
        }
        E::ConditionalOperator(c, t, e) => vec![(**c).clone(), (**t).clone(), (**e).clone()],
        E::Initializer(items) | E::Array(items) => items.clone(),
        _ => Vec::new(),
    }
}

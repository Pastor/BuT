//! Имена, которым тело присваивает, - для выбора `let` против `let mut`.
//!
//! В Takt изменяемость не объявляется (`var` изменяем всегда), а в Rust лишний `mut` -
//! это `unused_mut`, то есть отказ проверки цели. Вывести признак "по объявлению" нельзя:
//! `var delta := 1;`, которому больше не присваивают, обязан стать `let`. Поэтому тело
//! обходится **заранее**, целиком: печать потоковая, и в точке `let` будущие
//! присваивания ещё не видны.
//!
//! Модуль отделён от печати операторов по границе ответственности: здесь - только сбор
//! имён, ни одной строки вывода.

use std::collections::BTreeSet;

use crate::semantic::{ExpressionNode, StatementNode};

/// Собирает имена, которым в теле хоть раз присваивают.
///
/// Нужно, чтобы решить, печатать ли `let` или `let mut`. В Takt изменяемость не
/// объявляется - `var` изменяем всегда, - а в Rust лишний `mut` это `unused_mut`, то
/// есть отказ проверки. Вывести его "по объявлению" нельзя: `var delta := 1;`, которому
/// больше не присваивают, обязан стать `let`, а не `let mut`.
///
/// Обход идёт по **всему** телу заранее: печать потоковая, и в точке `let` будущие
/// присваивания ещё не видны.
pub(in crate::generator::rust) fn collect_assigned(
    stmt: &StatementNode,
    out: &mut BTreeSet<String>,
) {
    match stmt {
        StatementNode::Block(items) => items.iter().for_each(|i| collect_assigned(i, out)),
        StatementNode::Expression(expr, _) => collect_assigned_expr(expr, out),
        StatementNode::Variable(_, _, Some(init), _) => collect_assigned_expr(init, out),
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            collect_assigned_expr(cond, out);
            collect_assigned(then_, out);
            if let Some(alt) = else_ {
                collect_assigned(alt, out);
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(cond) = cond {
                collect_assigned_expr(cond, out);
            }
            collect_assigned(body, out);
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(init) = init {
                collect_assigned(init, out);
            }
            if let Some(cond) = cond {
                collect_assigned_expr(cond, out);
            }
            if let Some(step) = step {
                collect_assigned_expr(step, out);
            }
            collect_assigned(body, out);
        }
        StatementNode::Match { expr, arms, .. } => {
            collect_assigned_expr(expr, out);
            arms.iter().for_each(|a| collect_assigned(&a.body, out));
        }
        StatementNode::Return(Some(expr), _) => collect_assigned_expr(expr, out),
        // Тело вставки - операторы Takt: присваивания в нём считаются.
        StatementNode::Assembly { body, .. } => collect_assigned(body, out),
        StatementNode::Return(None, _)
        | StatementNode::Variable(_, _, None, _)
        | StatementNode::Continue(_)
        | StatementNode::Break(_)
        | StatementNode::Formula(_)
        | StatementNode::InlineFormula(_)
        | StatementNode::None
        | StatementNode::Unresolved(_) => {}
    }
}

/// Собирает имена, которым присваивают, из выражения.
fn collect_assigned_expr(expr: &ExpressionNode, out: &mut BTreeSet<String>) {
    if let ExpressionNode::Assign(target, value) = expr {
        if let ExpressionNode::Variable(var) = &**target {
            out.insert(var.borrow().name().to_string());
        }
        // Запись в поле структуры делает изменяемой саму переменную: `r.output := ...`
        // требует `let mut r`.
        if let ExpressionNode::BitAccess(base, crate::parser::ast::Member::Identifier(_)) =
            &**target
            && let ExpressionNode::Variable(var) = &**base
        {
            out.insert(var.borrow().name().to_string());
        }
        collect_assigned_expr(target, out);
        collect_assigned_expr(value, out);
        return;
    }
    // Присваивание - оператор, а не подвыражение: в Takt его негде спрятать глубже
    // одного уровня. Обход ограничен теми узлами, что реально несут вложенные
    // выражения-операторы.
    match expr {
        ExpressionNode::Parenthesis(inner) | ExpressionNode::CodeBlock(inner, _) => {
            collect_assigned_expr(inner, out)
        }
        ExpressionNode::ConditionalOperator(a, b, c) => {
            collect_assigned_expr(a, out);
            collect_assigned_expr(b, out);
            collect_assigned_expr(c, out);
        }
        _ => {}
    }
}

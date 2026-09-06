//! Граф вызовов функций модели.
//!
//! Строится по **сырому AST** тел функций (проход 5, до разрешения тел), чтобы
//! разрешать функции в **топологическом** порядке: вызываемая раньше вызывающей. Это
//! условие корректности при хранении функций по значению (см. ): точка вызова
//! встраивает копию уже разрешённого узла.
//!
//! Рёбра проводятся только к именам, объявленным как `fn` **в этой же модели**
//! (`local_names`). Вызовы функций родительских моделей рёбер не дают - они уже
//! разрешены стадийностью прохода 5 (карта родителя восстановлена до рекурсии во
//! вложенные модели). Встроенные функции (`min`, `abs`, ...) и `extern` тел не имеют и
//! в граф не входят.
//!
//! **Цикл** в графе (`f->f`, `f->g->f`, любой длины) - рекурсия, запрещённая
//! доменом - отвергается
//! диагностикой **SE-053** с цепочкой вызовов.

use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast;
use std::collections::{BTreeMap, BTreeSet};

/// Собирает имена функций, вызываемых из тела `stmt`, ограничиваясь именами из `local`.
/// Порядок сохраняется (для детерминизма и читаемой цепочки), дубликаты не отсеиваются -
/// их снимает построение графа.
fn callees_in_statement(stmt: &ast::Statement, local: &BTreeSet<String>, out: &mut Vec<String>) {
    match stmt {
        ast::Statement::Block { statements, .. } => {
            for s in statements {
                callees_in_statement(s, local, out);
            }
        }
        ast::Statement::Assembly { block, .. } => callees_in_statement(block, local, out),
        ast::Statement::If(_, cond, then_s, else_s) => {
            callees_in_expression(cond, local, out);
            callees_in_statement(then_s, local, out);
            if let Some(e) = else_s {
                callees_in_statement(e, local, out);
            }
        }
        ast::Statement::Loop(_, cond, body, _) => {
            if let Some(c) = cond {
                callees_in_expression(c, local, out);
            }
            callees_in_statement(body, local, out);
        }
        ast::Statement::Expression(_, e) => callees_in_expression(e, local, out),
        ast::Statement::Variable(_, _, Some(e)) => callees_in_expression(e, local, out),
        ast::Statement::For(_, init, cond, step, body) => {
            if let Some(s) = init {
                callees_in_statement(s, local, out);
            }
            if let Some(c) = cond {
                callees_in_expression(c, local, out);
            }
            if let Some(s) = step {
                callees_in_expression(s, local, out);
            }
            if let Some(b) = body {
                callees_in_statement(b, local, out);
            }
        }
        ast::Statement::Return(_, Some(e)) => callees_in_expression(e, local, out),
        ast::Statement::Match(_, scrutinee, arms) => {
            callees_in_expression(scrutinee, local, out);
            for arm in arms {
                for pat in &arm.patterns {
                    if let ast::MatchPattern::Value(e) = pat {
                        callees_in_expression(e, local, out);
                    }
                }
                callees_in_statement(&arm.body, local, out);
            }
        }
        // Листовые операторы без вложенных выражений/тел.
        ast::Statement::Variable(_, _, None)
        | ast::Statement::Continue(_)
        | ast::Statement::Break(_)
        | ast::Statement::Return(_, None)
        | ast::Statement::Error(_)
        | ast::Statement::StraySemicolon(_)
        | ast::Statement::Args(_, _)
        | ast::Statement::Formula { .. }
        | ast::Statement::InlineFormula(_) => {}
    }
}

/// Собирает имена вызываемых функций из выражения, ограничиваясь `local`.
fn callees_in_expression(expr: &ast::Expression, local: &BTreeSet<String>, out: &mut Vec<String>) {
    match expr {
        ast::Expression::Function(_, id, args) => {
            if local.contains(&id.name) {
                out.push(id.name.clone());
            }
            for a in args {
                callees_in_expression(a, local, out);
            }
        }
        ast::Expression::CodeBlock(_, inner, stmt) => {
            callees_in_expression(inner, local, out);
            callees_in_statement(stmt, local, out);
        }
        ast::Expression::NamedFunction(_, inner, _) => callees_in_expression(inner, local, out),
        ast::Expression::ArraySubscript(_, _, idx) => callees_in_expression(idx, local, out),
        ast::Expression::Parenthesis(_, e)
        | ast::Expression::BitAccess(_, e, _)
        | ast::Expression::Not(_, e)
        | ast::Expression::BitwiseNot(_, e)
        | ast::Expression::UnaryPlus(_, e)
        | ast::Expression::Negate(_, e)
        | ast::Expression::Cast(_, e, _) => callees_in_expression(e, local, out),
        ast::Expression::Power(_, l, r)
        | ast::Expression::Multiply(_, l, r)
        | ast::Expression::Divide(_, l, r)
        | ast::Expression::Modulo(_, l, r)
        | ast::Expression::Add(_, l, r)
        | ast::Expression::Subtract(_, l, r)
        | ast::Expression::ShiftLeft(_, l, r)
        | ast::Expression::ShiftRight(_, l, r)
        | ast::Expression::BitwiseAnd(_, l, r)
        | ast::Expression::BitwiseXor(_, l, r)
        | ast::Expression::BitwiseOr(_, l, r)
        | ast::Expression::Less(_, l, r)
        | ast::Expression::More(_, l, r)
        | ast::Expression::LessEqual(_, l, r)
        | ast::Expression::MoreEqual(_, l, r)
        | ast::Expression::Equal(_, l, r)
        | ast::Expression::NotEqual(_, l, r)
        | ast::Expression::And(_, l, r)
        | ast::Expression::Or(_, l, r)
        | ast::Expression::Assign(_, l, r) => {
            callees_in_expression(l, local, out);
            callees_in_expression(r, local, out);
        }
        ast::Expression::ConditionalOperator(_, c, t, e) => {
            callees_in_expression(c, local, out);
            callees_in_expression(t, local, out);
            callees_in_expression(e, local, out);
        }
        ast::Expression::Array(_, items) | ast::Expression::Initializer(_, items) => {
            for it in items {
                callees_in_expression(it, local, out);
            }
        }
        // Листовые выражения без вложенных вызовов.
        ast::Expression::ArraySlice(_, _, _, _)
        | ast::Expression::Number(_, _)
        | ast::Expression::Duration(_, _, _)
        | ast::Expression::Rational(_, _, _)
        | ast::Expression::String(_)
        | ast::Expression::Type(_, _)
        | ast::Expression::Address(_, _, _)
        // Анонимное обращение вызовов не содержит: адрес - литерал.
        | ast::Expression::AnonAddress(_, _, _)
        | ast::Expression::Bool(_, _)
        | ast::Expression::Variable(_)
        | ast::Expression::List(_, _) => {}
    }
}

/// Ребро графа: имя функции -> её вызываемые (внутри модели), в порядке появления без
/// дубликатов.
pub type CallGraph = BTreeMap<String, Vec<String>>;

/// Строит граф вызовов модели: для каждой локальной функции - список вызываемых
/// **локальных** функций (без дубликатов). `bodies` - тела в сыром AST по имени.
pub fn build_call_graph(bodies: &BTreeMap<String, Option<ast::Statement>>) -> CallGraph {
    let local: BTreeSet<String> = bodies.keys().cloned().collect();
    let mut graph = CallGraph::new();
    for (name, body) in bodies {
        let mut callees = Vec::new();
        if let Some(stmt) = body {
            callees_in_statement(stmt, &local, &mut callees);
        }
        // Дедупликация с сохранением порядка первого появления.
        let mut seen = BTreeSet::new();
        callees.retain(|c| seen.insert(c.clone()));
        graph.insert(name.clone(), callees);
    }
    graph
}

/// Цвет вершины при обходе DFS для поиска циклов.
#[derive(Clone, Copy, PartialEq)]
enum Color {
    White, // не посещалась
    Gray,  // в текущем стеке DFS
    Black, // полностью обработана
}

/// Возвращает функции в **топологическом** порядке (вызываемая раньше вызывающей) либо
/// диагностику `SE-053`, если в графе есть цикл (рекурсия).
///
/// `loc` - позиция для диагностики (объявление модели). Сообщение называет цепочку
/// вызовов, замыкающую цикл.
pub fn topological_order(graph: &CallGraph, loc: Location) -> Result<Vec<String>, Diagnostic> {
    let mut color: BTreeMap<String, Color> =
        graph.keys().map(|k| (k.clone(), Color::White)).collect();
    let mut order: Vec<String> = Vec::new();

    // Итеративный DFS с явным стеком пути - чтобы восстановить цепочку цикла.
    for start in graph.keys() {
        if color[start] != Color::White {
            continue;
        }
        // Стек кадров: (имя, индекс следующего необойдённого потомка).
        let mut stack: Vec<(String, usize)> = vec![(start.clone(), 0)];
        color.insert(start.clone(), Color::Gray);
        while let Some((node, idx)) = stack.last().cloned() {
            let callees = graph.get(&node).map(Vec::as_slice).unwrap_or(&[]);
            if idx < callees.len() {
                stack.last_mut().unwrap().1 += 1;
                let next = &callees[idx];
                match color.get(next).copied().unwrap_or(Color::Black) {
                    Color::White => {
                        color.insert(next.clone(), Color::Gray);
                        stack.push((next.clone(), 0));
                    }
                    Color::Gray => {
                        // Обратное ребро -> цикл. Восстанавливаем цепочку от `next` в
                        // стеке до текущей вершины и замыкаем на `next`.
                        let from = stack.iter().position(|(n, _)| n == next).unwrap_or(0);
                        let mut chain: Vec<String> =
                            stack[from..].iter().map(|(n, _)| n.clone()).collect();
                        chain.push(next.clone());
                        return Err(Diagnostic::error(
                            loc,
                            format!(
                                "Рекурсия функций запрещена: цепочка вызовов {}",
                                chain.join(" → ")
                            ),
                        )
                        .with_code("SE-053"));
                    }
                    Color::Black => {}
                }
            } else {
                color.insert(node.clone(), Color::Black);
                order.push(node.clone());
                stack.pop();
            }
        }
    }
    // `order` уже в постисправлениеном (топологическом) порядке: вызываемая раньше вызывающей.
    Ok(order)
}

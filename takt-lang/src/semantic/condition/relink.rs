//! Перепривязка вызовов функций в именованных условиях.
//!
//! # Правило
//!
//! После стадии 5 неразрешённое определение вызываемой функции заменяется тем, что
//! вернёт `search_func` теперь. Проход **идемпотентен** и **молчалив**: имя, которого
//! нет, остаётся как есть - о нём говорят прежние проверки (`SE-004`, отказы целей).
//!
//! Место в конвейере значимо: проход идёт сразу после стадии 5, то есть до стадии 4
//! (тела блоков) и стадии 6 (условия рёбер). Обе копируют значение именованного условия
//! себе, и перепривязка после них не дошла бы до копий.

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::semantic::{
    ConditionNode, ExpressionNode, FunctionDefinitionNode, ModelNode, StatementNode,
};

/// Перепривязывает вызовы функций в именованных условиях по всему дереву.
pub(crate) fn relink_condition_calls(root: &Rc<RefCell<ModelNode>>) {
    let mut visited = HashSet::new();
    relink_model(root, &mut visited);
}

fn relink_model(model: &Rc<RefCell<ModelNode>>, visited: &mut HashSet<*const RefCell<ModelNode>>) {
    if !visited.insert(Rc::as_ptr(model)) {
        return; // разделяемая под-модель уже обойдена
    }
    // Условия и функции обходятся В копии, а не изымаются: поиск функции читает ту же
    // модель, и пустая карта (после `mem::take`) вернула бы "функции нет" -
    // перепривязка молча не срабатывала бы там, где условие лежит в теле функции.
    let mut conditions = model.borrow().conditions.clone();
    for cond in conditions.values_mut() {
        relink_cond(&mut cond.value, model);
    }
    model.borrow_mut().conditions = conditions;

    // Тела функций уже построены на стадии 5 и держат свою копию именованного условия -
    // ячейку `ExpressionNode::Condition`. Правки карты им мало: `if warm { ... }` в теле
    // функции останется с неразрешённым вызовом. Прочие тела (блоки, рёбра) строятся
    // после этого прохода и копируют исправленную ячейку.
    let mut functions = model.borrow().functions.clone();
    for func in functions.values_mut() {
        if let FunctionDefinitionNode::Local { body, .. } = func {
            relink_stmt(body, model);
        }
    }
    model.borrow_mut().functions = functions;

    let nested: Vec<Rc<RefCell<ModelNode>>> = model.borrow().models.values().cloned().collect();
    for child in &nested {
        relink_model(child, visited);
    }
}

/// Заменяет неразрешённое определение вызываемой функции разрешённым.
fn relink_cond(cond: &mut ConditionNode, model: &Rc<RefCell<ModelNode>>) {
    match cond {
        ConditionNode::Function(def, args, _) => {
            let name: Option<String> = match &*def.borrow() {
                FunctionDefinitionNode::Unresolved(raw) => {
                    raw.name.as_ref().map(|id| id.name.clone())
                }
                _ => None,
            };
            if let Some(name) = name
                && let Some(found) = model.borrow().search_func(&name)
                && !matches!(&*found.borrow(), FunctionDefinitionNode::Unresolved(_))
            {
                *def = found;
            }
            for arg in args.iter_mut() {
                relink_cond(arg, model);
            }
        }
        ConditionNode::Parenthesis(inner)
        | ConditionNode::Not(inner)
        | ConditionNode::AfterExpr(inner)
        | ConditionNode::BitAccess(inner, _) => relink_cond(inner, model),
        ConditionNode::ArraySubscript(l, r)
        | ConditionNode::Add(l, r)
        | ConditionNode::Subtract(l, r)
        | ConditionNode::And(l, r)
        | ConditionNode::Or(l, r)
        | ConditionNode::Less(l, r)
        | ConditionNode::More(l, r)
        | ConditionNode::LessEqual(l, r)
        | ConditionNode::MoreEqual(l, r)
        | ConditionNode::Equal(l, r)
        | ConditionNode::NotEqual(l, r) => {
            relink_cond(l, model);
            relink_cond(r, model);
        }
        _ => {}
    }
}

/// Обходит тело функции в поисках ячеек именованного условия.
fn relink_stmt(stmt: &mut StatementNode, model: &Rc<RefCell<ModelNode>>) {
    match stmt {
        StatementNode::Block(items) => {
            for item in items.iter_mut() {
                relink_stmt(item, model);
            }
        }
        StatementNode::Expression(expr, _) => relink_expr(expr, model),
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            relink_expr(cond, model);
            relink_stmt(then_, model);
            if let Some(alt) = else_ {
                relink_stmt(alt, model);
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(c) = cond {
                relink_expr(c, model);
            }
            relink_stmt(body, model);
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(i) = init {
                relink_stmt(i, model);
            }
            if let Some(c) = cond {
                relink_expr(c, model);
            }
            if let Some(s) = step {
                relink_expr(s, model);
            }
            relink_stmt(body, model);
        }
        StatementNode::Match { expr, arms, .. } => {
            relink_expr(expr, model);
            for arm in arms.iter_mut() {
                relink_stmt(&mut arm.body, model);
            }
        }
        StatementNode::Assembly { body, .. } => relink_stmt(body, model),
        StatementNode::Return(Some(expr), _) => relink_expr(expr, model),
        StatementNode::Variable(_, _, Some(init), _) => relink_expr(init, model),
        _ => {}
    }
}

/// Ячейка именованного условия в выражении - единственное, что здесь ищется: обычный
/// вызов функции в теле разрешался на стадии 5, когда функции уже были.
fn relink_expr(expr: &mut ExpressionNode, model: &Rc<RefCell<ModelNode>>) {
    match expr {
        ExpressionNode::Condition(cell) => relink_cond(&mut cell.borrow_mut().value, model),
        ExpressionNode::Parenthesis(inner)
        | ExpressionNode::Not(inner)
        | ExpressionNode::Negate(inner)
        | ExpressionNode::BitwiseNot(inner)
        | ExpressionNode::UnaryPlus(inner)
        | ExpressionNode::Cast(inner, _) => relink_expr(inner, model),
        ExpressionNode::Assign(l, r)
        | ExpressionNode::Add(l, r)
        | ExpressionNode::Subtract(l, r)
        | ExpressionNode::Multiply(l, r)
        | ExpressionNode::Divide(l, r)
        | ExpressionNode::Modulo(l, r)
        | ExpressionNode::Power(l, r)
        | ExpressionNode::ShiftLeft(l, r)
        | ExpressionNode::ShiftRight(l, r)
        | ExpressionNode::BitwiseAnd(l, r)
        | ExpressionNode::BitwiseOr(l, r)
        | ExpressionNode::BitwiseXor(l, r)
        | ExpressionNode::Equal(l, r)
        | ExpressionNode::NotEqual(l, r)
        | ExpressionNode::Less(l, r)
        | ExpressionNode::LessEqual(l, r)
        | ExpressionNode::More(l, r)
        | ExpressionNode::MoreEqual(l, r)
        | ExpressionNode::And(l, r)
        | ExpressionNode::Or(l, r) => {
            relink_expr(l, model);
            relink_expr(r, model);
        }
        ExpressionNode::Function(_, args)
        | ExpressionNode::Array(args)
        | ExpressionNode::Initializer(args) => {
            for a in args.iter_mut() {
                relink_expr(a, model);
            }
        }
        _ => {}
    }
}

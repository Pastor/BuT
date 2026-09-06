//! Доставка выражений тел судьям правил.
//!
//! ## Как устроено
//!
//! Модуль **не решает** сам, что законно: он лишь доставляет выражения судьям. Иначе
//! правило оказалось бы в двух местах и разошлось бы - ровно тот дефект, который
//! закрывала 0188.
//!
//! Судей сегодня два, и обход у них **общий**:
//!
//! | Судья | Правило | Диагностика |
//! |---|---|---|
//! | [`super::common::validate_expression`] | направление порта | `SE-026`, `SE-027` |
//! | [`super::assignment_position`] | присваивание в именованном аргументе вызова функции | `SE-095` |
//!
//! Второму судье нужна **позиция** выражения ([`Position`]): присваивание на верхнем
//! уровне оператора законно, внутри вычисляемого выражения - нет. Обход знает позицию
//! по построению, судья её только принимает. После исправления бо́льшую часть форм отсекает
//! грамматика (`SY-006`), и судья остаётся страховкой на достижимом остатке -
//! именованном аргументе вызова.
//!
//! Прежнее имя модуля - `port_direction.rs`. Оно стало врать, когда судей стало двое:
//! модуль не о направлении, а о **доставке**.
//!
//! Накопление - по выражениям (одна ошибка на выражение), по образцу `literal_range`:
//! редактор подчёркивает все нарушения, а не первое.

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::{
    ExpressionNode, FunctionDefinitionNode, ModelNode, StateNode, StatementNode, VariableNode,
};
use std::cell::RefCell;
use std::rc::Rc;

/// Где стоит выражение - от этого зависит, законно ли в нём присваивание.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum Position {
    /// Позиция **оператора**: само выражение исполняется ради действия.
    ///
    /// Таких мест два: `x := 1;` как оператор тела и шаг цикла `for var i: u8 := 0;
    /// ...; i := i + 1`. Присваивание здесь - и есть смысл записи.
    Statement,
    /// Позиция **значения**: выражение вычисляется, а результат потребляется (операнд,
    /// условие, аргумент, инициализатор, `return`). Присваивание здесь запрещено -
    /// `SE-095`.
    Value,
}

/// Проверяет тела модели и её под-моделей всеми судьями выражений.
pub(super) fn check_bodies(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut found = Vec::new();
    check_model(&model, &mut found);
    found
}

fn check_model(model: &Rc<RefCell<ModelNode>>, found: &mut Vec<Diagnostic>) {
    let (vars, funcs, blocks, states, nested) = {
        let b = model.borrow();
        (
            b.variables.values().cloned().collect::<Vec<_>>(),
            b.functions.values().cloned().collect::<Vec<_>>(),
            b.named_blocks.clone(),
            b.states.values().cloned().collect::<Vec<_>>(),
            b.models.values().map(Rc::clone).collect::<Vec<_>>(),
        )
    };

    // Инициализатор переменной: `var x: u8 := led;` - тоже чтение выхода. Позиция -
    // **значение**: `var y: u8 := (x := 7);` записью не является, это вычисление
    // начального значения.
    for var in &vars {
        if let VariableNode::Simple { expr, .. } | VariableNode::Const { expr, .. } = var {
            check_expr(expr, model, found, Position::Value, Location::Builtin);
        }
    }
    for func in &funcs {
        if let FunctionDefinitionNode::Local { body, .. } = func {
            check_stmt(body, model, found);
        }
    }
    for block in &blocks {
        if let Some(stmt) = block.statement() {
            check_stmt(stmt, model, found);
        }
    }
    for state in &states {
        let named_blocks = match state {
            StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } => {
                named_blocks
            }
            StateNode::Unresolved => continue,
        };
        for block in named_blocks {
            if let Some(stmt) = block.statement() {
                check_stmt(stmt, model, found);
            }
        }
    }
    for child in &nested {
        check_model(child, found);
    }
}

/// Обход оператора: каждое выражение отдаётся судьям вместе с его позицией.
fn check_stmt(stmt: &StatementNode, model: &Rc<RefCell<ModelNode>>, found: &mut Vec<Diagnostic>) {
    match stmt {
        StatementNode::Block(items) => {
            for item in items {
                check_stmt(item, model, found);
            }
        }
        // Первая позиция оператора: `led := 1;` - запись как действие.
        StatementNode::Expression(expr, loc) => {
            check_expr(expr, model, found, Position::Statement, *loc)
        }
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            check_expr(cond, model, found, Position::Value, Location::Builtin);
            check_stmt(then_, model, found);
            if let Some(other) = else_ {
                check_stmt(other, model, found);
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(cond) = cond {
                check_expr(cond, model, found, Position::Value, Location::Builtin);
            }
            check_stmt(body, model, found);
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(init) = init {
                check_stmt(init, model, found);
            }
            if let Some(cond) = cond {
                check_expr(cond, model, found, Position::Value, Location::Builtin);
            }
            // Шаг цикла - вторая позиция оператора: `for var i: u8 := 0; ...; i := i +
            // 1` без присваивания в шаге не существует.
            if let Some(step) = step {
                check_expr(step, model, found, Position::Statement, Location::Builtin);
            }
            check_stmt(body, model, found);
        }
        StatementNode::Variable(_, _, Some(expr), _) => {
            check_expr(expr, model, found, Position::Value, Location::Builtin)
        }
        StatementNode::Return(Some(expr), _) => {
            check_expr(expr, model, found, Position::Value, Location::Builtin)
        }
        StatementNode::Match { expr, arms, .. } => {
            check_expr(expr, model, found, Position::Value, Location::Builtin);
            for arm in arms {
                check_stmt(&arm.body, model, found);
            }
        }
        // Прочие операторы выражений не несут: объявление без инициализатора,
        // `break`/`continue`, пустой `return`, встроенная формула, `None` и
        // неразрешённый АСД-оператор.
        _ => {}
    }
}

/// Отдаёт выражение обоим судьям; ошибки копит, а не прерывает обход.
fn check_expr(
    expr: &ExpressionNode,
    model: &Rc<RefCell<ModelNode>>,
    found: &mut Vec<Diagnostic>,
    position: Position,
    stmt_loc: Location,
) {
    if let Err(diagnostic) = super::common::validate_expression(expr, Rc::clone(model)) {
        found.push(diagnostic);
    }
    found.extend(super::assignment_position::check_expression(
        expr, position, stmt_loc,
    ));
    found.extend(super::assignment_place::check_expression(
        expr, position, stmt_loc,
    ));
}

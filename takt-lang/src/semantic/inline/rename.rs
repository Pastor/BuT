//! Обход выражений подстановки и переименование её локальных имён.
//!
//! # Как идентифицируется имя
//!
//! Ссылка на параметр и на локальную переменную - **своя ячейка** у каждого
//! употребления (`expression::resolve_expr` создаёт `Rc` на месте), общей ячейки у них
//! нет. Поэтому переименование идёт **по имени**: ячейка с именем из карты заменяется
//! новой, с тем же типом и владельцем места вызова.
//!
//! Ячейка переменной модели с таким же именем в теле функции появиться не может:
//! параметр затеняет одноимённое объявление при разрешении.
#![deny(clippy::wildcard_enum_match_arm)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::diagnostics::Location;
use crate::semantic::walk::walk_stmt_exprs_mut;
use crate::semantic::{ExpressionNode, ModelNode, StatementNode, VariableNode};

/// Переименовывает локальные имена подставленного оператора.
pub(crate) fn rename_stmt(
    stmt: &mut StatementNode,
    map: &HashMap<String, String>,
    owner: &Rc<RefCell<ModelNode>>,
) {
    if let StatementNode::Variable(name, _, _, _) = stmt
        && let Some(fresh) = map.get(name)
    {
        *name = fresh.clone();
    }
    if let StatementNode::Block(items) = stmt {
        for item in items.iter_mut() {
            rename_stmt(item, map, owner);
        }
        return;
    }
    if let StatementNode::If { then_, else_, .. } = stmt {
        rename_stmt(then_, map, owner);
        if let Some(alt) = else_ {
            rename_stmt(alt, map, owner);
        }
    }
    if let StatementNode::Loop { body, .. } = stmt {
        rename_stmt(body, map, owner);
    }
    if let StatementNode::For { init, body, .. } = stmt {
        if let Some(i) = init {
            rename_stmt(i, map, owner);
        }
        rename_stmt(body, map, owner);
    }
    if let StatementNode::Match { arms, .. } = stmt {
        for arm in arms.iter_mut() {
            rename_stmt(&mut arm.body, map, owner);
        }
    }
    walk_stmt_exprs_mut(stmt, &mut |expr| rename_one(expr, map, owner));
}

/// Заменяет ячейку переменной, если её имя переименовано.
fn rename_one(
    expr: &mut ExpressionNode,
    map: &HashMap<String, String>,
    owner: &Rc<RefCell<ModelNode>>,
) {
    let ExpressionNode::Variable(cell) = expr else {
        return;
    };
    let (name, ty) = match &*cell.borrow() {
        VariableNode::Simple { name, ty, .. }
        | VariableNode::Port { name, ty, .. }
        | VariableNode::Const { name, ty, .. } => (name.clone(), ty.clone()),
        VariableNode::Unresolved => return,
    };
    let Some(fresh) = map.get(&name) else {
        return;
    };
    *expr = ExpressionNode::Variable(Rc::new(RefCell::new(VariableNode::Simple {
        upper: Some(Rc::downgrade(owner)),
        loc: Location::Implicit,
        name: fresh.clone(),
        ty,
        expr: ExpressionNode::None,
    })));
}

//! Обход выражений и операторов семантического дерева — общий носитель.
//!
//! # Почему обход исчерпывающий
//!
//! Модуль объявляет `deny(clippy::wildcard_enum_match_arm)`: пропущенная форма
//! давала бы **молчаливую** ошибку у каждого потребителя разом. Сегодня их
//! трое: подстановка тела функции (переименование локальных, фича 0444), счёт
//! вызовов там же и сбор читаемых полей порта (фича 0453).
//!
//! ⚠️ Носитель один намеренно: три копии «как обойти выражение» разошлись бы
//! молча — класс 0084/0193/0195.
#![deny(clippy::wildcard_enum_match_arm)]

use crate::semantic::{ExpressionNode, StatementNode};

/// Применяет `f` к каждому подвыражению **снизу вверх** (сперва потомки).
///
/// Порядок значим: вложенный вызов `outer(inner(x))` обязан быть подставлен
/// раньше внешнего, иначе объявления встанут в `prelude` в обратном порядке.
pub(crate) fn walk_expr_mut(expr: &mut ExpressionNode, f: &mut dyn FnMut(&mut ExpressionNode)) {
    match expr {
        ExpressionNode::ArraySubscript(a, b)
        | ExpressionNode::Power(a, b)
        | ExpressionNode::Multiply(a, b)
        | ExpressionNode::Divide(a, b)
        | ExpressionNode::Modulo(a, b)
        | ExpressionNode::Add(a, b)
        | ExpressionNode::Subtract(a, b)
        | ExpressionNode::ShiftLeft(a, b)
        | ExpressionNode::ShiftRight(a, b)
        | ExpressionNode::BitwiseAnd(a, b)
        | ExpressionNode::BitwiseXor(a, b)
        | ExpressionNode::BitwiseOr(a, b)
        | ExpressionNode::Less(a, b)
        | ExpressionNode::More(a, b)
        | ExpressionNode::LessEqual(a, b)
        | ExpressionNode::MoreEqual(a, b)
        | ExpressionNode::Equal(a, b)
        | ExpressionNode::NotEqual(a, b)
        | ExpressionNode::And(a, b)
        | ExpressionNode::Or(a, b)
        | ExpressionNode::Assign(a, b) => {
            walk_expr_mut(a, f);
            walk_expr_mut(b, f);
        }
        ExpressionNode::ConditionalOperator(a, b, c) => {
            walk_expr_mut(a, f);
            walk_expr_mut(b, f);
            walk_expr_mut(c, f);
        }
        ExpressionNode::ArraySlice(a, _, _)
        | ExpressionNode::Parenthesis(a)
        | ExpressionNode::BitAccess(a, _)
        | ExpressionNode::Not(a)
        | ExpressionNode::BitwiseNot(a)
        | ExpressionNode::UnaryPlus(a)
        | ExpressionNode::Negate(a)
        | ExpressionNode::Cast(a, _) => walk_expr_mut(a, f),
        ExpressionNode::CodeBlock(a, _) => walk_expr_mut(a, f),
        // ⚠️ Именованные аргументы (`NamedFunctionBox`) — узлы **сырого** АСД:
        // на уровне АСД вызов функции и инстанцирование модели неразличимы
        // (0187), и разрешённых подвыражений там нет. Обходится только база.
        ExpressionNode::NamedFunctionBox(a, _) => walk_expr_mut(a, f),
        ExpressionNode::Function(_, args)
        | ExpressionNode::Array(args)
        | ExpressionNode::Initializer(args) => {
            for arg in args.iter_mut() {
                walk_expr_mut(arg, f);
            }
        }
        // Листья: своих подвыражений не имеют.
        ExpressionNode::None
        | ExpressionNode::Unresolved(_)
        | ExpressionNode::Number(_)
        | ExpressionNode::Duration(_)
        | ExpressionNode::Rational(_, _)
        | ExpressionNode::String(_)
        | ExpressionNode::Type(_)
        | ExpressionNode::Address(_, _)
        | ExpressionNode::AnonPort(_)
        | ExpressionNode::Bool(_)
        | ExpressionNode::Variable(_)
        | ExpressionNode::Model(_)
        | ExpressionNode::Condition(_)
        | ExpressionNode::List(_) => {}
    }
    f(expr);
}

/// Применяет `f` к каждому выражению оператора (рекурсивно по телам).
pub(crate) fn walk_stmt_exprs(stmt: &StatementNode, f: &mut dyn FnMut(&ExpressionNode)) {
    let mut copy = stmt.clone();
    walk_stmt_exprs_mut(&mut copy, &mut |e| f(e));
}

/// Изменяемый вариант [`walk_stmt_exprs`].
pub(crate) fn walk_stmt_exprs_mut(
    stmt: &mut StatementNode,
    f: &mut dyn FnMut(&mut ExpressionNode),
) {
    match stmt {
        StatementNode::Block(items) => {
            for item in items.iter_mut() {
                walk_stmt_exprs_mut(item, f);
            }
        }
        StatementNode::Expression(expr, _) => walk_expr_mut(expr, f),
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            walk_expr_mut(cond, f);
            walk_stmt_exprs_mut(then_, f);
            if let Some(alt) = else_ {
                walk_stmt_exprs_mut(alt, f);
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(c) = cond {
                walk_expr_mut(c, f);
            }
            walk_stmt_exprs_mut(body, f);
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(i) = init {
                walk_stmt_exprs_mut(i, f);
            }
            if let Some(c) = cond {
                walk_expr_mut(c, f);
            }
            if let Some(s) = step {
                walk_expr_mut(s, f);
            }
            walk_stmt_exprs_mut(body, f);
        }
        StatementNode::Variable(_, _, init, _) => {
            if let Some(e) = init {
                walk_expr_mut(e, f);
            }
        }
        StatementNode::Return(expr, _) => {
            if let Some(e) = expr {
                walk_expr_mut(e, f);
            }
        }
        StatementNode::Match { expr, arms, .. } => {
            walk_expr_mut(expr, f);
            for arm in arms.iter_mut() {
                walk_stmt_exprs_mut(&mut arm.body, f);
            }
        }
        // Вставка для цели — обычные операторы Takt (0484): обход обязан
        // спускаться в неё, иначе выражения тела не увидит ни один проход.
        StatementNode::Assembly { body, .. } => walk_stmt_exprs_mut(body, f),
        // Операторы без выражений. Блок формул адресован внешнему анализатору:
        // выражений Takt в нём нет вовсе (0484).
        StatementNode::None
        | StatementNode::Unresolved(_)
        | StatementNode::Continue(_)
        | StatementNode::Break(_)
        | StatementNode::Formula(_)
        | StatementNode::InlineFormula(_) => {}
    }
}

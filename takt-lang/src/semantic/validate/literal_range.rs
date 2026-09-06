//! `SE-089`: числовой литерал не помещается в тип приёмника.
//!
//! # Что проверяется
//!
//! Литерал в позиции, где тип приёмника **объявлен**: инициализатор `var`/`const` и
//! присваивание переменной. Тип без одного целочисленного диапазона (`Enum`, `q(m, n)`,
//! `duration`, структура, широкий бит-вектор) пропускается - у них своё представление и
//! свои проверки. `bit`/`bool` пропущены по той же причине: их значения проверяет
//! `validate_bit_values`, и вторая диагностика об одной причине была бы шумом.
//!
//! Проверяется **литерал автора**, а не всякое значение. Инициализатор объявления к
//! моменту проверки уже свёрнут, поэтому вычисленное значение приходит сюда литералом -
//! и было бы неотличимо от написанного руками. Разводит их **свёртка**: вычисленное она
//! нормирует по типу объявления, и до проверки доходит уже значение в диапазоне;
//! литерал автора она не трогает вовсе, и его выход за границы остаётся ошибкой. В
//! присваивании внутри тела свёртки нет - там проверяется именно записанный литерал.

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::type_node::TypeNode;
use crate::semantic::{
    ExpressionNode, FunctionDefinitionNode, ModelNode, StateNode, StatementNode, VariableNode,
};
use std::cell::RefCell;
use std::rc::Rc;

/// `SE-089` - литерал вне диапазона типа приёмника.
fn se089(loc: Location, what: &str, value: i128, ty: &TypeNode, lo: i128, hi: i128) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!("литерал {value} не помещается в тип '{ty}' {what}: допустимо [{lo}, {hi}]"),
    )
    .with_code("SE-089")
}

/// Диапазон значений типа, если он у него один и целочисленный.
///
/// `pub(crate)`: этот же диапазон нужен свёртке инициализатора - она нормирует
/// **вычисленное** значение по типу объявления. Вторая копия границ разошлась бы с
/// первой, и одна и та же запись получала бы у проверки и у свёртки разные вердикты.
pub(crate) fn type_range(ty: &TypeNode) -> Option<(i128, i128)> {
    match ty {
        // `bit`/`bool` специально пропущены: их значения проверяет
        // `validate_bit_values` (сообщение там называет допустимые формы - `0`, `1`,
        // `true`, `false`), и вторая диагностика о том же была бы шумом на одну
        // причину.
        TypeNode::Integer { bits, signed } if *bits >= 1 && *bits <= 64 => {
            let bits = u32::from(*bits);
            Some(if *signed {
                (-(1i128 << (bits - 1)), (1i128 << (bits - 1)) - 1)
            } else {
                (0, (1i128 << bits) - 1)
            })
        }
        // Бит-вектор `[bit;N]` при N <= 64 - упакованный скаляр; шире - массив слов,
        // одного диапазона у него нет.
        TypeNode::Array(n, _) if crate::semantic::bit_vector::is_bit_vector(ty).is_some() => {
            let n = u32::from(*n);
            (n <= 64).then(|| (0, (1i128 << n) - 1))
        }
        _ => None,
    }
}

/// Литерал выражения, если это именно литерал (в том числе в скобках).
fn literal(expr: &ExpressionNode) -> Option<i128> {
    match expr {
        ExpressionNode::Number(n) => Some(*n),
        ExpressionNode::Parenthesis(inner) => literal(inner),
        _ => None,
    }
}

/// Тип и имя переменной, если они у неё есть.
fn var_type(var: &VariableNode) -> Option<(&TypeNode, &str)> {
    match var {
        // Порт инициализируется **адресом**, а не значением.
        VariableNode::Simple { name, ty, .. } | VariableNode::Const { name, ty, .. } => {
            Some((ty, name.as_str()))
        }
        VariableNode::Port { .. } | VariableNode::Unresolved => None,
    }
}

/// Проверяет литерал против типа приёмника.
fn check(value: i128, ty: &TypeNode, loc: Location, what: &str, found: &mut Vec<Diagnostic>) {
    if let Some((lo, hi)) = type_range(ty)
        && (value < lo || value > hi)
    {
        found.push(se089(loc, what, value, ty, lo, hi));
    }
}

/// Все нарушения диапазона в модели (вложенные обходит вызывающий).
pub(super) fn check_literal_ranges(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut found = Vec::new();
    let (vars, funcs, blocks, states) = {
        let b = model.borrow();
        (
            b.variables.values().cloned().collect::<Vec<_>>(),
            b.functions.values().cloned().collect::<Vec<_>>(),
            b.named_blocks.clone(),
            b.states.values().cloned().collect::<Vec<_>>(),
        )
    };

    for var in &vars {
        if let Some((ty, name)) = var_type(var)
            && let Some(expr) = var_init(var)
            && let Some(value) = literal(expr)
        {
            check(
                value,
                ty,
                var.loc(),
                &format!("переменной '{name}'"),
                &mut found,
            );
        }
    }
    for func in &funcs {
        if let FunctionDefinitionNode::Local { body, .. } = func {
            check_stmt(body, &mut found);
        }
    }
    for block in &blocks {
        if let Some(stmt) = block.statement() {
            check_stmt(stmt, &mut found);
        }
    }
    for state in &states {
        check_state(state, &mut found);
    }
    found
}

/// Инициализатор переменной (или `None`, если его нет).
fn var_init(var: &VariableNode) -> Option<&ExpressionNode> {
    match var {
        VariableNode::Simple { expr, .. } | VariableNode::Const { expr, .. } => Some(expr),
        VariableNode::Port { .. } | VariableNode::Unresolved => None,
    }
}

fn check_state(state: &StateNode, found: &mut Vec<Diagnostic>) {
    let named_blocks = match state {
        StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } => {
            named_blocks
        }
        StateNode::Unresolved => return,
    };
    for block in named_blocks {
        if let Some(stmt) = block.statement() {
            check_stmt(stmt, found);
        }
    }
}

/// Обход оператора: интересны присваивания литерала переменной.
fn check_stmt(stmt: &StatementNode, found: &mut Vec<Diagnostic>) {
    match stmt {
        StatementNode::Block(items) => {
            for item in items {
                check_stmt(item, found);
            }
        }
        StatementNode::Expression(expr, _) => check_expr(expr, found),
        StatementNode::If { then_, else_, .. } => {
            check_stmt(then_, found);
            if let Some(other) = else_ {
                check_stmt(other, found);
            }
        }
        StatementNode::Loop { body, .. } | StatementNode::For { body, .. } => {
            check_stmt(body, found);
        }
        // Объявление локальной переменной с инициализатором-литералом.
        StatementNode::Variable(name, ty, Some(expr), _) => {
            if let Some(value) = literal(expr) {
                check(
                    value,
                    ty,
                    Location::Implicit,
                    &format!("переменной '{name}'"),
                    found,
                );
            }
        }
        // Прочие операторы литерала в объявленный тип не записывают: `return` отдаёт
        // значение функции (её тип проверяет вывод типов), остальные не содержат
        // присваивания.
        _ => {}
    }
}

/// Обход выражения: `переменная := литерал` на любой глубине последовательности.
fn check_expr(expr: &ExpressionNode, found: &mut Vec<Diagnostic>) {
    match expr {
        ExpressionNode::Assign(target, value) => {
            if let ExpressionNode::Variable(var_rc) = target.as_ref()
                && let Some(literal_value) = literal(value)
            {
                let borrowed = var_rc.borrow();
                if let Some((ty, name)) = var_type(&borrowed) {
                    let what = format!("переменной '{name}'");
                    check(literal_value, ty, borrowed.loc(), &what, found);
                }
            }
            check_expr(value, found);
        }
        ExpressionNode::Parenthesis(inner) => check_expr(inner, found),
        // Прочие узлы приёмника с объявленным типом не образуют: литерал в арифметике
        // или сравнении имеет тип выражения, а не переменной.
        _ => {}
    }
}

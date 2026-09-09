//! Число элементов агрегата обязано отвечать объявлению - `SE-123`.

use crate::diagnostics::lang::keys;
use crate::msg;
use super::*;
use crate::semantic::type_node::TypeNode;

/// Проверяет длину агрегатов в инициализаторах объявлений модели.
pub(super) fn check_aggregate_lengths(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let (vars, structs) = {
        let borrowed = model.borrow();
        (
            borrowed.variables.values().cloned().collect::<Vec<_>>(),
            borrowed.structs.clone(),
        )
    };
    // Накопление по объявлениям: каждое объявление высказывается.
    let mut found = Vec::new();
    for var in &vars {
        let (Some(ty), Some(expr)) = (declared_type(var), initializer(var)) else {
            continue;
        };
        let (ExpressionNode::Array(items) | ExpressionNode::Initializer(items)) = expr else {
            continue;
        };
        let name = var.name();
        match ty {
            TypeNode::Array(size, _) => {
                let expected = usize::from(size);
                if items.len() != expected {
                    found.push(mismatch(
                        var.loc(),
                        &msg!(keys::WHAT_ARRAY, name = name),
                        expected,
                        items.len(),
                    ));
                }
            }
            // Структура: "длина" - число объявленных полей. Порядок полей значим,
            // поэтому недостача не может означать "остальные по умолчанию".
            TypeNode::Struct(struct_name) => {
                let Some(def) = structs.get(&struct_name) else {
                    continue;
                };
                if items.len() != def.fields.len() {
                    found.push(mismatch(
                        var.loc(),
                        &msg!(keys::WHAT_STRUCT_OF_TYPE, name = name, ty = struct_name),
                        def.fields.len(),
                        items.len(),
                    ));
                }
            }
            _ => {}
        }
    }
    found
}

/// Диагностика `SE-123`: называет вид, объявленное и переданное числа.
fn mismatch(loc: Location, what: &str, expected: usize, got: usize) -> Diagnostic {
    Diagnostic::error(
        loc,
        msg!(
            keys::SE_123_AGGREGATE_LENGTH_MISMATCH,
            what = what,
            expected = expected,
            got = got
        ),
    )
    .with_code("SE-123")
}

/// Объявленный тип переменной либо константы.
fn declared_type(var: &VariableNode) -> Option<TypeNode> {
    match var {
        VariableNode::Simple { ty, .. } | VariableNode::Const { ty, .. } => Some(ty.clone()),
        // Порт агрегатом не инициализируется: его начальное значение - скаляр, и
        // правило здесь ни при чём.
        VariableNode::Port { .. } | VariableNode::Unresolved => None,
    }
}

/// Инициализатор объявления, если он есть.
fn initializer(var: &VariableNode) -> Option<&ExpressionNode> {
    match var {
        VariableNode::Simple { expr, .. } | VariableNode::Const { expr, .. } => Some(expr),
        VariableNode::Port { .. } | VariableNode::Unresolved => None,
    }
}

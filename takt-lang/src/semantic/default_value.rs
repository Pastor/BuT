//! Умолчание значения типа как выражение семантики.

use std::cell::RefCell;
use std::rc::Rc;

use crate::semantic::enum_node::enum_default;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, ModelNode};

/// Умолчание значения типа `ty` в области видимости модели `model`.
///
/// Умолчание **числовое** у скаляров (`0`/`false`) и **позиционный агрегат** у массива
/// и структуры - тот же порядок, что у инициализатора автора.
pub fn default_expression(ty: &TypeNode, model: &ModelNode) -> Option<ExpressionNode> {
    match ty {
        // Скаляры: ноль представим у всех целей и означает одно и то же.
        TypeNode::Bit | TypeNode::Integer { .. } | TypeNode::Duration => {
            Some(ExpressionNode::Number(0))
        }
        TypeNode::Bool => Some(ExpressionNode::Bool(false)),
        // q(m, n): представление нуля - сам ноль при любом масштабе.
        TypeNode::Fixed { .. } => Some(ExpressionNode::Number(0)),
        // Вещественное: ноль печатается литералом целевого языка.
        TypeNode::Rational => Some(ExpressionNode::Number(0)),
        // Перечисление: Первый по тексту вариант - ноль может не принадлежать набору
        // вовсе.
        TypeNode::Enum(name) => {
            let def = model.enums.get(name)?;
            let (_, value) = enum_default(&def.variants)?;
            Some(ExpressionNode::Number(value))
        }
        // Массив и структура: агрегат из умолчаний элементов, в объявленном порядке;
        // длину сверяет семантика (`SE-123`).
        TypeNode::Array(len, elem) => {
            let one = default_expression(elem, model)?;
            Some(ExpressionNode::Initializer(vec![one; *len as usize]))
        }
        TypeNode::Struct(name) => {
            let def = model.structs.get(name)?;
            let mut fields = Vec::with_capacity(def.fields.len());
            for (_, field_ty) in &def.fields {
                fields.push(default_expression(field_ty, model)?);
            }
            Some(ExpressionNode::Initializer(fields))
        }
        // Умолчания нет: тип не выведен, служебный либо адресный.
        TypeNode::Inference
        | TypeNode::Address(_, _)
        | TypeNode::Unsupported
        | TypeNode::Unit
        | TypeNode::BuiltinString
        | TypeNode::BuiltinModel
        | TypeNode::BuiltinState
        | TypeNode::BuiltinNumeric => None,
    }
}

/// То же по ссылке на ячейку модели - удобство для проходов, держащих `Rc`.
pub fn default_of(ty: &TypeNode, model: &Rc<RefCell<ModelNode>>) -> Option<ExpressionNode> {
    default_expression(ty, &model.borrow())
}

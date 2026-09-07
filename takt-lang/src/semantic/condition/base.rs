//! База постфиксной индексации в условии.
//!
//! Условие - своё дерево (`ConditionNode`), и общий носитель типа
//! (`semantic::validate::base_type`) работает по `ExpressionNode`. Поэтому знание о
//! цепочке места здесь **своё**, но узкое: переменная массива либо поле структуры типа
//! `[T; N]`.
//!
//! Живёт отдельным модулем: знание о базе индексации самостоятельно и к разбору
//! условия целиком не относится.

use super::*;

/// Как назвать базу условия в диагностике.
///
/// Имя переменной, когда оно есть: диагностика обязана называть предмет, а не говорить
/// о "значении" (тест `diagnostic_code_presence_tests` проверяет это).
pub(super) fn cond_base_label(base: &ConditionNode) -> String {
    match base {
        ConditionNode::Variable(var, _) => format!("'{}'", var.borrow().name()),
        ConditionNode::Parenthesis(inner) => cond_base_label(inner),
        ConditionNode::BitAccess(_, crate::parser::ast::Member::Identifier(field)) => {
            format!("поле '{}'", field.name)
        }
        ConditionNode::Unresolved(crate::parser::ast::Condition::Variable(id)) => {
            format!("'{}'", id.name)
        }
        _ => "индексируемое значение".to_string(),
    }
}

/// Приводит ли база условия к массиву.
///
/// Условие - своё дерево, поэтому знание о цепочке места здесь **своё**, но узкое:
/// переменная массива либо поле структуры типа `[T; N]`. Ошибка в сторону "не массив"
/// громкая (`SE-117`), поэтому неизвестное считается массивом - за пропуском стоят
/// проверки целей и эталона.
pub(super) fn cond_base_is_array(base: &ConditionNode, model: &ModelNode) -> bool {
    match base {
        // Вид объявления значения не имеет - значение имеет тип: ветвь спрашивала
        // только `Simple`, и `bus[1] > 3` при `in bus: [u8;2]` отвергалось `SE-117` "не
        // является массивом", тогда как та же запись в теле блока переводилась всеми
        // восемью целями.
        ConditionNode::Variable(var, _) => {
            matches!(declared_type(&var.borrow()),
                Some(ty) if matches!(ty, TypeNode::Array(..) | TypeNode::Inference))
        }
        ConditionNode::Parenthesis(inner) => cond_base_is_array(inner, model),
        ConditionNode::BitAccess(inner, crate::parser::ast::Member::Identifier(field)) => {
            match cond_field_type(inner, field, model) {
                Some(ty) => matches!(ty, TypeNode::Array(..) | TypeNode::Inference),
                None => true,
            }
        }
        // Неразрешённое имя индексировать нельзя: такой вход судит `SE-117`.
        ConditionNode::Unresolved(_) => false,
        // Прочее (вызов, арифметика) - не место; судить не берёмся.
        _ => true,
    }
}

/// Объявленный тип значения: у переменной и у порта он одинаково объявлен.
///
/// Прочие виды (константа, параметр) сюда не попадают намеренно: индексировать их язык
/// не даёт, а "неизвестно" здесь означает пропуск проверки.
fn declared_type(var: &VariableNode) -> Option<TypeNode> {
    match var {
        VariableNode::Simple { ty, .. } | VariableNode::Port { ty, .. } => Some(ty.clone()),
        _ => None,
    }
}

/// Тип поля `field` у базы условия, если он выводится.
fn cond_field_type(
    base: &ConditionNode,
    field: &crate::parser::ast::Identifier,
    model: &ModelNode,
) -> Option<TypeNode> {
    let base_ty = match base {
        ConditionNode::Variable(var, _) => declared_type(&var.borrow())?,
        ConditionNode::Parenthesis(inner) => return cond_field_type(inner, field, model),
        _ => return None,
    };
    let TypeNode::Struct(name) = base_ty else {
        return None;
    };
    model
        .search_struct(&name)?
        .fields
        .iter()
        .find(|(f, _)| *f == field.name)
        .map(|(_, t)| t.clone())
}

/// Тип базы условия, если он объявлен: нужен сведению индекса к разряду.
///
/// Отвечает **только** про имя и поле - то же, о чём судит [`cond_base_is_array`]: у
/// прочих форм (вызов, арифметика) типа здесь нет, и "неизвестно" обязано значить "не
/// сводим", а не "сведём наугад".
pub(super) fn cond_base_type(base: &ConditionNode, model: &ModelNode) -> Option<TypeNode> {
    match base {
        ConditionNode::Variable(var, _) => declared_type(&var.borrow()),
        ConditionNode::Parenthesis(inner) => cond_base_type(inner, model),
        ConditionNode::BitAccess(inner, crate::parser::ast::Member::Identifier(field)) => {
            cond_field_type(inner, field, model)
        }
        _ => None,
    }
}

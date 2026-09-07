//! Тип выражения-базы постфиксной операции - общий носитель.
//!
//! # Представления места, знание одно
//!
//! Одно и то же место автор записывает в двух разных деревьях, и оба доходят до
//! потребителей:
//!
//! | Вход | Представление | Откуда берётся |
//! |---|---|---|
//! | [`base_type`] | [`ExpressionNode`] | тело блока, тело функции |
//! | [`cond_base_type`] | [`ConditionNode`] | `cond`, формулы, инвариант, условие ребра (разрешается стадией 6) |
//!
//! Спуск по типу (структура -> поле, массив -> элемент) у них **общий**: копии этого
//! правила разъехались бы молча - компилятор о расхождении копий не скажет. Различаются
//! входы только формой дерева; тип корня оба берут у ячейки ссылки.
//!
//! Третьего входа - по сырому `ast::Condition` - здесь нет намеренно: неразрешённым до
//! целей доезжает только паттерн `S(Модель) = Состояние`, а в нём обе стороны суть
//! имена.
//!
//! # Консервативность - часть контракта
//!
//! `None` означает "тип надёжно не выводится", и проверка тогда **не срабатывает**.
//! Ложное срабатывание хуже пропуска: за пропуском стоят диагностики целей и эталона,
//! за ложным отказом - незаконно отвергнутая программа.

use crate::parser::ast::Member;
use crate::semantic::ModelNode;
use crate::semantic::condition_node::ConditionNode;
use crate::semantic::expression_node::ExpressionNode;
use crate::semantic::type_node::TypeNode;

/// Тип выражения `expr` в контексте модели `model`.
///
/// Разбирается цепочка `переменная(.поле | [индекс])*` - то, из чего состоит
/// **место** в языке. Всё прочее даёт `None`.
pub(crate) fn base_type(expr: &ExpressionNode, model: &ModelNode) -> Option<TypeNode> {
    base_type_with(expr, &|name| model.search_struct(name).map(|s| s.fields))
}

/// Способ узнать поля структуры по её имени.
///
/// Печатники целей носят объявления по-разному: у `st` и `rust` есть `ModelNode`, у
/// `sv` - снимок карты. Замыкание примиряет оба, не заводя второго знания о спуске по
/// типу (приём 0366).
pub(crate) type FieldsOf<'a> = &'a dyn Fn(&str) -> Option<Vec<(String, TypeNode)>>;

/// То же, но поля структур берутся замыканием.
///
/// Нужен печатникам целей: у `sv` модели нет - она носит снимок карты
/// (`Scope::structs`), и знание "как спускаться по типу" иначе пришлось бы писать
/// второй раз. Приём тот же, что у `aggregate::leaves`.
///
/// Ответ **консервативен**: `None` означает "тип надёжно не выводится", и потребитель
/// обязан идти прежним путём, а не догадываться.
pub(crate) fn base_type_with(expr: &ExpressionNode, fields_of: FieldsOf<'_>) -> Option<TypeNode> {
    match expr {
        ExpressionNode::Variable(var_rc) => Some(var_rc.borrow().ty().clone()),
        ExpressionNode::Parenthesis(inner) => base_type_with(inner, fields_of),
        ExpressionNode::BitAccess(inner, Member::Identifier(field)) => {
            field_of(base_type_with(inner, fields_of)?, &field.name, fields_of)
        }
        // Элемент массива - тип элемента; так `b.data[1].x` и `ps[1].x` разбираются
        // одним правилом.
        ExpressionNode::ArraySubscript(base, _) => element_type(base_type_with(base, fields_of)?),
        _ => None,
    }
}

/// Тип поля по замыканию - шаг спуска, общий для всех входов.
fn field_of(base: TypeNode, field: &str, fields_of: FieldsOf<'_>) -> Option<TypeNode> {
    let TypeNode::Struct(name) = base else {
        return None;
    };
    fields_of(&name)?
        .into_iter()
        .find(|(f, _)| f == field)
        .map(|(_, t)| t)
}

/// Тип места в разрешённом условии (`cond`, формулы) -.
///
/// Аналог [`base_type`] для [`ConditionNode`]: спуск тот же, отличается только
/// представление дерева.
pub(crate) fn cond_base_type(cond: &ConditionNode, model: &ModelNode) -> Option<TypeNode> {
    match cond {
        ConditionNode::Variable(var_rc, _) => Some(var_rc.borrow().ty().clone()),
        ConditionNode::Parenthesis(inner) => cond_base_type(inner, model),
        ConditionNode::BitAccess(inner, Member::Identifier(field)) => {
            field_type(cond_base_type(inner, model)?, &field.name, model)
        }
        ConditionNode::ArraySubscript(base, _) => element_type(cond_base_type(base, model)?),
        _ => None,
    }
}

/// Тип поля `field` у типа базы - общий шаг спуска для всех трёх входов.
fn field_type(base: TypeNode, field: &str, model: &ModelNode) -> Option<TypeNode> {
    let TypeNode::Struct(name) = base else {
        return None;
    };
    let s = model.search_struct(&name)?;
    s.fields
        .iter()
        .find(|(f, _)| f == field)
        .map(|(_, t)| t.clone())
}

/// Тип элемента массива - общий шаг спуска для всех трёх входов.
fn element_type(base: TypeNode) -> Option<TypeNode> {
    match base {
        TypeNode::Array(_, elem) => Some(*elem),
        _ => None,
    }
}

//! Тип операнда для цели `st` (выделено ).
//!
//! Имя функции преобразования IEC строится из **обоих** типов (`<ИЗ>_TO_<В>`), поэтому
//! печатнику нужен тип источника. Граница модуля - ответственность: `st_expr` печатает
//! выражение, а здесь отвечают на вопрос "какого типа операнд".
//!
//! # Что здесь своё, а что общее
//!
//! Цепочку `переменная(.поле | [индекс])*` разбирает общий носитель
//! `validate::base_type` - тот же, которым живут `SE-061`, `SE-028`/`SE-030` и печать
//! среза. Своё здесь - ответы, зависящие от **целевого языка**: разряд у `st`
//! печатается сравнением (значит `BOOL`), а литерал типа не имеет вовсе (иначе печатник
//! построил бы `INT_TO_...` для числа).
//!
//! Именно поэтому носители целей **не сводятся в один**: замер 2026-08-23 показал, что
//! на одном входе они обязаны отвечать по-разному, а общее у них только ядро.

use crate::parser::ast::Member;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ConditionNode, ExpressionNode, ModelNode, VariableNode};

/// Тип операнда с доступом к объявлениям модели.
///
/// Отличается от [`inner_expr_type`] одним случаем - полем структуры: без него
/// `t.hold as u32` отвергается как "тип не определяется статически", хотя тип поля
/// объявлен. Имя функции преобразования IEC строится из обоих типов, и без типа
/// источника печатать нечего.
pub(crate) fn inner_expr_type_in(expr: &ExpressionNode, model: &ModelNode) -> Option<TypeNode> {
    // Цепочка `переменная(.поле | [индекс])*` разбирается общим носителем
    // `validate::base_type`: своё знание о спуске по типу разошлось бы с тем, которым
    // живут `SE-061`, `SE-028`, `SE-030` и печать среза.
    if let Some(found) = crate::semantic::validate::base_type::base_type(expr, model) {
        return Some(found);
    }
    // Прочее - своё: у цели `st` разряд печатается сравнением, а литерал типа не имеет
    // вовсе. Общего ответа тут нет и быть не должно (см. шапку).
    inner_expr_type(expr)
}

/// Тип операнда-выражения, если его удаётся определить статически.
///
/// Определяется для переменных, скобок, приведения, разряда и элемента массива: общий
/// вывод типов выражения - не дело печатника. Если тип неизвестен, вызывающий обязан
/// вернуть `ST-011`, а не догадываться.
pub(crate) fn inner_expr_type(expr: &ExpressionNode) -> Option<TypeNode> {
    match expr {
        ExpressionNode::Variable(var) => variable_type(&var.borrow()),
        ExpressionNode::Parenthesis(inner) => inner_expr_type(inner),
        ExpressionNode::Cast(_, ty) => Some(ty.clone()),
        // Чтение разряда печатается сравнением (`(TO_BYTE(x) AND маска) <> 0`), то есть
        // его тип в ST - `BOOL` по построению, а не по выводу. Знать это нужно записи
        // разряда: `b.0 := btn.0;` иначе отвергалось бы "тип не определяется
        // статически".
        //
        // Доступ по имени (`p.x`) сюда не входит: это поле структуры, и его тип
        // печатнику неизвестен.
        ExpressionNode::BitAccess(_, Member::Number(_)) => Some(TypeNode::Bool),
        // Элемент массива: тип берётся у объявления носителя. Нужен записи разряда
        // (`arr[1].2 := 1`) - цель `c` и эталон её исполняют. База - выражение: тип
        // берётся у неё же, рекурсивно.
        ExpressionNode::ArraySubscript(base, _) => match inner_expr_type(base) {
            Some(TypeNode::Array(_, elem)) => Some((*elem).clone()),
            _ => None,
        },
        _ => None,
    }
}

/// Тип операнда-условия, если его удаётся определить статически.
pub(in crate::generator::st) fn inner_cond_type(cond: &ConditionNode) -> Option<TypeNode> {
    match cond {
        ConditionNode::Variable(var, _) => variable_type(&var.borrow()),
        ConditionNode::Parenthesis(inner) => inner_cond_type(inner),
        _ => None,
    }
}

/// Тип переменной.
pub(in crate::generator::st) fn variable_type(var: &VariableNode) -> Option<TypeNode> {
    match var {
        VariableNode::Simple { ty, .. }
        | VariableNode::Port { ty, .. }
        | VariableNode::Const { ty, .. } => Some(ty.clone()),
        VariableNode::Unresolved => None,
    }
}

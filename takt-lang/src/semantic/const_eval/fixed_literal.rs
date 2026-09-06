//! Приведение **литерала** к `q(m, n)` - общий носитель.

use super::fixed_repr;
use crate::semantic::ExpressionNode;
use crate::semantic::type_node::TypeNode;
use crate::semantic::variable::VariableNode;

/// Представление приведения `литерал as q(m, n)`, если источник - литерал.
///
/// `None` - "источник не литерал": пусть отвечает прежняя ветвь цели (печать
/// масштабирования у `c`, отказ у прочих). Ошибаться этот признак может только в
/// сторону прежнего поведения.
///
/// Имя константы литералом **не** считается: значение там тоже известно, но вопрос
/// принадлежит константному вычислителю, а не печатнику (граница названа в и вынесена
/// кандидатом).
pub(crate) fn cast_repr(inner: &ExpressionNode, target: &TypeNode) -> Option<i64> {
    let TypeNode::Fixed { m, n, sat } = target else {
        return None;
    };
    let raw = literal_repr(inner, *n)?;
    Some(fixed_repr::normalize(raw, *m, *n, *sat))
}

/// "Сырое" представление литерала - до нормализации по `W`.
fn literal_repr(expr: &ExpressionNode, n: u8) -> Option<i128> {
    match expr {
        // Скобки прозрачны: `(2.5) as q(8, 8)` - тот же литерал.
        ExpressionNode::Parenthesis(inner) => literal_repr(inner, n),
        ExpressionNode::Negate(inner) => literal_repr(inner, n).map(|v| -v),
        ExpressionNode::Number(v) => Some(fixed_repr::from_int(*v, n)),
        ExpressionNode::Rational(text, negative) => {
            fixed_repr::from_decimal_text(text, *negative, n)
        }
        // Имя константы - тот же случай: её инициализатор свёрнут в литерал ещё на
        // стадии 2, и в ячейке ссылки лежит именно он. Спрашивать вычислитель заново не
        // нужно - и нечем: печатник модели не видит.
        //
        // Изменяемая переменная сюда не подпадает: её значение известно только в такте.
        ExpressionNode::Variable(cell) => match &*cell.borrow() {
            VariableNode::Const { expr, .. } => literal_repr(expr, n),
            _ => None,
        },
        _ => None,
    }
}

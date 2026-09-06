//! Приоритет операций C - для расстановки скобок.
//!
//! Часть модуля `c_expr`.

use super::*;

/// Возвращает C-приоритет выражения (больше = сильнее связывает).
///
/// Используется для минимизации лишних скобок: обёртка добавляется только если
/// приоритет дочернего узла ниже требуемого минимума от родителя.
pub(in crate::generator::c) fn expr_precedence(expr: &ExpressionNode) -> u8 {
    match expr {
        // Присваивание - наименьший приоритет
        ExpressionNode::Assign(..) => 1,
        // Тернарный оператор
        ExpressionNode::ConditionalOperator(..) => 2,
        // Логическое или
        ExpressionNode::Or(..) => 3,
        // Логическое И
        ExpressionNode::And(..) => 4,
        // Побитовое или
        ExpressionNode::BitwiseOr(..) => 5,
        // Побитовое исключающее или
        ExpressionNode::BitwiseXor(..) => 6,
        // Побитовое И
        ExpressionNode::BitwiseAnd(..) => 7,
        // Равенство / неравенство
        ExpressionNode::Equal(..) | ExpressionNode::NotEqual(..) => 8,
        // Сравнение
        ExpressionNode::Less(..)
        | ExpressionNode::More(..)
        | ExpressionNode::LessEqual(..)
        | ExpressionNode::MoreEqual(..) => 9,
        // Битовые сдвиги
        ExpressionNode::ShiftLeft(..) | ExpressionNode::ShiftRight(..) => 10,
        // Аддитивные операторы
        ExpressionNode::Add(..) | ExpressionNode::Subtract(..) => 11,
        // Мультипликативные операторы
        ExpressionNode::Multiply(..) | ExpressionNode::Divide(..) | ExpressionNode::Modulo(..) => {
            12
        }
        // Унарные операторы и приведение типов
        ExpressionNode::Not(..)
        | ExpressionNode::BitwiseNot(..)
        | ExpressionNode::UnaryPlus(..)
        | ExpressionNode::Negate(..)
        | ExpressionNode::Cast(..) => 13,
        // Атомы: литералы, переменные, вызовы функций, скобки и т.п.
        _ => 15,
    }
}

//! Сдвиг на величину, не меньшую ширины типа, - **один** признак на цели.
//!
//! # Что здесь есть и чего здесь нет
//!
//! Здесь - **признак**: помещается ли литеральная величина в тип, и какое значение даёт
//! эталон, если нет. Здесь **нет** идиомы печати: `0` у обеих целей выглядит одинаково,
//! а знаковый случай `rust` печатает `(v >> (W − 1))`, цель `c` - так же, но круглые
//! скобки и приоритеты у них свои.
//!
//! **Порог принадлежит целевому языку, а не правилу.** В Rust сдвиг на
//! величину, не меньшую ширины типа, - ошибка компиляции (литерал) либо паника
//! (переменная), поэтому порог там равен ширине типа Takt. В C операнды
//! **продвигаются** до `int` (C11 6.5.7p3), поэтому `u8 >> 8` определено и
//! считается верно - UB начинается с ширины **продвинутого** типа. Отсюда у
//! цели `c` порог `max(32, W)`, и оттого вывод корпуса не меняется: сдвигов
//! такой величины в `examples/` нет ни одного.

use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, VariableNode};

/// Направление сдвига: природа общая, насыщения разные.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Direction {
    Left,
    Right,
}

/// Что печатать вместо сдвига.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Saturation {
    /// Величина помещается - печатает вызывающий, обычным оператором.
    AsIs,
    /// Все разряды ушли: `0`.
    Zero,
    /// Остался только знак: сдвиг на `W − 1` даёт −1 либо 0 - ровно то, что вычисляет
    /// эталон.
    SignOnly(u8),
}

/// Решение для **литеральной** величины сдвига.
///
/// `threshold` - ширина, начиная с которой целевой язык считает сдвиг неопределённым: у
/// Rust это ширина типа, у C - ширина продвинутого типа (см. шапку модуля). Тип и знак
/// берутся у **операнда**, а не у порога.
///
/// `AsIs` - и когда тип операнда статически неизвестен: гадать нельзя, а прежнее
/// поведение верно для всех величин, которые целевой язык принимает.
pub(crate) fn literal_saturation(
    direction: Direction,
    value: &ExpressionNode,
    amount: &ExpressionNode,
    threshold: u8,
) -> Saturation {
    let Some(bits) = width_of(value) else {
        return Saturation::AsIs;
    };
    let Some(shift) = literal(amount) else {
        return Saturation::AsIs;
    };
    if shift < i128::from(threshold) {
        return Saturation::AsIs;
    }
    // Отрицательная величина под правило не подпадает: эталон отвечает `SIM-002` и
    // останавливает прогон - у записи нет верного значения вовсе (разделение
    // обязанностей).
    if shift < 0 {
        return Saturation::AsIs;
    }
    match (direction, signed_of(value)) {
        (Direction::Right, true) => Saturation::SignOnly(bits - 1),
        _ => Saturation::Zero,
    }
}

/// Ширина типа выражения в битах, если она известна статически.
pub(crate) fn width_of(expr: &ExpressionNode) -> Option<u8> {
    match type_of(expr)? {
        TypeNode::Integer { bits, .. } => Some(bits),
        _ => None,
    }
}

/// Знаковый ли тип выражения.
pub(crate) fn signed_of(expr: &ExpressionNode) -> bool {
    matches!(type_of(expr), Some(TypeNode::Integer { signed: true, .. }))
}

/// Тип выражения - по объявлению переменной либо по явному приведению.
pub(crate) fn type_of(expr: &ExpressionNode) -> Option<TypeNode> {
    match expr {
        ExpressionNode::Variable(var) => match &*var.borrow() {
            VariableNode::Simple { ty, .. } | VariableNode::Const { ty, .. } => Some(ty.clone()),
            _ => None,
        },
        ExpressionNode::Cast(_, ty) => Some(ty.clone()),
        ExpressionNode::Parenthesis(inner) => type_of(inner),
        _ => None,
    }
}

/// Целое значение литерала величины сдвига.
pub(crate) fn literal(expr: &ExpressionNode) -> Option<i128> {
    match expr {
        ExpressionNode::Number(v) => Some(*v),
        ExpressionNode::Parenthesis(inner) => literal(inner),
        _ => None,
    }
}

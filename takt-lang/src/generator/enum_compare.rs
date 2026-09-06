//! Имя варианта перечисления рядом с целым операндом - общий признак.
//!
//! В Takt вариант перечисления есть число, поэтому `ref Halted: op = Hlt;` при `var op:
//! u8` - обычная запись: её исполняет эталон и переводят цели `c`, `c-hal`, `st`,
//! `st-at`. У целей с строгой типизацией имя варианта в этой позиции невыразимо:
//!
//! | Цель | Ответ инструмента до 0508 |
//! |---|---|
//! | `rust` | `E0308`: "expected `u8`, found `Op`" |
//! | `sv`, `sv-mmio` | `WIDTHEXPAND`: EQ ждёт 8 бит, `ENUMITEMREF` даёт 2 |
//!
//! Код возврата `taktc` при этом **нулевой**.
//!
//! Признак живёт здесь, а не в семантике: свернуть вариант в число на семантике значило
//! бы отнять имя у цели `c`, где константа перечисления печатается **именем** и
//! читается человеком. Форму печатает цель, признак - один.
//!
//! Соседний класс - обратный: Число рядом с перечислимым операндом (`ref Done: c =
//! 1;`), где вариант, наоборот, восстанавливается по значению. Оба правила смотрят на
//! тип соседа, а не на свою сторону.

use crate::semantic::ConditionNode;
use crate::semantic::type_node::TypeNode;

/// Что печатать вместо имени варианта.
pub(crate) struct Lowered {
    /// Значение варианта - им заменяется имя.
    pub(crate) value: i128,
    /// Сторона, на которой стоит имя варианта.
    pub(crate) variant_is_left: bool,
}

/// Опознаёт пару "имя варианта ↔ целый операнд".
///
/// `None` - пара иная (оба перечислимые, оба целые, тип соседа неизвестен): тогда
/// печать идёт прежним путём.
pub(crate) fn lowered(a: &ConditionNode, b: &ConditionNode) -> Option<Lowered> {
    match (variant_value(a), variant_value(b)) {
        (Some(value), None) if is_integer(b) => Some(Lowered {
            value,
            variant_is_left: true,
        }),
        (None, Some(value)) if is_integer(a) => Some(Lowered {
            value,
            variant_is_left: false,
        }),
        _ => None,
    }
}

/// Значение варианта, если операнд - имя варианта (в том числе в скобках).
fn variant_value(cond: &ConditionNode) -> Option<i128> {
    match cond {
        ConditionNode::EnumVariant(_, _, value) => Some(*value),
        ConditionNode::Parenthesis(inner) => variant_value(inner),
        _ => None,
    }
}

/// Целый ли тип у операнда. Тип берётся у объявления (`mixed_sign`), поэтому
/// перечислимая переменная сюда не попадает - и правило её не трогает.
fn is_integer(cond: &ConditionNode) -> bool {
    matches!(
        crate::generator::mixed_sign::operand_type_cond(cond),
        Some(TypeNode::Integer { .. })
    )
}

#[cfg(test)]
mod tests {
    use super::lowered;
    use crate::semantic::ConditionNode;
    use crate::semantic::EnumDefinitionNode;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn variant(value: i128) -> ConditionNode {
        let def = Rc::new(RefCell::new(EnumDefinitionNode {
            name: "Op".to_string(),
            variants: vec![("Hlt".to_string(), value)],
            ..Default::default()
        }));
        ConditionNode::EnumVariant(def, "Hlt".to_string(), value)
    }

    /// Пара "имя варианта ↔ число" правилу не подлежит: у числа нет объявления, а
    /// печать литерала целям и так по силам.
    #[test]
    fn number_peer_is_left_alone() {
        assert!(lowered(&variant(3), &ConditionNode::Number(3)).is_none());
    }

    /// Два имени варианта - тоже не предмет: сравнение однотипно.
    #[test]
    fn two_variants_are_left_alone() {
        assert!(lowered(&variant(1), &variant(2)).is_none());
    }
}

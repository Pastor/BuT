//! Неиспользуемая локальная переменная тела - общий признак.

use std::collections::HashSet;

use crate::semantic::StatementNode;
use crate::semantic::unused::{UsageSet, usage_from_stmt};

/// Имена локальных объявлений верхнего уровня блока, к которым блок не обращается, в
/// порядке объявления.
///
/// Порядок - текстовый, значит детерминированный.
pub(crate) fn unused_locals(block: &[StatementNode]) -> Vec<String> {
    crate::semantic::unused::unused_locals_of_block(block)
        .into_iter()
        .map(|(name, _)| name)
        .collect()
}

/// Прежняя реализация - перенесена в `semantic::unused`: признак понадобился и
/// семантике (`SE-036`), а зависимость "семантика -> генератор" была бы неверным
/// направлением.
#[allow(dead_code)]
fn unused_locals_moved(block: &[StatementNode]) -> Vec<String> {
    let mut used = UsageSet::default();
    for stmt in block {
        usage_from_stmt(stmt, &mut used);
    }
    let mut seen = HashSet::new();
    let mut out = Vec::new();
    for stmt in block {
        let StatementNode::Variable(name, _, _, _) = stmt else {
            continue;
        };
        // Объявление своё имя использованием не считает: `usage_from_stmt` берёт у
        // объявления только инициализатор. На этом признак и стоит.
        if !used.variables.contains(name) && seen.insert(name.clone()) {
            out.push(name.clone());
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::Location;
    use crate::semantic::type_node::TypeNode;
    use crate::semantic::{ExpressionNode, StatementNode};

    fn u8t() -> TypeNode {
        TypeNode::Integer {
            bits: 8,
            signed: false,
        }
    }

    fn decl(name: &str) -> StatementNode {
        StatementNode::Variable(
            name.to_string(),
            u8t(),
            Some(Box::new(ExpressionNode::Number(1))),
            crate::diagnostics::Location::Implicit,
        )
    }

    /// Объявление без единого обращения попадает в список.
    #[test]
    fn declaration_without_uses_is_reported() {
        let block = vec![decl("spare")];
        assert_eq!(unused_locals(&block), vec!["spare".to_string()]);
    }

    /// **Контрпример:** прочитанное объявление в список не попадает.
    ///
    /// Без него правило читалось бы как "гасим любое локальное объявление".
    #[test]
    fn read_declaration_is_not_reported() {
        let cell = std::rc::Rc::new(std::cell::RefCell::new(
            crate::semantic::VariableNode::Simple {
                upper: None,
                loc: crate::diagnostics::Location::Codegen,
                name: "live".to_string(),
                ty: u8t(),
                expr: ExpressionNode::None,
            },
        ));
        let block = vec![
            decl("live"),
            StatementNode::Return(
                Some(Box::new(ExpressionNode::Variable(cell))),
                Location::Codegen,
            ),
        ];
        assert!(unused_locals(&block).is_empty());
    }
}

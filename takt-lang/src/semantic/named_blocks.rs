//! Построение именованных блоков состояния.
//!
//! Слой один: блоки тела состояния (`enter`, `exit`, `always`, `every`) и вставка
//! `assembly`, которая разворачивается в блок `always` того же состояния.

use crate::diagnostics::lang::keys;
use crate::msg;
use crate::diagnostics::Diagnostic;
use crate::parser::ast::{StateDefine, StateElement};
use crate::semantic::{ModelNode, NamedCodeBlockDefinitionNode, StatementNode};
use std::cell::RefCell;
use std::rc::Weak;

/// Извлекает именованные блоки (`enter`/`exit`/`always`/`every`) состояния как
/// `Statement::Unresolved`; разрешение - в стадии 4. Одноимённые блоки (напр. два
/// `always`) сохраняются все и доступны через `get_named_blocks`.
pub(crate) fn construct_named_blocks(
    state: &StateDefine,
    upper: Option<Weak<RefCell<ModelNode>>>,
) -> Result<Vec<NamedCodeBlockDefinitionNode>, Diagnostic> {
    let mut named_blocks = Vec::new();
    for element in state.elements.iter() {
        if let StateElement::NamedBlockCode(def) = element {
            let name = def
                .name
                .as_ref()
                .ok_or_else(|| {
                    Diagnostic::error(
                        def.loc,
                        msg!(keys::SE_018_BLOCK_NEEDS_NAME),
                    )
                    .with_code("SE-018")
                })?
                .name
                .clone();
            let block = match name.as_str() {
                "enter" => NamedCodeBlockDefinitionNode::Enter {
                    upper: upper.clone(),
                    body: StatementNode::Unresolved(def.statement.clone()),
                },
                "exit" => NamedCodeBlockDefinitionNode::Exit {
                    upper: upper.clone(),
                    body: StatementNode::Unresolved(def.statement.clone()),
                },
                "always" => NamedCodeBlockDefinitionNode::Always {
                    upper: upper.clone(),
                    body: StatementNode::Unresolved(def.statement.clone()),
                },
                name => NamedCodeBlockDefinitionNode::Unknown {
                    upper: upper.clone(),
                    name: name.to_string(),
                    body: StatementNode::Unresolved(def.statement.clone()),
                },
            };
            named_blocks.push(block);
        } else if let StateElement::Assembly(block) = element {
            // Вставка уровня состояния разворачивается в блок `always` этого состояния:
            // места `formula` и `assembly` выровнены решением, а своей семантики
            // выравнивание не вводит - вставка исполняется там же, где исполнялась бы,
            // будучи написанной внутри `always`.
            named_blocks.push(NamedCodeBlockDefinitionNode::Always {
                upper: upper.clone(),
                body: StatementNode::Unresolved((**block).clone()),
            });
        } else if let StateElement::Every(def) = element {
            named_blocks.push(NamedCodeBlockDefinitionNode::Every {
                upper: upper.clone(),
                period_nanos: def.nanos,
                text: def.text.clone(),
                body: StatementNode::Unresolved(def.body.clone()),
            });
        }
    }
    Ok(named_blocks)
}

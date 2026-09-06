//! Автодополнение.
//!
//! Часть модуля `lsp`.

use super::*;

/// Генерирует элементы автодополнения для источника Takt.
///
/// Возвращает ключевые слова языка, а также идентификаторы из семантической модели
/// документа (имена моделей, состояний, переменных, функций).
pub fn completion_items(source: &str) -> Vec<CompletionItem> {
    let mut items: Vec<CompletionItem> = Vec::new();

    // Добавляем ключевые слова
    for (keyword, description) in TAKT_KEYWORDS {
        items.push(CompletionItem {
            label: keyword.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(description.to_string()),
            ..Default::default()
        });
    }

    // Добавляем встроенные типы
    for (type_name, description) in TAKT_BUILTIN_TYPES {
        items.push(CompletionItem {
            label: type_name.to_string(),
            kind: Some(CompletionItemKind::TYPE_PARAMETER),
            detail: Some(description.to_string()),
            ..Default::default()
        });
    }

    // Добавляем идентификаторы из семантической модели
    if let Ok((ast, _)) = crate::parse(source, 0)
        && let Ok(model) = semantic::tree::construct_model(&ast, None, &[])
    {
        let borrowed = model.borrow();

        // Имена вложенных моделей
        for name in borrowed.models.keys() {
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some("модель".to_string()),
                ..Default::default()
            });
        }

        // Имена переменных и их типы
        for (name, var) in &borrowed.variables {
            let detail = match var {
                VariableNode::Simple { ty, .. } => Some(format!("{}", ty)),
                VariableNode::Const { ty, .. } => Some(format!("const: {}", ty)),
                VariableNode::Port { ty, .. } => Some(format!("port: {}", ty)),
                VariableNode::Unresolved => None,
            };
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::VARIABLE),
                detail,
                ..Default::default()
            });
        }

        // Имена функций
        for (name, func) in &borrowed.functions {
            let detail = match func {
                FunctionDefinitionNode::Local { ret, .. } => Some(format!("fn -> {}", ret)),
                FunctionDefinitionNode::External { ret, .. } => {
                    Some(format!("extern fn -> {}", ret))
                }
                FunctionDefinitionNode::Builtin(_, _, ret) => {
                    Some(format!("builtin fn -> {}", ret))
                }
                _ => None,
            };
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail,
                ..Default::default()
            });
        }

        // Псевдонимы типов
        for name in borrowed.types.keys() {
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::TYPE_PARAMETER),
                detail: Some("тип".to_string()),
                ..Default::default()
            });
        }

        // Именованные условия
        for name in borrowed.conditions.keys() {
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::VALUE),
                detail: Some("cond".to_string()),
                ..Default::default()
            });
        }

        // Перечисления и их варианты
        for (enum_name, enum_node) in &borrowed.enums {
            items.push(CompletionItem {
                label: enum_name.clone(),
                kind: Some(CompletionItemKind::ENUM),
                detail: Some("enum".to_string()),
                ..Default::default()
            });
            for (variant_name, variant_val) in &enum_node.variants {
                items.push(CompletionItem {
                    label: variant_name.clone(),
                    kind: Some(CompletionItemKind::ENUM_MEMBER),
                    detail: Some(format!("{}::{} = {}", enum_name, variant_name, variant_val)),
                    ..Default::default()
                });
            }
        }

        // Имена состояний
        for (state_name, state) in &borrowed.states {
            let kind_str = match state {
                semantic::StateNode::Simple { kind, .. }
                | semantic::StateNode::Implement { kind, .. } => {
                    if matches!(kind, crate::semantic::StateNodeKind::Start) {
                        "start state"
                    } else {
                        "state"
                    }
                }
                semantic::StateNode::Unresolved => "state",
            };
            items.push(CompletionItem {
                label: state_name.clone(),
                kind: Some(CompletionItemKind::MODULE),
                detail: Some(kind_str.to_string()),
                ..Default::default()
            });
        }
    }

    items
}

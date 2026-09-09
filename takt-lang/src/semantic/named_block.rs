//! Разрешение именованных блоков кода языка Takt.
//!
//! Функция [`resolve_named_blocks`] преобразует список
//! [`NamedCodeBlockDefinitionNode`], вызывая [`resolve_statement`] для каждого блока с
//! его [`StatementNode`].
//!
//! Вариант [`NamedCodeBlockDefinitionNode::Unresolved`] (имя + сырой АСД-оператор)
//! разрешается в конкретный вариант: `Enter`, `Exit`, `Always` или `Unknown`.

use crate::diagnostics::Diagnostic;
use crate::diagnostics::lang::keys;
use crate::msg;
use crate::semantic::statement::resolve_statement;
use crate::semantic::{ModelNode, NamedCodeBlockDefinitionNode, StatementNode};
use std::cell::RefCell;
use std::rc::Rc;

/// Разрешает список именованных блоков кода, превращая каждый
/// [`NamedCodeBlockDefinitionNode::Unresolved`] в конкретный вариант (`Enter`, `Exit`,
/// `Always` или `Unknown`), и повторно разрешает тело уже-разрешённых блоков с помощью
/// [`resolve_statement`].
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`], если встречается [`NamedCodeBlockDefinitionNode::None`]
/// (неопределённый блок) или если [`resolve_statement`] завершается с ошибкой.
pub fn resolve_named_blocks(
    named_blocks: Vec<NamedCodeBlockDefinitionNode>,
    model: Rc<RefCell<ModelNode>>,
) -> Result<Vec<NamedCodeBlockDefinitionNode>, Diagnostic> {
    let mut blocks = Vec::with_capacity(named_blocks.len());
    for nb in named_blocks {
        let block = match nb {
            NamedCodeBlockDefinitionNode::None => {
                // Внутренний инвариант: неразрешённый блок сюда не доходит - его форму
                // проверяет разбор именованных блоков (`SE-045`).
                return Err(crate::semantic::internal::internal(&msg!(
                    keys::SE_119_NAMED_BLOCK_WITHOUT_BODY
                )));
            }
            NamedCodeBlockDefinitionNode::Unresolved(name, stmt) => {
                let stmt =
                    resolve_statement(&StatementNode::Unresolved(stmt), vec![], model.clone())?;
                match name.as_str() {
                    "enter" => NamedCodeBlockDefinitionNode::Enter {
                        upper: Some(Rc::downgrade(&model)),
                        body: stmt,
                    },
                    "exit" => NamedCodeBlockDefinitionNode::Exit {
                        upper: Some(Rc::downgrade(&model)),
                        body: stmt,
                    },
                    "always" => NamedCodeBlockDefinitionNode::Always {
                        upper: Some(Rc::downgrade(&model)),
                        body: stmt,
                    },
                    name => NamedCodeBlockDefinitionNode::Unknown {
                        upper: Some(Rc::downgrade(&model)),
                        name: name.to_string(),
                        body: stmt,
                    },
                }
            }
            NamedCodeBlockDefinitionNode::Enter { upper, body } => {
                NamedCodeBlockDefinitionNode::Enter {
                    upper,
                    body: resolve_statement(&body, vec![], model.clone())?,
                }
            }
            NamedCodeBlockDefinitionNode::Exit { upper, body } => {
                NamedCodeBlockDefinitionNode::Exit {
                    upper,
                    body: resolve_statement(&body, vec![], model.clone())?,
                }
            }
            NamedCodeBlockDefinitionNode::Always { upper, body } => {
                NamedCodeBlockDefinitionNode::Always {
                    upper,
                    body: resolve_statement(&body, vec![], model.clone())?,
                }
            }
            NamedCodeBlockDefinitionNode::Unknown { upper, name, body } => {
                NamedCodeBlockDefinitionNode::Unknown {
                    upper,
                    name: name.clone(),
                    body: resolve_statement(&body, vec![], model.clone())?,
                }
            }
            NamedCodeBlockDefinitionNode::Every {
                upper,
                period_nanos,
                text,
                body,
            } => NamedCodeBlockDefinitionNode::Every {
                upper,
                period_nanos,
                text,
                body: resolve_statement(&body, vec![], model.clone())?,
            },
        };
        blocks.push(block);
    }
    Ok(blocks)
}

// -- Тесты ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::Location;
    use crate::parser::ast;

    fn empty_model() -> Rc<RefCell<ModelNode>> {
        Rc::new(RefCell::new(ModelNode::default()))
    }

    // -- NamedCodeBlock::None -> ошибка -----------------------------------------

    /// `NamedCodeBlock::None` в списке - ошибка (блок не определён).
    ///
    /// # Контрпример
    /// Блок без оператора недопустим и должен производить диагностику.
    #[test]
    fn none_block_returns_error() {
        let result = resolve_named_blocks(vec![NamedCodeBlockDefinitionNode::None], empty_model());
        assert!(result.is_err(), "NamedCodeBlock::None должен давать ошибку");
    }

    // -- NamedCodeBlock::Unresolved -> разрешение -------------------------------

    /// Безвредный оператор-заглушка: `continue` здесь не годится - вне цикла он
    /// отвергается `SE-132`, а предмет этих тестов - разбор имён блоков, а не место
    /// прерывания.
    fn noop_stmt() -> ast::Statement {
        ast::Statement::Block {
            loc: Location::default(),
            unchecked: false,
            statements: Vec::new(),
        }
    }

    /// `Unresolved("enter", ...)` -> `NamedCodeBlock::Enter`.
    ///
    /// # Пример
    /// ```text
    /// NamedCodeBlock::Unresolved("enter", stmt) -> Enter { body: resolved_stmt, .. }
    /// ```
    #[test]
    fn unresolved_enter_resolves_to_enter() {
        let nb = NamedCodeBlockDefinitionNode::Unresolved("enter".into(), noop_stmt());
        let result = resolve_named_blocks(vec![nb], empty_model()).unwrap();
        assert!(
            matches!(result[0], NamedCodeBlockDefinitionNode::Enter { .. }),
            "ожидался Enter, получен {:?}",
            result[0]
        );
    }

    /// `Unresolved("exit", ...)` -> `NamedCodeBlock::Exit`.
    #[test]
    fn unresolved_exit_resolves_to_exit() {
        let nb = NamedCodeBlockDefinitionNode::Unresolved("exit".into(), noop_stmt());
        let result = resolve_named_blocks(vec![nb], empty_model()).unwrap();
        assert!(matches!(
            result[0],
            NamedCodeBlockDefinitionNode::Exit { .. }
        ));
    }

    /// `Unresolved("always", ...)` -> `NamedCodeBlock::Always`.
    #[test]
    fn unresolved_always_resolves_to_always() {
        let nb = NamedCodeBlockDefinitionNode::Unresolved("always".into(), noop_stmt());
        let result = resolve_named_blocks(vec![nb], empty_model()).unwrap();
        assert!(matches!(
            result[0],
            NamedCodeBlockDefinitionNode::Always { .. }
        ));
    }

    /// `Unresolved("custom", ...)` -> `NamedCodeBlock::Unknown { name: "custom", .. }`.
    ///
    /// Пользовательские именованные блоки сохраняются как `Unknown`.
    #[test]
    fn unresolved_custom_resolves_to_unknown() {
        let nb = NamedCodeBlockDefinitionNode::Unresolved("custom".into(), noop_stmt());
        let result = resolve_named_blocks(vec![nb], empty_model()).unwrap();
        assert!(
            matches!(&result[0], NamedCodeBlockDefinitionNode::Unknown { name, .. } if name == "custom"),
            "ожидался Unknown {{ custom }}, получен {:?}",
            result[0]
        );
    }

    // -- Уже разрешённые блоки -------------------------------------------------

    /// Уже разрешённый `Enter { body: Unresolved(..) }` ещё раз разрешается.
    #[test]
    fn already_enter_re_resolves() {
        let stmt = StatementNode::Unresolved(noop_stmt());
        let nb = NamedCodeBlockDefinitionNode::Enter {
            upper: None,
            body: stmt,
        };
        let result = resolve_named_blocks(vec![nb], empty_model()).unwrap();
        assert!(matches!(
            result[0],
            NamedCodeBlockDefinitionNode::Enter { .. }
        ));
    }

    /// Уже разрешённый `Exit` ещё раз разрешается.
    #[test]
    fn already_exit_re_resolves() {
        let nb = NamedCodeBlockDefinitionNode::Exit {
            upper: None,
            body: StatementNode::Unresolved(noop_stmt()),
        };
        let result = resolve_named_blocks(vec![nb], empty_model()).unwrap();
        assert!(matches!(
            result[0],
            NamedCodeBlockDefinitionNode::Exit { .. }
        ));
    }

    /// Уже разрешённый `Always` ещё раз разрешается.
    #[test]
    fn already_always_re_resolves() {
        let nb = NamedCodeBlockDefinitionNode::Always {
            upper: None,
            body: StatementNode::Unresolved(noop_stmt()),
        };
        let result = resolve_named_blocks(vec![nb], empty_model()).unwrap();
        assert!(matches!(
            result[0],
            NamedCodeBlockDefinitionNode::Always { .. }
        ));
    }

    /// Уже разрешённый `Unknown` ещё раз разрешается, имя сохраняется.
    #[test]
    fn already_unknown_re_resolves() {
        let nb = NamedCodeBlockDefinitionNode::Unknown {
            upper: None,
            name: "tick".into(),
            body: StatementNode::Unresolved(noop_stmt()),
        };
        let result = resolve_named_blocks(vec![nb], empty_model()).unwrap();
        assert!(
            matches!(&result[0], NamedCodeBlockDefinitionNode::Unknown { name, .. } if name == "tick"),
            "ожидался Unknown(tick)"
        );
    }

    /// Несколько блоков разрешаются последовательно.
    #[test]
    fn multiple_blocks_all_resolve() {
        let blocks = vec![
            NamedCodeBlockDefinitionNode::Unresolved("enter".into(), noop_stmt()),
            NamedCodeBlockDefinitionNode::Unresolved("exit".into(), noop_stmt()),
            NamedCodeBlockDefinitionNode::Unresolved("always".into(), noop_stmt()),
        ];
        let result = resolve_named_blocks(blocks, empty_model()).unwrap();
        assert_eq!(result.len(), 3);
        assert!(matches!(
            result[0],
            NamedCodeBlockDefinitionNode::Enter { .. }
        ));
        assert!(matches!(
            result[1],
            NamedCodeBlockDefinitionNode::Exit { .. }
        ));
        assert!(matches!(
            result[2],
            NamedCodeBlockDefinitionNode::Always { .. }
        ));
    }
}

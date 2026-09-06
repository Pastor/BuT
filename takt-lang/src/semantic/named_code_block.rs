//! Семантический узел именованного блока кода (`enter`/`exit`/`always`/`every`...).
//!
//! Вынесено из `semantic/mod.rs` (лимит размера модуля, добавила вариант `Every`).
//! Контракт держит реэкспорт `pub use` в `mod.rs` - пути потребителей
//! (`crate::semantic::NamedCodeBlockDefinitionNode`) не меняются.

use crate::parser::ast;
use crate::semantic::{ModelNode, StatementNode};
use std::cell::RefCell;
use std::rc::{Rc, Weak};

/// Семантический узел именованного блока кода (`enter`, `exit`, `always`, ...).
#[derive(Default, Debug, Clone)]
pub enum NamedCodeBlockDefinitionNode {
    /// Блок не задан.
    #[default]
    None,
    /// Неразрешённый блок кода: `(имя, AST-оператор)`.
    Unresolved(String, ast::Statement),
    /// Блок `enter` с разрешённым оператором.
    Enter {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Тело блока.
        body: StatementNode,
    },
    /// Блок `exit` с разрешённым оператором.
    Exit {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Тело блока.
        body: StatementNode,
    },
    /// Блок `always` с разрешённым оператором.
    Always {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Тело блока.
        body: StatementNode,
    },
    /// Пользовательский именованный блок.
    Unknown {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Имя блока.
        name: String,
        /// Тело блока.
        body: StatementNode,
    },
    /// Периодическое действие `every Nms { ... }`.
    ///
    /// Сахар над механизмом времени: тело исполняется, пока автомат в
    /// состоянии-владельце, каждые `period_nanos` (в профиле "часы") либо каждые
    /// столько же тактов (в профиле "такты"). Скрытое состояние - поглощённая
    /// срабатываниями `elapsed`-величина, видима в трассе симулятора.
    Every {
        /// Родительская модель (слабая ссылка для предотвращения циклов Rc).
        upper: Option<Weak<RefCell<ModelNode>>>,
        /// Период в наносекундах (каноническое представление литерала).
        period_nanos: i64,
        /// Литерал периода как записан (`100ms`) - для форматтера.
        text: String,
        /// Тело, исполняемое с этим периодом.
        body: StatementNode,
    },
}

impl PartialEq for NamedCodeBlockDefinitionNode {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::None, Self::None) => true,
            (Self::Unresolved(n1, s1), Self::Unresolved(n2, s2)) => n1 == n2 && s1 == s2,
            (Self::Enter { body: b1, .. }, Self::Enter { body: b2, .. }) => b1 == b2,
            (Self::Exit { body: b1, .. }, Self::Exit { body: b2, .. }) => b1 == b2,
            (Self::Always { body: b1, .. }, Self::Always { body: b2, .. }) => b1 == b2,
            (
                Self::Unknown {
                    name: n1, body: b1, ..
                },
                Self::Unknown {
                    name: n2, body: b2, ..
                },
            ) => n1 == n2 && b1 == b2,
            (
                Self::Every {
                    period_nanos: p1,
                    body: b1,
                    ..
                },
                Self::Every {
                    period_nanos: p2,
                    body: b2,
                    ..
                },
            ) => p1 == p2 && b1 == b2,
            _ => false,
        }
    }
}

impl Eq for NamedCodeBlockDefinitionNode {}

impl NamedCodeBlockDefinitionNode {
    /// Возвращает имя блока.
    pub fn name(&self) -> &str {
        match self {
            NamedCodeBlockDefinitionNode::None => "",
            NamedCodeBlockDefinitionNode::Unresolved(name, _) => name,
            NamedCodeBlockDefinitionNode::Enter { .. } => "enter",
            NamedCodeBlockDefinitionNode::Exit { .. } => "exit",
            NamedCodeBlockDefinitionNode::Always { .. } => "always",
            NamedCodeBlockDefinitionNode::Unknown { name, .. } => name,
            NamedCodeBlockDefinitionNode::Every { .. } => "every",
        }
    }

    /// Возвращает ссылку на семантический оператор блока, если он разрешён.
    pub fn statement(&self) -> Option<&StatementNode> {
        match self {
            NamedCodeBlockDefinitionNode::Enter { body, .. }
            | NamedCodeBlockDefinitionNode::Exit { body, .. }
            | NamedCodeBlockDefinitionNode::Always { body, .. }
            | NamedCodeBlockDefinitionNode::Unknown { body, .. }
            | NamedCodeBlockDefinitionNode::Every { body, .. } => Some(body),
            _ => None,
        }
    }

    /// Возвращает ссылку на родительскую модель блока.
    pub fn upper(&self) -> Option<Rc<RefCell<ModelNode>>> {
        match self {
            NamedCodeBlockDefinitionNode::Enter { upper, .. }
            | NamedCodeBlockDefinitionNode::Exit { upper, .. }
            | NamedCodeBlockDefinitionNode::Always { upper, .. }
            | NamedCodeBlockDefinitionNode::Unknown { upper, .. }
            | NamedCodeBlockDefinitionNode::Every { upper, .. } => {
                upper.as_ref().and_then(|w| w.upgrade())
            }
            _ => None,
        }
    }

    /// Период `every`-блока в наносекундах и его исходный литерал (для `every`).
    pub fn every_period(&self) -> Option<(i64, &str)> {
        match self {
            NamedCodeBlockDefinitionNode::Every {
                period_nanos, text, ..
            } => Some((*period_nanos, text)),
            _ => None,
        }
    }
}

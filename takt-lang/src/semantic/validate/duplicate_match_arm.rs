//! Повторяющийся образец `match` - `SE-131`.
//!
//! # Что и почему
//!
//! `match` берёт **первое** совпадение, поэтому ветвь, чей образец уже стоял выше, не
//! сработает никогда.
//!
//! ```takt
//! match op {
//!     1 => { acc := acc + 1; }
//!     1 => { acc := acc + 10; }   // не сработает никогда
//! }
//! ```
//!
//! | Потребитель | Ответ |
//! |---|---|
//! | эталон, `st`, `st-at`, `sv`, `sv-mmio`, `plantuml` | берут первую ветвь, инструменты принимают |
//! | `c`, `c-hal` | **`cc`: "duplicate case value"** |
//! | `rust` | **`clippy`: "these `if` branches have the same condition"** |
//!
//! Код возврата `taktc` - **нулевой**: автору не говорил никто.

use crate::diagnostics::Diagnostic;
use crate::semantic::{FunctionDefinitionNode, MatchArmNode, ModelNode, StateNode, StatementNode};
use std::cell::RefCell;
use std::rc::Rc;

/// Собирает предупреждения о недостижимых ветвях `match` модели и вложенных.
pub fn check_duplicate_match_arms(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut warnings = Vec::new();
    check_model(&model, &mut warnings);
    warnings
}

fn check_model(model: &Rc<RefCell<ModelNode>>, warnings: &mut Vec<Diagnostic>) {
    let (funcs, blocks, states, nested) = {
        let b = model.borrow();
        (
            b.functions.values().cloned().collect::<Vec<_>>(),
            b.named_blocks.clone(),
            b.states.values().cloned().collect::<Vec<_>>(),
            b.models.values().map(Rc::clone).collect::<Vec<_>>(),
        )
    };
    for func in &funcs {
        if let FunctionDefinitionNode::Local { body, .. } = func {
            check_stmt(body, warnings);
        }
    }
    for block in &blocks {
        if let Some(stmt) = block.statement() {
            check_stmt(stmt, warnings);
        }
    }
    for state in &states {
        let named_blocks = match state {
            StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } => {
                named_blocks
            }
            StateNode::Unresolved => continue,
        };
        for block in named_blocks {
            if let Some(stmt) = block.statement() {
                check_stmt(stmt, warnings);
            }
        }
    }
    for child in &nested {
        check_model(child, warnings);
    }
}

/// Обход оператора: `match` судится, вложенные тела обходятся дальше.
fn check_stmt(stmt: &StatementNode, warnings: &mut Vec<Diagnostic>) {
    match stmt {
        StatementNode::Block(items) => {
            for item in items {
                check_stmt(item, warnings);
            }
        }
        StatementNode::If { then_, else_, .. } => {
            check_stmt(then_, warnings);
            if let Some(other) = else_ {
                check_stmt(other, warnings);
            }
        }
        StatementNode::Loop { body, .. } => check_stmt(body, warnings),
        StatementNode::For { init, body, .. } => {
            if let Some(init) = init {
                check_stmt(init, warnings);
            }
            check_stmt(body, warnings);
        }
        StatementNode::Match { arms, .. } => {
            check_arms(arms, warnings);
            for arm in arms {
                check_stmt(&arm.body, warnings);
            }
        }
        // Прочие операторы `match` не содержат: объявление, присваивание, `return`,
        // `break`, `continue`, формулы, вставки цели.
        _ => {}
    }
}

/// Одна диагностика на недостижимую ветвь, накопительно.
fn check_arms(arms: &[MatchArmNode], warnings: &mut Vec<Diagnostic>) {
    for index in 0..arms.len() {
        if !crate::semantic::match_arms::pattern_repeats_above(arms, index) {
            continue;
        }
        warnings.push(
            Diagnostic::warning(
                arms[index].loc,
                "образец этой ветви `match` уже встречался выше: `match` берёт \
                 ПЕРВОЕ совпадение, поэтому ветвь не сработает никогда. \
                 Объедините ветви либо уточните образец"
                    .to_string(),
            )
            .with_code("SE-131"),
        );
    }
}

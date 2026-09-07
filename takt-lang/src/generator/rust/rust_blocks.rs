//! Эмиссия именованных блоков (`enter`/`exit`/`always`) в цель Rust.
//!
//! Источников блоков два - состояние и сама модель (`always` уровня модели).

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::rust::rust_expr::Scope;
use crate::generator::rust::rust_stmt::{StmtOutput, print_statement};
use crate::semantic::{ModelNode, StateNode};

/// Печатает именованные блоки состояния (`enter`/`exit`/`always`).
pub(super) fn emit_named_blocks(
    p: &mut Printer,
    state: &StateNode,
    kind: &str,
    scope: &mut Scope,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    for block in state.get_named_blocks(kind) {
        if let Some(stmt) = block.statement() {
            print_statement(stmt, scope, p, out)?;
        }
    }
    Ok(())
}

/// Печатает именованные блоки **уровня модели**: `always` вне состояния. Аналог
/// [`emit_named_blocks`], но источник - сама модель.
pub(super) fn emit_model_named_blocks(
    p: &mut Printer,
    model: &ModelNode,
    kind: &str,
    scope: &mut Scope,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    for block in model.get_named_blocks(kind) {
        if let Some(stmt) = block.statement() {
            print_statement(stmt, scope, p, out)?;
        }
    }
    Ok(())
}

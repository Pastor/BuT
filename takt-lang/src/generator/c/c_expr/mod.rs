//! Генерация C-выражений, операторов, блоков кода и разрешение переменных.
//!
//! Содержит всю логику генерации C-выражений из семантических узлов: [`generate_expr`],
//! [`generate_code_block`], [`generate_stmt_expression`], а также вспомогательные
//! функции разрешения имён переменных и функций.

use super::{PortClass, c_type_or_diagnostic, typed_variable_or_diagnostic};
use crate::diagnostics::{Diagnostic, Location};
use crate::generator::c::c_map::CMap;
use crate::generator::indent::Printer;
use crate::parser::ast::Member;
use crate::semantic::extend::Extend;
use crate::semantic::minimap::{Element, Name};
use crate::semantic::naming::normalize_lowercase_snakecase;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{
    ConditionNode, ExpressionNode, Formula, FunctionDefinitionNode, MatchArmNode, MatchPatternNode,
    ModelNode, StateNode, StatementNode, VariableNode,
};
use std::cell::RefCell;
use std::rc::Rc;

mod aggregate;
mod call;
pub(in crate::generator::c) mod condition;
mod expr;
mod fixed;
mod names;
mod port_element;
mod precedence;
mod resolve;
mod stmt;

// Внутренние помощники, которые зовут соседние подмодули: `use super::*` в каждом
// подмодуле подхватывает их отсюда. Наружу `c_expr` они не выходят.
use call::generate_function_call;
use names::path_from_root;
use resolve::find_in_extend;

// Реэкспорт: внешние пути импорта не меняются - `c_decl.rs`, `c_model.rs` и
// `c_source.rs` продолжают писать `use crate::generator::c::c_expr::...`. Контракт
// модуля - свойство `mod.rs`, а не расположения функций внутри него.
pub(super) use condition::generate_condition_expr;
pub(super) use expr::generate_expr;
pub(super) use fixed::insert_fixed_helpers;
pub(super) use names::{field_name_in_parent, get_function_name};
pub(super) use precedence::expr_precedence;
pub(super) use resolve::{resolve_simple_var_in_context, resolve_variable_c_expr};
pub(super) use stmt::{generate_code_block, generate_formula_check, generate_stmt_expression};

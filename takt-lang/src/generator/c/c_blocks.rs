//! Эмиссия именованных блоков (`enter`/`exit`/`always`) в цель C.
//!
//! Вынесено из `c_model.rs`. Два источника блоков - состояние и **сама модель**
//! (model-level `always`); обе печати идут в одинаковых условиях (`owner` - модель),
//! различаясь лишь источником списка блоков.

use super::c_expr::{generate_code_block, generate_expr};
use crate::diagnostics::Diagnostic;
use crate::generator::c::c_map::CMap;
use crate::generator::indent::Printer;
use crate::semantic::minimap::Element;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, ModelNode, StateNode};

/// Инициализация скалярной (не массивной) переменной в `_init`: `model->name =
/// <expr>;`.
///
/// **структура** присваивается составным литералом `(Type){...}` - голый `model->p =
/// {1, 2};` невалиден (`cc: expected expression`): фигурный инициализатор допустим
/// только в объявлении, не в присваивании.
pub(super) fn generate_scalar_init(
    printer: &mut Printer,
    map: &CMap,
    owner: &Element,
    var_name: &str,
    ty: &TypeNode,
    expr: &ExpressionNode,
    scope: &std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>>,
) -> Result<(), Diagnostic> {
    let cast = if let TypeNode::Struct(s) = ty {
        format!("({})", s)
    } else {
        String::new()
    };
    printer.ident(&format!("model->{} = {}", var_name, cast));
    // Начальное значение перечислимой переменной - тоже присваивание, и тип здесь
    // известен. Без этого `_init` печатал `model->c = 0;` там, где тело уже печатает
    // `ENUM_..._STOP`: вывод противоречил бы сам себе внутри одного файла.
    if let ExpressionNode::Number(value) = expr
        && let Some(name) = crate::generator::c::c_enum::constant_of(ty, *value, scope)
    {
        printer.print(&name).print(";").nl();
        return Ok(());
    }
    generate_expr(printer, map, owner, vec![], expr, 0, true)?;
    printer.print(";").nl();
    Ok(())
}

/// Генерирует именованные блоки состояния (`enter`/`exit`/`always`).
pub(super) fn generate_named_blocks(
    printer: &mut Printer,
    state: &StateNode,
    map: &CMap,
    owner: &Element,
    block_name: &str,
) -> Result<(), Diagnostic> {
    for block in state.get_named_blocks(block_name) {
        let Some(stmt) = block.statement() else {
            continue;
        };
        generate_code_block(printer, map, owner, vec![], stmt, true)?;
    }
    Ok(())
}

/// Генерирует именованные блоки **уровня модели**: `always` вне состояния. Источник -
/// сама модель, а не состояние; тело печатается в тех же условиях (`owner` - модель).
pub(super) fn generate_model_named_blocks(
    printer: &mut Printer,
    model_node: &ModelNode,
    map: &CMap,
    owner: &Element,
    block_name: &str,
) -> Result<(), Diagnostic> {
    for block in model_node.get_named_blocks(block_name) {
        let Some(stmt) = block.statement() else {
            continue;
        };
        generate_code_block(printer, map, owner, vec![], stmt, true)?;
    }
    Ok(())
}

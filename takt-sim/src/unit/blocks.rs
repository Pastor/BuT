//! Именованные блоки **уровня модели** (`always` вне состояния) -, общий вынос.
//!
//! # Контракт
//!
//! Тело `always` уровня модели исполняется **каждый такт до диспетчеризации
//! состояния**, безусловно по состоянию (контракт 0083, ему следуют все четыре цели).
//! Исполняет его `Unit::execution("always")`, вызываемый из `tick_body` до
//! `tick_node`/`tick_parallel`/`tick_sequential`, - то есть порядок задан тактом, а не
//! этим модулем.
//!
//! Блоки кладутся в **один** юнит - тот, чья модель их объявила. Спускать их в дочерние
//! юниты нельзя: `Unit::execution` в детей не ходит намеренно, иначе тело композиции
//! исполнится по разу на ветвь.

use crate::context::Context;
use crate::unit::Executions;
use crate::unit::statement::compile_block_body;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use takt_lang::semantic::{ModelNode, NamedCodeBlockDefinitionNode};

/// Компилирует именованные блоки уровня модели в исполнители.
///
/// Ключ - имя блока (`always`); безымянные блоки пропускаются. Контекст `ctx` - тот, в
/// который блок пишет значения: он обязан быть **тем же**, который видят состояния или
/// ветви композиции, иначе запись уйдёт в другой экземпляр `ModelNodeContext` и
/// наблюдаемая не изменится.
pub(crate) fn model_level_executions(
    model: &Rc<RefCell<ModelNode>>,
    ctx: Rc<RefCell<dyn Context>>,
) -> Executions {
    let blocks: Vec<NamedCodeBlockDefinitionNode> = model.borrow().named_blocks.clone();
    let mut executions: Executions = HashMap::new();
    for block in &blocks {
        let kind = block.name();
        if kind.is_empty() {
            continue;
        }
        if let Some(body) = block.statement() {
            let fns = compile_block_body(body, ctx.clone());
            if !fns.is_empty() {
                executions.entry(kind.to_string()).or_default().extend(fns);
            }
        }
    }
    executions
}

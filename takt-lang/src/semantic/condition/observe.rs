//! Наблюдение состояния соседа сводится к общей переменной.
//!
//! # Правило
//!
//! Компилятор заводит в корне переменную "состояние модели M", обновляет её при входе в
//! каждое состояние M и заменяет условие сравнением этой переменной с номером. За
//! границей семантики формы не существует - печатники целей о ней не знают (приём
//! 0143/0192/0199/0400).
//!
//! **Номер состояния - Свой**, а не тот, что печатает цель: порядок берётся у
//! `model.states` (`BTreeMap`, детерминирован - 0048), и он один для записи и сравнения
//! по построению.
//!
//! **Обновление стоит в `enter`, и это соблюдает **: наблюдатель обязан увидеть переход
//! соседа **на том же такте**. `enter` исполняется в тике соседа, то есть до тика
//! наблюдателя, - ровно как чтение поля `main->ping.state` в порождённом C.
//!
//! **Начальное значение - номер стартового состояния**: до первого такта `enter` ещё не
//! исполнялся, и ноль означал бы состояние, в котором автомат не находится.
//!
//! Имя переменной проверяется на занятость: оно обязано быть идентификатором
//! **целевых** языков, а значит написать такое же может и автор.

use std::cell::RefCell;
use std::collections::{BTreeMap, HashSet};
use std::rc::Rc;

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::condition::state_of::{compared_state_name, state_of_model};
use crate::semantic::type_node::TypeNode;
use crate::semantic::{
    ConditionNode, ExpressionNode, ModelNode, NamedCodeBlockDefinitionNode, StateNode,
    StatementNode, VariableNode,
};

/// Префикс синтетической переменной. Обязан быть допустимым идентификатором целевых
/// языков - C, IEC, Rust, SystemVerilog.
const PREFIX: &str = "takt_state_";

/// Разворачивает наблюдение состояния соседа по всему дереву.
pub(crate) fn expand_state_observation(root: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    // Сперва собираем, чьё состояние наблюдают: заводить переменную и писать в неё
    // имеет смысл только для наблюдаемых моделей.
    let mut observed: BTreeMap<String, Rc<RefCell<ModelNode>>> = BTreeMap::new();
    collect_observed(root, &mut HashSet::new(), &mut observed);
    if observed.is_empty() {
        return Ok(());
    }
    let taken = taken_names(root, &mut HashSet::new());
    let mut cells: BTreeMap<String, Rc<RefCell<VariableNode>>> = BTreeMap::new();
    for (key, model) in &observed {
        let name = fresh_name(key, &taken);
        let cell = declare(root, &name, model);
        publish(model, &cell);
        cells.insert(key.clone(), cell);
    }
    rewrite(root, &mut HashSet::new(), &cells);
    Ok(())
}

/// Ключ модели - её уникальное имя (путь по `upper`), как у карты адресов 0084.
fn key_of(model: &Rc<RefCell<ModelNode>>) -> String {
    let mut parts = vec![model.borrow().name.clone().unwrap_or_default()];
    let mut current = model.borrow().upper.as_ref().and_then(|w| w.upgrade());
    while let Some(node) = current {
        parts.push(node.borrow().name.clone().unwrap_or_default());
        current = node.borrow().upper.as_ref().and_then(|w| w.upgrade());
    }
    parts.reverse();
    parts.join(":")
}

/// Собирает модели, чьё состояние наблюдают условия дерева.
fn collect_observed(
    model: &Rc<RefCell<ModelNode>>,
    visited: &mut HashSet<*const RefCell<ModelNode>>,
    out: &mut BTreeMap<String, Rc<RefCell<ModelNode>>>,
) {
    if !visited.insert(Rc::as_ptr(model)) {
        return;
    }
    let b = model.borrow();
    for cond in b.conditions.values() {
        scan_cond(&cond.value, out);
    }
    for state in b.states.values() {
        for reference in state.references() {
            scan_cond(&reference.cond, out);
        }
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> = b.models.values().cloned().collect();
    drop(b);
    for child in &nested {
        collect_observed(child, visited, out);
    }
}

fn scan_cond(cond: &ConditionNode, out: &mut BTreeMap<String, Rc<RefCell<ModelNode>>>) {
    if let ConditionNode::Equal(left, right) = cond
        && let Some(model) = state_of_model(left)
        && compared_state_name(right).is_some()
    {
        out.insert(key_of(model), Rc::clone(model));
        return;
    }
    match cond {
        ConditionNode::And(l, r)
        | ConditionNode::Or(l, r)
        | ConditionNode::Equal(l, r)
        | ConditionNode::NotEqual(l, r) => {
            scan_cond(l, out);
            scan_cond(r, out);
        }
        ConditionNode::Not(inner) | ConditionNode::Parenthesis(inner) => scan_cond(inner, out),
        _ => {}
    }
}

/// Имена, занятые автором, - чтобы синтетическое не затенило чужое.
fn taken_names(
    model: &Rc<RefCell<ModelNode>>,
    visited: &mut HashSet<*const RefCell<ModelNode>>,
) -> HashSet<String> {
    let mut out = HashSet::new();
    if !visited.insert(Rc::as_ptr(model)) {
        return out;
    }
    let b = model.borrow();
    out.extend(b.variables.keys().cloned());
    let nested: Vec<Rc<RefCell<ModelNode>>> = b.models.values().cloned().collect();
    drop(b);
    for child in &nested {
        out.extend(taken_names(child, visited));
    }
    out
}

/// Свободное имя переменной для модели с ключом `key`.
fn fresh_name(key: &str, taken: &HashSet<String>) -> String {
    // Имя строится в нижнем регистре: цель `c` печатает доступ через `to_lowercase`, а
    // объявление - как есть, и `takt_state__Ping` в структуре против `takt_state__ping`
    // в теле дают "use of undeclared identifier". Двойных подчёркиваний тоже не
    // оставляем.
    let base: String = key
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect::<String>()
        .to_lowercase()
        .split('_')
        .filter(|p| !p.is_empty())
        .collect::<Vec<_>>()
        .join("_");
    let mut name = format!("{PREFIX}{base}");
    let mut n = 1;
    while taken.contains(&name) {
        n += 1;
        name = format!("{PREFIX}{base}_{n}");
    }
    name
}

/// Объявляет переменную в корне и возвращает ячейку-снимок для тел.
fn declare(
    root: &Rc<RefCell<ModelNode>>,
    name: &str,
    observed: &Rc<RefCell<ModelNode>>,
) -> Rc<RefCell<VariableNode>> {
    // Начальное значение - номер стартового состояния: до первого такта `enter` не
    // исполнялся, и ноль означал бы чужое состояние.
    let start = {
        let b = observed.borrow();
        b.states
            .values()
            .position(|s| s.kind() == crate::semantic::StateNodeKind::Start)
            .unwrap_or(0)
    };
    // Позиция - `Codegen`, а не `Implicit`: цель `c` считает `Implicit` признаком
    // **локальной** переменной тела и печатает её без квалификации владельцем
    // (`takt_state_ping` вместо `main->takt_state_ping`) - "use of undeclared
    // identifier" при нулевом коде возврата `taktc`.
    let var = VariableNode::Simple {
        upper: Some(Rc::downgrade(root)),
        loc: Location::Codegen,
        name: name.to_string(),
        ty: TypeNode::Integer {
            bits: 8,
            signed: false,
        },
        expr: ExpressionNode::Number(start as i128),
    };
    root.borrow_mut()
        .variables
        .insert(name.to_string(), var.clone());
    Rc::new(RefCell::new(var))
}

/// Дописывает в `enter` каждого состояния запись его номера.
fn publish(model: &Rc<RefCell<ModelNode>>, cell: &Rc<RefCell<VariableNode>>) {
    let names: Vec<String> = model.borrow().states.keys().cloned().collect();
    for (index, state_name) in names.iter().enumerate() {
        let assign = StatementNode::Expression(
            Box::new(ExpressionNode::Assign(
                Box::new(ExpressionNode::Variable(Rc::clone(cell))),
                Box::new(ExpressionNode::Number(index as i128)),
            )),
            Location::Implicit,
        );
        let mut b = model.borrow_mut();
        let weak = Rc::downgrade(model);
        let Some(state) = b.states.get_mut(state_name) else {
            continue;
        };
        let blocks = match state {
            StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } => {
                named_blocks
            }
            StateNode::Unresolved => continue,
        };
        // Существующий `enter` дополняется, а не подменяется: тело автора обязано
        // исполниться. Запись идёт первой - она не зависит от прочего, а вот условие
        // наблюдателя от неё зависит.
        if let Some(NamedCodeBlockDefinitionNode::Enter { body, .. }) = blocks
            .iter_mut()
            .find(|b| matches!(b, NamedCodeBlockDefinitionNode::Enter { .. }))
        {
            let existing = std::mem::take(body);
            *body = StatementNode::Block(vec![assign, existing]);
        } else {
            blocks.push(NamedCodeBlockDefinitionNode::Enter {
                upper: Some(weak),
                body: assign,
            });
        }
    }
}

/// Заменяет условия наблюдения сравнением с синтетической переменной.
fn rewrite(
    model: &Rc<RefCell<ModelNode>>,
    visited: &mut HashSet<*const RefCell<ModelNode>>,
    cells: &BTreeMap<String, Rc<RefCell<VariableNode>>>,
) {
    if !visited.insert(Rc::as_ptr(model)) {
        return;
    }
    {
        let mut b = model.borrow_mut();
        let mut conditions = std::mem::take(&mut b.conditions);
        for cond in conditions.values_mut() {
            rewrite_cond(&mut cond.value, cells);
        }
        b.conditions = conditions;
        let mut states = std::mem::take(&mut b.states);
        for state in states.values_mut() {
            // Список рёбер берётся у варианта напрямую: метода-доступа у `StateNode`
            // нет, и заводить его значило бы растить `semantic/mod.rs`, который стоит в
            // реестре долга по размеру.
            let references = match state {
                StateNode::Simple { references, .. } | StateNode::Implement { references, .. } => {
                    references
                }
                StateNode::Unresolved => continue,
            };
            for reference in references.iter_mut() {
                rewrite_cond(&mut reference.cond, cells);
            }
        }
        b.states = states;
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> = model.borrow().models.values().cloned().collect();
    for child in &nested {
        rewrite(child, visited, cells);
    }
}

fn rewrite_cond(cond: &mut ConditionNode, cells: &BTreeMap<String, Rc<RefCell<VariableNode>>>) {
    // Заменяемое условие вычисляется отдельно: заимствование `cond` живо, пока в нём
    // ищут модель, и присвоение внутри `if let` компилятор не пропустит.
    let replacement = match cond {
        ConditionNode::Equal(left, right) => state_of_model(left).and_then(|model| {
            let state = compared_state_name(right)?;
            let cell = cells.get(&key_of(model))?;
            let index = model.borrow().states.keys().position(|k| *k == state)?;
            Some(ConditionNode::Equal(
                Box::new(ConditionNode::Variable(Rc::clone(cell), Location::Implicit)),
                Box::new(ConditionNode::Number(index as i128)),
            ))
        }),
        _ => None,
    };
    if let Some(new_cond) = replacement {
        *cond = new_cond;
        return;
    }
    match cond {
        ConditionNode::And(l, r)
        | ConditionNode::Or(l, r)
        | ConditionNode::Equal(l, r)
        | ConditionNode::NotEqual(l, r) => {
            rewrite_cond(l, cells);
            rewrite_cond(r, cells);
        }
        ConditionNode::Not(inner) | ConditionNode::Parenthesis(inner) => rewrite_cond(inner, cells),
        _ => {}
    }
}

/// Понижения, зависящие от цели.
///
/// Оба прохода снимают формы, которые часть целей не переводит, и зовутся не из общих
/// стадий, а из конвейера конкретной цели: у `st`/`sv` наблюдение состояния соседа
/// работает напрямую (0245/0267), у `st`/`sv` - и составной порт, и общий разворот
/// изменил бы их вывод без нужды.
///
/// Собрано **одной** точкой входа: `lib.rs` пришпилен реестром размеров, и шесть пар
/// "комментарий + вызов" в него не помещались.
pub(crate) fn lower_for_target(
    model: &std::rc::Rc<std::cell::RefCell<crate::semantic::ModelNode>>,
    split_ports: super::port_split::PortSplit,
    fold_state_observe: bool,
) -> Result<(), crate::diagnostics::Diagnostic> {
    super::port_split::split_composite_ports(model, split_ports)?;
    if fold_state_observe {
        expand_state_observation(model)?;
    }
    Ok(())
}

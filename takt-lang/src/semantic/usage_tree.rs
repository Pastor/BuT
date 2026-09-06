//! Использование имён с учётом детей по вызову.
//!
//! Признаки целей спрашивают не "что упоминает эта модель", а "что упоминает она вместе
//! с теми, кого тикает": вложенными моделями и теми, которыми реализованы её состояния
//! (`= M`, `A | B`, `A + B`). Вторых в
//! [`ModelNode::models`](crate::semantic::ModelNode) нет, и пока каждая цель считала
//! сама, три из них печатали вывод, отвергаемый их же инструментами при нулевом коде
//! возврата `taktc`.
//!
//! Модуль отделён от [`unused`](super::unused) по размеру (правило `docs/CODE.md`), а
//! не по смыслу: правило "что считать упоминанием" остаётся одно - обход тот же,
//! меняется лишь объём дерева.

use std::cell::RefCell;
use std::collections::{BTreeSet, HashSet};
use std::rc::Rc;

use crate::parser::ast::Member;
use crate::semantic::unused::{UsageSet, collect_model_usage, compute_usage};
use crate::semantic::{ExpressionNode, ModelNode, StatementNode};

/// Имена, которые модель и её дети по вызову **читают**.
///
/// Отличие от [`usage_with_implementations`] - режим `reads_only`: цель присваивания
/// местом чтения не считается. Нужен там, где сторона порта печатается по факту: у
/// двунаправленного порта цели `sv` сигнал `_i` есть **вход модуля**, и без чтения
/// `verilator` под `-Wall` отвечает `UNUSEDSIGNAL`.
pub fn reads_with_implementations(model: &Rc<RefCell<ModelNode>>) -> UsageSet {
    let mut set = UsageSet {
        reads_only: true,
        ..UsageSet::default()
    };
    collect_model_usage(Rc::clone(model), &mut set);
    let mut queue = crate::semantic::extend::implementation_children(&model.borrow());
    let mut seen: HashSet<*const RefCell<ModelNode>> = HashSet::new();
    while let Some(child) = queue.pop() {
        if !seen.insert(Rc::as_ptr(&child)) {
            continue;
        }
        collect_model_usage(Rc::clone(&child), &mut set);
        queue.extend(crate::semantic::extend::implementation_children(
            &child.borrow(),
        ));
    }
    set
}

/// Использование имён моделью **и её детьми по вызову**.
///
/// Дети по вызову - это не только вложенные модели (их обходит [`compute_usage`]), но и
/// те, которыми **реализованы состояния** (`= M`, `A | B`, `A + B`): их тик зовёт эта
/// же модель и передаёт им своё окружение.
///
/// Носитель общий у двух целей. `rust` спрашивает его о параметре `&mut Shared`, `st` -
/// о секции `VAR_IN_OUT`; до каждая считала сама через `compute_usage`, реализаций не
/// видела, и обе печатали вывод, который отвергают их инструменты при нулевом коде
/// возврата `taktc`: `rustc` - "cannot find value `shared` in this scope", `iec2c` -
/// "Ambiguous enumerate value or Variable not declared in this scope".
pub fn usage_with_implementations(model: &Rc<RefCell<ModelNode>>) -> UsageSet {
    let mut set = compute_usage(Rc::clone(model));
    let mut queue = crate::semantic::extend::implementation_children(&model.borrow());
    let mut seen: HashSet<*const RefCell<ModelNode>> = HashSet::new();
    while let Some(child) = queue.pop() {
        if !seen.insert(Rc::as_ptr(&child)) {
            continue;
        }
        let child_usage = compute_usage(Rc::clone(&child));
        set.variables.extend(child_usage.variables);
        set.constants.extend(child_usage.constants);
        set.ports.extend(child_usage.ports);
        set.functions.extend(child_usage.functions);
        queue.extend(crate::semantic::extend::implementation_children(
            &child.borrow(),
        ));
    }
    set
}

/// Поля структурного порта, которые модель и её дети по вызову **читают**.
pub fn read_port_fields(model: &Rc<RefCell<ModelNode>>, port: &str) -> BTreeSet<String> {
    let mut fields = BTreeSet::new();
    let mut queue = vec![Rc::clone(model)];
    let mut seen: HashSet<*const RefCell<ModelNode>> = HashSet::new();
    while let Some(current) = queue.pop() {
        if !seen.insert(Rc::as_ptr(&current)) {
            continue;
        }
        {
            let b = current.borrow();
            for block in &b.named_blocks {
                collect_fields(block.statement(), port, &mut fields);
            }
            for state in b.states.values() {
                for block in state.named_blocks() {
                    collect_fields(block.statement(), port, &mut fields);
                }
            }
            for func in b.functions.values() {
                if let crate::semantic::FunctionDefinitionNode::Local { body, .. } = func {
                    collect_fields(Some(body), port, &mut fields);
                }
            }
            queue.extend(b.models.values().cloned());
        }
        let children = crate::semantic::extend::implementation_children(&current.borrow());
        queue.extend(children);
    }
    fields
}

/// Собирает поля порта, читаемые оператором.
fn collect_fields(stmt: Option<&StatementNode>, port: &str, out: &mut BTreeSet<String>) {
    let Some(stmt) = stmt else {
        return;
    };
    crate::semantic::walk::walk_stmt_exprs(stmt, &mut |expr| {
        let ExpressionNode::BitAccess(base, Member::Identifier(field)) = expr else {
            return;
        };
        let ExpressionNode::Variable(cell) = &**base else {
            return;
        };
        if cell.borrow().name() == port {
            out.insert(field.name.clone());
        }
    });
}

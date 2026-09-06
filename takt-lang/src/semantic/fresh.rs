//! Свежее имя временной переменной и множество занятых автором имён.
//!
//! Носитель общий у проходов, которые заводят временные переменные в семантике:
//! разворот среза в аргументе и подъём результата вызова
//! ([`slice::argument`](crate::semantic::slice::argument), /0431/0432) и подстановка
//! тела функции ([`inline`](crate::semantic::inline)).
//!
//! **Правило "какие имена заняты" обязано быть одно.** Второй сборщик разошёлся бы с
//! первым молча, и один проход затенял бы имя, которое другой считает свободным.
//!
//! Имя временной обязано быть допустимым идентификатором **целевых** языков (C, IEC,
//! Rust, SystemVerilog) - первая редакция 0400 брала `#...`, и `cc` отвечал "expected
//! identifier". А раз такое имя может написать и автор, занятость проверяется, а не
//! предполагается.

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::semantic::{
    FunctionDefinitionNode, ModelNode, NamedCodeBlockDefinitionNode, StateNode, StatementNode,
};

/// Счётчик свежих имён с заданным преисправлением.
pub(crate) struct Fresh<'a> {
    prefix: &'static str,
    counter: usize,
    taken: &'a HashSet<String>,
}

impl<'a> Fresh<'a> {
    /// Создаёт счётчик над множеством занятых имён.
    pub(crate) fn new(prefix: &'static str, taken: &'a HashSet<String>) -> Self {
        Self {
            prefix,
            counter: 0,
            taken,
        }
    }

    /// Номер следующей подстановки (для имён вида `<префикс><n>_<имя>`).
    pub(crate) fn next_index(&mut self) -> usize {
        self.counter += 1;
        self.counter
    }

    /// Занято ли имя автором.
    pub(crate) fn is_taken(&self, name: &str) -> bool {
        self.taken.contains(name)
    }

    /// Свободное имя вида `<префикс><n>`.
    pub(crate) fn fresh_name(&mut self) -> String {
        loop {
            let index = self.next_index();
            let name = format!("{}{index}", self.prefix);
            if !self.taken.contains(&name) {
                return name;
            }
        }
    }
}

/// Имена, занятые автором во всём дереве: объявления моделей и локальные тел.
pub(crate) fn taken_names(model: &Rc<RefCell<ModelNode>>) -> HashSet<String> {
    let mut out = HashSet::new();
    collect_names(model, &mut HashSet::new(), &mut out);
    out
}

fn collect_names(
    model: &Rc<RefCell<ModelNode>>,
    visited: &mut HashSet<*const RefCell<ModelNode>>,
    out: &mut HashSet<String>,
) {
    if !visited.insert(Rc::as_ptr(model)) {
        return;
    }
    let b = model.borrow();
    out.extend(b.variables.keys().cloned());
    for func in b.functions.values() {
        if let FunctionDefinitionNode::Local { body, params, .. } = func {
            out.extend(params.iter().map(|(name, _)| name.clone()));
            collect_locals(body, out);
        }
    }
    for blk in &b.named_blocks {
        collect_block_locals(blk, out);
    }
    for st in b.states.values() {
        if let StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } =
            st
        {
            for blk in named_blocks {
                collect_block_locals(blk, out);
            }
        }
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> = b.models.values().cloned().collect();
    drop(b);
    for child in &nested {
        collect_names(child, visited, out);
    }
}

fn collect_block_locals(blk: &NamedCodeBlockDefinitionNode, out: &mut HashSet<String>) {
    match blk {
        NamedCodeBlockDefinitionNode::Enter { body, .. }
        | NamedCodeBlockDefinitionNode::Exit { body, .. }
        | NamedCodeBlockDefinitionNode::Always { body, .. }
        | NamedCodeBlockDefinitionNode::Unknown { body, .. }
        | NamedCodeBlockDefinitionNode::Every { body, .. } => collect_locals(body, out),
        NamedCodeBlockDefinitionNode::None | NamedCodeBlockDefinitionNode::Unresolved(_, _) => {}
    }
}

/// Имена локальных объявлений тела.
///
/// Обход **не** исчерпывающий: пропущенная форма даёт лишь риск столкнуться с чужим
/// именем, а не порчу вывода, - и обе стороны здесь безопасны.
pub(crate) fn collect_locals(stmt: &StatementNode, out: &mut HashSet<String>) {
    match stmt {
        StatementNode::Variable(name, _, _, _) => {
            out.insert(name.clone());
        }
        StatementNode::Block(items) => items.iter().for_each(|s| collect_locals(s, out)),
        StatementNode::If { then_, else_, .. } => {
            collect_locals(then_, out);
            if let Some(alt) = else_ {
                collect_locals(alt, out);
            }
        }
        StatementNode::Loop { body, .. } => collect_locals(body, out),
        StatementNode::For { init, body, .. } => {
            if let Some(i) = init {
                collect_locals(i, out);
            }
            collect_locals(body, out);
        }
        StatementNode::Match { arms, .. } => arms.iter().for_each(|a| collect_locals(&a.body, out)),
        _ => {}
    }
}

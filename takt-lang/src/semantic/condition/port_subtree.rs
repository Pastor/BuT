//! Узел-Поддерево составного порта в позиции значения.
//!
//! # Правило
//!
//! Узел собирается из листьев во **временную**:
//!
//! ```text
//! holder := cfg.tail;
//! ⇓
//! var <tmp>: Inner := {cfg_tail_a, cfg_tail_b};
//! holder := <tmp>;
//! ```
//!
//! Приём - тот же, что у подъёма среза и агрегата (0400, 0431, 0493), а вот форма своя
//! и **измерена**: объявление С агрегатом-инициализатором принимают
//! инструменты всех восьми целей, тогда как пара "объявление без значения +
//! присваивания по полям" валидна у семи - у `rust` её отвергает `clippy`
//! (`field_reassign_with_default`) флагами проверки самой цели. Имя временной берётся у
//! общего носителя `semantic::fresh` - второй счётчик разошёлся бы с первым молча.
//!
//! **Поднимать можно не везде, и граница названа.** Временную объявляют внутри тела
//! блока - там есть куда вставить пролог (та же граница, что у 0400). В условии ребра,
//! именованном условии, охранной формуле и условии цикла места под объявление нет:
//! пролог цикла встал бы перед ним, и значение перестало бы обновляться по итерациям -
//! молчаливое расхождение вместо громкого отказа. Такие позиции отвечают `SE-130`,
//! называя обход: перенести значение узла в переменную модели (её присваивание проход
//! уже разворачивает).

use std::cell::RefCell;
use std::rc::Rc;

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ExpressionNode, ModelNode, StatementNode, VariableNode};

use super::port_split::{LeafCells, Step};

/// Префикс имени временной. Обязан быть допустимым идентификатором **целевых** языков -
/// C, IEC, Rust, SystemVerilog.
const PREFIX: &str = "takt_port_";

/// Состояние подъёма: свежие имена и модель-владелец временных.
pub(super) struct Lift<'a> {
    fresh: crate::semantic::fresh::Fresh<'a>,
    owner: Rc<RefCell<ModelNode>>,
}

impl<'a> Lift<'a> {
    pub(super) fn new(
        taken: &'a std::collections::HashSet<String>,
        owner: &Rc<RefCell<ModelNode>>,
    ) -> Self {
        Self {
            fresh: crate::semantic::fresh::Fresh::new(PREFIX, taken),
            owner: Rc::clone(owner),
        }
    }

    /// Владельца временных задаёт та модель, чьи тела сейчас переписываются.
    pub(super) fn set_owner(&mut self, owner: &Rc<RefCell<ModelNode>>) {
        self.owner = Rc::clone(owner);
    }
}

/// Тип узла-Поддерева: путь ведёт к развёрнутому порту, но не к листу.
///
/// `None` - путь адресует лист (его переписывает разворот) либо к развёрнутому порту не
/// ведёт вовсе.
pub(super) fn subtree_type(cells: &LeafCells, name: &str, path: &[Step]) -> Option<TypeNode> {
    let split = cells.get(name)?;
    if split.leaves.iter().any(|leaf| leaf.path.as_slice() == path) {
        return None;
    }
    if !split
        .leaves
        .iter()
        .any(|leaf| leaf.path.starts_with(path) && leaf.path.len() > path.len())
    {
        return None;
    }
    split
        .nodes
        .iter()
        .find(|(node, _)| node.as_slice() == path)
        .map(|(_, ty)| ty.clone())
}

/// Поднимает узел-поддерево во временную, дописывая пролог.
///
/// Возвращает выражение-замену (ссылку на временную).
///
/// Форма - объявление С агрегатом-инициализатором, и она **измерена** (2026-09-02, все
/// восемь целей и их инструменты): `var t: Inner := {a, b};` принимают `cc`, `iec2c`,
/// `rustc`, `clippy`, `verilator` и `yosys`. Пара "объявление без значения +
/// присваивание по полям" тоже валидна у семи, но у `rust` её отвергает `clippy`
/// (`field_reassign_with_default`) - флагами проверки самой цели.
pub(super) fn lift(
    cells: &LeafCells,
    name: &str,
    path: &[Step],
    ty: TypeNode,
    lift: &mut Lift<'_>,
    prelude: &mut Vec<StatementNode>,
    _loc: Location,
) -> ExpressionNode {
    let tmp = lift.fresh.fresh_name();
    let value = match cells.get(name) {
        Some(split) => aggregate_of(split, path),
        None => ExpressionNode::None,
    };
    prelude.push(StatementNode::Variable(
        tmp.clone(),
        ty.clone(),
        Some(Box::new(value)),
        Location::Implicit,
    ));
    ExpressionNode::Variable(Rc::new(RefCell::new(VariableNode::Simple {
        upper: Some(Rc::downgrade(&lift.owner)),
        loc: Location::Implicit,
        name: tmp,
        ty,
        expr: ExpressionNode::None,
    })))
}

/// Собирает значение узла из листьев - агрегатом, вложенным по глубине.
///
/// Порядок элементов значим, и он берётся у порядка листьев: их собирал обход
/// объявления, то есть в порядке полей структуры и индексов массива. Второго знания о
/// порядке заводить нельзя.
fn aggregate_of(split: &super::port_split::Split, path: &[Step]) -> ExpressionNode {
    let mut items: Vec<ExpressionNode> = Vec::new();
    let mut seen: Vec<Step> = Vec::new();
    for leaf in split
        .leaves
        .iter()
        .filter(|leaf| leaf.path.starts_with(path) && leaf.path.len() > path.len())
    {
        let step = leaf.path[path.len()].clone();
        if seen.contains(&step) {
            continue;
        }
        seen.push(step.clone());
        let mut child = path.to_vec();
        child.push(step);
        let value = match split
            .leaves
            .iter()
            .find(|leaf| leaf.path.as_slice() == child.as_slice())
        {
            Some(leaf) => ExpressionNode::Variable(Rc::clone(&leaf.cell)),
            None => aggregate_of(split, &child),
        };
        items.push(value);
    }
    // Вид агрегата - по типу узла: у массива своя форма записи.
    let array = split
        .nodes
        .iter()
        .find(|(node, _)| node.as_slice() == path)
        .is_some_and(|(_, ty)| matches!(ty, TypeNode::Array(..)));
    if array {
        ExpressionNode::Array(items)
    } else {
        ExpressionNode::Initializer(items)
    }
}

/// Отказ `SE-130`: узел-поддерево там, где временную объявить негде.
///
/// Текст называет обход, и обход работает: присваивание узла переменной модели этот же
/// проход разворачивает по листьям.
pub(super) fn refuse(name: &str, path: &[Step], loc: Location) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "Обращение '{}' берёт СОСТАВНУЮ часть порта там, где цель не может \
             объявить временную (условие перехода, именованное условие, охранная \
             формула, условие цикла): порт развёрнут по листьям, и узел собирается \
             из них. Перенесите значение в переменную модели — \
             `var v: …; always {{ v := {}; }}` — и пользуйтесь ей",
            text_of(name, path),
            text_of(name, path)
        ),
    )
    .with_code("SE-130")
}

/// Запись обращения для диагностики: `cfg.tail`, `bus[0]`.
fn text_of(name: &str, path: &[Step]) -> String {
    let mut out = name.to_string();
    for step in path {
        match step {
            Step::Field(field) => out.push_str(&format!(".{field}")),
            Step::Index(index) => out.push_str(&format!("[{index}]")),
        }
    }
    out
}

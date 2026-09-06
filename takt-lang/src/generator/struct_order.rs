//! Порядок объявления структур - общий носитель целей.
//!
//! Структуры хранятся в `BTreeMap`, то есть **по алфавиту**, и печатались в этом же
//! порядке. Но структура, содержащая другую структуру, обязана быть объявлена **после**
//! неё - в C, в IEC 61131-3 и в SystemVerilog одинаково.
//!
//! # Устойчивость
//!
//! Порядок **детерминирован**: обход идёт по алфавитному списку, и зависимость
//! печатается перед зависимым. Цикл (`A` содержит `B`, `B` содержит `A`) невозможен по
//! смыслу - размер такой структуры бесконечен, - но носитель его переживает: участник
//! цикла печатается один раз, порядок остаётся алфавитным. Диагностику цикла заводить
//! не здесь: это вопрос семантики, а не печати.

use crate::semantic::struct_node::StructDefinitionNode;
use crate::semantic::type_node::TypeNode;
use std::collections::{BTreeMap, BTreeSet};

/// Структуры в порядке зависимостей: вложенная раньше вмещающей.
pub(crate) fn sorted(defs: &BTreeMap<String, StructDefinitionNode>) -> Vec<StructDefinitionNode> {
    let mut out: Vec<StructDefinitionNode> = Vec::new();
    let mut done: BTreeSet<String> = BTreeSet::new();
    let mut active: BTreeSet<String> = BTreeSet::new();
    for name in defs.keys() {
        visit(name, defs, &mut done, &mut active, &mut out);
    }
    out
}

/// Обход в глубину: сперва поля-структуры, потом сама структура.
fn visit(
    name: &str,
    defs: &BTreeMap<String, StructDefinitionNode>,
    done: &mut BTreeSet<String>,
    active: &mut BTreeSet<String>,
    out: &mut Vec<StructDefinitionNode>,
) {
    if done.contains(name) || !active.insert(name.to_string()) {
        // `active` держит текущую ветвь обхода: повторный вход в неё - цикл, и второй
        // раз печатать участника нельзя.
        return;
    }
    if let Some(def) = defs.get(name) {
        for (_, ty) in &def.fields {
            if let Some(dep) = struct_name(ty) {
                visit(dep, defs, done, active, out);
            }
        }
        if done.insert(name.to_string()) {
            out.push(def.clone());
        }
    }
    active.remove(name);
}

/// Имя структуры, если тип поля - структура (в том числе массив структур).
fn struct_name(ty: &TypeNode) -> Option<&str> {
    match ty {
        TypeNode::Struct(name) => Some(name),
        TypeNode::Array(_, elem) => struct_name(elem),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::Location;

    fn def(name: &str, fields: Vec<(&str, TypeNode)>) -> StructDefinitionNode {
        StructDefinitionNode {
            name: name.to_string(),
            fields: fields
                .into_iter()
                .map(|(f, t)| (f.to_string(), t))
                .collect(),
            loc: Location::Codegen,
        }
    }

    /// Вложенная структура печатается раньше вмещающей, хотя в алфавите позже.
    #[test]
    fn dependency_comes_first() {
        let mut defs = BTreeMap::new();
        defs.insert(
            "Line".to_string(),
            def(
                "Line",
                vec![
                    ("a", TypeNode::Struct("Point".to_string())),
                    ("b", TypeNode::Struct("Point".to_string())),
                ],
            ),
        );
        defs.insert(
            "Point".to_string(),
            def("Point", vec![("x", TypeNode::Bit), ("y", TypeNode::Bit)]),
        );
        let order: Vec<String> = sorted(&defs).into_iter().map(|d| d.name).collect();
        assert_eq!(order, vec!["Point".to_string(), "Line".to_string()]);
    }

    /// Массив структур - тоже зависимость.
    #[test]
    fn array_of_structs_is_a_dependency() {
        let mut defs = BTreeMap::new();
        defs.insert(
            "Bag".to_string(),
            def(
                "Bag",
                vec![(
                    "items",
                    TypeNode::Array(2, Box::new(TypeNode::Struct("Point".to_string()))),
                )],
            ),
        );
        defs.insert(
            "Point".to_string(),
            def("Point", vec![("x", TypeNode::Bit)]),
        );
        let order: Vec<String> = sorted(&defs).into_iter().map(|d| d.name).collect();
        assert_eq!(order, vec!["Point".to_string(), "Bag".to_string()]);
    }

    /// Цикл не роняет печать и не даёт дублей.
    ///
    /// Такая структура бессмысленна (бесконечный размер), но носитель порядка не место
    /// для этой диагностики - он обязан лишь пережить вход.
    #[test]
    fn cycle_is_survived_without_duplicates() {
        let mut defs = BTreeMap::new();
        defs.insert(
            "A".to_string(),
            def("A", vec![("b", TypeNode::Struct("B".to_string()))]),
        );
        defs.insert(
            "B".to_string(),
            def("B", vec![("a", TypeNode::Struct("A".to_string()))]),
        );
        let order: Vec<String> = sorted(&defs).into_iter().map(|d| d.name).collect();
        assert_eq!(
            order.len(),
            2,
            "каждая структура печатается один раз: {order:?}"
        );
    }
}

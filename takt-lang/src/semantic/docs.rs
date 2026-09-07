//! Привязка `///`-комментариев к семантическому дереву Takt.
//!
//! После построения семантического дерева через [`construct_model`] вызов
//! [`attach_docs`] заполняет поля [`ModelNode::doc`] и [`ModelNode::docs`] в каждом
//! [`ModelNode`] на основе позиций `///`-комментариев в исходном тексте.
//!
//! ## Алгоритм привязки
//!
//! Для каждого именованного объявления (состояния, переменной, функции и т.д.) алгоритм
//! ищет все `///`-комментарии, у которых ближайший **следующий элемент** в исходном
//! тексте - это именно данное объявление. "Ближайший следующий элемент" для комментария
//! C - это элемент с наименьшей стартовой позицией, превышающей конечную позицию C.
//!
//! Такой подход не требует исходного текста для анализа пробелов и корректно
//! обрабатывает как вплотную стоящие, так и разделённые пустыми строками блоки.
//!
//! ## Пример
//!
//! ```text
//! // Документация модели TrafficLight.
//! // Управляет переходом между состояниями.
//! model TrafficLight {
//!     // Счётчик тактов.
//!     var timer: u8 = 0;
//!
//!     // Начальное состояние - красный свет.
//!     start Red { ref Green: timer = 100; }
//!
//!     state Green { next Red; }
//! }
//! ```
//!
//! После вызова [`attach_docs`]:
//! - `root.docs["TrafficLight"]` -> `["Документация модели TrafficLight.", "Управляет переходом между состояниями."]`
//! - `root.models["TrafficLight"].doc` -> те же строки
//! - `root.models["TrafficLight"].docs["timer"]` -> `["Счётчик тактов."]`
//! - `root.models["TrafficLight"].docs["Red"]` -> `["Начальное состояние - красный свет."]`
//! - `root.models["TrafficLight"].docs["Green"]` -> `[]` (нет doc-комментария)
//!
//! [`construct_model`]: crate::semantic::tree::construct_model [`ModelNode::doc`]:
//! crate::semantic::ModelNode::doc [`ModelNode::docs`]:
//! crate::semantic::ModelNode::docs

use crate::diagnostics::Location;
use crate::parser::ast;
use crate::parser::ast::ModelElement;
use crate::semantic::ModelNode;
use crate::semantic::naming::normalize_camelcase_name;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;
// --- Вспомогательные функции -------------------------------------------------

/// Убирает префикс `///` из строки документационного комментария.
///
/// Удаляет ровно три слеша в начале и один пробел после них (если есть).
///
/// # Примеры
///
/// ```text
/// "/// описание функции" -> "описание функции"
/// "///"                  -> ""
/// "///без пробела"       -> "без пробела"
/// ```
fn strip_doc_prefix(text: &str) -> String {
    // Убираем "///" (ровно три символа)
    let stripped = text.strip_prefix("///").unwrap_or(text);
    // Убираем один пробел после "///" (если есть)
    stripped.strip_prefix(' ').unwrap_or(stripped).to_string()
}

/// Возвращает стартовую байтовую позицию элемента модели, если она доступна.
fn element_start(element: &ModelElement) -> Option<usize> {
    match element {
        ModelElement::Variable(v) => Some(variable_loc(v).start()),
        ModelElement::State(s) => Some(s.loc.start()),
        ModelElement::Model(m) => Some(m.loc.start()),
        ModelElement::Function(f) => Some(f.loc.start()),
        ModelElement::Condition(c) => Some(c.loc.start()),
        ModelElement::Invariant(i) => Some(i.loc.start()),
        // Вставка уровня модели: позиция - у самого оператора.
        ModelElement::Assembly(a) => Some(a.loc().start()),
        ModelElement::Type(t) => Some(t.loc.start()),
        ModelElement::Import(imp) => Some(import_loc(imp).start()),
        ModelElement::NamedBlockCode(nb) => Some(nb.loc.start()),
        ModelElement::Formula(f) => Some(f.loc.start()),
        ModelElement::StraySemicolon(loc) => Some(loc.start()),
        ModelElement::Enum(e) => Some(e.loc.start()),
        ModelElement::Struct(s) => Some(s.loc.start()),
        ModelElement::InlineFormula(f) => Some(inline_formula_loc(f).start()),
        ModelElement::Address(a) => Some(a.loc.start()),
        ModelElement::Clock(c) => Some(c.loc.start()),
    }
}

/// Возвращает местоположение встроенной формулы.
fn inline_formula_loc(f: &ast::InlineFormulaDefine) -> Location {
    match f {
        ast::InlineFormulaDefine::Guard { loc, .. } => *loc,
        ast::InlineFormulaDefine::Ltl { loc, .. } => *loc,
    }
}

/// Возвращает имя именованного элемента модели (если есть).
///
/// Для вложенной `model M` имя нормализуется в CamelCase.
fn element_name(element: &ModelElement) -> Option<String> {
    match element {
        ModelElement::Variable(v) => variable_name(v),
        ModelElement::State(s) => s.name.as_ref().map(|id| id.name.clone()),
        ModelElement::Model(m) => m.name.as_ref().map(|id| normalize_camelcase_name(&id.name)),
        ModelElement::Function(f) => f.name.as_ref().map(|id| id.name.clone()),
        ModelElement::Condition(c) => c.name.as_ref().map(|id| id.name.clone()),
        ModelElement::Type(t) => Some(t.name.name.clone()),
        _ => None,
    }
}

/// Возвращает местоположение описания переменной/константы/порта.
fn variable_loc(v: &ast::VariableDefine) -> Location {
    match v {
        ast::VariableDefine::Variable { loc, .. } => *loc,
        ast::VariableDefine::Port { loc, .. } => *loc,
        ast::VariableDefine::Constant { loc, .. } => *loc,
        ast::VariableDefine::Parameter { loc, .. } => *loc,
    }
}

/// Возвращает имя переменной/константы/порта.
fn variable_name(v: &ast::VariableDefine) -> Option<String> {
    match v {
        ast::VariableDefine::Variable { name, .. } => name.as_ref().map(|id| id.name.clone()),
        ast::VariableDefine::Port { name, .. } => name.as_ref().map(|id| id.name.clone()),
        ast::VariableDefine::Constant { name, .. } => name.as_ref().map(|id| id.name.clone()),
        ast::VariableDefine::Parameter { name, .. } => name.as_ref().map(|id| id.name.clone()),
    }
}

/// Возвращает местоположение директивы импорта.
fn import_loc(imp: &ast::ImportDefine) -> Location {
    match imp {
        ast::ImportDefine::Plain(_, loc) => *loc,
        ast::ImportDefine::GlobalSymbol(_, _, loc) => *loc,
        ast::ImportDefine::Rename(_, _, loc) => *loc,
    }
}

/// Собирает `///`-комментарии, ближайшим следующим элементом для которых является
/// элемент с позицией `elem_start`.
///
/// "Ближайший следующий элемент" для комментария с концом `c_end` - это элемент с
/// наименьшей стартовой позицией из тех, что превышают `c_end`.
///
/// # Параметры
///
/// - `elem_start` - позиция целевого элемента.
/// - `all_element_starts` - отсортированный список позиций всех элементов.
/// - `doc_comments` - список `(start, end, очищенный_текст)` для DocLine-комментариев.
fn collect_docs_for(
    elem_start: usize,
    all_element_starts: &[usize],
    doc_comments: &[(usize, usize, String)],
) -> Vec<String> {
    doc_comments
        .iter()
        .filter(|(_, c_end, _)| {
            *c_end <= elem_start
                && all_element_starts
                    .iter()
                    .filter(|&&s| s > *c_end)
                    .min()
                    .copied()
                    == Some(elem_start)
        })
        .map(|(_, _, text)| text.clone())
        .collect()
}

// --- Публичный API ------------------------------------------------------------

/// Привязывает `///`-комментарии к узлам семантического дерева.
///
/// Для каждого именованного элемента в `ast_model.elements` ищет `///`-комментарии,
/// непосредственно предшествующие ему (по позиции в тексте), и помещает их в
/// `model_node.docs[имя_элемента]`.
///
/// Если элемент является вложенной моделью, её поле [`ModelNode::doc`] также
/// заполняется теми же строками (дублирование для удобного доступа).
///
/// Функция рекурсивно обходит все вложенные модели в семантическом дереве.
///
/// # Параметры
///
/// - `model_node` - семантический узел, чьи документационные поля заполняются.
/// - `ast_model` - соответствующий узел AST.
/// - `all_comments` - все комментарии файла, возвращённые [`parse`](crate::parse).
///
/// # Пример
///
/// ```rust,ignore
/// let (ast, comments) = parse(src, 0)?;
/// let root = construct_model(&ast, None, &[])?;
/// attach_docs(&root, &ast, &comments);
/// // Теперь:
/// // root.borrow().docs.get("TrafficLight") -> Some([...])
/// // root.borrow().models["TrafficLight"].borrow().doc -> [...]
/// ```
pub(crate) fn attach_docs(
    model_node: &Rc<RefCell<ModelNode>>,
    ast_model: &ast::Model,
    all_comments: &[ast::Comment],
) {
    // Собираем DocLine-комментарии: (start, end, очищенный_текст)
    let mut doc_lines: Vec<(usize, usize, String)> = all_comments
        .iter()
        .filter_map(|c| {
            if let ast::Comment::DocLine(loc, text) = c
                // Location::Builtin не имеет числовых позиций - пропускаем
                && let Location::Source(_, start, end) = loc
            {
                return Some((*start as usize, *end as usize, strip_doc_prefix(text)));
            }
            None
        })
        .collect();
    // Сортируем по позиции (обычно уже упорядочены, но явная сортировка гарантирует)
    doc_lines.sort_by_key(|(start, _, _)| *start);

    // Все стартовые позиции элементов - нужны алгоритму "ближайший следующий"
    let all_starts: Vec<usize> = ast_model
        .elements
        .iter()
        .filter_map(element_start)
        .collect();

    // Карта: имя -> doc-строки
    let mut docs_map: BTreeMap<String, Vec<String>> = BTreeMap::new();
    // Документация вложенных моделей (для заполнения.doc дочернего узла)
    let mut nested_model_docs: Vec<(String, Vec<String>)> = Vec::new();

    for element in &ast_model.elements {
        let (Some(e_start), Some(name)) = (element_start(element), element_name(element)) else {
            continue;
        };

        let e_docs = collect_docs_for(e_start, &all_starts, &doc_lines);
        if e_docs.is_empty() {
            continue;
        }

        docs_map.insert(name.clone(), e_docs.clone());

        // Для вложенных моделей запоминаем, чтобы установить.doc дочернего узла
        if matches!(element, ModelElement::Model(_)) {
            nested_model_docs.push((name, e_docs));
        }
    }

    // Записываем docs в семантический узел
    model_node.borrow_mut().docs = docs_map;

    // Устанавливаем.doc у дочерних семантических моделей
    {
        let mn = model_node.borrow();
        for (name, docs) in &nested_model_docs {
            if let Some(nested) = mn.models.get(name) {
                nested.borrow_mut().doc = docs.clone();
            }
        }
    }

    // Карта: нормализованное имя -> &ast::Model для вложенных моделей
    let ast_nested: HashMap<String, &ast::Model> = ast_model
        .elements
        .iter()
        .filter_map(|e| {
            if let ModelElement::Model(m) = e {
                m.name
                    .as_ref()
                    .map(|id| (normalize_camelcase_name(&id.name), m.as_ref()))
            } else {
                None
            }
        })
        .collect();

    // Рекурсивный спуск в вложенные модели (собираем заранее, чтобы не держать заём)
    let nested_semantic: Vec<(String, Rc<RefCell<ModelNode>>)> = model_node
        .borrow()
        .models
        .iter()
        .map(|(k, v)| (k.clone(), Rc::clone(v)))
        .collect();

    for (name, nested_node) in nested_semantic {
        if let Some(ast_m) = ast_nested.get(&name) {
            attach_docs(&nested_node, ast_m, all_comments);
        }
    }
}

// --- Тесты -------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- strip_doc_prefix -------------------------------------------------

    /// Стандартный формат с пробелом: "/// текст" -> "текст".
    #[test]
    fn strip_with_space() {
        assert_eq!(strip_doc_prefix("/// описание"), "описание");
    }

    /// Без пробела после "///": "///текст" -> "текст".
    #[test]
    fn strip_without_space() {
        assert_eq!(strip_doc_prefix("///текст"), "текст");
    }

    /// Только "///": "///" -> "".
    #[test]
    fn strip_empty_doc() {
        assert_eq!(strip_doc_prefix("///"), "");
    }

    /// Строка без префикса остаётся неизменной.
    #[test]
    fn strip_no_prefix() {
        assert_eq!(strip_doc_prefix("обычный текст"), "обычный текст");
    }

    // --- collect_docs_for -------------------------------------------------

    /// Единственный doc-комментарий перед единственным элементом.
    #[test]
    fn single_doc_before_element() {
        let all_starts = vec![10usize];
        let docs = vec![(0usize, 8usize, "Документация".to_string())];
        let result = collect_docs_for(10, &all_starts, &docs);
        assert_eq!(result, vec!["Документация"]);
    }

    /// Doc-комментарий перед первым элементом не достаётся второму.
    #[test]
    fn doc_before_first_not_second() {
        let all_starts = vec![10usize, 50usize];
        let docs = vec![(0usize, 8usize, "Первая".to_string())];
        let result_first = collect_docs_for(10, &all_starts, &docs);
        let result_second = collect_docs_for(50, &all_starts, &docs);
        assert_eq!(result_first, vec!["Первая"]);
        assert!(result_second.is_empty());
    }

    /// Два doc-комментария подряд - оба достаются одному элементу.
    #[test]
    fn two_docs_before_same_element() {
        let all_starts = vec![30usize];
        let docs = vec![
            (0usize, 12usize, "Строка 1".to_string()),
            (13usize, 25usize, "Строка 2".to_string()),
        ];
        let result = collect_docs_for(30, &all_starts, &docs);
        assert_eq!(result, vec!["Строка 1", "Строка 2"]);
    }

    /// Doc-комментарий между двумя элементами достаётся второму (не первому).
    #[test]
    fn doc_between_elements_goes_to_second() {
        let all_starts = vec![10usize, 50usize];
        // Комментарий после первого элемента (позиция 20-28)
        let docs = vec![(20usize, 28usize, "Для второго".to_string())];
        let result_first = collect_docs_for(10, &all_starts, &docs);
        let result_second = collect_docs_for(50, &all_starts, &docs);
        assert!(result_first.is_empty());
        assert_eq!(result_second, vec!["Для второго"]);
    }

    /// Doc-комментарий после последнего элемента не достаётся никому.
    #[test]
    fn doc_after_last_element_unassigned() {
        let all_starts = vec![10usize];
        let docs = vec![(20usize, 28usize, "Висячий".to_string())];
        let result = collect_docs_for(10, &all_starts, &docs);
        assert!(result.is_empty());
    }
}

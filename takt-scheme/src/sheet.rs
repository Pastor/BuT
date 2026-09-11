//! Листы схемы по графу модели и файлу раскладки.
//!
//! Листы те же, что открывает холст страницы: лист каждой модели и лист каждого
//! составного состояния с цепочкой, параллелью либо скобками. Узлы и рёбра
//! собираются по правилам холста (`modelSheet`, `compositionSheet`): порядок узлов
//! - ярус, затем место в ярусе; знак состояния `S` с номером, знак условия `K`.
//!
//! Автораскладки здесь нет: узел без записи в файле - не координата, а причина
//! отказа, и отказ называет все такие узлы разом. Частичная картинка молча прошла
//! бы за полную.

use std::collections::BTreeMap;

use takt_lang::layout::{EdgeKind, Graph, Implement, NodeKind};

use crate::geometry::{
    Frame, LabelPlace, Point, Shape, Size, Tree, compose, frames_of, sheet_size,
};
use crate::layout::{Layout, composition_key, edge_key};

/// Узел листа.
#[derive(Debug, Clone, PartialEq)]
pub struct DrawNode {
    pub name: String,
    pub kind: NodeKind,
    pub start: bool,
    pub x: f64,
    pub y: f64,
    /// Подпись автора; пусто - нет.
    pub alias: String,
    /// Знак: `S` и номер.
    pub mark: String,
    /// Модель шага листа композиции - ею шаг и называется в легенде.
    pub model: Option<String>,
}

impl DrawNode {
    /// Форма узла для геометрии.
    pub fn shape(&self) -> Shape {
        Shape {
            name: self.name.clone(),
            x: self.x,
            y: self.y,
            square: self.kind == NodeKind::Composition,
        }
    }
}

/// Ребро листа.
#[derive(Debug, Clone, PartialEq)]
pub struct DrawEdge {
    pub key: String,
    pub from: String,
    pub to: String,
    pub kind: EdgeKind,
    /// Цитата условия; `None` - безусловный переход.
    pub cond: Option<String>,
    /// Подпись автора у условия.
    pub alias: String,
    pub points: Vec<Point>,
    pub end_from: Option<u32>,
    pub end_to: Option<u32>,
    /// Своё место знака; `None` - умолчание файла.
    pub label: Option<LabelPlace>,
    /// Знак: `K` и номер; пусто у безусловного.
    pub mark: String,
}

/// Лист схемы.
#[derive(Debug, Clone, PartialEq)]
pub struct DrawSheet {
    /// Ключ записи в файле раскладки.
    pub key: String,
    /// Заголовок: имя модели либо составного состояния.
    pub title: String,
    /// Лист композиции (путь модели у него не хранится).
    pub composition: bool,
    pub nodes: Vec<DrawNode>,
    pub edges: Vec<DrawEdge>,
    pub frames: Vec<Frame>,
    pub size: Size,
}

/// Отказ: узлы, которых нет в файле раскладки, по листам.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unplaced(pub Vec<(String, Vec<String>)>);

impl std::fmt::Display for Unplaced {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let parts: Vec<String> = self
            .0
            .iter()
            .map(|(sheet, names)| format!("{} — {}", shown(sheet), names.join(", ")))
            .collect();
        write!(
            f,
            "в файле раскладки не размещены узлы: {}",
            parts.join("; ")
        )
    }
}

/// Имя листа для человека: корень - «корень», лист композиции - через `/`.
fn shown(key: &str) -> String {
    let (owner, state) = key
        .split_once('#')
        .map_or((key, None), |(o, s)| (o, Some(s)));
    let path = if owner == "/" { "" } else { owner };
    match (path, state) {
        ("", None) => "корень".to_string(),
        ("", Some(state)) => state.to_string(),
        (path, Some(state)) => format!("{path}/{state}"),
        (path, None) => path.to_string(),
    }
}

/// Все листы модели; узлы без записи в файле - отказ со списком.
///
/// # Ошибки
/// Хотя бы один узел хотя бы одного листа не размещён.
pub fn sheets(graph: &Graph, layout: &Layout) -> Result<Vec<DrawSheet>, Unplaced> {
    let mut out = Vec::new();
    let mut missing = Vec::new();
    let mut check = |sheet: Result<DrawSheet, (String, Vec<String>)>| match sheet {
        Ok(sheet) => out.push(sheet),
        Err(miss) => missing.push(miss),
    };
    for found in &graph.sheets {
        check(model_sheet(found, layout));
        for node in &found.nodes {
            if let Some(implement) = &node.implements
                && !matches!(implement, Implement::Model { .. })
            {
                check(composition_sheet(
                    &found.path,
                    &node.name,
                    implement,
                    layout,
                ));
            }
        }
    }
    if missing.is_empty() {
        Ok(out)
    } else {
        Err(Unplaced(missing))
    }
}

fn model_sheet(
    found: &takt_lang::layout::Sheet,
    layout: &Layout,
) -> Result<DrawSheet, (String, Vec<String>)> {
    let key = found.path.clone();
    let stored = layout.sheets.get(&key).cloned().unwrap_or_default();
    let mut ordered: Vec<&takt_lang::layout::Node> = found.nodes.iter().collect();
    ordered.sort_by_key(|n| (n.rank, n.order));
    let missing: Vec<String> = ordered
        .iter()
        .filter(|n| !stored.nodes.contains_key(&n.name))
        .map(|n| n.name.clone())
        .collect();
    if !missing.is_empty() {
        return Err((key, missing));
    }
    let nodes: Vec<DrawNode> = ordered
        .iter()
        .enumerate()
        .map(|(i, n)| {
            let [x, y] = stored.nodes[&n.name];
            DrawNode {
                name: n.name.clone(),
                kind: n.kind,
                start: n.start,
                x,
                y,
                alias: stored.names.get(&n.name).cloned().unwrap_or_default(),
                mark: format!("S{}", i + 1),
                model: None,
            }
        })
        .collect();
    let mut k = 0;
    let edges: Vec<DrawEdge> = found
        .edges
        .iter()
        .map(|e| {
            let key = edge_key(&e.from, &e.to, e.ordinal);
            let record = stored.edges.get(&key).cloned().unwrap_or_default();
            let mark = if e.condition.is_some() {
                k += 1;
                format!("K{k}")
            } else {
                String::new()
            };
            DrawEdge {
                key,
                from: e.from.clone(),
                to: e.to.clone(),
                kind: e.kind,
                cond: e.condition.clone(),
                alias: record.name.unwrap_or_default(),
                points: record.points,
                end_from: record.end_from,
                end_to: record.end_to,
                label: record.label,
                mark,
            }
        })
        .collect();
    // Своё место знака - тоже часть рисунка: лист обязан его вместить, как излом.
    let mut extra: Vec<Point> = edges
        .iter()
        .flat_map(|e| e.points.iter().copied())
        .collect();
    extra.extend(edges.iter().filter_map(|e| match e.label {
        Some(LabelPlace::Own(x, y)) => Some([x, y]),
        _ => None,
    }));
    let shapes: Vec<Shape> = nodes.iter().map(DrawNode::shape).collect();
    let size = sheet_size(&shapes, &extra);
    let title = if found.path == "/" {
        String::new()
    } else {
        found.name.clone()
    };
    Ok(DrawSheet {
        key,
        title,
        composition: false,
        nodes,
        edges,
        frames: Vec::new(),
        size,
    })
}

/// Дерево реализации в форме геометрии.
pub fn tree_of(implement: &Implement) -> Tree<'_> {
    match implement {
        Implement::Model { name, .. } => Tree::Model(name),
        Implement::Chain(items) => Tree::Chain(items.iter().map(tree_of).collect()),
        Implement::Parallel(items) => Tree::Parallel(items.iter().map(tree_of).collect()),
        Implement::Group(inner) => Tree::Group(Box::new(tree_of(inner))),
    }
}

fn composition_sheet(
    path: &str,
    state: &str,
    implement: &Implement,
    layout: &Layout,
) -> Result<DrawSheet, (String, Vec<String>)> {
    let key = composition_key(path, state);
    let stored = layout.sheets.get(&key).cloned().unwrap_or_default();
    let tree = tree_of(implement);
    let (steps, links) = compose(&tree);
    let missing: Vec<String> = steps
        .iter()
        .filter(|(name, _)| !stored.nodes.contains_key(name))
        .map(|(name, _)| name.clone())
        .collect();
    if !missing.is_empty() {
        return Err((key, missing));
    }
    let nodes: Vec<DrawNode> = steps
        .iter()
        .enumerate()
        .map(|(i, (name, model))| {
            let [x, y] = stored.nodes[name];
            DrawNode {
                name: name.clone(),
                kind: NodeKind::Composition,
                start: false,
                x,
                y,
                alias: stored.names.get(name).cloned().unwrap_or_default(),
                mark: format!("S{}", i + 1),
                model: Some(model.clone()),
            }
        })
        .collect();
    let edges: Vec<DrawEdge> = links
        .iter()
        .map(|(from, to, ordinal)| {
            let key = edge_key(from, to, *ordinal);
            let record = stored.edges.get(&key).cloned().unwrap_or_default();
            DrawEdge {
                key,
                from: from.clone(),
                to: to.clone(),
                kind: EdgeKind::Next,
                cond: None,
                alias: String::new(),
                points: record.points,
                end_from: record.end_from,
                end_to: record.end_to,
                label: None,
                mark: String::new(),
            }
        })
        .collect();
    let at: BTreeMap<String, Point> = nodes.iter().map(|n| (n.name.clone(), [n.x, n.y])).collect();
    let frames = frames_of(&tree, &at);
    let mut extra: Vec<Point> = edges
        .iter()
        .flat_map(|e| e.points.iter().copied())
        .collect();
    extra.extend(frames.iter().flat_map(Frame::corners));
    let shapes: Vec<Shape> = nodes.iter().map(DrawNode::shape).collect();
    let size = sheet_size(&shapes, &extra);
    Ok(DrawSheet {
        key,
        title: state.to_string(),
        composition: true,
        nodes,
        edges,
        frames,
        size,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::layout::parse;

    const SOURCE: &str = "model E { start A { ref B: go; } state B; }\nstart Main = E + (E | E) { next Done; }\nstate Done;\n";

    #[test]
    fn every_sheet_needs_every_node_in_the_file() {
        let graph = takt_lang::layout::graph_of(SOURCE).expect("граф");
        let refused = sheets(&graph, &Layout::default()).expect_err("пустой файл");
        let text = refused.to_string();
        assert!(text.contains("корень — Main, Done"), "{text}");
        assert!(text.contains("E — A, B"), "{text}");
        assert!(
            text.contains("Main — E#1, E#2, E#3"),
            "лист композиции тоже: {text}"
        );
    }

    #[test]
    fn a_complete_file_gives_all_sheets() {
        let graph = takt_lang::layout::graph_of(SOURCE).expect("граф");
        let layout = parse(
            r#"{"format": 1, "sheets": {
                "/": {"nodes": {"Main": {"x": 72, "y": 72}, "Done": {"x": 72, "y": 312}}},
                "E": {"nodes": {"A": {"x": 72, "y": 72}, "B": {"x": 72, "y": 192}}, "names": {"A": "Ожидание"}},
                "/#Main": {"nodes": {"E#1": {"x": 144, "y": 168}, "E#2": {"x": 360, "y": 96}, "E#3": {"x": 360, "y": 264}}}
            }}"#,
        )
        .expect("файл");
        let all = sheets(&graph, &layout).expect("листы");
        let keys: Vec<&str> = all.iter().map(|s| s.key.as_str()).collect();
        assert_eq!(keys, ["/", "/#Main", "E"]);
        let e = &all[2];
        assert_eq!(e.nodes[0].alias, "Ожидание");
        assert_eq!(e.edges[0].mark, "K1");
        let main = &all[1];
        assert!(main.composition);
        assert_eq!(main.frames.len(), 2, "скобки и параллель");
        assert_eq!(main.edges.len(), 2);
    }
}

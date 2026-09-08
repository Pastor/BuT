//! Граф модели для схемы: ответ `takt_lang::layout` в форме страницы.
//!
//! Позиции переводятся в строки и колонки протокола LSP, как у прочих операций
//! редактора: страница ставит курсор и подсвечивает диапазоны одним и тем же способом.
//! Ничего своего здесь нет - ни ярусов, ни разбора: считает библиотека.

use serde::Serialize;
use takt_lang::diagnostics::Location;
use takt_lang::layout::{self, Edge, Implement, Node, Sheet};
use takt_lang::lsp::offset_to_range;

use crate::editor::RangeJson;
use crate::reply;

/// Лист графа.
#[derive(Debug, Serialize)]
struct SheetJson {
    path: String,
    name: String,
    range: Option<RangeJson>,
    name_range: Option<RangeJson>,
    start: Option<String>,
    implements: Option<ImplementJson>,
    nodes: Vec<NodeJson>,
    edges: Vec<EdgeJson>,
}

/// Узел листа.
#[derive(Debug, Serialize)]
struct NodeJson {
    name: String,
    kind: &'static str,
    start: bool,
    range: Option<RangeJson>,
    name_range: Option<RangeJson>,
    implements: Option<ImplementJson>,
    rank: u32,
    order: u32,
}

/// Ребро листа.
#[derive(Debug, Serialize)]
struct EdgeJson {
    from: String,
    to: String,
    ordinal: u32,
    kind: &'static str,
    condition: Option<String>,
    range: Option<RangeJson>,
}

/// Дерево реализации: `{"model": {...}}`, `{"chain": [...]}`, `{"parallel": [...]}`,
/// `{"group": {...}}`.
#[derive(Debug, Serialize)]
#[serde(rename_all = "lowercase")]
enum ImplementJson {
    Model {
        name: String,
        path: Option<String>,
        range: Option<RangeJson>,
    },
    Chain(Vec<ImplementJson>),
    Parallel(Vec<ImplementJson>),
    Group(Box<ImplementJson>),
}

/// Граф модели по тексту.
///
/// Неразбираемый текст - отказ с диагностикой разбора, как у компиляции: страница
/// показывает пустой лист и диагностику, а не падает.
pub fn graph(source: &str) -> String {
    #[derive(Serialize)]
    struct Reply {
        sheets: Vec<SheetJson>,
    }
    match layout::graph_of(source) {
        Ok(graph) => reply::ok(Reply {
            sheets: graph
                .sheets
                .into_iter()
                .map(|sheet| sheet_json(sheet, source))
                .collect(),
        }),
        Err(diagnostic) => reply::failed(&diagnostic, source),
    }
}

fn sheet_json(sheet: Sheet, source: &str) -> SheetJson {
    SheetJson {
        path: sheet.path,
        name: sheet.name,
        range: range_of(sheet.loc, source),
        name_range: sheet.name_loc.and_then(|loc| range_of(loc, source)),
        start: sheet.start,
        implements: sheet.implements.map(|i| implement_json(i, source)),
        nodes: sheet
            .nodes
            .into_iter()
            .map(|node| node_json(node, source))
            .collect(),
        edges: sheet
            .edges
            .into_iter()
            .map(|edge| edge_json(edge, source))
            .collect(),
    }
}

fn node_json(node: Node, source: &str) -> NodeJson {
    NodeJson {
        name: node.name,
        kind: node.kind.as_str(),
        start: node.start,
        range: range_of(node.loc, source),
        name_range: range_of(node.name_loc, source),
        implements: node.implements.map(|i| implement_json(i, source)),
        rank: node.rank,
        order: node.order,
    }
}

fn edge_json(edge: Edge, source: &str) -> EdgeJson {
    EdgeJson {
        from: edge.from,
        to: edge.to,
        ordinal: edge.ordinal,
        kind: edge.kind.as_str(),
        condition: edge.condition,
        range: range_of(edge.loc, source),
    }
}

fn implement_json(implement: Implement, source: &str) -> ImplementJson {
    match implement {
        Implement::Model { name, path, loc } => ImplementJson::Model {
            name,
            path,
            range: range_of(loc, source),
        },
        Implement::Chain(items) => ImplementJson::Chain(
            items
                .into_iter()
                .map(|i| implement_json(i, source))
                .collect(),
        ),
        Implement::Parallel(items) => ImplementJson::Parallel(
            items
                .into_iter()
                .map(|i| implement_json(i, source))
                .collect(),
        ),
        Implement::Group(inner) => ImplementJson::Group(Box::new(implement_json(*inner, source))),
    }
}

/// Диапазон позиции в исходнике; у позиции вне файла диапазона нет.
fn range_of(loc: Location, source: &str) -> Option<RangeJson> {
    match loc {
        Location::Source(_, start, end) => Some(RangeJson::of(offset_to_range(
            source,
            start as usize,
            end as usize,
        ))),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    const ELEVATOR: &str = include_str!("../../examples/elevator.takt");

    fn json(text: &str) -> Value {
        serde_json::from_str(text).expect("ответ моста - JSON")
    }

    /// Лифт: два листа, у `Engine` пять узлов и семь рёбер, позиции - диапазонами.
    #[test]
    fn elevator_graph_has_sheets_nodes_and_edges() {
        let reply = json(&graph(ELEVATOR));
        assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
        let sheets = reply["sheets"].as_array().unwrap();
        assert_eq!(sheets.len(), 2);
        assert_eq!(sheets[0]["path"], "/");
        assert_eq!(sheets[1]["path"], "Engine");
        let engine = &sheets[1];
        assert_eq!(engine["nodes"].as_array().unwrap().len(), 5);
        assert_eq!(engine["edges"].as_array().unwrap().len(), 7);
        assert_eq!(engine["start"], "Idle");
        let idle = &engine["nodes"][0];
        assert_eq!(idle["name"], "Idle");
        assert_eq!(idle["kind"], "start");
        assert_eq!(idle["rank"], 0);
        assert!(idle["name_range"]["start_line"].is_number(), "{idle}");
        let middle = sheets[0]["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|n| n["name"] == "Middle")
            .unwrap();
        assert_eq!(middle["kind"], "composition");
        assert_eq!(middle["implements"]["chain"].as_array().unwrap().len(), 5);
        assert_eq!(
            middle["implements"]["chain"][1]["group"]["parallel"]
                .as_array()
                .unwrap()
                .len(),
            2
        );
        assert_eq!(middle["implements"]["chain"][0]["model"]["path"], "Engine");
    }

    /// Неразбираемый текст - отказ с диагностикой, не паника; повтор даёт тот же ответ.
    #[test]
    fn broken_source_is_a_diagnostic_and_answers_repeat() {
        let reply = json(&graph("start S {"));
        assert_eq!(reply["ok"], Value::Bool(false), "{reply}");
        assert!(reply["error"]["code"].is_string(), "{reply}");
        assert_eq!(graph(ELEVATOR), graph(ELEVATOR), "ответ детерминирован");
    }
}

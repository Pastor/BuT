//! Форма графа для потребителя вне компилятора: страница и сервер.
//!
//! # Форма общая, потому что потребителей двое
//!
//! Граф рисуют двое - страница онлайн-редактора и панель плагина, - а считает
//! его один [`super::graph_of`]. Форма ответа при этом обязана быть общей: заведи
//! её каждый потребитель у себя, и они разойдутся молча, а панель нарисует не
//! тот автомат, что страница.
//!
//! # Позиции
//!
//! Позиции переводятся в строки и колонки протокола LSP, как у прочих операций
//! редактора: потребитель ставит курсор и подсвечивает диапазоны одним способом.

use serde::Serialize;

use super::{Edge, Graph, Implement, Node, Sheet};
use crate::diagnostics::Location;
use crate::lsp::offset_to_range;

/// Диапазон в форме потребителя: строки и колонки с нуля, как в LSP.
#[derive(Debug, Serialize)]
pub struct RangeJson {
    /// Строка начала, с нуля.
    pub start_line: u32,
    /// Колонка начала в единицах протокола, с нуля.
    pub start_character: u32,
    /// Строка конца.
    pub end_line: u32,
    /// Колонка конца.
    pub end_character: u32,
}

impl RangeJson {
    /// Диапазон протокола LSP в форме потребителя.
    pub fn of(range: crate::lsp::lsp_types::Range) -> Self {
        Self {
            start_line: range.start.line,
            start_character: range.start.character,
            end_line: range.end.line,
            end_character: range.end.character,
        }
    }
}

/// Граф модели: листы в порядке объявления, корень первым.
#[derive(Debug, Serialize)]
pub struct GraphJson {
    /// Листы: корень первым, далее вложенные модели в порядке объявления.
    pub sheets: Vec<SheetJson>,
}

/// Лист графа.
#[derive(Debug, Serialize)]
pub struct SheetJson {
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
pub struct NodeJson {
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
pub struct EdgeJson {
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
pub enum ImplementJson {
    /// Ссылка на модель.
    Model {
        /// Имя модели в тексте.
        name: String,
        /// Путь листа, если модель объявлена в этом же файле.
        path: Option<String>,
        /// Позиция ссылки.
        range: Option<RangeJson>,
    },
    /// Последовательная композиция.
    Chain(Vec<ImplementJson>),
    /// Параллельная композиция.
    Parallel(Vec<ImplementJson>),
    /// Скобки: форма записи автора сохраняется.
    Group(Box<ImplementJson>),
}

/// Граф в форме потребителя.
pub fn graph_json(graph: Graph, source: &str) -> GraphJson {
    GraphJson {
        sheets: graph
            .sheets
            .into_iter()
            .map(|sheet| sheet_json(sheet, source))
            .collect(),
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

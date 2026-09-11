//! Рисунок листа в числах: трассы рёбер, мостики, совпадения, пути SVG, места
//! знаков, стрелка входа.
//!
//! Это то, что холст страницы считает в `draw()` перед тем, как класть элементы:
//! рёбра трассируются разом, затем обходятся в порядке листа, и каждое спрашивает
//! пересечения и совпадения только с уже нарисованными. Чертёж берёт отсюда
//! геометрию, а сверка паритета сравнивает эти числа с числами холста.

use std::collections::BTreeMap;

use serde::Serialize;
use takt_lang::layout::NodeKind;

use crate::geometry::{
    Corners, EdgeInput, Overlap, Point, Shape, build_path, crossings, dash_for, mark_spot,
    overlap_with, port_point, route_sheet,
};
use crate::layout::Layout;
use crate::sheet::DrawSheet;

/// Ребро в числах.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct DrawnEdge {
    pub key: String,
    /// Ломаная после раздачи точек привязки.
    pub points: Vec<Point>,
    /// Пересечения с уже нарисованными рёбрами.
    pub hops: Vec<Point>,
    /// Путь SVG.
    pub d: String,
    /// Штрих с пропусками на участках, совпадающих с нарисованными; `None` - их нет.
    pub dash: Option<String>,
    /// Ребро приходит в конец нарисованного тем же ходом.
    pub ending: bool,
    /// Место знака условия; `None` - переход безусловный.
    pub mark: Option<Point>,
}

/// Узел в числах: центр и точка стрелки начального состояния.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct DrawnNode {
    pub name: String,
    pub x: f64,
    pub y: f64,
    pub entry: Option<Point>,
}

/// Лист в числах.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Drawn {
    pub key: String,
    pub ox: f64,
    pub oy: f64,
    pub w: f64,
    pub h: f64,
    pub nodes: Vec<DrawnNode>,
    pub frames: Vec<[f64; 4]>,
    pub edges: Vec<DrawnEdge>,
}

/// Считает рисунок листа.
pub fn drawn(sheet: &DrawSheet, layout: &Layout) -> Drawn {
    let shapes: Vec<Shape> = sheet.nodes.iter().map(|n| n.shape()).collect();
    let by_name: BTreeMap<&str, &Shape> = shapes.iter().map(|s| (s.name.as_str(), s)).collect();
    let entry = layout.entry_of(&sheet.key);
    // Стрелка начального состояния занимает свою точку привязки: рёбра встают в
    // соседние, а не под неё.
    let reserved: Vec<(String, u32)> = sheet
        .nodes
        .iter()
        .filter(|n| n.start && n.kind != NodeKind::Composition)
        .map(|n| (n.name.clone(), entry))
        .collect();
    let inputs: Vec<EdgeInput<'_>> = sheet
        .edges
        .iter()
        .map(|e| EdgeInput {
            from: &e.from,
            to: &e.to,
            points: &e.points,
            end_from: e.end_from,
            end_to: e.end_to,
        })
        .collect();
    let routes = route_sheet(&by_name, &inputs, &reserved);
    let corners = layout.corners;
    let crossing = layout.crossing();
    let mut drawn: Vec<Vec<Point>> = Vec::new();
    let mut edges = Vec::new();
    for (edge, route) in sheet.edges.iter().zip(routes) {
        let Some(pts) = route else {
            continue;
        };
        let bezier = corners == Corners::Bezier;
        let hops = if bezier {
            Vec::new()
        } else {
            crossings(&pts, &drawn)
        };
        let covered: Option<Overlap> = (!bezier).then(|| overlap_with(&pts, &drawn));
        drawn.push(pts.clone());
        let auto = edge.points.is_empty();
        let d = build_path(&pts, &hops, corners, crossing, auto);
        let mark = edge.cond.as_ref().map(|_| {
            mark_spot(
                edge.label.unwrap_or(layout.label_place),
                &pts,
                corners,
                auto,
            )
        });
        edges.push(DrawnEdge {
            key: edge.key.clone(),
            dash: covered
                .as_ref()
                .filter(|c| !c.runs.is_empty())
                .map(|c| dash_for(c.total, &c.runs)),
            ending: covered.is_some_and(|c| c.ending),
            points: pts,
            hops,
            d,
            mark,
        });
    }
    let nodes = sheet
        .nodes
        .iter()
        .zip(&shapes)
        .map(|(n, shape)| DrawnNode {
            name: n.name.clone(),
            x: n.x,
            y: n.y,
            entry: (n.start && n.kind != NodeKind::Composition).then(|| port_point(shape, entry)),
        })
        .collect();
    Drawn {
        key: sheet.key.clone(),
        ox: sheet.size.ox,
        oy: sheet.size.oy,
        w: sheet.size.w,
        h: sheet.size.h,
        nodes,
        frames: sheet.frames.iter().map(|f| [f.x, f.y, f.w, f.h]).collect(),
        edges,
    }
}

/// Рисунок всех листов модели в JSON: ответ сверки паритета с холстом.
///
/// # Ошибки
/// Текст модели не разбирается, файл раскладки не читается либо неполон.
pub fn geometry_json(source: &str, layout_text: &str) -> Result<String, String> {
    let graph = takt_lang::layout::graph_of(source).map_err(|d| d.message.clone())?;
    let layout = crate::layout::parse(layout_text).map_err(|e| e.0)?;
    let sheets = crate::sheet::sheets(&graph, &layout).map_err(|e| e.to_string())?;
    let all: Vec<Drawn> = sheets.iter().map(|s| drawn(s, &layout)).collect();
    serde_json::to_string(&all).map_err(|e| e.to_string())
}

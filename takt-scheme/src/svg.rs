//! Чертёж листа в SVG: чертёжный и цветной виды, легенда, строка трассы кадра.
//!
//! Рисунок тот же, что у холста страницы, но без вида холста: ни сетки, ни
//! выделения, ни ореолов наведения, ни кнопок и мишеней. Свойства пишутся
//! атрибутами, а не классами: одна картинка у растеризатора и у браузера, и
//! правила каскада не решают за рисунок. Шрифт едет с картинкой в `@font-face`,
//! и только те гарнитуры, которыми набран лист.
//!
//! Запись детерминирована: тот же лист, раскладка и такт дают тот же текст байт в
//! байт - на этом стоит равенство экспорта со страницы и из командной строки.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;

use takt_lang::layout::{EdgeKind, Implement};

use crate::drawn::{Drawn, drawn};
use crate::fonts::Face;
use crate::geometry::{R, SIDE, route};
use crate::js::num;
use crate::layout::{Layout, LegendPlace, composition_key};
use crate::run::{Lit, Place, Tick, inner_label, sheet_run, steps_label, steps_of};
use crate::sheet::{DrawNode, DrawSheet};
use crate::style::{INDEX_EM, Inks, Levels, Palette, View, arrow_shape, text_px};

/// Что рисовать.
#[derive(Debug, Clone)]
pub struct Options {
    pub view: View,
    /// Легенда: таблицы знаков состояний и условий с подписями автора.
    pub legend: bool,
    /// Такт прогона для цветного вида; `None` - без подсветки.
    pub tick: Option<Tick>,
    /// Строка трассы под листом - у кадра видео.
    pub trace: Option<String>,
    /// Вшивать ли шрифты `@font-face`: растеризатору они не нужны - начертания он
    /// получает байтами.
    pub fonts: bool,
}

/// Чертёж листа `key`; `None` - такого листа нет.
pub fn svg(key: &str, sheets: &[DrawSheet], layout: &Layout, options: &Options) -> Option<String> {
    let sheet = sheets.iter().find(|s| s.key == key)?;
    let picture = drawn(sheet, layout);
    let levels = Levels::of(layout);
    let inks = Inks::of(options.view);
    let tick = options.tick.as_ref().filter(|_| options.view == View::Run);
    let lit = tick.map(|t| sheet_run(sheet, t)).unwrap_or_default();

    let mut body = String::new();
    frames(&mut body, sheet);
    edges(
        &mut body,
        sheet,
        &picture,
        &lit,
        &levels,
        &inks,
        options.view,
    );
    for node in &sheet.nodes {
        node_svg(
            &mut body,
            sheet,
            sheets,
            node,
            &picture,
            &lit,
            tick,
            &levels,
            &inks,
            options.view,
            layout.entry_of(&sheet.key),
        );
    }

    let (mut w, mut h) = (sheet.size.w, sheet.size.h);
    let (x0, y0) = (sheet.size.ox, sheet.size.oy);
    if options.legend {
        let table = legend(sheet, &levels);
        if !table.rows.is_empty() {
            let (lx, ly) = match layout.legend {
                LegendPlace::Right => (x0 + w, y0 + MARGIN_LEGEND),
                LegendPlace::Bottom | LegendPlace::Float => (x0 + MARGIN_LEGEND, y0 + h),
            };
            table.write(&mut body, lx, ly, &levels);
            match layout.legend {
                LegendPlace::Right => {
                    w += table.width + MARGIN_LEGEND;
                    h = h.max(table.height + 2.0 * MARGIN_LEGEND);
                }
                _ => {
                    w = w.max(table.width + 2.0 * MARGIN_LEGEND);
                    h += table.height + MARGIN_LEGEND;
                }
            }
        }
    }
    if let Some(line) = &options.trace {
        h += trace(
            &mut body,
            line,
            x0 + MARGIN_LEGEND,
            y0 + h,
            w - 2.0 * MARGIN_LEGEND,
        );
    }

    let mut out = String::new();
    let _ = write!(
        out,
        r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="{} {} {} {}" width="{}" height="{}">"#,
        num(x0),
        num(y0),
        num(w),
        num(h),
        num(w),
        num(h)
    );
    if options.fonts {
        out.push_str("<style>");
        // Шрифты - те, которыми лист набран на деле: запасная гарнитура появляется
        // и у знака, которого в основной нет.
        for face in [Face::Gost, Face::Mono]
            .iter()
            .filter(|face| body.contains(&face.css()))
        {
            out.push_str(&face.font_face());
        }
        out.push_str("</style>");
    }
    out.push_str(&defs(levels.arrow, &inks, levels.edge));
    out.push_str(&body);
    out.push_str("</svg>\n");
    Some(out)
}

/// Отступ легенды и строки трассы от края листа.
const MARGIN_LEGEND: f64 = 24.0;

fn frames(out: &mut String, sheet: &DrawSheet) {
    for f in &sheet.frames {
        let _ = write!(
            out,
            r#"<rect x="{}" y="{}" width="{}" height="{}" rx="12" fill="none" stroke="{}" stroke-width="1.5" stroke-dasharray="6 4"/>"#,
            num(f.x),
            num(f.y),
            num(f.w),
            num(f.h),
            Palette::INK_OFF
        );
    }
}

/// Наконечники: переход (`open`) и продолжение (`solid`), обычные и ожидаемые.
fn defs(form: &str, inks: &Inks, edge: f64) -> String {
    let (d, close, size) = arrow_shape(form);
    let mut out = String::from("<defs>");
    for (id, solid, ink, width) in [
        ("arrow-open", false, inks.arrow, edge),
        ("arrow-solid", true, inks.arrow, edge),
        ("arrow-open-next", false, Palette::WARN_INK, 1.5),
        ("arrow-solid-next", true, Palette::WARN_INK, 1.5),
    ] {
        let paint = if solid {
            format!(r#"fill="{ink}" stroke="none""#)
        } else {
            format!(
                r#"fill="none" stroke="{ink}" stroke-width="{}""#,
                num(width)
            )
        };
        let close = if solid { "z" } else { close };
        let _ = write!(
            out,
            r#"<marker id="{id}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="{s}" markerHeight="{s}" orient="auto-start-reverse"><path d="{d}{close}" {paint}/></marker>"#,
            s = num(size)
        );
    }
    out.push_str("</defs>");
    out
}

/// Знак с номером нижним индексом: `S` и `1`, `K` и `2`.
fn mark_text(mark: &str) -> String {
    let (head, tail) = mark.split_at(mark.chars().next().map_or(0, char::len_utf8));
    format!(
        r#"{}<tspan dy="0.3em" font-size="{}em">{}</tspan>"#,
        escape(head),
        num(INDEX_EM),
        escape(tail)
    )
}

/// Текст гарнитурой `face`: знаки, которых в ней нет, - отрезками запасной
/// вшитой гарнитуры. Растеризатор, не найдя знака, набирает запасной гарнитурой
/// весь кусок текста, а браузер - только недостающий знак; отрезок делает картинку
/// одной у обоих.
fn runs(face: Face, text: &str) -> String {
    let fallback = face.fallback();
    let mut out = String::new();
    let mut run = String::new();
    let mut foreign = false;
    let flush = |run: &mut String, foreign: bool, out: &mut String| {
        if run.is_empty() {
            return;
        }
        if foreign {
            let _ = write!(
                out,
                r#"<tspan font-family="{}">{}</tspan>"#,
                fallback.css(),
                escape(run)
            );
        } else {
            out.push_str(&escape(run));
        }
        run.clear();
    };
    for c in text.chars() {
        let missing = !face.covers(c) && fallback.covers(c);
        if missing != foreign {
            flush(&mut run, foreign, &mut out);
            foreign = missing;
        }
        run.push(c);
    }
    flush(&mut run, foreign, &mut out);
    out
}

fn escape(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

#[allow(clippy::too_many_arguments)]
fn edges(
    out: &mut String,
    sheet: &DrawSheet,
    picture: &Drawn,
    lit: &Lit,
    levels: &Levels,
    inks: &Inks,
    view: View,
) {
    let by_key: BTreeMap<&str, &crate::sheet::DrawEdge> =
        sheet.edges.iter().map(|e| (e.key.as_str(), e)).collect();
    for (i, edge) in picture.edges.iter().enumerate() {
        let Some(source) = by_key.get(edge.key.as_str()) else {
            continue;
        };
        let next = view == View::Run && lit.next_edges.contains(&edge.key);
        if next {
            let _ = write!(
                out,
                r#"<path d="{}" fill="none" stroke="{}" stroke-width="{}" stroke-linecap="round" stroke-linejoin="round"/>"#,
                edge.d,
                Palette::WARN_BG,
                num(levels.edge + 7.5)
            );
        }
        let marker = format!(
            "arrow-{}{}",
            if source.kind == EdgeKind::Next {
                "solid"
            } else {
                "open"
            },
            if next { "-next" } else { "" }
        );
        let (stroke, width) = if next {
            (Palette::WARN_INK, 2.0)
        } else {
            (inks.edge, levels.edge)
        };
        let mut attrs = format!(
            r#"d="{}" fill="none" stroke="{stroke}" stroke-width="{}""#,
            edge.d,
            num(width)
        );
        if !edge.ending {
            let _ = write!(attrs, r#" marker-end="url(#{marker})""#);
        }
        if let Some(dash) = &edge.dash {
            let total: f64 = dash.split(' ').filter_map(|p| p.parse::<f64>().ok()).sum();
            let _ = write!(
                attrs,
                r#" pathLength="{}" stroke-dasharray="{dash}""#,
                num(total)
            );
        }
        if let (Some(mark), Some(_)) = (edge.mark, &source.cond) {
            // Линия расступается под знаком: щель вырезана маской, а не закрыта
            // заливкой - поле под знаком остаётся полем.
            let id = format!("gap-{i}");
            let gw = source.mark.chars().count() as f64 * 8.0 + 14.0;
            let (x, y, w, h) = (picture.ox, picture.oy, picture.w, picture.h);
            let _ = write!(
                out,
                r#"<mask id="{id}" maskUnits="userSpaceOnUse" x="{x}" y="{y}" width="{w}" height="{h}"><rect x="{x}" y="{y}" width="{w}" height="{h}" fill="white"/><rect x="{}" y="{}" width="{}" height="21" fill="black"/></mask>"#,
                num(mark[0] - gw / 2.0),
                num(mark[1] - 10.5),
                num(gw),
                x = num(x),
                y = num(y),
                w = num(w),
                h = num(h)
            );
            let _ = write!(out, r#"<path {attrs} mask="url(#{id})"/>"#);
            let _ = write!(
                out,
                r#"<text x="{}" y="{}" font-family="{}" font-size="{}" fill="{}" text-anchor="middle">{}</text>"#,
                num(mark[0]),
                num(mark[1] + 4.0),
                levels.cond_face.css(),
                num(levels.cond_px),
                inks.edge_mark,
                mark_text(&source.mark)
            );
        } else {
            let _ = write!(out, "<path {attrs}/>");
        }
    }
}

/// Раскраска узла в цветном виде: заливка, рамка, толщина, штрих, чернила знака.
fn paint(node: &DrawNode, lit: &Lit, levels: &Levels, inks: &Inks) -> (String, &'static str) {
    let square = node.kind == takt_lang::layout::NodeKind::Composition;
    let base_width = if square { 1.5 } else { levels.node };
    if lit.running.contains(&node.name) {
        return (
            format!(
                r#"fill="{}" stroke="{}" stroke-width="2.5""#,
                Palette::YES,
                Palette::ACCENT
            ),
            Palette::YES_INK,
        );
    }
    if lit.expected.contains(&node.name) {
        return (
            format!(
                r#"fill="{}" stroke="{}" stroke-width="2.5""#,
                Palette::ALARM_BG,
                Palette::ALARM
            ),
            Palette::ALARM_INK,
        );
    }
    if lit.reachable.contains(&node.name) {
        return (
            format!(
                r#"fill="{}" stroke="{}" stroke-width="2" stroke-dasharray="6 3""#,
                Palette::WARN_BG,
                Palette::WARN
            ),
            Palette::WARN_INK,
        );
    }
    (
        format!(
            r#"fill="{}" stroke="{}" stroke-width="{}""#,
            inks.node_fill,
            inks.node_stroke,
            num(base_width)
        ),
        inks.node_mark,
    )
}

#[allow(clippy::too_many_arguments)]
fn node_svg(
    out: &mut String,
    sheet: &DrawSheet,
    sheets: &[DrawSheet],
    node: &DrawNode,
    picture: &Drawn,
    lit: &Lit,
    tick: Option<&Tick>,
    levels: &Levels,
    inks: &Inks,
    view: View,
    entry_port: u32,
) {
    let h = SIDE / 2.0;
    let square = node.kind == takt_lang::layout::NodeKind::Composition;
    let inner = square.then(|| inner_of(sheet, sheets, node)).flatten();
    let (inner_lit, plate) = match (inner, tick) {
        (Some(inner), Some(tick)) if view == View::Run => {
            inner_run(sheet, sheets, node, inner, tick)
        }
        _ => (BTreeSet::new(), String::new()),
    };
    let mut lit_here = lit.clone();
    if !plate.is_empty() {
        lit_here.running.insert(node.name.clone());
    }
    let (body_paint, mark_ink) = paint(node, &lit_here, levels, inks);
    let (x, y) = (node.x, node.y);
    if square {
        let _ = write!(
            out,
            r#"<rect x="{}" y="{}" width="{s}" height="{s}" rx="8" {body_paint}/>"#,
            num(x - h),
            num(y - h),
            s = num(SIDE)
        );
        if let Some(inner) = inner {
            mini(out, node, inner, &inner_lit);
        }
        let _ = write!(
            out,
            r#"<text x="{}" y="{}" font-family="{}" font-size="{}" fill="{mark_ink}" text-anchor="middle">{}</text>"#,
            num(x - h + 22.0),
            num(y - h + 26.0),
            levels.state_face.css(),
            num(levels.state_px),
            mark_text(&node.mark)
        );
        if !plate.is_empty() {
            // Плашка не уже квадрата и не уже своего текста: у холста текст
            // выходит за плашку, а картинке его обрезать нечем - она его покажет
            // целиком.
            let px = text_px("xs");
            let width = SIDE.max(levels.state_face.width(&plate, px) + 16.0);
            let _ = write!(
                out,
                r#"<rect x="{}" y="{}" width="{}" height="20" rx="10" fill="{}" stroke="{}" stroke-width="1"/><text x="{}" y="{}" font-family="{}" font-size="{}" fill="{}" text-anchor="middle">{}</text>"#,
                num(x - width / 2.0),
                num(y + h + 8.0),
                num(width),
                Palette::YES,
                Palette::ACCENT,
                num(x),
                num(y + h + 22.0),
                levels.state_face.css(),
                num(px),
                Palette::YES_INK,
                runs(levels.state_face, &plate)
            );
        }
    } else {
        let _ = write!(
            out,
            r#"<circle cx="{}" cy="{}" r="{}" {body_paint}/>"#,
            num(x),
            num(y),
            num(R)
        );
        if picture
            .nodes
            .iter()
            .any(|n| n.name == node.name && n.entry.is_some())
        {
            entry_arrow(out, node, entry_port);
        }
        if node.kind == takt_lang::layout::NodeKind::End {
            let _ = write!(
                out,
                r#"<circle cx="{}" cy="{}" r="{}" fill="none" stroke="{}" stroke-width="1.5"/>"#,
                num(x),
                num(y),
                num(R - 5.0),
                Palette::INK_SOFT
            );
        }
        let _ = write!(
            out,
            r#"<text x="{}" y="{}" font-family="{}" font-size="{}" fill="{mark_ink}" text-anchor="middle">{}</text>"#,
            num(x),
            num(y + 5.0),
            levels.state_face.css(),
            num(levels.state_px),
            mark_text(&node.mark)
        );
    }
    let count = lit.counts.get(&node.name).copied().unwrap_or(0);
    if view == View::Run && count >= 2 {
        let edge = if square { h } else { R * 0.72 };
        let _ = write!(
            out,
            r#"<circle cx="{}" cy="{}" r="9" fill="{}" stroke="{}" stroke-width="1"/><text x="{}" y="{}" font-family="{}" font-size="{}" fill="{}" text-anchor="middle">{count}</text>"#,
            num(x + edge),
            num(y - edge),
            Palette::YES,
            Palette::ACCENT,
            num(x + edge),
            num(y - edge + 4.0),
            levels.state_face.css(),
            num(text_px("xs")),
            Palette::YES_INK
        );
    }
}

/// Стрелка начального состояния: входит в круг в своей точке привязки.
fn entry_arrow(out: &mut String, node: &DrawNode, port: u32) {
    let angle = f64::from(port) * 2.0 * std::f64::consts::PI / f64::from(crate::geometry::PORTS);
    let dir = [angle.cos(), angle.sin()];
    let side = [-dir[1], dir[0]];
    let at = |reach: f64, lateral: f64| {
        format!(
            "{:.1} {:.1}",
            node.x + dir[0] * reach + side[0] * lateral,
            node.y + dir[1] * reach + side[1] * lateral
        )
    };
    let tip = at(R + 4.0, 0.0);
    let _ = write!(
        out,
        r#"<path d="M{}L{tip}" fill="none" stroke="{ink}" stroke-width="1.5"/><path d="M{}L{tip}L{}" fill="none" stroke="{ink}" stroke-width="1.5"/>"#,
        at(R + 16.0, 0.0),
        at(R + 8.0, 4.0),
        at(R + 8.0, -4.0),
        ink = Palette::INK
    );
}

/// Лист, куда ведёт вход в квадрат: лист модели либо лист композиции.
fn inner_of<'a>(
    sheet: &DrawSheet,
    sheets: &'a [DrawSheet],
    node: &DrawNode,
) -> Option<&'a DrawSheet> {
    let key = match node.implements.as_ref()? {
        Implement::Model { path, .. } => path.clone()?,
        _ if !sheet.composition => composition_key(&sheet.owner_path, &node.name),
        _ => return None,
    };
    sheets.iter().find(|s| s.key == key)
}

/// Внутренний прогон квадрата: горящие узлы миниатюры и текст плашки (правило -
/// у `innerRun` холста).
fn inner_run(
    sheet: &DrawSheet,
    sheets: &[DrawSheet],
    node: &DrawNode,
    inner: &DrawSheet,
    tick: &Tick,
) -> (BTreeSet<String>, String) {
    let place = Place::of(sheet);
    if matches!(node.implements, Some(Implement::Model { .. })) {
        let group = if sheet.composition {
            let step = sheet
                .nodes
                .iter()
                .position(|n| n.name == node.name)
                .map_or(0, |i| i + 1);
            steps_of(
                &tick.active,
                &place,
                place.owner.as_deref().unwrap_or_default(),
            )
            .remove(&step)
        } else {
            steps_of(&tick.active, &place, &node.name).remove(&1)
        };
        let states = group.map(|g| g.states()).unwrap_or_default();
        let label = inner_label(inner, &states);
        return (states, label);
    }
    let groups = steps_of(&tick.active, &place, &node.name);
    let lit: BTreeSet<String> = groups
        .keys()
        .filter_map(|k| inner.nodes.get(k - 1))
        .map(|n| n.name.clone())
        .collect();
    let label = steps_label(inner, &groups, |step, group| {
        let states = group.states();
        match inner
            .nodes
            .get(step - 1)
            .and_then(|n| inner_of(inner, sheets, n))
        {
            Some(own) => inner_label(own, &states),
            None => states.into_iter().collect::<Vec<_>>().join(", "),
        }
    });
    (lit, label)
}

/// Миниатюра внутреннего листа в квадрате: точки и линии, без текста.
fn mini(out: &mut String, node: &DrawNode, inner: &DrawSheet, lit: &BTreeSet<String>) {
    if inner.nodes.is_empty() {
        return;
    }
    let h = SIDE / 2.0;
    let s = (SIDE - 40.0) / inner.size.w.max(inner.size.h);
    let ox = node.x - h + 20.0 - inner.size.ox * s;
    let oy = node.y - h + 28.0 - inner.size.oy * s;
    let shapes: BTreeMap<&str, crate::geometry::Shape> = inner
        .nodes
        .iter()
        .map(|n| (n.name.as_str(), n.shape()))
        .collect();
    for edge in &inner.edges {
        let (Some(from), Some(to)) = (shapes.get(edge.from.as_str()), shapes.get(edge.to.as_str()))
        else {
            continue;
        };
        let target = if edge.from == edge.to { from } else { to };
        let pts: Vec<String> = route(from, target, &edge.points)
            .iter()
            .map(|p| format!("{:.1} {:.1}", ox + p[0] * s, oy + p[1] * s))
            .collect();
        let _ = write!(
            out,
            r#"<path d="M{}" fill="none" stroke="{}" stroke-width="1"/>"#,
            pts.join("L"),
            Palette::INK_SOFT
        );
    }
    for inside in &inner.nodes {
        let fill = if lit.contains(&inside.name) {
            Palette::WARN
        } else {
            Palette::INK_SOFT
        };
        let _ = write!(
            out,
            r#"<circle cx="{:.1}" cy="{:.1}" r="3" fill="{fill}"/>"#,
            ox + inside.x * s,
            oy + inside.y * s
        );
    }
}

/// Легенда: строки знаков, выложенные колонками.
struct Table {
    rows: Vec<Vec<(String, Face, f64, bool)>>,
    widths: Vec<f64>,
    width: f64,
    height: f64,
}

/// Наибольшая ширина колонки легенды, кеглей ступени: длинная подпись обрезается
/// многоточием.
const COLUMN_EM: f64 = 18.0;
const COLUMN_GAP: f64 = 16.0;

fn legend(sheet: &DrawSheet, levels: &Levels) -> Table {
    let (sf, sp, cf, cp) = (
        levels.state_face,
        levels.state_px,
        levels.cond_face,
        levels.cond_px,
    );
    let mut rows: Vec<Vec<(String, Face, f64, bool)>> = Vec::new();
    for node in &sheet.nodes {
        let name = node.model.clone().unwrap_or_else(|| node.name.clone());
        rows.push(vec![
            (node.mark.clone(), sf, sp, true),
            (name, sf, sp, false),
            (node.alias.clone(), sf, sp, false),
        ]);
    }
    let marks: BTreeMap<&str, &str> = sheet
        .nodes
        .iter()
        .map(|n| (n.name.as_str(), n.mark.as_str()))
        .collect();
    for edge in sheet.edges.iter().filter(|e| e.cond.is_some()) {
        let pair = format!(
            "{} → {}",
            marks.get(edge.from.as_str()).copied().unwrap_or(&edge.from),
            marks.get(edge.to.as_str()).copied().unwrap_or(&edge.to)
        );
        rows.push(vec![
            (edge.mark.clone(), cf, cp, true),
            (edge.cond.clone().unwrap_or_default(), cf, cp, false),
            (edge.alias.clone(), cf, cp, false),
            (pair, cf, cp, false),
        ]);
    }
    let columns = rows.iter().map(Vec::len).max().unwrap_or(0);
    let mut widths = vec![0f64; columns];
    for row in &mut rows {
        for (i, (text, face, px, _)) in row.iter_mut().enumerate() {
            *text = face.fit(text, *px, COLUMN_EM * *px);
            widths[i] = widths[i].max(face.width(text, *px));
        }
    }
    let line = sp.max(cp) * 1.6;
    let width = widths.iter().sum::<f64>() + COLUMN_GAP * columns.saturating_sub(1) as f64;
    Table {
        height: line * rows.len() as f64,
        rows,
        widths,
        width,
    }
}

impl Table {
    fn write(&self, out: &mut String, x: f64, y: f64, levels: &Levels) {
        let line = levels.state_px.max(levels.cond_px) * 1.6;
        for (r, row) in self.rows.iter().enumerate() {
            let baseline = y + line * (r as f64 + 0.75);
            let mut at = x;
            for (i, (text, face, px, is_mark)) in row.iter().enumerate() {
                if !text.is_empty() {
                    let content = if *is_mark {
                        mark_text(text)
                    } else {
                        runs(*face, text)
                    };
                    let _ = write!(
                        out,
                        r#"<text x="{}" y="{}" font-family="{}" font-size="{}" fill="{}">{content}</text>"#,
                        num(at),
                        num(baseline),
                        face.css(),
                        num(*px),
                        Palette::INK
                    );
                }
                at += self.widths[i] + COLUMN_GAP;
            }
        }
    }
}

/// Строка трассы под листом, перенесённая по ширине; ответ - занятая высота.
fn trace(out: &mut String, line: &str, x: f64, y: f64, width: f64) -> f64 {
    let px = text_px("xs");
    let per_line = (width / Face::Mono.width("0", px)).floor().max(20.0) as usize;
    let chars: Vec<char> = line.chars().collect();
    let lines: Vec<String> = chars.chunks(per_line).map(|c| c.iter().collect()).collect();
    let step = px * 1.5;
    for (i, text) in lines.iter().enumerate() {
        let _ = write!(
            out,
            r#"<text x="{}" y="{}" font-family="{}" font-size="{}" fill="{}" xml:space="preserve">{}</text>"#,
            num(x),
            num(y + step * (i as f64 + 1.0)),
            Face::Mono.css(),
            num(px),
            Palette::INK,
            escape(text)
        );
    }
    step * lines.len() as f64 + MARGIN_LEGEND / 2.0
}

/// Имя файла листа без расширения: путь модели с `/`, заменённым на `.`; корень -
/// имя файла модели; лист композиции - `<лист>#<состояние>`.
pub fn file_stem(key: &str, root: &str) -> String {
    let (owner, state) = key
        .split_once('#')
        .map_or((key, None), |(o, s)| (o, Some(s)));
    let base = if owner == "/" {
        root.to_string()
    } else {
        owner.replace('/', ".")
    };
    match state {
        Some(state) => format!("{base}#{state}"),
        None => base,
    }
}

/// Чертежи всех листов модели: пары "ключ листа, SVG" в порядке листов.
///
/// # Ошибки
/// Текст модели не разбирается, файл раскладки не читается либо неполон.
pub fn render_all(
    source: &str,
    layout_text: &str,
    options: &Options,
) -> Result<Vec<(String, String)>, String> {
    let graph = takt_lang::layout::graph_of(source).map_err(|d| d.message.clone())?;
    let layout = crate::layout::parse(layout_text).map_err(|e| e.0)?;
    let sheets = crate::sheet::sheets(&graph, &layout).map_err(|e| e.to_string())?;
    Ok(sheets
        .iter()
        .filter_map(|s| svg(&s.key, &sheets, &layout, options).map(|text| (s.key.clone(), text)))
        .collect())
}

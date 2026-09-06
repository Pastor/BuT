use svg::Document;
use svg::node::element::{Circle, Definitions, Group, Line, Marker, Path, Rectangle, Style, Text};

use crate::graphics_config::GraphicsConfig;
use crate::unit::Unit;
use takt_lang::diagnostics::Diagnostic;

// -- Данные легенды ------------------------------------------------------------

/// Данные для отображения легенды портов и переменных на кадре симуляции.
pub(crate) struct LegendData {
    pub in_ports: Vec<(String, String)>,
    pub out_ports: Vec<(String, String)>,
    pub inout_ports: Vec<(String, String)>,
    pub vars: Vec<(String, String)>,
}

// -- Общий тип позиций ---------------------------------------------------------

/// Вектор позиций узлов: каждая запись - координаты центра (x, y).
type Positions = Vec<(f64, f64)>;

// -- Перечисления вывода -------------------------------------------------------

/// Результат отрисовки: обёртка над конкретным документом.
// `SVG` - устоявшийся акроним формата (как `HTML`/`PNG`); `Svg` читался бы хуже.
#[allow(clippy::upper_case_acronyms)]
pub(crate) enum Viewport {
    SVG(Document),
}

impl Viewport {
    /// Сохраняет Viewport в файл по указанному пути.
    #[allow(dead_code)]
    pub fn save_to_file(&self, path: &str) -> Result<(), std::io::Error> {
        match self {
            Viewport::SVG(document) => Ok(svg::save(path, document)?),
        }
    }
}

// -- Публичный API -------------------------------------------------------------

/// Кэшированный результат вычисления раскладки графа.
///
/// Раскладка (positions) зависит только от структуры модели и не меняется в ходе
/// симуляции, поэтому её достаточно вычислить один раз.
pub(crate) struct CachedLayout {
    /// Полные имена состояний - используются для сопоставления с active_states.
    pub(crate) node_labels: Vec<String>,
    /// Краткие псевдонимы S1/S2/... - отображаются в кружках на графе.
    pub(crate) node_aliases: Vec<String>,
    /// Рёбра с полными именами предикатов.
    pub(crate) edges_vec: Vec<(usize, usize, String)>,
    pub(crate) positions: Positions,
}

/// Вычисляет раскладку графа для `unit`.
///
/// Дорогостоящий шаг (имитация отжига): вызывать один раз перед записью GIF, затем
/// передавать результат в [`render_from_layout`] для каждого кадра.
pub(crate) fn compute_layout(unit: &Unit, cfg: &GraphicsConfig) -> CachedLayout {
    let g = graph::unit_to_graph(unit);
    let (node_labels, edges_vec, positions) = graph::calculate_graph(g, cfg);
    let node_aliases: Vec<String> = (1..=node_labels.len()).map(|i| format!("S{i}")).collect();
    CachedLayout {
        node_labels,
        node_aliases,
        edges_vec,
        positions,
    }
}

/// Отрисовывает кадр из заранее вычисленной раскладки.
///
/// `highlighted_edge` - индексы рёбра `(from_idx, to_idx)`, которое нужно подсветить;
/// `None` означает обычный кадр без подсветки.
pub(crate) fn render_from_layout(
    layout: &CachedLayout,
    cfg: &GraphicsConfig,
    active_states: &[&str],
    legend: Option<&LegendData>,
    model_name: Option<&str>,
    highlighted_edge: Option<(usize, usize)>,
    is_svg: bool,
) -> Result<Viewport, Diagnostic> {
    let document = create_svg(
        &layout.node_labels,
        &layout.node_aliases,
        &layout.edges_vec,
        &layout.positions,
        cfg,
        active_states,
        legend,
        model_name,
        highlighted_edge,
        is_svg,
    );
    Ok(Viewport::SVG(document))
}

/// Создаёт [`Viewport`] из симуляционного [`Unit`].
///
/// `active_states` - срез имён состояний, которые нужно подсветить. Каждый вызов
/// пересчитывает раскладку. Для GIF-записи используйте [`compute_layout`] +
/// [`render_from_layout`].
#[allow(dead_code)]
pub(crate) fn create_viewport(
    unit: &Unit,
    configuration: GraphicsConfig,
    active_states: &[&str],
    legend: Option<&LegendData>,
) -> Result<Viewport, Diagnostic> {
    let layout = compute_layout(unit, &configuration);
    render_from_layout(
        &layout,
        &configuration,
        active_states,
        legend,
        None,
        None,
        false,
    )
}

// -- Геометрические вспомогательные функции для SVG ---------------------------

/// Проверяет, пересекается ли прямоугольник AABB `(rx1, ry1)-(rx2, ry2)` с кругом
/// центром `(cx, cy)` и радиусом `r`.
///
/// Алгоритм: ближайшая точка прямоугольника к центру круга находится через зажим
/// (clamp), затем проверяется строгое неравенство `d² < r²`.
fn rect_overlaps_circle(rx1: f64, ry1: f64, rx2: f64, ry2: f64, cx: f64, cy: f64, r: f64) -> bool {
    let nx = cx.clamp(rx1, rx2);
    let ny = cy.clamp(ry1, ry2);
    (cx - nx).powi(2) + (cy - ny).powi(2) < r * r
}

/// Возвращает площадь пересечения двух выровненных прямоугольников AABB.
///
/// Прямоугольники задаются парами углов: `(ax1, ay1)-(ax2, ay2)` и `(bx1, by1)-(bx2,
/// by2)`. Если прямоугольники не перекрываются - возвращает `0.0`.
#[allow(clippy::too_many_arguments)]
fn rects_intersection_area(
    ax1: f64,
    ay1: f64,
    ax2: f64,
    ay2: f64,
    bx1: f64,
    by1: f64,
    bx2: f64,
    by2: f64,
) -> f64 {
    ((ax2.min(bx2) - ax1.max(bx1)).max(0.0)) * ((ay2.min(by2) - ay1.max(by1)).max(0.0))
}

// -- Генератор SVG -------------------------------------------------------------

/// Отрисовывает граф в SVG-документ.
///
/// Шаги отрисовки:
/// 1. Создаёт холст с параметрами из `cfg`.
/// 2. Добавляет стили шрифта ГОСТ и маркер-стрелку для рёбер.
/// 3. Рисует рёбра в виде кривых Безье со стрелками и подписями.
///    - Параллельные рёбра между одной парой узлов разводятся разным изгибом.
///    - Подписи рёбер размещаются жадным алгоритмом на концентрических кольцах
///      вокруг середины кривой Безье, минимизируя перекрытия с узлами и другими подписями.
/// 4. Рисует кружки узлов с подписями.
///
/// # Параметры
/// - `node_labels` - имена состояний в порядке индексации.
/// - `edges_vec` - рёбра `(индекс_источника, индекс_цели, подпись)`.
/// - `positions` - координаты центров узлов, соответствующие `node_labels`.
/// - `cfg` - конфигурация с размерами холста и параметрами отрисовки.
#[allow(clippy::too_many_arguments)]
fn create_svg(
    node_labels: &[String],
    node_aliases: &[String],
    edges_vec: &[(usize, usize, String)],
    positions: &Positions,
    cfg: &GraphicsConfig,
    active_states: &[&str],
    legend: Option<&LegendData>,
    model_name: Option<&str>,
    highlighted_edge: Option<(usize, usize)>,
    is_svg: bool,
) -> Document {
    // Легенда разбивается на два независимых блока: слева - состояния, справа - порты.
    // Граф рисуется по центру через <g transform="translate(left_w, 0)">.
    let leg_w = if legend.is_some() {
        cfg.legend.width
    } else {
        0.0
    };
    let total_width = leg_w + cfg.canvas.width + leg_w;
    let is_highlight_frame = highlighted_edge.is_some();

    let mut document = Document::new()
        .set("viewBox", (0, 0, total_width, cfg.canvas.height))
        .set("xmlns", "http://www.w3.org/2000/svg");

    let style = format!(
        r#"
        .gost-text {{
            font-family: {node_font};
            font-size: {node_fs}px;
            fill: {node_color};
            text-anchor: middle;
            dominant-baseline: central;
        }}
        .edge-label {{
            font-family: {edge_font};
            font-size: {edge_fs}px;
            fill: {edge_color};
            text-anchor: middle;
            dominant-baseline: central;
        }}
        "#,
        node_font = cfg.node.text_font_family,
        node_fs = cfg.node.text_font_size,
        node_color = cfg.node.text_color,
        edge_font = cfg.edge_label.font_family,
        edge_fs = cfg.edge_label.font_size,
        edge_color = cfg.edge_label.text_color,
    );
    document = document.add(Style::new(style));

    document = document.add(
        Definitions::new()
            .add(
                Marker::new()
                    .set("id", "arrow")
                    .set("viewBox", "0 0 10 10")
                    .set("refX", "10")
                    .set("refY", "5")
                    .set("markerWidth", cfg.edge.arrow_size)
                    .set("markerHeight", cfg.edge.arrow_size)
                    .set("orient", "auto")
                    .add(
                        Path::new()
                            .set("d", "M 0 0 L 10 5 L 0 10 z")
                            .set("fill", cfg.edge.stroke.as_str()),
                    ),
            )
            .add(
                Marker::new()
                    .set("id", "arrow-hl")
                    .set("viewBox", "0 0 10 10")
                    .set("refX", "10")
                    .set("refY", "5")
                    .set("markerWidth", cfg.edge.arrow_size)
                    .set("markerHeight", cfg.edge.arrow_size)
                    .set("orient", "auto")
                    .add(
                        Path::new()
                            .set("d", "M 0 0 L 10 5 L 0 10 z")
                            .set("fill", cfg.edge.highlight_stroke.as_str()),
                    ),
            ),
    );

    // Фон (SVG-режим): покрывает всю ширину включая панели легенды.
    if is_svg && let Some(ref bg) = cfg.canvas.svg_background {
        document = document.add(
            Rectangle::new()
                .set("x", 0)
                .set("y", 0)
                .set("width", total_width)
                .set("height", cfg.canvas.height)
                .set("fill", bg.as_str()),
        );
    }

    // -- Граф (рёбра + узлы + шапка) в сдвинутой группе ----------------------
    // Координаты внутри группы - те же [0, canvas.width] x [0, canvas.height].
    let mut graph = Group::new();
    if leg_w > 0.0 {
        graph = graph.set("transform", format!("translate({leg_w}, 0)"));
    }

    // Highlight-рамка (только GIF).
    if is_highlight_frame && !is_svg {
        let inset = cfg.highlight.border_inset;
        graph = graph.add(
            Rectangle::new()
                .set("x", inset)
                .set("y", inset)
                .set("width", cfg.canvas.width - 2.0 * inset)
                .set("height", cfg.canvas.height - 2.0 * inset)
                .set("fill", "none")
                .set("stroke", cfg.highlight.border_color.as_str())
                .set("stroke-width", cfg.highlight.border_width),
        );
    }

    // Центры узлов сохраняются для проверки перекрытий подписей рёбер.
    let node_circles: Vec<(f64, f64)> = positions.clone();
    let mut placed_boxes: Vec<(f64, f64, f64, f64)> = Vec::new();

    // Подсчёт кратности: сколько раз встречается пара (min(i,j), max(i,j)).
    let mut connection_counts: std::collections::HashMap<(usize, usize), usize> =
        std::collections::HashMap::new();
    let mut edge_multiplicities = Vec::with_capacity(edges_vec.len());
    for &(i, j, _) in edges_vec.iter() {
        let key = if i < j { (i, j) } else { (j, i) };
        let count = connection_counts.entry(key).or_insert(0);
        edge_multiplicities.push(*count);
        *count += 1;
    }

    // -- Рёбра ----------------------------------------------------------------
    for (idx, (i, j, edge_label)) in edges_vec.iter().enumerate() {
        let is_highlighted = highlighted_edge.is_some_and(|(hi, hj)| *i == hi && *j == hj);
        let multiplicity = edge_multiplicities[idx];
        let (x1, y1) = positions[*i];
        let (x2, y2) = positions[*j];

        let dx = x2 - x1;
        let dy = y2 - y1;
        let d = (dx * dx + dy * dy).sqrt();
        if d < 1e-6 {
            continue;
        }
        let ux = dx / d;
        let uy = dy / d;

        // Канонический перпендикуляр: встречные рёбра (A->B и B->A) получают
        // противоположные смещения и не наслаиваются.
        let (cpx, cpy) = if *i <= *j { (-uy, ux) } else { (uy, -ux) };
        let perp_sign = if multiplicity % 2 == 0 { 1.0 } else { -1.0 };
        let perp_mag = perp_sign * (3.0 + (multiplicity as f64 / 2.0).floor() * 5.0);

        let start_x = x1 + ux * cfg.node.radius + cpx * perp_mag;
        let start_y = y1 + uy * cfg.node.radius + cpy * perp_mag;
        let end_x = x2 - ux * cfg.node.radius + cpx * perp_mag;
        let end_y = y2 - uy * cfg.node.radius + cpy * perp_mag;

        let mid_x = (start_x + end_x) / 2.0;
        let mid_y = (start_y + end_y) / 2.0;
        let curve_factor = cfg.edge.curve_coefficient;
        let cp_x = mid_x - uy * d * curve_factor;
        let cp_y = mid_y + ux * d * curve_factor;

        // В SVG-режиме подсвеченное ребро - оранжевое, но обычной толщины.
        let (edge_stroke, edge_width, arrow_marker) = if is_highlighted {
            let width = if is_svg {
                cfg.edge.stroke_width
            } else {
                cfg.edge.highlight_stroke_width
            };
            (cfg.edge.highlight_stroke.as_str(), width, "url(#arrow-hl)")
        } else {
            (
                cfg.edge.stroke.as_str(),
                cfg.edge.stroke_width,
                "url(#arrow)",
            )
        };
        graph = graph.add(
            Path::new()
                .set(
                    "d",
                    format!(
                        "M {} {} Q {} {} {} {}",
                        start_x, start_y, cp_x, cp_y, end_x, end_y
                    ),
                )
                .set("fill", "none")
                .set("stroke", edge_stroke)
                .set("stroke-width", edge_width)
                .set("marker-end", arrow_marker),
        );

        // Подпись только для подсвеченного ребра.
        if is_highlighted {
            let bez_mid_x = 0.25 * start_x + 0.5 * cp_x + 0.25 * end_x;
            let bez_mid_y = 0.25 * start_y + 0.5 * cp_y + 0.25 * end_y;

            let box_w = edge_label.chars().count() as f64 * cfg.edge_label.char_width + 8.0;
            let box_h = cfg.edge_label.font_size + 4.0;
            let n_angles = cfg.edge_label.search_angles.max(1);
            let mut best_center = (bez_mid_x, bez_mid_y);
            let mut best_score = f64::MAX;

            'search: for &r in &cfg.edge_label.search_radii {
                let n = if r < 1.0 { 1 } else { n_angles };
                for k in 0..n {
                    let angle = (k as f64) * std::f64::consts::TAU / (n as f64);
                    let cx = bez_mid_x + r * angle.cos();
                    let cy = bez_mid_y + r * angle.sin();
                    let bx1 = cx - box_w / 2.0 - cfg.edge_label.margin;
                    let by1 = cy - box_h / 2.0 - cfg.edge_label.margin;
                    let bx2 = cx + box_w / 2.0 + cfg.edge_label.margin;
                    let by2 = cy + box_h / 2.0 + cfg.edge_label.margin;

                    let mut score = r;
                    if bx1 < 0.0 || bx2 > cfg.canvas.width || by1 < 0.0 || by2 > cfg.canvas.height {
                        score += 1e8;
                    }
                    for &(nx, ny) in &node_circles {
                        if rect_overlaps_circle(
                            bx1,
                            by1,
                            bx2,
                            by2,
                            nx,
                            ny,
                            cfg.node.radius + cfg.edge_label.margin,
                        ) {
                            score += 1e5;
                        }
                    }
                    for &(lx1, ly1, lx2, ly2) in &placed_boxes {
                        let area = rects_intersection_area(bx1, by1, bx2, by2, lx1, ly1, lx2, ly2);
                        if area > 0.0 {
                            score += 500.0 * area;
                        }
                    }
                    if score < best_score {
                        best_score = score;
                        best_center = (cx, cy);
                    }
                    if score < 1.0 {
                        break 'search;
                    }
                }
            }

            let (label_x, label_y) = best_center;
            placed_boxes.push((
                label_x - box_w / 2.0,
                label_y - box_h / 2.0,
                label_x + box_w / 2.0,
                label_y + box_h / 2.0,
            ));

            let mut grp = Group::new();
            let lead_dist = ((label_x - bez_mid_x).powi(2) + (label_y - bez_mid_y).powi(2)).sqrt();
            if lead_dist > 5.0 {
                grp = grp.add(
                    Line::new()
                        .set("x1", bez_mid_x)
                        .set("y1", bez_mid_y)
                        .set("x2", label_x)
                        .set("y2", label_y)
                        .set("stroke", cfg.edge_label.leader_color.as_str())
                        .set("stroke-width", cfg.edge_label.leader_width)
                        .set("stroke-dasharray", cfg.edge_label.leader_dasharray.as_str()),
                );
            }
            let bg = Rectangle::new()
                .set("x", label_x - box_w / 2.0)
                .set("y", label_y - box_h / 2.0)
                .set("width", box_w)
                .set("height", box_h)
                .set("fill", cfg.edge_label.bg_fill.as_str())
                .set("stroke", cfg.edge_label.bg_stroke.as_str())
                .set("stroke-width", cfg.edge_label.bg_stroke_width)
                .set("rx", cfg.edge_label.bg_radius)
                .set("ry", cfg.edge_label.bg_radius);
            let lbl = Text::new(edge_label.clone())
                .set("x", label_x)
                .set("y", label_y)
                .set("class", "edge-label");
            graph = graph.add(grp.add(bg).add(lbl));
        }
    }

    // -- Узлы -----------------------------------------------------------------
    for (i, full_label) in node_labels.iter().enumerate() {
        let (cx, cy) = positions[i];
        let is_active = active_states.contains(&full_label.as_str());
        let fill = if is_active {
            cfg.node.active_fill.as_str()
        } else {
            cfg.node.inactive_fill.as_str()
        };
        let stroke_w = if is_active {
            cfg.node.stroke_width_active
        } else {
            cfg.node.stroke_width
        };
        let alias = node_aliases
            .get(i)
            .map(String::as_str)
            .unwrap_or(full_label.as_str());
        graph = graph.add(
            Circle::new()
                .set("cx", cx)
                .set("cy", cy)
                .set("r", cfg.node.radius)
                .set("fill", fill)
                .set("stroke", cfg.node.stroke.as_str())
                .set("stroke-width", stroke_w),
        );
        graph = graph.add(
            Text::new(alias)
                .set("x", cx)
                .set("y", cy)
                .set("class", "gost-text"),
        );
    }

    // -- Шапка: имя модели (SVG) или переход (GIF) ----------------------------
    if let Some((hi, hj)) = highlighted_edge {
        if !is_svg {
            let from_alias = node_aliases.get(hi).map(String::as_str).unwrap_or("");
            let to_alias = node_aliases.get(hj).map(String::as_str).unwrap_or("");
            let edge_label = edges_vec
                .iter()
                .find(|(i, j, _)| *i == hi && *j == hj)
                .map(|(_, _, lbl)| lbl.as_str())
                .unwrap_or("");
            let transition_text = if edge_label.is_empty() {
                format!("{from_alias} → {to_alias}")
            } else {
                format!("{from_alias} → {to_alias}:  {edge_label}")
            };
            graph = graph.add(
                Rectangle::new()
                    .set("x", 0)
                    .set("y", 0)
                    .set("width", cfg.canvas.width)
                    .set("height", cfg.highlight.header_height)
                    .set("fill", cfg.highlight.header_bg_color.as_str()),
            );
            graph = graph.add(
                Text::new(transition_text)
                    .set("x", cfg.canvas.width / 2.0)
                    .set("y", cfg.highlight.header_height / 2.0)
                    .set("font-family", cfg.highlight.header_font_family.as_str())
                    .set("font-size", format!("{}px", cfg.highlight.header_font_size))
                    .set("font-weight", cfg.highlight.header_font_weight.as_str())
                    .set("fill", cfg.highlight.header_text_color.as_str())
                    .set("text-anchor", "middle")
                    .set("dominant-baseline", "middle"),
            );
        } else if let Some(name) = model_name {
            graph = graph.add(
                Text::new(name)
                    .set("x", cfg.canvas.width / 2.0)
                    .set("y", cfg.model_name.y_offset)
                    .set("font-family", cfg.model_name.font_family.as_str())
                    .set("font-size", format!("{}px", cfg.model_name.font_size))
                    .set("font-weight", cfg.model_name.font_weight.as_str())
                    .set("fill", cfg.model_name.color.as_str())
                    .set("text-anchor", "middle")
                    .set("dominant-baseline", "middle"),
            );
        }
    } else if let Some(name) = model_name {
        graph = graph.add(
            Text::new(name)
                .set("x", cfg.canvas.width / 2.0)
                .set("y", cfg.model_name.y_offset)
                .set("font-family", cfg.model_name.font_family.as_str())
                .set("font-size", format!("{}px", cfg.model_name.font_size))
                .set("font-weight", cfg.model_name.font_weight.as_str())
                .set("fill", cfg.model_name.color.as_str())
                .set("text-anchor", "middle")
                .set("dominant-baseline", "middle"),
        );
    }

    document = document.add(graph);

    // -- Левая легенда: состояния + цвета -------------------------------------
    if let Some(leg) = legend {
        let pad = cfg.legend.padding;
        let line_h = cfg.legend.line_height;
        let font_size = format!("{}px", cfg.legend.font_size);
        let inner_w = cfg.legend.width - 2.0 * pad;

        // Высота левого блока: Состояния + N строк + цвета + 2 строки.
        let left_lines = 1 + node_labels.len() + 1 + 2;
        let left_box_h = left_lines as f64 * line_h + 2.0 * pad;

        document = document.add(
            Rectangle::new()
                .set("x", pad)
                .set("y", pad)
                .set("width", inner_w)
                .set("height", left_box_h)
                .set("fill", cfg.legend.bg_fill.as_str())
                .set("stroke", cfg.legend.bg_stroke.as_str())
                .set("stroke-width", cfg.legend.bg_stroke_width)
                .set("rx", cfg.legend.bg_radius)
                .set("ry", cfg.legend.bg_radius),
        );

        let lx = pad + pad; // отступ внутри рамки
        let mut y = pad + line_h;

        // Состояния
        document = document.add(
            Text::new(cfg.legend.state_header.as_str())
                .set("x", lx)
                .set("y", y)
                .set("font-family", cfg.legend.font_family.as_str())
                .set("font-size", font_size.clone())
                .set("font-weight", "bold")
                .set("fill", cfg.legend.header_color.as_str())
                .set("dominant-baseline", "middle"),
        );
        y += line_h;
        for (alias, full) in node_aliases.iter().zip(node_labels.iter()) {
            document = document.add(
                Text::new(format!("{alias}={full}"))
                    .set("x", lx)
                    .set("y", y)
                    .set("font-family", cfg.legend.font_family.as_str())
                    .set("font-size", font_size.clone())
                    .set("fill", cfg.legend.value_color.as_str())
                    .set("dominant-baseline", "middle"),
            );
            y += line_h;
        }

        // Цвета
        document = document.add(
            Text::new(cfg.legend.colors_header.as_str())
                .set("x", lx)
                .set("y", y)
                .set("font-family", cfg.legend.font_family.as_str())
                .set("font-size", font_size.clone())
                .set("font-weight", "bold")
                .set("fill", cfg.legend.header_color.as_str())
                .set("dominant-baseline", "middle"),
        );
        y += line_h;
        for (color, label) in [
            (
                cfg.node.active_fill.as_str(),
                cfg.legend.active_label.as_str(),
            ),
            (
                cfg.node.inactive_fill.as_str(),
                cfg.legend.inactive_label.as_str(),
            ),
        ] {
            document = document.add(
                Circle::new()
                    .set("cx", lx + cfg.legend.swatch_radius + 1.0)
                    .set("cy", y)
                    .set("r", cfg.legend.swatch_radius)
                    .set("fill", color)
                    .set("stroke", cfg.legend.swatch_stroke.as_str())
                    .set("stroke-width", cfg.legend.swatch_stroke_width),
            );
            document = document.add(
                Text::new(label)
                    .set("x", lx + 2.0 * cfg.legend.swatch_radius + 5.0)
                    .set("y", y)
                    .set("font-family", cfg.legend.font_family.as_str())
                    .set("font-size", font_size.clone())
                    .set("fill", cfg.legend.value_color.as_str())
                    .set("dominant-baseline", "middle"),
            );
            y += line_h;
        }

        // -- Правая легенда: порты и переменные -------------------------------
        let mut port_sections: Vec<(&str, &[(String, String)])> = vec![];
        if !leg.in_ports.is_empty() {
            port_sections.push((cfg.legend.in_header.as_str(), &leg.in_ports));
        }
        if !leg.out_ports.is_empty() {
            port_sections.push((cfg.legend.out_header.as_str(), &leg.out_ports));
        }
        if !leg.inout_ports.is_empty() {
            port_sections.push((cfg.legend.inout_header.as_str(), &leg.inout_ports));
        }
        if !leg.vars.is_empty() {
            port_sections.push((cfg.legend.vars_header.as_str(), &leg.vars));
        }

        if !port_sections.is_empty() {
            let right_lines: usize = port_sections.iter().map(|(_, v)| 1 + v.len()).sum();
            let right_box_h = right_lines as f64 * line_h + 2.0 * pad;
            let rx_box = leg_w + cfg.canvas.width + pad; // x левого края правого блока
            let rx = rx_box + pad; // x контента внутри блока

            document = document.add(
                Rectangle::new()
                    .set("x", rx_box)
                    .set("y", pad)
                    .set("width", inner_w)
                    .set("height", right_box_h)
                    .set("fill", cfg.legend.bg_fill.as_str())
                    .set("stroke", cfg.legend.bg_stroke.as_str())
                    .set("stroke-width", cfg.legend.bg_stroke_width)
                    .set("rx", cfg.legend.bg_radius)
                    .set("ry", cfg.legend.bg_radius),
            );

            let mut y_r = pad + line_h;
            for (header, entries) in &port_sections {
                document = document.add(
                    Text::new(*header)
                        .set("x", rx)
                        .set("y", y_r)
                        .set("font-family", cfg.legend.font_family.as_str())
                        .set("font-size", font_size.clone())
                        .set("font-weight", "bold")
                        .set("fill", cfg.legend.header_color.as_str())
                        .set("dominant-baseline", "middle"),
                );
                y_r += line_h;
                for (name, value) in *entries {
                    document = document.add(
                        Text::new(format!("{name}={value}"))
                            .set("x", rx)
                            .set("y", y_r)
                            .set("font-family", cfg.legend.font_family.as_str())
                            .set("font-size", font_size.clone())
                            .set("fill", cfg.legend.value_color.as_str())
                            .set("dominant-baseline", "middle"),
                    );
                    y_r += line_h;
                }
            }
        }
    }

    document
}

// -- Модуль: граф из Unit и оптимизация раскладки -----------------------------

/// Функции построения ориентированного графа из [`Unit`] и размещения узлов.
///
/// Внутренний модуль скрывает детали алгоритма имитации отжига и обхода дерева Unit.
/// Снаружи доступны только [`unit_to_graph`] и [`calculate_graph`].
pub(super) mod graph;

// -- Тесты viewport ------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::unit::{Unit, UnitKind};

    // -- rect_overlaps_circle --------------------------------------------------

    #[test]
    fn test_rect_circle_overlap_center_inside() {
        // Круг (5,5) r=3 полностью внутри прямоугольника (0,0)-(10,10)
        assert!(rect_overlaps_circle(0.0, 0.0, 10.0, 10.0, 5.0, 5.0, 3.0));
    }

    #[test]
    fn test_rect_circle_no_overlap_far_away() {
        assert!(!rect_overlaps_circle(0.0, 0.0, 2.0, 2.0, 10.0, 10.0, 1.0));
    }

    #[test]
    fn test_rect_circle_touching_edge_not_overlap() {
        // Ближайшая точка (5,2.5), расстояние = 2.0, r = 2.0 -> строгое неравенство не
        // выполняется
        assert!(!rect_overlaps_circle(0.0, 0.0, 5.0, 5.0, 7.0, 2.5, 2.0));
    }

    #[test]
    fn test_rect_circle_partially_inside() {
        // Круг центром на краю прямоугольника: (10, 5) r=2, ближайшая точка (10,5), d=0
        // < r
        assert!(rect_overlaps_circle(0.0, 0.0, 10.0, 10.0, 10.0, 5.0, 2.0));
    }

    // -- rects_intersection_area -----------------------------------------------

    #[test]
    fn test_rects_area_overlapping() {
        // (0,0,4,4) ∩ (2,2,6,6) = (2,2,4,4) -> площадь 4
        let area = rects_intersection_area(0.0, 0.0, 4.0, 4.0, 2.0, 2.0, 6.0, 6.0);
        assert!((area - 4.0).abs() < 1e-10, "ожидалось 4.0, получено {area}");
    }

    #[test]
    fn test_rects_area_no_overlap() {
        let area = rects_intersection_area(0.0, 0.0, 1.0, 1.0, 5.0, 5.0, 6.0, 6.0);
        assert_eq!(area, 0.0);
    }

    #[test]
    fn test_rects_area_touching_edge_is_zero() {
        // Касание по вертикальному ребру: (0,0,2,2) и (2,0,4,2)
        let area = rects_intersection_area(0.0, 0.0, 2.0, 2.0, 2.0, 0.0, 4.0, 2.0);
        assert_eq!(area, 0.0);
    }

    #[test]
    fn test_rects_area_one_inside_other() {
        // (1,1,3,3) полностью внутри (0,0,4,4) -> площадь = 4
        let area = rects_intersection_area(0.0, 0.0, 4.0, 4.0, 1.0, 1.0, 3.0, 3.0);
        assert!((area - 4.0).abs() < 1e-10);
    }

    // -- create_viewport -------------------------------------------------------

    #[test]
    fn test_create_viewport_empty_unit_returns_ok() {
        let result = create_viewport(
            &Unit::default(),
            crate::graphics_config::GraphicsConfig::default(),
            &[],
            None,
        );
        assert!(result.is_ok(), "ожидался Ok, получено Err");
    }

    #[test]
    fn test_create_viewport_node_unit_returns_ok() {
        use std::collections::HashMap;
        let pred =
            crate::unit::Predicate::new("cond", |_: &mut dyn crate::context::Context| Ok(true));
        let mut transitions = HashMap::new();
        transitions.insert("A".to_string(), vec![("B".to_string(), pred)]);
        transitions.insert("B".to_string(), vec![]);
        let unit = Unit::from_kind(UnitKind::Node {
            time_ns: 0,
            ticks_in_state: 0,
            state_entered_ns: 0,
            model_name: None,
            entered_initial: false,
            exited_terminal: false,
            context: None,
            executions: HashMap::new(),
            state: Some("A".to_string()),
            state_transitions: transitions,
            state_executions: HashMap::new(),
            state_every: HashMap::new(),
            state_impls: HashMap::new(),
            every_consumed: Vec::new(),
            guards: Default::default(),
            invariant_violations: Vec::new(),
            last_transition: None,
        });
        let result = create_viewport(
            &unit,
            crate::graphics_config::GraphicsConfig::default(),
            &["A"],
            None,
        );
        assert!(result.is_ok());
    }
}

#[cfg(test)]
mod test_highlight {
    use super::*;
    use std::collections::HashMap;

    fn make_node(t: HashMap<String, Vec<(String, crate::unit::Predicate)>>) -> Unit {
        Unit::from_kind(crate::unit::UnitKind::Node {
            time_ns: 0,
            ticks_in_state: 0,
            state_entered_ns: 0,
            model_name: None,
            entered_initial: false,
            exited_terminal: false,
            context: None,
            executions: HashMap::new(),
            state: Some("Off".to_string()),
            state_transitions: t,
            state_executions: HashMap::new(),
            state_every: HashMap::new(),
            state_impls: HashMap::new(),
            every_consumed: Vec::new(),
            guards: Default::default(),
            invariant_violations: Vec::new(),
            last_transition: None,
        })
    }

    #[test]
    fn test_highlight_frame_contains_orange_bg() {
        let pred =
            crate::unit::Predicate::new("btn", |_: &mut dyn crate::context::Context| Ok(true));
        let mut t = HashMap::new();
        t.insert("Off".to_string(), vec![("On".to_string(), pred)]);
        t.insert("On".to_string(), vec![]);
        let unit = make_node(t);
        let layout = compute_layout(&unit, &crate::graphics_config::GraphicsConfig::default());
        let fi = layout.node_labels.iter().position(|n| n == "Off").unwrap();
        let ti = layout.node_labels.iter().position(|n| n == "On").unwrap();
        let vp = render_from_layout(
            &layout,
            &crate::graphics_config::GraphicsConfig::default(),
            &["On"],
            None,
            None,
            Some((fi, ti)),
            false, // GIF-режим - оранжевая шапка должна быть
        )
        .unwrap();
        let Viewport::SVG(doc) = vp;
        let svg_str = doc.to_string();
        // Усечение по границам символов (`&svg_str[..500]` паникует на короткой/
        // многобайтовой строке). Запись в `/tmp` убрана: нет на Windows.
        let head: String = svg_str.chars().take(500).collect();
        assert!(
            svg_str.contains("FFF3DC") || svg_str.contains("FF8C00"),
            "в SVG нет оранжевых цветов подсветки: {head}"
        );
    }
}

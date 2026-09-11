//! Построение ориентированного графа из [`Unit`] и оптимизация раскладки узлов.
//!
//! Подмодуль самодостаточен: он зависит только от `super::Positions` и типов крейта.
//! Публичные пути - `viewport::graph::{unit_to_graph, calculate_graph}`.

use petgraph::graph::{Graph, NodeIndex};
use petgraph::visit::EdgeRef;
use rand::rngs::StdRng;
use rand::{RngExt, SeedableRng};
use std::collections::HashMap;

/// Зерно генератора раскладки.
///
/// Раскладка ищется имитацией отжига - по существу случайным поиском, - и с генератором
/// от системного источника один и тот же прогон давал разный кадр. Для эталона это
/// недопустимо: трасса воспроизводима, а картинка рядом с ней - нет; сверить два
/// прогона по кадрам было нельзя, и всякий снимок вывода мигал бы. Зерно постоянно,
/// поэтому кадр - функция модели и настроек холста, а не времени запуска.
///
/// Качество раскладки от постоянного зерна не страдает: отжиг ищет минимум энергии, а
/// не "разные" картинки.
const LAYOUT_SEED: u64 = 0x7461_6b74_5f30_3533; // "takt_053" -

use super::Positions;
use crate::graphics_config::GraphicsConfig;
use crate::unit::{Unit, UnitKind};

// -- Преобразование Unit -> Graph -------------------------------------------

/// Строит ориентированный граф petgraph из дерева [`Unit`].
///
/// - `Unit::None` порождает пустой граф.
/// - `Unit::Node`: каждый ключ `state_transitions` становится узлом; каждый
///   переход `(from, to, _pred)` - ориентированным ребром.
/// - `Unit::Parallel` и `Unit::Sequential`: узлы и рёбра дочерних Unit
///   добавляются в общий граф рекурсивно.
///
/// Метки узлов - имена состояний (`String`); метки рёбер - пустые строки (предикаты не
/// имеют строкового представления в модели симуляции).
pub(super) fn unit_to_graph(unit: &Unit) -> Graph<String, String> {
    let mut graph = Graph::new();
    populate_graph(unit, &mut graph);
    graph
}

/// Рекурсивно добавляет состояния и переходы из `unit` в `graph`.
///
/// Вызывается из [`unit_to_graph`]. Для `Unit::Node` сначала создаются все узлы, затем
/// рёбра - чтобы переходы к ещё не добавленным состояниям не терялись. Если целевое
/// состояние перехода отсутствует в `state_transitions`, ребро пропускается.
///
/// Метка ребра - `String` из кортежа `Predicate = Rc<(String, dyn Fn)>`, то есть имя
/// предиката (`pred.0`).
fn populate_graph(unit: &Unit, graph: &mut Graph<String, String>) {
    match unit.kind() {
        UnitKind::None => {}
        UnitKind::Node {
            state_transitions, ..
        } => {
            // Порядок узлов и рёбер берётся отсортированным, а не обходом `HashMap`:
            // раскладка ищется отжигом от начального размещения, и порядок узлов входит
            // в результат. Сортировка живёт здесь, в раскладке: порядок проверки
            // переходов - дело исполнения, и его она не касается.
            let mut names: Vec<&String> = state_transitions.keys().collect();
            names.sort();
            let mut node_map: HashMap<String, NodeIndex> = HashMap::new();
            for name in &names {
                let idx = graph.add_node((*name).clone());
                node_map.insert((*name).clone(), idx);
            }
            for from in &names {
                let Some(transitions) = state_transitions.get(*from) else {
                    continue;
                };
                for (to, pred) in transitions {
                    if let (Some(&fi), Some(&ti)) = (node_map.get(*from), node_map.get(to)) {
                        graph.add_edge(fi, ti, pred.name.clone());
                    }
                }
            }
        }
        UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => {
            for u in units {
                populate_graph(&u.borrow(), graph);
            }
        }
    }
}

// -- Вычисление позиций ----------------------------------------------------

/// Извлекает узлы и рёбра из графа, генерирует начальное случайное размещение и
/// оптимизирует его методом имитации отжига.
///
/// Возвращает:
/// - `Vec<String>` - метки узлов в порядке индексации (0..N).
/// - `Vec<(usize, usize, String)>` - рёбра как `(источник, цель, метка)`.
/// - [`Positions`] - оптимизированные координаты центров узлов.
///
/// Позиции гарантированно находятся в пределах `[cfg.radius, cfg.width - cfg.radius]` x
/// `[cfg.radius, cfg.height - cfg.radius]`.
pub(super) fn calculate_graph(
    graph: Graph<String, String>,
    cfg: &GraphicsConfig,
) -> (Vec<String>, Vec<(usize, usize, String)>, Positions) {
    let nodes: Vec<(NodeIndex, String)> = graph
        .node_indices()
        .map(|idx| (idx, graph[idx].clone()))
        .collect();

    let node_labels: Vec<String> = nodes.iter().map(|(_, label)| label.clone()).collect();

    let mut index_map: HashMap<NodeIndex, usize> = HashMap::new();
    for (i, (node_idx, _)) in nodes.iter().enumerate() {
        index_map.insert(*node_idx, i);
    }

    let mut edges_vec: Vec<(usize, usize, String)> = Vec::new();
    for edge in graph.edge_references() {
        let u = index_map[&edge.source()];
        let v = index_map[&edge.target()];
        edges_vec.push((u, v, edge.weight().clone()));
    }

    let n = node_labels.len();
    let mut rng = StdRng::seed_from_u64(LAYOUT_SEED);
    let mut positions: Positions = (0..n)
        .map(|_| {
            (
                rng.random_range(cfg.node.radius..cfg.canvas.width - cfg.node.radius),
                rng.random_range(cfg.node.radius..cfg.canvas.height - cfg.node.radius),
            )
        })
        .collect();

    let edges_for_layout: Vec<(usize, usize)> = edges_vec.iter().map(|&(u, v, _)| (u, v)).collect();
    optimize_layout(&mut positions, &edges_for_layout, cfg, &mut rng);

    (node_labels, edges_vec, positions)
}

// -- Оптимизация размещения ------------------------------------------------

/// Оптимизирует позиции узлов методом имитации отжига (simulated annealing).
///
/// На каждом шаге случайно выбирается узел и смещается на гауссовский вектор с
/// дисперсией, пропорциональной текущей температуре. Новое состояние принимается, если
/// оно снижает энергию или с вероятностью `exp(-ΔE / T)`.
///
/// Функция энергии [`delta_energy`] штрафует за:
/// 1. Перекрытие кружков узлов.
/// 2. Большую суммарную длину рёбер.
/// 3. Пересечения рёбер.
///
/// Для пустого графа функция завершается немедленно.
fn optimize_layout(
    positions: &mut Positions,
    edges: &[(usize, usize)],
    cfg: &GraphicsConfig,
    rng: &mut StdRng,
) {
    let n = positions.len();
    if n == 0 {
        return;
    }

    // Предвычисляем список рёбер, инцидентных каждому узлу, чтобы не перебирать все
    // рёбра при каждом вычислении delta_energy.
    let mut incident: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (ei, &(u, v)) in edges.iter().enumerate() {
        incident[u].push(ei);
        incident[v].push(ei);
    }

    let radius = cfg.node.radius;
    let min_distance = cfg.node.min_distance;
    let width = cfg.canvas.width;
    let height = cfg.canvas.height;
    let w_overlap = cfg.layout.weight_overlap;
    let w_length = cfg.layout.weight_length;
    let w_cross = cfg.layout.weight_cross;
    let cross_penalty = cfg.layout.cross_penalty;

    let t_start = cfg.layout.temperature_start;
    let t_end = cfg.layout.temperature_end;
    let alpha = cfg.layout.cooling_alpha;
    let iterations_per_t = cfg.layout.iterations_per_temperature_factor * n;
    let mut t = t_start;

    while t > t_end {
        for _ in 0..iterations_per_t {
            let idx = rng.random_range(0..n);
            let old_pos = positions[idx];

            let sigma = 15.0 * (t / t_start).max(0.01);
            let dx: f64 = rng.sample::<f64, _>(rand_distr::StandardNormal) * sigma;
            let dy: f64 = rng.sample::<f64, _>(rand_distr::StandardNormal) * sigma;
            let new_pos = clamp_to_canvas(old_pos.0 + dx, old_pos.1 + dy, radius, width, height);

            if (new_pos.0 - old_pos.0).abs() < 1e-6 && (new_pos.1 - old_pos.1).abs() < 1e-6 {
                continue; // смещение подавлено зажимом - пропускаем
            }

            // Инкрементальная дельта-энергия: пересчитываем только вклад перемещённого
            // узла - O(n) вместо O(n²) полного пересчёта.
            let delta = delta_energy(
                positions,
                idx,
                old_pos,
                new_pos,
                edges,
                &incident,
                radius,
                min_distance,
                w_overlap,
                w_length,
                w_cross,
                cross_penalty,
            );

            if delta < 0.0 || rng.random::<f64>() < (-delta / t).exp() {
                positions[idx] = new_pos;
            }
        }
        t *= alpha;
    }
}

/// Вычисляет изменение энергии при перемещении узла `idx` из `old_pos` в `new_pos`.
///
/// Работает за O(n + degree x m) вместо O(n² + m²) полного пересчёта: перебираются
/// только пары, в которых участвует перемещённый узел.
#[allow(clippy::too_many_arguments)]
fn delta_energy(
    positions: &Positions,
    idx: usize,
    old_pos: (f64, f64),
    new_pos: (f64, f64),
    edges: &[(usize, usize)],
    incident: &[Vec<usize>],
    radius: f64,
    min_distance: f64,
    w_overlap: f64,
    w_length: f64,
    w_cross: f64,
    cross_penalty: f64,
) -> f64 {
    let min_d = 2.0 * radius + min_distance;
    let mut delta = 0.0;

    // Перекрытие: пары (idx, j) для всех j != idx
    for (j, &pj) in positions.iter().enumerate() {
        if j == idx {
            continue;
        }
        let old_d = dist(old_pos, pj);
        if old_d < min_d {
            delta -= w_overlap * (min_d - old_d).powi(2);
        }
        let new_d = dist(new_pos, pj);
        if new_d < min_d {
            delta += w_overlap * (min_d - new_d).powi(2);
        }
    }

    // Длина рёбер: только рёбра, инцидентные idx
    for &ei in &incident[idx] {
        let (u, v) = edges[ei];
        let other = if u == idx { positions[v] } else { positions[u] };
        delta -= w_length * dist(old_pos, other).powi(2);
        delta += w_length * dist(new_pos, other).powi(2);
    }

    // Пересечения: рёбра, инцидентные idx, против остальных рёбер
    for &ei in &incident[idx] {
        let (u1, v1) = edges[ei];
        let other_node = if u1 == idx { v1 } else { u1 };
        let p_other = positions[other_node];

        for (ej, &(u2, v2)) in edges.iter().enumerate() {
            if ej == ei {
                continue;
            }
            // Пропускаем рёбра, разделяющие вершину с ei
            if u2 == u1 || u2 == v1 || v2 == u1 || v2 == v1 {
                continue;
            }
            let q1 = positions[u2];
            let q2 = positions[v2];
            if segments_intersect(old_pos, p_other, q1, q2) {
                delta -= w_cross * cross_penalty;
            }
            if segments_intersect(new_pos, p_other, q1, q2) {
                delta += w_cross * cross_penalty;
            }
        }
    }

    delta
}

/// Зажимает координаты точки так, чтобы кружок радиуса `radius` не выходил за холст.
///
/// Допустимая область для центра: `[radius, width - radius] x [radius, height -
/// radius]`.
fn clamp_to_canvas(x: f64, y: f64, radius: f64, width: f64, height: f64) -> (f64, f64) {
    (
        x.clamp(radius, width - radius),
        y.clamp(radius, height - radius),
    )
}

/// Возвращает евклидово расстояние между двумя точками на плоскости.
fn dist(a: (f64, f64), b: (f64, f64)) -> f64 {
    ((a.0 - b.0).powi(2) + (a.1 - b.1).powi(2)).sqrt()
}

/// Проверяет, пересекаются ли два отрезка `p1-p2` и `p3-p4` в общей точке (не на
/// конце).
///
/// Алгоритм ориентированных площадей: отрезки пересекаются тогда и только тогда, когда
/// конечные точки каждого из них лежат по разные стороны от другого отрезка.
/// Коллинеарные случаи (пересечение на вершине) не обнаруживаются - это приемлемо для
/// задачи минимизации пересечений рёбер в визуализации графа.
fn segments_intersect(p1: (f64, f64), p2: (f64, f64), p3: (f64, f64), p4: (f64, f64)) -> bool {
    fn orient(a: (f64, f64), b: (f64, f64), c: (f64, f64)) -> f64 {
        (b.0 - a.0) * (c.1 - a.1) - (b.1 - a.1) * (c.0 - a.0)
    }
    let o1 = orient(p1, p2, p3);
    let o2 = orient(p1, p2, p4);
    let o3 = orient(p3, p4, p1);
    let o4 = orient(p3, p4, p2);
    o1 * o2 < 0.0 && o3 * o4 < 0.0
}

// -- Тесты модуля graph ----------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use petgraph::graph::Graph;
    use std::collections::HashMap;

    #[test]
    fn test_dist_pythagorean_triple() {
        let d = dist((0.0, 0.0), (3.0, 4.0));
        assert!((d - 5.0).abs() < 1e-10, "ожидалось 5.0, получено {d}");
    }

    #[test]
    fn test_dist_same_point_is_zero() {
        assert!(dist((2.5, -1.0), (2.5, -1.0)).abs() < 1e-10);
    }

    #[test]
    fn test_dist_horizontal() {
        let d = dist((0.0, 0.0), (7.0, 0.0));
        assert!((d - 7.0).abs() < 1e-10);
    }

    #[test]
    fn test_segments_cross_at_center() {
        // Крест: (-1,0)-(1,0) и (0,-1)-(0,1)
        assert!(segments_intersect(
            (-1.0, 0.0),
            (1.0, 0.0),
            (0.0, -1.0),
            (0.0, 1.0),
        ));
    }

    #[test]
    fn test_segments_diagonal_cross() {
        // X-образное: (0,0)-(1,1) и (0,1)-(1,0)
        assert!(segments_intersect(
            (0.0, 0.0),
            (1.0, 1.0),
            (0.0, 1.0),
            (1.0, 0.0),
        ));
    }

    #[test]
    fn test_segments_parallel_no_cross() {
        assert!(!segments_intersect(
            (0.0, 0.0),
            (1.0, 0.0),
            (0.0, 1.0),
            (1.0, 1.0),
        ));
    }

    #[test]
    fn test_segments_t_shape_no_cross() {
        // T-образное: (1,0) лежит на первом отрезке, но коллинеарность не детектируется
        assert!(!segments_intersect(
            (0.0, 0.0),
            (2.0, 0.0),
            (1.0, 0.0),
            (1.0, 1.0),
        ));
    }

    #[test]
    fn test_segments_no_cross_far_apart() {
        assert!(!segments_intersect(
            (0.0, 0.0),
            (1.0, 0.0),
            (5.0, 5.0),
            (6.0, 6.0),
        ));
    }

    #[test]
    fn test_clamp_point_within_bounds_unchanged() {
        let (x, y) = clamp_to_canvas(400.0, 300.0, 25.0, 800.0, 600.0);
        assert_eq!(x, 400.0);
        assert_eq!(y, 300.0);
    }

    #[test]
    fn test_clamp_left_top_corner() {
        // Точка (0, 0) зажимается до (radius, radius) = (25, 25)
        let (x, y) = clamp_to_canvas(0.0, 0.0, 25.0, 800.0, 600.0);
        assert_eq!(x, 25.0);
        assert_eq!(y, 25.0);
    }

    #[test]
    fn test_clamp_right_bottom_corner() {
        // Точка (900, 700) зажимается до (775, 575)
        let (x, y) = clamp_to_canvas(900.0, 700.0, 25.0, 800.0, 600.0);
        assert_eq!(x, 775.0);
        assert_eq!(y, 575.0);
    }

    // -- unit_to_graph -----------------------------------------------------

    #[test]
    fn test_unit_to_graph_none_is_empty() {
        let g = unit_to_graph(&Unit::default());
        assert_eq!(g.node_count(), 0);
        assert_eq!(g.edge_count(), 0);
    }

    #[test]
    fn test_unit_to_graph_node_states_become_nodes() {
        let mut transitions = HashMap::new();
        transitions.insert("A".to_string(), vec![]);
        transitions.insert("B".to_string(), vec![]);
        let unit = make_node(transitions);
        let g = unit_to_graph(&unit);
        assert_eq!(g.node_count(), 2);
        assert_eq!(g.edge_count(), 0);
    }

    #[test]
    fn test_unit_to_graph_transitions_become_edges() {
        let pred =
            crate::unit::Predicate::new("is_ready", |_: &mut dyn crate::context::Context| Ok(true));
        let mut transitions = HashMap::new();
        transitions.insert("A".to_string(), vec![("B".to_string(), pred)]);
        transitions.insert("B".to_string(), vec![]);
        let unit = make_node(transitions);
        let g = unit_to_graph(&unit);
        assert_eq!(g.node_count(), 2);
        assert_eq!(g.edge_count(), 1);
        let label = g.edge_references().next().unwrap().weight();
        assert_eq!(
            label, "is_ready",
            "метка ребра должна совпадать с именем предиката"
        );
    }

    #[test]
    fn test_unit_to_graph_parallel_merges_children() {
        use std::cell::RefCell;
        use std::rc::Rc;
        let child1 = {
            let mut t = HashMap::new();
            t.insert("A".to_string(), vec![]);
            make_node(t)
        };
        let child2 = {
            let mut t = HashMap::new();
            t.insert("B".to_string(), vec![]);
            make_node(t)
        };
        let unit = Unit::from_kind(UnitKind::Parallel {
            units: vec![Rc::new(RefCell::new(child1)), Rc::new(RefCell::new(child2))],
            executions: HashMap::new(),
        });
        let g = unit_to_graph(&unit);
        assert_eq!(g.node_count(), 2);
    }

    #[test]
    fn test_unit_to_graph_sequential_merges_children() {
        use std::cell::RefCell;
        use std::rc::Rc;
        let child1 = {
            let mut t = HashMap::new();
            t.insert("X".to_string(), vec![]);
            make_node(t)
        };
        let child2 = {
            let mut t = HashMap::new();
            t.insert("Y".to_string(), vec![]);
            t.insert("Z".to_string(), vec![]);
            make_node(t)
        };
        let unit = Unit::from_kind(UnitKind::Sequential {
            units: vec![Rc::new(RefCell::new(child1)), Rc::new(RefCell::new(child2))],
            index: 0,
            executions: HashMap::new(),
        });
        let g = unit_to_graph(&unit);
        assert_eq!(g.node_count(), 3);
    }

    // -- воспроизводимость раскладки ---------------------------

    /// Раскладка воспроизводима: одна модель - одни и те же координаты.
    ///
    /// Тест ловит возврат `rand::rng()`.
    #[test]
    fn layout_is_reproducible() {
        let cfg = crate::graphics_config::GraphicsConfig::default();
        let build = || {
            let mut g: Graph<String, String> = Graph::new();
            let a = g.add_node("A".to_string());
            let b = g.add_node("B".to_string());
            let c = g.add_node("C".to_string());
            g.add_edge(a, b, String::new());
            g.add_edge(b, c, String::new());
            g.add_edge(c, a, String::new());
            g
        };
        let (labels, edges, first) = calculate_graph(build(), &cfg);
        for run in 1..4 {
            let (l, e, positions) = calculate_graph(build(), &cfg);
            assert_eq!(l, labels, "прогон {run}: порядок меток уехал");
            assert_eq!(e, edges, "прогон {run}: порядок рёбер уехал");
            assert_eq!(
                positions, first,
                "прогон {run}: раскладка не воспроизвелась"
            );
        }
    }

    /// Порядок узлов не зависит от порядка обхода `HashMap` переходов.
    ///
    /// Ключи `state_transitions` обходятся отсортированными: иначе раскладка зависела
    /// бы от случайного затравочного значения `RandomState`, и воспроизводимость
    /// держалась бы только внутри одного процесса.
    #[test]
    fn node_order_does_not_depend_on_map_order() {
        let names = ["Zeta", "Alpha", "Mu"];
        let mut first: Option<Vec<String>> = None;
        for shift in 0..names.len() {
            let mut t: HashMap<String, Vec<(String, crate::unit::Predicate)>> = HashMap::new();
            for i in 0..names.len() {
                t.insert(names[(i + shift) % names.len()].to_string(), vec![]);
            }
            let graph = unit_to_graph(&make_node(t));
            let labels: Vec<String> = graph.node_indices().map(|idx| graph[idx].clone()).collect();
            match &first {
                None => first = Some(labels),
                Some(expected) => assert_eq!(
                    &labels, expected,
                    "порядок узлов зависит от порядка вставки в карту"
                ),
            }
        }
        assert_eq!(
            first.unwrap(),
            vec!["Alpha".to_string(), "Mu".to_string(), "Zeta".to_string()]
        );
    }

    // -- calculate_graph ---------------------------------------------------

    #[test]
    fn test_calculate_graph_empty_returns_empty() {
        let cfg = crate::graphics_config::GraphicsConfig::default();
        let g: Graph<String, String> = Graph::new();
        let (labels, edges, positions) = calculate_graph(g, &cfg);
        assert!(labels.is_empty());
        assert!(edges.is_empty());
        assert!(positions.is_empty());
    }

    #[test]
    fn test_calculate_graph_counts_match() {
        let cfg = crate::graphics_config::GraphicsConfig::default();
        let mut g: Graph<String, String> = Graph::new();
        let a = g.add_node("A".to_string());
        let b = g.add_node("B".to_string());
        g.add_edge(a, b, String::new());
        let (labels, edges, positions) = calculate_graph(g, &cfg);
        assert_eq!(labels.len(), 2);
        assert_eq!(edges.len(), 1);
        assert_eq!(positions.len(), 2);
    }

    #[test]
    fn test_calculate_graph_positions_within_bounds() {
        let cfg = crate::graphics_config::GraphicsConfig::default();
        let mut g: Graph<String, String> = Graph::new();
        for name in ["A", "B", "C", "D"] {
            g.add_node(name.to_string());
        }
        let (_, _, positions) = calculate_graph(g, &cfg);
        for &(x, y) in &positions {
            assert!(
                x >= cfg.node.radius && x <= cfg.canvas.width - cfg.node.radius,
                "x={x} вне холста"
            );
            assert!(
                y >= cfg.node.radius && y <= cfg.canvas.height - cfg.node.radius,
                "y={y} вне холста"
            );
        }
    }

    #[test]
    fn test_calculate_graph_labels_contain_all_nodes() {
        let cfg = crate::graphics_config::GraphicsConfig::default();
        let mut g: Graph<String, String> = Graph::new();
        g.add_node("Alpha".to_string());
        g.add_node("Beta".to_string());
        let (labels, _, _) = calculate_graph(g, &cfg);
        assert!(labels.contains(&"Alpha".to_string()));
        assert!(labels.contains(&"Beta".to_string()));
    }

    // -- Вспомогательные конструкторы --------------------------------------

    fn make_node(
        state_transitions: HashMap<String, Vec<(String, crate::unit::Predicate)>>,
    ) -> Unit {
        Unit::from_kind(UnitKind::Node {
            time_ns: 0,
            ticks_in_state: 0,
            state_entered_ns: 0,
            model_name: None,
            entered_initial: false,
            exited_terminal: false,
            path: Vec::new(),
            context: None,
            executions: HashMap::new(),
            state: None,
            state_transitions,
            state_executions: HashMap::new(),
            state_every: HashMap::new(),
            state_impls: HashMap::new(),
            every_consumed: Vec::new(),
            guards: Default::default(),
            invariant_violations: Vec::new(),
            last_transition: None,
        })
    }
}

//! Геометрия схемы: точки привязки, трассировка рёбер, пересечения, места знаков,
//! размер листа, рамки листа композиции.
//!
//! Это та же геометрия, что у холста страницы (`web/static/scheme-geometry.js`),
//! функция в функцию: картинка обязана совпадать с тем, что автор видит на холсте,
//! а совпадение держит сверка паритета на корпусе. Автораскладки здесь нет: чертёж
//! читает готовые координаты файла раскладки, а раскладывает только страница.

use std::collections::{BTreeMap, BTreeSet};

use crate::js::{millionth, num, round, sign, tenth};

/// Точка листа.
pub type Point = [f64; 2];

/// Радиус круга узла: половина `--node-d`.
pub const R: f64 = 24.0;
/// Сторона квадрата композиции: `--comp-side`.
pub const SIDE: f64 = 144.0;
/// Шаг привязки к сетке: `--gap`.
pub const SNAP: f64 = 8.0;
/// Радиус мостика на пересечении линий.
pub const HOP: f64 = 4.0;
/// Радиус скругления углов ломаной: `--radius-sm`.
pub const CORNER: f64 = 5.0;
/// Отступ знака условия от узла при местах "начало" и "конец".
pub const MARK_OFF: f64 = SNAP * 3.0;
/// Поле листа вокруг рисунка.
pub const MARGIN: f64 = SNAP * 9.0;
/// Шаг автораскладки - нижняя граница размера листа.
pub const COL: f64 = SNAP * 18.0;
pub const ROW: f64 = SNAP * 15.0;
/// Число точек привязки на рамке узла: через 22,5°.
pub const PORTS: u32 = 16;
/// Точка стрелки начального состояния по умолчанию: слева.
pub const ENTRY_PORT: u32 = 8;
/// Отступ рамки скобок и параллели от своих шагов.
pub const FRAME_PAD: f64 = SNAP * 3.0;
/// Запас вокруг предмета листа со стороны начала: полширины знака с подложкой.
const REACH: f64 = SNAP * 3.0;

/// Узел листа для геометрии: имя, центр и форма.
#[derive(Debug, Clone, PartialEq)]
pub struct Shape {
    pub name: String,
    pub x: f64,
    pub y: f64,
    /// Квадрат композиции либо круг состояния.
    pub square: bool,
}

impl Shape {
    fn half(&self) -> f64 {
        if self.square { SIDE / 2.0 } else { R }
    }

    fn center(&self) -> Point {
        [self.x, self.y]
    }
}

/// Точка привязки `index` узла: на окружности круга либо на рамке квадрата по лучу из
/// центра; счёт от направления "вправо" по часовой стрелке экрана.
pub fn port_point(node: &Shape, index: u32) -> Point {
    let angle = f64::from(index) * 2.0 * std::f64::consts::PI / f64::from(PORTS);
    let (sin, cos) = angle.sin_cos();
    let h = node.half();
    let reach = if node.square {
        h / cos.abs().max(sin.abs())
    } else {
        h
    } + 2.0;
    [tenth(node.x + cos * reach), tenth(node.y + sin * reach)]
}

/// Направление на точку `to` в шагах точек привязки: дробное, от 0 до 16.
fn bearing(node: &Shape, to: Point) -> f64 {
    let turn = (to[1] - node.y).atan2(to[0] - node.x) / (2.0 * std::f64::consts::PI);
    let ports = f64::from(PORTS);
    millionth((((turn * ports) % ports) + ports) % ports)
}

/// Номер точки привязки, ближайшей к направлению на точку `to`.
pub fn port_toward(node: &Shape, to: Point) -> u32 {
    (round(bearing(node, to)) as u32) % PORTS
}

/// Расстояние по кругу между точкой `port` и направлением `at`, в шагах.
fn turn_gap(port: f64, at: f64) -> f64 {
    let ports = f64::from(PORTS);
    let d = (port - at).abs() % ports;
    millionth(d.min(ports - d))
}

/// Конец ребра, ждущий точку привязки.
pub struct End<'a> {
    pub node: &'a Shape,
    pub toward: Point,
    pub far: Point,
}

/// Раздаёт концам рёбер точки привязки: каждый конец берёт ближайшую к своему ходу
/// свободную точку своего узла (правило - у `assignPorts` холста).
pub fn assign_ports(ends: &[End<'_>], taken: &mut BTreeMap<String, BTreeSet<u32>>) -> Vec<u32> {
    let wanted: Vec<f64> = ends
        .iter()
        .map(|end| bearing(end.node, end.toward))
        .collect();
    let aim: Vec<f64> = ends.iter().map(|end| bearing(end.node, end.far)).collect();
    let miss = |i: usize| turn_gap(f64::from((round(wanted[i]) as u32) % PORTS), wanted[i]);
    let bend = |i: usize| turn_gap(wanted[i], aim[i]);
    let mut order: Vec<usize> = (0..ends.len()).collect();
    order.sort_by(|&i, &j| {
        miss(i)
            .total_cmp(&miss(j))
            .then(bend(i).total_cmp(&bend(j)))
            .then(i.cmp(&j))
    });
    let mut out = vec![0; ends.len()];
    for i in order {
        let used = taken.entry(ends[i].node.name.clone()).or_default();
        let score =
            |port: u32| turn_gap(f64::from(port), wanted[i]) + turn_gap(f64::from(port), aim[i]);
        let mut free: Vec<u32> = (0..PORTS).filter(|port| !used.contains(port)).collect();
        free.sort_by(|&p, &q| {
            score(p)
                .total_cmp(&score(q))
                .then(
                    turn_gap(f64::from(p), wanted[i]).total_cmp(&turn_gap(f64::from(q), wanted[i])),
                )
                .then(p.cmp(&q))
        });
        let port = free
            .first()
            .copied()
            .unwrap_or((round(wanted[i]) as u32) % PORTS);
        used.insert(port);
        out[i] = port;
    }
    out
}

/// Ход ребра без привязки: центры узлов и изломы; без изломов - один угол.
fn course(from: &Shape, to: &Shape, points: &[Point]) -> Vec<Point> {
    if points.is_empty() && from.x != to.x && from.y != to.y {
        return vec![from.center(), [from.x, to.y], to.center()];
    }
    let mut pts = vec![from.center()];
    pts.extend_from_slice(points);
    pts.push(to.center());
    pts
}

/// Ребро листа для трассировки.
pub struct EdgeInput<'a> {
    pub from: &'a str,
    pub to: &'a str,
    /// Изломы автора.
    pub points: &'a [Point],
    /// Концы, закреплённые автором за точками привязки.
    pub end_from: Option<u32>,
    pub end_to: Option<u32>,
}

/// Ломаные всех рёбер листа с раздачей точек привязки (правило - у `routeSheet`
/// холста). `None` - узла ребра нет на листе.
pub fn route_sheet(
    by_name: &BTreeMap<&str, &Shape>,
    edges: &[EdgeInput<'_>],
    reserved: &[(String, u32)],
) -> Vec<Option<Vec<Point>>> {
    let mut taken: BTreeMap<String, BTreeSet<u32>> = BTreeMap::new();
    let take = |taken: &mut BTreeMap<String, BTreeSet<u32>>, name: &str, port: u32| {
        taken.entry(name.to_string()).or_default().insert(port);
    };
    for (name, port) in reserved {
        take(&mut taken, name, *port);
    }
    for edge in edges {
        if edge.from == edge.to {
            continue;
        }
        if let Some(port) = edge.end_from {
            take(&mut taken, edge.from, port);
        }
        if let Some(port) = edge.end_to {
            take(&mut taken, edge.to, port);
        }
    }
    let mut out: Vec<Option<Vec<Point>>> = edges.iter().map(|_| None).collect();
    let mut fixed: Vec<(usize, usize, &Shape, u32)> = Vec::new();
    let mut free_ends: Vec<(usize, usize, End<'_>)> = Vec::new();
    let mut corners = Vec::new();
    for (k, edge) in edges.iter().enumerate() {
        let (Some(from), Some(to)) = (by_name.get(edge.from), by_name.get(edge.to)) else {
            continue;
        };
        if edge.from == edge.to {
            let pts = route(from, from, edge.points);
            take(&mut taken, &from.name, port_toward(from, pts[0]));
            take(
                &mut taken,
                &from.name,
                port_toward(from, pts[pts.len() - 1]),
            );
            out[k] = Some(pts);
            continue;
        }
        let pts = course(from, to, edge.points);
        let last = pts.len() - 1;
        if edge.points.is_empty() && pts.len() == 3 {
            corners.push(k);
        }
        match edge.end_from {
            Some(port) => fixed.push((k, 0, from, port)),
            None => free_ends.push((
                k,
                0,
                End {
                    node: from,
                    toward: pts[1],
                    far: to.center(),
                },
            )),
        }
        match edge.end_to {
            Some(port) => fixed.push((k, last, to, port)),
            None => free_ends.push((
                k,
                last,
                End {
                    node: to,
                    toward: pts[last - 1],
                    far: from.center(),
                },
            )),
        }
        out[k] = Some(pts);
    }
    for (k, at, node, port) in fixed {
        if let Some(pts) = out[k].as_mut() {
            pts[at] = port_point(node, port);
        }
    }
    let ends: Vec<End<'_>> = free_ends
        .iter()
        .map(|(_, _, end)| End {
            node: end.node,
            toward: end.toward,
            far: end.far,
        })
        .collect();
    let ports = assign_ports(&ends, &mut taken);
    for ((k, at, end), port) in free_ends.iter().zip(ports) {
        if let Some(pts) = out[*k].as_mut() {
            pts[*at] = port_point(end.node, port);
        }
    }
    for k in corners {
        if let Some(pts) = out[k].as_mut() {
            pts[1] = [pts[0][0], pts[2][1]];
        }
    }
    out
}

/// Точка присоединения ребра к грани узла со стороны точки `to`.
fn anchor(node: &Shape, to: Point) -> Point {
    let dx = to[0] - node.x;
    let dy = to[1] - node.y;
    let h = node.half() + 2.0;
    if dx == 0.0 && dy == 0.0 {
        return [node.x + h, node.y];
    }
    if dx.abs() > dy.abs() {
        [node.x + sign(dx) * h, node.y]
    } else {
        [node.x, node.y + sign(dy) * h]
    }
}

/// Ломаная ребра: от грани источника через изломы к грани цели; самопереход - петля
/// у правого верхнего угла.
pub fn route(from: &Shape, to: &Shape, points: &[Point]) -> Vec<Point> {
    if from.name == to.name {
        let h = from.half();
        let x = from.x + h - 6.0;
        return vec![
            [x, from.y - h + 4.0],
            [x, from.y - h - 20.0],
            [x + 56.0, from.y - h - 20.0],
            [x + 56.0, from.y - 8.0],
            [from.x + h + 2.0, from.y - 8.0],
        ];
    }
    let mut pts = course(from, to, points);
    let last = pts.len() - 1;
    pts[0] = anchor(from, pts[1]);
    pts[last] = anchor(to, pts[last - 1]);
    pts
}

fn near(a: Point, b: Point) -> f64 {
    (a[0] - b[0]).hypot(a[1] - b[1])
}

fn cross(u: Point, v: Point) -> f64 {
    u[0] * v[1] - u[1] * v[0]
}

fn segments(pts: &[Point]) -> impl Iterator<Item = (Point, Point)> + '_ {
    pts.windows(2).map(|w| (w[0], w[1]))
}

/// Точки, где ломаная `mine` пересекает ломаные `others` (правило - у `crossings`).
pub fn crossings(mine: &[Point], others: &[Vec<Point>]) -> Vec<Point> {
    let mut out = Vec::new();
    for (a, b) in segments(mine) {
        let r = [b[0] - a[0], b[1] - a[1]];
        let len = r[0].hypot(r[1]);
        if len == 0.0 {
            continue;
        }
        for list in others {
            for (c, d) in segments(list) {
                let s = [d[0] - c[0], d[1] - c[1]];
                let other_len = s[0].hypot(s[1]);
                let denom = cross(r, s);
                if other_len == 0.0 || denom.abs() < 1e-9 * len * other_len {
                    continue;
                }
                let ac = [c[0] - a[0], c[1] - a[1]];
                let t = cross(ac, s) / denom;
                let u = cross(ac, r) / denom;
                let inside_mine = t * len > HOP + 2.0 && t * len < len - HOP - 2.0;
                let inside_other = u * other_len > 1.0 && u * other_len < other_len - 1.0;
                let p = [tenth(a[0] + r[0] * t), tenth(a[1] + r[1] * t)];
                if inside_mine
                    && inside_other
                    && near(p, mine[0]) > HOP * 2.0
                    && near(p, mine[mine.len() - 1]) > HOP * 2.0
                {
                    out.push(p);
                }
            }
        }
    }
    out
}

fn unit(v: Point) -> Point {
    let len = v[0].hypot(v[1]);
    let len = if len == 0.0 { 1.0 } else { len };
    [v[0] / len, v[1] / len]
}

fn tangent_at(pts: &[Point], i: usize) -> Point {
    let prev = pts[i.saturating_sub(1)];
    let next = pts[(i + 1).min(pts.len() - 1)];
    unit([next[0] - prev[0], next[1] - prev[1]])
}

fn curve_controls(pts: &[Point], i: usize) -> (Point, Point) {
    let (a, b) = (pts[i], pts[i + 1]);
    let reach = (b[0] - a[0]).hypot(b[1] - a[1]) / 3.0;
    let (ta, tb) = (tangent_at(pts, i), tangent_at(pts, i + 1));
    (
        [a[0] + ta[0] * reach, a[1] + ta[1] * reach],
        [b[0] - tb[0] * reach, b[1] - tb[1] * reach],
    )
}

/// Путь SVG кривыми Безье через точки ломаной (правило - у `curvePath`).
fn curve_path(pts: &[Point], auto: bool) -> String {
    let p = |q: Point| format!("{} {}", num(q[0]), num(q[1]));
    if auto && pts.len() == 3 {
        return format!("M{}Q{} {}", p(pts[0]), p(pts[1]), p(pts[2]));
    }
    let mut d = format!("M{}", p(pts[0]));
    for i in 0..pts.len() - 1 {
        let (c1, c2) = curve_controls(pts, i);
        d += &format!(
            "C{} {} {} {} {}",
            num(tenth(c1[0])),
            num(tenth(c1[1])),
            num(tenth(c2[0])),
            num(tenth(c2[1])),
            p(pts[i + 1])
        );
    }
    d
}

fn curve_point(pts: &[Point], i: usize, t: f64) -> Point {
    let (c1, c2) = curve_controls(pts, i);
    let (a, b) = (pts[i], pts[i + 1]);
    let u = 1.0 - t;
    let at = |k: usize| {
        tenth(
            u * u * u * a[k] + 3.0 * u * u * t * c1[k] + 3.0 * u * t * t * c2[k] + t * t * t * b[k],
        )
    };
    [at(0), at(1)]
}

/// Где ломаная `mine` лежит на уже нарисованных (правило - у `overlapWith`).
#[derive(Debug, Clone, PartialEq)]
pub struct Overlap {
    pub runs: Vec<[f64; 2]>,
    pub total: f64,
    pub ending: bool,
}

pub fn overlap_with(mine: &[Point], others: &[Vec<Point>]) -> Overlap {
    let mut raw: Vec<[f64; 2]> = Vec::new();
    let mut offset = 0.0;
    for (a, b) in segments(mine) {
        let r = [b[0] - a[0], b[1] - a[1]];
        let len = r[0].hypot(r[1]);
        if len == 0.0 {
            continue;
        }
        for list in others {
            for (c, d) in segments(list) {
                let s = [d[0] - c[0], d[1] - c[1]];
                let other_len = s[0].hypot(s[1]);
                if other_len == 0.0 || cross(r, s).abs() > 1e-6 * len * other_len {
                    continue;
                }
                if cross(r, [c[0] - a[0], c[1] - a[1]]).abs() / len > 0.5 {
                    continue;
                }
                let along = |p: Point| ((p[0] - a[0]) * r[0] + (p[1] - a[1]) * r[1]) / len;
                let lo = 0f64.max(along(c).min(along(d)));
                let hi = len.min(along(c).max(along(d)));
                if hi - lo > 0.5 {
                    raw.push([offset + lo, offset + hi]);
                }
            }
        }
        offset += len;
    }
    raw.sort_by(|p, q| p[0].total_cmp(&q[0]));
    let mut runs: Vec<[f64; 2]> = Vec::new();
    for run in raw {
        match runs.last_mut() {
            Some(last) if run[0] <= last[1] + 0.5 => last[1] = last[1].max(run[1]),
            _ => runs.push(run),
        }
    }
    let end = mine[mine.len() - 1];
    let before = if mine.len() >= 2 {
        mine[mine.len() - 2]
    } else {
        end
    };
    let way = unit([end[0] - before[0], end[1] - before[1]]);
    let ending = others.iter().any(|list| {
        let theirs = list[list.len() - 1];
        let prev = if list.len() >= 2 {
            list[list.len() - 2]
        } else {
            theirs
        };
        let their = unit([theirs[0] - prev[0], theirs[1] - prev[1]]);
        near(end, theirs) < 0.5
            && cross(way, their).abs() < 1e-6
            && way[0] * their[0] + way[1] * their[1] > 0.0
    });
    Overlap {
        runs: runs.iter().map(|r| [tenth(r[0]), tenth(r[1])]).collect(),
        total: tenth(offset),
        ending,
    }
}

/// Штрих с пропусками в форме `stroke-dasharray`.
pub fn dash_for(total: f64, runs: &[[f64; 2]]) -> String {
    let mut parts = Vec::new();
    let mut at = 0.0;
    for [from, to] in runs {
        parts.push(tenth((from - at).max(0.0)));
        parts.push(tenth(to - from));
        at = *to;
    }
    parts.push(tenth((total - at).max(0.0)));
    parts.iter().map(|p| num(*p)).collect::<Vec<_>>().join(" ")
}

/// Форма углов ребра.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Corners {
    Square,
    Round,
    Bezier,
}

/// Вид пересечения: мостик либо разрыв.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Crossing {
    Hop,
    Gap,
}

/// Путь SVG по ломаной с мостиками либо разрывами (правило - у `buildPath`).
pub fn build_path(
    pts: &[Point],
    hops: &[Point],
    shape: Corners,
    crossing: Crossing,
    auto: bool,
) -> String {
    if shape == Corners::Bezier {
        return curve_path(pts, auto);
    }
    let round_corners = shape == Corners::Round;
    let mut cur = pts[0];
    let mut d = format!("M{} {}", num(cur[0]), num(cur[1]));
    for i in 1..pts.len() {
        let (a, b) = (cur, pts[i]);
        let raw = (b[0] - a[0]).hypot(b[1] - a[1]);
        let len = if raw == 0.0 { 1.0 } else { raw };
        let dir = [(b[0] - a[0]) / len, (b[1] - a[1]) / len];
        let trim = if round_corners && i < pts.len() - 1 {
            CORNER.min(len / 2.0)
        } else {
            0.0
        };
        let end = [b[0] - dir[0] * trim, b[1] - dir[1] * trim];
        let mut on: Vec<(Point, f64)> = hops
            .iter()
            .filter(|h| ((h[0] - a[0]) * dir[1] - (h[1] - a[1]) * dir[0]).abs() < 0.5)
            .map(|h| (*h, (h[0] - a[0]) * dir[0] + (h[1] - a[1]) * dir[1]))
            .filter(|(_, t)| *t > HOP && *t < len - trim - HOP)
            .collect();
        on.sort_by(|p, q| p.1.total_cmp(&q.1));
        for (h, _) in on {
            d += &format!("L{} {}", num(h[0] - dir[0] * HOP), num(h[1] - dir[1] * HOP));
            let (x, y) = (num(h[0] + dir[0] * HOP), num(h[1] + dir[1] * HOP));
            d += &match crossing {
                Crossing::Gap => format!("M{x} {y}"),
                Crossing::Hop => format!("A{} {} 0 0 1 {x} {y}", num(HOP), num(HOP)),
            };
        }
        d += &format!("L{} {}", num(end[0]), num(end[1]));
        if trim != 0.0 {
            let nb = pts[i + 1];
            let raw2 = (nb[0] - b[0]).hypot(nb[1] - b[1]);
            let len2 = if raw2 == 0.0 { 1.0 } else { raw2 };
            let t2 = CORNER.min(len2 / 2.0);
            let q = [
                b[0] + ((nb[0] - b[0]) / len2) * t2,
                b[1] + ((nb[1] - b[1]) / len2) * t2,
            ];
            d += &format!("Q{} {} {} {}", num(b[0]), num(b[1]), num(q[0]), num(q[1]));
            cur = q;
        } else {
            cur = b;
        }
    }
    d
}

/// Точка на ломаной на расстоянии `dist` от начала либо от конца.
fn point_along(pts: &[Point], from_end: bool, dist: f64) -> Point {
    let list: Vec<Point> = if from_end {
        pts.iter().rev().copied().collect()
    } else {
        pts.to_vec()
    };
    let mut left = dist;
    for w in list.windows(2) {
        let (a, b) = (w[0], w[1]);
        let raw = (b[0] - a[0]).hypot(b[1] - a[1]);
        let len = if raw == 0.0 { 1.0 } else { raw };
        if len >= left {
            return [
                a[0] + ((b[0] - a[0]) / len) * left,
                a[1] + ((b[1] - a[1]) / len) * left,
            ];
        }
        left -= len;
    }
    list[list.len() - 1]
}

fn longest(pts: &[Point]) -> (usize, Point) {
    let mut best = -1.0;
    let mut index = 0;
    let mut mid = pts[0];
    for i in 1..pts.len() {
        let len = (pts[i][0] - pts[i - 1][0]).hypot(pts[i][1] - pts[i - 1][1]);
        if len > best {
            best = len;
            index = i - 1;
            mid = [
                (pts[i][0] + pts[i - 1][0]) / 2.0,
                (pts[i][1] + pts[i - 1][1]) / 2.0,
            ];
        }
    }
    (index, mid)
}

/// Место знака условия.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LabelPlace {
    Start,
    Center,
    End,
    Own(f64, f64),
}

/// Место знака условия на ломаной (правило - у `markSpot`).
pub fn mark_spot(label: LabelPlace, pts: &[Point], shape: Corners, auto: bool) -> Point {
    match label {
        LabelPlace::Own(x, y) => [x, y],
        LabelPlace::Start => point_along(pts, false, MARK_OFF),
        LabelPlace::End => point_along(pts, true, MARK_OFF),
        LabelPlace::Center => {
            if shape == Corners::Bezier && auto && pts.len() == 3 {
                let at = |k: usize| tenth(0.25 * pts[0][k] + 0.5 * pts[1][k] + 0.25 * pts[2][k]);
                return [at(0), at(1)];
            }
            if shape == Corners::Bezier && pts.len() > 1 {
                return curve_point(pts, longest(pts).0, 0.5);
            }
            longest(pts).1
        }
    }
}

/// Размер листа: начало (может уйти в минус) и ширина с высотой.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Size {
    pub ox: f64,
    pub oy: f64,
    pub w: f64,
    pub h: f64,
}

/// Размер листа по узлам и прочим точкам рисунка (правило - у `sheetSize`).
pub fn sheet_size(nodes: &[Shape], extra: &[Point]) -> Size {
    let (mut min_x, mut min_y, mut max_x, mut max_y) = (0f64, 0f64, 0f64, 0f64);
    for node in nodes {
        let h = node.half();
        min_x = min_x.min(node.x - h - REACH);
        min_y = min_y.min(node.y - h - REACH);
        max_x = max_x.max(node.x + h);
        max_y = max_y.max(node.y + h + SNAP * 4.0);
    }
    for [x, y] in extra {
        min_x = min_x.min(x - REACH);
        min_y = min_y.min(y - REACH);
        max_x = max_x.max(*x);
        max_y = max_y.max(*y);
    }
    let snap = |v: f64| round(v / SNAP) * SNAP;
    let ox = if min_x < 0.0 {
        snap(min_x - MARGIN)
    } else {
        0.0
    };
    let oy = if min_y < 0.0 {
        snap(min_y - MARGIN)
    } else {
        0.0
    };
    Size {
        ox,
        oy,
        w: (COL * 2.0).max(max_x + MARGIN - ox),
        h: (ROW * 2.0).max(max_y + MARGIN - oy),
    }
}

/// Рамка скобок либо параллели.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Frame {
    pub x: f64,
    pub y: f64,
    pub w: f64,
    pub h: f64,
    pub parallel: bool,
}

impl Frame {
    /// Углы рамки - точки рисунка для размера листа.
    pub fn corners(&self) -> [Point; 2] {
        [[self.x, self.y], [self.x + self.w, self.y + self.h]]
    }
}

/// Узел дерева реализации для листа композиции.
pub enum Tree<'a> {
    Model(&'a str),
    Chain(Vec<Tree<'a>>),
    Parallel(Vec<Tree<'a>>),
    Group(Box<Tree<'a>>),
}

/// Шаг листа композиции: имя `Модель#номер` и модель.
pub type Step = (String, String);

/// Ребро листа композиции: от, к и номер в паре.
pub type Link = (String, String, u32);

/// Шаги и рёбра листа композиции по дереву реализации: шаг - `Модель#номер` в
/// порядке обхода слева направо, ребро - между соседними шагами цепочки (правило -
/// у `composeSheet` без координат).
pub fn compose(tree: &Tree<'_>) -> (Vec<Step>, Vec<Link>) {
    fn walk(
        item: &Tree<'_>,
        seen: &mut BTreeMap<String, u32>,
        steps: &mut Vec<Step>,
        link: &mut dyn FnMut(&str, &str),
    ) -> (Vec<String>, Vec<String>) {
        match item {
            Tree::Model(model) => {
                let count = seen.entry((*model).to_string()).or_insert(0);
                *count += 1;
                let name = format!("{model}#{count}");
                steps.push((name.clone(), (*model).to_string()));
                (vec![name.clone()], vec![name])
            }
            Tree::Group(inner) => walk(inner, seen, steps, link),
            Tree::Chain(items) => {
                let mut first: Option<Vec<String>> = None;
                let mut prev: Option<Vec<String>> = None;
                for child in items {
                    let placed = walk(child, seen, steps, link);
                    if let Some(prev) = &prev {
                        for a in prev {
                            for b in &placed.0 {
                                link(a, b);
                            }
                        }
                    }
                    if first.is_none() {
                        first = Some(placed.0.clone());
                    }
                    prev = Some(placed.1);
                }
                (first.unwrap_or_default(), prev.unwrap_or_default())
            }
            Tree::Parallel(items) => {
                let (mut first, mut last) = (Vec::new(), Vec::new());
                for child in items {
                    let placed = walk(child, seen, steps, link);
                    first.extend(placed.0);
                    last.extend(placed.1);
                }
                (first, last)
            }
        }
    }
    let mut steps = Vec::new();
    let mut edges: Vec<Link> = Vec::new();
    let mut pairs: BTreeMap<(String, String), u32> = BTreeMap::new();
    let mut link = |a: &str, b: &str| {
        let ordinal = pairs.entry((a.to_string(), b.to_string())).or_insert(0);
        edges.push((a.to_string(), b.to_string(), *ordinal));
        *ordinal += 1;
    };
    walk(tree, &mut BTreeMap::new(), &mut steps, &mut link);
    (steps, edges)
}

/// Рамки листа композиции по положению шагов (правило - у `framesOf`): рамка скобок
/// и рамка параллели обнимают свои шаги и вложенные рамки, внешняя - первой.
pub fn frames_of(tree: &Tree<'_>, at: &BTreeMap<String, Point>) -> Vec<Frame> {
    fn union(a: Option<[f64; 4]>, b: Option<[f64; 4]>) -> Option<[f64; 4]> {
        match (a, b) {
            (None, x) | (x, None) => x,
            (Some(a), Some(b)) => Some([
                a[0].min(b[0]),
                a[1].min(b[1]),
                a[2].max(b[2]),
                a[3].max(b[3]),
            ]),
        }
    }
    fn walk(
        item: &Tree<'_>,
        seen: &mut BTreeMap<String, u32>,
        at: &BTreeMap<String, Point>,
        frames: &mut Vec<Option<Frame>>,
    ) -> Option<[f64; 4]> {
        let (framed, parallel, children): (bool, bool, Vec<&Tree<'_>>) = match item {
            Tree::Model(model) => {
                let count = seen.entry((*model).to_string()).or_insert(0);
                *count += 1;
                let point = at.get(&format!("{model}#{count}"))?;
                let h = SIDE / 2.0;
                return Some([point[0] - h, point[1] - h, point[0] + h, point[1] + h]);
            }
            Tree::Group(inner) => (true, false, vec![inner.as_ref()]),
            Tree::Chain(items) => (false, false, items.iter().collect()),
            Tree::Parallel(items) => (true, true, items.iter().collect()),
        };
        let slot = framed.then(|| {
            frames.push(None);
            frames.len() - 1
        });
        let mut bbox = None;
        for child in children {
            bbox = union(bbox, walk(child, seen, at, frames));
        }
        let slot = slot?;
        let b = bbox?;
        let (x0, y0, x1, y1) = (
            b[0] - FRAME_PAD,
            b[1] - FRAME_PAD,
            b[2] + FRAME_PAD,
            b[3] + FRAME_PAD,
        );
        frames[slot] = Some(Frame {
            x: x0,
            y: y0,
            w: x1 - x0,
            h: y1 - y0,
            parallel,
        });
        Some([x0, y0, x1, y1])
    }
    let mut frames = Vec::new();
    walk(tree, &mut BTreeMap::new(), at, &mut frames);
    frames.into_iter().flatten().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn circle(name: &str, x: f64, y: f64) -> Shape {
        Shape {
            name: name.into(),
            x,
            y,
            square: false,
        }
    }

    #[test]
    fn port_points_sit_on_the_frame_with_a_gap() {
        assert_eq!(port_point(&circle("A", 0.0, 0.0), 0), [26.0, 0.0]);
        assert_eq!(
            port_point(&circle("A", 0.0, 0.0), 4),
            [0.0, 26.0],
            "вниз - ось y экрана"
        );
        let square = Shape {
            name: "C".into(),
            x: 0.0,
            y: 0.0,
            square: true,
        };
        assert_eq!(port_point(&square, 2), [73.4, 73.4], "угол квадрата");
    }

    #[test]
    fn a_straight_edge_takes_facing_ports() {
        let (a, b) = (circle("A", 0.0, 0.0), circle("B", 200.0, 0.0));
        let by_name = BTreeMap::from([("A", &a), ("B", &b)]);
        let edges = [EdgeInput {
            from: "A",
            to: "B",
            points: &[],
            end_from: None,
            end_to: None,
        }];
        let routes = route_sheet(&by_name, &edges, &[]);
        assert_eq!(routes[0].as_deref(), Some(&[[26.0, 0.0], [174.0, 0.0]][..]));
    }

    #[test]
    fn composition_steps_are_named_by_model_and_order() {
        let tree = Tree::Chain(vec![
            Tree::Model("E"),
            Tree::Group(Box::new(Tree::Parallel(vec![
                Tree::Model("E"),
                Tree::Model("P"),
            ]))),
            Tree::Model("E"),
        ]);
        let (steps, edges) = compose(&tree);
        let names: Vec<&str> = steps.iter().map(|s| s.0.as_str()).collect();
        assert_eq!(names, ["E#1", "E#2", "P#1", "E#3"]);
        let pairs: Vec<(&str, &str)> = edges.iter().map(|e| (e.0.as_str(), e.1.as_str())).collect();
        assert_eq!(
            pairs,
            [
                ("E#1", "E#2"),
                ("E#1", "P#1"),
                ("E#2", "E#3"),
                ("P#1", "E#3")
            ]
        );
    }
}

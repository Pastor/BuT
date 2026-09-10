// Геометрия схемы: размеры, трассировка рёбер, ярусное размещение, лист композиции.
//
// Здесь только чистые функции над числами: ни DOM, ни знания о языке. Размеры
// принадлежат оформлению (числа - производные шкал книги, см. `app.css`), а что
// рисовать, говорит граф модуля. Модуль проверяется в `node`: ошибка трассировки
// даёт валидный рисунок с другим автоматом, и глазом её видно не всегда.

/** Радиус круга узла: половина `--node-d`. */
export const R = 24;
/** Сторона квадрата композиции: `--comp-side`. */
export const SIDE = 144;
/** Шаг привязки к сетке: `--gap`. */
export const SNAP = 8;
/** Радиус мостика на пересечении линий: половина шага привязки. */
export const HOP = 4;
/** Радиус скругления углов ломаной: `--radius-sm`. */
export const CORNER = 5;
/** Отступ знака условия от узла при базовых местах "начало" и "конец". */
export const MARK_OFF = SNAP * 3;
/** Поле листа вокруг рисунка. */
export const MARGIN = SNAP * 9;
/** Шаг размещения по горизонтали и вертикали при автораскладке. */
export const COL = SNAP * 18;
export const ROW = SNAP * 15;
/** Пределы и шаг масштаба. */
export const ZOOM_MIN = 0.25;
export const ZOOM_MAX = 4;
export const ZOOM_STEP = 1.1;
/**
 * Шаг масштаба у колеса - мельче кнопочного.
 *
 * Кнопку и клавишу нажимают по разу, а колесо шлёт события очередью: одним
 * движением их приходит десяток, и на кнопочном шаге лист прыгал от края до края
 * предела. Мельче шаг - тот же путь колеса даёт плавный ход, а добраться до
 * предела по-прежнему можно.
 */
export const WHEEL_ZOOM_STEP = 1.04;
/** Порог начала переноса, пикселей экрана. */
export const DRAG_THRESHOLD = 4;

/** Привязка к сетке. */
export function snap(value) {
  return Math.round(value / SNAP) * SNAP;
}

/** Половина размера узла по оси: круг либо квадрат композиции. */
function half(node) {
  return node.kind === "composition" ? SIDE / 2 : R;
}

/**
 * Число точек привязки на рамке узла: через 22,5°.
 *
 * Четыре точки (середины граней) сводили в одну все рёбра, идущие в одну
 * сторону: три входа справа сливались у круга в одну линию, и по рисунку нельзя
 * было сосчитать переходы. На шестнадцати соседние точки круга расходятся
 * примерно на десять пикселей - шаг, различимый глазом.
 */
export const PORTS = 16;
/** Точка стрелки начального состояния по умолчанию: слева, откуда читают лист. */
export const ENTRY_PORT = 8;

/** Координата с точностью до десятой: запись пути короче, а минус-ноль не рождается. */
const tenth = (value) => Math.round(value * 10) / 10 + 0;

/**
 * Точка привязки `index` узла: на окружности круга либо на рамке квадрата
 * композиции по лучу из центра, с тем же зазором, что у `anchor`. Счёт идёт от
 * направления "вправо" по часовой стрелке экрана (ось y смотрит вниз).
 */
export function portPoint(node, index) {
  const angle = (index * 2 * Math.PI) / PORTS;
  const cos = Math.cos(angle);
  const sin = Math.sin(angle);
  const h = half(node);
  const reach = (node.kind === "composition" ? h / Math.max(Math.abs(cos), Math.abs(sin)) : h) + 2;
  return [tenth(node.x + cos * reach), tenth(node.y + sin * reach)];
}

/**
 * Направление на точку `to` в шагах точек привязки: дробное, от 0 до 16.
 *
 * Округлено до миллионной: ход строго вниз обязан давать ровно 4, иначе шум
 * вычисления решал бы за правило, какой из двух равных соседей взять.
 */
function bearing(node, to) {
  const turn = Math.atan2(to[1] - node.y, to[0] - node.x) / (2 * Math.PI);
  return Math.round(((((turn * PORTS) % PORTS) + PORTS) % PORTS) * 1e6) / 1e6;
}

/** Номер точки привязки, ближайшей к направлению на точку `to`. */
export function portToward(node, to) {
  return Math.round(bearing(node, to)) % PORTS;
}

/**
 * Расстояние по кругу между точкой `port` и направлением `at`, в шагах.
 *
 * Округлено до миллионной: равные по смыслу расстояния обязаны сравниваться
 * равными, иначе порядок раздачи решал бы последний разряд вычитания.
 */
function turnGap(port, at) {
  const d = Math.abs(port - at) % PORTS;
  return Math.round(Math.min(d, PORTS - d) * 1e6) / 1e6;
}

/**
 * Раздаёт концам рёбер точки привязки: каждый конец берёт ближайшую к своему
 * ходу свободную точку своего узла.
 *
 * Первыми выбирают концы, чей ход ближе всего к какой-нибудь точке. При равном
 * ходе первым выбирает прямое ребро - то, чей узел на другом конце лежит прямо
 * по ходу (`far`). Точка оценивается суммой двух расстояний: до хода и до
 * направления на дальний узел, - так ребро, которое дальше поворачивает, уходит
 * со своей занятой точки в сторону поворота, даже если соседняя с другой стороны
 * ближе: иначе два ребра перехлёстывались бы у самого узла. При равной сумме
 * берётся точка ближе к ходу. Дальше решает порядок листа, поэтому один
 * лист раскладывается одинаково от открытия к открытию. Концов больше
 * шестнадцати - лишние делят ближайшую точку: иначе им негде встать.
 *
 * @param {{node: {name: string, x: number, y: number}, toward: number[], far?: number[]}[]} ends концы рёбер
 * @param {Map<string, Set<number>>} taken занятые заранее точки по имени узла (меняется на месте)
 * @returns {number[]} номер точки на каждый конец, в порядке `ends`
 */
export function assignPorts(ends, taken = new Map()) {
  const wanted = ends.map((end) => bearing(end.node, end.toward));
  const aim = ends.map((end) => bearing(end.node, end.far ?? end.toward));
  const miss = (i) => turnGap(Math.round(wanted[i]) % PORTS, wanted[i]);
  const bend = (i) => turnGap(wanted[i], aim[i]);
  const order = ends.map((_, i) => i).sort((i, j) => miss(i) - miss(j) || bend(i) - bend(j) || i - j);
  const out = new Array(ends.length);
  for (const i of order) {
    const name = ends[i].node.name;
    if (!taken.has(name)) taken.set(name, new Set());
    const used = taken.get(name);
    const score = (port) => turnGap(port, wanted[i]) + turnGap(port, aim[i]);
    const free = [...Array(PORTS).keys()]
      .filter((port) => !used.has(port))
      .sort((p, q) => score(p) - score(q) || turnGap(p, wanted[i]) - turnGap(q, wanted[i]) || p - q);
    const port = free[0] ?? Math.round(wanted[i]) % PORTS;
    used.add(port);
    out[i] = port;
  }
  return out;
}

/** Ход ребра без привязки: центры узлов и изломы; без изломов - один угол. */
function course(from, to, points) {
  const pins = (points ?? []).map((p) => [p[0], p[1]]);
  if (pins.length === 0 && from.x !== to.x && from.y !== to.y) {
    return [[from.x, from.y], [from.x, to.y], [to.x, to.y]];
  }
  return [[from.x, from.y], ...pins, [to.x, to.y]];
}

/**
 * Ломаные всех рёбер листа с раздачей точек привязки.
 *
 * Ход ребра тот же, что у `route` (центры узлов и изломы автора), а концы
 * встают в точки, розданные `assignPorts`. Петля самоперехода своей формы не
 * меняет: её концы занимают точки узла, и чужие рёбра их обходят. Так же
 * занимает точку стрелка начального состояния (`reserved`).
 *
 * Угол, поставленный раскладкой (у ребра без изломов автора), следует за
 * точками: он встаёт на вертикаль точки начала и горизонталь точки конца, и
 * ломаная остаётся ортогональной. Оставь его в центре хода - конец в соседней
 * точке тянулся бы наискось через весь отрезок. Изломы автора не двигаются:
 * они хранятся в файле и видны точками.
 *
 * @param {Map<string, object>} byName узлы листа по именам
 * @param {{from: string, to: string, loop?: boolean, points: number[][]}[]} edges рёбра в порядке листа
 * @param {Map<string, number>} reserved занятые заранее точки: имя узла - номер
 * @returns {(number[][]|null)[]} ломаная на каждое ребро; `null` - узла нет на листе
 */
export function routeSheet(byName, edges, reserved = new Map()) {
  const taken = new Map();
  const take = (name, port) => {
    if (!taken.has(name)) taken.set(name, new Set());
    taken.get(name).add(port);
  };
  for (const [name, port] of reserved) take(name, port);
  const out = edges.map(() => null);
  const ends = [];
  const corners = [];
  edges.forEach((edge, k) => {
    const from = byName.get(edge.from);
    const to = byName.get(edge.to);
    if (!from || !to) return;
    if (edge.loop || from === to) {
      const pts = route(from, from, edge.points);
      take(from.name, portToward(from, pts[0]));
      take(from.name, portToward(from, pts[pts.length - 1]));
      out[k] = pts;
      return;
    }
    const pts = course(from, to, edge.points);
    out[k] = pts;
    if ((edge.points ?? []).length === 0 && pts.length === 3) corners.push(k);
    ends.push({ k, at: 0, node: from, toward: pts[1], far: [to.x, to.y] });
    ends.push({ k, at: pts.length - 1, node: to, toward: pts[pts.length - 2], far: [from.x, from.y] });
  });
  const ports = assignPorts(ends, taken);
  ends.forEach((end, i) => {
    out[end.k][end.at] = portPoint(end.node, ports[i]);
  });
  for (const k of corners) out[k][1] = [out[k][0][0], out[k][2][1]];
  return out;
}

/**
 * Точка присоединения ребра к грани узла со стороны точки `to`: горизонтальная грань
 * при преимущественно горизонтальном ходе, иначе вертикальная; зазор в два пикселя,
 * чтобы наконечник не въезжал в рамку.
 */
export function anchor(node, to) {
  const dx = to[0] - node.x;
  const dy = to[1] - node.y;
  const h = half(node) + 2;
  if (dx === 0 && dy === 0) return [node.x + h, node.y];
  return Math.abs(dx) > Math.abs(dy)
    ? [node.x + Math.sign(dx) * h, node.y]
    : [node.x, node.y + Math.sign(dy) * h];
}

/**
 * Ломаная ребра: от грани источника через сохранённые изломы к грани цели. Без изломов
 * ход прямой либо один угол; самопереход - петля у правого верхнего угла.
 *
 * @param {object} from узел-источник `{x, y, kind}`
 * @param {object} to узел-цель
 * @param {number[][]} points сохранённые изломы
 */
export function route(from, to, points) {
  if (from === to || (from.x === to.x && from.y === to.y && from.name === to.name)) {
    const h = half(from);
    const x = from.x + h - 6;
    return [
      [x, from.y - h + 4],
      [x, from.y - h - 20],
      [x + 56, from.y - h - 20],
      [x + 56, from.y - 8],
      [from.x + h + 2, from.y - 8],
    ];
  }
  const pts = course(from, to, points);
  pts[0] = anchor(from, pts[1]);
  pts[pts.length - 1] = anchor(to, pts[pts.length - 2]);
  return pts;
}

const near = (a, b) => Math.hypot(a[0] - b[0], a[1] - b[1]);
const segments = (pts) => pts.slice(1).map((p, i) => [pts[i], p]);
const cross = (u, v) => u[0] * v[1] - u[1] * v[0];

/**
 * Точки, где ломаная `mine` пересекает ломаные `others`: встречи под любым углом
 * внутри обоих отрезков. Угол нужен: конец ребра в соседней точке привязки идёт
 * наискось, и пересечение с ним не менее настоящее, чем прямое. Параллельные
 * отрезки пересечения не дают - общий путь разводит раскладка изломами. Общая
 * точка у входа в узел (слияние) мостика не получает, и на неё пересечение не
 * считается.
 */
export function crossings(mine, others) {
  const out = [];
  for (const [a, b] of segments(mine)) {
    const r = [b[0] - a[0], b[1] - a[1]];
    const len = Math.hypot(r[0], r[1]);
    if (len === 0) continue;
    for (const list of others) {
      for (const [c, d] of segments(list)) {
        const s = [d[0] - c[0], d[1] - c[1]];
        const otherLen = Math.hypot(s[0], s[1]);
        const denom = cross(r, s);
        if (otherLen === 0 || Math.abs(denom) < 1e-9 * len * otherLen) continue;
        const ac = [c[0] - a[0], c[1] - a[1]];
        const t = cross(ac, s) / denom;
        const u = cross(ac, r) / denom;
        const insideMine = t * len > HOP + 2 && t * len < len - HOP - 2;
        const insideOther = u * otherLen > 1 && u * otherLen < otherLen - 1;
        const p = [tenth(a[0] + r[0] * t), tenth(a[1] + r[1] * t)];
        if (
          insideMine &&
          insideOther &&
          near(p, mine[0]) > HOP * 2 &&
          near(p, mine[mine.length - 1]) > HOP * 2
        ) {
          out.push(p);
        }
      }
    }
  }
  return out;
}

/**
 * Путь SVG по ломаной: с мостиками либо разрывами на пересечениях и скруглёнными
 * углами по форме.
 *
 * @param {number[][]} pts точки ломаной
 * @param {number[][]} hops точки пересечений
 * @param {boolean} round углы скруглённые
 * @param {"hop"|"gap"} crossing вид пересечения: мостик-полукруг либо разрыв линии
 */
export function buildPath(pts, hops = [], round = false, crossing = "hop") {
  let cur = pts[0];
  let d = `M${cur[0]} ${cur[1]}`;
  for (let i = 1; i < pts.length; i += 1) {
    const a = cur;
    const b = pts[i];
    const len = Math.hypot(b[0] - a[0], b[1] - a[1]) || 1;
    const dir = [(b[0] - a[0]) / len, (b[1] - a[1]) / len];
    const trim = round && i < pts.length - 1 ? Math.min(CORNER, len / 2) : 0;
    const end = [b[0] - dir[0] * trim, b[1] - dir[1] * trim];
    const on = hops
      .filter((h) => Math.abs((h[0] - a[0]) * dir[1] - (h[1] - a[1]) * dir[0]) < 0.5)
      .map((h) => ({ h, t: (h[0] - a[0]) * dir[0] + (h[1] - a[1]) * dir[1] }))
      .filter((o) => o.t > HOP && o.t < len - trim - HOP)
      .sort((p, q) => p.t - q.t);
    for (const { h } of on) {
      d += `L${h[0] - dir[0] * HOP} ${h[1] - dir[1] * HOP}`;
      // Разрыв той же ширины, что мостик: вид меняется, место пересечения нет.
      d += crossing === "gap"
        ? `M${h[0] + dir[0] * HOP} ${h[1] + dir[1] * HOP}`
        : `A${HOP} ${HOP} 0 0 1 ${h[0] + dir[0] * HOP} ${h[1] + dir[1] * HOP}`;
    }
    d += `L${end[0]} ${end[1]}`;
    if (trim) {
      const nb = pts[i + 1];
      const len2 = Math.hypot(nb[0] - b[0], nb[1] - b[1]) || 1;
      const t2 = Math.min(CORNER, len2 / 2);
      const q = [b[0] + ((nb[0] - b[0]) / len2) * t2, b[1] + ((nb[1] - b[1]) / len2) * t2];
      d += `Q${b[0]} ${b[1]} ${q[0]} ${q[1]}`;
      cur = q;
    } else {
      cur = b;
    }
  }
  return d;
}

/** Точка на ломаной на расстоянии `dist` от начала либо от конца. */
export function pointAlong(pts, from, dist) {
  const list = from === "end" ? [...pts].reverse() : pts;
  let left = dist;
  for (let i = 1; i < list.length; i += 1) {
    const a = list[i - 1];
    const b = list[i];
    const len = Math.hypot(b[0] - a[0], b[1] - a[1]) || 1;
    if (len >= left) return [a[0] + ((b[0] - a[0]) / len) * left, a[1] + ((b[1] - a[1]) / len) * left];
    left -= len;
  }
  return list[list.length - 1];
}

/**
 * Точки, в которых линия действительно изгибается.
 *
 * Звено - это изгиб: точка, лежащая на прямой между соседями, ничего о форме
 * линии не говорит, и в раскладке ей делать нечего. Порог назван половиной шага
 * сетки: точка, отстоящая на меньшее, на глаз лежит на линии, а раскладка,
 * хранящая такие точки, растёт от каждого случайного касания.
 *
 * @param {{x: number, y: number}} from узел начала
 * @param {{x: number, y: number}} to узел конца
 * @param {number[][]} points изломы автора
 * @returns {number[][]} те из них, что дают изгиб
 */
export function bendingPoints(from, to, points) {
  const kept = [];
  const list = Array.isArray(points) ? points : [];
  for (let i = 0; i < list.length; i += 1) {
    const before = i > 0 ? list[i - 1] : [from.x, from.y];
    const after = i + 1 < list.length ? list[i + 1] : [to.x, to.y];
    if (offLine(list[i], before, after) > SNAP / 2) kept.push(list[i]);
  }
  return kept;
}

/**
 * Расстояние от точки до прямой через соседей; совпали соседи - до самой точки.
 *
 * Мерится прямая, а не отрезок: точка на линии, но за спиной соседа, изгиба не
 * даёт - линия там идёт по себе же. Считай до отрезка, и такая точка объявлялась
 * бы изгибом, потому что до ближнего конца ей далеко (нашлось прогоном стенда:
 * две точки на одной вертикали пережили снятие фокуса).
 */
function offLine(p, a, b) {
  const vx = b[0] - a[0];
  const vy = b[1] - a[1];
  const len = Math.hypot(vx, vy);
  if (len === 0) return Math.hypot(p[0] - a[0], p[1] - a[1]);
  return Math.abs((p[0] - a[0]) * vy - (p[1] - a[1]) * vx) / len;
}

/** Середина самого длинного сегмента ломаной. */
export function longestMid(pts) {
  let best = -1;
  let mid = pts[0];
  for (let i = 1; i < pts.length; i += 1) {
    const len = Math.hypot(pts[i][0] - pts[i - 1][0], pts[i][1] - pts[i - 1][1]);
    if (len > best) {
      best = len;
      mid = [(pts[i][0] + pts[i - 1][0]) / 2, (pts[i][1] + pts[i - 1][1]) / 2];
    }
  }
  return mid;
}

/**
 * Место знака условия: своё (координаты), у начала, у конца либо по центру.
 *
 * @param {{place: string, x?: number, y?: number}|null} label запись места
 * @param {number[][]} pts ломаная ребра
 */
export function markSpot(label, pts) {
  const placeName = label?.place ?? "center";
  if (placeName === "own" && Number.isFinite(label.x) && Number.isFinite(label.y)) {
    return [label.x, label.y];
  }
  if (placeName === "start") return pointAlong(pts, "start", MARK_OFF);
  if (placeName === "end") return pointAlong(pts, "end", MARK_OFF);
  return longestMid(pts);
}

/**
 * Номер сегмента ломаной, ближайшего к точке: туда встаёт новый излом.
 *
 * @returns {number} индекс начала сегмента
 */
export function nearestSegment(pts, p) {
  let best = 0;
  let bestDist = Infinity;
  for (let i = 1; i < pts.length; i += 1) {
    const a = pts[i - 1];
    const b = pts[i];
    const len2 = (b[0] - a[0]) ** 2 + (b[1] - a[1]) ** 2 || 1;
    const t = Math.max(0, Math.min(1, ((p[0] - a[0]) * (b[0] - a[0]) + (p[1] - a[1]) * (b[1] - a[1])) / len2));
    const d = Math.hypot(a[0] + (b[0] - a[0]) * t - p[0], a[1] + (b[1] - a[1]) * t - p[1]);
    if (d < bestDist) {
      bestDist = d;
      best = i - 1;
    }
  }
  return best;
}

/**
 * Ярусное размещение узлов листа по `rank` и `order` из графа: ярус - строка, порядок -
 * столбец; строки с композицией выше на сторону квадрата. Ответ - центры по именам.
 *
 * @param {{name: string, kind: string, rank: number, order: number}[]} nodes
 */
export function autoPlace(nodes) {
  const rows = new Map();
  for (const node of nodes) {
    if (!rows.has(node.rank)) rows.set(node.rank, []);
    rows.get(node.rank).push(node);
  }
  const ranks = [...rows.keys()].sort((a, b) => a - b);
  const width = Math.max(1, ...ranks.map((r) => rows.get(r).length));
  const out = {};
  let y = MARGIN;
  for (const rank of ranks) {
    const row = rows.get(rank).sort((a, b) => a.order - b.order);
    const tall = row.some((n) => n.kind === "composition");
    const step = row.some((n) => n.kind === "composition") ? Math.max(COL, SIDE + SNAP * 6) : COL;
    const rowWidth = (row.length - 1) * step;
    const offset = MARGIN + ((width - 1) * COL - rowWidth) / 2;
    if (tall) y += (SIDE - R * 2) / 2;
    row.forEach((node, i) => {
      out[node.name] = { x: snap(offset + i * step), y: snap(y) };
    });
    y += tall ? ROW + SIDE - R * 2 : ROW;
  }
  return out;
}

/**
 * Запас вокруг предмета листа со стороны начала: полширины знака условия с подложкой.
 *
 * Им же покрыты петля самоперехода и стрелка начального состояния - обе выходят за
 * круг узла меньше чем на запас. Без запаса излом на нулевой вертикали (точка
 * сетки, куда её ставит привязка) давал лист, начинающийся ровно на изломе, и
 * половину излома, линии и знака срезал край листа.
 */
const REACH = SNAP * 3;

/**
 * Размер листа по узлам: рисунок с полями; пустой лист - минимальный. Лист растёт и в
 * сторону отрицательных координат: начало (`ox`, `oy`) уходит левее и выше нуля, когда
 * рисунок вместе с запасом `REACH` заходит туда, - иначе край листа обрезал бы его.
 *
 * @param {object[]} nodes узлы листа
 * @param {number[][]} extra прочие точки рисунка: изломы и свои места знаков
 */
export function sheetSize(nodes, extra = []) {
  let minX = 0;
  let minY = 0;
  let maxX = 0;
  let maxY = 0;
  for (const node of nodes) {
    minX = Math.min(minX, node.x - half(node) - REACH);
    minY = Math.min(minY, node.y - half(node) - REACH);
    maxX = Math.max(maxX, node.x + half(node));
    maxY = Math.max(maxY, node.y + half(node) + SNAP * 4);
  }
  for (const [x, y] of extra) {
    minX = Math.min(minX, x - REACH);
    minY = Math.min(minY, y - REACH);
    maxX = Math.max(maxX, x);
    maxY = Math.max(maxY, y);
  }
  const ox = minX < 0 ? snap(minX - MARGIN) : 0;
  const oy = minY < 0 ? snap(minY - MARGIN) : 0;
  return { ox, oy, w: Math.max(COL * 2, maxX + MARGIN - ox), h: Math.max(ROW * 2, maxY + MARGIN - oy) };
}

/** Масштаб и сдвиг, при которых лист `w x h` целиком виден в области `box`. */
export function fitView(box, w, h) {
  const k = Math.min(ZOOM_MAX, Math.max(ZOOM_MIN, Math.min((box.width - 32) / w, (box.height - 32) / h)));
  return { k, x: (box.width - w * k) / 2, y: (box.height - h * k) / 2 };
}

/** Масштаб вокруг точки экрана `(mx, my)`: точка под курсором остаётся на месте. */
export function zoomAt(view, mx, my, factor) {
  const k = Math.min(ZOOM_MAX, Math.max(ZOOM_MIN, view.k * factor));
  const r = k / view.k;
  return { k, x: mx - (mx - view.x) * r, y: my - (my - view.y) * r };
}

/** Точка листа по точке области экрана. */
export function toSheet(view, px, py) {
  return [(px - view.x) / view.k, (py - view.y) / view.k];
}

/** Вид, центрирующий точку листа в области. */
export function centerOn(view, box, x, y) {
  return { k: view.k, x: box.width / 2 - x * view.k, y: box.height / 2 - y * view.k };
}

/**
 * Лист композиции по дереву реализации: цепочка слева направо, ветви параллели друг
 * под другом в рамке, скобки - вложенной рамкой. Лист не хранится - его задаёт
 * выражение; ответ - узлы (все композиции), рёбра между шагами и рамки.
 *
 * @param {object} implement дерево `{model}|{chain}|{parallel}|{group}` из ответа модуля
 */
export function composeSheet(implement) {
  const nodes = [];
  const edges = [];
  const frames = [];
  const gap = SNAP * 6;
  const pad = SNAP * 3;

  const measure = (item) => {
    if (item.model) return { w: SIDE, h: SIDE };
    if (item.group) return pad2(measure(item.group));
    const items = item.chain ?? item.parallel ?? [];
    const sizes = items.map(measure);
    if (item.chain) {
      return {
        w: sizes.reduce((s, z) => s + z.w, 0) + gap * Math.max(0, items.length - 1),
        h: Math.max(0, ...sizes.map((z) => z.h)),
      };
    }
    return pad2({
      w: Math.max(0, ...sizes.map((z) => z.w)),
      h: sizes.reduce((s, z) => s + z.h, 0) + gap * Math.max(0, items.length - 1),
    });
  };
  const pad2 = (size) => ({ w: size.w + pad * 2, h: size.h + pad * 2 });

  // Раскладывает элемент в прямоугольнике с левым верхним углом `(x, y)`; отвечает
  // именами первого и последнего узла для рёбер цепочки.
  const lay = (item, x, y, size) => {
    if (item.model) {
      const name = `${item.model.name}#${nodes.length + 1}`;
      nodes.push({
        name,
        model: item.model.name,
        path: item.model.path ?? null,
        kind: "composition",
        x: snap(x + size.w / 2),
        y: snap(y + size.h / 2),
      });
      return { first: [name], last: [name] };
    }
    if (item.group) {
      frames.push({ x, y, w: size.w, h: size.h });
      return lay(item.group, x + pad, y + pad, measure(item.group));
    }
    if (item.chain) {
      let at = x;
      let first = null;
      let prev = null;
      for (const child of item.chain) {
        const childSize = measure(child);
        const placed = lay(child, at, y + (size.h - childSize.h) / 2, childSize);
        if (prev) for (const a of prev.last) for (const b of placed.first) edges.push({ from: a, to: b, kind: "next" });
        if (!first) first = placed.first;
        prev = placed;
        at += childSize.w + gap;
      }
      return { first: first ?? [], last: prev?.last ?? [] };
    }
    const items = item.parallel ?? [];
    frames.push({ x, y, w: size.w, h: size.h, parallel: true });
    let at = y + pad;
    const first = [];
    const last = [];
    for (const child of items) {
      const childSize = measure(child);
      const placed = lay(child, x + pad + (size.w - pad * 2 - childSize.w) / 2, at, childSize);
      first.push(...placed.first);
      last.push(...placed.last);
      at += childSize.h + gap;
    }
    return { first, last };
  };

  const size = measure(implement);
  lay(implement, MARGIN, MARGIN, size);
  return { nodes, edges, frames, w: size.w + MARGIN * 2, h: size.h + MARGIN * 2 };
}

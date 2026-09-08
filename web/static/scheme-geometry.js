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
  const pins = (points ?? []).map((p) => [p[0], p[1]]);
  let pts = [[from.x, from.y], ...pins, [to.x, to.y]];
  if (pins.length === 0 && from.x !== to.x && from.y !== to.y) {
    pts = [[from.x, from.y], [from.x, to.y], [to.x, to.y]];
  }
  pts[0] = anchor(from, pts[1]);
  pts[pts.length - 1] = anchor(to, pts[pts.length - 2]);
  return pts;
}

const near = (a, b) => Math.hypot(a[0] - b[0], a[1] - b[1]);
const between = (v, a, b, m) => v > Math.min(a, b) + m && v < Math.max(a, b) - m;
const segments = (pts) => pts.slice(1).map((p, i) => [pts[i], p]);

/**
 * Точки, где ломаная `mine` пересекает ломаные `others`: только строго перпендикулярные
 * встречи внутри обоих отрезков. Общая точка у входа в узел (слияние) мостика не
 * получает, и на неё пересечение не считается.
 */
export function crossings(mine, others) {
  const out = [];
  for (const [a, b] of segments(mine)) {
    const vertical = a[0] === b[0];
    if (vertical && a[1] === b[1]) continue;
    for (const list of others) {
      for (const [c, d] of segments(list)) {
        const otherVertical = c[0] === d[0];
        if (vertical === otherVertical) continue;
        const x = vertical ? a[0] : c[0];
        const y = vertical ? c[1] : a[1];
        const insideMine = vertical ? between(y, a[1], b[1], HOP + 2) : between(x, a[0], b[0], HOP + 2);
        const insideOther = otherVertical ? between(y, c[1], d[1], 1) : between(x, c[0], d[0], 1);
        const p = [x, y];
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
 * Путь SVG по ломаной: с мостиками на пересечениях и скруглёнными углами по форме.
 *
 * @param {number[][]} pts точки ломаной
 * @param {number[][]} hops точки пересечений, где рисуется мостик
 * @param {boolean} round углы скруглённые
 */
export function buildPath(pts, hops = [], round = false) {
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
      d += `A${HOP} ${HOP} 0 0 1 ${h[0] + dir[0] * HOP} ${h[1] + dir[1] * HOP}`;
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
    if (offSegment(list[i], before, after) > SNAP / 2) kept.push(list[i]);
  }
  return kept;
}

/** Расстояние от точки до отрезка; вырожденный отрезок - расстояние до его точки. */
function offSegment(p, a, b) {
  const vx = b[0] - a[0];
  const vy = b[1] - a[1];
  const len = vx * vx + vy * vy;
  if (len === 0) return Math.hypot(p[0] - a[0], p[1] - a[1]);
  const t = Math.max(0, Math.min(1, ((p[0] - a[0]) * vx + (p[1] - a[1]) * vy) / len));
  return Math.hypot(p[0] - (a[0] + t * vx), p[1] - (a[1] + t * vy));
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
 * Размер листа по узлам: рисунок с полями; пустой лист - минимальный. Лист растёт и в
 * сторону отрицательных координат: начало (`ox`, `oy`) уходит левее и выше нуля, когда
 * автор утянул туда узел или излом, - иначе край листа обрезал бы рисунок.
 */
export function sheetSize(nodes, extra = []) {
  let minX = 0;
  let minY = 0;
  let maxX = 0;
  let maxY = 0;
  for (const node of nodes) {
    minX = Math.min(minX, node.x - half(node));
    minY = Math.min(minY, node.y - half(node));
    maxX = Math.max(maxX, node.x + half(node));
    maxY = Math.max(maxY, node.y + half(node) + SNAP * 4);
  }
  for (const [x, y] of extra) {
    minX = Math.min(minX, x);
    minY = Math.min(minY, y);
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

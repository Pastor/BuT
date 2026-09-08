// Файл раскладки схемы: чтение, канон, сверка с графом, запись.
//
// # Что хранится
//
// Раскладка - работа автора: где стоят карточки состояний, как изломаны рёбра, какими
// словами подписаны знаки и где стоит легенда. Файл лежит рядом с моделью
// (`elevator.takt` -> `elevator.takt-ui`) и хранит только это: размеров в нём нет, их
// задаёт оформление, и хранить их значило бы зафиксировать вид.
//
// # Форма
//
//   { "format": 1, "corners": "square" | "round",
//     "legend": { "place": "bottom" | "right" | "float", "x": 24, "y": 72 },
//     "sheets": { "<путь листа>": {
//         "nodes": { "<имя>": { "x": 0, "y": 0 } },
//         "names": { "<имя>": "подпись автора" },
//         "edges": { "<от>><к>:<номер>": {
//             "points": [[x, y], ...],
//             "label": { "place": "start" | "center" | "end" | "own", "x": 0, "y": 0 },
//             "name": "подпись условия" } } } } }
//
// Запись каноническая: ключи отсортированы, отступ два пробела, числа целые, перевод
// строки в конце, умолчания и пустые листы не пишутся - дифф файла в git несёт только
// смысл.
//
// # Расхождение с моделью
//
// Файл - подсказка, ключ - имя. Состояние без записи ставится автоматически и
// помечается неразмещённым; запись без состояния читается, не рисуется и снимается
// при записи. Отказа нет ни в каком случае: компилятор файла не читает.
//
// Знания о языке здесь нет: что есть на листе, говорит граф модуля (`takt_graph`).

/** Версия формата файла. */
export const FORMAT = 1;

/** Формы углов ребра. */
export const CORNERS = ["square", "round"];

/** Места легенды относительно холста. */
export const LEGEND_PLACES = ["bottom", "right", "float"];

/** Места знака условия относительно стрелки; `own` - своё, с координатами. */
export const LABEL_PLACES = ["start", "center", "end", "own"];

/** Расширение файла раскладки. */
export const EXTENSION = ".takt-ui";

/** Расширение файла модели. */
const MODEL_EXTENSION = ".takt";

/**
 * Имя файла раскладки для файла модели; `null` - имя не модели.
 *
 * @param {string} name имя файла модели
 * @returns {string|null}
 */
export function layoutName(name) {
  if (typeof name !== "string" || !name.endsWith(MODEL_EXTENSION)) return null;
  return name.slice(0, -MODEL_EXTENSION.length) + EXTENSION;
}

/**
 * Имя файла модели для файла раскладки; `null` - имя не раскладки.
 *
 * @param {string} name имя файла раскладки
 * @returns {string|null}
 */
export function modelName(name) {
  if (!isLayoutName(name)) return null;
  return name.slice(0, -EXTENSION.length) + MODEL_EXTENSION;
}

/** Является ли имя файлом раскладки. */
export function isLayoutName(name) {
  return typeof name === "string" && name.endsWith(EXTENSION) && name.length > EXTENSION.length;
}

/** Пустая раскладка; порядок ключей - канонический (по алфавиту). */
export function empty() {
  return { corners: CORNERS[0], format: FORMAT, sheets: {} };
}

/**
 * Читает файл раскладки.
 *
 * Негодный файл не роняет схему: возвращается пустая раскладка и названная причина
 * ключом словаря. Пустой текст - тоже пустая раскладка, но без причины: файла ещё нет.
 *
 * @param {string} text текст файла
 * @returns {{layout: object, problem: {key: string, params: object}|null}}
 */
export function parse(text) {
  if (typeof text !== "string" || text.trim() === "") return { layout: empty(), problem: null };
  let raw;
  try {
    raw = JSON.parse(text);
  } catch (error) {
    return {
      layout: empty(),
      problem: { key: "scheme.fileUnreadable", params: { error: error?.message ?? String(error) } },
    };
  }
  if (!isObject(raw)) {
    return { layout: empty(), problem: { key: "scheme.fileUnreadable", params: { error: "" } } };
  }
  if (raw.format !== FORMAT) {
    return {
      layout: empty(),
      problem: { key: "scheme.fileFormat", params: { format: String(raw.format), known: String(FORMAT) } },
    };
  }
  return { layout: normalize(raw), problem: null };
}

/**
 * Каноническая запись раскладки.
 *
 * @param {object} layout раскладка
 * @returns {string} текст файла
 */
export function canonical(layout) {
  return `${JSON.stringify(normalize(layout), null, 2)}\n`;
}

/** Ключ ребра в файле: `от>к:номер`. */
export function edgeKey(edge) {
  return `${edge.from}>${edge.to}:${edge.ordinal ?? 0}`;
}

/** Разбирает ключ ребра; `null` - ключ не той формы. */
export function parseEdgeKey(key) {
  const found = /^(.+)>(.+):(\d+)$/.exec(key);
  if (!found) return null;
  return { from: found[1], to: found[2], ordinal: Number(found[3]) };
}

/**
 * Лист раскладки по пути; создаётся пустым, если его нет.
 *
 * @param {object} layout раскладка (меняется на месте)
 * @param {string} path путь листа
 */
export function sheet(layout, path) {
  if (!isObject(layout.sheets)) layout.sheets = {};
  if (!isObject(layout.sheets[path])) layout.sheets[path] = { edges: {}, names: {}, nodes: {} };
  const found = layout.sheets[path];
  if (!isObject(found.nodes)) found.nodes = {};
  if (!isObject(found.edges)) found.edges = {};
  if (!isObject(found.names)) found.names = {};
  return found;
}

/**
 * Ставит центр узла.
 *
 * @param {object} layout раскладка (меняется на месте)
 * @param {string} path путь листа
 * @param {string} name имя состояния
 * @param {number} x
 * @param {number} y
 */
export function place(layout, path, name, x, y) {
  sheet(layout, path).nodes[name] = { x: whole(x), y: whole(y) };
  return layout;
}

/**
 * Ставит подпись автора у состояния; пустая подпись снимает запись.
 *
 * @param {object} layout раскладка (меняется на месте)
 * @param {string} path путь листа
 * @param {string} name имя состояния
 * @param {string} alias подпись
 */
export function nameNode(layout, path, name, alias) {
  const names = sheet(layout, path).names;
  const text = typeof alias === "string" ? alias.trim() : "";
  if (text === "") delete names[name];
  else names[name] = text;
  return layout;
}

/** Запись ребра листа: создаётся пустой, если её нет. */
function edgeRecord(layout, path, key) {
  const edges = sheet(layout, path).edges;
  if (!isObject(edges[key])) edges[key] = {};
  return edges[key];
}

/** Снимает запись ребра, если в ней ничего не осталось. */
function dropIfBare(layout, path, key) {
  const edges = sheet(layout, path).edges;
  const record = edges[key];
  if (!record) return;
  const bare =
    !(Array.isArray(record.points) && record.points.length > 0) &&
    !record.label &&
    !record.name;
  if (bare) delete edges[key];
}

/**
 * Ставит точки излома ребра; пустой список снимает их.
 *
 * @param {object} layout раскладка (меняется на месте)
 * @param {string} path путь листа
 * @param {string} key ключ ребра (`edgeKey`)
 * @param {number[][]} points точки `[x, y]`
 */
export function bend(layout, path, key, points) {
  const record = edgeRecord(layout, path, key);
  const kept = cleanPoints(points);
  if (kept.length === 0) delete record.points;
  else record.points = kept;
  dropIfBare(layout, path, key);
  return layout;
}

/**
 * Ставит место знака условия: базовое (`start`, `center`, `end`) либо своё (`own` с
 * координатами). Центр - умолчание и не записывается.
 */
export function labelAt(layout, path, key, placeName, x, y) {
  const record = edgeRecord(layout, path, key);
  const label = cleanLabel({ place: placeName, x, y });
  if (label) record.label = label;
  else delete record.label;
  dropIfBare(layout, path, key);
  return layout;
}

/**
 * Ставит подпись автора у условия ребра; пустая подпись снимает запись.
 */
export function nameEdge(layout, path, key, alias) {
  const record = edgeRecord(layout, path, key);
  const text = typeof alias === "string" ? alias.trim() : "";
  if (text === "") delete record.name;
  else record.name = text;
  dropIfBare(layout, path, key);
  return layout;
}

/**
 * Ставит место легенды; полка снизу - умолчание и не записывается.
 */
export function legendAt(layout, placeName, x, y) {
  const legend = cleanLegend({ place: placeName, x, y });
  if (legend) layout.legend = legend;
  else delete layout.legend;
  return layout;
}

/**
 * Сверяет раскладку с графом модуля.
 *
 * Для каждого листа графа: `unplaced` - состояния без записи; `extraNodes` и
 * `extraEdges` - записи, которых в модели нет (подписи считаются наравне с
 * координатами); листы, которых нет в графе, - в `extraSheets`. Числа сведены в
 * `extras`: ноль - уведомление не нужно.
 *
 * @param {object} layout раскладка
 * @param {{sheets: object[]}} graph ответ `takt_graph`
 */
export function reconcile(layout, graph) {
  const known = new Set();
  const sheets = {};
  let extras = 0;
  for (const sheetOfGraph of graph?.sheets ?? []) {
    known.add(sheetOfGraph.path);
    const stored = layout?.sheets?.[sheetOfGraph.path];
    const nodes = isObject(stored?.nodes) ? stored.nodes : {};
    const names = isObject(stored?.names) ? stored.names : {};
    const edges = isObject(stored?.edges) ? stored.edges : {};
    const present = new Set(sheetOfGraph.nodes.map((n) => n.name));
    const keys = new Set(sheetOfGraph.edges.map(edgeKey));
    const unplaced = sheetOfGraph.nodes.map((n) => n.name).filter((n) => !isPoint(nodes[n]));
    const extraNodes = [...new Set([...Object.keys(nodes), ...Object.keys(names)])]
      .filter((n) => !present.has(n))
      .sort();
    const extraEdges = Object.keys(edges).filter((k) => !keys.has(k)).sort();
    extras += extraNodes.length + extraEdges.length;
    sheets[sheetOfGraph.path] = { unplaced, extraNodes, extraEdges };
  }
  const extraSheets = Object.keys(layout?.sheets ?? {}).filter((p) => !known.has(p)).sort();
  extras += extraSheets.length;
  return { sheets, extraSheets, extras };
}

/**
 * Снимает записи, которых в модели нет.
 *
 * @param {object} layout раскладка
 * @param {{sheets: object[]}} graph ответ `takt_graph`
 * @returns {object} новая раскладка
 */
export function prune(layout, graph) {
  const report = reconcile(layout, graph);
  const out = normalize(layout);
  for (const path of report.extraSheets) delete out.sheets[path];
  for (const [path, found] of Object.entries(report.sheets)) {
    const stored = out.sheets[path];
    if (!stored) continue;
    for (const name of found.extraNodes) {
      delete stored.nodes[name];
      delete stored.names[name];
    }
    for (const key of found.extraEdges) delete stored.edges[key];
  }
  return normalize(out);
}

/**
 * Переименовывает состояние в записях листа: ключ узла, ключ подписи и ключи рёбер.
 *
 * @param {object} layout раскладка
 * @param {string} path путь листа
 * @param {string} from прежнее имя
 * @param {string} to новое имя
 * @returns {object} новая раскладка
 */
export function rename(layout, path, from, to) {
  const out = normalize(layout);
  const stored = out.sheets[path];
  if (!stored || from === to) return out;
  for (const map of [stored.nodes, stored.names]) {
    if (map[from] !== undefined) {
      map[to] = map[from];
      delete map[from];
    }
  }
  const moved = [];
  for (const [key, value] of Object.entries(stored.edges)) {
    const parsed = parseEdgeKey(key);
    if (!parsed) continue;
    const renamed = {
      from: parsed.from === from ? to : parsed.from,
      to: parsed.to === from ? to : parsed.to,
      ordinal: parsed.ordinal,
    };
    moved.push([edgeKey(renamed), value]);
  }
  stored.edges = Object.fromEntries(moved);
  return normalize(out);
}

/**
 * Приводит раскладку к канонической форме: известные поля, отсортированные ключи,
 * целые числа, умолчания опущены. Чужие поля и негодные записи отбрасываются.
 */
function normalize(raw) {
  const out = empty();
  if (CORNERS.includes(raw?.corners)) out.corners = raw.corners;
  const legend = cleanLegend(raw?.legend);
  if (legend) out.legend = legend;
  const sheets = isObject(raw?.sheets) ? raw.sheets : {};
  for (const path of Object.keys(sheets).sort()) {
    const stored = sheets[path];
    if (!isObject(stored)) continue;
    const edges = {};
    const storedEdges = isObject(stored.edges) ? stored.edges : {};
    for (const key of Object.keys(storedEdges).sort()) {
      const record = storedEdges[key];
      if (!parseEdgeKey(key) || !isObject(record)) continue;
      const clean = {};
      const label = cleanLabel(record.label);
      if (label) clean.label = label;
      if (typeof record.name === "string" && record.name.trim() !== "") clean.name = record.name.trim();
      const points = cleanPoints(record.points);
      if (points.length > 0) clean.points = points;
      if (Object.keys(clean).length > 0) edges[key] = clean;
    }
    const names = {};
    const storedNames = isObject(stored.names) ? stored.names : {};
    for (const name of Object.keys(storedNames).sort()) {
      const alias = storedNames[name];
      if (typeof alias === "string" && alias.trim() !== "") names[name] = alias.trim();
    }
    const nodes = {};
    const storedNodes = isObject(stored.nodes) ? stored.nodes : {};
    for (const name of Object.keys(storedNodes).sort()) {
      const point = storedNodes[name];
      if (isPoint(point)) nodes[name] = { x: whole(point.x), y: whole(point.y) };
    }
    // Пустой лист - шум: записи о нём нет, как нет и файла у модели без раскладки.
    if (Object.keys(edges).length + Object.keys(names).length + Object.keys(nodes).length === 0) continue;
    out.sheets[path] = { edges, names, nodes };
  }
  return out;
}

/** Точки излома: только пары конечных чисел, округлённые до целых. */
function cleanPoints(points) {
  return (Array.isArray(points) ? points : [])
    .filter((p) => Array.isArray(p) && p.length === 2 && p.every(finite))
    .map((p) => [whole(p[0]), whole(p[1])]);
}

/** Место знака: базовое без координат, своё - с ними; центр - умолчание. */
function cleanLabel(label) {
  if (!isObject(label) || !LABEL_PLACES.includes(label.place)) return null;
  if (label.place === "own") {
    if (!finite(label.x) || !finite(label.y)) return null;
    return { place: "own", x: whole(label.x), y: whole(label.y) };
  }
  if (label.place === "center") return null;
  return { place: label.place };
}

/** Место легенды: полка снизу - умолчание; у плавающей - координаты. */
function cleanLegend(legend) {
  if (!isObject(legend) || !LEGEND_PLACES.includes(legend.place)) return null;
  if (legend.place === "bottom") return null;
  const out = { place: legend.place };
  if (legend.place === "float") {
    out.x = finite(legend.x) ? whole(legend.x) : 0;
    out.y = finite(legend.y) ? whole(legend.y) : 0;
  }
  return out;
}

function isObject(value) {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

function finite(value) {
  return typeof value === "number" && Number.isFinite(value);
}

function isPoint(value) {
  return isObject(value) && finite(value.x) && finite(value.y);
}

function whole(value) {
  return Math.round(value);
}

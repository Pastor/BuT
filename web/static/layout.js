// Файл раскладки схемы: чтение, канон, сверка с графом, запись.
//
// # Что хранится
//
// Раскладка - работа автора: где стоят карточки состояний, как изломаны рёбра, какими
// словами подписаны знаки, где стоит легенда и каким начертанием нарисована схема.
// Файл лежит рядом с моделью (`elevator.takt` -> `elevator.takt-ui`).
//
// Размеров в файле нет: настройки вида записаны ступенями ("тонкая", "обычная",
// "жирная"), а числа за ступенями задаёт оформление. Запиши сюда пиксели - и вид
// схемы окажется закреплён файлом автора мимо оформления страницы, а сменить шкалу
// станет нельзя, не переписав чужие файлы.
//
// # Форма
//
//   { "format": 1, "corners": "square" | "round" | "bezier",
//     "meta": { "createdBy": "автор", "createdAt": "2026-09-08T18:00:00Z",
//               "edits": [ { "by": "кто", "at": "2026-09-08T18:20:00Z" } ] },
//     "labelPlace": "start" | "center" | "end",
//     "view": { "edgeWidth": "thin" | "normal" | "bold", "nodeWidth": тоже,
//               "arrow": "open" | "solid" | "line",
//               "stateFont" | "condFont": "gost" | "mono",
//               "stateSize" | "condSize": "xs" | "sm" | "md" | "lg",
//               "gamma": "draft" | "color" | "contrast",
//               "grid": "off" | "small" | "medium" | "large",
//               "snap": true | false, "marks": true | false,
//               "crossing": "hop" | "gap" },
//     "legend": { "place": "bottom" | "right" | "float", "x": 24, "y": 72 },
//     "sheets": { "<путь листа>": {
//         "entry": 0,
//         "nodes": { "<имя>": { "x": 0, "y": 0 } },
//         "names": { "<имя>": "подпись автора" },
//         "edges": { "<от>><к>:<номер>": {
//             "points": [[x, y], ...],
//             "label": { "place": "start" | "center" | "end" | "own", "x": 0, "y": 0 },
//             "ends": { "from": 0, "to": 8 },
//             "name": "подпись условия" } } } } }
//
// Запись каноническая: ключи отсортированы, отступ два пробела, числа целые, перевод
// строки в конце, умолчания и пустые листы не пишутся - дифф файла в git несёт только
// смысл.
//
// # Кто и когда
//
// Файл называет создателя и время создания, а правки ведёт журналом: строка на
// сеанс правки, не чаще раза в час на одного человека, последние двадцать. Час -
// не украшение: раскладку правят перетаскиванием, и запись на каждое движение
// узла сделала бы журнал длиннее самой раскладки. Предел тоже: без него файл
// растёт бесконечно и перестаёт читаться глазами.
//
// Читатель без входа записывается гостем (`guest`): имени у него нет, а знать,
// что файл правили, полезно.
//
// # Расхождение с моделью
//
// Файл - подсказка, ключ - имя. Состояние без записи ставится автоматически и
// помечается неразмещённым; запись без состояния читается, не рисуется и снимается
// при записи. Отказа нет ни в каком случае: компилятор файла не читает.
//
// Знания о языке здесь нет: что есть на листе, говорит граф модуля (`takt_graph`).

import { ENTRY_PORT, PORTS } from "./scheme-geometry.js";

/** Версия формата файла. */
export const FORMAT = 1;

/** Формы углов ребра. */
export const CORNERS = ["square", "round", "bezier"];

/** Места легенды относительно холста. */
export const LEGEND_PLACES = ["bottom", "right", "float"];

/** Места знака условия относительно стрелки; `own` - своё, с координатами. */
export const LABEL_PLACES = ["start", "center", "end", "own"];

/**
 * Настройки вида: наборы ступеней и умолчания.
 *
 * Ступень, а не число: файл хранит решение автора ("линия тонкая"), а не его
 * оформление в пикселях. Первое значение набора - умолчание, и в файл оно не
 * пишется: канон несёт только отличия от вида по умолчанию.
 */
export const VIEW = {
  edgeWidth: ["thin", "normal", "bold"],
  nodeWidth: ["normal", "thin", "bold"],
  arrow: ["open", "solid", "line"],
  // Подписи состояний и подписи условий набираются порознь: знак состояния читают
  // на рисунке, а цитату условия - как код, и одна гарнитура на двоих означала бы,
  // что выбор для одного навязан другому.
  stateFont: ["gost", "mono"],
  stateSize: ["md", "sm", "lg"],
  condFont: ["gost", "mono"],
  condSize: ["sm", "xs", "md"],
  gamma: ["color", "draft", "contrast"],
  grid: ["medium", "small", "large", "off"],
  snap: [true, false],
  marks: [true, false],
  // Пересечение рёбер: мостик либо разрыв линии. Оба говорят "не соединено";
  // мостик виднее на мелком масштабе, разрыв тише на плотном листе.
  crossing: ["hop", "gap"],
};

/** Настройки вида по умолчанию. */
export function defaultView() {
  const out = {};
  for (const [key, values] of Object.entries(VIEW)) out[key] = values[0];
  return out;
}

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
    !record.name &&
    !(isObject(record.ends) && Object.keys(record.ends).length > 0);
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
 * Имя, которым записывается правка читателя без входа.
 *
 * Латиницей и в файле: это идентификатор записи, а не подпись читателю. Русское
 * слово в файле зависело бы от языка той страницы, где правку сделали, - и один
 * файл нёс бы "гость" и "guest" вперемешку.
 */
export const GUEST = "guest";

/** Сколько записей журнала правок хранится; старые вытесняются. */
export const EDITS_KEPT = 20;

/** Насколько частые правки одного человека сливаются в одну запись, мс. */
const EDIT_WINDOW = 60 * 60 * 1000;

/**
 * Отмечает правку раскладки: создателя - однажды, правившего - журналом.
 *
 * Запись создателя ставится при первой же правке пустого файла и больше не
 * меняется: создатель у файла один. Журнал ведётся по правилу "строка на сеанс":
 * правка тем же человеком в пределах часа обновляет время последней строки, а не
 * заводит новую - иначе перетаскивание одного узла дало бы десяток записей.
 *
 * @param {object} layout раскладка (меняется на месте)
 * @param {string} who имя правившего; пусто - гость
 * @param {number} now время правки (мс эпохи)
 */
export function touch(layout, who, now = Date.now()) {
  const by = typeof who === "string" && who.trim() !== "" ? who.trim() : GUEST;
  const at = new Date(now).toISOString().replace(/\.\d{3}Z$/, "Z");
  if (!isObject(layout.meta)) layout.meta = {};
  const meta = layout.meta;
  if (typeof meta.createdBy !== "string" || meta.createdBy === "") {
    meta.createdBy = by;
    meta.createdAt = at;
  }
  const edits = Array.isArray(meta.edits) ? meta.edits : [];
  const last = edits[edits.length - 1];
  if (last && last.by === by && now - Date.parse(last.at) < EDIT_WINDOW) last.at = at;
  else edits.push({ at, by });
  meta.edits = edits.slice(-EDITS_KEPT);
  return layout;
}

/** Сведения о создателе и правках; пусто - файла ещё никто не трогал. */
export function metaOf(layout) {
  return cleanMeta(layout?.meta);
}

/**
 * Ставит настройку вида; значение вне набора и умолчание записи не оставляют.
 *
 * @param {object} layout раскладка (меняется на месте)
 * @param {string} key имя настройки из `VIEW`
 * @param {string|boolean} value ступень
 */
export function setView(layout, key, value) {
  const values = VIEW[key];
  if (!values || !values.includes(value)) return layout;
  if (!isObject(layout.view)) layout.view = {};
  if (value === values[0]) delete layout.view[key];
  else layout.view[key] = value;
  if (Object.keys(layout.view).length === 0) delete layout.view;
  return layout;
}

/**
 * Раскладка при открытии модели: черновика либо проекта.
 *
 * Черновик сильнее проекта, как и для текста модели: автор расставил узлы и не
 * сохранил, а перезагрузка страницы не вправе стереть эту работу. Сравниваются
 * канонические формы - файл проекта мог быть записан не каноном, и различие
 * пробелов не правка. Пустая раскладка черновика ничего не говорит: черновик
 * пишет её и тогда, когда схему не открывали.
 *
 * @param {string} draftText раскладка из черновика (пусто - черновика нет)
 * @param {string} savedText раскладка из файла проекта
 * @returns {{text: string, fromDraft: boolean}}
 */
export function preferDraft(draftText, savedText) {
  const saved = savedText ?? "";
  if (typeof draftText !== "string" || draftText === "") return { text: saved, fromDraft: false };
  const own = canonical(parse(draftText).layout);
  if (own === canonical(empty()) || own === canonical(parse(saved).layout)) return { text: saved, fromDraft: false };
  return { text: draftText, fromDraft: true };
}

/** Настройки вида раскладки, дополненные умолчаниями. */
export function viewOf(layout) {
  return { ...defaultView(), ...cleanView(layout?.view) };
}

/** Номер точки привязки: целое от нуля до числа точек на рамке узла. */
const isPort = (value) => Number.isInteger(value) && value >= 0 && value < PORTS;

/**
 * Ставит точку привязки стрелки начального состояния листа; умолчание (слева)
 * и негодный номер записи не оставляют.
 *
 * Место стрелки - свойство листа, а не узла: начальное состояние у листа одно,
 * и запись переживает переименование состояния.
 */
export function entryAt(layout, path, port) {
  const stored = sheet(layout, path);
  if (isPort(port) && port !== ENTRY_PORT) stored.entry = port;
  else delete stored.entry;
  return layout;
}

/**
 * Закрепляет конец ребра за точкой привязки узла; `null` снимает закрепление.
 *
 * Конец без записи ставит раздача (`geo.routeSheet`): автор закрепляет только те,
 * что ему нужны на своём месте, и остальные концы продолжают обходить занятые.
 *
 * @param {"from"|"to"} side конец ребра: у источника либо у цели
 */
export function endAt(layout, path, key, side, port) {
  const record = edgeRecord(layout, path, key);
  const ends = isObject(record.ends) ? record.ends : {};
  if (isPort(port)) ends[side] = port;
  else delete ends[side];
  if (Object.keys(ends).length > 0) record.ends = ends;
  else delete record.ends;
  dropIfBare(layout, path, key);
  return layout;
}

/** Закреплённые концы ребра: `{from?, to?}`, негодные номера отброшены. */
export function endsOf(record) {
  const out = {};
  for (const side of ["from", "to"]) {
    if (isPort(record?.ends?.[side])) out[side] = record.ends[side];
  }
  return out;
}

/** Точка привязки стрелки начального состояния листа. */
export function entryOf(layout, path) {
  const port = layout?.sheets?.[path]?.entry;
  return isPort(port) ? port : ENTRY_PORT;
}

/**
 * Ставит умолчание места знака условия; центр - умолчание и не записывается.
 *
 * Место каждого ребра сильнее умолчания: автор мог отвести один знак от стрелки
 * руками, и смена общего правила не вправе стирать эту работу.
 */
export function labelPlaceAt(layout, placeName) {
  if (placeName === "center" || !LABEL_PLACES.includes(placeName) || placeName === "own") {
    delete layout.labelPlace;
    return layout;
  }
  layout.labelPlace = placeName;
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
  if (["start", "end"].includes(raw?.labelPlace)) out.labelPlace = raw.labelPlace;
  const meta = cleanMeta(raw?.meta);
  if (meta) out.meta = meta;
  const view = cleanView(raw?.view);
  if (Object.keys(view).length > 0) out.view = view;
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
      const ends = endsOf(record);
      if (Object.keys(ends).length > 0) clean.ends = ends;
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
    const entry = isPort(stored.entry) && stored.entry !== ENTRY_PORT ? stored.entry : null;
    // Пустой лист - шум: записи о нём нет, как нет и файла у модели без раскладки.
    if (Object.keys(edges).length + Object.keys(names).length + Object.keys(nodes).length === 0 && entry === null) continue;
    out.sheets[path] = entry === null ? { edges, names, nodes } : { edges, entry, names, nodes };
  }
  return out;
}

/**
 * Сведения о создателе и правках: строки и время, журнал - в пределе.
 *
 * Негодная запись отбрасывается молча, как и чужая ступень: файл - подсказка, и
 * испорченная запись об авторстве не вправе ни рисоваться, ни доживать до записи.
 */
function cleanMeta(raw) {
  if (!isObject(raw)) return null;
  const out = {};
  if (typeof raw.createdBy === "string" && raw.createdBy.trim() !== "") {
    out.createdBy = raw.createdBy.trim();
    if (isTime(raw.createdAt)) out.createdAt = raw.createdAt;
  }
  const edits = (Array.isArray(raw.edits) ? raw.edits : [])
    .filter((e) => isObject(e) && typeof e.by === "string" && e.by.trim() !== "" && isTime(e.at))
    .map((e) => ({ at: e.at, by: e.by.trim() }))
    .slice(-EDITS_KEPT);
  if (edits.length > 0) out.edits = edits;
  return Object.keys(out).length > 0 ? out : null;
}

/** Время записи: только форма ISO с секундами и зоной UTC. */
function isTime(value) {
  return typeof value === "string" && /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$/.test(value)
    && !Number.isNaN(Date.parse(value));
}

/**
 * Настройки вида: только известные ключи с известными ступенями, умолчания сняты.
 *
 * Незнакомая настройка отбрасывается молча, как и запись листа без узла: файл -
 * подсказка, и чужая ступень не вправе ни рисоваться, ни доживать до записи.
 */
function cleanView(raw) {
  const out = {};
  if (!isObject(raw)) return out;
  for (const key of Object.keys(VIEW).sort()) {
    const values = VIEW[key];
    const value = raw[key];
    if (values.includes(value) && value !== values[0]) out[key] = value;
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

// Холст схемы: лист, узлы, рёбра, крошки, миникарта, панель, правка раскладки.
//
// # Что откуда
//
// Что рисовать, говорит граф модуля (`takt_graph`): листы, узлы с видом и позициями
// в тексте, рёбра с цитатой условия, ярусы. Где рисовать - файл раскладки автора
// (`layout.js`): центры узлов, изломы рёбер, подписи, место знака и легенды. Здесь
// сходятся оба: узел без записи встаёт по ярусу и помечается неразмещённым, запись
// без узла называется в уведомлении и снимается по просьбе.
//
// Знания о языке здесь нет ни строки: страница не разбирает текст модели.
//
// # Что схема не делает
//
// Не редактирует модель: добавления состояния, ребра и правки условия нет ни жестом,
// ни клавишей - текст остаётся единственным описанием автомата. Не сохраняет молча:
// каждое изменение раскладки уходит вызывающему, а тот пишет черновик и проект по
// общим правилам страницы.

import * as geo from "./scheme-geometry.js";
import * as layoutFile from "./layout.js";
import { paintLegend, paintNav, tipOf } from "./legend.js";
import { Settings } from "./scheme-settings.js";
import { Panels, PANELS } from "./panels.js";

const NS = "http://www.w3.org/2000/svg";

/** Узел SVG с атрибутами. */
function mk(tag, attrs = {}) {
  const node = document.createElementNS(NS, tag);
  for (const [key, value] of Object.entries(attrs)) node.setAttribute(key, String(value));
  return node;
}

/** Ширина подложки под текст знака или подписи: по числу знаков кода. */
/**
 * Знак с подложкой: подложка обычно невидима и проявляется под указателем.
 *
 * Знак и подложка лежат в своей группе, а не рядом в общей: правило наведения
 * должно смотреть на пару целиком - иначе рамка вспыхивала бы от прикосновения к
 * линии, проходящей мимо, и гасла бы, стоит указателю попасть между буквами.
 */
function backing(parent, cx, cy, text, cls) {
  const w = text.length * 8 + 8;
  const box = mk("g", { class: `${cls}-box` });
  box.appendChild(mk("rect", { class: `${cls}-bg`, x: cx - w / 2, y: cy - 11, width: w, height: 17, rx: 3 }));
  const node = mk("text", { class: cls, x: cx, y: cy + 2 });
  node.textContent = text;
  box.appendChild(node);
  parent.appendChild(box);
  return node;
}

/**
 * Знак с номером нижним индексом, рисованным смещением базовой линии.
 *
 * Смещение задано долей кегля (`em`), а не числом: кегль знака выбирает автор
 * настройками, и номер, смещённый на постоянные четыре единицы, при крупном знаке
 * оказывался бы почти на его базовой линии.
 */
function markText(node, mark, numClass) {
  node.textContent = mark.slice(0, 1);
  const num = mk("tspan", { class: numClass, dy: "0.3em" });
  num.textContent = mark.slice(1);
  node.appendChild(num);
}

/**
 * Наконечники: два вида (переход и продолжение) по два состояния - маркер не
 * наследует чернила ссылающегося ребра.
 *
 * Форма задаётся настройкой вида: `open` - раскрытая стрелка, `solid` - залитый
 * треугольник, `line` - узкий штрих. Форма и размер стоят рядом: узкий штрих
 * длиннее раскрытой стрелки, иначе на тонкой линии он читается точкой.
 */
const ARROWS = {
  open: { d: "M1 1L9 5L1 9", close: "", size: 7 },
  solid: { d: "M1 1L9 5L1 9", close: "z", size: 7 },
  line: { d: "M2 2L9 5L2 8", close: "", size: 9 },
};

/** Шаг сетки ступенями: множитель к `geo.SNAP`; `off` - сетки нет. */
const GRID_STEPS = { small: 2, medium: 3, large: 5, off: 0 };

function defs(arrow = "open") {
  const shape = ARROWS[arrow] ?? ARROWS.open;
  const out = mk("defs");
  for (const [id, cls] of [
    ["arrow-open", "arrow-open"],
    ["arrow-solid", "arrow-solid"],
    ["arrow-open-sel", "arrow-open-sel"],
    ["arrow-solid-sel", "arrow-solid-sel"],
    ["arrow-open-next", "arrow-open-next"],
    ["arrow-solid-next", "arrow-solid-next"],
  ]) {
    // Залитый наконечник у ребра `next` остаётся залитым при любой форме: вид
    // ребра говорит о роде перехода, а настройка - о рисунке стрелки.
    const close = id.startsWith("arrow-solid") ? "z" : shape.close;
    const marker = mk("marker", {
      id,
      viewBox: "0 0 10 10",
      refX: 9,
      refY: 5,
      markerWidth: shape.size,
      markerHeight: shape.size,
      orient: "auto-start-reverse",
    });
    marker.appendChild(mk("path", { class: cls, d: `${shape.d}${close}` }));
    out.appendChild(marker);
  }
  return out;
}

/** Холст схемы. */
/**
 * Мера щипка: расстояние между двумя пальцами и точка между ними.
 *
 * Точка нужна опорой масштаба: лист растёт от того места, которое держат, а не
 * от угла холста, - иначе рисунок уезжает из-под пальцев.
 */
function span(touches) {
  const [a, b] = [...touches.values()];
  return {
    dist: Math.hypot(a.clientX - b.clientX, a.clientY - b.clientY),
    x: (a.clientX + b.clientX) / 2,
    y: (a.clientY + b.clientY) / 2,
  };
}

/**
 * Текст плашки композиции: активные внутренние состояния - подписью автора, если
 * она есть, иначе именем (у шага композиции - именем его модели); у параллели их
 * несколько, через запятую.
 *
 * @param {{name: string, alias?: string, model?: string}[]} nodes узлы внутреннего листа
 * @param {Set<string>} running активные состояния такта (имена всех уровней)
 */
export function innerLabel(nodes, running) {
  return nodes
    .filter((node) => running.has(node.name))
    .map((node) => node.alias || node.model || node.name)
    .join(", ");
}

/**
 * Модели, стоящие в выражениях реализации графа: у них есть шаги на листах
 * композиций.
 *
 * @param {{sheets: object[]}|null} graph ответ `takt_graph`
 * @returns {Set<string>} имена моделей так, как они записаны в выражении
 */
export function composedModels(graph) {
  const out = new Set();
  const walk = (item) => {
    if (!item) return;
    if (item.model) out.add(item.model.name);
    else if (item.group) walk(item.group);
    else for (const child of item.chain ?? item.parallel ?? []) walk(child);
  };
  for (const sheet of graph?.sheets ?? []) for (const node of sheet.nodes) walk(node.implements);
  return out;
}

/**
 * Как назвать лист раскладки в уведомлении: путь модели, а у листа композиции -
 * путь владельца и имя состояния через ту же косую черту. Корень - пустая строка.
 * Служебный ключ `/#Main` автору ничего не говорит, `Main` - говорит.
 */
export function sheetLabel(key) {
  const [owner, state] = key.split("#");
  const path = owner === "/" ? "" : owner;
  if (state === undefined) return path;
  return path === "" ? state : `${path}/${state}`;
}

/** Запас белого поля маски за краем листа: больше любого переноса за один жест. */
const REACH_ALL = 100000;

/**
 * Рёбра, которые сработают на следующем такте: ожидаемый переход модуля - пара
 * "из, в", и ребро узнаётся по ней, если его начало сейчас активно.
 *
 * @param {{key: string, from: string, to: string}[]} edges рёбра листа
 * @param {Set<string>} running активные состояния такта
 * @param {string[][]} next ожидаемые переходы парами "из, в"
 * @returns {Set<string>} ключи рёбер
 */
export function nextEdgeKeys(edges, running, next) {
  const pairs = new Set((next ?? []).filter((pair) => running.has(pair[0])).map((pair) => `${pair[0]}\u0000${pair[1]}`));
  return new Set(edges.filter((edge) => pairs.has(`${edge.from}\u0000${edge.to}`)).map((edge) => edge.key));
}

/** Наконечник ребра: род перехода задаёт форму, выбор и прогон - чернила. */
function markerOf(kind, selected, next) {
  return `${kind === "next" ? "arrow-solid" : "arrow-open"}${selected ? "-sel" : next ? "-next" : ""}`;
}

/** Предел между касаниями двойного касания, мс. */
const DOUBLE_TAP_MS = 400;

/**
 * Двойное касание: второе касание того же узла вскоре после первого.
 *
 * @param {{name: string, at: number}|null|undefined} last прежнее касание
 * @param {string} name узел нынешнего касания
 * @param {number} at время нынешнего касания, мс
 */
export function doubleTap(last, name, at) {
  return Boolean(last) && last.name === name && at - last.at <= DOUBLE_TAP_MS;
}

export class Scheme {
  /**
   * @param {object} dom узлы: `scheme`, `sheet`, `map`, `stage`, `legend`, `crumbs`,
   *   `nav`, `tools`, `empty`, `notice`, `noticeText`, `noticeDrop`, `zoom`
   * @param {object} options `{t, onSelect(node), onChange()}`
   */
  constructor(dom, options) {
    this.dom = dom;
    this.t = options.t;
    this.onSelect = options.onSelect ?? (() => {});
    this.onChange = options.onChange ?? (() => {});
    // Кто правит - спрашивается у страницы на каждую правку, а не запоминается:
    // вход и выход случаются посреди работы над схемой.
    this.who = options.who ?? (() => "");
    // Настройки страницы окно показывает, но не держит: язык и перенос строк -
    // дело страницы, и второй их носитель разошёлся бы с первым молча.
    this.pageValues = options.pageValues ?? (() => ({}));
    this.onPage = options.onPage ?? (() => {});
    this.graph = null;
    this.layout = layoutFile.empty();
    this.undo = [];
    this.redo = [];
    this.trail = [];
    this.view = { k: 1, x: 0, y: 0 };
    this.selected = null;
    this.selectedEdge = null;
    this.running = new Set();
    this.expected = new Set();
    this.nextEdges = new Set();
    this.fitPending = true;
    // Счётчик масок щели под знаком: имя маски обязано быть своим у каждого ребра,
    // а ключ ребра содержит знаки, которых в имени быть не может.
    this.gapSeq = 0;
    // Панели холста: где стоят и показывать ли их. Хозяин у них здесь - холст
    // им место, а окно настроек спрашивает у той же стороны, что и вид листа.
    this.panels = dom.panels
      ? new Panels(
          { scheme: dom.scheme, panels: dom.panels, docks: dom.docks, legendSplits: dom.legendSplits },
          { store: options.store },
        )
      : null;
    this.kinds = {
      start: this.t("scheme.kind.start"),
      state: this.t("scheme.kind.state"),
      end: this.t("scheme.kind.end"),
      composition: this.t("scheme.kind.composition"),
    };
    this.wire();
  }

  // ── Вход: граф и раскладка ──────────────────────────────────────────────

  /** Принимает граф модуля; `null` - модель не разбирается. */
  setGraph(graph) {
    this.graph = graph?.ok ? graph : null;
    this.trail = this.trail.filter((level) => this.sheetOf(level) !== null);
    if (this.trail.length === 0) this.trail = [{ kind: "model", path: "/" }];
    if (this.selected && !this.current().nodes.some((n) => n.name === this.selected)) this.selected = null;
    this.draw();
    this.checkStale();
  }

  /** Принимает раскладку (объект `layout.js`); история отмены сбрасывается. */
  setLayout(layout) {
    this.layout = layout ?? layoutFile.empty();
    this.undo = [];
    this.redo = [];
    this.fitPending = true;
    this.placeLegend(this.layout.legend?.place ?? "bottom", false);
    this.draw();
    this.checkStale();
  }

  /** Текущая раскладка каноническим текстом. */
  text() {
    return layoutFile.canonical(this.layout);
  }

  /** Подсвечивает узел активного листа по позиции курсора в тексте. */
  highlight(line, character) {
    const sheet = this.current();
    if (!sheet.editable) return;
    const hit = sheet.nodes.find((n) => {
      const r = n.range;
      if (!r) return false;
      const after = line > r.start_line || (line === r.start_line && character >= r.start_character);
      const before = line < r.end_line || (line === r.end_line && character <= r.end_character);
      return after && before;
    });
    if (hit && hit.name !== this.selected) {
      this.selected = hit.name;
      this.focusEdge(null);
      this.applySelection();
    }
  }

  /**
   * Переименование символа из редактора переносит записи раскладки: состояния - на
   * листах, модели - в шагах листов композиций.
   *
   * Род символа редактор не сообщает, и спрашивается граф до правки: шаги
   * переносятся, только если такая модель стоит в выражении реализации. Иначе
   * переименование состояния, совпавшего по имени с моделью, увело бы чужие записи.
   */
  renamed(from, to) {
    const before = this.text();
    for (const path of Object.keys(this.layout.sheets ?? {})) {
      this.layout = layoutFile.rename(this.layout, path, from, to);
    }
    if (composedModels(this.graph).has(from)) this.layout = layoutFile.renameModel(this.layout, from, to);
    if (this.selected === from) this.selected = to;
    if (this.text() !== before) this.commit(before);
  }

  /**
   * Подсветка прогона: активные состояния, состояния, куда есть переход, и состояния,
   * куда переход ожидается при нынешних значениях. Вид переключается без перестроения
   * листа.
   *
   * @param {string[]} names активные состояния такта
   * @param {string[][]} next ожидаемые переходы парами "из, в"
   */
  setRunning(names, next = []) {
    this.runningBefore = this.running;
    this.running = new Set(names ?? []);
    this.expected = new Set((next ?? []).filter((pair) => this.running.has(pair[0])).map((pair) => pair[1]));
    const sheet = this.current();
    this.nextEdges = nextEdgeKeys(sheet.edges, this.running, next);
    const reachable = new Set(
      sheet.edges.filter((e) => this.running.has(e.from)).map((e) => e.to),
    );
    for (const node of this.dom.sheet.querySelectorAll(".node")) {
      const name = node.dataset.name;
      const running = this.running.has(name);
      node.classList.toggle("running", running);
      node.classList.toggle("expected", !running && this.expected.has(name));
      node.classList.toggle("reachable", !running && !this.expected.has(name) && reachable.has(name));
    }
    // Стрелка, которая сработает, - в пару к состоянию, куда уйдёт автомат: без
    // неё из нескольких выходящих переходов нужный угадывался по условиям.
    for (const group of this.dom.sheet.querySelectorAll(".edge-group")) {
      const next = this.nextEdges.has(group.dataset.key);
      group.classList.toggle("next", next);
      group.querySelector(".edge")?.setAttribute("marker-end", `url(#${markerOf(group.dataset.kind, group.dataset.key === this.selectedEdge, next)})`);
    }
    this.paintInner();
  }

  /** Снимает записи, которых в модели нет. */
  dropStale() {
    if (!this.graph) return;
    const before = this.text();
    this.layout = layoutFile.prune(this.layout, this.graph);
    this.commit(before);
    this.checkStale();
  }

  // ── Уровни и листы ──────────────────────────────────────────────────────

  /** Лист графа по пути; `null` - его нет. */
  graphSheet(path) {
    return this.graph?.sheets?.find((s) => s.path === path) ?? null;
  }

  /** Данные листа по записи уровня; `null` - уровня больше нет. */
  sheetOf(level) {
    if (!this.graph) return null;
    if (level.kind === "model") {
      const found = this.graphSheet(level.path);
      return found ? this.modelSheet(found) : null;
    }
    const owner = this.graphSheet(level.sheetPath);
    const node = owner?.nodes.find((n) => n.name === level.node);
    if (!node?.implements) return null;
    return this.compositionSheet(node, level.sheetPath);
  }

  /** Активный лист; без графа - пустой. */
  current() {
    const level = this.trail[this.trail.length - 1];
    return (level && this.sheetOf(level)) ?? { key: "", title: "", editable: false, nodes: [], edges: [], frames: [], ox: 0, oy: 0, w: geo.COL * 2, h: geo.ROW * 2 };
  }

  /** Лист модели: узлы по раскладке либо по ярусам, рёбра с изломами. */
  modelSheet(found) {
    const stored = this.layout.sheets?.[found.path] ?? {};
    const auto = geo.autoPlace(found.nodes);
    const ordered = [...found.nodes].sort((a, b) => a.rank - b.rank || a.order - b.order);
    const nodes = ordered.map((n, i) => {
      const kept = stored.nodes?.[n.name];
      return {
        name: n.name,
        kind: n.kind,
        start: n.start,
        x: kept?.x ?? auto[n.name].x,
        y: kept?.y ?? auto[n.name].y,
        unplaced: !kept,
        alias: stored.names?.[n.name] ?? "",
        range: n.range,
        nameRange: n.name_range,
        implements: n.implements,
        mark: `S${i + 1}`,
      };
    });
    let k = 0;
    const edges = found.edges.map((e) => {
      const key = layoutFile.edgeKey(e);
      const record = stored.edges?.[key] ?? {};
      return {
        key,
        from: e.from,
        to: e.to,
        ordinal: e.ordinal,
        kind: e.kind,
        cond: e.condition ?? null,
        alias: record.name ?? "",
        points: record.points ?? [],
        ends: layoutFile.endsOf(record),
        label: record.label ?? null,
        loop: e.from === e.to,
        range: e.range,
        mark: e.condition ? `K${(k += 1)}` : "",
      };
    });
    // Своё место знака - тоже часть рисунка: автор отводит знак от линии, и лист
    // обязан его вместить так же, как излом.
    const spots = edges.flatMap((e) => (e.label?.place === "own" ? [[e.label.x, e.label.y]] : []));
    const size = geo.sheetSize(nodes, [...edges.flatMap((e) => e.points), ...spots]);
    return {
      key: found.path,
      path: found.path,
      title: found.path === "/" ? this.t("scheme.root") : found.name,
      editable: true,
      nodes,
      edges,
      frames: [],
      ...size,
    };
  }

  /**
   * Лист композиции: состав шагов и рамок задаёт выражение, положение - запись
   * листа в раскладке под ключом "путь листа-владельца#составное состояние".
   *
   * Шаг без записи стоит по форме выражения и помечен неразмещённым - те же
   * правила, что у листа модели. Рамки скобок и параллели следуют за шагами.
   */
  compositionSheet(node, sheetPath) {
    const composed = geo.composeSheet(node.implements);
    const key = layoutFile.compositionKey(sheetPath, node.name);
    const stored = this.layout.sheets?.[key] ?? {};
    const nodes = composed.nodes.map((n, i) => {
      const kept = stored.nodes?.[n.name];
      return {
        name: n.name,
        kind: "composition",
        start: false,
        x: kept?.x ?? n.x,
        y: kept?.y ?? n.y,
        unplaced: !kept,
        alias: stored.names?.[n.name] ?? "",
        model: n.model,
        implements: n.path ? { model: { name: n.model, path: n.path } } : null,
        mark: `S${i + 1}`,
      };
    });
    const edges = composed.edges.map((e) => {
      const edgeKey = layoutFile.edgeKey(e);
      const record = stored.edges?.[edgeKey] ?? {};
      return {
        key: edgeKey,
        from: e.from,
        to: e.to,
        ordinal: e.ordinal,
        kind: "next",
        cond: null,
        alias: "",
        points: record.points ?? [],
        ends: layoutFile.endsOf(record),
        label: null,
        loop: false,
        mark: "",
      };
    });
    const frames = geo.framesOf(node.implements, new Map(nodes.map((n) => [n.name, n])));
    // Рамка - часть рисунка: шаг, унесённый к краю, тянет за собой свою рамку, и
    // лист обязан вместить её так же, как излом.
    const corners = frames.flatMap((f) => [[f.x, f.y], [f.x + f.w, f.y + f.h]]);
    const size = geo.sheetSize(nodes, [...edges.flatMap((e) => e.points), ...corners]);
    return {
      key,
      // Пути модели у листа композиции нет: во вложенную композицию с него не
      // входят, а запись идёт по ключу листа.
      path: null,
      title: node.name,
      editable: true,
      nodes,
      edges,
      frames,
      ...size,
    };
  }

  /** Куда ведёт вход в узел; `null` - входа нет. */
  target(sheet, node) {
    const impl = node.implements;
    if (!impl) return null;
    if (impl.model) return impl.model.path ? { kind: "model", path: impl.model.path } : null;
    if (sheet.path === null) return null;
    return { kind: "composition", sheetPath: sheet.path, node: node.name };
  }

  /** Вход в композицию узла активного листа. */
  enter(name) {
    const sheet = this.current();
    const node = sheet.nodes.find((n) => n.name === name);
    const level = node ? this.target(sheet, node) : null;
    if (!level) return;
    // Фокус снимается до смены листа: правило звеньев работает на том листе, где
    // ребро живёт, а после смены его там уже нет.
    this.focusEdge(null);
    this.trail = [...this.trail, level];
    this.selected = null;
    this.fitPending = true;
    this.draw();
  }

  /** Назад по крошкам; на корне - снятие выбора. */
  back() {
    this.focusEdge(null);
    if (this.trail.length > 1) {
      this.trail = this.trail.slice(0, -1);
      this.selected = null;
      this.fitPending = true;
      this.draw();
    } else if (this.selected) {
      this.selected = null;
      this.draw();
    }
  }

  // ── Правка раскладки ────────────────────────────────────────────────────

  /** Запоминает прежнее состояние, перерисовывает и сообщает вызывающему. */
  commit(before) {
    if (before === this.text()) return;
    // Кто и когда правил - часть файла раскладки, и отмечается это здесь: одна
    // точка фиксации правки на весь холст. Отмечай в местах правки - и первая же
    // забытая точка дала бы файл, о правке которого никто не узнал.
    layoutFile.touch(this.layout, this.who());
    this.undo.push(before);
    if (this.undo.length > 100) this.undo.shift();
    this.redo = [];
    this.draw();
    this.onChange();
  }

  undoLast() {
    const before = this.undo.pop();
    if (before === undefined) return;
    this.redo.push(this.text());
    this.layout = layoutFile.parse(before).layout;
    this.draw();
    this.onChange();
  }

  redoLast() {
    const next = this.redo.pop();
    if (next === undefined) return;
    this.undo.push(this.text());
    this.layout = layoutFile.parse(next).layout;
    this.draw();
    this.onChange();
  }

  /** Перенос узла: запись координат снимает метку "не размещён". */
  moveNode(name, x, y) {
    const sheet = this.current();
    if (!sheet.editable) return;
    const before = this.text();
    layoutFile.place(this.layout, sheet.key, name, this.snapped(x), this.snapped(y));
    this.commit(before);
  }

  /** Автораскладка листа: координаты и изломы снимаются, подписи остаются. */
  autoLayout() {
    const sheet = this.current();
    if (!sheet.editable) return;
    const before = this.text();
    const stored = layoutFile.sheet(this.layout, sheet.key);
    stored.nodes = {};
    for (const key of Object.keys(stored.edges)) layoutFile.bend(this.layout, sheet.key, key, []);
    this.commit(before);
  }

  setCorners(corners) {
    const before = this.text();
    this.layout.corners = corners;
    this.commit(before);
  }

  // ── Настройки отрисовки ─────────────────────────────────────────────────

  /**
   * Открывает окно настроек.
   *
   * Снимок раскладки снимается здесь: правка идёт по живой раскладке (лист
   * перерисовывается на каждый выбор), и "отменить" возвращает текст снимка
   * целиком. Пошаговой отмены у окна нет намеренно: настройка - не работа над
   * раскладкой, а её вид, и история отмены холста им не забивается.
   */
  openSettings() {
    if (!this.settings) return;
    this.settingsBefore = this.text();
    this.panelsBefore = this.panels?.snapshot() ?? null;
    this.settings.open();
    if (this.dom.settingsBody) this.dom.settingsBody.scrollTop = 0;
  }

  /** Ступени вида, показанные окну: настройки и то, что живёт рядом с ними. */
  settingValues() {
    const values = {
      ...layoutFile.viewOf(this.layout),
      corners: this.layout.corners ?? "square",
      labelPlace: this.labelPlace(),
    };
    // Видимость панелей стоит в том же окне, но живёт не в раскладке: она
    // принадлежит читателю. Ключ помечен, чтобы выбор ушёл своему хозяину.
    for (const panel of PANELS) values[`panel:${panel.id}`] = this.panels?.whenOf(panel.id);
    for (const [key, value] of Object.entries(this.pageValues())) values[`page:${key}`] = value;
    return values;
  }

  /** Выбор ступени в окне: раскладка правится сразу, лист перерисовывается. */
  pickSetting(key, value) {
    if (key.startsWith("page:")) {
      this.onPage(key.slice(5), value);
      return;
    }
    if (key.startsWith("panel:")) {
      this.panels?.setWhen(key.slice(6), value);
      return;
    }
    if (key === "corners") this.layout.corners = value;
    else if (key === "labelPlace") layoutFile.labelPlaceAt(this.layout, value);
    else layoutFile.setView(this.layout, key, value);
    this.draw();
  }

  /** Закрепление настроек: правка уходит вызывающему как всякая другая. */
  saveSettings() {
    this.commit(this.settingsBefore ?? this.text());
    this.settingsBefore = null;
    this.panelsBefore = null;
  }

  /** Отказ: раскладка возвращается к снимку, снятому при открытии окна. */
  cancelSettings() {
    if (this.panelsBefore) {
      this.panels?.restore(this.panelsBefore);
      this.panelsBefore = null;
    }
    if (this.settingsBefore === null || this.settingsBefore === undefined) return;
    this.layout = layoutFile.parse(this.settingsBefore).layout;
    this.settingsBefore = null;
    this.draw();
  }

  /** Место легенды: часть раскладки, а не настройка браузера. */
  placeLegend(placeName, remember = true) {
    const before = this.text();
    const { legend, stage, scheme } = this.dom;
    legend.classList.remove("legend-bottom", "legend-right", "legend-float");
    legend.classList.add(`legend-${placeName}`);
    stage.classList.toggle("legend-side-bottom", placeName !== "right");
    stage.classList.toggle("legend-side-right", placeName === "right");
    if (placeName === "float") {
      scheme.appendChild(legend);
      legend.style.left = `${this.layout.legend?.x ?? geo.SNAP * 3}px`;
      legend.style.top = `${this.layout.legend?.y ?? geo.SNAP * 9}px`;
    } else {
      stage.appendChild(legend);
      legend.style.left = "";
      legend.style.top = "";
    }
    for (const button of this.dom.tools.querySelectorAll("[data-act^='legend-']")) {
      button.setAttribute("aria-pressed", String(button.dataset.act === `legend-${placeName}`));
    }
    if (remember) {
      layoutFile.legendAt(this.layout, placeName, this.layout.legend?.x ?? geo.SNAP * 3, this.layout.legend?.y ?? geo.SNAP * 9);
      this.commit(before);
    }
  }

  // ── Отрисовка ───────────────────────────────────────────────────────────

  draw() {
    const { sheet: svg, scheme, empty } = this.dom;
    const sheet = this.current();
    const view = layoutFile.viewOf(this.layout);
    empty.hidden = this.graph !== null;
    // Масштаб задан размером самого рисунка, а не растяжением готовой картинки:
    // `viewBox` остаётся в единицах листа, а ширина и высота растут вместе с
    // масштабом - браузер рисует вектор в нужном разрешении. Растяни готовый SVG
    // трансформацией, и он масштабируется как растр: линии и знаки мылятся, и
    // заметнее всего это на подсветке прогона, где обводка толще.
    svg.setAttribute("viewBox", `${sheet.ox} ${sheet.oy} ${sheet.w} ${sheet.h}`);
    svg.setAttribute("width", sheet.w * this.view.k);
    svg.setAttribute("height", sheet.h * this.view.k);
    svg.replaceChildren(defs(view.arrow));
    // Ступени вида уходят на холст признаками, а числа за ними стоят в оформлении
    // (`app.css`): толщина линии, кегль и гамма - решения книги контролов, а файл
    // автора хранит выбор ступени, а не пиксели.
    for (const [key, value] of Object.entries(view)) {
      // Признак пишется через `dataset`, и имя ступени приезжает в разметку через
      // дефис (`edgeWidth` -> `data-edge-width`): правила оформления написаны в
      // этой же форме, и другая их не находит - нашлось прогоном страницы.
      scheme.dataset[key] = String(value);
    }
    // Вписать лист можно только в область ненулевого размера: пока панель скрыта,
    // просьба ждёт её появления (наблюдатель размера позовёт перерисовку).
    if (this.fitPending) {
      const box = scheme.getBoundingClientRect();
      if (box.width > 0 && box.height > 0) {
        this.fitPending = false;
        this.fit();
        return;
      }
    }
    const byName = new Map(sheet.nodes.map((n) => [n.name, n]));
    for (const frame of sheet.frames) {
      svg.appendChild(mk("rect", { class: "node-frame", x: frame.x, y: frame.y, width: frame.w, height: frame.h, rx: 12 }));
    }
    // Стрелка начального состояния занимает свою точку привязки: рёбра встают
    // в соседние, а не под неё.
    const reserved = new Map();
    for (const node of sheet.nodes) {
      if (node.start && node.kind !== "composition") reserved.set(node.name, layoutFile.entryOf(this.layout, sheet.key));
    }
    const routes = geo.routeSheet(byName, sheet.edges, reserved);
    this.routes = new Map();
    const drawn = [];
    sheet.edges.forEach((edge, i) => {
      const pts = routes[i];
      if (!pts) return;
      const hops = this.layout.corners === "bezier" ? [] : geo.crossings(pts, drawn);
      const covered = this.layout.corners === "bezier" ? null : geo.overlapWith(pts, drawn);
      drawn.push(pts);
      this.routes.set(edge.key, pts);
      this.drawEdge(sheet, edge, pts, hops, covered);
    });
    for (const node of sheet.nodes) this.drawNode(sheet, node);
    this.paintInner();
    svg.style.transformOrigin = "0 0";
    svg.style.transform = `translate(${this.view.x}px, ${this.view.y}px)`;
    // Сетка холста едет с листом: шаг масштабируется, а при мелком масштабе удваивается,
    // чтобы точки не сливались в заливку; начало сетки - начало листа. Шаг задан
    // ступенью настроек; `off` снимает сетку вовсе - подложку гасит оформление.
    const cells = GRID_STEPS[view.grid] ?? GRID_STEPS.medium;
    let step = geo.SNAP * cells * this.view.k;
    while (cells > 0 && step < geo.SNAP * cells / 2) step *= 2;
    scheme.style.backgroundSize = `${step}px ${step}px`;
    scheme.style.backgroundPosition = `${this.view.x - sheet.ox * this.view.k}px ${this.view.y - sheet.oy * this.view.k}px`;
    scheme.classList.toggle("small", this.view.k < 0.5);
    this.dom.zoom.textContent = this.t("scheme.zoomValue", { zoom: this.view.k.toFixed(2) });
    this.drawCrumbs(sheet);
    this.drawMap(sheet);
    this.paintSide(sheet);
  }

  drawEdge(sheet, edge, pts, hops, covered = null) {
    const selected = edge.key === this.selectedEdge;
    const next = this.nextEdges.has(edge.key);
    const group = mk("g", {
      class: `edge-group${selected ? " selected" : ""}${next ? " next" : ""}`,
      "data-key": edge.key,
      "data-kind": edge.kind,
      tabindex: 0,
      role: "button",
      "aria-label": edge.cond
        ? this.t("scheme.edgeCondition", { from: edge.from, to: edge.to, condition: edge.cond })
        : this.t("scheme.edge", { from: edge.from, to: edge.to }),
    });
    const marker = markerOf(edge.kind, selected, next);
    const d = geo.buildPath(pts, hops, this.layout.corners, layoutFile.viewOf(this.layout).crossing, edge.points.length === 0);
    // Ореол под линией проступает при наведении: так видно, что щелчок выделит
    // именно это ребро. Полоса нажатия поверх линии шире штриха - в ребро
    // попадают, не целясь в пиксель.
    group.appendChild(mk("path", { class: "edge-halo", d }));
    const line = mk("path", {
      class: `edge${selected ? " selected" : ""}${edge.loop ? " edge-loop" : ""}`,
      d,
      "marker-end": `url(#${marker})`,
    });
    // Участок, совпадающий с уже нарисованным ребром, второй раз не рисуется: два
    // штриха в одном месте складываются сглаживанием и читаются жирной линией.
    // Длина пути задаётся ломаной (`pathLength`), и пропуски ложатся по ней.
    // Пропуски ставятся всегда, а у выбранного ребра их снимает оформление: выбор
    // переключает класс без перестроения листа, и решение, принятое при отрисовке,
    // застряло бы на ребре, которое уже не выбрано.
    if (covered?.runs.length) {
      line.setAttribute("pathLength", covered.total);
      line.setAttribute("stroke-dasharray", geo.dashFor(covered.total, covered.runs));
    }
    if (covered?.ending) line.classList.add("edge-shared-end");
    group.appendChild(line);
    group.appendChild(mk("path", { class: "edge-hit", d }));
    if (edge.cond) {
      // Умолчание места знака - настройка вида, своё место ребра сильнее: автор мог
      // отвести один знак руками, и общее правило не вправе стирать эту работу.
      const [mx, my] = geo.markSpot(edge.label ?? { place: this.labelPlace() }, pts, this.layout.corners, edge.points.length === 0);
      // Линия расступается под знаком: щель вырезана маской, а не закрыта заливкой -
      // холст под ней остаётся холстом, и точки сетки в просвете видны. Заливка
      // цвета листа поверх линии дала бы прямоугольную заплату на сетке.
      this.gapUnderMark(group, line, sheet, edge.mark, mx, my);
      const text = backing(group, mx, my + 2, edge.mark, "edge-mark");
      markText(text, edge.mark, "edge-num");
      text.setAttribute("data-tip", tipOf(edge.mark, edge.cond, edge.alias));
      if (sheet.editable) {
        text.addEventListener("pointerdown", (event) => this.dragMark(event, sheet, edge, pts, text));
        const center = () => {
          const before = this.text();
          layoutFile.labelAt(this.layout, sheet.key, edge.key, "center");
          this.commit(before);
        };
        text.addEventListener("dblclick", (event) => {
          event.stopPropagation();
          center();
        });
        this.onDoubleTap(text, `mark:${edge.key}`, center);
      }
    }
    if (sheet.editable) {
      edge.points.forEach((p, index) => {
        const pin = mk("circle", { class: "pin", cx: p[0], cy: p[1], r: geo.SNAP / 2, tabindex: 0, role: "button", "aria-label": this.t("scheme.pinHint") });
        pin.addEventListener("pointerdown", (event) => this.dragPin(event, sheet, edge, index, pin));
        group.appendChild(pin);
      });
      group.addEventListener("dblclick", (event) => this.addPinAt(event, sheet, edge, pts));
      // Излом ставят касанием по самой линии: касание знака или ручки у них своё.
      this.onDoubleTap(group, `edge:${edge.key}`, (event) => this.addPinAt(event, sheet, edge, pts), (event) =>
        event.target.classList.contains("edge-hit") || event.target.classList.contains("edge"),
      );
      if (!edge.loop) {
        for (const side of ["from", "to"]) group.appendChild(this.endPin(sheet, edge, pts, side));
      }
    }
    // Место, где ребро выделили по линии, запоминается: туда кнопка панели ставит
    // излом. Помнится точка листа, а не элемент - лист между касанием и кнопкой
    // бывает перерисован.
    const choose = (event) => {
      if (event.target.classList.contains("edge-hit") || event.target.classList.contains("edge")) {
        this.edgeSpot = { key: edge.key, at: this.sheetPoint(event, false) };
      }
      if (this.selectedEdge !== edge.key || this.selected) this.selectEdge(edge.key);
    };
    group.addEventListener("click", (event) => {
      event.stopPropagation();
      choose(event);
    });
    // Щелчок после касания iPad присылает не всегда (см. кнопку входа в квадрат):
    // ребро выделяется и отпусканием пальца без сдвига.
    let down = null;
    group.addEventListener("pointerdown", (event) => {
      down = event.pointerType === "mouse" ? null : { x: event.clientX, y: event.clientY };
    });
    group.addEventListener("pointerup", (event) => {
      const tap = down && Math.hypot(event.clientX - down.x, event.clientY - down.y) < geo.TOUCH_THRESHOLD;
      down = null;
      if (tap) choose(event);
    });
    this.dom.sheet.appendChild(group);
  }

  /**
   * Вырезает в линии щель под знаком условия.
   *
   * Щель - ширина знака и по три единицы с боков: линия не должна касаться букв,
   * но и просвет во всю ширину знака с запасом рвал бы короткое ребро надвое.
   * Маска живёт при своём ребре: она зависит от места знака, а место у каждого
   * ребра своё.
   */
  gapUnderMark(group, line, sheet, mark, mx, my) {
    const w = mark.length * 8 + 8 + 6;
    const h = 21;
    const id = `edge-gap-${this.gapSeq++}`;
    // Область маски задаётся явно: по умолчанию она - окно листа с запасом в
    // десятую долю, и за ним линия гасла бы так же, как за белым полем.
    const field = { x: sheet.ox - REACH_ALL, y: sheet.oy - REACH_ALL, width: sheet.w + 2 * REACH_ALL, height: sheet.h + 2 * REACH_ALL };
    const mask = mk("mask", { id, maskUnits: "userSpaceOnUse", ...field });
    // Белое поле маски заведомо шире любого листа: пока автор ведёт излом за край,
    // лист ещё не расширен, и поле размером с лист гасило бы линию за старой
    // границей - видны оставались одни кружки изломов.
    mask.appendChild(mk("rect", { ...field, fill: "white" }));
    mask.appendChild(mk("rect", { x: mx - w / 2, y: my - h / 2, width: w, height: h, fill: "black" }));
    group.appendChild(mask);
    line.setAttribute("mask", `url(#${id})`);
  }

  drawNode(sheet, node) {
    const classes = ["node"];
    if (node.name === this.selected) classes.push("selected");
    if (node.unplaced && sheet.editable) classes.push("unplaced");
    if (this.running.has(node.name)) classes.push("running");
    else if (this.expected.has(node.name)) classes.push("expected");
    else if (sheet.edges.some((e) => this.running.has(e.from) && e.to === node.name)) classes.push("reachable");
    const composition = node.kind === "composition";
    const group = mk("g", {
      class: classes.join(" "),
      "data-name": node.name,
      tabindex: 0,
      role: "button",
      "aria-label": composition
        ? this.t("scheme.nodeComposition", { mark: node.mark, name: node.name })
        : this.t("scheme.node", { mark: node.mark, name: node.name }),
      "data-tip": node.unplaced && sheet.editable
        ? `${tipOf(node.mark, node.model ?? node.name, node.alias)} · ${this.t("scheme.unplaced")}`
        : tipOf(node.mark, node.model ?? node.name, node.alias),
    });
    if (composition) {
      const h = geo.SIDE / 2;
      group.appendChild(mk("rect", { class: "node-ring", x: node.x - h - 4, y: node.y - h - 4, width: geo.SIDE + 8, height: geo.SIDE + 8, rx: 12 }));
      group.appendChild(mk("rect", { class: "node-comp", x: node.x - h, y: node.y - h, width: geo.SIDE, height: geo.SIDE }));
      this.drawMini(group, sheet, node);
      this.drawInner(group, node);
      const text = mk("text", { class: "node-mark", x: node.x - h + 22, y: node.y - h + 26 });
      markText(text, node.mark, "node-num");
      group.appendChild(text);
      if (this.target(sheet, node)) {
        const enter = mk("g", { class: "node-enter-btn", role: "button", "aria-label": this.t("scheme.enter") });
        enter.appendChild(mk("circle", { class: "node-enter", cx: node.x + h - 20, cy: node.y + h - 20, r: 14 }));
        enter.appendChild(mk("path", { class: "node-enter-mark", d: `M${node.x + h - 26} ${node.y + h - 20}h10` }));
        enter.appendChild(mk("path", { class: "node-enter-mark", d: `M${node.x + h - 24} ${node.y + h - 24}l4 4-4 4` }));
        // Кнопка входа - не часть квадрата: касание её не начинает перенос, а вход
        // делает отпускание над ней. Щелчок после касания iPad присылает не всегда,
        // и кнопка под пальцем молчала.
        enter.addEventListener("pointerdown", (event) => event.stopPropagation());
        enter.addEventListener("pointerup", (event) => {
          event.stopPropagation();
          this.enter(node.name);
        });
        group.appendChild(enter);
      }
    } else {
      group.appendChild(mk("circle", { class: "node-ring", cx: node.x, cy: node.y, r: geo.R + 4 }));
      group.appendChild(mk("circle", { class: "node-body", cx: node.x, cy: node.y, r: geo.R }));
      if (node.start) this.drawEntry(group, sheet, node);
      if (node.kind === "end") group.appendChild(mk("circle", { class: "node-final", cx: node.x, cy: node.y, r: geo.R - 5 }));
      const text = mk("text", { class: "node-mark", x: node.x, y: node.y + 5 });
      markText(text, node.mark, "node-num");
      group.appendChild(text);
    }
    group.addEventListener("pointerdown", (event) => this.dragNode(event, sheet, node, group));
    group.addEventListener("dblclick", () => this.enter(node.name));
    group.addEventListener("keydown", (event) => {
      if (event.key === "Enter") this.enter(node.name);
    });
    group.addEventListener("focus", () => {
      if (this.selected !== node.name) {
        this.selected = node.name;
        this.focusEdge(null);
        this.applySelection();
      }
    });
    this.dom.sheet.appendChild(group);
  }

  /**
   * Стрелка начального состояния: входит в круг в своей точке привязки.
   *
   * Место стрелки - работа автора, как и место узла: её ведут по окружности за
   * наконечник (либо стрелками клавиатуры), и она встаёт в ближайшую из
   * шестнадцати точек. Занятую стрелкой точку рёбра обходят.
   */
  drawEntry(group, sheet, node) {
    const port = layoutFile.entryOf(this.layout, sheet.key);
    const angle = (port * 2 * Math.PI) / geo.PORTS;
    const out = [Math.cos(angle), Math.sin(angle)];
    const side = [-out[1], out[0]];
    const at = (reach, lateral = 0) =>
      `${(node.x + out[0] * reach + side[0] * lateral).toFixed(1)} ${(node.y + out[1] * reach + side[1] * lateral).toFixed(1)}`;
    const tip = at(geo.R + 4);
    const grip = mk("g", { class: "node-entry-grip" });
    if (sheet.editable) grip.appendChild(mk("path", { class: "node-entry-hit", d: `M${at(geo.R + 18)}L${tip}` }));
    grip.appendChild(mk("path", { class: "node-entry", d: `M${at(geo.R + 16)}L${tip}` }));
    grip.appendChild(mk("path", { class: "node-entry", d: `M${at(geo.R + 8, 4)}L${tip}L${at(geo.R + 8, -4)}` }));
    group.appendChild(grip);
    if (!sheet.editable) return;
    grip.setAttribute("tabindex", 0);
    grip.setAttribute("role", "button");
    grip.setAttribute("aria-label", this.t("scheme.entryHint"));
    grip.addEventListener("pointerdown", (event) => this.dragEntry(event, sheet, node, grip));
    grip.addEventListener("keydown", (event) => {
      const step = { ArrowLeft: -1, ArrowUp: -1, ArrowRight: 1, ArrowDown: 1 }[event.key];
      if (!step) return;
      event.preventDefault();
      event.stopPropagation();
      const before = this.text();
      layoutFile.entryAt(this.layout, sheet.key, (port + step + geo.PORTS) % geo.PORTS);
      this.commit(before);
    });
  }

  /** Миниатюра листа внутри квадрата композиции: точки и линии, без текста. */
  drawMini(group, sheet, node) {
    const level = this.target(sheet, node);
    const inner = level ? this.sheetOf(level) : null;
    if (!inner || inner.nodes.length === 0) return;
    const h = geo.SIDE / 2;
    const s = (geo.SIDE - 40) / Math.max(inner.w, inner.h);
    const ox = node.x - h + 20 - inner.ox * s;
    const oy = node.y - h + 28 - inner.oy * s;
    const byName = new Map(inner.nodes.map((n) => [n.name, n]));
    for (const edge of inner.edges) {
      const from = byName.get(edge.from);
      const to = byName.get(edge.to);
      if (!from || !to) continue;
      const pts = geo.route(from, edge.loop ? from : to, edge.points).map((p) => [ox + p[0] * s, oy + p[1] * s]);
      group.appendChild(mk("path", {
        class: "mini-edge",
        "data-from": edge.from,
        "data-to": edge.to,
        d: `M${pts.map((p) => `${p[0].toFixed(1)} ${p[1].toFixed(1)}`).join("L")}`,
      }));
    }
    for (const inside of inner.nodes) {
      group.appendChild(mk("circle", { class: "mini-node", "data-name": inside.name, cx: (ox + inside.x * s).toFixed(1), cy: (oy + inside.y * s).toFixed(1), r: 3 }));
    }
  }

  /**
   * Плашка под квадратом композиции: текущее внутреннее состояние на прогоне.
   *
   * Рисуется пустой и скрытой; текст и видимость ставит `paintInner` - на каждом
   * такте, без перестроения листа.
   */
  drawInner(group, node) {
    const h = geo.SIDE / 2;
    const box = mk("g", { class: "node-inner-box", "data-inner": node.name, hidden: "" });
    box.appendChild(mk("rect", { class: "node-inner-bg", x: node.x - h, y: node.y + h + 8, width: geo.SIDE, height: 20, rx: 10 }));
    box.appendChild(mk("text", { class: "node-inner", x: node.x, y: node.y + h + 18 }));
    group.appendChild(box);
  }

  /**
   * Прогон внутри композиций листа: миниатюра заливает активное внутреннее
   * состояние и подсвечивает только что пройденный переход, плашка называет
   * текущее внутреннее состояние - подписью автора, если она есть.
   *
   * Активные состояния модуль отдаёт плоским списком имён всех уровней, поэтому
   * внутреннее состояние узнаётся по имени на внутреннем листе.
   */
  paintInner() {
    const sheet = this.current();
    const before = this.runningBefore ?? new Set();
    for (const node of sheet.nodes) {
      if (node.kind !== "composition") continue;
      const group = [...this.dom.sheet.querySelectorAll(".node")].find((g) => g.dataset.name === node.name);
      if (!group) continue;
      const level = this.target(sheet, node);
      const inner = level ? this.sheetOf(level) : null;
      for (const dot of group.querySelectorAll(".mini-node")) dot.classList.toggle("run", this.running.has(dot.dataset.name));
      for (const line of group.querySelectorAll(".mini-edge")) {
        const hot = line.dataset.from !== line.dataset.to && before.has(line.dataset.from) && this.running.has(line.dataset.to) && !before.has(line.dataset.to);
        line.classList.toggle("hot", hot);
      }
      const box = group.querySelector(".node-inner-box");
      if (!box) continue;
      // Имени самого квадрата в списке модуля может не быть: у состояния,
      // реализованного моделью, список несёт одни внутренние имена. Плашку и
      // отметку прогона даёт само внутреннее состояние.
      const label = inner ? innerLabel(inner.nodes, this.running) : "";
      box.querySelector(".node-inner").textContent = label;
      box.toggleAttribute("hidden", label === "");
      if (label) group.classList.add("running");
    }
  }

  drawCrumbs(sheet) {
    const { crumbs } = this.dom;
    crumbs.replaceChildren();
    // Кнопка "наверх" живёт в углу холста, а не в этой строке: она про рисунок, и
    // рука тянется к ней там, где смотрит глаз. Появляется ровно тогда, когда есть
    // куда подниматься - на верхнем листе подъём никуда не ведёт, и погашенная
    // кнопка занимала бы угол, ничего не обещая.
    this.dom.crumbsUp.hidden = this.trail.length < 2;
    this.trail.forEach((level, index) => {
      if (index > 0) {
        const sep = document.createElement("span");
        sep.className = "crumb-sep";
        sep.setAttribute("aria-hidden", "true");
        crumbs.appendChild(sep);
      }
      const title = index === this.trail.length - 1 ? sheet.title : (this.sheetOf(level)?.title ?? "");
      if (index === this.trail.length - 1) {
        const current = document.createElement("span");
        current.className = "crumb crumb-current";
        current.setAttribute("aria-current", "page");
        current.textContent = title;
        crumbs.appendChild(current);
      } else {
        const button = document.createElement("button");
        button.className = "crumb";
        button.type = "button";
        button.textContent = title;
        button.addEventListener("click", () => {
          this.focusEdge(null);
          this.trail = this.trail.slice(0, index + 1);
          this.selected = null;
          this.fitPending = true;
          this.draw();
        });
        crumbs.appendChild(button);
      }
    });
  }

  drawMap(sheet) {
    const { map, scheme } = this.dom;
    map.replaceChildren();
    const s = Math.min(128 / sheet.w, 96 / sheet.h);
    for (const node of sheet.nodes) {
      const w = (node.kind === "composition" ? geo.SIDE : geo.R * 2) * s;
      map.appendChild(mk("rect", { class: "map-node", x: (node.x - sheet.ox) * s - w / 2, y: (node.y - sheet.oy) * s - w / 2, width: Math.max(w, 3), height: Math.max(w, 3), rx: 1 }));
    }
    const box = scheme.getBoundingClientRect();
    map.appendChild(
      mk("rect", {
        class: "map-view",
        x: (-this.view.x / this.view.k) * s,
        y: (-this.view.y / this.view.k) * s,
        width: (box.width / this.view.k) * s,
        height: (box.height / this.view.k) * s,
      }),
    );
  }

  /** Легенда и навигатор: их выбор синхронен с выбором на листе. */
  paintSide(sheet) {
    const ctx = {
      t: this.t,
      sheet,
      kinds: this.kinds,
      title: sheet.title,
      selectedNode: this.selected,
      selectedEdge: this.selectedEdge,
      floating: this.dom.legend.classList.contains("legend-float"),
      marks: layoutFile.viewOf(this.layout).marks,
      onNode: (name) => this.selectNode(name),
      onEdge: (key) => this.selectEdge(key),
      onEnter: (name) => this.enter(name),
      onAlias: (name, text) => {
        const before = this.text();
        layoutFile.nameNode(this.layout, sheet.key, name, text);
        this.commitQuiet(before);
      },
      onEdgeAlias: (key, text) => {
        const before = this.text();
        layoutFile.nameEdge(this.layout, sheet.key, key, text);
        this.commitQuiet(before);
      },
      onTitleDown: (event) => this.dragLegend(event),
    };
    for (const node of this.dom.sheet.querySelectorAll(".node")) {
      node.classList.toggle("selected", node.dataset.name === this.selected);
    }
    paintLegend(this.dom.legend, ctx);
    paintNav(this.dom.nav, ctx);
  }

  /** Правка подписи: запись без перерисовки узлов - поле ввода остаётся в фокусе. */
  commitQuiet(before) {
    if (before === this.text()) return;
    this.undo.push(before);
    this.redo = [];
    const sheet = this.current();
    for (const node of this.dom.sheet.querySelectorAll(".node")) node.remove();
    for (const node of sheet.nodes) this.drawNode(sheet, node);
    paintNav(this.dom.nav, { t: this.t, sheet, kinds: this.kinds, title: sheet.title, selectedNode: this.selected, onNode: (name) => this.selectNode(name), onEnter: (name) => this.enter(name) });
    this.onChange();
  }

  // ── Выбор ───────────────────────────────────────────────────────────────

  selectNode(name, fromUser = true) {
    this.selected = name;
    this.focusEdge(null);
    this.applySelection();
    const node = this.current().nodes.find((n) => n.name === name);
    if (fromUser && node?.nameRange) this.onSelect(node);
  }

  selectEdge(key) {
    this.focusEdge(key);
    this.selected = null;
    this.applySelection();
  }

  /**
   * Ставит ребро в фокус, а с прежнего снимает звенья, не дающие изгиба.
   *
   * Правило одно: звено - это изгиб. Явно заведённое звено ложится на линию и
   * живёт, пока ребро в фокусе: автор ведёт его туда, где линии нужен угол. Ушёл
   * фокус - линия остаётся с теми звеньями, которые её действительно гнут, а
   * лежащие на прямой снимаются сами. Удалять звено отдельным действием нечем и
   * незачем: выпрямил линию - звена нет.
   */
  focusEdge(key) {
    const leaving = this.selectedEdge;
    this.selectedEdge = key;
    if (leaving && leaving !== key) this.straighten(leaving);
  }

  /** Снимает с ребра звенья, лежащие на прямой; правит раскладку, если снял. */
  straighten(key) {
    const sheet = this.current();
    if (!sheet.editable) return;
    const edge = sheet.edges.find((e) => e.key === key);
    if (!edge || edge.points.length === 0) return;
    const from = sheet.nodes.find((n) => n.name === edge.from);
    const to = sheet.nodes.find((n) => n.name === edge.to);
    if (!from || !to) return;
    const kept = geo.bendingPoints(from, edge.loop ? from : to, edge.points);
    if (kept.length === edge.points.length) return;
    const before = this.text();
    layoutFile.bend(this.layout, sheet.key, edge.key, kept);
    this.commit(before);
  }

  /**
   * Переносит выбор на узлы и рёбра без перестроения листа: перестроенный элемент не
   * получил бы второго щелчка, и двойной щелчок никогда не наступал бы.
   */
  applySelection() {
    const sheet = this.current();
    for (const node of this.dom.sheet.querySelectorAll(".node")) {
      node.classList.toggle("selected", node.dataset.name === this.selected);
    }
    for (const group of this.dom.sheet.querySelectorAll(".edge-group")) {
      const chosen = group.dataset.key === this.selectedEdge;
      group.classList.toggle("selected", chosen);
      const path = group.querySelector(".edge");
      path?.classList.toggle("selected", chosen);
      path?.setAttribute("marker-end", `url(#${markerOf(group.dataset.kind, chosen, this.nextEdges.has(group.dataset.key))})`);
    }
    this.paintSide(sheet);
  }

  // ── Вид ─────────────────────────────────────────────────────────────────

  /**
   * Проверяет, видно ли лист после отрисовки, и вписывает его, когда не видно.
   *
   * Зовётся при открытии панели схемы. Вид - величина стойкая: панель могли закрыть
   * с отведённым в сторону холстом либо при другой ширине области, и открыв её, автор
   * увидел бы пустое поле при живой модели. Вписывать лист всякий раз нельзя - это
   * сбрасывало бы заданный вручную масштаб, поэтому вид трогается ровно тогда, когда в
   * холсте не оказалось ни одного узла.
   */
  ensureVisible() {
    const sheet = this.current();
    if (sheet.nodes.length === 0) return;
    const box = this.dom.scheme.getBoundingClientRect();
    // Область без размера ещё ничего не показывает: вписывание ждёт наблюдателя размера
    // (тот же приём, что у `draw`).
    if (box.width === 0 || box.height === 0) {
      this.fitPending = true;
      return;
    }
    const seen = [...this.dom.sheet.querySelectorAll(".node")].some((node) => {
      const at = node.getBoundingClientRect();
      return at.right > box.left && at.left < box.right && at.bottom > box.top && at.top < box.bottom;
    });
    if (!seen) this.fit();
  }

  fit() {
    const sheet = this.current();
    this.view = geo.fitView(this.dom.scheme.getBoundingClientRect(), sheet.w, sheet.h);
    this.draw();
  }

  center() {
    const sheet = this.current();
    const node = sheet.nodes.find((n) => n.name === this.selected);
    if (!node) {
      this.fit();
      return;
    }
    this.view = geo.centerOn(this.view, this.dom.scheme.getBoundingClientRect(), node.x - sheet.ox, node.y - sheet.oy);
    this.draw();
  }

  zoom(factor) {
    const box = this.dom.scheme.getBoundingClientRect();
    this.view = geo.zoomAt(this.view, box.width / 2, box.height / 2, factor);
    this.draw();
  }

  /** Точка листа под указателем; `snap` - с привязкой к сетке. */
  sheetPoint(event, snap = true) {
    const box = this.dom.scheme.getBoundingClientRect();
    const sheet = this.current();
    const [x, y] = geo.toSheet(this.view, event.clientX - box.left, event.clientY - box.top);
    if (!snap) return [x + sheet.ox, y + sheet.oy];
    return [this.snapped(x + sheet.ox), this.snapped(y + sheet.oy)];
  }

  /** Привязка к сетке по настройке вида: выключена - координата остаётся точной. */
  snapped(value) {
    return layoutFile.viewOf(this.layout).snap ? geo.snap(value) : Math.round(value);
  }

  /** Умолчание места знака условия из файла раскладки. */
  labelPlace() {
    return this.layout.labelPlace ?? "center";
  }

  // ── Тяга: узел, излом, знак, легенда ────────────────────────────────────

  /**
   * Двойное касание предмета листа - то же, что двойной щелчок.
   *
   * iPad не присылает `dblclick` на касание, и двойное касание узнаётся по двум
   * касаниям подряд одного предмета: не дольше `DOUBLE_TAP_MS` и без сдвига
   * пальца за порог тяги - иначе это перенос. Предмет называет `key`, а не
   * элемент: между касаниями лист бывает перерисован. `accept` отсеивает касания
   * вложенных предметов со своим двойным касанием (знак и ручки внутри ребра).
   */
  onDoubleTap(element, key, handler, accept = () => true) {
    let start = null;
    element.addEventListener("pointerdown", (event) => {
      start = event.pointerType !== "mouse" && accept(event) ? { x: event.clientX, y: event.clientY } : null;
    });
    element.addEventListener("pointerup", (event) => {
      const tap = start && Math.hypot(event.clientX - start.x, event.clientY - start.y) < geo.TOUCH_THRESHOLD;
      start = null;
      if (!tap) return;
      if (doubleTap(this.lastTap, key, event.timeStamp)) {
        this.lastTap = null;
        handler(event);
      } else {
        this.lastTap = { name: key, at: event.timeStamp };
      }
    });
  }

    /** Общий приём тяги: порог, движение, отпускание, отмена по Escape. */
  drag(event, { onStart, onMove, onEnd, onCancel }) {
    if (event.button) return;
    event.stopPropagation();
    // Перенос - не выделение текста: протяжка указателя иначе выделяла бы холст
    // целиком синей заливкой браузера.
    window.getSelection?.()?.removeAllRanges();
    const start = { x: event.clientX, y: event.clientY };
    const threshold = event.pointerType === "mouse" ? geo.DRAG_THRESHOLD : geo.TOUCH_THRESHOLD;
    let moved = false;
    const move = (e) => {
      if (!moved && Math.hypot(e.clientX - start.x, e.clientY - start.y) < threshold) return;
      if (!moved) {
        moved = true;
        onStart?.();
      }
      onMove((e.clientX - start.x) / this.view.k, (e.clientY - start.y) / this.view.k);
    };
    const stop = () => {
      window.removeEventListener("pointermove", move);
      window.removeEventListener("pointerup", stop);
      window.removeEventListener("keydown", esc);
      onEnd(moved);
    };
    const esc = (e) => {
      if (e.key === "Escape") {
        window.removeEventListener("pointermove", move);
        window.removeEventListener("pointerup", stop);
        window.removeEventListener("keydown", esc);
        onCancel(moved);
      }
    };
    window.addEventListener("pointermove", move);
    window.addEventListener("pointerup", stop);
    window.addEventListener("keydown", esc);
  }

  dragNode(event, sheet, node, group) {
    const before = this.text();
    const origin = { x: node.x, y: node.y };
    let last = null;
    this.drag(event, {
      onStart: () => group.classList.add("dragging"),
      onMove: (dx, dy) => {
        if (!sheet.editable) return;
        last = [this.snapped(origin.x + dx), this.snapped(origin.y + dy)];
        layoutFile.place(this.layout, sheet.key, node.name, last[0], last[1]);
        this.draw();
      },
      onEnd: (moved) => {
        group.classList.remove("dragging");
        if (moved && sheet.editable) {
          this.selected = node.name;
          this.commit(before);
        } else {
          // Двойного щелчка касание не даёт - iPad его не присылает, - и вход в
          // квадрат двойным касанием узнаётся по двум касаниям подряд одного узла.
          // Время берётся у самого касания и проверяется до выбора: выбор ведёт
          // курсор редактора к имени, это небыстро, и замер после него растягивал
          // промежуток между касаниями за предел.
          if (event.pointerType !== "mouse" && doubleTap(this.lastTap, node.name, event.timeStamp)) {
            this.lastTap = null;
            this.enter(node.name);
            return;
          }
          this.lastTap = event.pointerType === "mouse" ? null : { name: node.name, at: event.timeStamp };
          this.selectNode(node.name);
        }
      },
      onCancel: () => {
        this.layout = layoutFile.parse(before).layout;
        this.draw();
      },
    });
  }

  /**
   * Ручка конца ребра: там, где ребро примыкает к узлу.
   *
   * Видна тогда же, когда изломы, - при выборе ребра и наведении. Конец ведут по
   * окружности узла (либо стрелками клавиатуры), и он встаёт в ближайшую из
   * шестнадцати точек; двойной щелчок снимает закрепление, и конец снова ставит
   * раздача.
   */
  endPin(sheet, edge, pts, side) {
    const at = side === "from" ? pts[0] : pts[pts.length - 1];
    const node = sheet.nodes.find((n) => n.name === edge[side]);
    const pin = mk("circle", {
      class: `pin end-pin${Number.isInteger(edge.ends[side]) ? " fixed" : ""}`,
      cx: at[0],
      cy: at[1],
      r: geo.SNAP / 2,
      tabindex: 0,
      role: "button",
      "aria-label": this.t("scheme.endHint"),
    });
    const set = (port) => {
      const before = this.text();
      layoutFile.endAt(this.layout, sheet.key, edge.key, side, port);
      this.selectedEdge = edge.key;
      this.commit(before);
    };
    pin.addEventListener("pointerdown", (event) => this.dragEnd(event, sheet, edge, side, node, at, pin));
    pin.addEventListener("dblclick", (event) => {
      event.stopPropagation();
      set(null);
    });
    this.onDoubleTap(pin, `end:${edge.key}:${side}`, () => set(null));
    pin.addEventListener("keydown", (event) => {
      const step = { ArrowLeft: -1, ArrowUp: -1, ArrowRight: 1, ArrowDown: 1 }[event.key];
      if (!step || !node) return;
      event.preventDefault();
      event.stopPropagation();
      set((geo.portToward(node, at) + step + geo.PORTS) % geo.PORTS);
    });
    return pin;
  }

  /** Перенос конца ребра: точка - ближайшая к указателю по направлению из центра узла. */
  dragEnd(event, sheet, edge, side, node, origin, pin) {
    if (!node) return;
    const before = this.text();
    this.drag(event, {
      onStart: () => pin.classList.add("dragging"),
      onMove: (dx, dy) => {
        layoutFile.endAt(this.layout, sheet.key, edge.key, side, geo.portToward(node, [origin[0] + dx, origin[1] + dy]));
        this.selectedEdge = edge.key;
        this.selected = null;
        this.draw();
      },
      onEnd: (moved) => {
        if (moved) this.commit(before);
        else this.selectEdge(edge.key);
      },
      onCancel: () => {
        this.layout = layoutFile.parse(before).layout;
        this.draw();
      },
    });
  }

  dragPin(event, sheet, edge, index, pin) {
    const before = this.text();
    const origin = edge.points[index];
    this.drag(event, {
      onStart: () => pin.classList.add("dragging"),
      onMove: (dx, dy) => {
        const points = edge.points.map((p, i) => (i === index ? [this.snapped(origin[0] + dx), this.snapped(origin[1] + dy)] : p));
        layoutFile.bend(this.layout, sheet.key, edge.key, points);
        this.selectedEdge = edge.key;
        this.selected = null;
        this.draw();
      },
      onEnd: (moved) => {
        if (moved) this.commit(before);
        else this.selectEdge(edge.key);
      },
      onCancel: () => {
        this.layout = layoutFile.parse(before).layout;
        this.draw();
      },
    });
  }

  /** Перенос стрелки начального состояния: точка - ближайшая к указателю по направлению из центра. */
  dragEntry(event, sheet, node, grip) {
    const before = this.text();
    const origin = geo.portPoint(node, layoutFile.entryOf(this.layout, sheet.key));
    this.drag(event, {
      onStart: () => grip.classList.add("dragging"),
      onMove: (dx, dy) => {
        layoutFile.entryAt(this.layout, sheet.key, geo.portToward(node, [origin[0] + dx, origin[1] + dy]));
        this.draw();
      },
      onEnd: (moved) => {
        if (moved) this.commit(before);
      },
      onCancel: () => {
        this.layout = layoutFile.parse(before).layout;
        this.draw();
      },
    });
  }

  dragMark(event, sheet, edge, pts, text) {
    const before = this.text();
    const origin = geo.markSpot(edge.label ?? { place: this.labelPlace() }, pts, this.layout.corners, edge.points.length === 0);
    this.drag(event, {
      onStart: () => text.classList.add("dragging"),
      onMove: (dx, dy) => {
        layoutFile.labelAt(this.layout, sheet.key, edge.key, "own", this.snapped(origin[0] + dx), this.snapped(origin[1] + dy));
        this.selectedEdge = edge.key;
        this.selected = null;
        this.draw();
      },
      onEnd: (moved) => {
        if (moved) this.commit(before);
        else this.selectEdge(edge.key);
      },
      onCancel: () => {
        this.layout = layoutFile.parse(before).layout;
        this.draw();
      },
    });
  }

  dragLegend(event) {
    if (event.button) return;
    event.preventDefault();
    const { legend, scheme } = this.dom;
    const before = this.text();
    const origin = { x: this.layout.legend?.x ?? geo.SNAP * 3, y: this.layout.legend?.y ?? geo.SNAP * 9 };
    const start = { x: event.clientX, y: event.clientY };
    legend.classList.add("dragging");
    const move = (e) => {
      const area = scheme.getBoundingClientRect();
      const box = legend.getBoundingClientRect();
      const x = Math.max(0, Math.min(area.width - box.width, geo.snap(origin.x + e.clientX - start.x)));
      const y = Math.max(0, Math.min(area.height - box.height, geo.snap(origin.y + e.clientY - start.y)));
      layoutFile.legendAt(this.layout, "float", x, y);
      legend.style.left = `${x}px`;
      legend.style.top = `${y}px`;
    };
    const stop = () => {
      legend.classList.remove("dragging");
      window.removeEventListener("pointermove", move);
      window.removeEventListener("pointerup", stop);
      this.commit(before);
    };
    window.addEventListener("pointermove", move);
    window.addEventListener("pointerup", stop);
  }

  // ── Изломы ──────────────────────────────────────────────────────────────

  addPinAt(event, sheet, edge, pts) {
    event.stopPropagation();
    const p = this.sheetPoint(event);
    const at = geo.nearestSegment(pts, p);
    const points = [...edge.points];
    points.splice(at, 0, p);
    const before = this.text();
    layoutFile.bend(this.layout, sheet.key, edge.key, points);
    this.focusEdge(edge.key);
    this.selected = null;
    this.commit(before);
  }

  /**
   * Звено кнопкой панели: туда, где выбранное ребро выделили.
   *
   * Место касания опускается на линию - изгиба точка пока не даёт и живёт, пока
   * ребро в фокусе. Автор ведёт её туда, где линии нужен угол; оставит на месте -
   * звено снимется само, когда фокус уйдёт. Места нет (ребро выбрано клавиатурой
   * либо место уже занято прошлым нажатием) - середина самого длинного сегмента.
   */
  pinByButton() {
    const sheet = this.current();
    const edge = sheet.edges.find((e) => e.key === this.selectedEdge);
    if (!edge || !sheet.editable) return;
    const before = this.text();
    const from = sheet.nodes.find((n) => n.name === edge.from);
    const to = sheet.nodes.find((n) => n.name === edge.to);
    const pts = this.routes?.get(edge.key) ?? geo.route(from, edge.loop ? from : to, edge.points);
    const spot = this.edgeSpot?.key === edge.key && !edge.loop ? this.edgeSpot.at : null;
    this.edgeSpot = null;
    const { at, point } = geo.nearestOnLine(pts, spot ?? geo.longestMid(pts));
    const points = [...edge.points];
    // Петля строится без изломов автора, и сегмент её ломаной на них не ложится.
    points.splice(edge.loop ? points.length : at, 0, [this.snapped(point[0]), this.snapped(point[1])]);
    layoutFile.bend(this.layout, sheet.key, edge.key, points);
    this.commit(before);
  }

    // ── Панель, холст, клавиатура ───────────────────────────────────────────

  wire() {
    const { scheme, map, tools, nav, noticeDrop } = this.dom;
    // Пальцы на холсте: один возит лист, два меняют масштаб. Указатели ведутся
    // здесь, а не в обработчике панорамы: второй палец обязан её прекратить -
    // иначе лист поедет и растянется разом.
    const touches = new Map();
    let pinch = null;
    scheme.addEventListener("pointerdown", (event) => {
      if (event.pointerType === "mouse") return;
      if (event.target.closest(".scheme-tools, .scheme-nav, .minimap, .legend")) return;
      touches.set(event.pointerId, event);
      if (touches.size === 2) {
        this.panStop?.();
        pinch = span(touches);
      }
    });
    const pinchMove = (event) => {
      if (!touches.has(event.pointerId)) return;
      touches.set(event.pointerId, event);
      if (touches.size !== 2 || !pinch) return;
      const now = span(touches);
      if (!pinch.dist || !now.dist) return;
      const box = scheme.getBoundingClientRect();
      // Масштаб считается от прошлого шага, а не от начала жеста: щипок идёт
      // мелкими приращениями, и точка опоры едет вместе с пальцами.
      this.view = geo.zoomAt(this.view, now.x - box.left, now.y - box.top, now.dist / pinch.dist);
      pinch = now;
      this.draw();
    };
    const pinchDrop = (event) => {
      touches.delete(event.pointerId);
      if (touches.size < 2) pinch = null;
    };
    window.addEventListener("pointermove", pinchMove);
    window.addEventListener("pointerup", pinchDrop);
    window.addEventListener("pointercancel", pinchDrop);

    scheme.addEventListener("pointerdown", (event) => {
      if (event.target.closest(".scheme-tools, .scheme-nav, .minimap, .legend")) return;
      if (event.button) return;
      // Второй палец уже на холсте - это щипок, а не панорама.
      if (touches.size > 1) return;
      const start = { x: event.clientX, y: event.clientY, vx: this.view.x, vy: this.view.y };
      const threshold = event.pointerType === "mouse" ? geo.DRAG_THRESHOLD : geo.TOUCH_THRESHOLD;
      let moved = false;
      scheme.classList.add("dragging");
      const move = (e) => {
        // Панорама начинается за порогом тяги: палец на iPad дрожит всегда, и
        // перерисовка на каждое движение пересоздавала предмет под пальцем -
        // касание ребра не доходило ни до выбора, ни до двойного касания.
        if (!moved && Math.hypot(e.clientX - start.x, e.clientY - start.y) < threshold) return;
        moved = true;
        this.view = { k: this.view.k, x: start.vx + e.clientX - start.x, y: start.vy + e.clientY - start.y };
        this.draw();
      };
      const stop = () => {
        scheme.classList.remove("dragging");
        window.removeEventListener("pointermove", move);
        window.removeEventListener("pointerup", stop);
        this.panStop = null;
      };
      this.panStop = stop;
      window.addEventListener("pointermove", move);
      window.addEventListener("pointerup", stop);
    });
    scheme.addEventListener("click", (event) => {
      if (event.target === scheme || event.target === this.dom.sheet) {
        if (this.selected || this.selectedEdge) {
          this.selected = null;
          this.focusEdge(null);
          this.applySelection();
        }
      }
    });
    scheme.addEventListener(
      "wheel",
      (event) => {
        event.preventDefault();
        const box = scheme.getBoundingClientRect();
        const step = geo.WHEEL_ZOOM_STEP;
        this.view = geo.zoomAt(this.view, event.clientX - box.left, event.clientY - box.top, event.deltaY < 0 ? step : 1 / step);
        this.draw();
      },
      { passive: false },
    );
    map.addEventListener("click", (event) => {
      const sheet = this.current();
      const box = map.getBoundingClientRect();
      const s = Math.min(128 / sheet.w, 96 / sheet.h);
      const x = (((event.clientX - box.left) / box.width) * 128) / s;
      const y = (((event.clientY - box.top) / box.height) * 96) / s;
      this.view = geo.centerOn(this.view, scheme.getBoundingClientRect(), x, y);
      this.draw();
    });
    tools.addEventListener("click", (event) => {
      const button = event.target.closest("[data-act]");
      if (!button) return;
      const act = button.dataset.act;
      if (act === "zoom-in") this.zoom(geo.ZOOM_STEP);
      else if (act === "zoom-out") this.zoom(1 / geo.ZOOM_STEP);
      else if (act === "fit") this.fit();
      else if (act === "center") this.center();
      else if (act === "auto") this.autoLayout();
      else if (act.startsWith("legend-")) this.placeLegend(act.slice(7));
      else if (act === "settings") this.openSettings();
      else if (act === "pin-add") this.pinByButton();
      else if (act === "nav") {
        nav.hidden = !nav.hidden;
        button.setAttribute("aria-pressed", String(!nav.hidden));
      }
    });
    if (this.dom.settingsModal) {
      this.settings = new Settings(
        {
          modal: this.dom.settingsModal,
          tabs: this.dom.settingsTabs,
          body: this.dom.settingsBody,
          save: this.dom.settingsSave,
          cancel: this.dom.settingsCancel,
        },
        {
          t: this.t,
          values: () => this.settingValues(),
          onPick: (key, value) => this.pickSetting(key, value),
          onSave: () => this.saveSettings(),
          onCancel: () => this.cancelSettings(),
        },
      );
    }
    this.dom.crumbsUp.addEventListener("click", () => this.back());
    noticeDrop.addEventListener("click", () => this.dropStale());
    scheme.addEventListener("keydown", (event) => this.onKey(event));
    if (typeof ResizeObserver === "function") {
      new ResizeObserver(() => {
        if (scheme.getBoundingClientRect().width > 0) this.draw();
      }).observe(scheme);
    }
  }

  /** Клавиатура холста: действует только с фокусом внутри него. */
  onKey(event) {
    if (event.target.tagName === "INPUT") return;
    const sheet = this.current();
    const step = event.shiftKey ? geo.SNAP * 5 : geo.SNAP;
    const moves = { ArrowLeft: [-step, 0], ArrowRight: [step, 0], ArrowUp: [0, -step], ArrowDown: [0, step] };
    const node = sheet.nodes.find((n) => n.name === this.selected);
    if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === "z") {
      event.preventDefault();
      if (event.shiftKey) this.redoLast();
      else this.undoLast();
    } else if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === "y") {
      event.preventDefault();
      this.redoLast();
    } else if (moves[event.key] && node && sheet.editable) {
      event.preventDefault();
      this.moveNode(node.name, node.x + moves[event.key][0], node.y + moves[event.key][1]);
    } else if (event.key === "Escape") this.back();
    else if (event.key === "Enter" && node) this.enter(node.name);
    else if (event.key === "+" || event.key === "=") this.zoom(geo.ZOOM_STEP);
    else if (event.key === "-") this.zoom(1 / geo.ZOOM_STEP);
    else if (event.key === "0") this.fit();
  }

  /** Уведомление о расхождении: лишние записи названы, действие - снять. */
  checkStale() {
    const { notice, noticeText } = this.dom;
    if (!this.graph) {
      notice.hidden = true;
      return;
    }
    const report = layoutFile.reconcile(this.layout, this.graph);
    if (report.extras === 0) {
      notice.hidden = true;
      return;
    }
    const names = [];
    for (const [path, found] of Object.entries(report.sheets)) {
      const label = sheetLabel(path);
      const prefix = label === "" ? "" : `${label}/`;
      names.push(...found.extraNodes.map((n) => `${prefix}${n}`), ...found.extraEdges.map((k) => `${prefix}${k}`));
    }
    names.push(...report.extraSheets.map((key) => sheetLabel(key) || key));
    noticeText.textContent = this.t("scheme.stale", { names: names.join(", ") });
    notice.hidden = false;
  }
}

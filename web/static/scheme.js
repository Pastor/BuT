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
    this.fitPending = true;
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

  /** Переименование состояния из редактора переносит записи раскладки. */
  renamed(from, to) {
    const before = this.text();
    for (const path of Object.keys(this.layout.sheets ?? {})) {
      this.layout = layoutFile.rename(this.layout, path, from, to);
    }
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
    this.running = new Set(names ?? []);
    this.expected = new Set((next ?? []).filter((pair) => this.running.has(pair[0])).map((pair) => pair[1]));
    const sheet = this.current();
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
        label: record.label ?? null,
        loop: e.from === e.to,
        range: e.range,
        mark: e.condition ? `K${(k += 1)}` : "",
      };
    });
    const size = geo.sheetSize(nodes, edges.flatMap((e) => e.points));
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

  /** Лист композиции: строится из выражения и не хранится. */
  compositionSheet(node, sheetPath) {
    const composed = geo.composeSheet(node.implements);
    const nodes = composed.nodes.map((n, i) => ({
      name: n.name,
      kind: "composition",
      start: false,
      x: n.x,
      y: n.y,
      unplaced: false,
      alias: n.model,
      implements: n.path ? { model: { name: n.model, path: n.path } } : null,
      mark: `S${i + 1}`,
    }));
    const edges = composed.edges.map((e, i) => ({
      key: `${e.from}>${e.to}:${i}`,
      from: e.from,
      to: e.to,
      ordinal: i,
      kind: "next",
      cond: null,
      alias: "",
      points: [],
      label: null,
      loop: false,
      mark: "",
    }));
    return {
      key: `${sheetPath}#${node.name}`,
      path: null,
      title: node.name,
      editable: false,
      nodes,
      edges,
      frames: composed.frames,
      ox: 0,
      oy: 0,
      w: composed.w,
      h: composed.h,
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
    this.trail = [...this.trail, level];
    this.selected = null;
    this.selectedEdge = null;
    this.fitPending = true;
    this.draw();
  }

  /** Назад по крошкам; на корне - снятие выбора. */
  back() {
    if (this.trail.length > 1) {
      this.trail = this.trail.slice(0, -1);
      this.selected = null;
      this.selectedEdge = null;
      this.fitPending = true;
      this.draw();
    } else if (this.selected || this.selectedEdge) {
      this.selected = null;
      this.selectedEdge = null;
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
    layoutFile.place(this.layout, sheet.path, name, this.snapped(x), this.snapped(y));
    this.commit(before);
  }

  /** Автораскладка листа: координаты и изломы снимаются, подписи остаются. */
  autoLayout() {
    const sheet = this.current();
    if (!sheet.editable) return;
    const before = this.text();
    const stored = layoutFile.sheet(this.layout, sheet.path);
    stored.nodes = {};
    for (const key of Object.keys(stored.edges)) layoutFile.bend(this.layout, sheet.path, key, []);
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
    this.settings.open();
  }

  /** Ступени вида, показанные окну: настройки и то, что живёт рядом с ними. */
  settingValues() {
    return {
      ...layoutFile.viewOf(this.layout),
      corners: this.layout.corners ?? "square",
      labelPlace: this.labelPlace(),
    };
  }

  /** Выбор ступени в окне: раскладка правится сразу, лист перерисовывается. */
  pickSetting(key, value) {
    if (key === "corners") this.layout.corners = value;
    else if (key === "labelPlace") layoutFile.labelPlaceAt(this.layout, value);
    else layoutFile.setView(this.layout, key, value);
    this.draw();
  }

  /** Закрепление настроек: правка уходит вызывающему как всякая другая. */
  saveSettings() {
    this.commit(this.settingsBefore ?? this.text());
    this.settingsBefore = null;
  }

  /** Отказ: раскладка возвращается к снимку, снятому при открытии окна. */
  cancelSettings() {
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
    svg.setAttribute("viewBox", `${sheet.ox} ${sheet.oy} ${sheet.w} ${sheet.h}`);
    svg.setAttribute("width", sheet.w);
    svg.setAttribute("height", sheet.h);
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
    const drawn = [];
    for (const edge of sheet.edges) {
      const from = byName.get(edge.from);
      const to = byName.get(edge.to);
      if (!from || !to) continue;
      const pts = geo.route(from, edge.loop ? from : to, edge.points);
      const hops = geo.crossings(pts, drawn);
      drawn.push(pts);
      this.drawEdge(sheet, edge, pts, hops);
    }
    for (const node of sheet.nodes) this.drawNode(sheet, node);
    svg.style.transformOrigin = "0 0";
    svg.style.transform = `translate(${this.view.x}px, ${this.view.y}px) scale(${this.view.k})`;
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

  drawEdge(sheet, edge, pts, hops) {
    const selected = edge.key === this.selectedEdge;
    const group = mk("g", {
      class: `edge-group${selected ? " selected" : ""}`,
      "data-key": edge.key,
      "data-kind": edge.kind,
      tabindex: 0,
      role: "button",
      "aria-label": edge.cond
        ? this.t("scheme.edgeCondition", { from: edge.from, to: edge.to, condition: edge.cond })
        : this.t("scheme.edge", { from: edge.from, to: edge.to }),
    });
    const marker = `${edge.kind === "next" ? "arrow-solid" : "arrow-open"}${selected ? "-sel" : ""}`;
    group.appendChild(
      mk("path", {
        class: `edge${selected ? " selected" : ""}${edge.loop ? " edge-loop" : ""}`,
        d: geo.buildPath(pts, hops, this.layout.corners === "round"),
        "marker-end": `url(#${marker})`,
      }),
    );
    if (edge.cond) {
      // Умолчание места знака - настройка вида, своё место ребра сильнее: автор мог
      // отвести один знак руками, и общее правило не вправе стирать эту работу.
      const [mx, my] = geo.markSpot(edge.label ?? { place: this.labelPlace() }, pts);
      const text = backing(group, mx, my + 2, edge.mark, "edge-mark");
      markText(text, edge.mark, "edge-num");
      text.setAttribute("data-tip", tipOf(edge.mark, edge.cond, edge.alias));
      if (sheet.editable) {
        text.addEventListener("pointerdown", (event) => this.dragMark(event, sheet, edge, pts, text));
        text.addEventListener("dblclick", (event) => {
          event.stopPropagation();
          const before = this.text();
          layoutFile.labelAt(this.layout, sheet.path, edge.key, "center");
          this.commit(before);
        });
      }
    }
    if (sheet.editable) {
      edge.points.forEach((p, index) => {
        const pin = mk("circle", { class: "pin", cx: p[0], cy: p[1], r: geo.SNAP / 2, tabindex: 0, role: "button", "aria-label": this.t("scheme.pinHint") });
        pin.addEventListener("pointerdown", (event) => this.dragPin(event, sheet, edge, index, pin));
        group.appendChild(pin);
      });
      group.addEventListener("dblclick", (event) => this.addPinAt(event, sheet, edge, pts));
    }
    group.addEventListener("click", (event) => {
      event.stopPropagation();
      this.selectEdge(edge.key);
    });
    this.dom.sheet.appendChild(group);
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
        ? `${tipOf(node.mark, node.name, node.alias)} · ${this.t("scheme.unplaced")}`
        : tipOf(node.mark, node.name, node.alias),
    });
    if (composition) {
      const h = geo.SIDE / 2;
      group.appendChild(mk("rect", { class: "node-ring", x: node.x - h - 4, y: node.y - h - 4, width: geo.SIDE + 8, height: geo.SIDE + 8, rx: 12 }));
      group.appendChild(mk("rect", { class: "node-comp", x: node.x - h, y: node.y - h, width: geo.SIDE, height: geo.SIDE }));
      this.drawMini(group, sheet, node);
      const text = mk("text", { class: "node-mark", x: node.x - h + 22, y: node.y - h + 26 });
      markText(text, node.mark, "node-num");
      group.appendChild(text);
      if (this.target(sheet, node)) {
        const enter = mk("g", { class: "node-enter-btn", role: "button", "aria-label": this.t("scheme.enter") });
        enter.appendChild(mk("circle", { class: "node-enter", cx: node.x + h - 20, cy: node.y + h - 20, r: 14 }));
        enter.appendChild(mk("path", { class: "node-enter-mark", d: `M${node.x + h - 26} ${node.y + h - 20}h10` }));
        enter.appendChild(mk("path", { class: "node-enter-mark", d: `M${node.x + h - 24} ${node.y + h - 24}l4 4-4 4` }));
        enter.addEventListener("click", (event) => {
          event.stopPropagation();
          this.enter(node.name);
        });
        group.appendChild(enter);
      }
    } else {
      group.appendChild(mk("circle", { class: "node-ring", cx: node.x, cy: node.y, r: geo.R + 4 }));
      group.appendChild(mk("circle", { class: "node-body", cx: node.x, cy: node.y, r: geo.R }));
      if (node.start) {
        group.appendChild(mk("path", { class: "node-entry", d: `M${node.x - geo.R - 16} ${node.y}h12` }));
        group.appendChild(mk("path", { class: "node-entry", d: `M${node.x - geo.R - 8} ${node.y - 4}l4 4-4 4` }));
      }
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
      group.appendChild(mk("path", { class: "mini-edge", d: `M${pts.map((p) => `${p[0].toFixed(1)} ${p[1].toFixed(1)}`).join("L")}` }));
    }
    for (const inside of inner.nodes) {
      group.appendChild(mk("circle", { class: "mini-node", cx: (ox + inside.x * s).toFixed(1), cy: (oy + inside.y * s).toFixed(1), r: 3 }));
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
          this.trail = this.trail.slice(0, index + 1);
          this.selected = null;
          this.selectedEdge = null;
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
        layoutFile.nameNode(this.layout, sheet.path, name, text);
        this.commitQuiet(before);
      },
      onEdgeAlias: (key, text) => {
        const before = this.text();
        layoutFile.nameEdge(this.layout, sheet.path, key, text);
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
    layoutFile.bend(this.layout, sheet.path, edge.key, kept);
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
      path?.setAttribute("marker-end", `url(#${group.dataset.kind === "next" ? "arrow-solid" : "arrow-open"}${chosen ? "-sel" : ""})`);
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

  /** Точка листа под указателем с привязкой к сетке. */
  sheetPoint(event) {
    const box = this.dom.scheme.getBoundingClientRect();
    const sheet = this.current();
    const [x, y] = geo.toSheet(this.view, event.clientX - box.left, event.clientY - box.top);
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

    /** Общий приём тяги: порог, движение, отпускание, отмена по Escape. */
  drag(event, { onStart, onMove, onEnd, onCancel }) {
    if (event.button) return;
    event.stopPropagation();
    const start = { x: event.clientX, y: event.clientY };
    let moved = false;
    const move = (e) => {
      if (!moved && Math.hypot(e.clientX - start.x, e.clientY - start.y) < geo.DRAG_THRESHOLD) return;
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
        layoutFile.place(this.layout, sheet.path, node.name, last[0], last[1]);
        this.draw();
      },
      onEnd: (moved) => {
        group.classList.remove("dragging");
        if (moved && sheet.editable) {
          this.selected = node.name;
          this.commit(before);
        } else {
          this.selectNode(node.name);
        }
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
        layoutFile.bend(this.layout, sheet.path, edge.key, points);
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

  dragMark(event, sheet, edge, pts, text) {
    const before = this.text();
    const origin = geo.markSpot(edge.label ?? { place: this.labelPlace() }, pts);
    this.drag(event, {
      onStart: () => text.classList.add("dragging"),
      onMove: (dx, dy) => {
        layoutFile.labelAt(this.layout, sheet.path, edge.key, "own", this.snapped(origin[0] + dx), this.snapped(origin[1] + dy));
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
    layoutFile.bend(this.layout, sheet.path, edge.key, points);
    this.focusEdge(edge.key);
    this.selected = null;
    this.commit(before);
  }

  /**
   * Звено кнопкой панели: на середину самого длинного сегмента выбранного ребра.
   *
   * Точка ложится ровно на линию - изгиба она пока не даёт и живёт, пока ребро в
   * фокусе. Автор ведёт её туда, где линии нужен угол; оставит на месте - звено
   * снимется само, когда фокус уйдёт.
   */
  pinByButton() {
    const sheet = this.current();
    const edge = sheet.edges.find((e) => e.key === this.selectedEdge);
    if (!edge || !sheet.editable) return;
    const before = this.text();
    const from = sheet.nodes.find((n) => n.name === edge.from);
    const to = sheet.nodes.find((n) => n.name === edge.to);
    const pts = geo.route(from, edge.loop ? from : to, edge.points);
    const [mx, my] = geo.longestMid(pts);
    layoutFile.bend(this.layout, sheet.path, edge.key, [...edge.points, [this.snapped(mx), this.snapped(my)]]);
    this.commit(before);
  }

    // ── Панель, холст, клавиатура ───────────────────────────────────────────

  wire() {
    const { scheme, map, tools, nav, noticeDrop } = this.dom;
    scheme.addEventListener("pointerdown", (event) => {
      if (event.target.closest(".scheme-tools, .scheme-nav, .minimap, .legend")) return;
      if (event.button) return;
      const start = { x: event.clientX, y: event.clientY, vx: this.view.x, vy: this.view.y };
      scheme.classList.add("dragging");
      const move = (e) => {
        this.view = { k: this.view.k, x: start.vx + e.clientX - start.x, y: start.vy + e.clientY - start.y };
        this.draw();
      };
      const stop = () => {
        scheme.classList.remove("dragging");
        window.removeEventListener("pointermove", move);
        window.removeEventListener("pointerup", stop);
      };
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
      const prefix = path === "/" ? "" : `${path}/`;
      names.push(...found.extraNodes.map((n) => `${prefix}${n}`), ...found.extraEdges.map((k) => `${prefix}${k}`));
    }
    names.push(...report.extraSheets);
    noticeText.textContent = this.t("scheme.stale", { names: names.join(", ") });
    notice.hidden = false;
  }
}

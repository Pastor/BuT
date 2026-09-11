// Паритет геометрии: холст страницы и чертёж считают один рисунок.
//
// Геометрия схемы живёт в двух языках: холст на JS (`scheme-geometry.js`) и
// носитель чертежа на Rust (`takt-scheme`). Картинка обязана совпадать с тем, что
// автор видит на холсте, и держит это только сверка: расхождение трассировки даёт
// валидный рисунок с другим автоматом, и глазом его видно не всегда.
//
// Сверяется весь корпус `examples/` в двух раскладках: полной (`placeAll` - та,
// что пишет скрипт примеров) и возмущённой - изломы, закреплённые концы, свои
// места знаков, три формы углов, оба вида пересечения, место стрелки входа.
// Числа сравниваются с допуском в тысячную: библиотеки математики у языков
// разные, и последний разряд тригонометрии совпадать не обязан; дискретное -
// ключи, число мостиков, признак совпадения - сравнивается точно.
//
// Подключается из `web-tests.mjs`; путь к модулю - тот же аргумент.

import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import test from "node:test";

import { Bridge } from "../static/bridge.js";
import * as layoutFile from "../static/layout.js";
import * as geo from "../static/scheme-geometry.js";

const EXAMPLES = new URL("../../examples/", import.meta.url);
const EPS = 1e-3;

let loaded = null;
async function instance(path) {
  const { instance: made } = await WebAssembly.instantiate(await readFile(path), {});
  return new Bridge(made.exports);
}

/**
 * Модули сверки: ядро отвечает графом и прогоном, модуль экспорта - рисунком в
 * числах. Путь модуля экспорта - рядом с ядром, если не назван переменной.
 */
async function loadBridge() {
  if (loaded) return loaded;
  const core = process.argv[2] ?? process.env.TAKT_WASM;
  const drawing = process.env.TAKT_EXPORT_WASM ?? core.replace(/takt_wasm\.wasm$/, "takt_wasm_export.wasm");
  const bridge = await instance(core);
  bridge.drawing = await instance(drawing);
  loaded = bridge;
  return loaded;
}

/** Холст без страницы: методы листа на объекте класса. */
async function canvas(graph, stored) {
  const { Scheme } = await import("../static/scheme.js");
  const scheme = Object.create(Scheme.prototype);
  Object.assign(scheme, { graph, layout: stored, run: { active: [], next: [] }, t: (key) => key });
  return scheme;
}

/** Уровни, которые открывает холст: листы моделей и листы композиций. */
function levels(graph) {
  const out = [];
  for (const sheet of graph.sheets) {
    out.push({ kind: "model", path: sheet.path });
    for (const node of sheet.nodes) {
      if (layoutFile.hasCompositionSheet(node.implements)) out.push({ kind: "composition", sheetPath: sheet.path, node: node.name });
    }
  }
  return out;
}

/** То, что считает `draw()` холста, - в тех же числах, что отдаёт модуль. */
function drawnOf(sheet, stored) {
  const byName = new Map(sheet.nodes.map((n) => [n.name, n]));
  const entry = layoutFile.entryOf(stored, sheet.key);
  const reserved = new Map();
  for (const node of sheet.nodes) if (node.start && node.kind !== "composition") reserved.set(node.name, entry);
  const routes = geo.routeSheet(byName, sheet.edges, reserved);
  const corners = stored.corners;
  const crossing = layoutFile.viewOf(stored).crossing;
  const labelPlace = stored.labelPlace ?? "center";
  const drawn = [];
  const edges = [];
  sheet.edges.forEach((edge, i) => {
    const pts = routes[i];
    if (!pts) return;
    const bezier = corners === "bezier";
    const hops = bezier ? [] : geo.crossings(pts, drawn);
    const covered = bezier ? null : geo.overlapWith(pts, drawn);
    drawn.push(pts);
    const auto = edge.points.length === 0;
    edges.push({
      key: edge.key,
      points: pts,
      hops,
      d: geo.buildPath(pts, hops, corners, crossing, auto),
      dash: covered?.runs.length ? geo.dashFor(covered.total, covered.runs) : null,
      ending: Boolean(covered?.ending),
      mark: edge.cond ? geo.markSpot(edge.label ?? { place: labelPlace }, pts, corners, auto) : null,
    });
  });
  return {
    key: sheet.key,
    ox: sheet.ox,
    oy: sheet.oy,
    w: sheet.w,
    h: sheet.h,
    nodes: sheet.nodes.map((n) => ({
      name: n.name,
      x: n.x,
      y: n.y,
      entry: n.start && n.kind !== "composition" ? geo.portPoint(n, entry) : null,
    })),
    frames: sheet.frames.map((f) => [f.x, f.y, f.w, f.h]),
    edges,
  };
}

/** Сравнивает два значения: числа - с допуском, строки пути - по числам и буквам. */
function differ(a, b, path, out) {
  if (typeof a === "number" && typeof b === "number") {
    if (Math.abs(a - b) > EPS) out.push(`${path}: холст ${a}, чертёж ${b}`);
    return;
  }
  if (typeof a === "string" && typeof b === "string" && /^[MLAQC]/.test(a)) {
    const ta = a.match(/-?\d+(\.\d+)?(e-?\d+)?|[A-Za-z]/g) ?? [];
    const tb = b.match(/-?\d+(\.\d+)?(e-?\d+)?|[A-Za-z]/g) ?? [];
    if (ta.length !== tb.length) return out.push(`${path}: путь разной длины\n  холст  ${a}\n  чертёж ${b}`);
    ta.forEach((x, i) => (/[A-Za-z]/.test(x) || /[A-Za-z]/.test(tb[i]) ? x !== tb[i] && out.push(`${path}: команда ${x} / ${tb[i]}`) : differ(Number(x), Number(tb[i]), `${path}[${i}]`, out)));
    return;
  }
  if (typeof a === "string" && typeof b === "string" && /^[\d. ]+$/.test(a)) {
    return differ(a.split(" ").map(Number), b.split(" ").map(Number), path, out);
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return out.push(`${path}: длина ${a.length} / ${b.length}`);
    a.forEach((x, i) => differ(x, b[i], `${path}[${i}]`, out));
    return;
  }
  if (a && b && typeof a === "object" && typeof b === "object") {
    for (const k of new Set([...Object.keys(a), ...Object.keys(b)])) differ(a[k], b[k], `${path}.${k}`, out);
    return;
  }
  if (a !== b) out.push(`${path}: холст ${JSON.stringify(a)}, чертёж ${JSON.stringify(b)}`);
}

/** Возмущённая раскладка: у каждого ребра что-то своё, детерминированно. */
function perturbed(graph, base, variant) {
  const stored = layoutFile.parse(layoutFile.canonical(base)).layout;
  stored.corners = layoutFile.CORNERS[variant % 3];
  if (variant % 2) layoutFile.setView(stored, "crossing", "gap");
  if (variant % 4 === 1) layoutFile.labelPlaceAt(stored, "start");
  let i = 0;
  for (const level of levels(graph)) {
    const key = level.kind === "model" ? level.path : layoutFile.compositionKey(level.sheetPath, level.node);
    const record = layoutFile.sheet(stored, key);
    if (level.kind === "model") layoutFile.entryAt(stored, key, (variant * 5) % geo.PORTS);
    const edges = level.kind === "model"
      ? graph.sheets.find((s) => s.path === level.path).edges
      : geo.composeSheet(graph.sheets.find((s) => s.path === level.sheetPath).nodes.find((n) => n.name === level.node).implements).edges;
    for (const edge of edges) {
      const edgeKey = layoutFile.edgeKey(edge);
      const a = record.nodes[edge.from];
      const b = record.nodes[edge.to];
      i += 1;
      if (!a || !b || edge.from === edge.to) continue;
      if (i % 3 === 0) layoutFile.bend(stored, key, edgeKey, [[geo.snap((a.x + b.x) / 2 + 48), geo.snap((a.y + b.y) / 2 + 24)]]);
      if (i % 5 === 1) layoutFile.endAt(stored, key, edgeKey, "from", (i * 3) % geo.PORTS);
      if (i % 7 === 2) layoutFile.endAt(stored, key, edgeKey, "to", (i * 5) % geo.PORTS);
      if (i % 4 === 3) layoutFile.labelAt(stored, key, edgeKey, i % 8 === 3 ? "own" : "end", a.x + 40, a.y + 16);
    }
  }
  return stored;
}

test("паритет: холст и чертёж считают один рисунок на всём корпусе", async () => {
  const bridge = await loadBridge();
  const files = (await readdir(EXAMPLES)).filter((name) => name.endsWith(".takt")).sort();
  const problems = [];
  let sheets = 0;
  let edges = 0;
  let variant = 0;
  for (const file of files) {
    const source = await readFile(new URL(file, EXAMPLES), "utf8");
    const graph = bridge.graph(source);
    if (!graph.ok || !graph.sheets.some((s) => s.nodes.length > 0)) continue;
    const base = layoutFile.placeAll(graph);
    for (const stored of [base, perturbed(graph, base, (variant += 1))]) {
      const reply = bridge.drawing.schemeGeometry(source, layoutFile.canonical(stored));
      assert.equal(reply.ok, true, `${file}: ${JSON.stringify(reply.error)}`);
      const rust = new Map(reply.sheets.map((s) => [s.key, s]));
      const scheme = await canvas(graph, stored);
      for (const level of levels(graph)) {
        const mine = drawnOf(scheme.sheetOf(level), stored);
        const theirs = rust.get(mine.key);
        if (!theirs) {
          problems.push(`${file} ${mine.key}: листа нет у чертежа`);
          continue;
        }
        sheets += 1;
        edges += mine.edges.length;
        differ(mine, theirs, `${file} ${stored.corners} ${mine.key}`, problems);
      }
    }
  }
  assert.ok(sheets >= 20 && edges >= 100, `выборка мала: листов ${sheets}, рёбер ${edges}`);
  assert.deepEqual(problems.slice(0, 30), [], `расхождений ${problems.length}`);
});

test("паритет: неполная раскладка - отказ, названы узлы", async () => {
  const bridge = await loadBridge();
  const source = await readFile(new URL("elevator.takt", EXAMPLES), "utf8");
  const reply = bridge.drawing.schemeGeometry(source, layoutFile.canonical(layoutFile.empty()));
  assert.equal(reply.ok, false);
  assert.match(reply.error.message, /не размещены узлы/);
  assert.match(reply.error.message, /Engine — /);
});

test("паритет: подсветка прогона у холста и чертежа одна на каждом такте", async () => {
  const run = await import("../static/scheme-run.js");
  const bridge = await loadBridge();
  const read = (url) => readFile(url, "utf8");
  const cases = [
    { source: await read(new URL("elevator.takt", EXAMPLES)), files: {}, ticks: 40 },
    { source: await read(new URL("pid_heater.takt", EXAMPLES)), files: { "pid_law.takt": await read(new URL("pid_law.takt", EXAMPLES)) }, ticks: 40 },
    { source: await read(new URL("../../takt-scheme/tests/data/line.takt", import.meta.url)), files: {}, ticks: 30 },
  ];
  const problems = [];
  let compared = 0;
  let lit = 0;
  for (const { source, files, ticks } of cases) {
    const graph = bridge.graph(source);
    const stored = layoutFile.placeAll(graph);
    const scheme = await canvas(graph, stored);
    const opened = bridge.simOpen(source, "", 0, files, ticks);
    assert.equal(opened.ok, true, JSON.stringify(opened));
    const reply = bridge.simTick(opened.id, ticks);
    assert.equal(reply.ok, true, JSON.stringify(reply));
    reply.active.forEach((active, i) => {
      const next = reply.next[i] ?? [];
      const rust = bridge.drawing.schemeGeometry(source, layoutFile.canonical(stored), { active, next });
      assert.equal(rust.ok, true, JSON.stringify(rust.error));
      const byKey = new Map(rust.sheets.map((s) => [s.key, s.lit]));
      for (const level of levels(graph)) {
        const sheet = scheme.sheetOf(level);
        const mine = run.sheetRun(sheet, { active, next }, run.placeOf(sheet, graph));
        const theirs = byKey.get(sheet.key);
        const shape = (l) => ({
          running: [...l.running].sort(),
          expected: [...l.expected].sort(),
          reachable: [...l.reachable].sort(),
          next_edges: [...(l.nextEdges ?? l.next_edges)].sort(),
          counts: Object.fromEntries([...(l.counts instanceof Map ? l.counts : Object.entries(l.counts))].sort()),
        });
        compared += 1;
        if (mine.running.size) lit += 1;
        try {
          assert.deepEqual(shape(theirs), shape(mine));
        } catch {
          problems.push(`такт ${i + 1} ${sheet.key}: холст ${JSON.stringify(shape(mine))}, чертёж ${JSON.stringify(shape(theirs))}`);
        }
      }
    });
  }
  assert.ok(compared >= 100 && lit >= 50, `выборка мала: сверено ${compared}, горящих ${lit}`);
  assert.deepEqual(problems.slice(0, 10), [], `расхождений ${problems.length}`);
});

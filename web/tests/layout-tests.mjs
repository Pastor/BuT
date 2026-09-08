// Проверки файла раскладки, геометрии схемы и графа модуля.
//
// Что ломается молча и потому проверяется машиной:
//
//   1. **канон** - две записи одной раскладки обязаны совпасть байт в байт,
//      иначе дифф файла в git несёт шум вместо смысла;
//   2. **сверка** - переименованное, удалённое и добавленное состояние обязаны
//      показываться, а не ломать схему;
//   3. **геометрия** - ошибка трассировки даёт валидный рисунок с другим
//      автоматом: слияние вместо пересечения, знак не на своём ребре;
//   4. **граф** - страница ничего не знает о языке и рисует ровно то, что
//      отвечает модуль.
//
// Подключается из `web-tests.mjs`; путь к модулю - тот же аргумент.

import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

import { Bridge } from "../static/bridge.js";
import * as layout from "../static/layout.js";
import * as geo from "../static/scheme-geometry.js";
import { encodeState, decodeState } from "../static/share.js";
import * as draft from "../static/draft.js";

/** Лифт: композиция из шести экземпляров одной модели. */
const ELEVATOR = new URL("../../examples/elevator.takt", import.meta.url);

let loaded = null;
async function loadBridge() {
  if (loaded) return loaded;
  const wasmPath = process.argv[2] ?? process.env.TAKT_WASM;
  assert.ok(wasmPath, "путь к модулю: node web/tests/web-tests.mjs <модуль.wasm>");
  const bytes = await readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(bytes, {});
  loaded = new Bridge(instance.exports);
  return loaded;
}

/** Хранилище в памяти для черновика. */
function memoryStorage() {
  const map = new Map();
  return {
    getItem: (k) => (map.has(k) ? map.get(k) : null),
    setItem: (k, v) => map.set(k, String(v)),
    removeItem: (k) => map.delete(k),
  };
}

/** Граф с двумя состояниями и двумя рёбрами одной пары. */
const GRAPH = {
  sheets: [
    {
      path: "/",
      nodes: [
        { name: "A", kind: "start" },
        { name: "B", kind: "end" },
      ],
      edges: [
        { from: "A", to: "B", ordinal: 0, kind: "ref" },
        { from: "A", to: "B", ordinal: 1, kind: "ref" },
      ],
    },
  ],
};

test("раскладка: имя файла парное имени модели", () => {
  assert.equal(layout.layoutName("elevator.takt"), "elevator.takt-ui");
  assert.equal(layout.modelName("elevator.takt-ui"), "elevator.takt");
  assert.equal(layout.layoutName("readme.md"), null, "не модель");
  assert.equal(layout.modelName("elevator.takt"), null, "не раскладка");
  assert.equal(layout.isLayoutName(".takt-ui"), false, "пустое имя");
});

test("раскладка: запись канонична и переживает круговой рейс", () => {
  const messy = {
    format: 1,
    corners: "round",
    extra: "чужое поле уходит",
    legend: { place: "bottom" },
    sheets: {
      Engine: {
        nodes: { Idle: { x: 10.4, y: 20.6 }, Bad: { x: "нет" } },
        names: { Idle: " Ожидание ", Empty: "   " },
        edges: {
          "Idle>DoorClosing:0": { points: [[3, 4.4], [5, 6]], label: { place: "center" }, name: "Есть вызов" },
          "Idle>DoorClosing:1": { points: [], label: { place: "own", x: 7.2, y: 8 } },
          "Idle>DoorClosing:2": { points: [] },
          "битый ключ": { points: [[1, 1]] },
        },
      },
      "/": { nodes: {}, edges: {} },
    },
  };
  const text = layout.canonical(messy);
  assert.equal(
    text,
    [
      "{",
      '  "corners": "round",',
      '  "format": 1,',
      '  "sheets": {',
      '    "Engine": {',
      '      "edges": {',
      '        "Idle>DoorClosing:0": {',
      '          "name": "Есть вызов",',
      '          "points": [',
      "            [",
      "              3,",
      "              4",
      "            ],",
      "            [",
      "              5,",
      "              6",
      "            ]",
      "          ]",
      "        },",
      '        "Idle>DoorClosing:1": {',
      '          "label": {',
      '            "place": "own",',
      '            "x": 7,',
      '            "y": 8',
      "          }",
      "        }",
      "      },",
      '      "names": {',
      '        "Idle": "Ожидание"',
      "      },",
      '      "nodes": {',
      '        "Idle": {',
      '          "x": 10,',
      '          "y": 21',
      "        }",
      "      }",
      "    }",
      "  }",
      "}",
      "",
    ].join("\n"),
  );
  const { layout: read, problem } = layout.parse(text);
  assert.equal(problem, null);
  assert.equal(layout.canonical(read), text, "чтение и запись без правок дают тот же файл");
  assert.equal(read.sheets["/"], undefined, "пустой лист не записывается");
  assert.equal(layout.canonical(layout.parse(layout.canonical(read)).layout), text, "и повторно");
  // Умолчания не пишутся: центр знака, полка легенды снизу, пустые поля.
  const withDefaults = layout.empty();
  layout.legendAt(withDefaults, "bottom");
  layout.labelAt(withDefaults, "/", "A>B:0", "center");
  layout.nameNode(withDefaults, "/", "A", "  ");
  assert.equal(layout.canonical(withDefaults), layout.canonical(layout.empty()));
  // Плавающая легенда и базовое место знака пишутся.
  layout.legendAt(withDefaults, "float", 24.4, 72);
  layout.labelAt(withDefaults, "/", "A>B:0", "end");
  const kept = layout.parse(layout.canonical(withDefaults)).layout;
  assert.deepEqual(kept.legend, { place: "float", x: 24, y: 72 });
  assert.deepEqual(kept.sheets["/"].edges["A>B:0"], { label: { place: "end" } });
});

test("раскладка: настройки вида пишутся ступенями, а умолчания не пишутся", () => {
  // Файл хранит решение автора ("линия жирная"), а не его оформление в пикселях:
  // запиши сюда числа - и вид схемы окажется закреплён мимо оформления страницы.
  const file = layout.empty();
  layout.setView(file, "edgeWidth", "bold");
  layout.setView(file, "gamma", "draft");
  layout.setView(file, "snap", false);
  const text = layout.canonical(file);
  assert.match(text, /"edgeWidth": "bold"/);
  assert.match(text, /"snap": false/);

  // Умолчание записи не оставляет: канон несёт только отличия от вида по
  // умолчанию, иначе дифф файла в git пух бы на каждую открытую схему.
  layout.setView(file, "gamma", "color");
  assert.doesNotMatch(layout.canonical(file), /gamma/);

  // Ступень вне набора не принимается: чужое значение не вправе ни рисоваться,
  // ни доживать до записи.
  layout.setView(file, "edgeWidth", "чужая");
  assert.equal(layout.viewOf(file).edgeWidth, "bold");
  const back = layout.parse(layout.canonical(file)).layout;
  assert.deepEqual(layout.viewOf(back), layout.viewOf(file), "круговой рейс потерял ступени");

  // Умолчание места знака - тоже настройка, и центр в файле не хранится.
  layout.labelPlaceAt(file, "start");
  assert.match(layout.canonical(file), /"labelPlace": "start"/);
  layout.labelPlaceAt(file, "center");
  assert.doesNotMatch(layout.canonical(file), /labelPlace/);
});

test("раскладка: негодный файл - названная причина и пустая раскладка", () => {
  const empty = layout.canonical(layout.empty());
  for (const [text, key] of [
    ["{ не json", "scheme.fileUnreadable"],
    ["[1, 2]", "scheme.fileUnreadable"],
    ['{"format": 2, "sheets": {}}', "scheme.fileFormat"],
    ['{"sheets": {}}', "scheme.fileFormat"],
  ]) {
    const { layout: read, problem } = layout.parse(text);
    assert.equal(problem?.key, key, text);
    assert.equal(layout.canonical(read), empty, "раскладка пуста, а не частична");
  }
  const blank = layout.parse("   \n");
  assert.equal(blank.problem, null, "файла ещё нет - это не ошибка");
  assert.equal(layout.canonical(blank.layout), empty);
  // Неизвестная форма углов и место легенды - умолчание, а не отказ.
  const odd = layout.parse('{"format": 1, "corners": "wavy", "legend": {"place": "top"}}').layout;
  assert.equal(odd.corners, "square");
  assert.equal(odd.legend, undefined);
});

test("раскладка: сверка находит неразмещённые и лишние записи", () => {
  const stored = layout.empty();
  layout.place(stored, "/", "A", 8, 16);
  layout.place(stored, "/", "Gone", 0, 0);
  layout.nameNode(stored, "/", "Ghost", "призрак");
  layout.bend(stored, "/", "A>B:0", [[1, 2]]);
  layout.bend(stored, "/", "A>Gone:0", [[1, 2]]);
  layout.bend(stored, "/", "A>B:1", []);
  layout.place(stored, "Lost", "X", 0, 0);

  const report = layout.reconcile(stored, GRAPH);
  assert.deepEqual(report.sheets["/"], {
    unplaced: ["B"],
    extraNodes: ["Ghost", "Gone"],
    extraEdges: ["A>Gone:0"],
  });
  assert.deepEqual(report.extraSheets, ["Lost"]);
  assert.equal(report.extras, 4);

  const cleaned = layout.prune(stored, GRAPH);
  assert.deepEqual(layout.reconcile(cleaned, GRAPH), {
    sheets: { "/": { unplaced: ["B"], extraNodes: [], extraEdges: [] } },
    extraSheets: [],
    extras: 0,
  });
  assert.deepEqual(cleaned.sheets["/"].nodes, { A: { x: 8, y: 16 } }, "размещённое уцелело");
  assert.deepEqual(Object.keys(cleaned.sheets["/"].edges), ["A>B:0"]);
  assert.deepEqual(cleaned.sheets["/"].names, {});
  assert.deepEqual(stored.sheets.Lost.nodes, { X: { x: 0, y: 0 } }, "исходник не тронут");

  // Пустая раскладка: всё не размещено, лишнего нет - уведомления не будет.
  const fresh = layout.reconcile(layout.empty(), GRAPH);
  assert.deepEqual(fresh.sheets["/"].unplaced, ["A", "B"]);
  assert.equal(fresh.extras, 0);
});

test("раскладка: переименование состояния переносит узел, подпись и ключи рёбер", () => {
  const stored = layout.empty();
  layout.place(stored, "/", "A", 8, 16);
  layout.place(stored, "/", "B", 24, 16);
  layout.nameNode(stored, "/", "A", "Первое");
  layout.bend(stored, "/", "A>B:0", [[16, 8]]);
  layout.bend(stored, "/", "B>A:0", [[16, 24]]);
  layout.bend(stored, "/", "A>A:0", [[0, 0]]);
  layout.nameEdge(stored, "/", "A>B:0", "вперёд");

  const renamed = layout.rename(stored, "/", "A", "Run");
  assert.deepEqual(Object.keys(renamed.sheets["/"].nodes), ["B", "Run"]);
  assert.deepEqual(renamed.sheets["/"].names, { Run: "Первое" });
  assert.deepEqual(Object.keys(renamed.sheets["/"].edges), ["B>Run:0", "Run>B:0", "Run>Run:0"]);
  assert.deepEqual(renamed.sheets["/"].edges["Run>B:0"], { name: "вперёд", points: [[16, 8]] });
  // Чужой лист и то же имя - без изменений.
  assert.equal(layout.canonical(layout.rename(stored, "Other", "A", "Run")), layout.canonical(stored));
  assert.equal(layout.canonical(layout.rename(stored, "/", "A", "A")), layout.canonical(stored));
});

test("геометрия: ломаная ребра, пересечение мостиком, знак условия", () => {
  const a = { name: "A", kind: "state", x: 100, y: 100 };
  const b = { name: "B", kind: "state", x: 300, y: 300 };
  // Без изломов - один угол; концы стоят на гранях с зазором, а не в центрах.
  const pts = geo.route(a, b, []);
  assert.deepEqual(pts, [[100, 126], [100, 300], [274, 300]]);
  // Сохранённые изломы уважаются как есть.
  assert.deepEqual(geo.route(a, b, [[200, 100], [200, 300]]), [[126, 100], [200, 100], [200, 300], [274, 300]]);
  // Самопереход - петля у правого верхнего угла.
  const loop = geo.route(a, a, []);
  assert.equal(loop.length, 5);
  assert.ok(loop.every((p) => p[1] <= a.y), "петля идёт над узлом");

  // Пересечение строго перпендикулярных отрезков - мостик; слияние у грани - нет.
  const mine = [[0, 50], [100, 50]];
  assert.deepEqual(geo.crossings(mine, [[[50, 0], [50, 100]]]), [[50, 50]]);
  assert.deepEqual(geo.crossings(mine, [[[0, 50], [100, 50]]]), [], "параллельные не пересекаются");
  assert.deepEqual(geo.crossings(mine, [[[100, 0], [100, 100]]]), [], "встреча у конца - слияние");
  const path = geo.buildPath(mine, [[50, 50]], false);
  assert.match(path, /A4 4 0 0 1 54 50/, "мостик радиусом в половину шага");
  assert.match(geo.buildPath([[0, 0], [0, 100], [100, 100]], [], true), /Q0 100 5 100/, "скруглённый угол");
  assert.equal(geo.buildPath([[0, 0], [0, 100], [100, 100]], [], false), "M0 0L0 100L100 100");

  // Знак условия: центр самого длинного сегмента, начало и конец с отступом, своё.
  const line = [[0, 0], [0, 40], [100, 40]];
  assert.deepEqual(geo.markSpot(null, line), [50, 40]);
  assert.deepEqual(geo.markSpot({ place: "start" }, line), [0, 24]);
  assert.deepEqual(geo.markSpot({ place: "end" }, line), [76, 40]);
  assert.deepEqual(geo.markSpot({ place: "own", x: 8, y: 16 }, line), [8, 16]);
  assert.equal(geo.nearestSegment(line, [60, 38]), 1, "излом встаёт в ближайший сегмент");
  assert.equal(geo.snap(13), 16);
});

test("геометрия: ярусы дают строки, порядок - столбцы, и всё на сетке", () => {
  const placed = geo.autoPlace([
    { name: "A", kind: "start", rank: 0, order: 0 },
    { name: "B", kind: "state", rank: 1, order: 1 },
    { name: "C", kind: "state", rank: 1, order: 0 },
    { name: "D", kind: "composition", rank: 2, order: 0 },
  ]);
  assert.equal(placed.A.y, placed.A.y, "старт есть");
  assert.ok(placed.B.y === placed.C.y && placed.C.x < placed.B.x, "ярус - строка, порядок - столбец");
  assert.ok(placed.A.y < placed.B.y && placed.B.y < placed.D.y, "ярусы идут вниз");
  for (const p of Object.values(placed)) {
    assert.equal(p.x % geo.SNAP, 0);
    assert.equal(p.y % geo.SNAP, 0);
  }
  const size = geo.sheetSize(Object.entries(placed).map(([name, p]) => ({ name, kind: name === "D" ? "composition" : "state", ...p })));
  assert.ok(size.w >= placed.B.x + geo.R && size.h >= placed.D.y + geo.SIDE / 2, "лист вмещает рисунок");
  assert.deepEqual([size.ox, size.oy], [0, 0], "рисунок в положительной четверти - начало в нуле");
  const shifted = geo.sheetSize([{ name: "A", kind: "state", x: -40, y: 100 }]);
  assert.ok(shifted.ox < -40 - geo.R && shifted.oy === 0, "узел левее нуля двигает начало листа по своей оси");
  assert.equal(Math.abs(shifted.ox % geo.SNAP), 0, "начало листа на сетке");

  // Вид: вписать и масштаб вокруг точки.
  const box = { width: 400, height: 300 };
  const view = geo.fitView(box, 800, 600);
  assert.equal(view.k, Math.min((400 - 32) / 800, (300 - 32) / 600), "вписать по тесной оси");
  const zoomed = geo.zoomAt({ k: 1, x: 0, y: 0 }, 100, 100, 2);
  assert.deepEqual(geo.toSheet(zoomed, 100, 100), [100, 100], "точка под курсором остаётся на месте");
  assert.equal(geo.zoomAt({ k: 4, x: 0, y: 0 }, 0, 0, 2).k, geo.ZOOM_MAX, "предел масштаба");
});

test("геометрия: лист композиции - цепочка слева направо, ветви друг под другом", () => {
  const composed = geo.composeSheet({
    chain: [
      { model: { name: "Engine", path: "Engine" } },
      { group: { parallel: [{ model: { name: "Engine", path: "Engine" } }, { model: { name: "Engine", path: "Engine" } }] } },
      { model: { name: "Engine", path: "Engine" } },
    ],
  });
  assert.equal(composed.nodes.length, 4);
  const [a, b1, b2, c] = composed.nodes;
  assert.ok(a.x < b1.x && b1.x === b2.x && b2.x < c.x, "цепочка идёт вправо");
  assert.ok(b1.y < b2.y, "ветви параллели друг под другом");
  assert.deepEqual(
    composed.edges.map((e) => [e.from, e.to]),
    [[a.name, b1.name], [a.name, b2.name], [b1.name, c.name], [b2.name, c.name]],
    "шаг ведёт в каждую ветвь, каждая ветвь - в следующий шаг",
  );
  assert.equal(composed.frames.length, 2, "скобки и параллель - по рамке");
  assert.ok(composed.w > composed.h, "цепочка шире, чем выше");
  assert.equal(new Set(composed.nodes.map((n) => n.name)).size, 4, "имена узлов уникальны");
});

test("ссылка и черновик: раскладка переживает оба рейса", async () => {
  const text = layout.canonical(layout.place(layout.empty(), "/", "A", 8, 16));
  const restored = await decodeState("#" + (await encodeState({ version: "1", source: "start A;", layout: text })));
  assert.equal(restored.layout, text);
  const bare = await decodeState("#" + (await encodeState({ version: "1", source: "start A;" })));
  assert.equal(bare.layout, "", "без раскладки поле пусто, а не отсутствует");

  const storage = memoryStorage();
  draft.save(storage, { source: "start A;", layout: text });
  assert.equal(draft.load(storage).layout, text);
  draft.saveFile(storage, { project: "p", file: "a.takt", source: "start A;", layout: text });
  assert.equal(draft.loadFile(storage, "p", "a.takt").layout, text);
});

test("граф: модуль отдаёт листы лифта, а страница знает только их форму", async () => {
  const bridge = await loadBridge();
  const source = await readFile(ELEVATOR, "utf8");
  const graph = bridge.graph(source);
  assert.equal(graph.ok, true, JSON.stringify(graph));
  assert.deepEqual(
    graph.sheets.map((s) => s.path),
    ["/", "Engine"],
  );
  const engine = graph.sheets[1];
  assert.equal(engine.nodes.length, 5);
  assert.equal(engine.edges.length, 7);
  assert.equal(engine.start, "Idle");
  assert.ok(engine.edges.some((e) => e.kind === "next"), "у DoorOpening переход next");
  assert.ok(engine.edges.every((e) => e.range?.start_line !== undefined), "у ребра есть позиция");
  const middle = graph.sheets[0].nodes.find((n) => n.name === "Middle");
  assert.equal(middle.kind, "composition");
  assert.equal(middle.implements.chain.length, 5);
  assert.equal(middle.implements.chain[1].group.parallel.length, 2);
  assert.equal(middle.implements.chain[0].model.path, "Engine");
  // Лист композиции строится из выражения: шесть экземпляров, пять шагов.
  const composed = geo.composeSheet(middle.implements);
  assert.equal(composed.nodes.length, 6);
  assert.ok(composed.nodes.every((n) => n.path === "Engine"));
  // Ответ детерминирован: два вызова - один текст.
  assert.equal(JSON.stringify(bridge.graph(source)), JSON.stringify(graph));

  // Раскладка сверяется с настоящим графом: свежая - все не размещены, лишнего нет.
  const report = layout.reconcile(layout.empty(), graph);
  assert.equal(report.sheets.Engine.unplaced.length, 5);
  assert.equal(report.extras, 0);
  // Ярусы лифта раскладываются без наложений: центры узлов различны.
  const placed = geo.autoPlace(engine.nodes);
  assert.equal(new Set(Object.values(placed).map((p) => `${p.x}:${p.y}`)).size, 5);

  // Прогон отдаёт активные состояния списком на каждый такт, и все они - узлы графа.
  const opened = bridge.simOpen(source, "", 0);
  assert.equal(opened.ok, true, JSON.stringify(opened));
  const ticked = bridge.simTick(opened.id, 3);
  assert.equal(ticked.ok, true, JSON.stringify(ticked));
  assert.equal(ticked.states.length, ticked.lines.length, "по списку состояний на строку");
  // Взгляд вперёд: по списку ожидаемых переходов на строку, каждая пара - имена узлов.
  assert.equal(ticked.next.length, ticked.lines.length, "по списку ожиданий на строку");
  const known = new Set(graph.sheets.flatMap((s) => s.nodes.map((n) => n.name)));
  for (const step of ticked.states) {
    assert.ok(step.length > 0, "у такта есть активное состояние");
    for (const name of step) assert.ok(known.has(name), `состояние ${name} не узел графа`);
  }
  for (const pairs of ticked.next) {
    for (const [from, to] of pairs) assert.ok(known.has(from) && known.has(to), `переход ${from} -> ${to} мимо графа`);
  }
  bridge.simClose(opened.id);

  // Неразбираемый текст - диагностика, а не падение.
  const broken = bridge.graph("start S {");
  assert.equal(broken.ok, false);
  assert.ok(broken.error?.code, JSON.stringify(broken));
});

test("страница проекта: раскладка берётся парой к активной модели", async () => {
  const { read } = await import("../static/project.js");
  const files = [
    { name: "model.takt", kind: "takt", size_bytes: 9 },
    { name: "model.takt-ui", kind: "layout", size_bytes: 30 },
    { name: "other.takt-ui", kind: "layout", size_bytes: 30 },
  ];
  const answers = {
    "/api/projects/AbCd": { id: "AbCd", main_file: "model.takt", files },
    "/api/projects/AbCd/files/model.takt": { text: "start S;\n" },
    "/api/projects/AbCd/files/model.takt-ui": { text: '{"format": 1, "sheets": {}}\n' },
  };
  const asked = [];
  const get = async (url) => {
    asked.push(url);
    const body = answers[url];
    if (!body) return { ok: false, status: 404, json: async () => ({}) };
    return { ok: true, status: 200, json: async () => body };
  };
  const opened = await read("AbCd", "/", get);
  assert.equal(opened.layoutFile, "model.takt-ui", "пара по имени, а не первая раскладка");
  assert.equal(opened.layout, '{"format": 1, "sheets": {}}\n');
  assert.equal(asked.length, 3, `обращений ${asked.length}: ${asked.join(", ")}`);

  // Без раскладки проект открывается как до этого: пусто и без лишних обращений.
  const bare = await read("AbCd", "/", async (url) => {
    const body = url.endsWith("/AbCd")
      ? { id: "AbCd", main_file: "model.takt", files: [files[0]] }
      : answers[url];
    return { ok: true, status: 200, json: async () => body };
  });
  assert.equal(bare.layoutFile, null);
  assert.equal(bare.layout, "");
});

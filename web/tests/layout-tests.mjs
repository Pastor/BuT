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

test("раскладка: файл называет создателя, а правки ведёт журналом", () => {
  const file = layout.empty();
  const t0 = Date.parse("2026-09-08T18:00:00Z");
  layout.touch(file, "pastor", t0);
  layout.touch(file, "pastor", t0 + 5 * 60_000);
  // Правка тем же человеком в пределах часа обновляет время, а не заводит вторую
  // запись: раскладку правят перетаскиванием, и журнал стал бы длиннее раскладки.
  assert.equal(layout.metaOf(file).edits.length, 1);
  assert.equal(layout.metaOf(file).edits[0].at, "2026-09-08T18:05:00Z");

  // Читатель без входа записывается гостем: имени у него нет, а знать, что файл
  // правили, полезно.
  layout.touch(file, "", t0 + 2 * 3_600_000);
  assert.equal(layout.metaOf(file).edits.at(-1).by, layout.GUEST);
  assert.equal(layout.GUEST, "guest", "имя гостя в файле обязано быть латиницей");

  // Создатель у файла один и не меняется вторым правившим.
  assert.equal(layout.metaOf(file).createdBy, "pastor");
  assert.equal(layout.metaOf(file).createdAt, "2026-09-08T18:00:00Z");

  // Журнал в пределе: старые записи вытесняются, файл не растёт бесконечно.
  for (let i = 0; i < layout.EDITS_KEPT + 5; i += 1) {
    layout.touch(file, `автор${i}`, t0 + (i + 3) * 3_600_000);
  }
  assert.equal(layout.metaOf(file).edits.length, layout.EDITS_KEPT);
  assert.equal(layout.metaOf(file).edits.at(-1).by, `автор${layout.EDITS_KEPT + 4}`);

  // Круговой рейс сведения сохраняет, а испорченную запись отбрасывает.
  const back = layout.parse(layout.canonical(file)).layout;
  assert.deepEqual(layout.metaOf(back), layout.metaOf(file));
  const dirty = layout.parse(JSON.stringify({
    format: 1,
    meta: { createdBy: "", edits: [{ by: "кто", at: "вчера" }, { by: "", at: "2026-09-08T18:00:00Z" }] },
  })).layout;
  assert.equal(layout.metaOf(dirty), null, "негодные записи об авторстве приняты");
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

test("раскладка: подписи шагов композиции хранятся и уходят с состоянием", () => {
  const graph = {
    sheets: [{ path: "/", nodes: [{ name: "Main", implements: { model: { name: "Heater" } } }, { name: "Done" }], edges: [] }],
  };
  const stored = layout.empty();
  layout.place(stored, "/", "Main", 8, 8);
  layout.place(stored, "/", "Done", 16, 8);
  layout.nameNode(stored, layout.compositionKey("/", "Main"), "Heater#1", "Нагреватель");
  assert.deepEqual(layout.reconcile(stored, graph).extraSheets, [], "запись композиции - не лишняя");
  assert.equal(layout.prune(stored, graph).sheets["/#Main"].names["Heater#1"], "Нагреватель", "чистка её не снимает");
  const renamed = layout.rename(stored, "/", "Main", "Work");
  assert.equal(renamed.sheets["/#Work"].names["Heater#1"], "Нагреватель", "уходит с состоянием");
  assert.equal(renamed.sheets["/#Main"], undefined);
  // Состояния больше нет - запись его композиции лишняя.
  const gone = { sheets: [{ path: "/", nodes: [{ name: "Done" }], edges: [] }] };
  assert.deepEqual(layout.reconcile(stored, gone).extraSheets, ["/#Main"]);
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
  // Кривые Безье: путь проходит через каждую точку ломаной, отрезок на точку,
  // мостиков нет; касательная у конца - вдоль последнего отрезка.
  const curve = geo.buildPath([[0, 0], [0, 100], [100, 100]], [[0, 50]], "bezier");
  assert.match(curve, /^M0 0C[^C]+ 0 100C[^C]+ 100 100$/, curve);
  assert.ok(!curve.includes("A"), "на кривой мостика нет");
  assert.equal(geo.buildPath([[0, 0], [0, 100]], [], "bezier"), "M0 0C0 33.3 0 66.7 0 100", "две точки - прямая");
  // Ребро без изломов: угол раскладки - контрольная точка одной дуги, крюка за
  // угол нет; знак - на дуге.
  assert.equal(geo.buildPath([[216, 98], [216, 144], [82, 144]], [], "bezier", "hop", true), "M216 98Q216 144 82 144");
  assert.deepEqual(geo.markSpot(null, [[0, 0], [0, 100], [60, 100]], "bezier", true), [15, 75]);
  // С изломами - ручки по длине своего отрезка: короткий отрезок рядом с длинным
  // не уводит кривую за ломаную дальше, чем на треть короткого отрезка.
  const hook = geo.buildPath([[216, 98], [216, 144], [82, 144]], [], "bezier");
  const xs = [...hook.matchAll(/(-?[\d.]+) (-?[\d.]+)/g)].map((m) => Number(m[1]));
  assert.ok(Math.max(...xs) - 216 <= 46 / 3 + 0.1, `крюк за угол: ${hook}`);
  // Знак у кривой - на самой кривой, а не на середине отрезка ломаной.
  const bent = [[0, 0], [0, 100], [60, 100]];
  const spot = geo.markSpot(null, bent, "bezier");
  assert.notDeepEqual(spot, geo.markSpot(null, bent), "знак встал мимо кривой");
  assert.deepEqual(spot, geo.curvePoint(bent, 0, 0.5));

  // Знак условия: центр самого длинного сегмента, начало и конец с отступом, своё.
  const line = [[0, 0], [0, 40], [100, 40]];
  assert.deepEqual(geo.markSpot(null, line), [50, 40]);
  assert.deepEqual(geo.markSpot({ place: "start" }, line), [0, 24]);
  assert.deepEqual(geo.markSpot({ place: "end" }, line), [76, 40]);
  assert.deepEqual(geo.markSpot({ place: "own", x: 8, y: 16 }, line), [8, 16]);
  assert.equal(geo.nearestSegment(line, [60, 38]), 1, "излом встаёт в ближайший сегмент");
  // Касание рядом с линией опускается на неё: звено кнопкой изгиба не даёт.
  assert.deepEqual(geo.nearestOnLine(line, [60, 46]), { at: 1, point: [60, 40] });
  assert.deepEqual(geo.nearestOnLine(line, [-5, 10]), { at: 0, point: [0, 10] });
  assert.equal(geo.snap(13), 16);
});

test("геометрия: шестнадцать точек привязки, раздача без слияния, пересечение под углом", () => {
  const a = { name: "A", kind: "state", x: 100, y: 100 };
  // Точки идут от направления "вправо" по часовой стрелке экрана, с зазором в два пикселя.
  assert.deepEqual(geo.portPoint(a, 0), [126, 100]);
  assert.deepEqual(geo.portPoint(a, 4), [100, 126]);
  assert.deepEqual(geo.portPoint(a, 8), [74, 100]);
  assert.deepEqual(geo.portPoint(a, 12), [100, 74]);
  assert.deepEqual(geo.portPoint({ name: "C", kind: "composition", x: 0, y: 0 }, 2), [73.4, 73.4], "угол квадрата");
  assert.equal(geo.portToward(a, [100, 0]), 12);

  // Три входа в одну сторону узла - три разные точки, а не одна на всех.
  const b = { name: "B", kind: "state", x: 300, y: 100 };
  const byName = new Map([
    ["A", a],
    ["B", b],
    ["D", { name: "D", kind: "state", x: 100, y: 300 }],
    ["E", { name: "E", kind: "state", x: 100, y: -100 }],
  ]);
  const edges = [
    { from: "A", to: "B", points: [] },
    { from: "D", to: "B", points: [] },
    { from: "E", to: "B", points: [] },
  ];
  const routes = geo.routeSheet(byName, edges);
  const ends = routes.map((pts) => pts[pts.length - 1]);
  assert.equal(new Set(ends.map(String)).size, 3, `три входа - три точки: ${JSON.stringify(ends)}`);
  assert.deepEqual(ends[0], [274, 100], "прямой ход получает свою точку первым");
  for (const [x, y] of ends) assert.ok(Math.abs(Math.hypot(x - b.x, y - b.y) - (geo.R + 2)) < 0.2, "точка на окружности");
  assert.deepEqual(routes, geo.routeSheet(byName, edges), "лист раскладывается одинаково");
  // Ребро снизу берёт соседнюю точку снизу, ребро сверху - сверху: не перехлёстываются.
  assert.ok(ends[1][1] > b.y && ends[2][1] < b.y, JSON.stringify(ends));
  // Угол раскладки следует за точками: ломаная остаётся ортогональной.
  const [start, corner, end] = routes[1];
  assert.deepEqual(corner, [start[0], end[1]], JSON.stringify(routes[1]));
  // Своя точка занята - ребро уходит в сторону поворота, а не в ближнюю с другой
  // стороны: три ребра вниз из одного узла, прямое посередине.
  const top = { name: "T", kind: "state", x: 216, y: 72 };
  const fan = new Map([
    ["T", top],
    ["L", { name: "L", kind: "state", x: 72, y: 192 }],
    ["M", { name: "M", kind: "state", x: 216, y: 192 }],
    ["R", { name: "R", kind: "state", x: 360, y: 192 }],
  ]);
  const down = geo.routeSheet(fan, ["L", "R", "M"].map((to) => ({ from: "T", to, points: [] })));
  const [left, right, middle] = down.map((pts) => pts[0]);
  assert.deepEqual(middle, [216, 98], "прямое ребро - точка строго вниз");
  assert.ok(left[0] < top.x && right[0] > top.x, `повороты расходятся по сторонам: ${JSON.stringify(down)}`);
  // Конец, закреплённый автором, стоит в своей точке, а свободные его обходят.
  const pinned = geo.routeSheet(byName, [{ ...edges[0] }, { ...edges[1], ends: { to: 8 } }]);
  assert.deepEqual(pinned[1][pinned[1].length - 1], geo.portPoint(b, 8), "закреплённый конец на месте");
  assert.notDeepEqual(pinned[0][pinned[0].length - 1], geo.portPoint(b, 8), "свободный конец не встаёт в чужую точку");
  // Точку, занятую стрелкой начального состояния, ребро обходит.
  const kept = geo.routeSheet(byName, [edges[0]], new Map([["B", 8]]));
  assert.notDeepEqual(kept[0][kept[0].length - 1], geo.portPoint(b, 8));

  // Общий участок двух рёбер рисуется один раз: у второго он уходит в пропуск
  // штриха; конец в той же точке тем же ходом не несёт второго наконечника.
  const first = [[0, 0], [0, 100], [100, 100]];
  const shared = geo.overlapWith([[0, 0], [0, 100], [-50, 100]], [first]);
  assert.deepEqual(shared.runs, [[0, 100]]);
  assert.equal(shared.total, 150);
  assert.equal(shared.ending, false, "концы разные");
  assert.equal(geo.dashFor(shared.total, shared.runs), "0 100 50");
  const same = geo.overlapWith([[100, 0], [100, 100]], [first]);
  assert.deepEqual(same.runs, [], "перпендикулярный конец - не совпадение");
  assert.equal(same.ending, false, "в ту же точку, но другим ходом - наконечник свой");
  assert.equal(geo.overlapWith([[50, 100], [100, 100]], [first]).ending, true, "тот же конец тем же ходом");
  assert.deepEqual(geo.overlapWith([[0, 200], [0, 300]], [first]).runs, [], "на одной прямой, но не встык - не совпадение");
  // Пересечение под углом - тоже пересечение; вид "разрыв" прерывает линию.
  assert.deepEqual(geo.crossings([[0, 0], [100, 100]], [[[0, 100], [100, 0]]]), [[50, 50]]);
  assert.equal(geo.buildPath([[0, 50], [100, 50]], [[50, 50]], false, "gap"), "M0 50L46 50M54 50L100 50");
});

test("раскладка: закреплённые концы ребра пишутся номерами точек и снимаются", () => {
  const stored = layout.empty();
  layout.endAt(stored, "/", "A>B:0", "to", 8);
  layout.endAt(stored, "/", "A>B:0", "from", 99);
  const text = layout.canonical(stored);
  assert.match(text, /"ends": \{\s*"to": 8\s*\}/, "негодный номер не пишется");
  assert.deepEqual(layout.endsOf(layout.parse(text).layout.sheets["/"].edges["A>B:0"]), { to: 8 });
  layout.endAt(stored, "/", "A>B:0", "to", null);
  assert.doesNotMatch(layout.canonical(stored), /A>B/, "снятое закрепление не оставляет пустой записи");
});

test("раскладка: черновик сильнее проекта, но только когда он о другом", () => {
  const saved = layout.empty();
  layout.place(saved, "/", "Idle", 72, 72);
  const savedText = layout.canonical(saved);
  const moved = layout.parse(savedText).layout;
  layout.place(moved, "/", "Idle", 216, 72);
  // Узел подвинут и не сохранён: перезагрузка берёт черновик.
  assert.deepEqual(layout.preferDraft(layout.canonical(moved), savedText), { text: layout.canonical(moved), fromDraft: true });
  // Тот же смысл в другой записи - не правка; пустой черновик и его отсутствие - тоже.
  assert.equal(layout.preferDraft(JSON.stringify(JSON.parse(savedText)), savedText).fromDraft, false);
  assert.equal(layout.preferDraft(layout.canonical(layout.empty()), savedText).fromDraft, false);
  assert.deepEqual(layout.preferDraft(undefined, savedText), { text: savedText, fromDraft: false });
  // Файла раскладки нет, а в черновике расстановка есть - она и берётся.
  assert.equal(layout.preferDraft(layout.canonical(moved), "").fromDraft, true);
});

test("схема: двойное касание входит в квадрат", async () => {
  const { doubleTap } = await import("../static/scheme.js");
  assert.equal(doubleTap(null, "Main", 100), false, "первое касание");
  assert.equal(doubleTap({ name: "Main", at: 100 }, "Main", 400), true, "второе вскоре - вход");
  assert.equal(doubleTap({ name: "Main", at: 100 }, "Main", 600), false, "второе поздно - снова первое");
  assert.equal(doubleTap({ name: "Idle", at: 100 }, "Main", 200), false, "другой узел");
});

test("прогон: на листе модели горит экземпляр, а стрелка - та, что сработает", async () => {
  const run = await import("../static/scheme-run.js");
  const sheet = {
    path: "Engine",
    nodes: [{ name: "Idle" }, { name: "Heat" }, { name: "Cool" }],
    edges: [
      { key: "a", from: "Idle", to: "Heat" },
      { key: "b", from: "Idle", to: "Cool" },
      { key: "c", from: "Heat", to: "Idle" },
    ],
  };
  const place = { root: false, model: "Engine", owner: null };
  const at = (state, model = "Engine") => ({ path: [{ owner: "Main", step: 1, model }], model, state, done: false });
  const lit = run.sheetRun(sheet, { active: [at("Idle")], next: [["Idle", "Heat"]] }, place);
  assert.deepEqual([...lit.running], ["Idle"]);
  assert.deepEqual([...lit.nextEdges], ["a"], "из нескольких выходящих - та, что сработает");
  assert.deepEqual([...lit.expected], ["Heat"]);
  assert.deepEqual([...lit.reachable], ["Cool"], "достижимое - прочие цели рёбер из горящего");
  assert.deepEqual([...run.sheetRun(sheet, { active: [at("Heat")], next: [["Idle", "Heat"]] }, place).nextEdges], [], "начало не горит - стрелки нет");
  // Чужая модель с тем же именем состояния на этом листе не горит.
  assert.deepEqual([...run.sheetRun(sheet, { active: [at("Idle", "Pump")], next: [] }, place).running], []);
  // Два экземпляра в одном состоянии - число на узле.
  const two = run.sheetRun(sheet, { active: [at("Idle"), at("Idle"), at("Heat")], next: [] }, place);
  assert.equal(two.counts.get("Idle"), 2);
  assert.equal(two.counts.get("Heat"), 1);
  // Корневой лист горит по адресам с пустым путём.
  const root = run.sheetRun(
    { path: "/", nodes: [{ name: "Main" }], edges: [] },
    { active: [{ path: [], model: null, state: "Main", done: false }, at("Main")], next: [] },
    { root: true, model: null, owner: null },
  );
  assert.equal(root.counts.get("Main"), 1, "экземпляр вложенной модели корневой лист не зажигает");
});

/** Лист композиции `Main = Heater + (Pump | Pump) + Heater` - так, как его строит холст. */
function composedSheet() {
  const composed = geo.composeSheet(COMPOSED.sheets[0].nodes[0].implements);
  return {
    path: null,
    owner: "Main",
    ownerPath: "/",
    nodes: composed.nodes,
    edges: composed.edges.map((e) => ({ ...e, key: layout.edgeKey(e) })),
  };
}

/** Адрес шага `step` листа `Main` корневой модели. */
const stepAt = (step, model, state, done = false) => ({ path: [{ owner: "Main", step, model }], model, state, done });

test("прогон: лист композиции горит идущим шагом, а не всеми шагами той же модели", async () => {
  const run = await import("../static/scheme-run.js");
  const sheet = composedSheet();
  const place = run.placeOf(sheet, COMPOSED);
  assert.deepEqual(place, { root: true, model: null, owner: "Main" });
  const first = run.sheetRun(sheet, { active: [stepAt(1, "Heater", "Heating")], next: [] }, place);
  assert.deepEqual([...first.running], ["Heater#1"], "горит первый шаг, второй шаг той же модели - нет");
  assert.deepEqual([...first.reachable].sort(), ["Pump#1", "Pump#2"]);
  assert.deepEqual([...first.expected], [], "шаг не завершён - следующего не ждут");

  // Шаг завершён: стрелки к следующему шагу горят, следующий шаг ожидается.
  const done = run.sheetRun(sheet, { active: [stepAt(1, "Heater", "Done", true)], next: [] }, place);
  assert.deepEqual([...done.expected].sort(), ["Pump#1", "Pump#2"]);
  assert.deepEqual([...done.nextEdges].sort(), ["Heater#1>Pump#1:0", "Heater#1>Pump#2:0"]);

  // Параллель горит обеими ветвями; следующий шаг ждут, только когда обе завершены.
  const half = run.sheetRun(sheet, { active: [stepAt(2, "Pump", "Off", true), stepAt(3, "Pump", "On")], next: [] }, place);
  assert.deepEqual([...half.running], ["Pump#1", "Pump#2"]);
  assert.deepEqual([...half.expected], [], "одна ветвь завершена - мало");
  const both = run.sheetRun(sheet, { active: [stepAt(2, "Pump", "Off", true), stepAt(3, "Pump", "Off", true)], next: [] }, place);
  assert.deepEqual([...both.expected], ["Heater#2"]);
  assert.deepEqual([...both.nextEdges].sort(), ["Pump#1>Heater#2:0", "Pump#2>Heater#2:0"]);
});

test("прогон: одноимённый владелец другой модели чужой лист не зажигает", async () => {
  const run = await import("../static/scheme-run.js");
  // Лист композиции состояния Run модели A: шаги под сегментом Run, перед которым
  // стоит экземпляр модели A.
  const place = { root: false, model: "A", owner: "Run" };
  const of = (path) => run.stepsOf([{ path, model: "E", state: "S", done: false }], place, "Run");
  assert.equal(of([{ owner: "Main", step: 1, model: "A" }, { owner: "Run", step: 2, model: "E" }]).has(2), true);
  assert.equal(of([{ owner: "Main", step: 1, model: "B" }, { owner: "Run", step: 2, model: "E" }]).size, 0, "владелец Run модели B - чужой");
  assert.equal(of([{ owner: "Run", step: 2, model: "E" }]).size, 0, "корневой Run - не этот лист");
});

test("прогон: плашка квадрата перечисляет шаги с состояниями", async () => {
  const run = await import("../static/scheme-run.js");
  const steps = [{ name: "Engine#1" }, { name: "Engine#2", alias: "Разгон" }, { name: "Engine#3" }];
  const groups = run.stepsOf([stepAt(2, "Engine", "Idle"), stepAt(3, "Engine", "Moving")], { root: true, model: null }, "Main");
  const label = (step, group) => [...run.statesOf(group)].join(", ");
  assert.equal(run.stepsLabel(steps, groups, label), "Разгон: Idle, Engine#3: Moving", "подпись шага сильнее имени");
  const one = run.stepsOf([stepAt(3, "Engine", "Moving")], { root: true, model: null }, "Main");
  assert.equal(run.stepsLabel(steps, one, label), "Moving", "один идущий шаг - без имени шага");
});

test("прогон: плашка композиции называет текущее внутреннее состояние", async () => {
  const { innerLabel } = await import("../static/scheme-run.js");
  const nodes = [{ name: "Idle", alias: "" }, { name: "Heat", alias: "Нагрев" }, { name: "Cool" }];
  assert.equal(innerLabel(nodes, new Set(["Main", "Idle"])), "Idle", "без подписи - имя");
  assert.equal(innerLabel(nodes, new Set(["Main", "Heat"])), "Нагрев", "подпись автора сильнее имени");
  assert.equal(innerLabel(nodes, new Set(["Main", "Heat", "Cool"])), "Нагрев, Cool", "у параллели - все активные");
  assert.equal(innerLabel(nodes, new Set(["Done"])), "", "внутри не идёт ничего");
});

test("прогон: два экземпляра Heater у pid_heater горят по очереди", async () => {
  const run = await import("../static/scheme-run.js");
  const bridge = await loadBridge();
  const source = await readFile(new URL("../../examples/pid_heater.takt", import.meta.url), "utf8");
  const library = await readFile(new URL("../../examples/pid_law.takt", import.meta.url), "utf8");
  const graph = bridge.graph(source);
  assert.equal(graph.ok, true, JSON.stringify(graph));
  const node = graph.sheets.find((s) => s.path === "/").nodes.find((n) => n.name === "PidHeater");
  const composed = geo.composeSheet(node.implements);
  const sheet = {
    path: null,
    owner: "PidHeater",
    ownerPath: "/",
    nodes: composed.nodes,
    edges: composed.edges.map((e) => ({ ...e, key: layout.edgeKey(e) })),
  };
  const place = run.placeOf(sheet, graph);
  const opened = bridge.simOpen(source, "", 0, { "pid_law.takt": library });
  assert.equal(opened.ok, true, JSON.stringify(opened));
  const ticked = bridge.simTick(opened.id, 200);
  assert.equal(ticked.ok, true, JSON.stringify(ticked));
  const seen = [];
  let handoff = false;
  for (const active of ticked.active) {
    const lit = run.sheetRun(sheet, { active, next: [] }, place);
    const now = [...lit.running].join();
    if (now && seen[seen.length - 1] !== now) seen.push(now);
    if (lit.expected.has("Heater#2")) handoff = true;
  }
  assert.deepEqual(seen, ["Heater#1", "Heater#2"], "шаги горят по очереди, а не оба разом");
  assert.ok(handoff, "завершённый первый шаг подсвечивает второй");
});

test("раскладка: форма рёбер «кривые Безье» переживает круговой рейс", () => {
  const stored = layout.empty();
  stored.corners = "bezier";
  assert.equal(layout.parse(layout.canonical(stored)).layout.corners, "bezier");
  assert.equal(layout.parse('{"format": 1, "corners": "spline", "sheets": {}}').layout.corners, "square", "чужая форма - умолчание");
});

test("раскладка: место стрелки начального состояния и вид пересечения пишутся ступенями", () => {
  const stored = layout.empty();
  layout.entryAt(stored, "/", 12);
  layout.setView(stored, "crossing", "gap");
  const text = layout.canonical(stored);
  assert.match(text, /"entry": 12/);
  assert.match(text, /"crossing": "gap"/);
  const read = layout.parse(text).layout;
  assert.equal(layout.entryOf(read, "/"), 12);
  assert.equal(layout.viewOf(read).crossing, "gap");
  // Умолчания не пишутся; чужой номер точки читается умолчанием.
  layout.entryAt(stored, "/", geo.ENTRY_PORT);
  layout.setView(stored, "crossing", "hop");
  assert.doesNotMatch(layout.canonical(stored), /entry|crossing/);
  const alien = layout.parse('{"format": 1, "sheets": {"/": {"entry": 99}}}').layout;
  assert.equal(layout.entryOf(alien, "/"), geo.ENTRY_PORT);
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
  // Излом на нулевой вертикали и узел у верхнего края: запас вмещает излом со
  // знаком и петлю самоперехода - край листа их не срезает.
  const edge = geo.sheetSize([{ name: "A", kind: "state", x: 216, y: 72 }], [[0, 200]]);
  assert.ok(edge.ox <= -geo.SNAP * 3, `излом у края срезан: ox ${edge.ox}`);
  const top = geo.sheetSize([{ name: "A", kind: "state", x: 216, y: 40 }]);
  assert.ok(40 - geo.R - 20 >= top.oy, `петля над узлом у края срезана: oy ${top.oy}`);

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
  // Имя шага - модель и номер среди её шагов: перестановка шагов разных моделей
  // не меняет имён, и подписи автора остаются у своих квадратов.
  const heater = { model: { name: "Heater", path: "Heater" } };
  const pump = { model: { name: "Pump", path: "Pump" } };
  assert.deepEqual(geo.composeSheet({ chain: [heater, pump] }).nodes.map((n) => n.name), ["Heater#1", "Pump#1"]);
  assert.deepEqual(geo.composeSheet({ chain: [pump, heater] }).nodes.map((n) => n.name), ["Pump#1", "Heater#1"]);
  assert.deepEqual(geo.composeSheet({ chain: [heater, pump, heater] }).nodes.map((n) => n.name), ["Heater#1", "Pump#1", "Heater#2"]);
});

/** Составное состояние `Main = Heater + (Pump | Pump) + Heater` и конец. */
const COMPOSED = {
  sheets: [{
    path: "/",
    nodes: [
      {
        name: "Main",
        kind: "composition",
        implements: {
          chain: [
            { model: { name: "Heater", path: "Heater" } },
            { group: { parallel: [{ model: { name: "Pump", path: "Pump" } }, { model: { name: "Pump", path: "Pump" } }] } },
            { model: { name: "Heater", path: "Heater" } },
          ],
        },
      },
      { name: "Done", kind: "end" },
    ],
    edges: [{ from: "Main", to: "Done", ordinal: 0, kind: "next" }],
  }],
};

test("раскладка: ключ ребра листа композиции - номер в паре, как у листа модели", () => {
  const composed = geo.composeSheet(COMPOSED.sheets[0].nodes[0].implements);
  const keys = composed.edges.map(layout.edgeKey);
  assert.deepEqual(keys, ["Heater#1>Pump#1:0", "Heater#1>Pump#2:0", "Pump#1>Heater#2:0", "Pump#2>Heater#2:0"]);
  assert.equal(new Set(keys).size, keys.length, "ключи различны");
  const again = geo.composeSheet(COMPOSED.sheets[0].nodes[0].implements).edges.map(layout.edgeKey);
  assert.deepEqual(again, keys, "ключи стабильны");
  // Номер - в паре, а не среди всех рёбер листа: вставка ветви в параллель не
  // сдвигает ключей соседних рёбер.
  const wider = geo.composeSheet({
    chain: [
      { model: { name: "Heater" } },
      { parallel: [{ model: { name: "Pump" } }, { model: { name: "Pump" } }, { model: { name: "Fan" } }] },
      { model: { name: "Heater" } },
    ],
  }).edges.map(layout.edgeKey);
  for (const key of keys) assert.ok(wider.includes(key), `ключ ${key} пережил вставку ветви`);
});

test("раскладка: сверка листа композиции - по шагам и рёбрам выражения", () => {
  const key = layout.compositionKey("/", "Main");
  const fresh = layout.reconcile(layout.empty(), COMPOSED);
  assert.deepEqual(fresh.sheets[key].unplaced, ["Heater#1", "Pump#1", "Pump#2", "Heater#2"], "свежий файл: шаги не размещены");
  assert.equal(fresh.extras, 0, "свежий файл - не расхождение");

  const stored = layout.empty();
  for (const [i, name] of ["Heater#1", "Pump#1", "Pump#2", "Heater#2"].entries()) layout.place(stored, key, name, i * 200, 72);
  layout.bend(stored, key, "Pump#1>Heater#2:0", [[400, 40]]);
  const placed = layout.reconcile(stored, COMPOSED);
  assert.deepEqual(placed.sheets[key], { unplaced: [], extraNodes: [], extraEdges: [] }, "все шаги на месте");

  // Выражение потеряло шаг: запись третьего насоса и его ребро - лишние.
  layout.place(stored, key, "Pump#3", 0, 0);
  layout.nameNode(stored, key, "Ghost#1", "призрак");
  layout.bend(stored, key, "Pump#3>Heater#2:0", [[1, 2]]);
  layout.bend(stored, key, "Heater#1>Heater#2:0", [[1, 2]]);
  const stale = layout.reconcile(stored, COMPOSED);
  assert.deepEqual(stale.sheets[key].extraNodes, ["Ghost#1", "Pump#3"]);
  assert.deepEqual(stale.sheets[key].extraEdges, ["Heater#1>Heater#2:0", "Pump#3>Heater#2:0"]);
  assert.equal(stale.extras, 4);
  assert.deepEqual(stale.extraSheets, [], "лист композиции - не лишний лист");

  const cleaned = layout.prune(stored, COMPOSED);
  assert.deepEqual(layout.reconcile(cleaned, COMPOSED).sheets[key], { unplaced: [], extraNodes: [], extraEdges: [] });
  assert.deepEqual(cleaned.sheets[key].edges["Pump#1>Heater#2:0"].points, [[400, 40]], "своё чистка не трогает");

  // Состояния больше нет - лист композиции лишний целиком.
  const gone = { sheets: [{ path: "/", nodes: [{ name: "Done", kind: "end" }], edges: [] }] };
  assert.deepEqual(layout.reconcile(stored, gone).extraSheets, [key]);
});

test("геометрия: рамки скобок и параллели обнимают свои шаги", () => {
  const implement = COMPOSED.sheets[0].nodes[0].implements;
  const composed = geo.composeSheet(implement);
  const at = new Map(composed.nodes.map((n) => [n.name, { x: n.x, y: n.y }]));
  assert.deepEqual(geo.framesOf(implement, at), composed.frames, "по форме выражения - те же рамки");
  const [outer, inner] = composed.frames;
  assert.equal(inner.parallel, true);
  assert.equal(outer.parallel, undefined, "внешняя - скобки, она идёт первой");

  // Шаг параллели унесён вниз за прежнюю рамку: обе рамки его вмещают. Унеси его
  // под соседний шаг - и рамка обнимет чужой: это названная граница, а не дефект.
  at.set("Pump#2", { x: at.get("Pump#2").x, y: at.get("Pump#2").y + 400 });
  const [movedOuter, movedInner] = geo.framesOf(implement, at);
  const h = geo.SIDE / 2;
  const inside = (frame, p) => frame.x <= p.x - h && p.x + h <= frame.x + frame.w && frame.y <= p.y - h && p.y + h <= frame.y + frame.h;
  assert.ok(inside(movedInner, at.get("Pump#2")), "параллель вмещает унесённый шаг");
  assert.ok(inside(movedInner, at.get("Pump#1")), "и оставшийся");
  assert.ok(
    movedOuter.x < movedInner.x && movedOuter.y < movedInner.y &&
      movedInner.x + movedInner.w < movedOuter.x + movedOuter.w && movedInner.y + movedInner.h < movedOuter.y + movedOuter.h,
    "вложенная рамка внутри внешней",
  );
  assert.ok(!inside(movedInner, at.get("Heater#2")), "чужой шаг рамка не забирает");
  assert.deepEqual(geo.framesOf({ group: { chain: [] } }, new Map()), [], "рамки без шагов нет");
});

/** Холст без страницы: методы листа и правки на объекте класса, отрисовка - заглушка. */
async function bareScheme(graph, stored = layout.empty()) {
  const { Scheme } = await import("../static/scheme.js");
  const scheme = Object.create(Scheme.prototype);
  Object.assign(scheme, {
    graph,
    layout: stored,
    undo: [],
    redo: [],
    trail: [{ kind: "model", path: "/" }, { kind: "composition", sheetPath: "/", node: "Main" }],
    selected: null,
    t: (key) => key,
    who: () => "",
    draw: () => {},
    onChange: () => {},
  });
  return scheme;
}

test("холст: лист композиции читает запись и правится по её ключу", async () => {
  const key = layout.compositionKey("/", "Main");
  const scheme = await bareScheme(COMPOSED);
  const fresh = scheme.current();
  assert.equal(fresh.key, key);
  assert.equal(fresh.path, null, "во вложенную композицию с листа композиции не входят");
  assert.equal(fresh.editable, true);
  assert.ok(fresh.nodes.every((n) => n.unplaced), "без записи шаги не размещены");
  assert.deepEqual(fresh.frames, geo.composeSheet(COMPOSED.sheets[0].nodes[0].implements).frames, "форма выражения");

  // Перенос шага пишет запись листа композиции, а не листа модели.
  scheme.moveNode("Pump#2", 603, 797);
  assert.deepEqual(scheme.layout.sheets[key].nodes["Pump#2"], { x: 600, y: 800 }, "с привязкой к сетке");
  assert.equal(scheme.layout.sheets["/"], undefined);
  const moved = scheme.current();
  const pump = moved.nodes.find((n) => n.name === "Pump#2");
  assert.deepEqual([pump.x, pump.y, pump.unplaced], [600, 800, false]);
  assert.ok(moved.nodes.filter((n) => n.name !== "Pump#2").every((n) => n.unplaced), "прочие шаги по-прежнему не размещены");
  // Рамки пошли за шагом, а лист вместил и шаг, и рамки.
  const h = geo.SIDE / 2;
  for (const frame of moved.frames) assert.ok(frame.y + frame.h >= 800 + h, "рамка вмещает унесённый шаг");
  for (const frame of moved.frames) {
    assert.ok(moved.ox <= frame.x && frame.x + frame.w <= moved.ox + moved.w, "лист вмещает рамку по ширине");
    assert.ok(moved.oy <= frame.y && frame.y + frame.h <= moved.oy + moved.h, "и по высоте");
  }

  // Излом и закреплённый конец ребра между шагами читаются из записи.
  layout.bend(scheme.layout, key, "Pump#2>Heater#2:0", [[640, 900]]);
  layout.endAt(scheme.layout, key, "Pump#2>Heater#2:0", "from", 4);
  const edge = scheme.current().edges.find((e) => e.key === "Pump#2>Heater#2:0");
  assert.deepEqual(edge.points, [[640, 900]]);
  assert.deepEqual(edge.ends, { from: 4 });
  assert.ok(scheme.current().oy + scheme.current().h >= 900, "лист вмещает излом");

  // Отмена возвращает прежнее положение; автораскладка снимает координаты и изломы,
  // подписи остаются.
  scheme.undoLast();
  assert.equal(scheme.layout.sheets?.[key]?.nodes?.["Pump#2"], undefined, "отмена снимает перенос");
  scheme.redoLast();
  assert.deepEqual(scheme.layout.sheets[key].nodes["Pump#2"], { x: 600, y: 800 }, "повтор возвращает");
  layout.bend(scheme.layout, key, "Pump#2>Heater#2:0", [[640, 900]]);
  layout.nameNode(scheme.layout, key, "Heater#1", "Нагрев");
  scheme.autoLayout();
  const auto = scheme.current();
  assert.ok(auto.nodes.every((n) => n.unplaced), "автораскладка - форма выражения");
  assert.deepEqual(auto.edges.find((e) => e.key === "Pump#2>Heater#2:0").points, []);
  assert.equal(auto.nodes.find((n) => n.name === "Heater#1").alias, "Нагрев", "подпись осталась");
});

test("холст: миниатюра квадрата композиции повторяет хранимую раскладку", async () => {
  const key = layout.compositionKey("/", "Main");
  const stored = layout.place(layout.empty(), key, "Heater#2", 960, 72);
  const scheme = await bareScheme(COMPOSED, stored);
  scheme.trail = [{ kind: "model", path: "/" }];
  const parent = scheme.current();
  const main = parent.nodes.find((n) => n.name === "Main");
  // Миниатюра рисует лист, куда ведёт вход в квадрат, - тот же, что открывается.
  const inner = scheme.sheetOf(scheme.target(parent, main));
  assert.deepEqual(
    [inner.nodes.find((n) => n.name === "Heater#2").x, inner.nodes.find((n) => n.name === "Heater#2").y],
    [960, 72],
  );
});

test("холст: переименование модели переносит шаги, состояния - только своё", async () => {
  const key = layout.compositionKey("/", "Main");
  const stored = layout.place(layout.empty(), key, "Pump#1", 272, 72);
  layout.place(stored, "/", "Done", 16, 8);
  const scheme = await bareScheme(COMPOSED, stored);
  scheme.renamed("Pump", "Blower");
  assert.deepEqual(Object.keys(scheme.layout.sheets[key].nodes), ["Blower#1"], "модель выражения - шаги переехали");
  scheme.renamed("Done", "Finish");
  assert.deepEqual(Object.keys(scheme.layout.sheets["/"].nodes), ["Finish"]);
  // Состояние, совпавшее именем с моделью, которой в выражениях нет, шагов не трогает.
  const { composedModels } = await import("../static/scheme.js");
  assert.deepEqual([...composedModels(COMPOSED)].sort(), ["Heater", "Pump"]);
  assert.deepEqual([...composedModels(null)], []);
});

test("холст: уведомление называет лист композиции именем состояния", async () => {
  const { sheetLabel } = await import("../static/scheme.js");
  assert.equal(sheetLabel("/"), "");
  assert.equal(sheetLabel("Engine"), "Engine");
  assert.equal(sheetLabel("/#Middle"), "Middle");
  assert.equal(sheetLabel("Engine#Run"), "Engine/Run");
});

test("раскладка: реализация одной моделью своего листа не имеет", () => {
  assert.equal(layout.hasCompositionSheet({ model: { name: "Heater" } }), false);
  assert.equal(layout.hasCompositionSheet({ group: { model: { name: "Heater" } } }), true, "скобки - уже лист");
  assert.equal(layout.hasCompositionSheet(COMPOSED.sheets[0].nodes[0].implements), true);
  const graph = { sheets: [{ path: "/", nodes: [{ name: "Main", implements: { model: { name: "Heater" } } }], edges: [] }] };
  const report = layout.reconcile(layout.empty(), graph);
  assert.equal(report.sheets["/#Main"], undefined, "сверять по шагам нечего");
});

test("раскладка: переименование модели переносит записи шагов композиции", () => {
  const key = layout.compositionKey("/", "Main");
  const stored = layout.empty();
  layout.place(stored, key, "Heater#1", 72, 72);
  layout.place(stored, key, "Pump#2", 272, 172);
  layout.nameNode(stored, key, "Heater#2", "Догрев");
  layout.bend(stored, key, "Pump#2>Heater#2:0", [[300, 200]]);
  // На листе модели имя без номера - не шаг, и переименование модели его не трогает.
  layout.place(stored, "/", "Heater", 8, 8);
  const renamed = layout.renameModel(stored, "Heater", "Boiler");
  const sheet = renamed.sheets[key];
  assert.deepEqual(Object.keys(sheet.nodes), ["Boiler#1", "Pump#2"]);
  assert.deepEqual(sheet.names, { "Boiler#2": "Догрев" });
  assert.deepEqual(Object.keys(sheet.edges), ["Pump#2>Boiler#2:0"]);
  assert.deepEqual(renamed.sheets["/"].nodes, { Heater: { x: 8, y: 8 } }, "лист модели не тронут");
  // Модели с общим началом имени не задеты: переносится только точное имя.
  const prefixed = layout.renameModel(layout.place(layout.empty(), key, "HeaterPro#1", 0, 0), "Heater", "Boiler");
  assert.deepEqual(Object.keys(prefixed.sheets[key].nodes), ["HeaterPro#1"]);
  assert.equal(layout.canonical(layout.renameModel(stored, "Heater", "Heater")), layout.canonical(stored), "то же имя - без правки");
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
  assert.equal(report.sheets["/#Middle"].unplaced.length, 6, "лист композиции сверяется по шагам");
  assert.equal(report.extras, 0);
  // Полная раскладка - та, что пишет скрипт примеров: каждый лист, включая лист
  // композиции, записан, и сверка не находит ни неразмещённых, ни лишнего.
  const full = layout.placeAll(graph);
  assert.equal(Object.keys(full.sheets["/#Middle"].nodes).length, 6, "шесть шагов лифта записаны");
  const complete = layout.reconcile(full, graph);
  for (const [key, found] of Object.entries(complete.sheets)) assert.deepEqual(found.unplaced, [], `лист ${key} полон`);
  assert.equal(complete.extras, 0);
  // Ярусы лифта раскладываются без наложений: центры узлов различны.
  const placed = geo.autoPlace(engine.nodes);
  assert.equal(new Set(Object.values(placed).map((p) => `${p.x}:${p.y}`)).size, 5);

  // Прогон отдаёт активные состояния списком на каждый такт, и все они - узлы графа.
  const opened = bridge.simOpen(source, "", 0);
  assert.equal(opened.ok, true, JSON.stringify(opened));
  const ticked = bridge.simTick(opened.id, 3);
  assert.equal(ticked.ok, true, JSON.stringify(ticked));
  // Адрес экземпляра идёт рядом со списком имён: по списку на строку, имена те же.
  assert.equal(ticked.active.length, ticked.lines.length, "адреса - по списку на строку");
  ticked.active.forEach((tick, i) => assert.deepEqual(tick.map((a) => a.state), ticked.states[i], "имена адресов - это states"));
  assert.ok(ticked.active.every((tick) => tick.every((a) => Array.isArray(a.path) && typeof a.done === "boolean")), "форма адреса");
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

test("сборка: цель с адресами получает карту проекта, явная карта сильнее", async () => {
  const { ADDRESS_TARGETS, namesAddressMap, withAddressMap } = await import("../static/project.js");
  assert.deepEqual(ADDRESS_TARGETS, ["c-hal", "st-at", "sv-mmio"]);
  assert.equal(withAddressMap("", "ports16.takt-map"), "--address-map ports16.takt-map");
  assert.equal(withAddressMap("--fsm=table", "ports16.takt-map"), "--fsm=table --address-map ports16.takt-map");
  assert.equal(withAddressMap("--address-map board.takt-map", "ports16.takt-map"), "--address-map board.takt-map", "явная карта сильнее");
  assert.equal(namesAddressMap("--address-map=board.takt-map"), true);
  assert.equal(namesAddressMap("--address-maps"), false, "чужой ключ - не карта");
});

test("справка: поиск без учёта регистра, раздел читателя - по положению заголовков", async () => {
  const { findRanges, sectionAt } = await import("../static/help.js");
  assert.deepEqual(findRanges("Модель model МОДЕЛЬ", "модель"), [[0, 6], [13, 19]]);
  assert.deepEqual(findRanges("aaaa", "aa"), [[0, 2], [2, 4]], "вхождения не перекрываются");
  assert.deepEqual(findRanges("текст", "т"), [], "одна буква - не поиск");
  const tops = [{ id: "a", top: -300 }, { id: "b", top: -10 }, { id: "c", top: 400 }];
  assert.equal(sectionAt(tops, 24), "b", "последний поднявшийся выше отметки");
  assert.equal(sectionAt([{ id: "a", top: 50 }], 24), "a", "выше первого заголовка - первый раздел");
  assert.equal(sectionAt([], 24), null);
});

test("прогон: задержка между тактами - секунды, дробные, в пределе", async () => {
  const { runDelay } = await import("../static/project.js");
  assert.equal(runDelay("0.5"), 0.5);
  assert.equal(runDelay("1.23456"), 1.235, "до миллисекунды");
  assert.equal(runDelay(""), 0, "пусто - без задержки");
  assert.equal(runDelay("-2"), 0, "отрицательное - без задержки");
  assert.equal(runDelay("abc"), 0);
  assert.equal(runDelay("600"), 60, "больше предела - предел");
});

test("страница проекта: сценарий прогона - только сценарий открытой модели", async () => {
  const { pickScenario } = await import("../static/project.js");
  const own = ["lift-busy.json", "lift-idle.json"];
  assert.equal(pickScenario(own, "lift-idle.json", "lift-busy.json"), "lift-idle.json", "свой текущий остаётся");
  assert.equal(pickScenario(own, "ports16-run.json", "lift-busy.json"), "lift-busy.json", "чужой текущий уступает названному проектом");
  assert.equal(pickScenario(own, "ports16-run.json", "ports16-run.json"), "lift-busy.json", "названный проектом, но чужой - первый свой");
  assert.equal(pickScenario([], "ports16-run.json", "ports16-run.json"), null, "своих нет - без сценария");
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

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

test("прогон: подсвечена стрелка, которая сработает на следующем такте", async () => {
  const { nextEdgeKeys } = await import("../static/scheme.js");
  const edges = [
    { key: "a", from: "Idle", to: "Heat" },
    { key: "b", from: "Idle", to: "Cool" },
    { key: "c", from: "Heat", to: "Idle" },
  ];
  assert.deepEqual([...nextEdgeKeys(edges, new Set(["Idle"]), [["Idle", "Heat"]])], ["a"], "из нескольких выходящих - та, что сработает");
  assert.deepEqual([...nextEdgeKeys(edges, new Set(["Heat"]), [["Idle", "Heat"]])], [], "начало не активно - не подсвечивается");
  assert.deepEqual([...nextEdgeKeys(edges, new Set(["Idle"]), [])], [], "перехода нет - нет и стрелки");
});

test("прогон: плашка композиции называет текущее внутреннее состояние", async () => {
  const { innerLabel } = await import("../static/scheme.js");
  const nodes = [{ name: "Idle", alias: "" }, { name: "Heat", alias: "Нагрев" }, { name: "Cool" }];
  assert.equal(innerLabel(nodes, new Set(["Main", "Idle"])), "Idle", "без подписи - имя");
  assert.equal(innerLabel(nodes, new Set(["Main", "Heat"])), "Нагрев", "подпись автора сильнее имени");
  assert.equal(innerLabel(nodes, new Set(["Main", "Heat", "Cool"])), "Нагрев, Cool", "у параллели - все активные");
  assert.equal(innerLabel(nodes, new Set(["Done"])), "", "внутри не идёт ничего");
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

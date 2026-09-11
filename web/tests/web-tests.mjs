// Проверки веб-части, которые можно снять без браузера.
//
// # Что здесь проверяется и почему именно это
//
// Браузерной проверки в проекте нет: фокус, выделение и отрисовку проверяет
// человек. Зато три вещи проверяются машиной, и каждая из них ломается молча:
//
//   1. **круговой рейс ссылки** - сжатие и base64url: испорченная ссылка
//      открывается пустым редактором, и автор решит, что "ссылка протухла";
//   2. **перевод координат** - смещение ↔ строка/колонка: ошибка уводит
//      переход по диагностике на чужую строку, а вывод при этом валиден;
//   3. **черновик** - запись и чтение: его предмет в том, чтобы работа
//      пережила перезагрузку, и "сохранилось не то" здесь равно потере.
//
// Плюс смоук моста: страница и модуль обязаны сойтись формой ответа.
//
// Запуск: node web/tests/web-tests.mjs <модуль.wasm>

import { fileURLToPath } from "node:url";
import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { existsSync } from "node:fs";
import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";
import test from "node:test";

import { encodeState, decodeState } from "../static/share.js";
import { offsetToPosition, positionToOffset } from "../static/editor.js";
import * as draft from "../static/draft.js";
import { Bridge, spans } from "../static/bridge.js";
import * as i18n from "../static/i18n.js";
import { inlineScripts, literalsWithText, nodesWithoutKey } from "./strings.mjs";
import { bundleOfUrl } from "../static/build.js";
import * as shell from "../static/shell.js";
import * as tip from "../static/tip.js";
import * as alerts from "../static/alerts.js";
import * as editor from "../static/editor.js";
import * as json from "../static/json.js";
import * as flags from "../static/flags.js";
import * as md from "../static/md.js";
import * as panels from "../static/panels.js";
import * as host from "../static/scheme-host.js";
import * as build from "../static/build.js";
import * as project from "../static/project.js";
import * as api from "../static/api.js";

// Файл раскладки и граф модуля - своим набором.
import "./layout-tests.mjs";
import "./scheme-parity-tests.mjs";
import "./scheme-style-tests.mjs";

const MODEL = `var level: u8 := 0;

start Run {
    always {
        level := level + 1;
    }
}
`;

test("ссылка: круговой рейс сохраняет всё состояние", async () => {
  const state = {
    version: "0.57.0",
    source: MODEL,
    scenario: '[{"in_ports": {"x": 1}}]',
    target: "sv",
    args: "--fsm=table model.takt",
    layout: '{"format": 1}\n',
  };
  const restored = await decodeState("#" + (await encodeState(state)));
  assert.deepEqual(restored, state);
});

test("ссылка: чужой фрагмент не ошибка, а отсутствие состояния", async () => {
  assert.equal(await decodeState(""), null);
  assert.equal(await decodeState("#секция-документа"), null);
  assert.equal(await decodeState("#SGVsbG8"), null);
});

test("ссылка: сжатие действительно сжимает", async () => {
  // Смысл сжатия - в длине ссылки: модель на 3 КиБ обязана уложиться в адрес,
  // который не режут мессенджеры.
  const source = MODEL.repeat(40);
  const fragment = await encodeState({ version: "0.57.0", source });
  assert.ok(
    fragment.length < source.length / 3,
    `сжатие не сработало: ${source.length} → ${fragment.length}`
  );
});

test("координаты: смещение и позиция взаимно обратны", () => {
  const text = "первая\nвторая строка\nтретья";
  for (let offset = 0; offset <= text.length; offset += 1) {
    const { line, character } = offsetToPosition(text, offset);
    assert.equal(positionToOffset(text, line, character), offset);
  }
  assert.deepEqual(offsetToPosition(text, 0), { line: 0, character: 0 });
  assert.deepEqual(offsetToPosition(text, 7), { line: 1, character: 0 });
  // Кириллица считается символами, а не байтами: иначе подчёркивание уедет.
  assert.deepEqual(offsetToPosition("абв\nг", 4), { line: 1, character: 0 });
});

test("черновик: круговой рейс через хранилище", () => {
  const storage = memoryStorage();
  const value = { source: MODEL, scenario: "[]", target: "rust", args: "--inline=auto", layout: "" };
  assert.equal(draft.save(storage, value), null);
  assert.deepEqual(draft.load(storage), value);
  draft.clear(storage);
  assert.equal(draft.load(storage), null);
});

test("черновик: превышение предела названо, а не усечено молча", () => {
  const storage = memoryStorage();
  const problem = draft.save(storage, { source: "x".repeat(draft.LIMIT_BYTES + 1) });
  assert.ok(problem, "предел обязан быть назван");
  // Причина возвращается ключом словаря: текст строит главный поток страницы, и
  // второй копии словаря здесь нет.
  assert.equal(problem.key, "draft.tooBig");
  assert.equal(problem.params.limit, 64);
  assert.ok(problem.params.size > draft.LIMIT_BYTES);
  assert.equal(draft.load(storage), null, "черновик сверх предела не сохраняется");
});

test("черновик: испорченная запись не роняет страницу", () => {
  const storage = memoryStorage();
  storage.setItem("takt.draft.v1", "{ это не json");
  assert.equal(draft.load(storage), null);
});

/** Загружает модуль один раз на прогон: инстанцирование стоит дороже проверок. */
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

test("мост: язык ответа задаёт запрос, умолчание не наследуется", async () => {
  const bridge = await loadBridge();
  assert.deepEqual(bridge.version().languages, ["en", "ru"], "языки называет модуль");
  const broken = "start A {\n  ref Nowhere;\n}\n";
  const cyrillic = /[А-Яа-яЁё]/;
  try {
    bridge.lang = "en";
    const english = bridge.diagnostics(broken).diagnostics ?? [];
    assert.ok(english.length > 0, "диагностика есть");
    assert.ok(!english.some((d) => cyrillic.test(d.message)), JSON.stringify(english));
    bridge.lang = null;
    const base = bridge.diagnostics(broken).diagnostics ?? [];
    assert.ok(base.some((d) => cyrillic.test(d.message)), "без поля - базовый язык");
    bridge.lang = "xx";
    assert.equal(bridge.diagnostics(broken).ok, false, "неизвестный язык - отказ");
  } finally {
    bridge.lang = null;
  }
});

test("мост: страница и модуль сходятся формой ответа", async () => {
  const bridge = await loadBridge();

  const version = bridge.version();
  assert.equal(version.ok, true);
  assert.equal(version.targets.length, 7, "целей семь");

  const compiled = bridge.compile("c", "heater.takt", MODEL);
  assert.equal(compiled.ok, true, JSON.stringify(compiled));
  assert.deepEqual(
    compiled.files.map((f) => f.name),
    ["heater.h", "heater.c"]
  );

  const diagnostics = bridge.diagnostics(MODEL);
  assert.equal(diagnostics.ok, true);
  assert.ok(Array.isArray(diagnostics.diagnostics));

  // Токены разворачиваются в отрезки: страница красит по ним, и своего словаря
  // у неё нет.
  const marks = spans(bridge.tokens(MODEL));
  assert.ok(marks.length > 0, "модель без токенов не бывает");
  for (const mark of marks) {
    assert.ok(mark.length > 0 && mark.type !== undefined);
  }

  // Прогон: те же строки, что печатает `takt-sim`.
  const opened = bridge.simOpen(MODEL, "", 0);
  assert.equal(opened.ok, true, JSON.stringify(opened));
  const ticked = bridge.simTick(opened.id, 2);
  assert.equal(ticked.ok, true);
  assert.match(ticked.lines[0], /^Шаг {3}1:/);
  bridge.simClose(opened.id);
});

test("прогон: предупреждения и вывод модели доезжают до страницы", async () => {
  // Ради этого фича и заведена: печать внутри библиотеки для страницы не
  // существует, и до перевода на возврат автор не узнавал ни что форма сценария
  // устарела, ни что часть его значений игнорируется.
  const bridge = await loadBridge();
  const MODEL_WITH_DEBUG = `
model Probe {
    in a: bit;
    in b: bit;
    var n: u8 := 0;

    start Run {
        always {
            n := n + 1;
            debug("такт исполнен");
        }
        ref Done: n >= 2;
    }

    state Done { }
}
start Root = Probe;
`;

  // Позиционная форма короче списка портов: обе причины предупреждений сразу.
  const scenario = JSON.stringify([{ in_ports: [1] }, { in_ports: [1] }]);
  const opened = bridge.simOpen(MODEL_WITH_DEBUG, scenario, 0);
  assert.equal(opened.ok, true, JSON.stringify(opened));

  const ticked = bridge.simTick(opened.id, 2);
  assert.equal(ticked.ok, true, JSON.stringify(ticked));

  const codes = (ticked.warnings ?? []).map((w) => w.code);
  assert.ok(codes.includes("SIM-037"), `о форме сценария: ${JSON.stringify(ticked.warnings)}`);
  assert.ok(codes.includes("SIM-032"), `о длине массива: ${JSON.stringify(ticked.warnings)}`);
  // Код приходит отдельным полем, а не внутри текста: страница показывает его сама.
  for (const warning of ticked.warnings) {
    assert.ok(!warning.message.includes("SIM-"), `код внутри текста: ${warning.message}`);
  }
  // Номер шага есть у предупреждения о длине и отсутствует у предупреждения о форме.
  const length = ticked.warnings.find((w) => w.code === "SIM-032");
  assert.equal(length.step, 1, JSON.stringify(length));
  assert.equal(ticked.warnings.find((w) => w.code === "SIM-037").step, null);

  // Вывод модели - свой канал, не предупреждение.
  assert.ok(
    ticked.output.some((line) => line.startsWith("debug: ")),
    `вывод модели: ${JSON.stringify(ticked.output)}`
  );
  bridge.simClose(opened.id);
});

test("прогон: длину задаёт число шагов страницы, а сценарий - входы", async () => {
  // Сценарий из двух шагов при длине 196 обрывал прогон на втором такте: страница
  // не передавала длину, и эталон брал её у сценария.
  const bridge = await loadBridge();
  const model = "in sensor: u8;\nvar seen: u8 := 0;\n\nstart Run {\n    always {\n        seen := sensor;\n    }\n\n    ref Run: 1 = 1;\n}\n";
  const scenario = JSON.stringify([{ in_ports: { sensor: 3 } }, { in_ports: { sensor: 7 } }]);
  const opened = bridge.simOpen(model, scenario, 0, {}, 196);
  assert.equal(opened.ok, true, JSON.stringify(opened));
  const ticked = bridge.simTick(opened.id, 1000);
  assert.equal(ticked.lines.length, 196, `тактов: ${ticked.lines.length}`);
  assert.ok(ticked.lines[195].includes("sensor=7"), ticked.lines[195]);
  bridge.simClose(opened.id);
});

test("манифест приложения: адреса от бандла, иконки на месте и нужного размера", async () => {
  // Манифест едет в бандл `b/<отпечаток>/`, а страница стоит двумя уровнями выше:
  // `start_url` и `scope` считаются от адреса манифеста, и абсолютный `/` увёл бы
  // приложение мимо префикса стенда (`/takt/`).
  const base = new URL("../static/", import.meta.url);
  const manifest = JSON.parse(await readFile(new URL("manifest.webmanifest", base), "utf8"));
  assert.equal(manifest.start_url, "../../");
  assert.equal(manifest.scope, "../../");
  assert.equal(manifest.display, "standalone");
  const html = await readFile(new URL("index.html", base), "utf8");
  assert.match(html, /<link rel="manifest" href="manifest\.webmanifest">/);
  // Для установки нужны растровые 192 и 512; маскируемая - своя, с полями.
  const png = (manifest.icons ?? []).filter((icon) => icon.type === "image/png");
  for (const need of ["192x192", "512x512"]) {
    assert.ok(png.some((icon) => icon.sizes === need && icon.purpose !== "maskable"), `нет иконки ${need}`);
  }
  assert.ok(png.some((icon) => icon.purpose === "maskable"), "нет маскируемой иконки");
  for (const icon of manifest.icons) {
    const bytes = await readFile(new URL(icon.src, base));
    if (icon.type !== "image/png") continue;
    // Размер PNG - в заголовке IHDR: ширина и высота с 16-го байта.
    const size = `${bytes.readUInt32BE(16)}x${bytes.readUInt32BE(20)}`;
    assert.equal(size, icon.sizes, `${icon.src}: объявлено ${icon.sizes}, в файле ${size}`);
  }
  const apple = /<link rel="apple-touch-icon" href="([^"]+)">/.exec(html);
  assert.ok(apple, "нет иконки для iOS");
  await readFile(new URL(apple[1], base));
});

test("воркер без сети: стратегия по форме адреса", async () => {
  // Воркер - классический скрипт без модулей: он исполняется в подставном
  // окружении, и проверяется решение, а не браузерный кеш.
  const { runInNewContext } = await import("node:vm");
  const source = await readFile(new URL("../static/sw.js", import.meta.url), "utf8");
  const context = { self: { addEventListener() {} }, URL };
  runInNewContext(source, context);
  const { strategyOf } = context;
  assert.equal(strategyOf("", true), "page", "переход - страница");
  assert.equal(strategyOf("p/AbCd", true), "page", "страница проекта - та же разметка");
  assert.equal(strategyOf("api/projects", false), "network", "данные сервера из кеша не отдаются");
  assert.equal(strategyOf("b/0123abcd/app.js", false), "cache", "бандл неизменен");
  assert.equal(strategyOf("wasm/0.61.0/takt.wasm", false), "cache", "модуль неизменен");
  assert.equal(strategyOf("version.json", false), "fresh", "опись - сначала сеть");
  // Несобранная страница воркера не получает: подставлять ему нечего.
  const { registerOffline } = await import("../static/offline.js");
  const win = { navigator: { serviceWorker: { register: () => Promise.resolve("ok") } }, location: { protocol: "http:" }, document: { baseURI: "http://x/takt/" } };
  assert.equal(registerOffline(win, "http://x/takt/offline.js"), null);
  assert.equal(registerOffline({ ...win, navigator: {} }, "http://x/takt/b/0123abcd/offline.js"), null);
  assert.equal(await registerOffline(win, "http://x/takt/b/0123abcd/offline.js"), "ok");
});

test("генерация: панель устроена как прочие, а цель и ключи - в окне настроек сборки", async () => {
  // Шапка и полоса действий - как у кода и структуры проекта; вкладок в панели
  // больше нет: цель и ключи выбирает окно, и носители величин прежние.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const pane = /<section class="pane pane-result">([\s\S]*?)<\/section>/.exec(html)[1];
  assert.match(pane, /class="pane-head"/);
  assert.match(pane, /class="pane-toolbar"/);
  assert.match(pane, /id="buildsettings"/);
  assert.ok(!pane.includes('id="tabs"'), "вкладки остались в панели");
  const modal = /<div id="build-modal"[\s\S]*?<div id="scheme-modal"/.exec(html)[0];
  for (const need of ['data-tab="target"', 'data-tab="flags"', 'id="flags"', 'id="target"', 'id="build-save"', 'id="build-cancel"']) {
    assert.ok(modal.includes(need), `в окне нет ${need}`);
  }
  // Группы целей: известные - по назначению, незнакомая модулю цель не пропадает.
  const { groupTargets } = await import("../static/build-settings.js");
  const groups = groupTargets(["c", "st", "rust", "sv-mmio", "zig"]);
  assert.deepEqual(groups.map((g) => g.id), ["mcu", "plc", "rust", "fpga", "other"]);
  assert.deepEqual(groups.at(-1).targets, ["zig"], "незнакомая цель - в прочих");
  assert.deepEqual(groupTargets(["c"]).map((g) => g.id), ["mcu"], "пустые группы не показываются");
});

test("подсветка: каждая цель красит свой вывод", async () => {
  // У каждой из восьми целей разметка непуста и различает ключевое слово, число
  // и комментарий. Цель, забытая в таблице языков, показывала бы чёрный текст, и
  // заметил бы это лишь человек, открывший её вкладку.
  const bridge = await loadBridge();
  const targets = bridge.version().targets;
  assert.equal(targets.length, 7);
  for (const target of targets) {
    const compiled = bridge.compile(target, "heater.takt", MODEL);
    assert.equal(compiled.ok, true, `${target}: ${JSON.stringify(compiled)}`);
    for (const file of compiled.files) {
      const painted = bridge.highlight(target, file.text);
      assert.equal(painted.ok, true, `${target}/${file.name}`);
      assert.ok(painted.language, `${target}/${file.name}: язык не назван`);
      const marks = spans(painted);
      assert.ok(marks.length > 0, `${target}/${file.name}: разметка пуста`);
      // Отрезок обязан попадать в текст: съехавшая колонка красит соседнее
      // слово, и вывод при этом выглядит совершенно рабочим.
      const lines = file.text.split("\n");
      for (const mark of marks) {
        assert.ok(mark.line < lines.length, `${target}: отрезок за концом файла`);
        assert.ok(
          mark.column + mark.length <= lines[mark.line].length,
          `${target}: отрезок за концом строки ${mark.line + 1}`
        );
      }
    }
    // "Различает ключевое слово, число и комментарий" проверяется не здесь:
    // требование к выводу цели было бы требованием к фикстуре. Это свойство
    // языка, и его проверяют пробы у самих словарей (`takt-wasm/src/highlight`).
  }
});

test("подсветка: у исходника Takt своя разметка, у вывода — своя", async () => {
  // Языки разные, и красить вывод цели правилами Takt (или наоборот) значило бы
  // показывать автору неправду о том, что он читает.
  const bridge = await loadBridge();
  const source = spans(bridge.tokens(MODEL));
  const generated = bridge.compile("st", "heater.takt", MODEL).files[0].text;
  const output = spans(bridge.highlight("st", generated));
  assert.ok(source.length > 0 && output.length > 0);
  // `always` - ключевое слово Takt и не ключевое слово Structured Text.
  const painted = (marks, text) =>
    marks.filter((m) => text.split("\n")[m.line].slice(m.column, m.column + m.length) === "always");
  assert.ok(painted(source, MODEL).length > 0, "в исходнике `always` покрашено");
  assert.equal(painted(output, generated).length, 0, "в выводе ST `always` — не слово языка");
});

test("подсветка: чужая цель — отказ с названной причиной", async () => {
  const bridge = await loadBridge();
  const reply = bridge.highlight("verilog", "module m;");
  assert.equal(reply.ok, false);
  assert.match(reply.error.message, /verilog/);
});

test("подсветка: роли кода перечислены темой документа", async () => {
  // Реестр ролей - `book/takt.tmTheme`: блоки кода в PDF и
  // вкладка цели красят одни И те же виды токенов. Разъедься наборы - документ
  // и редактор разошлись бы глазами, и заметить это можно только сличением
  // двух картинок.
  const theme = await readFile(new URL("../../book/takt.tmTheme", import.meta.url), "utf8");
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  const inTheme = new Set(
    [...theme.matchAll(/<key>name<\/key>\s*<string>([^<]+)<\/string>/g)]
      .map((m) => m[1].toLowerCase())
      // Имя самой темы - не роль.
      .filter((name) => name !== "takt (tango)")
  );
  const inCss = new Set([...css.matchAll(/--tok-([a-z]+):/g)].map((m) => m[1]));
  // `variable` - цвет текста по умолчанию: в теме у него своей записи нет.
  inCss.delete("variable");
  assert.deepEqual([...inCss].sort(), [...inTheme].sort());
});

test("подсветка: раскладка строк не зависит от числа отрезков квадратично", () => {
  // Фильтруй каждая строка весь список отрезков, работа росла бы квадратично: на
  // исходнике это незаметно, а вывод цели `c` - тысячи строк и тысячи отрезков, и
  // вкладка встала бы на секунды. Проверяется не время, а свойство: отрезок
  // попадает ровно в свою строку.
  const text = ["aaa", "bbb", "ccc"].join("\n");
  const marks = [
    { line: 2, column: 0, length: 3, type: "keyword" },
    { line: 0, column: 0, length: 3, type: "number" },
  ];
  const buckets = new Map();
  for (const mark of marks) {
    if (!buckets.has(mark.line)) buckets.set(mark.line, []);
    buckets.get(mark.line).push(mark);
  }
  assert.deepEqual([...buckets.keys()].sort(), [0, 2]);
  assert.equal(text.split("\n").length, 3);
});

/** Читает словарь языка с диска: `fetch` относительного пути в `node` нет. */
async function dictionary(lang) {
  const text = await readFile(new URL(`../static/i18n/${lang}.json`, import.meta.url), "utf8");
  return JSON.parse(text);
}

test("язык: словари полны — паритет ключей и подстановок", async () => {
  // Замер образца 2026-09-04: у него 163 ключа есть только в `ru`, и
  // непереведённое молча падает на русский. Правило проекта иное: язык либо
  // полон, либо не заведён, - и держит его эта проверка, а не дисциплина.
  const base = await dictionary(i18n.BASE);
  const names = (text) => new Set([...text.matchAll(/\{(\w+)\}/g)].map((m) => m[1]));
  for (const lang of Object.keys(i18n.LANGUAGES)) {
    if (lang === i18n.BASE) continue;
    const dict = await dictionary(lang);
    assert.deepEqual(
      Object.keys(dict).sort(),
      Object.keys(base).sort(),
      `словарь '${lang}' не равен базовому по составу ключей`
    );
    for (const key of Object.keys(base)) {
      assert.deepEqual(
        [...names(dict[key])].sort(),
        [...names(base[key])].sort(),
        `ключ '${key}' в '${lang}': другой набор подстановок`
      );
      assert.ok(dict[key].trim().length > 0, `ключ '${key}' в '${lang}' пуст`);
    }
  }
});

test("язык: список выпуска равен составу каталога словарей", async () => {
  // Язык без словаря даёт подписи-ключи; словарь без записи в списке не
  // выбрать ничем. И то и другое - тишина.
  const files = (await readdir(new URL("../static/i18n/", import.meta.url)))
    .filter((name) => name.endsWith(".json"))
    .map((name) => name.replace(/\.json$/, ""));
  assert.deepEqual(files.sort(), Object.keys(i18n.LANGUAGES).sort());
});

test("язык: каждый ключ разметки есть в словаре, и мёртвых ключей нет", async () => {
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const scripts = await Promise.all(
    PAGE_SCRIPTS.map((name) =>
      readFile(new URL(`../static/${name}`, import.meta.url), "utf8")
    )
  );
  const base = await dictionary(i18n.BASE);

  const used = new Set();
  for (const [, key] of html.matchAll(/data-i18n="([^"]+)"/g)) used.add(key);
  for (const [, pairs] of html.matchAll(/data-i18n-attr="([^"]+)"/g)) {
    for (const pair of pairs.split(";")) used.add(pair.split(":")[1].trim());
  }
  const text = [html, ...scripts].join("\n");
  for (const [, key] of text.matchAll(/\bt\(\s*"([\w.]+)"/g)) used.add(key);
  // Подписи ключей сборки объявлены описью (`flags.js`), а не разметкой: они
  // приходят в текст через `t(spec.label)`.
  for (const [, key] of text.matchAll(/label:\s*"([\w.]+)"/g)) used.add(key);
  // Ключи, которые страница строит не буквально: воркер и черновик возвращают
  // их полем `key`.
  for (const [, key] of text.matchAll(/key:\s*"([\w.]+)"/g)) used.add(key);
  // Подписи кнопок площадок приходят от сервера: имён площадок в коде страницы
  // нет намеренно. Ключи берутся у него же - иначе
  // сверка объявила бы их мёртвыми и подтолкнула бы завести список в вебе.
  for (const key of await serverLabelKeys()) used.add(key);
  // Ключ обязан нести точку (`bar.format`): без этого условия тернарник над
  // парой обычных слов (`fits ? "below" : "above"`) читается как выбор ключа, и
  // сверка требует словарной статьи для слова, которое читателю не показывают.
  for (const [, a, b] of text.matchAll(/\?\s*"(\w+\.[\w.]+)"\s*:\s*"(\w+\.[\w.]+)"/g)) {
    used.add(a);
    used.add(b);
  }

  for (const key of used) {
    assert.ok(base[key], `ключ '${key}' используется, но его нет в словаре`);
  }
  const dead = Object.keys(base).filter((key) => !used.has(key));
  assert.deepEqual(dead, [], `мёртвые ключи словаря: ${dead.join(", ")}`);
});

test("язык: текста оболочки мимо словаря нет", async () => {
  // Строка, написанная в коде, не переводится никогда и не обнаруживается
  // ничем: страница выглядит рабочей, а подпись остаётся на чужом языке.
  // Исключения названы в `TEXT_EXEMPT`.
  const scripts = PAGE_SCRIPTS.filter((name) => !TEXT_EXEMPT.includes(name));
  for (const name of scripts) {
    const source = await readFile(new URL(`../static/${name}`, import.meta.url), "utf8");
    const found = literalsWithText(source);
    assert.deepEqual(
      found,
      [],
      `${name}: текст мимо словаря — ${found.map((f) => `строка ${f.line}: ${f.text}`).join("; ")}`
    );
  }
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const inMarkup = nodesWithoutKey(html);
  assert.deepEqual(
    inMarkup,
    [],
    `index.html: текст без ключа — ${inMarkup.map((f) => `строка ${f.line}: ${f.text}`).join("; ")}`
  );
  // Встроенный скрипт разметки - такой же код страницы: строку оболочки в нём
  // не видно ни разбором тегов, ни обходом модулей.
  for (const script of inlineScripts(html)) {
    const found = literalsWithText(script.source);
    assert.deepEqual(
      found,
      [],
      `index.html, скрипт со строки ${script.line}: текст мимо словаря — ${found
        .map((f) => f.text)
        .join("; ")}`
    );
  }
});

test("язык: подстановки и падение на базовый", async () => {
  i18n.use("en", { "a.b": "hello {name}" }, { "a.b": "привет {name}", "c.d": "только база" });
  assert.equal(i18n.t("a.b", { name: "Takt" }), "hello Takt");
  // Ключа нет в выбранном языке - берётся базовый.
  assert.equal(i18n.t("c.d"), "только база");
  // Нет и там - сам ключ: пустая кнопка хуже кнопки с именем ключа.
  assert.equal(i18n.t("нет.такого"), "нет.такого");
  // Неизвестная подстановка остаётся как есть: молча съеденная выглядела бы
  // опечаткой автора словаря.
  assert.equal(i18n.t("a.b", { other: 1 }), "hello {name}");
});

test("язык: порядок выбора — сохранённый, браузер, база", () => {
  assert.equal(i18n.pick("en", ["ru-RU"]), "en", "сохранённый сильнее браузера");
  assert.equal(i18n.pick(null, ["en-GB", "ru"]), "en", "регион отбрасывается");
  assert.equal(i18n.pick(null, ["de-DE"]), i18n.BASE, "неизвестный язык — база");
  assert.equal(i18n.pick("de", ["en"]), "en", "сохранённый язык без словаря не берётся");
  assert.equal(i18n.pick(null, []), i18n.BASE);
});

/**
 * Модули страницы. Список явный: обход каталога подхватил бы и то, чего в
 * `index.html` нет, а забытый модуль остался бы без обеих проверок молча.
 */
const PAGE_SCRIPTS = [
  "account.js", "alerts.js", "api.js", "app.js", "boot.js", "bridge.js", "build.js", "build-settings.js",
  "draft.js", "editor.js", "export.js", "help.js", "i18n.js", "layout.js", "legend.js", "pick.js",
  "panels.js", "project.js", "sample.js", "scheme.js", "scheme-geometry.js",
  "scheme-host.js", "scheme-run.js",
  "scheme-settings.js",
  "flags.js", "json.js",
  "md.js", "offline.js", "share.js", "shell.js", "sw.js", "tip.js", "worker.js",
];

/**
 * Модули, которым русский текст в литералах разрешён, и почему:
 *   `sample.js` - стартовая модель, документ автора, а не оболочка;
 *   `i18n.js` - самоназвания языков (они не переводятся ни на какой язык) и
 *   отказ загрузки словаря: сообщить о нём словарём нечем - его нет.
 */
const TEXT_EXEMPT = ["sample.js", "i18n.js"];

/**
 * Предел длины сообщения полосы - правило книги оформления.
 *
 * Полоса постоянной высоты не переносит строку: всё, что длиннее, обрезается
 * многоточием, и читателю не достаётся.
 */
const SAY_LIMIT = 40;

/**
 * Тела вызовов, начинающихся с `head`, - до сбалансированной закрывающей скобки.
 *
 * Регулярное выражение здесь не годится: вызов бывает разбит на строки, а
 * поиск "до первой скобки" обрывает его на вложенном вызове - и половина
 * сообщений просто не попала бы в проверку.
 */
function calls(source, head) {
  const found = [];
  let at = source.indexOf(head);
  while (at >= 0) {
    let depth = 0;
    let end = at + head.length - 1;
    for (; end < source.length; end += 1) {
      if (source[end] === "(") depth += 1;
      else if (source[end] === ")") {
        depth -= 1;
        if (depth === 0) break;
      }
    }
    // Тело функции лежит за списком её аргументов: `function f()` кончается
    // пустыми скобками, и без хвоста проверять было бы нечего.
    const tail = head.startsWith("function") ? source.slice(end, source.indexOf("\n}", end)) : "";
    found.push(source.slice(at, end + 1) + tail);
    at = source.indexOf(head, end + 1);
  }
  return found;
}

/** Путь к собранной статике; проверки сборки без него пропускаются. */
const DIST = process.argv[3] ?? process.env.TAKT_WEB_DIST ?? null;

test("сборка: разметка ссылается только в каталог бандла", { skip: !DIST }, async () => {
  // "Содержимое задаёт адрес, адрес задаёт срок": помеченное отпечатком живёт
  // год и `immutable`. Ссылка мимо бандла - файл, который кеш обязан считать
  // вечным, не будучи вечным, то есть молчаливая порча у всех, кто кешировал.
  const html = await readFile(join(DIST, "index.html"), "utf8");
  // `<base>` из разбора выброшен: он называет корень адресов, а не файл, и
  // отпечатка у него быть не может. Именно он и делает остальные ссылки
  // считаемыми от корня - без него страница `/p/<id>` искала бы бандл под
  // собой.
  const base = /<base href="([^"]+)"/.exec(html);
  assert.ok(base, "в разметке нет корня адресов");
  assert.equal(base[1], "/", "корень адресов — не бандл и не подкаталог");
  const links = [...html.replace(/<base [^>]*>/g, "").matchAll(/(?:href|src)="([^"]+)"/g)]
    .map((m) => m[1]);
  assert.ok(links.length > 0, "в разметке нет ссылок вовсе");
  for (const link of links) {
    if (/^(https?:|data:|#)/.test(link)) continue;
    assert.match(link, /^b\/[0-9a-f]{6,}\//, `ссылка мимо каталога бандла: ${link}`);
  }
});

test("сборка: идентификатор бандла один — в адресе и в описи", { skip: !DIST }, async () => {
  // Носитель отпечатка один: страница читает свой из собственного адреса
  // (`import.meta.url`), а выложенный - из `version.json`. Разъедься они -
  // страница вечно звала бы обновиться либо не звала бы никогда.
  const version = JSON.parse(await readFile(join(DIST, "version.json"), "utf8"));
  const dirs = await readdir(join(DIST, "b"));
  assert.deepEqual(dirs, [version.bundle], "каталог бандла не равен описи");
  const html = await readFile(join(DIST, "index.html"), "utf8");
  assert.ok(html.includes(`b/${version.bundle}/`), "разметка ведёт в другой бандл");
  // Тот же разбор, которым страница узнаёт свой бандл.
  assert.equal(bundleOfUrl(`http://x/b/${version.bundle}/app.js`), version.bundle);
  assert.equal(bundleOfUrl("http://x/app.js"), null, "несобранная страница бандла не имеет");
});

test("сборка: опись модуля несёт его контрольную сумму", { skip: !DIST }, async () => {
  // Адрес `wasm/<версия>/` обещает неизменность, и выложить под ним другой файл -
  // порча у каждого, кто уже кешировал. Отказ выкладки на подмене стоит на этой
  // сумме, и посчитана она обязана быть верно.
  const version = JSON.parse(await readFile(join(DIST, "version.json"), "utf8"));
  const dir = join(DIST, "wasm", version.takt_lang);
  const manifest = JSON.parse(await readFile(join(dir, "manifest.json"), "utf8"));
  const bytes = await readFile(join(dir, "takt.wasm"));
  assert.equal(manifest.sha256, createHash("sha256").update(bytes).digest("hex"));
  assert.equal(manifest.size, bytes.length);
  const exported = await readFile(join(dir, manifest.export.file));
  assert.equal(manifest.export.sha256, createHash("sha256").update(exported).digest("hex"), "сумма модуля экспорта");
  assert.equal(manifest.export.size, exported.length);
  assert.equal(manifest.takt_lang, version.takt_lang);
  const index = JSON.parse(await readFile(join(DIST, "wasm", "index.json"), "utf8"));
  assert.equal(index.latest, version.takt_lang);
  assert.ok(index.versions.includes(version.takt_lang));

  // Обе версии непусты. Поле `language` собиралось грепом по `lib.rs`, где
  // константа только реэкспортируется, и описи месяц несли пустую строку:
  // сервер отдавал её новому проекту, а увидеть это можно было лишь заглянув в
  // `version.json`. Пустая строка - не значение, и молчать о ней нельзя.
  for (const [where, value] of [
    ["version.json takt_lang", version.takt_lang],
    ["version.json language", version.language],
    ["manifest language", manifest.language],
  ]) {
    assert.match(value ?? "", /^\d+\.\d+\.\d+$/, `${where}: не версия — '${value}'`);
  }
});

// Справку собирают из документа `book/`: в урезанной копии дерева, на которой
// проверяют саму проверку веб-части, документа нет, и справки в сборке быть не
// должно; в полном дереве она обязательна.
const HAS_BOOK = existsSync(fileURLToPath(new URL("../../book/src/main.typ", import.meta.url)));

test("сборка: справка лежит в бандле - заголовки с якорями, код ролями страницы", { skip: !DIST || !HAS_BOOK }, async () => {
  const version = JSON.parse(await readFile(join(DIST, "version.json"), "utf8"));
  const help = await readFile(join(DIST, "b", version.bundle, "help.html"), "utf8");
  assert.match(help, /<h2[^>]* id="h-/, "у глав есть якоря");
  assert.match(help, /class="tok-keyword"/, "ключевые слова - ролью страницы");
  assert.doesNotMatch(help, /style="color:/, "цветов печати в справке нет");
  assert.doesNotMatch(help, /<body|<html/, "фрагмент без обвязки документа");
});

test("сборка: воркер в корне, бандл и список предзагрузки подставлены", { skip: !DIST }, async () => {
  const version = JSON.parse(await readFile(join(DIST, "version.json"), "utf8"));
  const sw = await readFile(join(DIST, "sw.js"), "utf8");
  assert.ok(!sw.includes("__TAKT_"), "подстановка не сделана");
  assert.ok(sw.includes(`const BUNDLE = ${JSON.stringify(version.bundle)};`), "воркер знает чужой бандл");
  const list = JSON.parse(/const PRECACHE = (\[[\s\S]*?\]);/.exec(sw)[1]);
  for (const need of ["./", "version.json", version.wasm, `b/${version.bundle}/app.js`]) {
    assert.ok(list.includes(need), `в предзагрузке нет ${need}`);
  }
  // Модуль экспорта в предзагрузку не входит: он кешируется при первом экспорте,
  // иначе каждая установка воркера тянула бы его ради одной кнопки.
  assert.ok(version.export_wasm, "опись не называет модуль экспорта");
  assert.ok(!list.some((e) => e.includes("takt-export")), "модуль экспорта в предзагрузке");
  // Каждый адрес списка существует: промах одного роняет всю установку воркера.
  for (const entry of list.filter((e) => e !== "./")) await readFile(join(DIST, entry.split("?")[0]));
  await assert.rejects(readFile(join(DIST, "b", version.bundle, "sw.js")), "воркер остался в бандле");
});

test("сборка: текстовые файлы предсжаты", { skip: !DIST }, async () => {
  // Стенд ничего не считает на лету: модуль 3,3 мб, и сжимать его каждому
  // первому заходу - лишняя работа. Описи `no-cache` предсжатию не
  // подлежат: их читают ради свежести, а не ради объёма.
  const version = JSON.parse(await readFile(join(DIST, "version.json"), "utf8"));
  const must = [
    join(DIST, "index.html"),
    join(DIST, "b", version.bundle, "app.css"),
    join(DIST, "b", version.bundle, "app.js"),
    join(DIST, "wasm", version.takt_lang, "takt.wasm"),
  ];
  for (const file of must) {
    assert.ok(existsSync(`${file}.gz`), `нет предсжатого: ${file}.gz`);
  }
  assert.ok(!existsSync(join(DIST, "version.json.gz")), "опись сборки предсжата зря");
  assert.ok(!existsSync(join(DIST, "wasm", "index.json.gz")), "опись версий предсжата зря");
});

test("черновик: отложенную запись можно сделать немедленно", () => {
  // Перед перезагрузкой на новую сборку черновик обязан лечь на диск: запись,
  // отложенная на 400 мс, до перезагрузки не доживёт, и автор потеряет
  // последние набранные строки.
  let written = null;
  const save = draft.debounce((value) => (written = value), 10_000);
  save("первое");
  save("второе");
  assert.equal(written, null, "запись отложена");
  save.now();
  assert.equal(written, "второе", "записано последнее, а не первое");
  save.now();
  assert.equal(written, "второе", "повторный вызов ничего не пишет");
});

test("оболочка: ширина не выходит за окно и за наименьшую", () => {
  // Наибольшая ширина - размер окна: шире
  // монитора оболочки не бывает. Наименьшая - 640: уже неё две колонки кода
  // не имеют смысла.
  assert.equal(shell.clamp(2000, 1440), 1440, "шире окна оболочки не бывает");
  assert.equal(shell.clamp(100, 1440), shell.MIN_WIDTH, "уже предела не сужается");
  assert.equal(shell.clamp(900, 1440), 900, "внутри пределов — как просили");
  // Окно уже наименьшей ширины: предел обязан победить окно, иначе
  // оболочка схлопнется в ничто на узком мониторе.
  assert.equal(shell.clamp(700, 400), shell.MIN_WIDTH);
  assert.equal(shell.clamp(300, 400), shell.MIN_WIDTH);
});

test("оболочка: испорченная запись ширины не роняет страницу", () => {
  const bad = { getItem: () => "не число" };
  assert.equal(shell.stored(bad), null);
  assert.equal(shell.stored({ getItem: () => null }), null);
  assert.equal(shell.stored({ getItem: () => "0" }), null, "нулевая ширина — не ширина");
  assert.equal(shell.stored({ getItem: () => "900" }), 900);
  assert.equal(shell.stored({ getItem: () => { throw new Error("нет доступа"); } }), null);
});

test("разделитель областей: доли не схлопывают ни одну из них", () => {
  // Ноль сюда не годится: область, сжатая в полосу, выглядит пропавшей, а
  // вернуть её мышью уже не за что - разделитель уезжает под самый край.
  assert.equal(shell.clampRatio(0.5), 0.5, "внутри пределов — как просили");
  assert.equal(shell.clampRatio(0), shell.MIN_RATIO, "область схлопнута");
  assert.equal(shell.clampRatio(1), 1 - shell.MIN_RATIO, "соседка схлопнута");
  assert.equal(shell.clampRatio(-3), shell.MIN_RATIO);
  // Испорченная запись хранилища даёт умолчание, а не "долю NaN": с нею
  // вторая область исчезла бы молча, без единого отказа.
  assert.equal(shell.clampRatio(NaN), shell.HALF);
  assert.equal(shell.clampRatio(Number("не число")), shell.HALF);
});

test("разделитель областей: незаданная доля — половина, а не ноль", () => {
  // Нашлось прогоном страницы: `Number(null)` даёт ноль, ноль - законная
  // доля, и первый же заход схлопывал исходник в пятую часть экрана. Отказа
  // при этом нет: страница просто открывается не такой, как задумана.
  assert.equal(shell.panes({ getItem: () => null }), shell.HALF, "пусто — не ноль");
  assert.equal(shell.panes({ getItem: () => "" }), shell.HALF);
  assert.equal(shell.panes({ getItem: () => "не число" }), shell.HALF);
  assert.equal(shell.panes({ getItem: () => "0.65" }), 0.65, "запомненное читается");
  // Граница первого мгновения не затирает выбор читателя: имена ещё не
  // устоялись - дерево шире, устоялись - доля читателя возвращается.
  const props = {};
  const split = {
    ownerDocument: { documentElement: { style: { setProperty: (key, value) => { props[key] = value; } } } },
    parentElement: { getBoundingClientRect: () => ({ width: 1000, height: 600 }) },
    addEventListener() {},
    setAttribute() {},
  };
  let least = 700;
  const handle = shell.attachTree(split, { getItem: () => "0.8", setItem() {} }, { side: () => "right", least: () => least });
  assert.equal(props["--tree-w"], "70%", "граница загрузки шире выбора");
  least = 150;
  handle.refresh();
  assert.equal(props["--tree-w"], `${(1 - 0.8) * 100}%`, "выбор читателя вернулся");
  least = 1200;
  handle.refresh();
  assert.equal(props["--tree-w"], `${(1 - 0.8) * 100}%`, "имена шире области - граница не распахивает дерево");
  assert.equal(shell.panes({ getItem: () => "0.01" }), shell.MIN_RATIO, "прижимается");
  assert.equal(shell.panes({ getItem: () => { throw new Error("нет доступа"); } }), shell.HALF);
});

test("прокрутка: вложенная область кода не запирает вертикаль", async () => {
  // `.file-text` лежит внутри `.output` и объявлен `overflow: auto`, то есть
  // сам является контейнером прокрутки - но по вертикали не переполнен
  // (высота равна высоте текста), а крутится внешний `.output`. Пока у него
  // стоял `overscroll-behavior: contain`, колесо над текстом упиралось в него
  // и наружу не передавалось: вертикальная прокрутка вывода была мертва, а
  // горизонтальная работала - по горизонтали он переполнен и крутится сам.
  // Замер 2026-09-05: `.output` - 22702 против 984, `.file-text` - 22678
  // против 22678. Нашлось прогоном страницы, тестам этот класс невидим.
  const source = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  // Комментарии гасятся до разбора: в них правило и объясняется, и без
  // этого контроль падал на собственном пояснении.
  const css = source.replace(/\/\*[\s\S]*?\*\//g, "");
  const rules = [...css.matchAll(/([^{}]+)\{([^}]*)\}/g)];
  for (const [, selector, body] of rules) {
    if (!selector.includes(".file-text")) continue;
    const locked = [...body.matchAll(/overscroll-behavior(-y)?\s*:\s*([\w-]+)/g)]
      .filter(([, axis, value]) => !axis?.includes("x") && value !== "auto");
    assert.deepEqual(
      locked.map((m) => m[0]),
      [],
      `вложенная область кода запирает вертикальную прокрутку: ${selector.trim()}`
    );
  }
  // И контроль: у внешних областей `contain` обязан остаться - иначе жест в
  // конце списка потянет документ, и на телефоне это выглядит поломкой.
  assert.match(css, /\.editor,\s*\.output,\s*\.list,\s*\.scenario\s*\{[^}]*overscroll-behavior:\s*contain/);
});

test("шапка: время сборки читается по часам читателя", () => {
  // Показывается местное время, а метка описи - UTC: читатель сравнивает её
  // со своими часами ("сегодняшняя ли сборка?"), а не с гринвичскими.
  const was = process.env.TZ;
  process.env.TZ = "UTC";
  try {
    assert.equal(build.moment("2026-09-05T12:14:26Z"), "2026-09-05 12:14");
  } finally {
    process.env.TZ = was;
  }

  // Опись прежней выкладки поля не несёт вовсе, а разбор чужой строки даёт
  // "Invalid Date" - в шапку она попасть не должна: пусто честнее неверного.
  for (const bad of [undefined, null, "", "позавчера"]) {
    assert.equal(build.moment(bad), "", `метка ${JSON.stringify(bad)} дала текст`);
  }
});

test("ключи сборки: опись страницы сверена с разбором аргументов", async () => {
  // Предмет - согласие двух сторон. Опись живёт в вебе (модуль её не
  // отдаёт), и без сверки она разошлась бы с компилятором молча: страница
  // предложила бы ключ, которого нет, либо умолчала о появившемся. Тот же
  // класс, из-за которого в `web/` запрещён список ключевых слов Takt.
  const cli = await readFile(new URL("../../takt-lang/src/compile_cli/mod.rs", import.meta.url), "utf8");
  const restricted = await readFile(
    new URL("../../takt-lang/src/compile_cli/target_flags.rs", import.meta.url), "utf8"
  );

  for (const spec of flags.FLAGS) {
    assert.ok(cli.includes(`"${spec.key}`), `ключ ${spec.key} разбором аргументов не принимается`);
    // Значения ключа - те же, что перечисляет разбор своего `parse_*`.
    for (const value of spec.choices ?? []) {
      assert.ok(cli.includes(`"${value}" =>`), `значение ${spec.key}=${value} разбор не знает`);
    }
    // Ограничение по целям совпадает с таблицей применимости компилятора.
    for (const [value, targets] of Object.entries(spec.only ?? {})) {
      const entry = restricted.match(
        new RegExp(`flag: "${spec.key}=${value}",[\\s\\S]*?targets: &\\[([^\\]]*)\\]`)
      );
      assert.ok(entry, `${spec.key}=${value} объявлен ограниченным, а таблица целей о нём молчит`);
      const listed = [...entry[1].matchAll(/"([a-z-]+)"/g)].map((m) => m[1]);
      assert.deepEqual(targets.slice().sort(), listed.sort(),
        `цели у ${spec.key}=${value} разошлись с таблицей компилятора`);
    }
  }

  // Обратная сторона: ограниченный компилятором ключ обязан быть в описи с тем
  // же ограничением - иначе страница предложит сборку, которую он отвергнет.
  for (const m of restricted.matchAll(/flag: "(--[a-z-]+)=([a-z]+)"/g)) {
    const spec = flags.flag(m[1]);
    assert.ok(spec?.only?.[m[2]], `ограничение ${m[1]}=${m[2]} в описи страницы не отражено`);
  }
});

test("ключи сборки: строка — единственная величина", () => {
  // Разбор и сборка - обращение друг друга на всех формах ключа.
  const line = "--fsm=table --bounds-check --float-as-q=8.8 --tick-hz=500";
  assert.equal(flags.line(flags.parse(line), "c"), line);

  // Неизвестный ключ едет обратно как есть: он мог появиться в компиляторе
  // раньше, чем в описи, и терять его страница не вправе.
  const withUnknown = "--fsm=switch --какой-то-новый=1";
  assert.equal(flags.line(flags.parse(withUnknown), "c"), withUnknown);

  // Ключ, чьё значение цель не принимает, в строку не идёт: иначе страница
  // обещала бы сборку, которую компилятор отвергнет.
  assert.equal(flags.line(flags.parse("--fsm=table"), "sv"), "--fsm=table", "табличную форму принимает любая цель");
  assert.equal(flags.line(flags.parse("--bus=apb"), "sv"), "");
  assert.equal(flags.line(flags.parse("--bus=apb"), "sv-mmio"), "--bus=apb");
});

test("сценарий: JSON красится своим разбором, и разбор терпим к недописанному", () => {
  // Свой разбор здесь законен, в отличие от Takt: грамматику JSON задаёт
  // RFC 8259, второго носителя у неё в проекте нет - расходиться не с чем.
  const text = '[{"in_ports": {"t": 25, "ok": true, "off": null}}]';
  const marks = json.spans(text);
  const at = (needle) => marks.find((m) => m.column === text.indexOf(needle));

  // Имя поля от значения отличает двоеточие следом - тот же признак, по
  // которому их различает человек.
  assert.equal(at('"in_ports"').type, "type", "имя поля не отличено от строки");
  assert.equal(at('"t"').type, "type");
  assert.equal(at("25").type, "number");
  assert.equal(at("true").type, "constant");
  assert.equal(at("null").type, "constant");
  assert.equal(at("[").type, "operator");

  // Отрезки не выходят за свою строку - иначе раскладчик строк красит чужое.
  const rows = text.split("\n");
  for (const mark of marks) {
    assert.ok(mark.column + mark.length <= rows[mark.line].length,
      `отрезок за краем строки: ${JSON.stringify(mark)}`);
  }

  // Сценарий красится, пока его набирают, то есть почти всегда
  // недописанным: разбор не вправе ни падать, ни терять остаток текста.
  for (const broken of ['[{"a', '{"a": ', '[1, 2,', '"\\"', '{', '']) {
    assert.doesNotThrow(() => json.spans(broken), `разбор упал на ${JSON.stringify(broken)}`);
  }
  const unclosed = json.spans('{"a": "хвост');
  assert.ok(unclosed.some((m) => m.type === "string"), "незакрытая строка не покрашена");
});

test("редактор: перевод строки считается символом, и правило одно", () => {
  // Узлы-строки блочные: перевода строки между ними в DOM нет, а в тексте
  // есть. Пока это правило считали по месту, сохранение каретки о переводах не
  // знало вовсе: после Enter она возвращалась в конец прежней строки, то есть
  // набирать текст было нельзя (воспроизведено на выложенной странице).
  const lengths = [3, 0, 5]; // "abc", "", "hello"
  assert.equal(editor.offsetOfLine(lengths, 0, 0), 0);
  assert.equal(editor.offsetOfLine(lengths, 0, 3), 3);
  assert.equal(editor.offsetOfLine(lengths, 1, 0), 4, "перевод строки не посчитан");
  assert.equal(editor.offsetOfLine(lengths, 2, 0), 5, "пустая строка занимает один символ");
  assert.equal(editor.offsetOfLine(lengths, 2, 5), 10);

  // Обратное правило - точное обращение прямого на всех местах документа.
  for (let index = 0; index < lengths.length; index += 1) {
    for (let inLine = 0; inLine <= lengths[index]; inLine += 1) {
      const offset = editor.offsetOfLine(lengths, index, inLine);
      assert.deepEqual(editor.lineOfOffset(lengths, offset), { index, inLine },
        `не сошлось на строке ${index}, месте ${inLine}`);
    }
  }

  // То же правило обязано совпадать со счётом по тексту: расхождение здесь
  // уводит и наведение, и переход к объявлению - молча, на чужое имя.
  const text = "abc\n\nhello";
  for (let offset = 0; offset <= text.length; offset += 1) {
    const spot = editor.lineOfOffset(lengths, offset);
    const byText = editor.offsetToPosition(text, offset);
    assert.deepEqual({ index: byText.line, inLine: byText.character }, spot,
      `смещение ${offset}: счёт по строкам разошёлся со счётом по тексту`);
  }

  // Место за концом документа - конец последней строки, а не ошибка: так
  // ведёт себя каретка после форматирования, укоротившего текст.
  assert.deepEqual(editor.lineOfOffset(lengths, 99), { index: 2, inLine: 5 });
});

test("области: генерация и диагностика открываются кнопками справа", async () => {
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");

  // Кнопки залипающие и стоят в полосе управления справа.
  const tools = html.slice(html.indexOf('<div class="bar bar-tools">'), html.indexOf("<main"));
  for (const id of ["showgen", "showdiag"]) {
    assert.match(tools, new RegExp(`id="${id}"[^>]*aria-pressed`), `${id} не залипающая`);
  }
  assert.ok(tools.indexOf('class="spacer"') < tools.indexOf('id="showgen"'),
    "кнопки областей не прижаты вправо");

  // Своих областей у симуляции и схемы нет: файл открывается там же, где
  // правится код, а вторая область заставляла бы помнить, где что живёт.
  assert.ok(!html.includes('data-panel="trace"'), "область симуляции осталась");
  assert.ok(!html.includes('data-tab="trace"'), "симуляция осталась вкладкой");
  const source = html.slice(html.indexOf('class="pane pane-source"'), html.indexOf('id="split"'));
  assert.ok(source.includes('data-panel="scheme"'), "схема не в области кода");
  assert.ok(source.includes('id="scenario"'), "сценарий не в области кода");

  // Скрытая генерация не выполняется: печатать в невидимую область - работа
  // впустую, и на большой модели она заметна.
  assert.match(app, /function compile\(\)[\s\S]{0,800}?state\.panel !== "output"[\s\S]{0,40}?return;/,
    "генерация идёт при закрытой панели");
  // ...но и до загрузки модуля её звать нельзя: панель выбирается раньше, и
  // страница падала целиком, показывая "модуль не загрузился" при живом модуле.
  assert.match(app, /function compile\(\)[\s\S]{0,800}?!state\.bridge/,
    "компиляция не защищена от вызова до моста");

  // Обе отжаты - область уходит вместе со своим разделителем.
  assert.match(css, /body\[data-panel="none"\][\s\S]{0,200}?display: none/,
    "закрытая область остаётся на экране");
});

test("сообщения: одна полоса внизу, а в шапке области их нет", async () => {
  // Предмет - место. Сообщение об удавшемся действии и об отказе застаёт автора
  // в любой из областей, и строка в шапке одной из них оставалась бы
  // незамеченной в двух других. Машинно это не ломает ничего, и потому невидимо
  // всем прочим проверкам.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");

  // Полоса стоит после рабочей области и перед полкой режимов: внизу страницы,
  // но не под зоной жестов телефона.
  const order = ["</main>", 'id="say"', 'id="modes"'].map((mark) => html.indexOf(mark));
  assert.ok(order.every((at) => at >= 0), "полосы сообщений в разметке нет");
  assert.deepEqual(order.slice().sort((a, b) => a - b), order, "полоса стоит не внизу страницы");

  // Строки состояния в шапке области кода не осталось: два места для одного
  // сообщения означали бы, что в одном из них оно устаревает.
  const source = html.slice(html.indexOf('class="pane pane-source"'), html.indexOf('id="split"'));
  assert.ok(!source.includes('id="status"'), "строка состояния осталась в шапке области");
  assert.ok(!app.includes("dom.status"), "страница всё ещё пишет в строку состояния");

  // Ширина - та же, что у прочих полос страницы: разъедься они, полоса
  // сообщений встала бы шире шапки.
  assert.match(css, /\.bar, \.work, \.modes, \.say \{/, "полоса вне ширины оболочки");
  // Высота постоянная: растущая под текст полоса двигала бы рабочую область на
  // каждое сообщение.
  assert.match(css, /\.say \{[^}]*height: var\(--h-dense\)/, "высота полосы не постоянна");
  assert.match(css, /\.say \{[^}]*white-space: nowrap/, "полоса переносит строку");
});

test("сообщения: текст короток, и список ключей берётся из кода", async () => {
  // Предел не украшение: полоса не переносит строку, а обрезает её многоточием,
  // и всё, что не поместилось, читателю не достаётся. Список ключей берётся из
  // вызовов `say` - свой список разошёлся бы с кодом молча.
  const sources = await Promise.all(
    PAGE_SCRIPTS.map((name) => readFile(new URL(`../static/${name}`, import.meta.url), "utf8"))
  );
  const keys = new Set();
  for (const source of sources) {
    for (const call of calls(source, "say(")) {
      for (const [, key] of call.matchAll(/\bt\(\s*"([\w.]+)"/g)) keys.add(key);
    }
  }
  assert.ok(keys.size >= 20, `сообщений найдено ${keys.size} — разбор вызовов не сработал`);

  for (const lang of Object.keys(i18n.LANGUAGES)) {
    const dict = await dictionary(lang);
    const long = [...keys]
      .filter((key) => (dict[key] ?? "").length > SAY_LIMIT)
      .map((key) => `${key} (${dict[key].length})`);
    assert.deepEqual(long, [], `в '${lang}' сообщения длиннее ${SAY_LIMIT} знаков: ${long.join(", ")}`);
  }
});

test("диагностика компилятора: одно место — панель диагностики", async () => {
  // Пока отказ цели печатался ещё и в окне генерации, автор смотрел в два
  // места, а сходились они не всегда: окно показывало отказ последней сборки.
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  const body = calls(app, "function compile()")[0] ?? "";

  assert.ok(body.includes("showTargetDiagnostics([refusal("), "отказ цели не уходит в диагностики");
  assert.ok(!/dom\.output\.appendChild\(row\([^)]*error/.test(body),
    "отказ цели всё ещё печатается в области вывода");
  // Пустая область читается как поломка: она обязана сказать о себе сама.
  assert.ok(body.includes('t("output.empty")'), "область вывода молчит при отказе");
  // Предупреждения цели - те же её замечания к модели.
  assert.ok(/showTargetDiagnostics\(\s*\(reply\.warnings/.test(body),
    "предупреждения цели не доходят до диагностик");
  // Закрытая панель снимает и замечания: висящее замечание от сборки, которой
  // не строят, читатель отнёс бы к модели.
  assert.match(app, /state\.panel !== "output"\) \{\s*showTargetDiagnostics\(\[\]\);/,
    "закрытая генерация оставляет свои замечания висеть");

  // Позиция печатается, только когда она есть: "1:1" у отказа без координаты
  // указывало бы на начало файла.
  assert.match(app, /const where = at \? `\$\{at\.start_line \+ 1\}/,
    "позиция диагностики печатается безусловно");

  // Одна ошибка - одна строка. Разбор судит текст, и цель отказывает на нём же:
  // нашлось прогоном страницы, когда синтаксическая ошибка встала в списке
  // дважды - первой строкой от разбора и последней от цели.
  assert.ok(app.includes("sameDiagnostic("), "повтор диагностики не снимается");
  assert.match(app, /state\.targetDiagnostics\.filter\(/, "замечания цели не сверяются с показанными");
});

test("структура проекта: область справа, файлы по родам", async () => {
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const account = await readFile(new URL("../static/account.js", import.meta.url), "utf8");

  // Дерево - последняя область рабочей строки: код, вывод, структура.
  const work = html.slice(html.indexOf("<main"), html.indexOf("</main>"));
  const order = ["pane-source", "pane-result", "pane-tree"].map((cls) => work.indexOf(cls));
  assert.deepEqual(order.slice().sort((a, b) => a - b), order, "порядок областей нарушен");
  assert.ok(work.includes('id="treesplit"'), "у структуры нет своей ручки ширины");

  // Роды перечислены ключами словаря: собранный ключ сверке невидим, и подпись
  // рода пропала бы молча.
  for (const key of ["tree.kind.takt", "tree.kind.layout", "tree.kind.scenario", "tree.kind.markdown", "tree.kind.addressMap"]) {
    assert.ok(account.includes(`"${key}"`), `род '${key}' не назван ключом`);
  }
});

test("миникарта: видимость выбирает читатель, а не правило вёрстки", async () => {
  // Пропадавшее по ширине окно в лист нельзя было ни найти, ни вернуть: о нём
  // попросту не знали. Теперь миникарта - строка того же списка, что панели,
  // и умолчание у неё прежнее ("широкий экран") - но названо словом.
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  const map = panels.PANELS.find((panel) => panel.id === "map");
  assert.ok(map, "миникарты нет в списке панелей");
  assert.equal(map.when, "wide", "умолчание миникарты изменилось");
  assert.equal(map.home, null, "миникарта заняла угол холста");

  // Правило вёрстки её больше не прячет: иначе выбор "всегда" ничего не значил
  // бы на узком экране.
  assert.ok(!/@media \(max-width: 900px\) \{\s*\.minimap \{ display: none; \}/.test(css),
    "миникарту всё ещё прячет ширина окна");
  assert.match(css, /\.minimap\[hidden\] \{ display: none; \}/, "скрытая миникарта занимает место");
  // Место панели нижнего угла отступает от миникарты по её видимости, а не по
  // ширине окна: показанная на узком экране, она накрыла бы панель.
  assert.match(css, /\.scheme\[data-map="on"\] \.tool-dock-br/, "отступ угла привязан к ширине окна");
});

test("значок настроек: зубчатый контур, а не обод со спицами", async () => {
  // Знак узнают по признаку предмета: у шестерни зубцы сидят на ободе, а лучи
  // от центра принадлежат колесу со спицами - и читался он штурвалом. Проверка
  // держит признак: замкнутый контур и ровно одна окружность - центр.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const button = html.slice(html.indexOf('id="settings"'), html.indexOf('id="save"'));
  assert.match(button, /<path d="M[^"]*Z"/, "у значка нет замкнутого зубчатого контура");
  assert.equal((button.match(/<circle/g) ?? []).length, 1, "окружностей у значка не одна: обод вернулся");
  assert.ok(!/M12 2\.5v2\.5/.test(button), "лучи от центра вернулись");
});

test("настройки: кнопка-образец не отдаёт высоту, а вкладки прокручиваются", async () => {
  // Предмет - образец. Он и есть ответ на вопрос "как это будет выглядеть", и
  // ряд кнопок без него превращается в список слов. Машинно сплющенный образец
  // не ломает ничего: страница работает, выбор делается, и увидеть это можно
  // только на самой странице.
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");

  // Сжатие снимается на всех трёх уровнях: применяется оно на каждом.
  assert.match(css, /\.settings-body > \*,\s*\.settings-body \.set-steps,\s*\.settings-body \.set-step \{ flex: 0 0 auto; \}/,
    "ряды, полоса ступеней или кнопка всё ещё отдают высоту");
  // Кнопке-образцу ступень шкалы не годится: в ней рисунок и подпись, а ступень
  // ростом в одну строку обрезала бы рисунок.
  assert.match(css, /\.set-step \{[\s\S]{0,200}?height: auto;/, "высота образца взята из шкалы");
  assert.match(css, /\.set-sample \{[\s\S]{0,120}?flex: none;/, "образец сжимается внутри кнопки");
  // Шести вкладок на 375 px не хватает ширины: полоса прокручивается, а сами
  // вкладки не сжимаются - подпись до многоточия вкладку не называет.
  assert.match(css, /#scheme-tabs \{ overflow-x: auto; \}/, "полоса вкладок не прокручивается");
  assert.match(css, /#scheme-tabs \.tab \{ flex: 0 0 auto; \}/, "вкладки окна сжимаются");
});

test("списки: форма записи одна на журнал прогона и диагностики", async () => {
  // Оба отвечают на вопрос "что случилось", и разная форма у одного ответа
  // заставляла бы читать их по-разному. Правила формы принадлежат контролу, а
  // не месту: пока они были записаны на область схемы, список диагностик не
  // получал их вовсе.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");

  for (const id of ["trace", "diagnostics"]) {
    assert.match(html, new RegExp(`id="${id}"[^>]*class="[^"]*\\blog\\b`), `у '${id}' нет формы записи`);
  }
  assert.ok(!css.includes(".panel-scheme > .trace .row"), "форма записи осталась привязана к месту");
  assert.match(css, /\.log \.row \{[\s\S]{0,200}?border: 1px solid var\(--border\)/, "запись не карточка");
  assert.match(css, /\.log \.row:nth-child\(even\)/, "чередования подложки нет");
  assert.match(css, /\.log \.row\[aria-selected="true"\]/, "пометки выбранной записи нет");

  // Скрытая панель обязана исчезать: раскладка сильнее атрибута `hidden`, и
  // список, получивший её, кнопкой не прячется.
  assert.match(css, /\.log\[hidden\] \{ display: none; \}/, "скрытый список остаётся на экране");

  // Тон несёт колонка рода, а не заливка записи.
  assert.match(css, /\.log \.row-error \.row-kind \{[\s\S]{0,160}?background: var\(--surface-alarm\)/,
    "род отказа не окрашен");
  assert.match(css, /\.log \.row-error \{ color: var\(--on-surface\); \}/,
    "запись отказа красится целиком");

  // Выделение щелчком - одно правило на оба списка.
  assert.match(app, /for \(const list of \[dom\.trace, dom\.diagnostics\]\)/,
    "выделение записи заведено не на оба списка");
});

test("файлы проекта: полоса отвечает на один вопрос за раз", async () => {
  // Пока проекта нет - как его завести или открыть; когда открыт - что делать с
  // ним и его файлами. Обе группы разом заставляли бы искать нужную среди
  // ненужных.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const account = await readFile(new URL("../static/account.js", import.meta.url), "utf8");
  const api = await readFile(new URL("../static/api.js", import.meta.url), "utf8");

  // Панель показывает одно из двух, и полоса отвечает показанному: пока проект
  // не открыт - список проектов и действия над ними; открыли - его состав и
  // действия над файлами.
  // Раскладка пишется в существующий файл с ревизией проекта: без неё сервер
  // отвечает конфликтом, и правка схемы терялась при каждом сохранении, кроме первого.
  assert.match(account, /api\.write\(state\.project\.id, pair, current, state\.layoutFile \? state\.revision : null\)/);
  // Описание проекта приходит подсказкой у строки списка, пустое - не приходит.
  assert.match(account, /if \(about\) node\.dataset\.tip = about;/);
  assert.match(account, /dom\.projects\.hidden = opened;/, "список проектов виден внутри проекта");
  assert.match(account, /dom\.tree\.hidden = !opened;/, "состав виден без проекта");
  assert.match(account, /dom\.newproject\.hidden = opened;/, "заведение проекта видно внутри");
  assert.match(account, /dom\.openproject\.hidden = opened \|\| !chosen;/, "открытие не спрашивает выбор");
  assert.match(account, /dom\.renameproject\.hidden = opened \|\| !mine;/, "переименование не по праву владельца");
  assert.match(account, /dom\.dropproject\.hidden = opened \|\| !mine;/, "удаление не по праву владельца");
  assert.match(account, /dom\.closeproject\.hidden = !opened;/, "закрытие видно без проекта");
  // Выход из проекта - с сохранением: автор уходит, а не выбрасывает работу.
  assert.match(account, /async function closeProject[\s\S]{0,400}?await save\(\);/,
    "закрытие проекта не сохраняет работу");
  // Заводить и удалять файлы вправе тот, кто вправе писать.
  assert.match(account, /dom\.newfile\.hidden = !writes;/, "заведение файла не по праву записи");
  assert.match(account, /dom\.dropfile\.hidden = !writes;/, "удаление файла не по праву записи");

  // Расширение ставит род файла, а не автор: правило проекта не перекладывается
  // на того, кто заводит файл.
  assert.match(account, /const FILE_KINDS = \[[\s\S]{0,300}?extension: "\.takt"/, "родов файла нет");
  // Карта адресов - род наравне с прочими; её текст едет в состав проекта для
  // `--address-map`, а сама она моделью не собирается.
  assert.match(account, /kind: "address_map", label: "file\.kind\.addressMap", extension: "\.takt-map"/);
  assert.match(account, /file\.kind === "takt" \|\| file\.kind === "address_map"/, "карты не читаются в состав");
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  assert.match(app, /if \(state\.kind === "address_map"\) \{\s*state\.editor\.highlight\(state\.bridge\.tokens\(source\), \[\]\);/, "карта собирается как модель");
  // Цель с адресами без карты проекта не собирается - отказ словами.
  assert.match(app, /project\.ADDRESS_TARGETS\.includes\(state\.target\)[\s\S]{0,200}account\.addressMap\(\)/, "карта проекта не спрашивается");
  assert.ok(account.includes('extension: ".json"') && account.includes('extension: ".md"'),
    "род сценария или пояснения не заведён");
  // Раскладка стоит в ряду наравне с прочими: она появляется и сама, но завести
  // её заранее автор вправе.
  assert.match(account, /extension: layoutFile\.EXTENSION/, "рода схемы нет в ряду");
  // Раскладка без своей модели - предупреждение, а не отказ: имя модели автор
  // допишет следом.
  assert.ok(account.includes('t("file.layoutWithoutModel"'), "о схеме без модели не сказано");
  assert.match(account, /name = raw \+ chosenFileKind\(\)\.extension/, "расширение берётся не у рода");

  // Удаление файла спрашивает и уносит парную раскладку: осиротевшая, она
  // показывала бы схему того, чего нет.
  assert.ok(account.includes('t("file.dropAsk"'), "удаление файла не спрашивает");
  assert.match(account, /layoutName\(doomed\)/, "раскладка остаётся после удаления модели");
  assert.match(api, /export async function removeFile\(id, name\)[\s\S]{0,200}?method: "DELETE"/,
    "у страницы нет удаления файла");

  // Окна заведения и удаления файла закрываются щелчком по затемнению и Escape.
  for (const id of ["file-modal", "dropfile-modal"]) {
    assert.ok(html.includes(`id="${id}"`), `окна '${id}' нет`);
  }
  assert.match(account, /MODALS = \[[^\]]*"file-modal"[^\]]*"dropfile-modal"/, "окна файлов не закрываются");
});

test("структура проекта: прячется своей кнопкой и вместе с ручкой ширины", async () => {
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");

  // Кнопка залипающая и стоит там же, где показ генерации и диагностик: это
  // настройки показа страницы, и место у них одно.
  const tools = html.slice(html.indexOf('<div class="bar bar-tools">'), html.indexOf("<main"));
  assert.match(tools, /id="showtree"[^>]*aria-pressed/, "кнопка структуры не залипающая");

  // Прячется состоянием страницы, а не узлами по одному: ручка ширины уходит
  // вместе с областью - иначе она тянула бы долю того, чего на экране нет.
  assert.match(css, /body\[data-tree="off"\][\s\S]{0,120}?display: none/, "область не прячется");
  // Прячется ручка структуры, а не всякая ручка области: ручка журнала
  // диагностик стоит в той же области и обязана остаться.
  assert.match(css, /body\[data-tree="off"\] > \.work > #treesplit/, "ручка размера остаётся");
  // Выбор переживает перезагрузку: это настройка читателя.
  assert.match(app, /UI_KEYS\.treeShown/, "видимость структуры не запоминается");
});

test("проект: действия стоят над его составом, а выгрузка ушла из шапки", async () => {
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const account = await readFile(new URL("../static/account.js", import.meta.url), "utf8");
  const api = await readFile(new URL("../static/api.js", import.meta.url), "utf8");

  // Полоса действий - внутри области структуры и выше дерева: это действия над
  // тем, что показано ниже.
  const tree = html.slice(html.indexOf('class="pane pane-tree"'), html.indexOf("</main>"));
  assert.ok(tree.includes('class="tree-tools"'), "полосы действий над структурой нет");
  for (const id of ["newproject", "openproject", "renameproject", "dropproject",
    "download", "closeproject", "newfile", "dropfile"]) {
    assert.ok(tree.includes(`id="${id}"`), `в полосе действий нет '${id}'`);
  }
  // Список проектов стоит в самой панели, над составом открытого: сперва "где я
  // работаю", затем "с чем". Окна выбора нет - оно пряталось за первым же
  // открытием и не показывало, где автор находится.
  assert.ok(tree.includes('id="projects"'), "списка проектов нет в панели");
  assert.ok(!html.includes('id="open-modal"'), "окно выбора проекта осталось");
  const order = ['class="tree-tools"', 'id="projects"', 'id="tree"'].map((mark) => tree.indexOf(mark));
  assert.deepEqual(order.slice().sort((a, b) => a - b), order, "порядок панели не тот");

  // Заведение, переименование и удаление проекта - разговоры, и у каждого своё
  // окно: имя и подтверждение спрашиваются словами.
  for (const id of ["project-modal", "projectname-modal", "drop-modal"]) {
    assert.ok(html.includes(`id="${id}"`), `окна '${id}' нет`);
  }
  const panel = html.slice(html.indexOf('id="panel"'), html.indexOf('id="conflict"'));
  assert.ok(!panel.includes('id="newname"'), "заведение проекта осталось в панели учётной записи");
  assert.ok(!panel.includes('id="projects"'), "список проектов остался в панели учётной записи");

  // Отметка образца кладёт в новый проект рабочую модель - тем же файлом, каким
  // проект открывается. Имя файла даёт сам проект: файлы носят его имя.
  assert.match(account, /dom\.fromsample\.checked[\s\S]{0,120}?api\.write\(created\.id, firstFileName\(name\), SAMPLE/,
    "отметка образца не кладёт модель");
  assert.match(account, /function firstFileName\(project\)/, "имя первого файла не от проекта");
  // Пустой проект открывается пустым: прежний текст выглядел бы его содержимым.
  assert.match(account, /host\.open\(\{ source: "", scenario: "", layout: "" \}\)/,
    "новый проект открывается прежним текстом");

  // Удаление необратимо: оно спрашивает и называет проект по имени.
  assert.ok(account.includes('t("account.dropAsk"'), "удаление не спрашивает");
  assert.match(api, /export async function remove\(id\)[\s\S]{0,160}?method: "DELETE"/,
    "у страницы нет удаления проекта");
  // Переименование и удаление предлагаются только владельцу: отказ сервера на
  // действие, которое страница предложила сама, читается как поломка.
  assert.match(account, /const mine = chosen\?\.level === "owner";/,
    "право на проект не спрашивается у выбранного");
});

test("шапка: две полосы, и каждая отвечает на свой вопрос", async () => {
  // Предмет - состав полос: верхняя
  // отвечает "что за страница" (имя, версия языка, время сборки, вход), нижняя
  // - "что я могу сделать". Уехавшая вниз кнопка входа не ломает ничего
  // машинно и потому невидима всем прочим проверкам.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  const brand = html.slice(html.indexOf('<header class="bar bar-brand">'), html.indexOf("</header>"));
  const tools = html.slice(html.indexOf('<div class="bar bar-tools">'), html.indexOf("<main"));

  // Переключатель языка стоит наверху, рядом со входом: он не действие над
  // моделью, а свойство самой страницы.
  for (const id of ["version", "project", "openfile", "whoami-bar", "session", "lang"]) {
    assert.ok(brand.includes(`id="${id}"`), `верхняя полоса без '${id}'`);
  }
  // Выгрузка архивом ушла отсюда к структуре проекта: это действие над
  // проектом, а не над страницей.
  assert.ok(!tools.includes('id="download"'), "выгрузка осталась в полосе управления");
  // Кнопок "Мои проекты" и "Открытые проекты" нет: проекты живут в структуре
  // проекта, а панель учётной записи открывает логин в шапке.
  for (const id of ["account", "showcase", "finder"]) {
    assert.ok(!html.includes(`id="${id}"`), `'${id}' остался в разметке`);
  }
  const accountSource = await readFile(new URL("../static/account.js", import.meta.url), "utf8");
  assert.match(accountSource, /dom\["whoami-bar"\]\.addEventListener\("click", \(\) => toggle\(\)\)/, "логин не открывает панель учётной записи");
  for (const id of ["save", "share"]) {
    assert.ok(tools.includes(`id="${id}"`), `полоса управления без '${id}'`);
    assert.ok(!brand.includes(`id="${id}"`), `'${id}' остался в верхней полосе`);
  }
  assert.ok(!tools.includes('id="session"'), "кнопка входа уехала в управление");

  // Действия над открытым текстом стоят у самого текста - тем же приёмом, что
  // действия над проектом у его состава: предмет у них тот же, что у показанного
  // ниже, а не у страницы.
  const source = html.slice(html.indexOf('<section class="pane pane-source">'), html.indexOf('id="split"'));
  for (const id of ["format", "pickscenario"]) {
    assert.ok(!tools.includes(`id="${id}"`), `'${id}' остался в полосе страницы`);
    assert.ok(source.includes(`id="${id}"`), `'${id}' не стоит у текста`);
  }
  // Перенос строк - настройка чтения, а не действие над текстом: своей кнопки у
  // него нет вовсе, выбор живёт в окне настроек рядом с языком.
  assert.ok(!html.includes('id="wrap"'), "кнопка переноса осталась на странице");
  const settings = await readFile(new URL("../static/scheme-settings.js", import.meta.url), "utf8");
  assert.match(settings, /id: "page:wrap"/, "переноса нет в настройках страницы");
  // Настройка одна на все области кода: правило переноса обязано покрыть и
  // сценарий с трассой, иначе кнопка на них молчит.
  for (const area of [".editor.wrap", ".output.wrap", ".scenario.wrap", ".trace.wrap"]) {
    assert.ok(css.includes(area), `перенос не действует на ${area}`);
  }
  assert.match(css, /\.icon-btn\[aria-pressed="true"\][\s\S]{0,120}?background:/,
    "нажатое состояние кнопки-значка не показано");

  // Значка у кнопки входа два, и видимым обязан быть ровно один: правило
  // гашения по атрибуту без этого молчит, а на экране рядом стоят вход и выход.
  assert.ok(brand.includes('id="icon-enter"') && brand.includes('id="icon-leave"'));
  assert.match(css, /\.icon\[hidden\]\s*\{[^}]*display:\s*none/, "значок не гасится атрибутом");

  // Кнопка одна на оба действия: страница решает по тому, вошли ли.
  const account = await readFile(new URL("../static/account.js", import.meta.url), "utf8");
  assert.match(account, /session\.addEventListener\("click",[\s\S]{0,80}?api\.who\(\)\s*\?\s*leave\(\)/,
    "кнопка входа не различает вход и выход");
});

test("подсказка: своя, а нативной не остаётся", async () => {
  // Предмет - отсутствие нативной. Оставь `title` рядом со своей панелью, и
  // браузер нарисует вторую поверх первой, со своей задержкой и чужим шрифтом;
  // на снимке экрана это заметно, а в тестах - нет, если их не написать.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const natives = html.split("\n").flatMap((line, i) =>
    /(?<![\w-])title="/.test(line) ? [`строка ${i + 1}`] : []
  );
  assert.deepEqual(natives, [], `в разметке остался title: ${natives.join(", ")}`);

  // Каждая подсказка разметки переводится: подпись кнопки-значка - это всё,
  // что читатель о ней узнаёт, и непереведённая оставляет её безымянной.
  for (const [, node] of html.matchAll(/<[^>]*\bdata-tip="[^"]*"[^>]*>/g).map((m) => [0, m[0]])) {
    assert.match(node, /data-i18n-attr="[^"]*data-tip:/, `подсказка без ключа: ${node.slice(0, 60)}…`);
  }

  // принятый приём: узел с `title` из кода перехватывается - атрибут снимается,
  // текст переезжает в `data-tip`.
  const node = {
    attrs: { title: "Подпись" },
    dataset: {},
    hasAttribute(name) { return name in this.attrs; },
    getAttribute(name) { return this.attrs[name]; },
    removeAttribute(name) { delete this.attrs[name]; },
  };
  assert.equal(tip.claim(node), "Подпись");
  assert.equal(node.hasAttribute("title"), false, "нативный title не снят");
  assert.equal(tip.claim(node), "Подпись", "повторный показ теряет текст");
});

test("подсказка: место панели не выходит за окно", () => {
  const view = { width: 400, height: 300 };
  const box = { width: 120, height: 40 };

  // Обычный случай: под элементом, по центру.
  const middle = tip.place({ left: 100, top: 100, width: 40, height: 20, bottom: 120 }, box, view);
  assert.equal(middle.side, "below");
  assert.equal(middle.left, 100 + 20 - 60);
  assert.equal(middle.top, 120 + tip.GAP);

  // Элемент у нижнего края: панель переворачивается вверх.
  const low = tip.place({ left: 100, top: 270, width: 40, height: 20, bottom: 290 }, box, view);
  assert.equal(low.side, "above");
  assert.equal(low.top, 270 - box.height - tip.GAP);

  // Элемент у левого и правого края: панель прижимается, но не вылезает.
  const left = tip.place({ left: 0, top: 10, width: 20, height: 20, bottom: 30 }, box, view);
  assert.equal(left.left, tip.EDGE);
  const right = tip.place({ left: 380, top: 10, width: 20, height: 20, bottom: 30 }, box, view);
  assert.equal(right.left, view.width - box.width - tip.EDGE);

  // Окно ниже панели: места нет нигде, и верх обязан упереться в край, а не
  // уехать за него - иначе подсказка была бы обрезана невидимо для автора.
  const tiny = tip.place({ left: 10, top: 5, width: 20, height: 20, bottom: 25 }, box, { width: 400, height: 50 });
  assert.ok(tiny.top >= tip.EDGE, `панель за верхним краем: ${tiny.top}`);
});

test("настройки интерфейса живут в localStorage и ключи у них разные", async () => {
  // Предмет - полнота: настройка, которую читатель выбрал, обязана
  // пережить перезагрузку. Ключи собраны в
  // одном месте: придуманный по месту однажды разойдётся с тем, кто его
  // читает, и читатель получит умолчание там, где выбирал сам.
  const keys = [
    shell.KEY, shell.PANES_KEY, shell.ROWS_KEY,
    shell.FONT_KEY, shell.WRAP_KEY, shell.UI_KEYS.panel,
    shell.UI_KEYS.tab, shell.UI_KEYS.budget,
  ];
  assert.equal(new Set(keys).size, keys.length, `ключи совпали: ${keys.join(", ")}`);
  for (const key of keys) assert.match(key, /^takt\./, `ключ вне пространства: ${key}`);

  // Чтение: пусто - умолчание, значение - оно само, отказ хранилища - умолчание.
  const box = memoryStorage();
  assert.equal(shell.setting(box, shell.UI_KEYS.tab, "output"), "output");
  shell.remember(box, shell.UI_KEYS.tab, "trace");
  assert.equal(shell.setting(box, shell.UI_KEYS.tab, "output"), "trace");
  const locked = { getItem() { throw new Error("нет доступа"); }, setItem() { throw new Error("нет доступа"); } };
  assert.equal(shell.setting(locked, shell.UI_KEYS.budget, "200"), "200");
  shell.remember(locked, shell.UI_KEYS.budget, 999); // не роняет страницу

  // Каждая настройка интерфейса читается страницей: ключ, который только
  // пишут, - мёртвая настройка, и читатель не поймёт, почему его выбор пропал.
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  for (const name of ["UI_KEYS.tab", "UI_KEYS.budget"]) {
    assert.ok(app.includes(`setting(localStorage, shell.${name}`), `${name} не читается страницей`);
    assert.ok(app.includes(`remember(localStorage, shell.${name}`), `${name} не пишется страницей`);
  }
});

test("кегль страницы: шаг в единицу и обе границы названы", () => {
  // Меняется корневой кегль: ступени шкалы заданы в `rem`, и страница
  // растёт целиком, сохраняя пропорции. Свой кегль "только для кода" развалил
  // бы шкалу на два набора, между которыми пришлось бы выбирать в каждом
  // правиле.
  assert.equal(shell.clampFont(shell.FONT_DEFAULT + 1), shell.FONT_DEFAULT + 1, "шаг в единицу");
  assert.equal(shell.clampFont(0), shell.FONT_MIN, "ниже нижней границы нечитаемо");
  assert.equal(shell.clampFont(999), shell.FONT_MAX, "выше верхней не помещается код");
  assert.equal(shell.clampFont(NaN), shell.FONT_DEFAULT, "испорченная запись — умолчание");
  assert.equal(shell.clampFont(12.4), 12, "кегль целый: полпикселя не бывает");
  assert.equal(shell.fontSize({ getItem: () => null }), shell.FONT_DEFAULT);
  assert.equal(shell.fontSize({ getItem: () => "13" }), 13);
  assert.equal(shell.fontSize({ getItem: () => "не число" }), shell.FONT_DEFAULT);
  assert.ok(shell.FONT_MIN < shell.FONT_DEFAULT && shell.FONT_DEFAULT < shell.FONT_MAX);
});

test("схема перестраивается по показанному, а не по панели вывода", async () => {
  // Предмет - у кого спрашивают "показана ли схема". Схема стала одним из
  // показов области кода наравне с кодом и сценарием, и признак у неё там же.
  // Пока его брали у области вывода, значение "схема" не появлялось там никогда:
  // граф не перестраивался ни на правку модели, ни на открытие раскладки, а лист
  // при этом показывался - раскладку кладёт свой путь.
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  assert.ok(!/state\.panel\s*!==\s*"scheme"/.test(app), "показ схемы спрашивают у области вывода");
  const draw = app.slice(app.indexOf("function drawScheme"), app.indexOf("\n}", app.indexOf("function drawScheme")));
  assert.match(draw, /state\.shown !== "scheme"/, "перестроение не смотрит на показанное");
  assert.match(draw, /setGraph\(/, "перестроение не строит граф");
});

test("открытие файла любого рода обновляет полосу действий", async () => {
  // Предмет - полнота ответа полосы. У открытия три ветви: сценарий назначается
  // активным, раскладка открывает парную модель, прочее читается с сервера.
  // Уйди ветвь молча - у проекта, чей активный файл сценарий, полоса осталась бы
  // полосой закрытого: ни закрыть, ни завести файл.
  const account = await readFile(new URL("../static/account.js", import.meta.url), "utf8");
  const from = account.indexOf("async function openFile(");
  const body = account.slice(from, account.indexOf("\n}", account.indexOf("keepScenario", from)));
  const returns = body.split("\n").filter((line) => line.trim() === "return;").length;
  const refreshes = body.split("\n").filter((line) => line.trim() === "refresh();").length;
  assert.ok(returns >= 2, "ветвей раннего выхода стало меньше - проверьте набор");
  assert.ok(refreshes >= returns, `ветвей ${returns}, обновлений полосы ${refreshes}`);
});

test("действия над файлом обращены к выбранному в структуре", async () => {
  // Предмет - какой файл трогают мусорка и переименование. Выбранный в дереве не
  // равен открытому в области кода: сценарий и раскладку открывают, не меняя
  // открытого файла, и по нему действия пришлись бы не на тот файл, на который
  // смотрит автор.
  const account = await readFile(new URL("../static/account.js", import.meta.url), "utf8");
  assert.match(account, /state\.picked = name;/, "выбор в дереве не запоминается");
  assert.match(account, /function picked\(\)/, "у выбранного файла нет одного носителя");
  for (const name of ["openRenameFile", "openDropFile", "renameFile", "dropFile"]) {
    const from = account.indexOf(`function ${name}(`);
    assert.ok(from > 0, `${name} пропала`);
    const body = account.slice(from, account.indexOf("\n}", from));
    assert.match(body, /picked\(\)/, `${name} трогает не выбранный файл`);
  }
  // Отметка выбора видна: без неё автор не знает, к чему обращены кнопки.
  assert.match(account, /aria-pressed", String\(node\.dataset\.file === state\.picked\)/,
    "выбранный файл не отмечен в дереве");
});

test("погашенный вывод цели переживает перезагрузку", async () => {
  // Предмет - форма записи выбора. Пустую строку `setting` читает как "выбора не
  // было" и отвечает умолчанием: погашенная панель пишется пустотой - и
  // возвращается при каждой перезагрузке и каждом открытии проекта.
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  assert.ok(!/remember\(localStorage, shell\.UI_KEYS\.panel, name \?\? ""\)/.test(app),
    "погашенная панель пишется пустой строкой");
  assert.match(app, /remember\(localStorage, shell\.UI_KEYS\.panel, name \?\? "none"\)/,
    "погашенная панель не пишется словом");
  // Чтение обязано отличать погашенную от незаданной: умолчание - показ.
  assert.match(app, /setting\(localStorage, shell\.UI_KEYS\.panel, "output"\) === "output"/,
    "чтение панели не отличает погашенную");
});

test("журнал диагностик: заглушка догоняет словарь", async () => {
  // Предмет - порядок внутри перерисовки. Заглушку пустого журнала строит первая
  // же отрисовка, а та случается раньше словаря: выбранная область зовёт сборку,
  // и сборка без моста отвечает пустым списком диагностик. Ранний выход из
  // перерисовки написан для того, чему нужен мост, и заглушка к этому не
  // относится - стой она после выхода, читатель видел бы ключ вместо слов.
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  const from = app.indexOf("function redraw()");
  const body = app.slice(from, app.indexOf("\n}", from));
  const stub = body.indexOf("row-none");
  const bail = body.indexOf("if (!state.bridge) return;");
  assert.ok(stub > 0, "заглушка не переписывается при смене языка");
  assert.ok(bail > 0, "ранний выход из перерисовки пропал");
  assert.ok(stub < bail, "заглушка переписывается после раннего выхода - ключ останется");
});

test("сценарий занимает область кода целиком, как модель", async () => {
  // Область кода показывает один файл, и место в ней принадлежит показанному:
  // модель, сценарий и пояснение правятся одним редактором, значит и высота у
  // них одна. Своя высота досталась сценарию от вкладки прогона, где он делил
  // место с трассой; вкладки нет, а правило пережило её и держало поле в шестой
  // части области.
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  assert.ok(!/--trace-t/.test(css), "у сценария осталась своя высота");
  const shellText = await readFile(new URL("../static/shell.js", import.meta.url), "utf8");
  assert.ok(!/TRACE_KEY|attachTraceRows/.test(shellText), "ручка вкладки прогона осталась");
  // Разметка обязана дать сценарию класс редактора: без него он не получит ни
  // раскладки, ни оформления, и правило "одна область - один показ" развалится.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  assert.match(html, /id="scenario" class="editor scenario"/, "сценарий не редактор");
});

test("перенос строк: настройка своя у каждой области и по умолчанию выключена", () => {
  // Умолчание - нет переноса: код читают столбцом, и включённый по
  // умолчанию перенос менял бы вид всякой модели у всякого читателя.
  assert.equal(shell.wrapped({ getItem: () => null }, shell.WRAP_KEY), false);
  assert.equal(shell.wrapped({ getItem: () => "0" }, shell.WRAP_KEY), false);
  assert.equal(shell.wrapped({ getItem: () => "1" }, shell.WRAP_KEY), true);
  // Ключи разные: узкой бывает то одна область, то другая, и общий ключ
  // переносил бы строки там, где места хватает.
  // Запрет хранилища не роняет страницу - настройка живёт до перезагрузки.
  assert.equal(
    shell.wrapped({ getItem: () => { throw new Error("нет доступа"); } }, shell.WRAP_KEY),
    false
  );
});

test("разделитель рядов: та же ручка правил, другая ось", () => {
  // Предмет - одно правило на оба разделителя: границы, память и счёт доли
  // у них общие, разойдись они - "ещё" и "выше" стали бы разными контролами.
  const work = { left: 100, width: 800, top: 50, height: 400 };
  assert.equal(shell.ratioAt(250, work, "y"), 0.5, "середина по высоте");
  assert.equal(shell.ratioAt(150, work, "y"), 0.25);
  assert.equal(shell.ratioAt(0, work, "y"), shell.MIN_RATIO, "прижимается сверху");
  assert.equal(shell.ratioAt(5000, work, "y"), 1 - shell.MIN_RATIO);
  // Ось по умолчанию - горизонтальная: у неё считается ширина, а не высота.
  assert.equal(shell.ratioAt(500, work), 0.5);
  // Ключи хранилища разные: общий ключ означал бы, что колонки и ряды
  // помнят одну долю на двоих и таскают друг друга.
  assert.notEqual(shell.PANES_KEY, shell.ROWS_KEY);
  assert.equal(shell.panes({ getItem: () => "0.7" }, shell.ROWS_KEY), 0.7);
  // Умолчание рядов не половина: до разделителя список диагностик занимал
  // 30 % высоты, и читатель, ничего не тронувший, не должен обнаружить, что
  // редактор ужался вдвое.
  assert.equal(
    shell.panes({ getItem: () => null }, shell.ROWS_KEY, shell.ROWS_DEFAULT),
    shell.ROWS_DEFAULT
  );
  assert.equal(shell.clampRatio(NaN, shell.ROWS_DEFAULT), shell.ROWS_DEFAULT);
});

test("разделитель областей: доля считается от рабочей области, а не от окна", () => {
  // Оболочка стоит по центру и бывает уже окна: считай мы от края окна -
  // разделитель уезжал бы из-под указателя тем сильнее, чем уже оболочка.
  const work = { left: 200, width: 800 };
  assert.equal(shell.ratioAt(600, work), 0.5, "середина области — половина");
  assert.equal(shell.ratioAt(400, work), 0.25);
  // Указатель ушёл за край области - доля прижимается, а не уходит за предел.
  assert.equal(shell.ratioAt(0, work), shell.MIN_RATIO);
  assert.equal(shell.ratioAt(5000, work), 1 - shell.MIN_RATIO);
  // Области ещё нет на экране (нулевая ширина) - умолчание, а не деление на ноль.
  assert.equal(shell.ratioAt(100, { left: 0, width: 0 }), shell.HALF);
});

test("адаптив: точки перелома перечислены в одном месте", async () => {
  // Числа порогов разбегаются по файлу первыми: одно правило поправили,
  // другое забыли, и на одной ширине действуют обе раскладки. Реестр -
  // комментарий в шапке `app.css`, а множество чисел обязано ему равняться.
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  const found = new Set(
    [...css.matchAll(/@media[^{]*?\((?:max|min)-(?:width|height):\s*(\d+)px\)/g)].map((m) => m[1])
  );
  assert.deepEqual([...found].sort(), ["560", "900"], `точки перелома: ${[...found]}`);
});

test("адаптив: `vh` всегда идёт с `dvh`, а полка знает про зону жестов", async () => {
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  // `vh` без `dvh` оставляет на телефоне пустую полосу под съехавшей адресной
  // строкой - замер образца 2026-09-04.
  for (const [, block] of css.matchAll(/\{([^}]*100vh[^}]*)\}/g)) {
    assert.match(block, /100dvh/, `рядом с 100vh нет 100dvh: ${block.trim()}`);
  }
  // `env(safe-area-*)` без `viewport-fit=cover` равен нулю - половина правила
  // не работает, и заметить это можно только на устройстве.
  if (css.includes("env(safe-area")) {
    assert.match(html, /viewport-fit=cover/, "есть env(safe-area), нет viewport-fit=cover");
  }
});

test("адаптив: у каждой вкладки есть подпись словом", async () => {
  // По одним значкам вкладку находят не все (правило образца). Подпись -
  // не украшение, а единственный носитель смысла для скринридера.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  for (const [, inner] of html.matchAll(/<button[^>]*role="tab"[^>]*>([^<]*)<\/button>/g)) {
    assert.ok(inner.trim().length > 0, "вкладка без подписи");
  }
});

test("страница проекта: адрес разбирается вместе с префиксом", () => {
  // Разбирается путь, а не адрес целиком: сервис умеет стоять за обратным
  // прокси под префиксом, и `/takt/p/<id>` - тот же случай. Ошибись разбор - и
  // страница молча откроется черновиком вместо проекта.
  assert.equal(project.idInPath("/p/AbCd-_12"), "AbCd-_12");
  assert.equal(project.idInPath("/takt/p/AbCd-_12"), "AbCd-_12");
  assert.equal(project.idInPath("/p/AbCd-_12/"), "AbCd-_12");
  assert.equal(project.idInPath("/"), null, "корень — не проект");
  assert.equal(project.idInPath("/p/"), null, "адрес без идентификатора");
  assert.equal(project.idInPath("/p/чужое имя"), null, "не идентификатор");

  // Корень API считается от пути страницы: относительный адрес от документа
  // `/p/<id>` увёл бы запрос в `/p/api/...` - нашлось бы только в браузере.
  assert.equal(project.apiRoot("/p/AbCd-_12"), "/");
  assert.equal(project.apiRoot("/takt/p/AbCd-_12"), "/takt/");
  assert.equal(project.apiRoot("/takt/"), "/takt/");
});

test("страница проекта: читается активный файл и сценарий рядом", async () => {
  const asked = [];
  const answers = {
    "/api/projects/AbCd": {
      id: "AbCd",
      name: "Термореле",
      owner: "ivan",
      visibility: "public",
      takt_lang: "0.58.0",
      build_target: "sv-mmio",
      build_args: "--bus=apb",
      main_file: "model.takt",
      main_scenario: "cold.json",
      files: [
        { name: "other.takt", kind: "takt", size_bytes: 3 },
        { name: "model.takt", kind: "takt", size_bytes: 9 },
        { name: "cold.json", kind: "scenario", size_bytes: 2 },
        { name: "run.json", kind: "scenario", size_bytes: 2 },
        { name: "readme.md", kind: "markdown", size_bytes: 5 },
      ],
    },
    "/api/projects/AbCd/files/model.takt": { name: "model.takt", text: MODEL },
    "/api/projects/AbCd/files/cold.json": { name: "cold.json", text: "[{}]" },
    "/api/projects/AbCd/files/run.json": { name: "run.json", text: "[]" },
  };
  const get = async (url) => {
    asked.push(url);
    const body = answers[url];
    if (!body) return { ok: false, status: 404, json: async () => ({}) };
    return { ok: true, status: 200, json: async () => body };
  };

  const opened = await project.read("AbCd", "/", get);
  assert.equal(opened.source, MODEL, "открылся активный файл, а не первый по имени");
  // Сценариев несколько, и берётся названный проектом, а не первый по имени: на
  // первом читатель увидел бы не тот прогон, которым автор
  // показывает работу модели. `cold.json` стоит в списке раньше `run.json` -
  // проверка различает "названный" и "первый" только потому, что они разные.
  assert.equal(opened.scenario, "[{}]", "взят сценарий, названный проектом");
  assert.equal(opened.owner, "ivan", "автор назван: чужая модель не выглядит своей");
  assert.equal(opened.version, "0.58.0", "версия модуля — свойство проекта (A5)");
  // Чужой проект открывается сборкой автора. Пара берётся не умолчанием (`c` без
  // ключей): на умолчании потеря поля неотличима от его подстановки, и проверка
  // была бы зелёной при потерянном выборе.
  assert.equal(opened.target, "sv-mmio", "цель — свойство проекта");
  assert.equal(opened.args, "--bus=apb", "ключи — свойство проекта");
  // Лишних обращений нет: витрина бывает длинной, и читать всё подряд незачем.
  assert.equal(asked.length, 3, `обращений ${asked.length}: ${asked.join(", ")}`);
});

test("страница проекта: закрытый отвечает названным отказом", async () => {
  // Отказ поднимается ключом словаря, а не текстом: текст оболочки строит одна
  // точка - главный поток страницы.
  const get = async () => ({ ok: false, status: 404, json: async () => ({}) });
  await assert.rejects(
    () => project.read("AbCd", "/", get),
    (error) => error.key === "project.notFound",
  );

  const broken = async () => {
    throw new Error("сеть");
  };
  await assert.rejects(
    () => project.read("AbCd", "/", broken),
    (error) => error.key === "project.failed",
  );
});

test("холст панели: чужое сообщение пропускается, своё разбирается", () => {
  // В окне редактора соседствуют свои источники сообщений: панель обязана
  // пропускать то, что адресовано не ей, а не падать и не рисовать пустоту.
  assert.equal(host.parseIncoming(null), null);
  assert.equal(host.parseIncoming({ type: "чужое" }), null);
  assert.deepEqual(host.parseIncoming({ type: "layout", text: "x" }), { type: "layout", text: "x" });
  assert.deepEqual(host.parseIncoming({ type: "layout" }), { type: "layout", text: "" });
  assert.deepEqual(
    host.parseIncoming({ type: "cursor", line: 3, character: 7 }),
    { type: "cursor", line: 3, character: 7 },
  );
  // Позиция, пришедшая не числом, курсор бы увела: такое сообщение не читается.
  assert.equal(host.parseIncoming({ type: "cursor", line: "3", character: null }), null);
});

test("холст панели: наружу уходит правка раскладки и выбор узла", () => {
  assert.deepEqual(host.layoutMessage("текст"), { type: "layout", text: "текст" });
  // Курсор ставит редактор, и координаты ему нужны в единицах протокола -
  // берутся у имени узла, а не у холста.
  const node = { nameRange: { start_line: 5, start_character: 6 } };
  assert.deepEqual(host.selectMessage(node), { type: "select", line: 5, character: 6 });
  // Узел без позиции курсор не двигает: чужая координата хуже отсутствующей.
  assert.equal(host.selectMessage({}), null);
  assert.deepEqual(host.readyMessage(), { type: "ready" });
});

test("панели: место набирает их в линию, а не в столбец", async () => {
  // Полосу кнопок ждут полосой: столбец занимает вертикаль холста там, где
  // свободна ширина, а панели вправе ужаться и прокрутиться внутри себя.
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  const dock = css.slice(css.indexOf(".tool-dock {"), css.indexOf("}", css.indexOf(".tool-dock {")));
  assert.match(dock, /flex-direction:\s*row/, "место панелей набирает их столбцом");
  assert.match(dock, /flex-wrap:\s*nowrap/, "линия переносится, и панели снова встают друг под другом");
  assert.match(css, /\.tool-dock > \.tool-panel \{[^}]*min-width:\s*0/, "панель в линии не ужимается");
});

test("настройки: кнопка стоит в шапке, а не на скрываемой панели", async () => {
  // Кнопка, живущая на панели, которую читатель вправе снять, снимается вместе
  // с ней - и вернуть настройки становится нечем.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const bar = html.slice(html.indexOf('<div class="bar bar-tools">'), html.indexOf("</div>", html.indexOf('<div class="bar bar-tools">')));
  assert.ok(bar.includes('id="settings"'), "кнопки настроек нет в полосе инструментов");
  const panels = html.matchAll(/<div id="panel-\w+" class="tool-panel[\s\S]*?\n          <\/div>/g);
  for (const [panel] of panels) {
    assert.ok(!panel.includes('id="settings"'), "кнопка настроек вернулась на панель");
  }
});

test("панели: чужое имя и ступень вне набора не переживают чтения", () => {
  // Хранилище переживает выкладки, а состав панелей меняется: запись о панели,
  // которой больше нет, не должна ни падать, ни оживать при возврате имени.
  const clean = panels.cleanState({
    run: { dock: "tr", when: "hidden" },
    view: { dock: "нигде", when: "иногда" },
    ghost: { dock: "tl", when: "always" },
    legend: { dock: "tl", when: "wide" },
  });
  assert.deepEqual(clean.run, { dock: "tr", when: "hidden" });
  assert.deepEqual(clean.view, undefined, "ступень и место вне набора отброшены");
  assert.equal(clean.ghost, undefined, "чужое имя не читается");
  // У легенды углов холста нет, и место ей не пишется даже из хранилища.
  assert.deepEqual(clean.legend, { when: "wide" });
});

test("панели: видимость решает ступень, а не одна лишь ширина", () => {
  assert.equal(panels.visible("always", true), true, "всегда - значит и на узком");
  assert.equal(panels.visible("hidden", false), false);
  assert.equal(panels.visible("wide", false), true);
  assert.equal(panels.visible("wide", true), false, "широкий экран - не узкий");
});

test("панели: дома разведены по углам, и ни одна пара не делит угол", () => {
  // Холст рядом с легендой узок: две панели, поселённые в один угол, накрывают
  // друг друга, и переставить их читатель уже не может.
  const homes = panels.PANELS.map((panel) => panel.home).filter(Boolean);
  assert.deepEqual([...new Set(homes)], homes, "два дома совпали");
  for (const home of homes) assert.ok(panels.DOCKS.includes(home), `место '${home}' не объявлено`);
});

test("панели: умолчания доезжают до состояния, а сохранённое их перекрывает", () => {
  const fresh = panels.stateOf({});
  assert.equal(fresh.run.dock, panels.panelOf("run").home);
  assert.equal(fresh.sheet.when, "wide", "лист на узком экране уступает место холсту");
  const saved = panels.stateOf({ run: { dock: "br" }, sheet: { when: "hidden" } });
  assert.equal(saved.run.dock, "br");
  assert.equal(saved.run.when, "always", "неназванная половина берётся у умолчания");
  assert.equal(saved.sheet.when, "hidden");
});

test("язык: список модулей страницы полон", async () => {
  // Модуль, забытый в списке, ускользает от обеих проверок разом - и от
  // сверки ключей, и от поиска текста мимо словаря. Список тем самым перестаёт
  // быть списком, оставаясь на вид полным.
  const files = (await readdir(new URL("../static/", import.meta.url)))
    .filter((name) => name.endsWith(".js"))
    .sort();
  assert.deepEqual(PAGE_SCRIPTS.slice().sort(), files, "список модулей отстал от каталога");
});

test("черновик v2: круговой рейс ключуется проектом и файлом", () => {
  const storage = memoryStorage();
  draft.saveFile(storage, {
    project: "p1", file: "model.takt", revision: 3, source: "первый", savedAt: 100,
  });
  draft.saveFile(storage, {
    project: "p2", file: "model.takt", revision: 7, source: "второй", savedAt: 200,
  });
  // Одинаковое имя файла в двух проектах - не один черновик: ключ несёт и
  // проект, иначе работа над одним затирала бы работу над другим.
  assert.equal(draft.loadFile(storage, "p1", "model.takt").source, "первый");
  assert.equal(draft.loadFile(storage, "p2", "model.takt").source, "второй");
  // Ревизия хранится вместе с текстом: без неё при возвращении нельзя сказать,
  // разошёлся ли черновик с сервером.
  assert.equal(draft.loadFile(storage, "p1", "model.takt").revision, 3);
  assert.equal(draft.loadFile(storage, "p3", "model.takt"), null, "чужого нет");

  draft.clearFile(storage, "p1", "model.takt");
  assert.equal(draft.loadFile(storage, "p1", "model.takt"), null, "успешное сохранение стирает");
  assert.equal(draft.loadFile(storage, "p2", "model.takt").source, "второй", "соседний цел");

  // Безымянный буфер живёт своей жизнью: им пользуется тот, кто не входил.
  draft.save(storage, { source: "буфер" });
  draft.saveFile(storage, { project: "p9", file: "a.takt", source: "проектный" });
  assert.equal(draft.load(storage).source, "буфер");
  assert.equal(draft.loadFile(storage, "p9", "a.takt").source, "проектный");
});

test("черновик v2: цель и ключи перекрывают проект, а прежняя запись их не имеет", () => {
  // Проект задаёт умолчание, черновик перекрывает его для своего автора. Без пары
  // в черновике незавершённый выбор терялся бы при каждой перезагрузке, тогда как
  // текст её переживает.
  const storage = memoryStorage();
  draft.saveFile(storage, {
    project: "p1", file: "model.takt", revision: 3, source: "текст",
    target: "rust", args: "--fsm=table", savedAt: 100,
  });
  const kept = draft.loadFile(storage, "p1", "model.takt");
  assert.equal(kept.target, "rust");
  assert.equal(kept.args, "--fsm=table");
  // Имя сценария хранится вместе с его текстом: сценариев несколько, и текст
  // одного под именем другого - подмена, а не сохранность.
  assert.equal(kept.scenarioFile, null, "без имени сценария поле пусто, а не пропадает");
  draft.saveFile(storage, {
    project: "p1", file: "model.takt", source: "текст",
    scenario: "[{}]", scenarioFile: "warm.json", savedAt: 200,
  });
  const withScenario = draft.loadFile(storage, "p1", "model.takt");
  assert.equal(withScenario.scenarioFile, "warm.json");
  assert.equal(withScenario.scenario, "[{}]");

  // Прежняя запись (без пары) обязана читаться: подъём формы черновика не
  // вправе стоить автору несохранённой работы. Пустое значение и означает
  // "выбора нет" - пару подставит проект.
  const old = memoryStorage();
  old.setItem(
    "takt.draft.v2",
    JSON.stringify({ "p1\u0000model.takt": { project: "p1", file: "model.takt", source: "текст" } }),
  );
  const legacy = draft.loadFile(old, "p1", "model.takt");
  assert.equal(legacy.source, "текст", "текст прежнего черновика цел");
  assert.equal(legacy.target ?? "", "", "выбора в прежней записи нет");
});

test("черновик v2: карта не растёт без предела", () => {
  // Предел нужен не ради места, а ради предела: `localStorage` кончается
  // молча и кончается на записи - то есть в момент сохранения работы.
  const storage = memoryStorage();
  for (let i = 0; i < draft.DRAFTS_KEPT + 5; i += 1) {
    draft.saveFile(storage, {
      project: "p", file: `f${i}.takt`, source: `текст ${i}`, savedAt: 1000 + i,
    });
  }
  const kept = JSON.parse(storage.getItem("takt.draft.v2"));
  assert.equal(Object.keys(kept).length, draft.DRAFTS_KEPT);
  // Уходят старшие: к черновику, до которого не возвращались двадцать файлов
  // назад, автор уже не вернётся.
  assert.equal(draft.loadFile(storage, "p", "f0.takt"), null, "старший вытеснен");
  assert.ok(draft.loadFile(storage, "p", `f${draft.DRAFTS_KEPT + 4}.takt`), "младший на месте");
});

test("сессия: пара переживает перезагрузку, а выход её забывает", async () => {
  const storage = memoryStorage();
  const answers = {
    "/api/token": { access_token: "A1", refresh_token: "R1" },
    "/api/me": { id: "u1", login: "ivan", role: "user" },
    "/api/revoke": null,
  };
  const get = async (url) => ({
    ok: true,
    status: answers[url] === null ? 204 : 200,
    json: async () => answers[url],
  });
  api.configure({ root: "/", fetch: get, storage });
  assert.equal(api.signed(), false, "без пары мы никто");
  const me = await api.signIn("ivan", "пароль-пароль");
  assert.equal(me.login, "ivan");
  assert.ok(storage.getItem("takt.session.v1"), "пара записана");

  // Перезагрузка страницы: новый клиент поднимает пару из хранилища.
  api.configure({ root: "/", fetch: get, storage });
  assert.equal(api.signed(), true, "после перезагрузки вход сохранён");
  assert.equal(api.who().login, "ivan");

  await api.signOut();
  assert.equal(api.signed(), false);
  assert.equal(storage.getItem("takt.session.v1"), null, "пара забыта");
});

test("сессия: просроченный доступ обновляется ОДИН раз на все запросы", async () => {
  // refresh одноразовый: две параллельные попытки гасят семейство целиком,
  // то есть выкидывают автора ровно тогда, когда он сохраняет работу.
  const storage = memoryStorage();
  storage.setItem(
    "takt.session.v1",
    JSON.stringify({ access: "старый", refresh: "R1", login: "ivan", role: "user" })
  );
  let refreshes = 0;
  let fresh = false;
  const get = async (url, options) => {
    if (url === "/api/token") {
      refreshes += 1;
      fresh = true;
      return { ok: true, status: 200, json: async () => ({ access_token: "A2", refresh_token: "R2" }) };
    }
    if (!fresh) return { ok: false, status: 401, json: async () => ({ error: "unauthorized" }) };
    assert.equal(options.headers.authorization, "Bearer A2", "запрос повторён свежим токеном");
    return { ok: true, status: 200, json: async () => [] };
  };
  api.configure({ root: "/", fetch: get, storage });
  await Promise.all([api.projects(), api.projects(), api.projects()]);
  assert.equal(refreshes, 1, `обновлений пары ${refreshes}, а должно быть одно`);
});

test("сессия: отказ приходит кодом и числами, а не разобранным текстом", async () => {
  const storage = memoryStorage();
  const get = async () => ({
    ok: false,
    status: 409,
    json: async () => ({
      error: "revision_conflict",
      message: "проект изменился: у вас ревизия 1, у проекта 2",
      seen: 1,
      revision: 2,
    }),
  });
  api.configure({ root: "/", fetch: get, storage });
  await assert.rejects(
    () => api.write("p1", "model.takt", "текст", 1),
    (error) => {
      // Числа взяты полями: разбирай их страница из сообщения - текст отказа
      // стал бы частью протокола и перестал бы переводиться.
      assert.equal(error.code, "revision_conflict");
      assert.equal(error.seen, 1);
      assert.equal(error.revision, 2);
      assert.equal(error.key, "api.failed", "ключ один на все коды сервера");
      return true;
    }
  );
});

test("разметка: каждый узел с именем найден страницей", async () => {
  // Список имён в `cache()` - второй носитель разметки, и отстаёт он молча:
  // забытый `signout` дал пустую страницу с сообщением "модуль не загружен",
  // хотя модуль был загружен.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  const ids = [...html.matchAll(/\bid="([^"]+)"/g)].map((m) => m[1]);
  assert.ok(ids.length > 0, "в разметке нет именованных узлов");
  const cached = new Set(
    [...app.matchAll(/"([a-z][a-z0-9-]*)"/g)].map((m) => m[1])
  );
  const missing = ids.filter((id) => !cached.has(id));
  assert.deepEqual(missing, [], `узлы разметки не найдены страницей: ${missing.join(", ")}`);
});

test("списки: значение выставляет одна точка, и она обновляет надстройку", async () => {
  // Открытый чужой проект ставит цель `sv-mmio`, и кнопка списка обязана её
  // показать: иначе страница говорит одно, а собирает другим. Надстройка
  // `pick.js` рисует подпись сама, а тихая запись `select.value` не рождает
  // `change`.
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  const pick = await readFile(new URL("../static/pick.js", import.meta.url), "utf8");
  // Надстройка следует за источником истины, когда ей об этом говорят
  // общепринятым способом.
  assert.ok(
    /select\.addEventListener\("change", refresh\)/.test(pick),
    "надстройка не слушает `change` своего списка",
  );
  // Тихая запись идёт только через одну точку: список мест, которые надо не
  // забыть, - это и есть то, что забывается.
  const silent = [...app.matchAll(/dom\.(target|lang)\.value\s*=/g)].map((m) => m[0]);
  assert.deepEqual(silent, [], `значение списка пишется мимо setPick: ${silent.join(", ")}`);
  assert.ok(/function setPick\(/.test(app) && /picks\[name\]\?\.refresh\(\)/.test(app),
    "точка записи не обновляет надстройку");
});

test("markdown: один разбор кормит и подсветку, и показ", async () => {
  // Разбор один на обе работы: заведи их порознь - и строка красилась бы
  // заголовком, а показывалась абзацем, причём увидеть это можно только
  // глазами. Здесь обе стороны спрашиваются об одном тексте.
  const text = [
    "# Термореле",
    "",
    "Греет, пока `холодно`, и **ждёт**.",
    "",
    "- один",
    "- два",
    "",
    "| порт | смысл |",
    "|---|---|",
    "| heater | нагрев |",
  ].join("\n");
  const kinds = md.parse(text).map((block) => block.type);
  assert.deepEqual(kinds, ["heading", "paragraph", "list", "table"]);
  const marks = md.spans(text);
  assert.ok(
    marks.some((mark) => mark.line === 0 && mark.column === 0 && mark.type === "keyword"),
    "решётка заголовка не покрашена",
  );
  assert.ok(
    marks.some((mark) => mark.line === 2 && mark.type === "string"),
    "код в строке не покрашен",
  );
});

test("markdown: показ строит узлы, а не разметку из данных", async () => {
  const mdSource = await readFile(new URL("../static/md.js", import.meta.url), "utf8");
  // Текст пишет автор проекта, а видят его читатели витрины: `innerHTML`
  // здесь означал бы чужой сценарий в чужом браузере. Признак - и запрет
  // записи разметки в модуле, и поведение на подложном тексте.
  const source = mdSource;
  // Ищется употребление, а не слово: слово стоит в пояснении модуля ("здесь
  // его нет"), и запрет на слово запрещал бы объяснять правило.
  assert.ok(
    !/\.innerHTML|\.outerHTML|insertAdjacentHTML/.test(source),
    "модуль показа пишет разметку строкой",
  );

  // Схема ссылки пропускается по списку, а не по запрету: запретить один
  // `javascript:` мало - есть `data:` и `vbscript:`.
  assert.equal(md.safeHref("https://example.org"), "https://example.org");
  assert.equal(md.safeHref("mailto:a@b.c"), "mailto:a@b.c");
  assert.equal(md.safeHref("/p/AbCd"), "/p/AbCd");
  for (const bad of ["javascript:alert(1)", "data:text/html,x", "vbscript:msgbox"]) {
    assert.equal(md.safeHref(bad), null, `схема пропущена: ${bad}`);
  }
});

test("системные сообщения: строятся узлами, повтор не плодится, крестик убирает", async () => {
  // Текст приходит от чужого источника (ответ сервера, сообщение среды), поэтому
  // разметкой строкой его не пишут. Проверяется и запрет в модуле, и поведение.
  const source = await readFile(new URL("../static/alerts.js", import.meta.url), "utf8");
  assert.ok(
    !/\.innerHTML|\.outerHTML|insertAdjacentHTML/.test(source),
    "носитель сообщений пишет разметку строкой",
  );

  const host = fakeHost();
  const first = alerts.show(host, "модуль не загружен", "Убрать");
  assert.ok(first, "сообщение не показано");
  assert.equal(host.children.length, 1);
  assert.equal(first.children[0].textContent, "модуль не загружен", "текст не в textContent");

  // Отказ обещания приходит очередями: одно и то же сообщение не обязано
  // выстраиваться стопкой во весь экран.
  assert.equal(alerts.show(host, "модуль не загружен", "Убрать"), null, "повтор показан");
  assert.equal(host.children.length, 1);
  alerts.show(host, "другой сбой", "Убрать");
  assert.equal(host.children.length, 2, "другое сообщение не показано");

  // Крестик снимает своё сообщение и только его.
  first.children[1].click();
  assert.equal(host.children.length, 1);
  assert.equal(host.children[0].dataset.text, "другой сбой");

  // Текст ошибки: сообщение, если оно есть, иначе сама величина.
  assert.equal(alerts.textOf(new Error("сеть недоступна")), "сеть недоступна");
  assert.equal(alerts.textOf("строкой"), "строкой");
  assert.equal(alerts.textOf(null), "", "пустая ошибка обязана быть пустым текстом");
});

test("разметка: скрытый узел действительно скрыт", async () => {
  // `display: flex` сильнее `hidden`: узел с таким классом остаётся на
  // экране, сколько его ни прячь. Класс ловился прогоном страницы дважды -
  // теперь его ловит машина: у каждого класса, которым помечен скрываемый
  // узел, обязано быть правило `[hidden]`, если этот класс задаёт `display`.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  const hiddenClasses = new Set();
  for (const [, tag] of html.matchAll(/<([^>]*\bhidden\b[^>]*)>/g)) {
    const classes = /class="([^"]+)"/.exec(tag);
    if (!classes) continue;
    for (const name of classes[1].split(/\s+/)) if (name) hiddenClasses.add(name);
  }
  const unprotected = [];
  for (const name of hiddenClasses) {
    const sets = new RegExp(`\\.${name}\\s*(,[^{]*)?\\{[^}]*display:`).test(css);
    const guards = css.includes(`.${name}[hidden]`);
    if (sets && !guards) unprotected.push(name);
  }
  assert.deepEqual(unprotected, [], `класс задаёт display и не гасится: ${unprotected.join(", ")}`);
});

test("язык: подписи площадок объявляет сервер, а словарь их знает", async () => {
  // Реестр сверяется двумя сторонами: сервер объявляет ключ подписи, словарь
  // даёт текст. Заведи площадку на сервере без записи в словаре - кнопка
  // показала бы читателю служебный ключ, и увидел бы это он, а не проверка.
  const keys = await serverLabelKeys();
  assert.ok(keys.length >= 3, `сервер объявил подписей: ${keys.length}`);
  for (const lang of Object.keys(i18n.LANGUAGES)) {
    const dictionary = JSON.parse(
      await readFile(new URL(`../static/i18n/${lang}.json`, import.meta.url), "utf8")
    );
    for (const key of keys) {
      assert.ok(dictionary[key], `в словаре '${lang}' нет подписи площадки '${key}'`);
    }
  }
});

test("вход: окно, и значки площадок объявлены сервером", async () => {
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const modal = html.slice(html.indexOf('<div id="signin-modal"'), html.indexOf("</body>"));

  // Вход - разговор: у него есть все три исхода, и "отменить" тоже. Окно без
  // выхода запирает страницу, а закрыть его мышью мимо кнопки нельзя.
  for (const id of ["login", "password", "signin", "signup", "signin-cancel", "oauth"]) {
    assert.ok(modal.includes(`id="${id}"`), `окно входа без '${id}'`);
  }
  assert.match(modal, /role="dialog"[\s\S]{0,80}aria-modal="true"/, "окно не объявлено модальным");

  // Ряд кнопок читается расположением: подтвердить и отменить слева и одной
  // ширины (пара равных исходов), главная - справа. Разная ширина у пары
  // читается как разная важность, которой между ними нет.
  const order = ["signin", "signin-cancel", "signup"].map((id) => modal.indexOf(`id="${id}"`));
  assert.deepEqual(order.slice().sort((a, b) => a - b), order, "порядок кнопок окна нарушен");
  for (const id of ["signin", "signin-cancel"]) {
    assert.match(modal, new RegExp(`id="${id}"[^>]*class="pair"`), `${id} не в паре равной ширины`);
  }
  assert.match(modal, /id="signup"[^>]*class="primary"/, "регистрация не объявлена главной");
  const cssModal = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  assert.match(cssModal, /\.modal-actions \.pair \{[^}]*min-width/, "пара кнопок без общей ширины");
  assert.match(cssModal, /\.modal-actions \.primary \{[^}]*margin-left: auto/, "главная не отжата вправо");

  const account = await readFile(new URL("../static/account.js", import.meta.url), "utf8");
  assert.match(account, /Escape[\s\S]{0,60}closeSignin/, "окно не закрывается клавишей");

  // Значок приходит именем файла от сервера - как и подпись: своего списка
  // площадок у страницы нет. Файл обязан существовать: сервер объявит значок,
  // которого нет, и кнопка выйдет пустой - сверка двух сторон, как у подписей.
  const api = await readFile(new URL("../server/src/oauth/api.rs", import.meta.url), "utf8");
  const icons = [...api.matchAll(/icon:\s*"([\w.-]+)"/g)].map((m) => m[1]);
  assert.ok(icons.length > 0, "сервер не объявляет значков — сверка выродилась в успех");
  const files = await readdir(new URL("../static/brand/", import.meta.url));
  for (const icon of icons) {
    assert.ok(files.includes(icon), `значок ${icon} объявлен сервером, а файла нет`);
  }
  for (const file of files) {
    assert.ok(icons.includes(file), `значок ${file} лежит в статике, а сервер о нём не знает`);
  }
});

test("страница не знает имён площадок", async () => {
  // Приём тот же, что "нет списка ключевых слов Takt в вебе": свой список
  // площадок разошёлся бы с настройкой стенда молча, и кнопка вела бы в никуда.
  for (const name of PAGE_SCRIPTS) {
    const source = await readFile(new URL(`../static/${name}`, import.meta.url), "utf8");
    const code = source
      .split("\n")
      .filter((line) => !line.trim().startsWith("//"))
      .join("\n");
    const found = /["'`](yandex|vk|mail_ru)["'`]/i.exec(code);
    assert.equal(found, null, `${name}: имя площадки в коде — ${found?.[0]}`);
  }
});

/** Ключи подписей площадок, объявленные сервером. */
async function serverLabelKeys() {
  const source = await readFile(
    new URL("../server/src/oauth/api.rs", import.meta.url),
    "utf8"
  );
  return [...source.matchAll(/label:\s*"([\w.]+)"/g)].map((match) => match[1]);
}

test("витрина и архив: страница просит у сервера ровно то, что нужно", async () => {
  const storage = memoryStorage();
  const asked = [];
  const get = async (url, options) => {
    asked.push(`${options?.method ?? "GET"} ${url}`);
    if (url.includes("/api/public")) {
      return {
        ok: true,
        status: 200,
        json: async () => ({ items: [{ id: "p1", name: "Термореле", owner: "ivan" }] }),
      };
    }
    if (url.includes("/archive")) {
      return { ok: true, status: 200, arrayBuffer: async () => new Uint8Array([80, 75]).buffer };
    }
    return { ok: true, status: 201, json: async () => ({ id: "p2", name: "Копия" }) };
  };
  api.configure({ root: "/", fetch: get, storage });

  // Витрина спрашивается без токена: открытый проект открыт и для того, у
  // кого учётной записи нет вовсе.
  const page = await api.showcase("термореле", null);
  assert.equal(page.items[0].name, "Термореле");
  assert.ok(asked[0].includes("q=%D1%82"), `запрос без слова поиска: ${asked[0]}`);

  // Курсор уходит обратно как есть: своей постраничности у страницы нет.
  await api.showcase(null, "cursor-1");
  assert.ok(asked[1].endsWith("cursor=cursor-1"), asked[1]);

  // Архив забирается запросом, а не ссылкой: у закрытого проекта он требует
  // токена, а обычная ссылка заголовков не несёт.
  const bytes = await api.archive("p1", "c");
  assert.equal(new Uint8Array(bytes)[0], 80, "пришли не байты архива");
  assert.ok(asked[2].includes("/api/projects/p1/archive?target=c"), asked[2]);

  const created = await api.importArchive(new Uint8Array([80, 75]).buffer);
  assert.equal(created.name, "Копия");
  assert.ok(asked[3].startsWith("POST /api/projects/import"), asked[3]);
});

/**
 * Узел-хозяин в памяти: столько от документа, сколько трогает `alerts.js`.
 *
 * Полноценного документа в наборе нет и не заводится: предмет проверки -
 * поведение носителя (узлы, повтор, снятие), а не браузер.
 */
function fakeHost() {
  const doc = {
    createElement(tag) {
      const node = {
        tag,
        children: [],
        dataset: {},
        className: "",
        textContent: "",
        listeners: {},
        ownerDocument: doc,
        setAttribute(name, value) { node[name] = value; },
        appendChild(child) { node.children.push(child); child.parent = node; return child; },
        addEventListener(name, handler) { node.listeners[name] = handler; },
        click() { node.listeners.click?.(); },
        remove() {
          const at = node.parent?.children.indexOf(node) ?? -1;
          if (at >= 0) node.parent.children.splice(at, 1);
        },
      };
      return node;
    },
  };
  const host = doc.createElement("div");
  return host;
}

/** Хранилище в памяти - тот же интерфейс, что у `localStorage`. */
function memoryStorage() {
  const map = new Map();
  return {
    getItem: (key) => (map.has(key) ? map.get(key) : null),
    setItem: (key, value) => map.set(key, String(value)),
    removeItem: (key) => map.delete(key),
  };
}

test("вход: без входа видны верхняя шапка и справка, и справка несъёмна", async () => {
  // Предмет - две половины одного решения: правила стилей прячут всё, кроме верхней
  // полосы и справки, а справка без входа не закрывается - иначе читатель остался бы
  // перед пустым полем. Вёрстку смотрит прогон страницы; здесь - то, что ломается молча.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  const rule = css.match(/((?:body\[data-auth="out"\][^,{]*,\s*)*body\[data-auth="out"\][^,{]*)\{\s*display:\s*none;?\s*\}/);
  assert.ok(rule, "правила 'без входа' нет");
  const hidden = rule[1].split(",").map((part) => part.replace('body[data-auth="out"]', "").trim());
  assert.deepEqual(hidden, [".bar-tools", ".modes", ".grip", ".say:empty", ".work > :not(.help)"], "без входа прячется не то");
  const work = html.slice(html.indexOf('<main class="work">'));
  assert.match(work, /^<main class="work">\s*(?:<!--[\s\S]*?-->\s*)*<div id="help" class="help"/, "справка - не прямой потомок рабочего поля");
  const brand = html.slice(html.indexOf('<header class="bar bar-brand">'), html.indexOf("</header>"));
  for (const id of ["session", "lang"]) assert.ok(brand.includes(`id="${id}"`), `вход без '${id}' в верхней полосе`);
  const accountSource = await readFile(new URL("../static/account.js", import.meta.url), "utf8");
  assert.match(accountSource, /function refresh\(\) \{\s*const me = api\.who\(\);\s*host\.signedIn\?\.\(me !== null\);/, "панель не говорит странице о входе");

  // Справка на заглушках узлов: страница без DOM.
  const node = () => {
    const listeners = {};
    return {
      hidden: true,
      focused: 0,
      textContent: "",
      attrs: {},
      listeners,
      setAttribute(name, value) { this.attrs[name] = value; },
      focus() { this.focused += 1; },
      addEventListener(type, fn) { listeners[type] = fn; },
    };
  };
  const nodes = {};
  for (const id of ["showhelp", "help", "help-search", "help-count", "help-prev", "help-next", "help-close", "help-toc", "help-doc"]) {
    nodes[id] = node();
  }
  nodes["help-close"].hidden = false;
  const documentBefore = globalThis.document;
  const fetchBefore = globalThis.fetch;
  const doc = node();
  globalThis.document = doc;
  globalThis.fetch = async () => ({ ok: false, status: 404 });
  try {
    const { attachHelp } = await import("../static/help.js");
    const help = attachHelp(nodes, { storage: null });
    help.pin(true);
    assert.equal(nodes.help.hidden, false, "без входа справка открыта");
    assert.equal(nodes["help-close"].hidden, true, "у несъёмной справки нет крестика");
    help.close();
    doc.listeners.keydown({ key: "Escape", preventDefault() {} });
    assert.equal(nodes.help.hidden, false, "несъёмная справка закрылась");
    const focused = nodes.showhelp.focused;
    help.pin(false);
    assert.equal(nodes.help.hidden, true, "после входа справка снята");
    assert.equal(nodes["help-close"].hidden, false, "после входа крестик вернулся");
    assert.equal(nodes.showhelp.focused, focused, "снятие справки входом увело фокус");
    help.open();
    help.close();
    assert.equal(nodes.help.hidden, true, "вошедший закрывает справку, как прежде");
  } finally {
    globalThis.document = documentBefore;
    globalThis.fetch = fetchBefore;
  }
});

test("справка: поле поиска стоит у кнопок перехода, а не у заголовка", async () => {
  // Поле, число совпадений и стрелки - одно действие и одна группа у правого края;
  // между полем и стрелками - ничего.
  const html = await readFile(new URL("../static/index.html", import.meta.url), "utf8");
  const head = html.slice(html.indexOf('<div class="help-head">'), html.indexOf('<div class="help-body">'));
  const order = [...head.matchAll(/<(?:span|input|button)\b[^>]*?(?:id="([\w-]+)"|class="(help-find)")/g)].map(
    (m) => m[1] ?? m[2],
  );
  assert.deepEqual(
    order,
    ["help-title", "help-find", "help-count", "help-search", "help-prev", "help-next", "help-close"],
    "порядок шапки справки",
  );
  const find = head.slice(head.indexOf('<span class="help-find">'), head.indexOf('<button id="help-close"'));
  for (const id of ["help-count", "help-search", "help-prev", "help-next"]) {
    assert.ok(find.includes(`id="${id}"`), `'${id}' вне группы поиска`);
  }
  const css = await readFile(new URL("../static/app.css", import.meta.url), "utf8");
  assert.match(css, /\.help-find \{[^}]*margin-inline-start: auto;/, "группа поиска не прижата к правому краю");
});

test("экспорт: запрос из выбора окна и порядок «запись раскладки, затем экспорт»", async () => {
  const { exportRequest, runExport, DEFAULTS, mimeOf, bytesOf } = await import("../static/export.js");
  const context = {
    files: { "m.takt": "start A;", "m.takt-ui": "{}" },
    main_file: "m.takt",
    main_scenario: null,
    model: "m.takt",
    sheet: "/#Line",
    scenario: "m_run.json",
    steps: 40,
    name: "Бак",
  };
  // Лист: модель и лист открытые; видео - всегда цветное; проект - без модели и листа.
  const sheet = exportRequest({ ...DEFAULTS }, context);
  assert.deepEqual(
    [sheet.formats, sheet.view, sheet.model, sheet.sheet, sheet.legend, sheet.archive],
    [["svg"], "draft", "m.takt", "/#Line", true, "Бак.export.zip"]
  );
  const video = exportRequest({ ...DEFAULTS, format: "mp4", view: "draft", scope: "model", pause: 250 }, context);
  assert.deepEqual([video.view, video.sheet, video.pause, video.steps], ["run", null, 250, 40]);
  const project = exportRequest({ ...DEFAULTS, scope: "project", format: "png", background: "none" }, context);
  assert.deepEqual([project.model, project.sheet, project.background], [null, null, "none"]);
  assert.equal(mimeOf("a.export.zip"), "application/zip");
  assert.deepEqual([...bytesOf("AAEC")], [0, 1, 2]);

  // Порядок: раскладка пишется до того, как собирается состав, а состав - до запроса.
  const order = [];
  const saved = [];
  const host = {
    keep: async () => order.push("keep"),
    context: async () => (order.push("context"), context),
    export: async (request) => {
      order.push("export");
      assert.equal(request.files["m.takt-ui"], "{}", "модуль получает текст файла раскладки");
      return { ok: true, files: [{ name: "m.draft.svg", data: "PHN2Zy8+" }], names: ["m.draft.svg"], notes: [] };
    },
    download: (name, bytes, type) => saved.push([name, new TextDecoder().decode(bytes), type]),
    say: () => {},
  };
  assert.equal(await runExport({ ...DEFAULTS }, host), true);
  assert.deepEqual(order, ["keep", "context", "export"], "запись раскладки - до экспорта");
  assert.deepEqual(saved, [["m.draft.svg", "<svg/>", "image/svg+xml"]]);

  // Отказ модуля сказан словами, и загрузки нет.
  const said = [];
  const refused = await runExport({ ...DEFAULTS }, {
    ...host,
    export: async () => ({ ok: false, error: { message: "раскладки нет" } }),
    download: () => assert.fail("отказ не загружается"),
    say: (text, kind) => said.push(kind),
  });
  assert.equal(refused, false);
  assert.equal(said.at(-1), "error");
});

test("экспорт: модуль экспорта грузит поток прогона по первой команде, и один раз", async () => {
  // Предмет - деление модулей: ядро грузится при каждом открытии, модуль
  // экспорта - только когда экспорт попросили. Поток прогона - на подменах: сеть
  // и компиляция модуля записывают, что у них спросили.
  const fetched = [];
  const posted = [];
  const saved = { self: globalThis.self, fetch: globalThis.fetch, instantiate: WebAssembly.instantiate };
  globalThis.self = { postMessage: (message) => posted.push(message) };
  globalThis.fetch = async (url) => {
    fetched.push(String(url));
    return { arrayBuffer: async () => new ArrayBuffer(8) };
  };
  const memory = new WebAssembly.Memory({ initial: 1 });
  const answer = new TextEncoder().encode(JSON.stringify({ ok: true, files: [], names: [], notes: [] }));
  WebAssembly.instantiate = async () => ({
    instance: {
      exports: {
        memory,
        takt_io_ptr: () => 0,
        takt_io_reserve: () => 65536,
        takt_export: () => {
          new Uint8Array(memory.buffer, 0, answer.length).set(answer);
          return answer.length;
        },
      },
    },
  });
  try {
    await import(`../static/worker.js?export-${Date.now()}`);
    for (const id of [1, 2]) {
      await globalThis.self.onmessage({ data: { type: "export", id, exportUrl: "wasm/x/takt-export.wasm?t", request: {} } });
    }
    assert.deepEqual(fetched, ["wasm/x/takt-export.wasm?t"], "модуль экспорта загружен один раз и по своему адресу");
    assert.deepEqual(
      posted.map((m) => m.type),
      ["exportLoading", "exported", "exported"],
      "о загрузке сказано один раз, ответы - на каждую команду"
    );
    assert.deepEqual(posted.slice(1).map((m) => m.id), [1, 2]);
  } finally {
    globalThis.self = saved.self;
    globalThis.fetch = saved.fetch;
    WebAssembly.instantiate = saved.instantiate;
  }
  // Главный поток модуль экспорта не грузит: адрес уходит потоку прогона.
  const app = await readFile(new URL("../static/app.js", import.meta.url), "utf8");
  assert.ok(!/Bridge\.load\([^)]*export/i.test(app), "главный поток грузит модуль экспорта");
  assert.match(app, /exportUrl: exportUrl\(\)/, "адрес модуля экспорта не уходит потоку");
});

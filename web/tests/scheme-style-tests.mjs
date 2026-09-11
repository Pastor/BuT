// Оформление чертежа совпадает с оформлением холста.
//
// Числа и цвета листа схемы живут в `web/static/app.css`, а чертёж вне браузера
// берёт их из таблицы носителя (`takt-scheme/src/style.rs`). Смена шкалы правится в
// двух местах, и расхождение дошло бы до картинки молча: экспорт показал бы
// другую толщину линии или другой оттенок, чем холст. Сверка читает оба файла по
// форме правил.
//
// Подключается из `web-tests.mjs`.

import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const CSS = new URL("../static/app.css", import.meta.url);
const STYLE = new URL("../../takt-scheme/src/style.rs", import.meta.url);
const SCHEME = new URL("../static/scheme.js", import.meta.url);

/** Значение переменной CSS из первого объявления. */
function cssVar(css, name) {
  const found = new RegExp(`--${name}:\\s*([^;]+);`).exec(css);
  assert.ok(found, `в app.css нет --${name}`);
  return found[1].trim();
}

/** Константа палитры носителя. */
function rustConst(rust, name) {
  const found = new RegExp(`const ${name}: &str = "([^"]+)";`).exec(rust);
  assert.ok(found, `в style.rs нет ${name}`);
  return found[1];
}

test("оформление: палитра чертежа - светлая тема холста", async () => {
  const css = await readFile(CSS, "utf8");
  const rust = await readFile(STYLE, "utf8");
  const pairs = {
    INK: "raw-ink", INK_SOFT: "raw-ink-soft", INK_OFF: "raw-ink-off", PAPER_RAISED: "raw-paper-raised",
    LINE: "raw-line", ACCENT: "raw-accent", ALARM: "raw-alarm", ALARM_INK: "raw-alarm-ink", ALARM_BG: "raw-alarm-bg",
    WARN: "raw-warn", WARN_INK: "raw-warn-ink", WARN_BG: "raw-warn-bg", YES: "raw-yes", YES_INK: "raw-yes-ink", SHEET: "raw-sheet",
  };
  for (const [constant, variable] of Object.entries(pairs)) {
    assert.equal(rustConst(rust, constant).toUpperCase(), cssVar(css, variable).toUpperCase(), `${constant} против --${variable}`);
  }
});

test("оформление: ступени толщины, кегли и индекс - те же, что у холста", async () => {
  const css = await readFile(CSS, "utf8");
  const rust = await readFile(STYLE, "utf8");
  for (const [level, value] of [["thin", 1], ["normal", 1.5], ["bold", 2.5]]) {
    const rule = new RegExp(`data-edge-width="${level}"\\] \\{ --sheet-edge: ([\\d.]+);`).exec(css);
    assert.equal(Number(rule?.[1]), value, `ступень линии ${level} в app.css`);
    const arm = level === "normal" ? /_ => ([\d.]+),\s*\}\s*\}\s*\n\s*\/\/\/ Вид чертежа/s : new RegExp(`"${level}" => ([\\d.]+),`);
    assert.equal(Number(arm.exec(rust)?.[1]), value, `ступень линии ${level} в style.rs`);
  }
  assert.equal(cssVar(css, "text-root"), `${/ROOT_PX: f64 = ([\d.]+);/.exec(rust)[1].replace(/\.0$/, "")}px`, "корневой кегль");
  for (const [level, css_name] of [["xs", "text-xs"], ["sm", "text-sm"], ["lg", "text-lg"]]) {
    const rem = Number(cssVar(css, css_name).replace("rem", ""));
    assert.equal(Number(new RegExp(`"${level}" => ([\\d.]+),`).exec(rust)?.[1]), rem, `кегль ${level}`);
  }
  assert.equal(Number(/_ => ([\d.]+),\s*\}\s*\}\s*\n\s*\n\s*\/\/\/ Доля кегля/s.exec(rust)?.[1]), Number(cssVar(css, "text-md").replace("rem", "")), "кегль md");
  assert.equal(`${/INDEX_EM: f64 = ([\d.]+);/.exec(rust)[1]}em`, cssVar(css, "text-index"), "доля индекса");
});

test("оформление: формы наконечников - таблица холста", async () => {
  const js = await readFile(SCHEME, "utf8");
  const rust = await readFile(STYLE, "utf8");
  for (const form of ["open", "solid", "line"]) {
    const page = new RegExp(`${form}: \\{ d: "([^"]+)", close: "([^"]*)", size: (\\d+) \\}`).exec(js);
    assert.ok(page, `ARROWS.${form} в scheme.js`);
    const arm = form === "open" ? /_ => \("([^"]+)", "([^"]*)", ([\d.]+)\)/ : new RegExp(`"${form}" => \\("([^"]+)", "([^"]*)", ([\\d.]+)\\)`);
    const own = arm.exec(rust);
    assert.ok(own, `форма ${form} в style.rs`);
    assert.deepEqual([own[1], own[2], Number(own[3])], [page[1], page[2], Number(page[3])], `наконечник ${form}`);
  }
});

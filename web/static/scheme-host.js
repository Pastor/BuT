// Холст схемы в панели редактора: те же модули, данные снаружи.
//
// # Чем этот режим отличается от страницы
//
// В браузере граф считает модуль (`takt_graph`), а раскладка живёт в проекте.
// В панели редактора компилятор уже есть - это `takt-lsp`, - а раскладка лежит
// файлом рядом с моделью. Поэтому здесь холст не считает ничего: граф, текст
// раскладки и положение курсора приходят снаружи, а наружу уходят правка
// раскладки и выбор узла.
//
// # Разметка у холста одна
//
// Разметку холста строит сборка из `index.html`: второй её носитель разошёлся
// бы с первым молча, и панель показывала бы вчерашний холст. Модули те же -
// `scheme.js`, `panels.js`, `legend.js`, `layout.js`.
//
// # Протокол
//
// Сообщения ходят через `postMessage`. Снаружи внутрь: `graph` - граф модели в
// форме `takt/graph`, `layout` - содержимое файла раскладки, `cursor` - позиция
// курсора в редакторе, `lang` - язык оболочки. Изнутри наружу: `layout` -
// раскладка изменилась, `select` - выбран узел, `ready` - холст готов.

import * as i18n from "./i18n.js";
import { t } from "./i18n.js";
import * as layoutFile from "./layout.js";
import { Scheme } from "./scheme.js";
import * as tip from "./tip.js";

/** Виды сообщений снаружи внутрь. */
export const INCOMING = ["graph", "layout", "cursor", "lang"];

/** Виды сообщений изнутри наружу. */
export const OUTGOING = ["ready", "layout", "select"];

/**
 * Разбирает сообщение хоста.
 *
 * Чужое сообщение - не отказ: в окне редактора соседствуют свои источники, и
 * панель обязана пропускать то, что адресовано не ей.
 *
 * @param {unknown} data тело сообщения
 * @returns {object|null} сообщение известного вида либо `null`
 */
export function parseIncoming(data) {
  if (!data || typeof data !== "object") return null;
  const { type } = data;
  if (!INCOMING.includes(type)) return null;
  if (type === "graph") return { type, graph: data.graph ?? null };
  if (type === "layout") return { type, text: typeof data.text === "string" ? data.text : "" };
  if (type === "cursor") {
    // Позиция принимается числом, а не приводится к нему: `null` приводится к
    // нулю, а строка - к своему числу, и курсор уехал бы по чужому сообщению.
    const { line, character } = data;
    if (!Number.isInteger(line) || !Number.isInteger(character)) return null;
    return { type, line, character };
  }
  return { type, lang: typeof data.lang === "string" ? data.lang : "" };
}

/** Сообщение наружу: раскладка изменилась. */
export function layoutMessage(text) {
  return { type: "layout", text };
}

/**
 * Сообщение наружу: выбран узел.
 *
 * Позиция берётся у узла графа, а не у холста: курсор ставит редактор, и
 * координаты ему нужны в единицах протокола.
 */
export function selectMessage(node) {
  const range = node?.nameRange ?? node?.range ?? null;
  if (!range) return null;
  return { type: "select", line: range.start_line, character: range.start_character };
}

/** Панель готова принимать данные. */
export function readyMessage() {
  return { type: "ready" };
}

/**
 * Подключает холст к окну хоста.
 *
 * @param {object} dom узлы страницы
 * @param {object} options `post` - как отправить наружу, `subscribe` - как слушать
 */
export function attach(dom, options = {}) {
  const post = options.post ?? ((message) => window.parent?.postMessage(message, "*"));
  const subscribe = options.subscribe ?? ((handler) => window.addEventListener("message", (e) => handler(e.data)));

  const scheme = new Scheme(dom, {
    t,
    // Правку раскладки пишет хост: файл лежит рядом с моделью, а файловой
    // системы у страницы нет.
    onChange: () => post(layoutMessage(scheme.text())),
    // Щелчок по узлу ставит курсор в редакторе - та же синхронизация, что на
    // странице, только редактор чужой.
    onSelect: (node) => {
      const message = selectMessage(node);
      if (message) post(message);
    },
    who: () => options.who ?? "",
  });

  subscribe((data) => {
    const message = parseIncoming(data);
    if (!message) return;
    // Признак успеха у моста браузера лежит в самом ответе (`ok`), а у сервера
    // отказ приходит кодом протокола - до холста доезжает либо граф, либо
    // ничего. Признак ставится здесь, чтобы у графа осталась одна форма.
    if (message.type === "graph") scheme.setGraph(message.graph ? { ok: true, ...message.graph } : null);
    else if (message.type === "layout") scheme.setLayout(layoutFile.parse(message.text).layout);
    else if (message.type === "cursor") scheme.highlight(message.line, message.character);
    else if (message.type === "lang") applyLanguage(message.lang);
  });

  post(readyMessage());
  return scheme;
}

/** Смена языка оболочки: словарь тот же, что у страницы. */
async function applyLanguage(lang) {
  await i18n.load(lang);
  i18n.apply(document);
}

/** Точка входа режима: узлы берутся из собранной разметки холста. */
export async function main() {
  const dom = {};
  for (const id of [
    "scheme", "sheet", "map", "stage", "legend", "crumbs", "scheme-up", "nav",
    "scheme-empty", "scheme-notice", "scheme-notice-text", "scheme-drop", "zoom",
    "scheme-modal", "scheme-tabs", "scheme-settings", "scheme-save", "scheme-cancel",
    "panel-run", "panel-view", "panel-sheet", "legendrows", "legendcols",
  ]) {
    dom[id] = document.getElementById(id);
  }
  await i18n.load(i18n.pick(i18n.stored(localStorage), navigator.languages ?? []));
  i18n.apply(document);
  tip.attach(document);

  const docks = {};
  for (const node of document.querySelectorAll("[data-dock]")) docks[node.dataset.dock] = node;

  return attach(
    {
      scheme: dom.scheme,
      sheet: dom.sheet,
      map: dom.map,
      stage: dom.stage,
      legend: dom.legend,
      crumbs: dom.crumbs,
      crumbsUp: dom["scheme-up"],
      nav: dom.nav,
      tools: dom.scheme,
      empty: dom["scheme-empty"],
      notice: dom["scheme-notice"],
      noticeText: dom["scheme-notice-text"],
      noticeDrop: dom["scheme-drop"],
      zoom: dom.zoom,
      settingsModal: dom["scheme-modal"],
      settingsTabs: dom["scheme-tabs"],
      settingsBody: dom["scheme-settings"],
      settingsSave: dom["scheme-save"],
      settingsCancel: dom["scheme-cancel"],
      panels: {
        run: dom["panel-run"],
        view: dom["panel-view"],
        sheet: dom["panel-sheet"],
        legend: dom.legend,
      },
      docks,
      legendSplits: [dom.legendrows, dom.legendcols],
    },
    {},
  );
}

/**
 * Справка: описание языка из документа `book/` поверх рабочего места.
 *
 * Документ собирает Typst выгрузкой HTML при сборке страницы, фрагмент готовит
 * `scripts/build-web-help.py`. Читается он по первому открытию окна: мегабайт
 * текста ради кнопки, которую нажимают не каждый заход, при загрузке страницы не
 * нужен. Оглавление строится по заголовкам документа; раздел, на котором читатель
 * остановился, помнит память страницы, и окно открывается на нём; поиск
 * подсвечивает вхождения и ведёт по ним.
 */
import { t } from "./i18n.js";
import * as shell from "./shell.js";

/** Короче - не ищем: одна буква нашлась бы в каждом слове. */
const QUERY_MIN = 2;
/** Сколько вхождений подсвечивается: дальше строку поиска стоит уточнить. */
const HIT_LIMIT = 2000;
/** Раздел читателя - тот, чей заголовок поднялся выше этой отметки окна, пикселей. */
const SECTION_MARK = 24;

/**
 * Вхождения строки поиска в тексте без учёта регистра: пары `[начало, конец)`,
 * без перекрытий.
 *
 * @param {string} text
 * @param {string} query
 * @returns {number[][]}
 */
export function findRanges(text, query) {
  const needle = query.toLocaleLowerCase();
  const out = [];
  if (needle.length < QUERY_MIN) return out;
  const hay = text.toLocaleLowerCase();
  for (let at = hay.indexOf(needle); at !== -1; at = hay.indexOf(needle, at + needle.length)) {
    out.push([at, at + needle.length]);
  }
  return out;
}

/**
 * Раздел, который читает человек: последний заголовок, поднявшийся выше отметки.
 * Выше первого заголовка - первый раздел.
 *
 * @param {{id: string, top: number}[]} tops заголовки по порядку, верх от края окна
 * @param {number} mark отметка от края окна
 * @returns {string|null}
 */
export function sectionAt(tops, mark) {
  let found = null;
  for (const item of tops) {
    if (item.top > mark) break;
    found = item.id;
  }
  return found ?? tops[0]?.id ?? null;
}

/**
 * Подключает окно справки: кнопку открытия, оглавление, поиск и память раздела.
 *
 * @param {Record<string, HTMLElement>} nodes узлы страницы по `id` (`cache()` страницы)
 * @param {{storage?: Storage}} options
 */
export function attachHelp(nodes, { storage = globalThis.localStorage } = {}) {
  const root = globalThis.document;
  const dom = {
    button: nodes.showhelp,
    help: nodes.help,
    search: nodes["help-search"],
    count: nodes["help-count"],
    prev: nodes["help-prev"],
    next: nodes["help-next"],
    close: nodes["help-close"],
    toc: nodes["help-toc"],
    doc: nodes["help-doc"],
  };
  if (!dom.button || !dom.help) return null;
  let loading = null;
  let heads = [];
  /** Пункт оглавления каждого заголовка: у подразделов - пункт их раздела. */
  const owner = new Map();
  let hits = [];
  let current = -1;

  function load() {
    loading ??= fetch(new URL("help.html", import.meta.url))
      .then((reply) => {
        if (!reply.ok) throw new Error(`HTTP ${reply.status}`);
        return reply.text();
      })
      .then(paint)
      .catch((error) => {
        loading = null;
        throw error;
      });
    return loading;
  }

  function paint(text) {
    const parsed = new DOMParser().parseFromString(`<body>${text}</body>`, "text/html");
    dom.doc.replaceChildren(...[...parsed.body.childNodes].map((child) => root.importNode(child, true)));
    heads = [...dom.doc.querySelectorAll("h2[id], h3[id], h4[id], h5[id]")];
    const items = [];
    let last = null;
    for (const head of heads) {
      if (head.tagName === "H2" || head.tagName === "H3") {
        const item = root.createElement("button");
        item.type = "button";
        item.className = `help-toc-item help-toc-${head.tagName.toLowerCase()}`;
        item.textContent = head.textContent;
        item.addEventListener("click", () => go(head.id));
        items.push(item);
        last = item;
      }
      owner.set(head.id, last);
    }
    dom.toc.replaceChildren(...items);
  }

  /** Ведёт текст к заголовку; `false` - такого заголовка нет. */
  function go(id) {
    const target = id ? dom.doc.querySelector(`[id="${CSS.escape(id)}"]`) : null;
    if (!target) return false;
    dom.doc.scrollTop += target.getBoundingClientRect().top - dom.doc.getBoundingClientRect().top;
    show(id);
    return true;
  }

  /** Отмечает раздел в оглавлении и запоминает его. */
  function show(id) {
    const item = owner.get(id);
    for (const other of dom.toc.querySelectorAll("[aria-current]")) other.removeAttribute("aria-current");
    if (item) {
      item.setAttribute("aria-current", "true");
      const box = dom.toc.getBoundingClientRect();
      const at = item.getBoundingClientRect();
      if (at.top < box.top || at.bottom > box.bottom) dom.toc.scrollTop += at.top - box.top - box.height / 3;
    }
    shell.remember(storage, shell.UI_KEYS.helpAt, id);
  }

  let spying = 0;
  dom.doc.addEventListener("scroll", () => {
    if (spying || dom.help.hidden) return;
    spying = requestAnimationFrame(() => {
      spying = 0;
      const base = dom.doc.getBoundingClientRect().top;
      const id = sectionAt(heads.map((head) => ({ id: head.id, top: head.getBoundingClientRect().top - base })), SECTION_MARK);
      if (id) show(id);
    });
  });

  // Ссылки документа на свои разделы ведут внутри окна: адрес страницы несёт
  // ссылку на модель, и переход по якорю перезаписал бы её.
  dom.doc.addEventListener("click", (event) => {
    const link = event.target.closest?.('a[href^="#"]');
    if (!link) return;
    event.preventDefault();
    const raw = link.getAttribute("href").slice(1);
    let id = raw;
    try {
      id = decodeURIComponent(raw);
    } catch {
      // Якорь без процентной записи - как есть.
    }
    go(id);
  });

  function clearHits() {
    for (const hit of hits) {
      const parent = hit.parentNode;
      if (!parent) continue;
      parent.replaceChild(root.createTextNode(hit.textContent), hit);
      parent.normalize();
    }
    hits = [];
    current = -1;
  }

  async function search() {
    const query = dom.search.value.trim();
    if (!heads.length) {
      try {
        await load();
      } catch {
        return;
      }
    }
    clearHits();
    if (query.length < QUERY_MIN) {
      dom.count.textContent = "";
      return;
    }
    const walker = root.createTreeWalker(dom.doc, NodeFilter.SHOW_TEXT);
    const texts = [];
    while (walker.nextNode()) texts.push(walker.currentNode);
    for (const text of texts) {
      const ranges = findRanges(text.data, query);
      if (!ranges.length) continue;
      const found = [];
      // С конца: разрез не сдвигает начала вхождений левее.
      for (let i = ranges.length - 1; i >= 0; i -= 1) {
        const [from, to] = ranges[i];
        const piece = text.splitText(from);
        piece.splitText(to - from);
        const hit = root.createElement("mark");
        hit.className = "help-hit";
        piece.parentNode.replaceChild(hit, piece);
        hit.appendChild(piece);
        found.unshift(hit);
      }
      hits.push(...found);
      if (hits.length >= HIT_LIMIT) break;
    }
    if (!hits.length) {
      dom.count.textContent = t("help.none");
      return;
    }
    step(1);
  }

  /** Переходит к следующему (`1`) либо предыдущему (`-1`) вхождению. */
  function step(delta) {
    if (!hits.length) return;
    hits[current]?.classList.remove("current");
    current = (current + delta + hits.length) % hits.length;
    const hit = hits[current];
    hit.classList.add("current");
    const box = dom.doc.getBoundingClientRect();
    dom.doc.scrollTop += hit.getBoundingClientRect().top - box.top - box.height / 3;
    dom.count.textContent = t("help.found", { index: current + 1, count: hits.length });
  }

  async function open() {
    dom.help.hidden = false;
    dom.button.setAttribute("aria-pressed", "true");
    dom.search.focus();
    if (!heads.length) {
      dom.doc.textContent = t("help.loading");
      try {
        await load();
      } catch (error) {
        dom.doc.textContent = t("help.missing", { error: error?.message ?? String(error) });
        return;
      }
    }
    if (!go(shell.setting(storage, shell.UI_KEYS.helpAt, ""))) dom.doc.scrollTop = 0;
  }

  function close() {
    dom.help.hidden = true;
    dom.button.setAttribute("aria-pressed", "false");
    dom.button.focus();
  }

  let pending = 0;
  dom.button.addEventListener("click", () => (dom.help.hidden ? open() : close()));
  dom.close.addEventListener("click", close);
  dom.prev.addEventListener("click", () => step(-1));
  dom.next.addEventListener("click", () => step(1));
  dom.search.addEventListener("input", () => {
    clearTimeout(pending);
    pending = setTimeout(search, 250);
  });
  dom.search.addEventListener("keydown", (event) => {
    if (event.key === "Enter") {
      event.preventDefault();
      if (!hits.length) search();
      else step(event.shiftKey ? -1 : 1);
    } else if (event.key === "Escape" && dom.search.value) {
      event.preventDefault();
      event.stopPropagation();
      dom.search.value = "";
      clearHits();
      dom.count.textContent = "";
    }
  });
  root.addEventListener("keydown", (event) => {
    if (dom.help.hidden) return;
    if (event.key === "Escape") {
      event.preventDefault();
      close();
    } else if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === "f") {
      // Поиск браузера искал бы по странице под окном - ищем по справке.
      event.preventDefault();
      dom.search.focus();
      dom.search.select();
    }
  });
  return { open, close };
}

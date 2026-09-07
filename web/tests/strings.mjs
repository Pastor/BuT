// Поиск текста мимо словаря (фича, задача 10a).
//
// # Предмет
//
// Строка оболочки, написанная в коде, не переводится **никогда** и не
// обнаруживается ничем: страница выглядит рабочей, а одна подпись остаётся на
// чужом языке. Класс тот же, что у второго словаря ключевых слов (0232), и
// лечится он так же - машиной, падающей списком.
//
// # Почему свой сканер, а не регулярное выражение
//
// Кириллица в `web/static` есть везде: комментарии проекта пишутся по-русски
// (правило языка общения). Отличить комментарий от строкового литерала грепом
// нельзя, а разбирать JavaScript зависимостью - нельзя тем более: фронтенд без
// зависимостей (решение заказчика). Отсюда посимвольный сканер состояний:
// код -> строка -> шаблон -> комментарий. Файлов немного, и они наши.

/** Кириллица - признак текста оболочки: своих языков у страницы два. */
const CYRILLIC = /[Ѐ-ӿ]/;

/**
 * Находит строковые литералы JavaScript с кириллицей.
 *
 * @param {string} source текст модуля
 * @returns {{line: number, text: string}[]} находки
 */
export function literalsWithText(source) {
  const found = [];
  let at = 0;
  let line = 1;
  const advance = (count) => {
    for (let i = 0; i < count; i += 1) if (source[at + i] === "\n") line += 1;
    at += count;
  };

  while (at < source.length) {
    const ch = source[at];
    const next = source[at + 1];

    if (ch === "/" && next === "/") {
      const end = source.indexOf("\n", at);
      advance((end === -1 ? source.length : end) - at);
      continue;
    }
    if (ch === "/" && next === "*") {
      const end = source.indexOf("*/", at + 2);
      advance((end === -1 ? source.length : end + 2) - at);
      continue;
    }
    if (ch === '"' || ch === "'" || ch === "`") {
      const start = at;
      const startLine = line;
      advance(1);
      while (at < source.length) {
        if (source[at] === "\\") {
          advance(2);
          continue;
        }
        if (source[at] === ch) {
          advance(1);
          break;
        }
        advance(1);
      }
      const text = source.slice(start, at);
      if (CYRILLIC.test(text)) found.push({ line: startLine, text: text.trim() });
      continue;
    }
    advance(1);
  }
  return found;
}

/**
 * Находит текстовые узлы разметки с кириллицей и без `data-i18n`.
 *
 * Разбор нарочно грубый - по тегам, а не по дереву: предмет проверки в том,
 * что у текста есть ключ, а не в устройстве DOM.
 */
export function nodesWithoutKey(html) {
  const found = [];
  const withoutComments = blank(html, /<!--[\s\S]*?-->/g);
  // Тело `script` и `style` - код, а не текст страницы: его комментарии
  // по-русски по правилу языка проекта. Скрипт проверяется как JavaScript
  // (см. [`inlineScripts`]), а здесь гасится, иначе каждая его строка
  // выглядела бы подписью без ключа.
  const withoutCode = blank(withoutComments, /<(script|style)\b[^>]*>[\s\S]*?<\/\1>/gi);
  const tag = /<([a-zA-Z][\w-]*)([^>]*)>([^<]*)/g;
  let match;
  while ((match = tag.exec(withoutCode)) !== null) {
    const [, name, attributes, text] = match;
    if (!CYRILLIC.test(text)) continue;
    // `meta` и `og:*` - предпросмотр ссылки: он на одном языке (решение
    // проработки задачи 10), и ключа у него нет.
    if (name === "meta") continue;
    if (/\bdata-i18n\b/.test(attributes)) continue;
    const line = withoutCode.slice(0, match.index).split("\n").length;
    found.push({ line, text: text.trim() });
  }
  // Кириллица в значениях атрибутов без `data-i18n-attr`: `aria-label`,
  // `placeholder`, `title` - такой же текст оболочки, как подпись.
  const open = /<([a-zA-Z][\w-]*)([^>]*)>/g;
  while ((match = open.exec(withoutCode)) !== null) {
    const [, name, attributes] = match;
    if (name === "meta" || !CYRILLIC.test(attributes)) continue;
    if (/\bdata-i18n-attr\b/.test(attributes)) continue;
    const line = withoutCode.slice(0, match.index).split("\n").length;
    found.push({ line, text: `<${name} ${attributes.trim()}>` });
  }
  return found;
}

/** Гасит совпадения, сохраняя переводы строк: номера строк не должны съехать. */
function blank(text, pattern) {
  return text.replace(pattern, (match) => match.replace(/[^\n]/g, " "));
}

/**
 * Тела встроенных `<script>` разметки - с номером первой строки каждого.
 *
 * Встроенный скрипт - такой же код страницы, как модуль: строку оболочки в нём
 * не видно ничем, кроме этой проверки.
 */
export function inlineScripts(html) {
  const found = [];
  const pattern = /<script\b[^>]*>([\s\S]*?)<\/script>/gi;
  let match;
  while ((match = pattern.exec(html)) !== null) {
    const line = html.slice(0, match.index).split("\n").length;
    found.push({ line, source: match[1] });
  }
  return found;
}

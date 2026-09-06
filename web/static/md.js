/**
 * Разбор Markdown: подсветка исходника и безопасный рендер (задача `0531-09n`).
 *
 * # Почему свой разбор
 *
 * Основание то же, что у JSON сценария (`json.js`): подсветку Takt задаёт
 * МОДУЛЬ, потому что второй словарь языка разошёлся бы с лексером молча. У
 * Markdown второго носителя в проекте нет вовсе — расходиться не с чем, а
 * подключить чужую библиотеку нельзя: фронтенд без зависимостей (решение
 * заказчика). Разбор здесь один на обе работы — подсветку и рендер: заведи их
 * порознь, и заголовок красился бы заголовком, а показывался абзацем.
 *
 * # Что разбирается, а что нет
 *
 * Заголовки, абзацы, цитаты, списки (маркированные и нумерованные), блоки кода
 * с оградой, таблицы с вертикальной чертой, горизонтальная черта; внутри строки
 * — код, выделение, ссылки. ⚠️ КАРТИНОК НЕТ намеренно: показ картинки означает
 * загрузку по чужому адресу со страницы читателя, то есть обращение к чужому
 * серверу от его имени. Это названная граница, а не забытая возможность.
 *
 * # Безопасность
 *
 * Текст пишет автор проекта, а видят его читатели витрины. Поэтому рендер
 * СТРОИТ УЗЛЫ (`createElement`, `textContent`), и `innerHTML` в модуле нет ни
 * разу: разметка из данных — это чужой сценарий, исполняемый в чужом браузере.
 * Адрес ссылки пропускается только знакомой схемы (см. [`safeHref`]).
 */

/** Роли подсветки: те же `--tok-*`, что у модуля и у JSON. */
const ROLE = {
  heading: "keyword",
  marker: "operator",
  code: "string",
  strong: "type",
  em: "type",
  link: "function",
  url: "string",
  quote: "comment",
  fence: "operator",
};

/**
 * Разбирает текст в список блоков.
 *
 * Блок несёт СТРОКИ, из которых сложен, и разобранные отрезки строки: этого
 * хватает и рендеру (что показать), и подсветке (что покрасить).
 *
 * @param {string} text текст файла
 * @returns {object[]} блоки в порядке следования
 */
export function parse(text) {
  const lines = String(text ?? "").split("\n");
  const blocks = [];
  let at = 0;
  while (at < lines.length) {
    const line = lines[at];
    if (line.trim() === "") {
      at += 1;
      continue;
    }
    // Ограда записана кодами (`\x60` - обратная кавычка): посимвольный сканер текста
    // мимо словаря принимает три кавычки в регулярном выражении за начало шаблонной
    // строки, и следующий комментарий по-русски становится для него строкой оболочки.
    // Запись кодом читается так же, а сканеру однозначна.
    const fence = /^\s*(\x60{3}|~{3})(.*)$/.exec(line);
    if (fence) {
      // Незакрытая ограда кончается вместе с файлом, а не роняет разбор: текст
      // красится, пока его набирают, то есть почти всегда недописанным.
      const mark = fence[1];
      const body = [];
      let end = at + 1;
      while (end < lines.length && !lines[end].trim().startsWith(mark)) {
        body.push(lines[end]);
        end += 1;
      }
      blocks.push({
        type: "code",
        lang: fence[2].trim(),
        from: at,
        to: Math.min(end, lines.length - 1),
        lines: body,
      });
      at = end + 1;
      continue;
    }
    const heading = /^(#{1,6})\s+(.*)$/.exec(line);
    if (heading) {
      blocks.push({
        type: "heading",
        level: heading[1].length,
        line: at,
        marker: { line: at, column: 0, length: heading[1].length },
        inline: inline(heading[2], at, line.length - heading[2].length),
      });
      at += 1;
      continue;
    }
    if (/^\s*(-{3,}|\*{3,}|_{3,})\s*$/.test(line)) {
      blocks.push({ type: "rule", line: at, marker: { line: at, column: 0, length: line.length } });
      at += 1;
      continue;
    }
    if (/^\s*>/.test(line)) {
      const rows = [];
      while (at < lines.length && /^\s*>/.test(lines[at])) {
        const cut = lines[at].indexOf(">") + 1;
        rows.push({
          marker: { line: at, column: 0, length: cut },
          inline: inline(lines[at].slice(cut).replace(/^ /, ""), at, cut + (lines[at][cut] === " " ? 1 : 0)),
        });
        at += 1;
      }
      blocks.push({ type: "quote", rows });
      continue;
    }
    const item = listItem(line);
    if (item) {
      const ordered = item.ordered;
      const items = [];
      while (at < lines.length) {
        const parsed = listItem(lines[at]);
        if (!parsed || parsed.ordered !== ordered) break;
        items.push({
          marker: { line: at, column: parsed.column, length: parsed.length },
          inline: inline(parsed.text, at, parsed.column + parsed.length),
        });
        at += 1;
      }
      blocks.push({ type: "list", ordered, items });
      continue;
    }
    if (isTableRow(line) && at + 1 < lines.length && isTableRule(lines[at + 1])) {
      const header = cells(line, at);
      const rows = [];
      let end = at + 2;
      while (end < lines.length && isTableRow(lines[end])) {
        rows.push(cells(lines[end], end));
        end += 1;
      }
      blocks.push({ type: "table", header, rows, from: at, to: end - 1, rule: at + 1 });
      at = end;
      continue;
    }
    // Абзац: строки подряд до пустой либо до начала другого блока.
    const rows = [];
    while (at < lines.length && lines[at].trim() !== "" && !starts(lines[at])) {
      rows.push({ line: at, inline: inline(lines[at], at, 0) });
      at += 1;
    }
    blocks.push({ type: "paragraph", rows });
  }
  return blocks;
}

/** Начинает ли строка блок иного рода (нужно, чтобы оборвать абзац). */
function starts(line) {
  return (
    /^\s*(\x60{3}|~{3})/.test(line) ||
    /^#{1,6}\s/.test(line) ||
    /^\s*>/.test(line) ||
    /^\s*(-{3,}|\*{3,}|_{3,})\s*$/.test(line) ||
    listItem(line) !== null
  );
}

/** Разбирает строку как элемент списка; `null` — не элемент. */
function listItem(line) {
  const bullet = /^(\s*)([-*+])\s+(.*)$/.exec(line);
  if (bullet) {
    return {
      ordered: false,
      column: bullet[1].length,
      length: bullet[2].length + 1,
      text: bullet[3],
    };
  }
  const numbered = /^(\s*)(\d+[.)])\s+(.*)$/.exec(line);
  if (numbered) {
    return {
      ordered: true,
      column: numbered[1].length,
      length: numbered[2].length + 1,
      text: numbered[3],
    };
  }
  return null;
}

/** Похожа ли строка на строку таблицы. */
function isTableRow(line) {
  return line.includes("|") && line.trim().length > 0;
}

/** Строка-разделитель шапки таблицы. */
function isTableRule(line) {
  return /^\s*\|?\s*:?-{1,}:?\s*(\|\s*:?-{1,}:?\s*)*\|?\s*$/.test(line);
}

/** Ячейки строки таблицы с их разобранным содержимым. */
function cells(line, index) {
  const trimmed = line.replace(/^\s*\|/, "").replace(/\|\s*$/, "");
  const out = [];
  let column = line.length - line.replace(/^\s*\|?/, "").length;
  for (const part of trimmed.split("|")) {
    out.push({ inline: inline(part.trim(), index, column + (part.length - part.trimStart().length)) });
    column += part.length + 1;
  }
  return out;
}

/**
 * Разбирает содержимое строки на отрезки.
 *
 * Отрезок несёт и ТЕКСТ (нужен рендеру), и МЕСТО (нужно подсветке): одна опись
 * на две работы.
 *
 * @param {string} text содержимое без признака блока
 * @param {number} line номер строки в файле
 * @param {number} offset сколько символов строки отрезано признаком блока
 */
function inline(text, line, offset) {
  const parts = [];
  let at = 0;
  let plain = "";
  const flush = () => {
    if (plain !== "") parts.push({ kind: "text", text: plain });
    plain = "";
  };
  while (at < text.length) {
    const rest = text.slice(at);
    // Кавычки записаны кодом по той же причине, что ограда выше.
    const code = /^\x60([^\x60]+)\x60/.exec(rest);
    if (code) {
      flush();
      parts.push({ kind: "code", text: code[1], line, column: offset + at, length: code[0].length });
      at += code[0].length;
      continue;
    }
    const strong = /^(\*\*|__)(.+?)\1/.exec(rest);
    if (strong) {
      flush();
      parts.push({ kind: "strong", text: strong[2], line, column: offset + at, length: strong[0].length });
      at += strong[0].length;
      continue;
    }
    const em = /^([*_])([^*_]+)\1/.exec(rest);
    if (em) {
      flush();
      parts.push({ kind: "em", text: em[2], line, column: offset + at, length: em[0].length });
      at += em[0].length;
      continue;
    }
    const link = /^\[([^\]]*)\]\(([^)\s]*)\)/.exec(rest);
    if (link) {
      flush();
      parts.push({
        kind: "link",
        text: link[1],
        url: link[2],
        line,
        column: offset + at,
        length: link[0].length,
      });
      at += link[0].length;
      continue;
    }
    plain += text[at];
    at += 1;
  }
  flush();
  return parts;
}

/**
 * Отрезки подсветки в том же виде, что у модуля (`line`, `column`, `length`,
 * `type`), — их раскладывает по строкам общий `paintCode`.
 *
 * @param {string} text текст файла
 * @returns {{line: number, column: number, length: number, type: string}[]}
 */
export function spans(text) {
  const marks = [];
  const put = (place, role) => {
    if (place && place.length > 0) {
      marks.push({ line: place.line, column: place.column, length: place.length, type: role });
    }
  };
  const inlines = (parts) => {
    for (const part of parts ?? []) {
      if (part.kind === "text") continue;
      put(part, ROLE[part.kind] ?? ROLE.marker);
    }
  };
  for (const block of parse(text)) {
    switch (block.type) {
      case "heading":
        put(block.marker, ROLE.heading);
        inlines(block.inline);
        break;
      case "rule":
        put(block.marker, ROLE.marker);
        break;
      case "code":
        // Ограда и всё, что внутри: тело - строка целиком, поэтому длина берётся у
        // самой строки, а не считается по отрезкам.
        for (let line = block.from; line <= block.to; line += 1) {
          marks.push({ line, column: 0, length: lineLength(text, line), type: ROLE.code });
        }
        break;
      case "quote":
        for (const row of block.rows) {
          put(row.marker, ROLE.quote);
          inlines(row.inline);
        }
        break;
      case "list":
        for (const item of block.items) {
          put(item.marker, ROLE.marker);
          inlines(item.inline);
        }
        break;
      case "table":
        for (let line = block.from; line <= block.to; line += 1) {
          const row = lineText(text, line);
          for (let column = 0; column < row.length; column += 1) {
            if (row[column] === "|") marks.push({ line, column, length: 1, type: ROLE.marker });
          }
        }
        marks.push({ line: block.rule, column: 0, length: lineLength(text, block.rule), type: ROLE.marker });
        break;
      default:
        for (const row of block.rows ?? []) inlines(row.inline);
    }
  }
  return marks;
}

/** Текст строки по номеру. */
function lineText(text, index) {
  return String(text ?? "").split("\n")[index] ?? "";
}

/** Длина строки по номеру. */
function lineLength(text, index) {
  return lineText(text, index).length;
}

/**
 * Строит дерево узлов для показа.
 *
 * ⚠️ Узлы СТРОЯТСЯ, а не собираются строкой: `innerHTML` из авторского текста
 * означал бы разметку из данных, то есть чужой сценарий в браузере читателя.
 *
 * @param {string} text текст файла
 * @param {Document} document_ документ, в котором создаются узлы
 * @returns {DocumentFragment} готовое поддерево
 */
export function render(text, document_) {
  const out = document_.createDocumentFragment();
  for (const block of parse(text)) {
    switch (block.type) {
      case "heading": {
        const node = document_.createElement(`h${Math.min(block.level + 1, 6)}`);
        fill(node, block.inline, document_);
        out.append(node);
        break;
      }
      case "rule":
        out.append(document_.createElement("hr"));
        break;
      case "code": {
        const pre = document_.createElement("pre");
        const code = document_.createElement("code");
        if (block.lang) code.dataset.lang = block.lang;
        code.textContent = block.lines.join("\n");
        pre.append(code);
        out.append(pre);
        break;
      }
      case "quote": {
        const node = document_.createElement("blockquote");
        const paragraph = document_.createElement("p");
        fill(paragraph, block.rows.flatMap((row, index) => spaced(row.inline, index)), document_);
        node.append(paragraph);
        out.append(node);
        break;
      }
      case "list": {
        const node = document_.createElement(block.ordered ? "ol" : "ul");
        for (const item of block.items) {
          const li = document_.createElement("li");
          fill(li, item.inline, document_);
          node.append(li);
        }
        out.append(node);
        break;
      }
      case "table": {
        const table = document_.createElement("table");
        const head = document_.createElement("thead");
        head.append(row(block.header, "th", document_));
        table.append(head);
        const body = document_.createElement("tbody");
        for (const line of block.rows) body.append(row(line, "td", document_));
        table.append(body);
        out.append(table);
        break;
      }
      default: {
        const node = document_.createElement("p");
        fill(node, block.rows.flatMap((line, index) => spaced(line.inline, index)), document_);
        out.append(node);
      }
    }
  }
  return out;
}

/** Строка таблицы из ячеек. */
function row(line, tag, document_) {
  const node = document_.createElement("tr");
  for (const cell of line) {
    const box = document_.createElement(tag);
    fill(box, cell.inline, document_);
    node.append(box);
  }
  return node;
}

/** Склеивает строки абзаца пробелом: перенос в Markdown разрыва не значит. */
function spaced(parts, index) {
  return index === 0 ? parts : [{ kind: "text", text: " " }, ...parts];
}

/** Наполняет узел разобранными отрезками строки. */
function fill(node, parts, document_) {
  for (const part of parts) {
    if (part.kind === "text") {
      node.append(document_.createTextNode(part.text));
      continue;
    }
    if (part.kind === "link") {
      const href = safeHref(part.url);
      if (href === null) {
        // Негодный адрес показывается текстом, а не пропадает: автор увидит, что ссылка
        // не сработала, вместо тихо исчезнувшего куска текста.
        node.append(document_.createTextNode(`${part.text} (${part.url})`));
        continue;
      }
      const link = document_.createElement("a");
      link.href = href;
      link.textContent = part.text || href;
      // Чужая страница открывается отдельно и без доступа к нашей.
      link.target = "_blank";
      link.rel = "noopener noreferrer nofollow";
      node.append(link);
      continue;
    }
    const tag = { code: "code", strong: "strong", em: "em" }[part.kind] ?? "span";
    const box = document_.createElement(tag);
    box.textContent = part.text;
    node.append(box);
  }
}

/**
 * Пропускает адрес ссылки, если его схема знакома; иначе `null`.
 *
 * ⚠️ Разрешён СПИСОК, а не запрет: запретить `javascript:` мало — есть `data:`,
 * `vbscript:` и записи с переносами внутри схемы. Список из трёх схем
 * проверяем целиком, и новая схема попадёт в него осознанно.
 */
export function safeHref(url) {
  const text = String(url ?? "").trim();
  if (text === "") return null;
  // Относительный адрес и якорь - свои, чужой схемы в них нет.
  if (text.startsWith("/") || text.startsWith("#") || text.startsWith("./")) return text;
  const scheme = /^([a-zA-Z][a-zA-Z0-9+.-]*):/.exec(text);
  if (!scheme) return text;
  return ["http", "https", "mailto"].includes(scheme[1].toLowerCase()) ? text : null;
}

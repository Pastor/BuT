// Редактор текста с подсветкой по токенам модуля.
//
// # Чего здесь нет
//
// Знания о языке Takt. Ни списка ключевых слов, ни разбора: цвет каждому отрезку
// назначает модуль (`takt_tokens` -> `bridge.spans`), а страница лишь раскладывает
// отрезки по строкам. Заведи здесь свой словарь - и подсветка начнёт расходиться с
// компилятором молча, как расходились параллельные списки у LSP.

import { spans } from "./bridge.js";

/** Редактор поверх `contenteditable`-узла. */
export class Editor {
  /**
   * @param {HTMLElement} root узел редактора
   * @param {() => void} onChange зовётся после правки текста
   */
  constructor(root, onChange) {
    this.root = root;
    this.onChange = onChange ?? (() => {});
    this.text = "";
    this.root.setAttribute("contenteditable", "true");
    this.root.setAttribute("spellcheck", "false");
    this.root.setAttribute("autocorrect", "off");
    this.root.setAttribute("autocapitalize", "off");

    this.root.addEventListener("input", () => {
      this.text = readText(this.root);
      this.onChange();
    });

    // Вставка идёт только текстом: иначе в редактор попадает разметка со
    // страницы-источника (цвета, шрифты, ссылки), и текст модели перестаёт быть
    // текстом.
    this.root.addEventListener("paste", (event) => {
      event.preventDefault();
      const text = event.clipboardData?.getData("text/plain") ?? "";
      document.execCommand("insertText", false, text);
    });

    // Tab - отступ, а не уход фокуса: редактор кода, в котором нельзя набрать отступ,
    // бесполезен. Уйти с него можно Escape + Tab.
    this.root.addEventListener("keydown", (event) => {
      if (event.key === "Tab") {
        event.preventDefault();
        document.execCommand("insertText", false, "    ");
      }
    });
  }

  /** Текущий текст документа. */
  value() {
    return this.text;
  }

  /** Заменяет текст целиком (открытие ссылки, форматирование, черновик). */
  setValue(text) {
    this.text = text;
    this.root.textContent = text;
    this.onChange();
  }

  /** Позиция курсора: строка и колонка с нуля, как в LSP. */
  position() {
    const offset = caretOffset(this.root);
    return offsetToPosition(this.text, offset);
  }

  /** Ставит курсор на позицию LSP и забирает фокус (переход к объявлению). */
  moveTo(line, character) {
    this.root.focus();
    setCaretOffset(this.root, positionToOffset(this.text, line, character));
  }

  /** Позиция документа под точкой экрана - для наведения мышью. */
  positionAt(x, y) {
    const document_ = this.root.ownerDocument;
    const spot = document_.caretRangeFromPoint?.(x, y) ?? document_.caretPositionFromPoint?.(x, y);
    if (!spot) return null;
    const node = spot.startContainer ?? spot.offsetNode;
    if (!this.root.contains(node)) return null;
    const offset = offsetAt(this.root, node, spot.startOffset ?? spot.offset);
    return offsetToPosition(this.text, offset);
  }

  /**
   * Красит текст отрезками из ответа `takt_tokens`.
   *
   * Курсор сохраняется по смещению в тексте: перестройка разметки уносит
   * узел, в котором он стоял, и без сохранения каждая покраска отправляла бы
   * курсор в начало документа - то есть печатать было бы нельзя.
   */
  /**
   * Красит текст ответом модуля.
   *
   * Готовые отрезки принимаются как есть (`{ marks: [...] }`): сценарий -
   * это JSON, и его размечает `json.js`, а не компилятор. Второго раскладчика
   * строк при этом не заводится - он один на все области кода.
   */
  highlight(tokens, diagnostics) {
    // Курсор возвращается только в редактор, в котором он и стоял: перекраска не должна
    // забирать фокус. Иначе смена языка (она перерисовывает всё) уводила бы фокус с
    // переключателя в текст - нашлось прогоном страницы.
    const focused = this.root.contains(this.root.ownerDocument.activeElement);
    const offset = caretOffset(this.root);
    const marks = Array.isArray(tokens?.marks) ? tokens.marks : spans(tokens ?? {});
    const problems = new Map();
    for (const d of diagnostics ?? []) {
      const line = d.range?.start_line ?? 0;
      if (!problems.has(line)) problems.set(line, d.severity ?? "error");
    }

    this.root.replaceChildren(paintCode(this.text, marks, problems));
    if (focused) setCaretOffset(this.root, offset);
  }
}

/**
 * Красит текст отрезками и отдаёт готовые строки-узлы.
 *
 * Общая точка для двух мест: исходник на Takt красится отрезками `takt_tokens`,
 * вывод цели - отрезками `takt_highlight`. Формы отрезков совпадают, и второй
 * раскладчик строк здесь был бы вторым носителем правил вёрстки кода.
 *
 * Отрезки раскладываются по строкам одним проходом. Фильтруй каждая строка весь
 * список (`marks.filter`), работа росла бы квадратично: на исходнике в 30 строк
 * это незаметно, а на выводе цели `c` - тысячи строк и тысячи отрезков, и
 * вкладка встала бы на секунды.
 *
 * @param {string} text текст документа
 * @param {{line: number, column: number, length: number, type: string}[]} marks отрезки
 * @param {Map<number, string>} [problems] строки с диагностикой: номер -> уровень
 * @returns {DocumentFragment} узлы-строки
 */
export function paintCode(text, marks, problems) {
  const lines = text.split("\n");
  const byLine = new Map();
  for (const mark of marks ?? []) {
    if (!byLine.has(mark.line)) byLine.set(mark.line, []);
    byLine.get(mark.line).push(mark);
  }

  const fragment = document.createDocumentFragment();
  for (let i = 0; i < lines.length; i += 1) {
    const lineNode = document.createElement("div");
    lineNode.className = "line";
    const severity = problems?.get(i);
    if (severity) lineNode.classList.add(`has-${severity}`);
    paintLine(lineNode, lines[i], byLine.get(i) ?? []);
    fragment.appendChild(lineNode);
  }
  return fragment;
}

/** Раскладывает отрезки токенов по одной строке. */
function paintLine(lineNode, text, marks) {
  if (text.length === 0) {
    // Пустая строка обязана занимать высоту: узел без содержимого браузер схлопывает, и
    // документ "прыгает" при наборе.
    lineNode.appendChild(document.createElement("br"));
    return;
  }
  let at = 0;
  for (const mark of marks.sort((a, b) => a.column - b.column)) {
    if (mark.column < at) continue;
    if (mark.column > at) {
      lineNode.appendChild(document.createTextNode(text.slice(at, mark.column)));
    }
    const end = Math.min(mark.column + mark.length, text.length);
    const span = document.createElement("span");
    span.className = `tok tok-${mark.type}`;
    span.textContent = text.slice(mark.column, end);
    lineNode.appendChild(span);
    at = end;
  }
  if (at < text.length) {
    lineNode.appendChild(document.createTextNode(text.slice(at)));
  }
}

/**
 * Текст узла редактора - с переводами строк.
 *
 * Берётся `textContent` узлов-строк, а не `innerText` корня: каждая строка -
 * блочный узел, и `innerText` добавляет к ней перевод сверх того, что есть в
 * тексте. Прогон страницы 2026-09-04 показал это прямо: документ из семи строк
 * возвращался четырнадцатью, и лишние переводы уезжали в ссылку и в черновик.
 */
function readText(root) {
  const lines = root.querySelectorAll(".line");
  if (lines.length === 0) {
    return root.innerText.replace(/\r\n?/g, "\n").replace(/\n$/, "");
  }
  return [...lines].map((line) => line.textContent).join("\n");
}

/**
 * Правило перевода "строка и место в ней ↔ смещение в тексте".
 *
 * Носитель один, и это главное здесь. Узлы-строки блочные: перевода строки
 * между ними в DOM нет, а в тексте он есть. Пока это правило считали по месту,
 * оно разъехалось: сохранение каретки не знало о переводах вовсе (после Enter
 * каретка возвращалась в конец прежней строки - то есть набирать текст было
 * нельзя), переход к объявлению считал так же, а наведение - иначе.
 *
 * @param {number[]} lengths длины строк документа
 * @param {number} index номер строки
 * @param {number} inLine место внутри строки
 */
export function offsetOfLine(lengths, index, inLine) {
  const line = Math.max(0, Math.min(index, lengths.length - 1));
  let offset = 0;
  for (let i = 0; i < line; i += 1) offset += lengths[i] + 1;
  return offset + Math.max(0, Math.min(inLine, lengths[line] ?? 0));
}

/** Обратное правило: смещение в тексте -> строка и место в ней. */
export function lineOfOffset(lengths, offset) {
  let left = Math.max(0, offset);
  for (let i = 0; i < lengths.length; i += 1) {
    if (left <= lengths[i]) return { index: i, inLine: left };
    left -= lengths[i] + 1;
  }
  const last = Math.max(0, lengths.length - 1);
  return { index: last, inLine: lengths[last] ?? 0 };
}

/** Длины строк редактора по его узлам. */
function lineLengths(lines) {
  return lines.map((line) => line.textContent.length);
}

/**
 * Смещение точки DOM в символах текста - с переводами строк.
 *
 * @param {HTMLElement} root узел редактора
 * @param {Node} container узел точки
 * @param {number} inNode место внутри узла
 */
export function offsetAt(root, container, inNode) {
  const lines = [...root.querySelectorAll(".line")];
  if (lines.length === 0) return inNode;
  // Точка на самом редакторе: место - это номер строки, перед которой она.
  if (container === root) return offsetOfLine(lineLengths(lines), Math.min(inNode, lines.length - 1), 0);

  const index = lines.findIndex((line) => line === container || line.contains(container));
  if (index < 0) return 0;
  const range = root.ownerDocument.createRange();
  range.selectNodeContents(lines[index]);
  range.setEnd(container, inNode);
  return offsetOfLine(lineLengths(lines), index, range.toString().length);
}

/**
 * Точка DOM для смещения в тексте: узел и место внутри него.
 *
 * Пустая строка - это `div` с одним `<br>`, и точка в ней - сам `div` с
 * местом 0: поставь её в `<br>`, и каретка окажется после строки.
 */
export function pointAt(root, offset) {
  const lines = [...root.querySelectorAll(".line")];
  if (lines.length === 0) return { node: root, offset: 0 };
  const { index, inLine } = lineOfOffset(lineLengths(lines), offset);
  const line = lines[index];
  const walker = root.ownerDocument.createTreeWalker(line, NodeFilter.SHOW_TEXT);
  let seen = 0;
  let node = walker.nextNode();
  while (node) {
    const length = node.textContent.length;
    if (seen + length >= inLine) return { node, offset: inLine - seen };
    seen += length;
    node = walker.nextNode();
  }
  return { node: line, offset: 0 };
}

/** Смещение курсора в символах от начала документа. */
function caretOffset(root) {
  const selection = root.ownerDocument.getSelection();
  if (!selection || selection.rangeCount === 0) return 0;
  const range = selection.getRangeAt(0);
  if (!root.contains(range.endContainer) && range.endContainer !== root) return 0;
  return offsetAt(root, range.endContainer, range.endOffset);
}

/** Ставит курсор на смещение в символах. */
function setCaretOffset(root, offset) {
  const selection = root.ownerDocument.getSelection();
  if (!selection) return;
  const point = pointAt(root, offset);
  const range = root.ownerDocument.createRange();
  range.setStart(point.node, point.offset);
  range.collapse(true);
  selection.removeAllRanges();
  selection.addRange(range);
}

/**
 * Переводит смещение в позицию LSP: строка и колонка с нуля.
 *
 * Экспортируется ради проверки в `node`: DOM там нет, а правило перевода
 * координат - есть, и ошибка в нём уводит наведение и переход на чужое место.
 */
export function offsetToPosition(text, offset) {
  const clamped = Math.max(0, Math.min(offset, text.length));
  const before = text.slice(0, clamped);
  const line = before.split("\n").length - 1;
  const lastBreak = before.lastIndexOf("\n");
  const character = clamped - (lastBreak + 1);
  return { line, character };
}

/** Обратный перевод: позиция LSP -> смещение в символах. */
export function positionToOffset(text, line, character) {
  const lines = text.split("\n");
  let offset = 0;
  for (let i = 0; i < Math.min(line, lines.length); i += 1) {
    offset += lines[i].length + 1;
  }
  return Math.min(offset + character, text.length);
}

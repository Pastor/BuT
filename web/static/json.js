/**
 * Разметка JSON для подсветки сценария (задача `0531-09l`).
 *
 * # Почему свой разбор, а знание о Takt — нет
 *
 * Подсветку кода на Takt и вывода целей задаёт МОДУЛЬ (`takt_tokens`,
 * `takt_highlight`): свой словарь в вебе разошёлся бы с лексером молча, и это
 * запрещено гейтом. С JSON случай другой: его грамматика задана RFC 8259 и
 * второго носителя в проекте у неё нет — расходиться не с чем. Сценарий
 * читается `serde_json` внутри модуля, но описи отрезков он не отдаёт, и
 * заводить ради подсветки ручку в компиляторе — цена больше пользы.
 *
 * ⚠️ Разбор ТЕРПИМЫЙ: сценарий подсвечивается, пока его набирают, то есть
 * почти всегда — недописанным. Незакрытая строка красится как строка до конца
 * строки; мусор остаётся без цвета, а не роняет разметку.
 *
 * Отрезки отдаются в том же виде, что у модуля (`line`, `column`, `length`,
 * `type`), — их раскладывает по строкам общий `paintCode`.
 */

/** Роли, которыми красится JSON. Соответствуют `--tok-*` дизайн-системы. */
const KEY = "type";
const STRING = "string";
const NUMBER = "number";
const CONST = "constant";
const PUNCT = "operator";

/**
 * Отрезки подсветки для текста JSON.
 *
 * @param {string} text текст сценария
 * @returns {{line: number, column: number, length: number, type: string}[]}
 */
export function spans(text) {
  const marks = [];
  const lines = String(text ?? "").split("\n");
  for (let line = 0; line < lines.length; line += 1) {
    let at = 0;
    const row = lines[line];
    while (at < row.length) {
      const ch = row[at];
      if (ch === '"') {
        const end = closing(row, at);
        // Имя поля от значения отличает двоеточие следом - не разбор, а тот же признак,
        // по которому его читает человек.
        const after = row.slice(end + 1).match(/^\s*:/);
        marks.push({ line, column: at, length: end - at + 1, type: after ? KEY : STRING });
        at = end + 1;
        continue;
      }
      const rest = row.slice(at);
      const number = rest.match(/^-?\d+(\.\d+)?([eE][+-]?\d+)?/);
      if (number) {
        marks.push({ line, column: at, length: number[0].length, type: NUMBER });
        at += number[0].length;
        continue;
      }
      const word = rest.match(/^(true|false|null)\b/);
      if (word) {
        marks.push({ line, column: at, length: word[0].length, type: CONST });
        at += word[0].length;
        continue;
      }
      if ("{}[],:".includes(ch)) {
        marks.push({ line, column: at, length: 1, type: PUNCT });
      }
      at += 1;
    }
  }
  return marks;
}

/**
 * Конец строкового литерала с учётом экранирования; незакрытая строка
 * кончается вместе со строкой текста.
 */
function closing(row, from) {
  for (let i = from + 1; i < row.length; i += 1) {
    if (row[i] === "\\") {
      i += 1;
      continue;
    }
    if (row[i] === '"') return i;
  }
  return row.length - 1;
}

/**
 * Разметка JSON для подсветки сценария. Свой разбор здесь законен, а
 * знание о языке Takt - нет:
 *
 * Подсветку кода на Takt и вывода целей задаёт модуль (`takt_tokens`,
 * `takt_highlight`): свой словарь в вебе разошёлся бы с лексером молча, и это
 * запрещено проверкой. С JSON случай другой: его грамматика задана RFC 8259 и
 * второго носителя в проекте у неё нет - расходиться не с чем. Сценарий
 * читается `serde_json` внутри модуля, но описи отрезков он не отдаёт, и
 * заводить ради подсветки ручку в компиляторе - цена больше пользы.
 *
 * Разбор терпимый: сценарий подсвечивается, пока его набирают, то есть
 * почти всегда - недописанным. Незакрытая строка красится как строка до конца
 * строки; мусор остаётся без цвета, а не роняет разметку.
 *
 * Отрезки отдаются в том же виде, что у модуля (`line`, `column`, `length`,
 * `type`), - их раскладывает по строкам общий `paintCode`.
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

/**
 * Канон сценария: печать JSON с отступом в два пробела.
 *
 * Разбор здесь **строгий**, в отличие от разметки: подсветить недописанное
 * можно, а напечатать - нет. Печать по половине разбора потеряла бы то, что
 * автор ещё набирает, и потеряла бы молча, поэтому неразбираемый текст
 * возвращается отказом с текстом причины от самого разбора.
 *
 * @param {string} text текст сценария
 * @returns {{ok: true, text: string} | {ok: false, error: string}}
 */
export function format(text) {
  let value;
  try {
    value = JSON.parse(String(text ?? ""));
  } catch (error) {
    return { ok: false, error: String(error?.message ?? error) };
  }
  // Хвостовой перевод строки - как у файла на диске: без него канон менял бы
  // файл при каждом сохранении через инструмент, который его ставит.
  return { ok: true, text: `${JSON.stringify(value, null, 2)}\n` };
}

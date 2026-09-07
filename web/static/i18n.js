// Язык оболочки: словарь, подстановки, выбор языка.
//
// # Что переводится, а что нет
//
// Текстов на странице три рода, и переводится здесь только первый:
//
//   1. **оболочка** - подписи, кнопки, сообщения страницы: словарь ниже;
//   2. **тексты модуля** - диагностики, трасса, сводка прогона: они приходят
//      из `takt-lang` и `takt-sim` и сегодня существуют только по-русски.
//      Переводить их здесь словарём "код -> текст" нельзя: у многих кодов больше
//      одного места эмиссии, и второй носитель разошёлся бы с первым молча. Язык
//      сообщений инструментов задаёт сам инструмент;
//   3. **данные автора** - исходник модели и сценарий входов: не переводятся
//      никогда и ни при каких условиях.
//
// # Чего здесь нет
//
// Форм множественного числа. У русского их три, у английского две, и механизм выбора
// формы - отдельный предмет. Вместо него строки **сформулированы** так, что число стоит
// последним ("переименовано вхождений: 3"): приём дешевле механизма и не врёт ни на
// одном числе.

/** Базовый язык: на него падает ключ, которого нет в выбранном словаре. */
export const BASE = "ru";

/**
 * Языки выпуска и их самоназвания.
 *
 * Самоназвание не переводится: "Русский" остаётся "Русский" в английской
 * оболочке - так читатель находит свой язык, не зная текущего.
 * Список сверяется с составом `web/static/i18n/` проверкой: язык, у которого
 * нет словаря (или словарь без записи здесь), - отказ, а не пустая строка.
 */
export const LANGUAGES = {
  ru: "Русский",
  en: "English",
};

/**
 * Короткие метки языков - для кнопки в шапке.
 *
 * Метка - это код языка, а не перевод: "RU" читается одинаково в любой
 * оболочке, и правило "самоназвание не переводится" ею не нарушается. Полное
 * самоназвание остаётся в списке - по двум буквам свой язык узнают не все.
 */
export const SHORT = {
  ru: "RU",
  en: "EN",
};

/** Ключ хранилища выбранного языка. */
export const KEY = "takt.lang";

let current = BASE;
let dictionary = {};
let fallback = {};

/** Текущий язык оболочки. */
export function language() {
  return current;
}

/**
 * Ставит словари напрямую.
 *
 * Отдельно от [`load`], потому что проверки в `node` читают словари с диска, а
 * `fetch` относительного пути там не работает: механизм подстановки и падения
 * на базовый язык обязан проверяться без браузера.
 */
export function use(lang, dict, baseDict) {
  current = lang;
  dictionary = dict ?? {};
  fallback = baseDict ?? dictionary;
}

/**
 * Загружает словарь языка и базовый - параллельно.
 *
 * Базовый грузится всегда: он и есть падение для ключа, которого в выбранном
 * словаре не оказалось.
 */
export async function load(lang, fetchJson = defaultFetch) {
  const chosen = LANGUAGES[lang] ? lang : BASE;
  const [dict, base] = await Promise.all([
    fetchJson(chosen),
    chosen === BASE ? Promise.resolve(null) : fetchJson(BASE),
  ]);
  use(chosen, dict, base ?? dict);
  return chosen;
}

async function defaultFetch(lang) {
  // Адрес считается от этого модуля, а не от страницы: собранная статика лежит в
  // каталоге бандла `b/<отпечаток>/`, и адрес от документа увёл бы запрос в корень.
  const response = await fetch(new URL(`i18n/${lang}.json`, import.meta.url));
  if (!response.ok) throw new Error(`словарь ${lang}: ${response.status}`);
  return response.json();
}

/**
 * Строка по ключу с подстановками `{имя}`.
 *
 * Порядок поиска: выбранный язык -> базовый -> **сам ключ**. Последнее в работе
 * недостижимо (паритет словарей держит проверка) и оставлено намеренно: пустая
 * кнопка хуже кнопки с надписью `bar.format` - по ней хотя бы понятно, что
 * сломалось.
 */
export function t(key, params) {
  const template = dictionary[key] ?? fallback[key] ?? key;
  return format(template, params);
}

/** Подставляет `{имя}` значениями. Отсутствующее имя остаётся как есть. */
export function format(template, params) {
  if (!params) return template;
  return template.replace(/\{(\w+)\}/g, (whole, name) =>
    Object.hasOwn(params, name) ? String(params[name]) : whole
  );
}

/**
 * Выбирает язык: сохранённый -> язык браузера -> базовый.
 *
 * Язык браузера приходит в виде `en-GB`, `ru-RU`: сравнивается **первая
 * часть**. Точное совпадение потребовало бы словаря на каждый регион.
 */
export function pick(stored, preferred = []) {
  if (stored && LANGUAGES[stored]) return stored;
  for (const tag of preferred) {
    const short = String(tag).toLowerCase().split("-")[0];
    if (LANGUAGES[short]) return short;
  }
  return BASE;
}

/** Читает сохранённый язык; хранилище может быть недоступно. */
export function stored(storage) {
  try {
    return storage.getItem(KEY);
  } catch {
    return null;
  }
}

/** Запоминает выбор языка. Отказ хранилища здесь не событие: язык выбран. */
export function remember(storage, lang) {
  try {
    storage.setItem(KEY, lang);
  } catch {
    // Приватный режим либо запрет сайту: язык действует до перезагрузки.
  }
}

/**
 * Переводит разметку: `data-i18n` - текст узла, `data-i18n-attr` - атрибут.
 *
 * Формы `data-i18n-html` у образца здесь нет намеренно: `innerHTML` из
 * словаря - это разметка, приезжающая из данных, то есть лишний путь в DOM.
 * Строка, которой нужна разметка, разбивается на узлы, а не на теги в словаре.
 */
export function apply(root = document) {
  for (const node of root.querySelectorAll("[data-i18n]")) {
    node.textContent = t(node.dataset.i18n);
  }
  for (const node of root.querySelectorAll("[data-i18n-attr]")) {
    // Запись `placeholder:account.login` - атрибут и ключ через двоеточие; несколько
    // пар через точку с запятой.
    for (const pair of node.dataset.i18nAttr.split(";")) {
      const [attribute, key] = pair.split(":").map((s) => s.trim());
      if (attribute && key) node.setAttribute(attribute, t(key));
    }
  }
  if (root.documentElement) root.documentElement.lang = current;
}

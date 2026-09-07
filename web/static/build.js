// Опись сборки: какой бандл открыт и не вышел ли новый.
//
// # Как страница узнаёт свой бандл
//
// Из собственного адреса. Сборка кладёт страницу в `b/<отпечаток>/`, и
// `import.meta.url` этого модуля несёт отпечаток - второго носителя у него нет. Впиши
// его в код при сборке - и появился бы второй, который однажды отстанет от адреса.
//
// # Как узнаёт о новой
//
// Спрашивает `version.json` (он отдаётся `no-cache`) - **по событиям, а не по
// таймеру**: при открытии, при возвращении вкладки на глаза, перед публикацией. Опрос
// по таймеру у образца отвергнут прямо, и правильно: вкладка редактора живёт часами.
//
// **Молча не перезагружаемся никогда.** В редакторе лежит несохранённый исходник; о
// новой сборке говорит строка в шапке, а перезагружает - автор.

/** Отпечаток бандла из собственного адреса; `null` - страница не собрана. */
export function bundleOfUrl(url) {
  const match = /\/b\/([0-9a-f]{6,})\//.exec(String(url));
  return match ? match[1] : null;
}

/** Отпечаток бандла, в котором открыта страница. */
export function own() {
  return bundleOfUrl(import.meta.url);
}

/**
 * Читает опись сборки.
 *
 * Отсутствие описи - не ошибка: страницу могли открыть прямо из `web/static`,
 * где сборки нет вовсе. Тогда адрес модуля берёт умолчание, а об обновлениях
 * речи не идёт.
 */
export async function describe(fetchJson = defaultFetch) {
  const bundle = own();
  try {
    const remote = await fetchJson();
    return { bundle, ...remote, wasm: absolute(remote.wasm) };
  } catch {
    return { bundle, wasm: null };
  }
}

/**
 * Сравнивает открытую сборку с выложенной.
 *
 * @returns {Promise<boolean>} `true` - вышла другая сборка
 */
export async function outdated(current, fetchJson = defaultFetch) {
  if (!current?.bundle) return false;
  try {
    const remote = await fetchJson();
    return Boolean(remote.bundle) && remote.bundle !== current.bundle;
  } catch {
    // Сеть отвалилась - это не повод звать перезагрузку: страница работает целиком в
    // браузере и без сети.
    return false;
  }
}

/** Адрес модуля из описи - относительно корня статики, а не каталога бандла. */
function absolute(wasm) {
  if (!wasm) return null;
  return new URL(wasm, new URL("../../", import.meta.url)).href;
}

async function defaultFetch() {
  const url = new URL("../../version.json", import.meta.url);
  const response = await fetch(url, { cache: "no-store" });
  if (!response.ok) throw new Error(`version.json: ${response.status}`);
  return response.json();
}

/**
 * Время сборки для шапки: `ГГГГ-ММ-ДД ЧЧ:ММ` по часам читателя.
 *
 * Метка в описи - UTC (`built_at`), а показывается местное время: читатель
 * сравнивает её со своими часами ("это сегодняшняя сборка?"), а не с
 * гринвичскими. Секунд нет - они ничего не решают и мельтешат.
 *
 * Неразобранная метка даёт пустую строку, а не "Invalid Date": опись
 * приходит с сервера, и старая её версия поля не несёт вовсе.
 */
export function moment(iso) {
  if (!iso) return "";
  const at = new Date(iso);
  if (Number.isNaN(at.getTime())) return "";
  const pad = (n) => String(n).padStart(2, "0");
  return (
    `${at.getFullYear()}-${pad(at.getMonth() + 1)}-${pad(at.getDate())}` +
    ` ${pad(at.getHours())}:${pad(at.getMinutes())}`
  );
}

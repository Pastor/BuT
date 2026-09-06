// Живая страница проекта: адрес `/p/<id>`.
//
// # Две ссылки, разный смысл
//
// Ссылка-фрагмент (`share.js`) - **снимок**: она несёт исходник целиком, живёт вечно и
// работает без сервера. Ссылка `/p/<id>` - **живая** страница: она следует за правками
// автора и требует сервера. Одна другую не подменяет, и обе подписаны словами: снимок
// не портится удалением проекта, а живая показывает сегодняшнее.
//
// # Чего здесь нет
//
// Ни входа, ни сохранения, ни списка проектов - это задача `09e`. Здесь только чтение:
// страница открывает открытый проект (`public`) либо проект по ссылке (`link`) тому,
// кто пришёл по адресу, в том числе без учётной записи.
//
// Знания о правилах видимости здесь нет ни строки: кому что видно, решает сервер, а
// страница показывает его ответ. Заведи проверку здесь - и правило стало бы жить в двух
// местах, разойдясь при первой же правке.

/**
 * Достаёт идентификатор проекта из пути; `null` — путь не наш.
 *
 * ⚠️ Разбирается ПУТЬ, а не адрес целиком: сервис умеет стоять за обратным
 * прокси под префиксом (`BASE_PATH`), и `/takt/p/<id>` — тот же случай.
 *
 * @param {string} pathname путь адреса
 * @returns {string|null} идентификатор
 */
export function idInPath(pathname) {
  const found = /(?:^|\/)p\/([A-Za-z0-9_-]{1,64})\/?$/.exec(pathname ?? "");
  return found ? found[1] : null;
}

/**
 * Корень API для страницы, открытой по адресу проекта.
 *
 * @param {string} pathname путь адреса
 * @returns {string} корень с завершающей косой чертой
 */
export function apiRoot(pathname) {
  const path = pathname ?? "/";
  const cut = path.replace(/(?:^|\/)p\/[A-Za-z0-9_-]{1,64}\/?$/, "");
  return cut.endsWith("/") ? cut : `${cut}/`;
}

/**
 * Читает проект и его активный файл.
 *
 * Возвращает состояние в той же форме, что и ссылка-снимок: страница не должна
 * знать, откуда пришёл исходник — иначе путей применения стало бы два.
 *
 * @param {string} id идентификатор проекта
 * @param {string} root корень API
 * @param {typeof fetch} [get] способ сходить за данными (подменяется в тестах)
 * @returns {Promise<{name: string, owner: string, visibility: string,
 *   version: string, target: string, args: string, source: string,
 *   scenario: string}>}
 */
export async function read(id, root, get = fetch) {
  const meta = await ask(get, `${root}api/projects/${encodeURIComponent(id)}`);
  const files = Array.isArray(meta.files) ? meta.files : [];
  // Активный файл назначает владелец; не назначен - первый по имени. Порядок задаёт
  // сервер, и второй сортировки здесь нет.
  const main =
    files.find((file) => file.name === meta.main_file) ??
    files.find((file) => file.kind === "takt");
  // Сценарий берётся названный проектом: их бывает несколько, и первый по имени - не
  // тот, на котором автор показывает работу модели.
  const scenarioFile =
    files.find((file) => file.name === meta.main_scenario && file.kind === "scenario") ??
    files.find((file) => file.kind === "scenario");
  const source = main ? await text(get, root, id, main.name) : "";
  const scenario = scenarioFile ? await text(get, root, id, scenarioFile.name) : "";
  return {
    name: meta.name ?? "",
    owner: meta.owner ?? "",
    visibility: meta.visibility ?? "",
    // Версия модуля - свойство проекта (решение A5): вывод целей меняется вместе с
    // компилятором, и чужой образец обязан открываться тем модулем, которым его писали.
    version: meta.takt_lang ?? "",
    // Цель и ключи сборки - тоже свойство проекта: чужой образец обязан открываться
    // Сборкой автора.
    target: meta.build_target ?? "",
    args: meta.build_args ?? "",
    source,
    scenario,
  };
}

/** Читает текст файла проекта. */
async function text(get, root, id, name) {
  const body = await ask(
    get,
    `${root}api/projects/${encodeURIComponent(id)}/files/${encodeURIComponent(name)}`,
  );
  return body.text ?? "";
}

/**
 * Спрашивает сервер и разбирает ответ.
 *
 * ⚠️ Отказ поднимается **ключом словаря**, а не текстом: текст оболочки строит
 * одна точка — главный поток страницы (задача 10a). Строка здесь была бы
 * вторым словарём.
 */
async function ask(get, url) {
  let response;
  try {
    response = await get(url, { headers: { accept: "application/json" } });
  } catch (error) {
    throw named({ key: "project.failed", params: { error: error?.message ?? String(error) } });
  }
  if (response.status === 404) throw named({ key: "project.notFound", params: {} });
  if (!response.ok) {
    throw named({ key: "project.failed", params: { error: String(response.status) } });
  }
  return await response.json();
}

/** Отказ с ключом словаря: страница переведёт его сама. */
function named(problem) {
  const error = new Error(problem.key);
  error.key = problem.key;
  error.params = problem.params;
  return error;
}

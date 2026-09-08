// Черновик: работа автора переживает перезагрузку.

/**
 * Ключ безымянного буфера. Версия в имени: форма записи ещё может измениться.
 *
 * Черновик без проекта остаётся под прежним ключом: им
 * пользуется тот, кто не входил вовсе, и терять его при появлении проектов
 * незачем.
 */
const KEY = "takt.draft.v1";

/**
 * Предел черновика - тот же, что у публикации (64 КиБ,).
 *
 * Превышение сообщается, а не режется молча: усечённый исходник выглядит
 * целым и перестаёт компилироваться в месте, которого автор не писал.
 */
export const LIMIT_BYTES = 64 * 1024;

/**
 * Сохраняет черновик; возвращает `null` либо причину отказа.
 *
 * Причина возвращается **ключом словаря и подстановками**, а не строкой:
 * текст оболочки строит одна точка - главный поток страницы.
 * Здесь строкой была бы вторая копия словаря.
 */
export function save(storage, draft) {
  const text = JSON.stringify(draft);
  const size = new TextEncoder().encode(text).length;
  if (size > LIMIT_BYTES) {
    return { key: "draft.tooBig", params: { limit: Math.floor(LIMIT_BYTES / 1024), size } };
  }
  try {
    storage.setItem(KEY, text);
    return null;
  } catch (error) {
    // Приватный режим, переполненное хранилище, запрет сайту - записи нет, и автор
    // обязан об этом узнать: он рассчитывает, что текст переживёт перезагрузку.
    return { key: "draft.notSaved", params: { error: error?.message ?? error } };
  }
}

/** Читает черновик; `null` - его нет либо он испорчен. */
export function load(storage) {
  let text;
  try {
    text = storage.getItem(KEY);
  } catch {
    return null;
  }
  if (!text) return null;
  try {
    const parsed = JSON.parse(text);
    return {
      source: parsed.source ?? "",
      scenario: parsed.scenario ?? "",
      target: parsed.target ?? "",
      args: parsed.args ?? "",
      layout: parsed.layout ?? "",
    };
  } catch {
    // Испорченная запись - не повод падать: страница открывается с умолчаниями, как при
    // первом заходе.
    return null;
  }
}

/** Ключ черновиков проектов: их много, и каждый ключуется файлом. */
const KEY_V2 = "takt.draft.v2";

/**
 * Сколько черновиков проектов держим.
 *
 * Предел нужен не ради места, а ради предела: без него карта растёт по
 * числу открытых за всё время файлов, а `localStorage` кончается молча - и
 * кончится он на записи, то есть в момент сохранения работы.
 */
export const DRAFTS_KEPT = 20;

/** Ключ записи: проект и файл вместе. */
function slot(project, file) {
  return `${project}\u0000${file}`;
}

/**
 * Сохраняет черновик файла проекта; `null` либо причина отказа.
 *
 * Ревизия хранится вместе с текстом: без неё при возвращении нельзя
 * сказать, разошёлся ли черновик с сервером, и выбор пришлось бы предлагать
 * всегда - то есть приучать отвечать не читая.
 */
export function saveFile(storage, record) {
  const all = loadAll(storage);
  all[slot(record.project, record.file)] = {
    project: record.project,
    file: record.file,
    revision: record.revision ?? null,
    source: record.source ?? "",
    scenario: record.scenario ?? "",
    // Какому файлу принадлежит текст сценария: их несколько.
    scenarioFile: record.scenarioFile ?? null,
    // Цель и ключи сборки: проект задаёт умолчание, черновик его перекрывает - иначе
    // выбор автора терялся бы при каждой перезагрузке. Запись прежнего черновика полей
    // не имеет, и читается она как "выбора нет": там подставится пара проекта.
    target: record.target ?? "",
    args: record.args ?? "",
    // Раскладка схемы - авторская работа того же рода, что текст модели.
    layout: record.layout ?? "",
    savedAt: record.savedAt ?? Date.now(),
  };
  // Старшие уходят первыми: черновик, к которому не возвращались двадцать файлов назад,
  // автору уже не нужен.
  const kept = Object.entries(all)
    .sort((a, b) => (b[1].savedAt ?? 0) - (a[1].savedAt ?? 0))
    .slice(0, DRAFTS_KEPT);
  const text = JSON.stringify(Object.fromEntries(kept));
  const size = new TextEncoder().encode(text).length;
  if (size > LIMIT_BYTES) {
    return { key: "draft.tooBig", params: { limit: Math.floor(LIMIT_BYTES / 1024), size } };
  }
  try {
    storage.setItem(KEY_V2, text);
    return null;
  } catch (error) {
    return { key: "draft.notSaved", params: { error: error?.message ?? error } };
  }
}

/** Читает черновик файла проекта; `null` - его нет. */
export function loadFile(storage, project, file) {
  return loadAll(storage)[slot(project, file)] ?? null;
}

/** Забывает черновик файла: успешное сохранение делает его лишним. */
export function clearFile(storage, project, file) {
  const all = loadAll(storage);
  delete all[slot(project, file)];
  try {
    storage.setItem(KEY_V2, JSON.stringify(all));
  } catch {
    // Нечего забывать - не ошибка.
  }
}

/** Все черновики проектов. */
function loadAll(storage) {
  try {
    const text = storage.getItem(KEY_V2);
    if (!text) return {};
    const parsed = JSON.parse(text);
    return parsed && typeof parsed === "object" ? parsed : {};
  } catch {
    // Испорченная запись - не повод падать: считаем, что черновиков нет.
    return {};
  }
}

/** Забывает черновик (после публикации либо по просьбе автора). */
export function clear(storage) {
  try {
    storage.removeItem(KEY);
  } catch {
    // Нечего забывать - не ошибка.
  }
}

/**
 * Откладывает вызов: черновик пишется не на каждую букву.
 *
 * Задержка - не оптимизация ради красоты: `localStorage` синхронен, и
 * запись на каждое нажатие подтормаживала бы набор на длинной модели.
 */
export function debounce(fn, delayMs) {
  let timer = null;
  let pending = null;
  const wrapped = (...args) => {
    pending = args;
    if (timer !== null) clearTimeout(timer);
    timer = setTimeout(() => {
      timer = null;
      fn(...pending);
    }, delayMs);
  };
  /**
   * Записывает немедленно, если запись отложена.
   *
   * Нужно там, где страницу вот-вот перезагрузят (выход на новую сборку):
   * отложенная на 400 мс запись до перезагрузки не доживёт, и автор потеряет
   * последние набранные строки.
   */
  wrapped.now = () => {
    if (timer === null) return;
    clearTimeout(timer);
    timer = null;
    fn(...pending);
  };
  return wrapped;
}

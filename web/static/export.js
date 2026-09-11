// Окно экспорта схемы: картинка листа, модели или проекта и видео прогона.
//
// # Что здесь решается
//
// Окно спрашивает формат, объём, вид, фон, легенду и паузу и собирает из этого
// запрос модулю. Рисует не страница: модуль зовёт тот же носитель рисунка, что
// `takt-sim export`, и картинка со страницы равна картинке командной строки байт в
// байт. Холст здесь не участвует вовсе.
//
// # Порядок: правка файла, потом экспорт по нему
//
// Экспорт читает файл раскладки `.takt-ui`, а не состояние холста: сперва
// раскладка записывается в черновик и в проект (если он открыт и его можно
// править), затем собирается состав и уходит запрос. Обратный порядок выгрузил бы
// картинку по вчерашнему файлу.

import { t } from "./i18n.js";

/** Видео ли формат: видео - всегда цветной вид одного листа. */
export function isVideo(format) {
  return format === "gif" || format === "mp4";
}

/** Выбор окна по умолчанию. */
export const DEFAULTS = Object.freeze({
  format: "svg",
  scope: "sheet",
  view: "draft",
  background: "fill",
  legend: true,
  pause: 500,
});

/**
 * Запрос модулю из выбора окна и того, что открыто.
 *
 * @param {typeof DEFAULTS} choice выбор окна
 * @param {{files: Record<string, string>, main_file?: string|null, main_scenario?: string|null,
 *   model?: string|null, sheet?: string|null, scenario?: string|null, steps?: number|null,
 *   name?: string}} context состав и открытое: модель, лист, сценарий, предел тактов
 */
export function exportRequest(choice, context) {
  const video = isVideo(choice.format);
  const scope = choice.scope;
  return {
    files: context.files,
    main_file: context.main_file ?? null,
    main_scenario: context.main_scenario ?? null,
    formats: [choice.format],
    // Видео всегда цветное: чертёжный вид у видео модуль отвергнет словами, и
    // спрашивать о нём окну незачем.
    view: video ? "run" : choice.view,
    background: choice.background,
    legend: Boolean(choice.legend),
    pause: Math.max(10, Math.round(Number(choice.pause) || DEFAULTS.pause)),
    model: scope === "project" ? null : (context.model ?? null),
    sheet: scope === "sheet" ? (context.sheet ?? null) : null,
    scenario: context.scenario ?? null,
    steps: context.steps ?? null,
    archive: `${context.name || "takt"}.export.zip`,
  };
}

/** Байты из строки base64 ответа модуля. */
export function bytesOf(data) {
  const text = atob(data);
  const out = new Uint8Array(text.length);
  for (let i = 0; i < text.length; i += 1) out[i] = text.charCodeAt(i);
  return out;
}

/** Тип содержимого файла по расширению - для загрузки. */
export function mimeOf(name) {
  const ext = name.slice(name.lastIndexOf(".") + 1);
  return (
    { svg: "image/svg+xml", png: "image/png", gif: "image/gif", mp4: "video/mp4", zip: "application/zip" }[ext] ??
    "application/octet-stream"
  );
}

/**
 * Выгрузка: запись раскладки, состав, запрос, загрузка файлов.
 *
 * @param {typeof DEFAULTS} choice выбор окна
 * @param {{keep: () => Promise<void>, context: () => Promise<object>,
 *   export: (request: object) => Promise<object>, download: (name: string, bytes: Uint8Array, type: string) => void,
 *   say: (text: string, kind: string) => void}} host что умеет страница
 * @returns {Promise<boolean>} выгружено ли
 */
export async function runExport(choice, host) {
  host.say(t("export.working"), "ok");
  try {
    await host.keep();
    const context = await host.context();
    const reply = await host.export(exportRequest(choice, context));
    if (!reply?.ok) {
      host.say(t("export.failed", { error: reply?.error?.message ?? "" }), "error");
      return false;
    }
    for (const file of reply.files) host.download(file.name, bytesOf(file.data), mimeOf(file.name));
    const done = t("export.done", { count: reply.names.length });
    if (reply.notes?.length) host.say(`${done} ${reply.notes.join("; ")}`, "warning");
    else host.say(done, "ok");
    return true;
  } catch (error) {
    host.say(t("export.failed", { error: String(error?.message ?? error) }), "error");
    return false;
  }
}

/**
 * Подключает окно экспорта.
 *
 * @param {Record<string, HTMLElement>} nodes узлы окна по `id`
 * @param {object} host что умеет страница (см. [`runExport`])
 */
export function attachExport(nodes, host) {
  const dom = {
    open: nodes["scheme-export"],
    modal: nodes["export-modal"],
    format: nodes["export-format"],
    scope: nodes["export-scope"],
    view: nodes["export-view"],
    background: nodes["export-background"],
    legend: nodes["export-legend"],
    pause: nodes["export-pause"],
    cancel: nodes["export-cancel"],
    go: nodes["export-go"],
  };
  if (!dom.open || !dom.modal) return null;
  const choice = { ...DEFAULTS };
  const groups = { format: dom.format, scope: dom.scope, view: dom.view, background: dom.background };

  /** Обоймы показывают выбор, а неприменимое гасят, не пряча. */
  function paint() {
    const video = isVideo(choice.format);
    // Видео всегда цветное - обойма вида показывает это, не трогая выбор картинки.
    const shown = { ...choice, view: video ? "run" : choice.view };
    for (const [key, group] of Object.entries(groups)) {
      for (const button of group.querySelectorAll("button[data-value]")) {
        button.setAttribute("aria-pressed", String(button.dataset.value === shown[key]));
      }
    }
    for (const button of dom.view.querySelectorAll("button")) button.disabled = video;
    for (const button of dom.background.querySelectorAll("button")) button.disabled = choice.format !== "png";
    dom.pause.disabled = !video;
    dom.legend.checked = choice.legend;
  }

  for (const [key, group] of Object.entries(groups)) {
    group.addEventListener("click", (event) => {
      const button = event.target.closest("button[data-value]");
      if (!button || button.disabled) return;
      choice[key] = button.dataset.value;
      paint();
    });
  }
  dom.legend.addEventListener("change", () => {
    choice.legend = dom.legend.checked;
  });
  dom.pause.addEventListener("change", () => {
    choice.pause = Number(dom.pause.value) || DEFAULTS.pause;
  });

  function open() {
    dom.pause.value = String(choice.pause);
    paint();
    dom.modal.hidden = false;
    dom.go.focus();
  }
  function close() {
    dom.modal.hidden = true;
    dom.open.focus();
  }
  dom.open.addEventListener("click", open);
  dom.cancel.addEventListener("click", close);
  dom.modal.addEventListener("click", (event) => {
    if (event.target === dom.modal) close();
  });
  dom.modal.addEventListener("keydown", (event) => {
    if (event.key === "Escape") close();
  });
  dom.go.addEventListener("click", async () => {
    close();
    dom.open.disabled = true;
    try {
      await runExport({ ...choice }, host);
    } finally {
      dom.open.disabled = false;
    }
  });
  return { open, close };
}

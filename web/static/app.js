// Страница онлайн-редактора Takt.
//
// Связывает четыре вещи: редактор (`editor.js`), модуль (`bridge.js`), прогон в
// отдельном потоке (`worker.js`) и ссылку с черновиком (`share.js`, `draft.js`). Своей
// логики языка здесь нет - только показ ответов модуля.

import { Bridge, spans } from "./bridge.js";
import { Editor, paintCode, positionToOffset } from "./editor.js";
import * as draft from "./draft.js";
import { encodeState, decodeState } from "./share.js";
import * as i18n from "./i18n.js";
import { t } from "./i18n.js";
import { enhance } from "./pick.js";
import * as build from "./build.js";
import * as shell from "./shell.js";
import * as tip from "./tip.js";
import * as jsonSpans from "./json.js";
import * as md from "./md.js";
import * as flags from "./flags.js";
import * as project from "./project.js";
import * as layoutFile from "./layout.js";
import { Scheme } from "./scheme.js";
import * as api from "./api.js";
import * as account from "./account.js";
import * as alerts from "./alerts.js";

/**
 * Адрес модуля, если описи сборки нет.
 *
 * Обычно адрес берётся из `version.json` - там он с версией в пути, и ссылка
 * открывается своим модулем и через год. Умолчание нужно для случая "страницу
 * открыли без собранной статики".
 */
const WASM_DEFAULT = "takt.wasm";

/**
 * Порог, за которым вывод цели показывается без подсветки.
 *
 * Подсветка стоит разметки: у файла в сотни тысяч символов узлов-строк со
 * span-ами становится столько, что вкладка перестаёт открываться мгновенно.
 * Предел назван словами в шапке файла, а не молчаливо снят: "почему тут нет
 * цвета" - вопрос, на который автор должен получать ответ.
 */
const HIGHLIGHT_LIMIT = 200_000;

/**
 * Надстройки выпадающих списков (`pick.js`) по имени узла.
 *
 * Ручка нужна потому, что значение списка выставляется и молча: тихая
 * запись не рождает `change`, и надстройке некому сказать, что подпись
 * устарела. Пишет значение одна точка - [`setPick`].
 */
const picks = {};

const state = {
  bridge: null,
  editor: null,
  worker: null,
  target: "c",
  args: "",
  scenario: "",
  /** Область сценария: тот же редактор, что у модели (номера строк, перенос). */
  scenarioEditor: null,
  /**
   * Род открытого файла: `takt` либо `markdown`.
   *
   * Род решает всё, что делает страница с текстом: чем красить, что
   * показывать справа и звать ли компилятор. Пояснение не компилируется - у
   * него нет диагностик, и печатать по нему отказ значило бы врать.
   */
  kind: "takt",
  /** Имя открытого файла проекта; пусто - безымянный буфер. */
  file: "",
  /** Холст схемы: граф от модуля, раскладка из файла рядом с моделью. */
  scheme: null,
  /** Имя файла раскладки, парного открытой модели; `null` - файла в проекте нет. */
  layoutFile: null,
  /** Открытая панель правой области: "output", "trace" либо null. */
  panel: "output",
  /** Вкладка внутри панели генерации. */
  tab: "output",
  /**
   * Замечания к модели: от моста (разбор, семантика) и от цели генерации.
   *
   * Списка два, потому что обновляются они порознь: разбор идёт на каждую
   * правку текста, генерация - ещё и на смену цели или ключей сборки. Показ у
   * них один: место замечания компилятора - панель диагностики, и только она.
   */
  diagnostics: [],
  targetDiagnostics: [],
  // Журнал диагностик: записи копятся, самые старые уходят по ротации. Набор
  // прошлой отрисовки хранится рядом - по нему видно, что в журнал добавить.
  diagLast: [],
  diagKeep: 500,
  treeSide: "right",
  crumbsShown: true,
  // Какая из двух записей показана в области: `diagnostics` либо `trace`.
  diagTab: "diagnostics",
  wrap: false,
  // Что показано в области кода: `code`, `scenario` либо `scheme`. Род открытого
  // файла этого не говорит: сценарий и раскладку открывают, не меняя рода.
  shown: "code",
  scenarioFile: "",
  version: "",
  languageVersion: "",
  running: false,
  build: null,
  dirty: false,
};

const dom = {};

/** Точка входа страницы. */
export async function main() {
  cache();
  // Язык выбирается до модуля: он весит мегабайты, а подписи страницы обязаны быть на
  // месте с первого кадра - иначе оболочка успевает мигнуть чужим языком.
  shell.attach(dom.grip, localStorage);
  // Разделитель областей: доли внутри оболочки задаёт тот же читатель, что и её ширину, -
  // и той же ручкой правил (границы, память, клавиатура).
  shell.attachPanes(dom.split, localStorage);
  // Нижняя граница журнала диагностик - шапка и одна запись: столько, чтобы
  // последняя новость была видна.
  shell.attachRows(dom.hsplit, localStorage, { least: () => diagLeast() });
  // Схема делится так же: журнал под холстом, легенда полкой либо колонкой. Доли
  // помнит браузер читателя - это его вид, а не свойство проекта.
  shell.attachLegendRows(dom.legendrows, localStorage);
  shell.attachLegendCols(dom.legendcols, localStorage);
  // Размер структуры проекта: та же ручка правил, а ось и границы у неё свои -
  // сторону выбирает читатель, а нижнюю границу задаёт длина имён файлов.
  shell.attachTree(dom.treesplit, localStorage, {
    side: () => state.treeSide,
    least: () => measureTree() ?? 0,
  });
  // Перенос строк - одна настройка на все области кода: так человек читает код
  // вообще, а не конкретную панель. Своей кнопки у неё нет - её место в окне
  // настроек рядом с языком.
  state.wrap = shell.wrapped(localStorage, shell.WRAP_KEY);
  shell.applyWrap(wrapAreas(), state.wrap);
  // Кегль страницы: ±1 к корневому размеру, от которого считаются все ступени.
  shell.attachFontSize(dom.fontless, dom.fontmore, dom.fontsize, localStorage);
  // Прочие настройки интерфейса - оттуда же: вкладка и бюджет прогона.
  dom.budget.value = shell.setting(localStorage, shell.UI_KEYS.budget, dom.budget.value);
  selectTab(shell.setting(localStorage, shell.UI_KEYS.tab, "output"));
  // Читается только известное значение: в памяти читателя мог остаться выбор
  // области, которой больше нет, и страница открылась бы без вывода вовсе.
  selectPanel(shell.setting(localStorage, shell.UI_KEYS.panel, "output") === "output" ? "output" : null);
  showDiagnosticsPane(shell.setting(localStorage, shell.UI_KEYS.diagnostics, "1") === "1");
  showTree(shell.setting(localStorage, shell.UI_KEYS.treeShown, "1") === "1");
  showTreeSide(shell.treeSide(localStorage));
  showCrumbs(shell.setting(localStorage, shell.UI_KEYS.crumbs, "1") === "1");
  state.diagKeep = shell.diagKeep(localStorage);
  // Размер экрана меняется - меняется и половина, которой ограничен минимум
  // структуры: мерка привязана к окну, а не снята однажды при загрузке.
  window.addEventListener("resize", () => {
    measureTree();
    markDiagnosticsMore();
  });
  // Подсказки - свои, а не нативные: `title` в разметке нет вовсе.
  tip.attach(document);
  await useLanguage(i18n.pick(i18n.stored(localStorage), navigator.languages ?? []));
  // Перехват сбоев ставится сразу за словарём: раньше о них некому было сказать
  // словами, позже - первый же отказ ушёл бы в консоль, которую автор не открывал.
  alerts.watch(window, alarm);
  fillLanguages();
  picks.lang = enhance(dom.lang);

  // Опись сборки читается до модуля: в ней адрес модуля с версией в пути.
  state.build = await build.describe();
  state.bridge = await Bridge.load(state.build.wasm ?? WASM_DEFAULT);
  const version = state.bridge.version();
  state.version = version.takt_lang ?? "";
  state.languageVersion = version.language ?? "";
  // Метка "загрузка модуля..." снимается вместе со своим ключом: иначе смена языка
  // перерисовала бы её поверх версии - нашлось прогоном страницы.
  dom.version.removeAttribute("data-i18n");
  showVersion();
  fillTargets(version.targets ?? []);
  picks.target = enhance(dom.target);
  watchBuild();
  for (const node of [dom.editor, dom.output, dom.diagnostics, dom.trace]) fade(node);

  state.editor = new Editor(dom.editor, onEdit);
  // Сценарий - такая же область кода: тот же носитель строк, те же номера. Красится он
  // Своим разбором (`json.js`), а не словами компилятора: это JSON, а не Takt.
  state.scenarioEditor = new Editor(dom.scenario, () => {
    state.scenario = state.scenarioEditor.value();
    paintScenario();
    saveDraft();
  });
  wire();
  state.scheme = new Scheme(
    {
      scheme: dom.scheme,
      sheet: dom.sheet,
      map: dom.map,
      stage: dom.stage,
      legend: dom.legend,
      crumbs: dom.crumbs,
      crumbsUp: dom["scheme-up"],
      nav: dom.nav,
      // Кнопки холста разошлись по панелям, и слушает их сам холст: список
      // панелей иначе пришлось бы вести дважды - в разметке и в подписке.
      tools: dom.scheme,
      panels: {
        run: dom["panel-run"],
        view: dom["panel-view"],
        sheet: dom["panel-sheet"],
        // Легенда углов холста не занимает, но вопрос к ней тот же - показывать
        // или нет, - и отвечать на него читатель ходит в то же окно.
        legend: dom.legend,
        // Миникарта - не панель кнопок, но её видимость решается тем же
        // вопросом и в том же окне.
        map: dom.map,
      },
      scheme: dom.scheme,
      docks: docks(),
      legendSplits: [dom.legendrows, dom.legendcols],
      settingsOpen: dom.settings,
      empty: dom["scheme-empty"],
      notice: dom["scheme-notice"],
      noticeText: dom["scheme-notice-text"],
      noticeDrop: dom["scheme-drop"],
      zoom: dom.zoom,
      settingsModal: dom["scheme-modal"],
      settingsTabs: dom["scheme-tabs"],
      settingsBody: dom["scheme-settings"],
      settingsSave: dom["scheme-save"],
      settingsCancel: dom["scheme-cancel"],
    },
    {
      t,
      // Кто правит раскладку: логин вошедшего, а без входа - пусто (носитель
      // раскладки запишет гостя). Спрашивается на каждую правку.
      who: () => api.who()?.login ?? "",
      // Щелчок по узлу ставит курсор на имя состояния: выбор синхронен с текстом.
      onSelect: (node) => jump(node.nameRange.start_line, node.nameRange.start_character),
      // Настройки страницы окно показывает, но держит их страница: язык живёт в
      // памяти читателя, перенос строк - в своей кнопке. Спрашиваются они у тех
      // же носителей, что и правит читатель руками.
      pageValues: () => ({
        lang: i18n.language(),
        wrap: state.wrap,
        treeSide: state.treeSide,
        diagKeep: state.diagKeep,
        crumbs: state.crumbsShown,
      }),
      onPage: (key, value) => setPageSetting(key, value),
      // Правка раскладки - такая же работа, как правка текста: черновик пишется по тем
      // же правилам и с тем же вопросом при уходе.
      onChange: () => {
        state.dirty = true;
        saveDraft();
      },
    },
  );
  // Курсор в объявлении состояния подсвечивает узел: обратная половина синхронизации.
  document.addEventListener("selectionchange", syncCursor);

  // Учётная запись и проекты. Корень API берётся от пути страницы: за прокси она стоит
  // под префиксом, а на `/p/<id>` относительный адрес увёл бы запрос под неё саму.
  api.configure({ root: project.apiRoot(location.pathname), storage: localStorage });
  account.attach(dom, {
    source: () => state.editor.value(),
    scenario: () => state.scenario,
    // Цель и ключи берутся у проекта, а не у читателя: проект задаёт умолчание,
    // черновик автора его перекрывает. Пришло пусто - остаётся выбранное на странице: у
    // проекта прежней выгрузки пары нет вовсе.
    open: (restored) => {
      applyState({
        ...restored,
        target: restored.target || state.target,
        args: restored.args ?? state.args,
      });
      refresh();
    },
    // Черновик пишется немедленно перед уходом на площадку: отложенная запись до
    // перехода не доживёт.
    keep: () => saveDraft.now(),
    // Проект закрыт: имя файла уходит из шапки - она называла бы файл, которого
    // на странице больше нет. Текст остаётся: закрытие проекта не потеря работы.
    closed: () => {
      state.file = "";
      state.kind = "takt";
      dom.openfilename.textContent = "";
      // Проект закрыт - показывать нечего: ни кода, ни сценария, ни схемы.
      // Оставшийся показ говорил бы о файле, которого больше не открыто.
      showSource("none");
      dom.output.replaceChildren();
      clearDiagnostics();
    },
    // Раскладка схемы: текст файла `.takt-ui` для записи в проект и в черновик.
    layout: () => state.scheme.text(),
    // Цель выгрузки - та, что открыта во вкладке вывода: архив "с генерацией" берёт
    // выбранную цель. Она же уходит в метаданные проекта при сохранении - вместе с
    // ключами.
    target: () => state.target,
    args: () => state.args,
    // Сценариев в проекте бывает несколько (09n): список и выбранный приходят от того,
    // кто знает состав проекта, - страница их только показывает.
    // Список сценариев показывает структура проекта: своего выбора у прогона
    // нет, и второй список разошёлся бы с деревом.
    scenarios: () => {},
    openScenario: (text, file) => {
      state.scenario = text;
      state.scenarioFile = file ?? "";
      state.scenarioEditor.setValue(text);
      paintScenario();
      if (state.shown === "scenario") showSource("scenario");
    },
    // Дерево перерисовано: мерка его ширины считается по нарисованным именам.
    treeChanged: () => measureTree(),
    showTrace: () => showSource("scenario"),
    showScheme: () => showSource("scheme"),
    say,
  });
  // Структура проекта рисуется сразу: без входа она говорит, что проекта нет, -
  // пустая область читалась бы как поломка.
  account.paintEmptyTree();
  // Возврат с площадки разбирается до восстановления состояния: во фрагменте там
  // ticket, а не ссылка-снимок, и принять одно за другое нельзя.
  const returned = await account.handleReturn();

  // Порядок источников: адрес проекта -> последний открытый проект -> ссылка-снимок
  // -> черновик -> пусто. Живая страница сильнее снимка и черновика: читатель пришёл
  // по адресу проекта, и показать ему вместо проекта вчерашний черновик значило бы
  // ответить не на тот вопрос.
  const opened = await openProject();
  // Автор работает в проекте, и заход возвращает его туда же: спрашивать о том,
  // что уже известно, страница не должна. Проект открывает себя сам - состав
  // файлов и активный файл приходят от него.
  const last = opened ? false : await account.restoreLast();
  const restored = last
    ? null
    : opened ?? (returned ? null : await decodeState(location.hash)) ?? draft.load(localStorage);
  // Без проекта и без черновика области пусты: образец здесь был бы чужой работой
  // на месте своей.
  if (!last) applyState(restored ?? { source: "" });
  refresh();
}

/**
 * Открывает проект, если страницу открыли по его адресу (`/p/<id>`).
 *
 * @returns {Promise<object|null>} состояние редактора либо `null`
 */
async function openProject() {
  const id = project.idInPath(location.pathname);
  if (!id) return null;
  try {
    const opened = await project.read(id, project.apiRoot(location.pathname));
    // Подпись - часть ответа: читатель обязан видеть, чей образец у него открыт, иначе
    // чужая модель выглядит его собственной работой.
    dom.project.textContent = t("project.open", {
      name: opened.name,
      owner: opened.owner,
    });
    dom.project.hidden = false;
    // Читатель вправе скачать открытый проект архивом: текст всё равно у него.
    await account.adopt({ id, name: opened.name });
    return {
      source: opened.source,
      scenario: opened.scenario,
      target: opened.target,
      args: opened.args,
      layout: opened.layout,
      layoutFile: opened.layoutFile,
    };
  } catch (error) {
    // Отказ виден строкой, а не пустой страницей: удалённый или закрытый проект -
    // обычный ответ сервиса, и он обязан быть назван.
    dom.project.textContent = t(error?.key ?? "project.failed", error?.params ?? {});
    dom.project.hidden = false;
    return null;
  }
}

/**
 * Ставит язык оболочки: словарь, разметка, строка-граница.
 *
 * Смена идёт без перезагрузки: в редакторе лежит несохранённая работа, и
 * перезагрузка ради подписи была бы худшей ценой из возможных.
 */
async function useLanguage(lang) {
  await i18n.load(lang);
  i18n.apply(document);
  if (dom.lang) setPick("lang", i18n.language());
  // До диагностики, трасса и сводка приходят из модуля только по-русски. При другом
  // языке оболочки смешение называется строкой, а не прячется: читатель, увидевший
  // русский текст без предупреждения, решит, что перевод сломан.
  const mixed = i18n.language() !== i18n.BASE;
  if (dom["tools-lang"]) dom["tools-lang"].hidden = !mixed;
  if (dom["tools-lang-trace"]) dom["tools-lang-trace"].hidden = !mixed;
  redraw();
}

/**
 * Перерисовывает то, что построено кодом, а не разметкой.
 *
 * `i18n.apply` знает только узлы с `data-i18n`; список диагностик, вывод
 * цели и строка версии собираются на ходу, и без этого шага они остались бы на
 * прежнем языке. Нашлось прогоном страницы: после переключения подписи стали
 * английскими, а "Ошибок нет" под ними - нет.
 *
 * Трасса прогона не перерисовывается: это журнал уже случившегося, и
 * переписать его задним числом значило бы соврать о том, что было напечатано.
 */
function redraw() {
  // Журнал диагностик - история случившегося, и переписывать его задним числом
  // нельзя (то же правило, что у трассы). Строка "ошибок нет" историей не
  // является: это заглушка пустого журнала, и язык у неё сегодняшний.
  //
  // Стоит она до проверки моста намеренно: заглушку строит первая же отрисовка
  // журнала, а та случается раньше словаря - выбранная область зовёт сборку, и
  // сборка без моста отвечает пустым списком диагностик. Ранний выход написан
  // для перерисовок, которым нужен мост, и заглушка к ним не относится.
  const empty = dom.diagnostics?.querySelector(".row-none .row-text");
  if (empty) empty.textContent = t("diagnostics.none");
  // Зовётся и до того, как страница собрана: язык выбирается первым делом, раньше
  // модуля и редактора. Отсюда обе проверки - без них смена языка роняла бы загрузку
  // страницы, и отказ выглядел бы отказом модуля.
  if (!state.bridge) return;
  showVersion();
  if (state.editor) refresh();
  // Открытое окно настроек построено кодом: смена языка из него самого оставила
  // бы его подписи на прежнем языке, и читатель увидел бы два языка разом.
  if (state.scheme?.settings?.isOpen) state.scheme.settings.paint();
}

/**
 * Следит за выходом новой сборки - по событиям, а не по таймеру.
 *
 * Вкладка редактора живёт часами, и опрос по таймеру был бы обращением к
 * стенду каждые несколько минут ни за чем. Спрашиваем там, где ответ нужен:
 * при открытии, при возвращении вкладки на глаза и перед публикацией ссылки.
 */
function watchBuild() {
  const check = async () => {
    if (dom.update.hidden === false) return;
    if (await build.outdated(state.build)) dom.update.hidden = false;
  };
  check();
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "visible") check();
  });
  dom.update.addEventListener("click", () => {
    // Сначала черновик, потом перезагрузка: обновление не должно стоить автору ни
    // строки.
    saveDraft.now();
    location.reload();
  });
}

/**
 * Строка шапки: версия языка и номер сборки сервиса.
 *
 * Версии модуля здесь нет: читателю она
 * ничего не решает. Номер сборки решает - он говорит "свежее или старее"
 * одним взглядом, тогда как дата требует счёта в уме.
 *
 * Дата, коммит и ветка живут в подсказке: они нужны, когда о сборке спрашивают
 * предметно ("какой это коммит?"), и в строке шапки только шумели бы. Номера
 * нет - сборка вне git, - и тогда показывается время.
 */
function showVersion() {
  const at = build.moment(state.build?.built_at);
  const number = state.build?.build;
  dom.version.textContent = t("bar.version", {
    language: state.languageVersion,
    built: number ? t("bar.build", { number }) : at,
  });
  dom.version.dataset.tip = t("bar.buildTip", {
    built: at || "—",
    commit: state.build?.commit || "—",
    branch: state.build?.branch || "—",
  });
}

/** Наполняет переключатель языками выпуска. */
function fillLanguages() {
  dom.lang.replaceChildren();
  for (const [code, name] of Object.entries(i18n.LANGUAGES)) {
    const option = document.createElement("option");
    option.value = code;
    // Самоназвание: оно не переводится (см. Кнопка при этом показывает код
    // (`data-short`) - она размером с соседние значки.
    option.textContent = name;
    option.dataset.short = i18n.SHORT[code] ?? code;
    dom.lang.appendChild(option);
  }
  setPick("lang", i18n.language());
}

/**
 * Растворяющийся нижний край у прокручиваемой области.
 *
 * Полос прокрутки на странице нет вовсе, и
 * признак "внизу есть непрочитанное" несёт край: пока до конца не докрутили,
 * содержимое плавно исчезает; у конца край становится резким.
 *
 * Высота растворения ставится в `--fade` кодом, а не задана в стилях:
 * маска обязана исчезать вместе с непрочитанным, иначе последняя строка
 * списка навсегда останется полупрозрачной.
 */
function fade(node) {
  const FADE = 24;
  const update = () => {
    const rest = node.scrollHeight - node.scrollTop - node.clientHeight;
    const height = Math.max(0, Math.min(FADE, rest));
    node.style.setProperty("--fade", `${height}px`);
    node.classList.toggle("fade-bottom", height > 0);
  };
  node.addEventListener("scroll", update, { passive: true });
  // Содержимое меняется чаще, чем прокрутка: перекраска редактора, новая трасса, другой
  // вывод цели.
  new ResizeObserver(update).observe(node);
  new MutationObserver(update).observe(node, { childList: true, subtree: true });
  update();
}

/** Находит узлы страницы один раз: поиск в обработчике - лишняя работа. */
/**
 * Правка настройки страницы из окна настроек.
 *
 * Своих носителей окно не заводит: язык переключается тем же путём, что и
 * список в шапке, перенос строк - нажатием своей кнопки. Иначе у настройки
 * оказалось бы два хозяина, и они разошлись бы молча.
 */
function setPageSetting(key, value) {
  if (key === "lang") {
    setPick("lang", value);
    dom.lang.dispatchEvent(new Event("change"));
  } else if (key === "wrap") {
    state.wrap = value === true;
    shell.setWrap(wrapAreas(), state.wrap, localStorage, shell.WRAP_KEY);
  } else if (key === "crumbs") {
    showCrumbs(value !== false);
  } else if (key === "treeSide") {
    showTreeSide(value);
  } else if (key === "diagKeep") {
    // Объём журнала приводится к границам тем же носителем, что читает его из
    // памяти: второе правило разошлось бы с первым молча.
    shell.remember(localStorage, shell.UI_KEYS.diagKeep, value);
    state.diagKeep = shell.diagKeep(localStorage);
    rotateDiagnostics();
    markDiagnosticsMore();
  }
}

/** Области кода, которых касается перенос строк: настройка одна на все. */
function wrapAreas() {
  return [dom.editor, dom.output, dom.scenario, dom.trace];
}

/** Места панелей холста по имени: признак стоит в разметке. */
function docks() {
  const out = {};
  for (const node of document.querySelectorAll("[data-dock]")) out[node.dataset.dock] = node;
  return out;
}

function cache() {
  for (const id of [
    "editor", "diagnostics", "output", "trace", "version", "target", "args",
    "scenario", "budget", "share", "format", "say", "tabs", "modes",
    "lang", "tools-lang", "tools-lang-trace", "update", "showgen", "showdiag", "grip", "split", "hsplit", "fontless", "fontmore", "fontsize", "project", "flags", "flags-applies",
    "account", "session", "icon-enter", "icon-leave",
    "save", "openfile", "panel", "signedout", "signedin", "whoami",
    "whoami-bar",
    "signin-modal", "signin-cancel", "login", "password", "signin", "signup", "signout", "newname", "newproject",
    "projects", "conflict", "conflicttext", "reread", "overwrite",
    "oauth", "pick", "picklogin", "pickok", "profile", "links", "newpass",
    "setpass", "download", "upload", "showcase", "finder", "query", "findbtn",
    "found", "more", "doc", "sourcetitle", "openfilename",
    "scheme-notice", "scheme-notice-text", "scheme-drop",
    "tree", "treesplit", "diagnostics-head", "diagclear", "showtree",
    "openproject", "createproject", "createcancel",
    "renameproject", "dropproject", "exportproject",
    "projectname", "projectnameok", "projectnamecancel",
    "projectname-modal",
    "dropok", "dropcancel", "droptext", "fromsample", "closeproject",
    "newfile", "dropfile", "renamefile", "uploadfile", "downloadfile", "filepick",
    "importproject", "filekinds", "filename", "filepreview",
    "fileok", "filecancel", "dropfileok", "dropfilecancel", "dropfiletext",
    "renamename", "renamepreview", "renameok", "renamecancel",
    "project-modal", "drop-modal", "file-modal", "dropfile-modal",
    "rename-modal", "pickscenario", "scenario-modal", "scenarios",
    "scenarioload", "scenariocancel",
    "crumbs", "scheme-up", "stage", "scheme", "sheet", "nav", "map",
    "panel-run", "panel-view", "panel-sheet", "settings",
    "scheme-empty", "legend", "zoom", "alerts",
    "scheme-modal", "scheme-tabs", "scheme-settings", "scheme-save", "scheme-cancel",
    "showdiag-tab", "showtrace-tab", "legendrows", "legendcols",
  ]) {
    dom[id] = document.getElementById(id);
  }
}

function fillTargets(targets) {
  dom.target.replaceChildren();
  for (const target of targets) {
    const option = document.createElement("option");
    option.value = target;
    option.textContent = target;
    dom.target.appendChild(option);
  }
  setPick("target", state.target);
}

/**
 * Ставит значение списка и обновляет его надстройку.
 *
 * Точка одна на страницу: значение выставляется молча - при сборке списка
 * целей, восстановлении черновика, ссылке-снимке и открытии проекта (09p), - а
 * тихая запись не рождает `change`, и надстройка (`pick.js`) остаётся с
 * прежней подписью. Кнопка тогда показывает одну цель, а собирается другая.
 * Класс нашёлся прогоном страницы; контроль - греп в `web/tests`.
 */
function setPick(name, value) {
  dom[name].value = value;
  picks[name]?.refresh();
}

function wire() {
  dom.target.addEventListener("change", () => {
    state.target = dom.target.value;
    // Ключи перестраиваются вместе с целью: применимость - свойство цели, и ключ,
    // которого новая цель не принимает, обязан уйти из строки сразу.
    state.args = flags.line(flags.parse(state.args), state.target);
    dom.args.value = state.args;
    drawFlags();
    compile();
    saveDraft();
  });
  dom.args.addEventListener("input", () => {
    state.args = dom.args.value;
    // Конструктор перечитывает строку: величина одна, способов задать два, и своё
    // состояние у конструктора завело бы расхождение с полем.
    drawFlags();
    compile();
    saveDraft();
  });
  dom.settings.addEventListener("click", () => state.scheme.openSettings());
  dom.lang.addEventListener("change", async () => {
    // Язык - свойство читателя, а не документа: в ссылку-снимок и в черновик он не
    // входит, иначе переданная ссылка меняла бы язык у получателя.
    i18n.remember(localStorage, dom.lang.value);
    await useLanguage(dom.lang.value);
  });
  dom.format.addEventListener("click", format);
  // Прогон, шаг и стоп живут в строке уровня схемы: прогон смотрят на схеме -
  // она показывает ход автомата подсветкой, а вкладка прогона несёт лог и
  // настройки. Кнопки узнаются признаком `data-run`, а не именами узлов:
  // список действий один, и заводить трёх имён на три кнопки незачем.
  for (const button of document.querySelectorAll("[data-run]")) {
    const act = { run, step: stepOnce, stop, reset: resetRun }[button.dataset.run];
    if (act) button.addEventListener("click", act);
  }
  dom.budget.addEventListener("change", () =>
    shell.remember(localStorage, shell.UI_KEYS.budget, dom.budget.value)
  );
  // Панель включается своей кнопкой и выключается ею же: нажатая ещё раз кнопка
  // закрывает область - так автор освобождает экран под модель.
  dom.showgen.addEventListener("click", () => selectPanel(state.panel === "output" ? null : "output"));
  // Состояние области спрашивается у страницы, а не у списка: в области две
  // записи, и список диагностик скрыт и тогда, когда показан журнал прогона.
  dom.showdiag.addEventListener("click", () => showDiagnosticsPane(document.body.dataset.diag === "off"));
  dom.diagclear.addEventListener("click", () => clearDiagnostics());
  dom.diagnostics.addEventListener("scroll", () => markDiagnosticsMore());
  dom.showtree.addEventListener("click", () => showTree(document.body.dataset.tree === "off"));
  // Запись выделяется щелчком: в длинном списке так не теряют место, к которому
  // вернулись. Выделена всегда одна - это отметка чтения, а не отбор. Правило
  // одно на журнал прогона и на список диагностик: у них одна форма записи.
  for (const list of [dom.trace, dom.diagnostics]) {
    list.addEventListener("click", (event) => {
      const line = event.target.closest(".row");
      if (line && list.contains(line)) markRow(list, line);
    });
  }
  // Журнал прогона убирается со схемы кнопкой: лист и журнал читают вместе, но
  // когда рисунок велик, полоса строк отнимает у него половину области.
  showDiagTab(shell.setting(localStorage, shell.UI_KEYS.diagTab, "diagnostics"));
  dom["showdiag-tab"].addEventListener("click", () => showDiagTab("diagnostics"));
  dom["showtrace-tab"].addEventListener("click", () => showDiagTab("trace"));
  dom.share.addEventListener("click", share);
  dom.tabs.addEventListener("click", (event) => {
    const tab = event.target.closest("[data-tab]");
    if (tab) {
      selectTab(tab.dataset.tab);
      // Открытая вкладка - настройка читателя: он вернётся туда, где работал.
      shell.remember(localStorage, shell.UI_KEYS.tab, tab.dataset.tab);
    }
  });
  dom.modes.addEventListener("click", (event) => {
    const mode = event.target.closest("[data-mode]");
    if (mode) selectMode(mode.dataset.mode);
  });

  // Наведение: подсказка идёт в строку состояния, а не всплывающим окном. Всплывашка
  // над кодом закрывает сам код, а на устройстве без наведения её не существует вовсе
  // (правило образца: `hover` есть не везде).
  dom.editor.addEventListener("mousemove", onHover);
  dom.editor.addEventListener("mouseleave", () => say("", "ok"));

  // Alt+клик - переход к объявлению и подсветка использований: правая рука остаётся на
  // мыши, а сочетание не занято браузером.
  dom.editor.addEventListener("click", (event) => {
    if (!event.altKey) return;
    event.preventDefault();
    declarationAndUses();
  });

  // F2 - переименование, как в редакторах на машине.
  dom.editor.addEventListener("keydown", (event) => {
    if (event.key === "F2") {
      event.preventDefault();
      renameSymbol();
    }
  });

  // Несохранённая работа не теряется молча: подтверждение ухода - единственное,
  // что браузер позволяет здесь сделать.
  //
  // Спрашиваем, пока работа расходится с сохранённым, а не "пока в редакторе есть
  // текст". Текст есть всегда - с первого открытия там пример, - и безусловный вопрос
  // приучает отвечать "уйти" не читая, то есть перестаёт защищать.
  window.addEventListener("beforeunload", (event) => {
    if (!state.dirty) return;
    event.preventDefault();
    event.returnValue = "";
  });
}

const saveDraft = draft.debounce(() => {
  state.dirty = false;
  // Черновик открытого файла проекта ключуется проектом и файлом (`v2`), а безымянный
  // буфер остаётся под прежним ключом: им пользуется тот, кто не входил вовсе, и терять
  // его при появлении проектов незачем.
  const layout = state.scheme ? state.scheme.text() : "";
  const problem = account.editing()
    ? account.keepDraft(state.editor.value(), state.scenario, state.target, state.args, layout)
    : draft.save(localStorage, {
        source: state.editor.value(),
        scenario: state.scenario,
        target: state.target,
        args: state.args,
        layout,
      });
  if (problem) say(t(problem.key, problem.params), "warning");
}, 400);

function applyState(restored) {
  state.target = restored.target || state.target;
  state.args = restored.args ?? "";
  // Сценарий не задан - остаётся прежним, а не пустеет: с появлением нескольких
  // сценариев (09n) он живёт своим файлом, и открытие модели его не касается. Прежняя
  // запись `?? ""` очищала бы область при каждом открытии.
  if (restored.scenario !== undefined) state.scenario = restored.scenario ?? "";
  // Род открытого файла (09n): пусто - модель, как было до появления пояснений.
  state.kind = restored.kind ?? "takt";
  state.file = restored.file ?? "";
  // Раскладка схемы приходит вместе с моделью (файл проекта, черновик, ссылка); не
  // пришла - лист пуст, и состояния встанут по ярусам.
  state.layoutFile = restored.layoutFile ?? null;
  if (state.scheme) {
    const read = layoutFile.parse(restored.layout ?? "");
    state.scheme.setLayout(read.layout);
    if (read.problem) say(t(read.problem.key, read.problem.params), "warning");
  }
  setPick("target", state.target);
  dom.args.value = state.args;
  drawFlags();
  state.scenarioEditor.setValue(state.scenario);
  paintScenario();
  // Текста нет - область пуста: образец на месте открытого файла читался бы как
  // чужая работа. Он остаётся у нового проекта "по шаблону", где его просят явно.
  state.editor.setValue(restored.source ?? "");
  showKind();
}

/** Что имеет канон записи (см. `format` и `canonOf`). */
const HAS_CANON = new Set(["takt", "scenario"]);

/**
 * Приводит страницу к роду открытого файла.
 *
 * Подпись области - ключ словаря, а не текст: текст оболочки строит одна
 * точка, и написанная здесь строка была бы вторым словарём.
 */
function showKind() {
  // Чем показать файл, решает его род: модель правится кодом, сценарий - своим
  // полем, раскладка показывается схемой, пояснение - разметкой. Отдельных
  // областей у них нет: это такие же файлы проекта, как модель.
  // Пояснение правится как текст - тем же редактором и с подсветкой разметки.
  showSource({ scenario: "scenario", layout: "scheme" }[state.kind] ?? "code");
  // Ключи сборки к пояснению отношения не имеют: вкладки уходят вместе с выводом цели,
  // а их место занимает показ.
  if (state.panel === "output") selectPanel("output");
}

/** Правка текста: подсветка, диагностики, вывод цели. */
/**
 * Строит вкладку ключей сборки по строке `state.args`.
 *
 * Состояние берётся из строки на каждую перерисовку: строка - единственная
 * величина, а конструктор лишь её вид. Ключ, которого опись не знает, едет
 * обратно как есть (`rest`) - страница не вправе терять то, чего не знает.
 */
function drawFlags() {
  if (!dom.flags) return;
  const parsed = flags.parse(state.args);
  const target = state.target;
  dom.flags.replaceChildren();

  const change = () => {
    state.args = flags.line(parsed, target);
    dom.args.value = state.args;
    drawFlags();
    compile();
    saveDraft();
  };

  for (const spec of flags.FLAGS) {
    const chosen = parsed.chosen.get(spec.key) ?? { on: false };
    parsed.chosen.set(spec.key, chosen);
    const usable = flags.applicable(spec, target);

    const box = document.createElement("div");
    box.className = "flag" + (chosen.on ? " on" : "") + (usable ? "" : " unusable");

    const head = document.createElement("label");
    head.className = "flag-head";
    const box_ = document.createElement("input");
    box_.type = "checkbox";
    box_.checked = Boolean(chosen.on);
    box_.disabled = !usable;
    box_.addEventListener("change", () => {
      chosen.on = box_.checked;
      if (chosen.on && spec.clash) {
        const other = parsed.chosen.get(spec.clash);
        if (other) other.on = false;
      }
      change();
    });
    const name = document.createElement("span");
    name.className = "flag-name";
    name.textContent = spec.key;
    head.append(box_, name);
    box.append(head);

    const why = document.createElement("div");
    why.className = "flag-why";
    why.textContent = t(spec.label);
    box.append(why);

    if (chosen.on && usable) box.append(flagValue(spec, chosen, target, change));
    if (!usable) box.append(notice(t("flags.notForTarget", { target })));
    else if (chosen.on && spec.choices && !flags.allows(spec, chosen.value ?? spec.fallback, target)) {
      box.append(notice(t("flags.valueNotForTarget", { value: chosen.value ?? spec.fallback })));
    }
    dom.flags.append(box);
  }

  const usable = flags.FLAGS.filter((spec) => flags.applicable(spec, target)).length;
  dom["flags-applies"].textContent = t("flags.applies", { n: usable, all: flags.FLAGS.length });
}

/** Выбор значения ключа: сегменты для набора, поля для чисел. */
function flagValue(spec, chosen, target, change) {
  const row = document.createElement("div");
  row.className = "flag-value";
  if (spec.choices) {
    const group = document.createElement("div");
    group.className = "seg";
    for (const value of spec.choices) {
      const button = document.createElement("button");
      button.type = "button";
      button.textContent = value;
      button.setAttribute("aria-pressed", String((chosen.value ?? spec.fallback) === value));
      button.disabled = !flags.allows(spec, value, target);
      button.addEventListener("click", () => {
        chosen.value = value;
        change();
      });
      group.append(button);
    }
    row.append(group);
  }
  for (const number of spec.numbers ?? []) {
    const label = document.createElement("label");
    label.className = "flag-number";
    label.append(document.createTextNode(number.name));
    const input = document.createElement("input");
    input.type = "number";
    input.min = number.min;
    input.max = number.max;
    input.value = chosen[number.name] ?? number.fallback;
    input.addEventListener("change", () => {
      chosen[number.name] = Number(input.value);
      change();
    });
    label.append(input);
    row.append(label);
  }
  return row;
}

/** Строка-замечание внутри карточки ключа. */
function notice(text) {
  const node = document.createElement("div");
  node.className = "flag-note";
  node.textContent = text;
  return node;
}

/** Красит сценарий как JSON: разбор свой, раскладчик строк общий. */
function paintScenario() {
  state.scenarioEditor.highlight({ marks: jsonSpans.spans(state.scenario) }, []);
}

function onEdit() {
  // Работа разошлась с сохранённым: запись черновика отложена, и до неё уходить со
  // страницы без вопроса нельзя.
  state.dirty = true;
  refresh();
  saveDraft();
}

function refresh() {
  const source = state.editor.value();
  // Пояснение не компилируется и не судится: у текста нет ни диагностик, ни вывода
  // цели, и позвать компилятор значило бы напечатать отказ на том, что моделью не
  // является.
  if (state.kind === "markdown") {
    state.editor.highlight({ marks: md.spans(source) }, []);
    showDiagnostics([]);
    showDoc();
    return;
  }
  const diagnostics = state.bridge.diagnostics(source);
  const tokens = state.bridge.tokens(source);
  state.editor.highlight(tokens, diagnostics.diagnostics ?? []);
  showDiagnostics(diagnostics.diagnostics ?? []);
  compile();
  drawScheme();
}

/**
 * Перестраивает схему по тексту, когда она показана.
 *
 * Показана ли схема, знает область кода (`state.shown`), а не область вывода:
 * схема - один из показов открытого файла, наравне с кодом и сценарием. Пока
 * признак брался у панели вывода, значение "схема" не появлялось там никогда, и
 * граф не перестраивался ни на правку модели, ни на открытие раскладки.
 *
 * Непоказанная схема не считается: граф модели на каждую правку текста - работа,
 * и печатать её в невидимую область незачем (то же правило, что у вывода цели).
 *
 * @param {boolean} opened схему только что открыли: после отрисовки лист
 *   проверяется на видимость (`Scheme.ensureVisible`).
 */
function drawScheme(opened = false) {
  if (state.shown !== "scheme" || !state.bridge || !state.scheme) return;
  if (state.kind === "markdown") {
    state.scheme.setGraph(null);
    return;
  }
  state.scheme.setGraph(state.bridge.graph(state.editor.value()));
  if (opened) state.scheme.ensureVisible();
}

/** Курсор в объявлении состояния подсвечивает его узел на схеме. */
const syncCursor = draft.debounce(() => {
  if (state.shown !== "scheme" || !state.scheme || !dom.editor.contains(document.activeElement)) return;
  const at = state.editor.position();
  if (at) state.scheme.highlight(at.line, at.character);
}, 120);

/** Показывает разметку пояснения: узлы строит `md.js`, а не `innerHTML`. */
function showDoc() {
  if (state.kind !== "markdown") return;
  dom.doc.replaceChildren(md.render(state.editor.value(), document));
}

/** Замечания разбора и семантики: приходят на каждую правку текста. */
function showDiagnostics(items) {
  state.diagnostics = items;
  drawDiagnostics();
}

/** Замечания цели генерации: приходят на сборку, а не на правку. */
function showTargetDiagnostics(items) {
  state.targetDiagnostics = items;
  drawDiagnostics();
}

/**
 * Рисует список диагностик - оба их источника разом.
 *
 * Место замечания компилятора одно, и это оно: пока отказ цели печатался ещё и
 * в окне генерации, автору приходилось смотреть в два места, а сходились они не
 * всегда - окно показывало отказ той сборки, которую строили последней.
 */
/**
 * Ключ записи диагностики: род, код, текст и место.
 *
 * По нему решается, новая ли это запись. Место входит в ключ намеренно: одна и
 * та же ошибка в двух строках - две разные новости для читателя.
 */
function diagnosticKey(item) {
  const at = item.range;
  const where = at ? `${at.start_line}:${at.start_character}` : "";
  return `${item.severity ?? "error"}|${item.code ?? ""}|${item.message}|${where}`;
}

/**
 * Дописывает в журнал диагностик то, чего в нём ещё не было.
 *
 * Журнал накапливает, а не перерисовывается: правка идёт за
 * правкой, и читателю важно видеть, что было сказано раньше. Отсюда два правила.
 * Первое - в журнал уходят только записи, которых не было в прошлом наборе:
 * иначе каждая нажатая клавиша дописывала бы туда всё те же три ошибки.
 * Второе - объём ограничен (настройка читателя, умолчание 500): страница живёт
 * часами, и список без предела растёт, пока браузер не начнёт спотыкаться на
 * его отрисовке. Уходят самые старые - новость важнее истории.
 */
function drawDiagnostics() {
  // Одна и та же ошибка приходит двумя путями: разбор судит текст, а цель
  // отказывает на нём же - и в списке она стояла бы дважды. Повтор снимается
  // сравнением, а не запретом показывать отказ при живых диагностиках: цель
  // отказывает и там, где разбор молчит.
  const items = [
    ...state.diagnostics,
    ...state.targetDiagnostics.filter(
      (item) => !state.diagnostics.some((shown) => sameDiagnostic(shown, item))
    ),
  ];
  const before = new Set(state.diagLast.map(diagnosticKey));
  state.diagLast = items;
  const fresh = items.filter((item) => !before.has(diagnosticKey(item)));
  if (fresh.length === 0) {
    paintDiagnosticsEmpty();
    return;
  }
  // Читатель, стоящий внизу, следит за новостями: список довозится к концу.
  // Отлистанный вверх - читает старое, и уезжать из-под него нельзя.
  const atEnd =
    dom.diagnostics.scrollTop + dom.diagnostics.clientHeight >= dom.diagnostics.scrollHeight - 4;
  const empty = dom.diagnostics.querySelector(".row-none");
  if (empty) empty.remove();
  for (const item of fresh) {
    // Диагностика показывается словами, а не только подчёркиванием: цвет - не
    // единственный носитель состояния.
    //
    // Позиция печатается, только когда она есть: у отказа цели координаты
    // может не быть вовсе, и "1:1" указывало бы на начало файла - место
    // достовернее отсутствующего лишь на вид.
    const at = item.range;
    const where = at ? `${at.start_line + 1}:${at.start_character + 1}: ` : "";
    const code = item.code ? `[${item.code}] ` : "";
    const node = row(`${where}${code}${item.message}`, item.severity ?? "error");
    if (at) node.addEventListener("click", () => jump(at.start_line, at.start_character));
    dom.diagnostics.appendChild(node);
  }
  rotateDiagnostics();
  if (atEnd) dom.diagnostics.scrollTop = dom.diagnostics.scrollHeight;
  markDiagnosticsMore();
}

/** Пустой журнал говорит словами: пустая область читалась бы как поломка. */
function paintDiagnosticsEmpty() {
  const shown = dom.diagnostics.querySelector(".row-none .row-text");
  if (dom.diagnostics.children.length > 0) {
    // Заглушка переписывается на каждой отрисовке: словарь грузится своим
    // рейсом, и первая отрисовка успевает случиться раньше него - тогда в
    // журнале стоял бы ключ вместо слов.
    if (shown) shown.textContent = t("diagnostics.none");
    markDiagnosticsMore();
    return;
  }
  const node = row(t("diagnostics.none"), "ok");
  node.classList.add("row-none");
  dom.diagnostics.appendChild(node);
  markDiagnosticsMore();
}

/** Ротация журнала: сверх объёма уходят самые старые записи. */
function rotateDiagnostics() {
  while (dom.diagnostics.children.length > state.diagKeep) {
    dom.diagnostics.firstElementChild.remove();
  }
}

/**
 * Очищает показанную запись: диагностики либо журнал прогона.
 *
 * Кнопка говорит о том, что перед глазами: чисти она обе, читатель терял бы
 * невидимую половину, о которой не просил.
 */
function clearDiagnostics() {
  if (state.diagTab === "trace") {
    dom.trace.replaceChildren();
    return;
  }
  state.diagLast = [];
  dom.diagnostics.replaceChildren();
  paintDiagnosticsEmpty();
}

/**
 * Отмечает, что записи есть и ниже видимой кромки.
 *
 * Затенение ставится по факту прокрутки, а не правилом: правило блёкло бы и на
 * последней записи, под которой ничего нет.
 */
function markDiagnosticsMore() {
  const more =
    dom.diagnostics.scrollHeight - dom.diagnostics.scrollTop - dom.diagnostics.clientHeight > 2;
  dom.diagnostics.classList.toggle("more", more);
}

function jump(line, character) {
  // Своего обхода узлов здесь нет: правило "смещение ↔ точка DOM" живёт в редакторе
  // одним носителем. Пока его считали по месту, три места считали по-разному, и
  // переводы строк не знало ни одно (см.
  state.editor.moveTo(line, character);
}

/** Подсказка при наведении: тип и объявление под курсором мыши. */
const onHover = draft.debounce((event) => {
  const at = state.editor.positionAt(event.clientX, event.clientY);
  if (!at) return;
  const reply = state.bridge.hover(state.editor.value(), at.line, at.character);
  // Пусто - не ошибка: под курсором просто нет имени, и молчание здесь верно.
  say(reply.ok ? (reply.contents ?? "") : "", "ok");
}, 120);

/** Переход к объявлению и показ использований символа под курсором. */
function declarationAndUses() {
  const at = state.editor.position();
  const source = state.editor.value();
  const uses = state.bridge.references(source, at.line, at.character);
  const target = state.bridge.goto(source, at.line, at.character);
  if (target.ok && target.range) {
    jump(target.range.start_line, target.range.start_character);
    say(
      t("editor.declaration", {
        line: target.range.start_line + 1,
        uses: uses.ranges?.length ?? 0,
      }),
      "ok"
    );
    return;
  }
  say(t("editor.noSymbol"), "warning");
}

/** Переименование символа под курсором. */
function renameSymbol() {
  const at = state.editor.position();
  const source = state.editor.value();
  const newName = prompt(t("editor.renamePrompt"));
  if (!newName) return;
  const reply = state.bridge.rename(source, at.line, at.character, newName);
  if (!reply.ok) {
    // Отказ переименования назван причиной слоя: "полнота или отказ" - его правило, и
    // прятать причину значило бы оставить автора в догадках.
    say(reply.error?.message ?? t("editor.renameUnavailable"), "warning");
    return;
  }
  // Правки применяются С конца: иначе каждая сдвигала бы координаты следующих, и текст
  // расползся бы.
  const edits = [...(reply.edits ?? [])].sort(
    (a, b) => b.range.start_line - a.range.start_line || b.range.start_character - a.range.start_character
  );
  let text = source;
  for (const edit of edits) {
    const from = positionToOffset(text, edit.range.start_line, edit.range.start_character);
    const to = positionToOffset(text, edit.range.end_line, edit.range.end_character);
    text = text.slice(0, from) + edit.new_text + text.slice(to);
  }
  state.editor.setValue(text);
  // Ключи раскладки едут за именем: иначе каждое переименование роняло бы позицию.
  const first = edits[0];
  if (first) {
    const from = positionToOffset(source, first.range.start_line, first.range.start_character);
    const to = positionToOffset(source, first.range.end_line, first.range.end_character);
    state.scheme.renamed(source.slice(from, to), newName);
  }
  say(t("editor.renamed", { count: edits.length }), "ok");
}

/** Компилирует текущей целью и показывает вывод. */
function compile() {
  // Панель закрыта - генерации нет вовсе: печатать в невидимую область работа впустую.
  // Диагностика при этом идёт своим путём: её показывает область под моделью. Мост
  // может быть ещё не загружен: панель выбирается до модуля (страница помнит её с
  // прошлого раза), и без этой проверки старт падает целиком - страница показывает
  // "модуль не загрузился" при живом модуле.
  if (!state.bridge || state.panel !== "output") {
    showTargetDiagnostics([]);
    return;
  }
  const reply = state.bridge.compile(
    state.target,
    state.args,
    state.editor.value(),
    state.file
  );
  dom.output.replaceChildren();
  if (!reply.ok) {
    // Отказ цели - замечание к модели, и место у него то же, что у прочих:
    // панель диагностики. Область вывода при этом не молчит - пустая область
    // читается как поломка, - но говорит она о себе: строить нечего.
    showTargetDiagnostics([refusal(reply.error)]);
    dom.output.appendChild(row(t("output.empty"), "ok"));
    return;
  }
  for (const file of reply.files ?? []) {
    const header = document.createElement("div");
    header.className = "file-name";
    header.textContent = file.name;
    const body = document.createElement("pre");
    body.className = "file-text";
    paintOutput(body, file.text, header);
    dom.output.append(header, body);
  }
  // Предупреждения цели - тоже её замечания к модели: они идут туда же, куда
  // отказ, и рядом с ними стоят замечания разбора.
  showTargetDiagnostics(
    (reply.warnings ?? []).map((warning) => ({
      code: warning.code,
      message: warning.message,
      severity: "warning",
    }))
  );
}

/** Две диагностики об одном: код, место и текст. */
function sameDiagnostic(a, b) {
  return (
    a.code === b.code &&
    a.message === b.message &&
    a.range?.start_line === b.range?.start_line &&
    a.range?.start_character === b.range?.start_character
  );
}

/**
 * Отказ цели в форме диагностики.
 *
 * Координата у цели считается от единицы (её печатает `taktc`), а список
 * диагностик - от нуля: разница снимается здесь, в одном месте. Отказ без
 * координаты остаётся без неё - выдуманное место хуже отсутствующего.
 */
function refusal(error) {
  const at = error?.line
    ? { start_line: error.line - 1, start_character: (error.column ?? 1) - 1 }
    : null;
  return {
    code: error?.code,
    message: error?.message ?? t("output.refused"),
    severity: "error",
    ...(at ? { range: at } : {}),
  };
}

/**
 * Красит порождённый файл по правилам его языка (задача,).
 *
 * Отрезки приходят от модуля (`takt_highlight`) в той же форме, что токены
 * исходника, и раскладывает их тот же `paintCode`: своего разбора C, ST, Rust,
 * SystemVerilog или PlantUML в браузере нет - он разошёлся бы и с целями, и с
 * подсветкой блоков кода в документе.
 */
function paintOutput(body, text, header) {
  if (text.length > HIGHLIGHT_LIMIT) {
    body.textContent = text;
    header.append(note(t("output.noHighlight", { limit: HIGHLIGHT_LIMIT })));
    return;
  }
  const reply = state.bridge.highlight(state.target, text);
  if (!reply.ok) {
    // Отказ подсветки - не отказ сборки: файл показывается как есть, а причина
    // называется. Молчаливый чёрный текст выглядел бы дефектом вёрстки.
    body.textContent = text;
    header.append(note(reply.error?.message ?? t("output.highlightFailed")));
    return;
  }
  header.append(note(reply.language));
  body.replaceChildren(paintCode(text, spans(reply)));
}

/** Приписка у имени файла: язык вывода либо причина, по которой цвета нет. */
function note(text) {
  const node = document.createElement("span");
  node.className = "file-note";
  node.textContent = text;
  return node;
}

/**
 * Приводит открытый файл к канону его языка.
 *
 * Кнопка одна, а канонов два: модель приводит к канону компилятор (`taktc fmt`
 * тем же кодом, что на диске), сценарий - печать JSON. Родов, у которых канона
 * нет (пояснение, раскладка), кнопка не касается: она гасится в `showKind`, и
 * этот отказ - защита в глубину.
 */
function format() {
  const canon = canonOf();
  if (canon === "scenario") {
    formatScenario();
    return;
  }
  if (canon !== "takt") {
    say(t("editor.formatNotForKind"), "warning");
    return;
  }
  const reply = state.bridge.format(state.editor.value());
  if (!reply.ok) {
    say(reply.error?.message ?? t("editor.formatUnavailable"), "warning");
    return;
  }
  if (reply.text === null || reply.text === undefined) {
    say(t("editor.alreadyFormatted"), "ok");
    return;
  }
  state.editor.setValue(reply.text);
  say(t("editor.formatted"), "ok");
}

/**
 * Канон сценария - печать JSON с отступом.
 *
 * Неразбираемый текст не форматируется: печать по половине разбора потеряла бы
 * недописанное, а сценарий правят на странице и почти всегда видят
 * недописанным. Отказ называет место - разбор JSON его знает.
 */
function formatScenario() {
  const done = jsonSpans.format(state.scenario);
  if (!done.ok) {
    say(t("editor.formatBadJson", { error: done.error }), "warning");
    return;
  }
  if (done.text === state.scenario) {
    say(t("editor.alreadyFormatted"), "ok");
    return;
  }
  state.scenarioEditor.setValue(done.text);
  state.scenario = done.text;
  paintScenario();
  saveDraft();
  say(t("editor.formatted"), "ok");
}

/**
 * Поток прогона: один на страницу, сессия в нём живёт между шагами и прогонами.
 *
 * Адрес - от этого модуля: собранная страница лежит в каталоге бандла, и адрес от
 * документа увёл бы запрос в корень.
 */
function worker() {
  if (!state.worker) {
    state.worker = new Worker(new URL("worker.js", import.meta.url), { type: "module" });
    state.worker.onmessage = (event) => onWorker(event.data ?? {});
  }
  return state.worker;
}

/** Что нужно потоку, чтобы открыть либо продолжить сессию. */
function session() {
  return {
    wasmUrl: state.build?.wasm ?? new URL(WASM_DEFAULT, location.href).href,
    source: state.editor.value(),
    scenario: state.scenario,
    tickMs: 0,
  };
}

/** Запускает прогон в отдельном потоке: до конца модели либо до бюджета. */
function run() {
  if (state.running) return;
  showRun();
  state.running = true;
  setRunButtons({ run: true, step: true, stop: false });
  worker().postMessage({ type: "run", ...session(), budget: Number(dom.budget.value) || 10_000 });
}

/** Один такт: продолжает открытую сессию либо открывает новую по текущему тексту. */
function stepOnce() {
  if (state.running) return;
  showRun();
  worker().postMessage({ type: "step", ...session() });
}

/**
 * Куда смотреть во время прогона.
 *
 * Ход показывает схема: узлы подсвечиваются, а журнал идёт под холстом. Открыта
 * она - трогать нечего; открыто другое - схема и открывается, иначе прогон идёт
 * молча.
 */
function showRun() {
  if (!panel("scheme")?.hidden) return;
  showSource("scheme");
}

function stop() {
  state.worker?.postMessage({ type: "stop" });
}

/**
 * Сброс: автомат возвращается в начальное состояние.
 *
 * Трасса при этом остаётся: прежний прогон стоит рядом с новым,
 * и их можно сличить. Подсветка со схемы снимается - активных состояний больше нет,
 * а оставленная подсветка говорила бы о ходе, которого не идёт.
 */
function resetRun() {
  if (state.running) return;
  state.worker?.postMessage({ type: "reset" });
  state.scheme?.setRunning([]);
}

function onWorker(message) {
  switch (message.type) {
    case "opened":
      // Новая сессия - новая трасса: прежние строки принадлежали другому тексту либо
      // законченному прогону.
      dom.trace.replaceChildren();
      state.scheme.setRunning([]);
      break;
    case "stepped":
      break;
    case "reset":
      // Сессия закрыта: следующий пуск начнёт с первого такта. Трасса остаётся -
      // её чистит открытие новой сессии, и до пуска сличать есть что.
      say(t("trace.wasReset"), "ok");
      break;
    case "lines":
      for (const line of message.lines) dom.trace.appendChild(row(line, "trace"));
      dom.trace.scrollTop = dom.trace.scrollHeight;
      // Схема подсвечивает активные состояния последнего такта порции: список приходит
      // от эталона, разбирать строку трассы страница не вправе.
      if (message.states?.length) {
        const last = message.states.length - 1;
        state.scheme.setRunning(message.states[last], message.next?.[last] ?? []);
      }
      break;
    case "warnings":
      // Код показывается отдельно от текста - как у предупреждений компиляции.
      for (const item of message.items ?? []) {
        const place = item.step ? t("trace.warningStep", { step: item.step }) : "";
        const code = item.code ? `[${item.code}] ` : "";
        dom.trace.appendChild(row(`${code}${place}${item.message}`.trim(), "warning"));
      }
      break;
    case "output":
      // Вывод модели - не замечание к ней: своя строка и свой цвет.
      for (const line of message.lines) dom.trace.appendChild(row(line, "output"));
      break;
    case "finished":
      for (const line of message.info ?? []) dom.trace.appendChild(row(line, "ok"));
      for (const line of message.errors ?? []) dom.trace.appendChild(row(line, "error"));
      // По завершении подсветка прогона снимается: узел "в прогоне" говорит о текущем
      // такте, а текущего такта больше нет. Остановленный прогон подсветку держит: его
      // продолжают шагами.
      finish();
      state.scheme.setRunning([]);
      break;
    case "halted":
      // Останов называется словами - и по бюджету, и по просьбе автора: молчаливо
      // оборванный прогон неотличим от завершившегося.
      dom.trace.appendChild(row(t(message.key, message.params), "warning"));
      finish();
      break;
    case "failed":
      // Текст модуля сильнее ключа оболочки: он называет причину точнее, чем общее
      // "такт не выполнен", и это его язык, а не наш.
      dom.trace.appendChild(row(message.message ?? t(message.key, message.params), "error"));
      finish();
      state.scheme.setRunning([]);
      break;
    default:
      break;
  }
}

function finish() {
  state.running = false;
  setRunButtons({ run: false, step: false, stop: true });
}

/** Кладёт состояние редактора в адресную строку и в буфер обмена. */
async function share() {
  const fragment = await encodeState({
    version: state.version,
    source: state.editor.value(),
    scenario: state.scenario,
    target: state.target,
    args: state.args,
    // Раскладка едет в ссылке только непустой: пустая - умолчание, и место в ссылке ей
    // не нужно.
    layout: state.scheme.text() === layoutFile.canonical(layoutFile.empty()) ? "" : state.scheme.text(),
  });
  if (await build.outdated(state.build)) dom.update.hidden = false;
  const url = `${location.origin}${location.pathname}#${fragment}`;
  history.replaceState(null, "", `#${fragment}`);
  try {
    await navigator.clipboard.writeText(url);
    say(t("share.copied", { length: url.length }), "ok");
  } catch {
    // Буфер обмена требует разрешения и жеста; ссылка уже в адресной строке - сказать
    // об этом важнее, чем промолчать об отказе.
    say(t("share.inAddressBar"), "warning");
  }
}

/**
 * Выбирает вкладку внутри панели генерации: вывод цели либо ключи сборки.
 *
 * Симуляция вкладкой больше не является:
 * у неё своя панель, и открывается она своей кнопкой.
 */
function selectTab(name) {
  for (const tab of dom.tabs.querySelectorAll("[data-tab]")) {
    const active = tab.dataset.tab === name;
    tab.classList.toggle("active", active);
    tab.setAttribute("aria-selected", String(active));
  }
  for (const panel of panels("output", "flags")) {
    panel.hidden = panel.dataset.panel !== name;
  }
  state.tab = name;
}

/**
 * Показывает панель правой области: генерацию, симуляцию либо ни одной.
 *
 * Скрытая генерация не выполняется: при закрытой панели проверяются только
 * синтаксис и семантика. Печатать вывод в
 * невидимую область - работа впустую, а на большой модели она заметна.
 *
 * Панель ровно одна: область под них одна, и держать в ней два ответа
 * сразу негде. Обе отжаты - область уходит целиком, и модель занимает экран.
 */
/**
 * Панель правой области по имени.
 *
 * Ищется внутри рабочей области, а не по документу: `data-panel` стоит и на `body`
 * (текущая панель для стилей), и запрос по документу первым находил бы `body`, когда
 * его значение совпадает с искомым, - панель при этом оставалась бы на экране.
 */
function panel(name) {
  return document.querySelector(`.work [data-panel="${name}"]`);
}

function panels(...names) {
  return names.map(panel).filter(Boolean);
}

/**
 * Показывает или прячет журнал диагностик.
 *
 * Правило смотрит на состояние страницы (`body[data-diag]`), а не прячет узлы
 * по одному: дорожка сетки, оставшаяся от спрятанной области, держала бы пустое
 * место, а ручка, которой нечего делить, тянулась бы в пустоту.
 */
function showDiagnosticsPane(show) {
  document.body.dataset.diag = show ? "on" : "off";
  dom.hsplit.hidden = !show;
  dom["diagnostics-head"].hidden = !show;
  if (show) showDiagTab(state.diagTab);
  else {
    dom.diagnostics.hidden = true;
    dom.trace.hidden = true;
  }
  dom.showdiag.setAttribute("aria-pressed", String(show));
  shell.remember(localStorage, shell.UI_KEYS.diagnostics, show ? "1" : "0");
}

/**
 * Показывает или прячет путь открытого файла и ревизию в шапке.
 *
 * Это сведения о месте работы, а не о самой работе: кто помнит, где он, вправе
 * освободить строку. Признак стоит на `body` - показ решает одно правило, а не
 * перечисление узлов по одному.
 */
function showCrumbs(show) {
  state.crumbsShown = show;
  document.body.dataset.crumbs = show ? "on" : "off";
  shell.remember(localStorage, shell.UI_KEYS.crumbs, show ? "1" : "0");
}

/**
 * Ставит структуру проекта на выбранную сторону.
 *
 * Сторона - настройка читателя: место рабочего инструмента выбирает тот, кто им
 * работает. Раскладку меняет одно правило по признаку на `body`; ручка размера
 * спрашивает сторону сама и меняет вместе с ней ось.
 */
function showTreeSide(side) {
  state.treeSide = shell.TREE_SIDES.includes(side) ? side : shell.TREE_SIDE_DEFAULT;
  document.body.dataset.treeside = state.treeSide;
  shell.remember(localStorage, shell.UI_KEYS.treeSide, state.treeSide);
  measureTree();
}

/**
 * Наименьший размер структуры проекта: столько, чтобы имена файлов были видны.
 *
 * Меряется содержимое, а не берётся число из стиля: имена файлов пишет автор, и
 * их длину знает только шрифт. Верхняя граница - половина экрана: имя длиннее
 * неё сокращается затенением, а не растит панель через всю страницу.
 */
function measureTree() {
  if (!dom.tree) return 0;
  const vertical = state.treeSide === "top" || state.treeSide === "bottom";
  if (vertical) {
    // Стоя сверху или снизу, структура делит высоту, и мера у неё другая: шапка,
    // полоса действий и хотя бы одна запись. Имена там уходят вширь, где места
    // столько же, сколько у страницы.
    const head = dom.tree.parentElement?.firstElementChild?.offsetHeight ?? 0;
    const tools = dom.tree.parentElement?.querySelector(".tree-tools")?.offsetHeight ?? 0;
    const line = dom.tree.querySelector(".tree-file")?.offsetHeight ?? 24;
    const room = Math.min(window.innerHeight / 2, head + tools + line * 2);
    document.documentElement.style.setProperty("--tree-min-y", `${Math.round(room)}px`);
    return room;
  }
  // Меряется текст, а не узел: строка дерева растянута во всю его ширину, и
  // ширина узла отвечала бы шириной панели. Пока мерили её, нижняя граница
  // росла вслед за растянутой панелью, и сузить дерево было уже нечем.
  const ruler = document.createRange();
  let need = 0;
  for (const node of dom.tree.querySelectorAll(".tree-file, .tree-kind")) {
    ruler.selectNodeContents(node);
    need = Math.max(need, ruler.getBoundingClientRect().width);
  }
  // Полоса действий тоже часть области, но мерится она **одной кнопкой**, а не
  // собой: кнопки переносятся по строкам, и ширина полосы всегда равна ширине
  // панели - взяв её, нижняя граница росла бы вслед за растянутой панелью, и
  // сузить дерево было бы уже нечем.
  const button = dom.tree.parentElement?.querySelector(".tree-tools .icon-btn:not([hidden])");
  const least = button ? button.getBoundingClientRect().width + TREE_PADDING : 0;
  const room = Math.min(window.innerWidth / 2, Math.max(need + TREE_PADDING, least));
  document.documentElement.style.setProperty("--tree-min", `${Math.round(room)}px`);
  return room;
}

/** Поля области дерева: отступы записи и место под полосу прокрутки. */
const TREE_PADDING = 32;

/**
 * Наименьшая высота журнала диагностик: шапка и одна запись.
 *
 * Меряется по месту, а не берётся числом: высота шапки и строки зависят от
 * кегля, а его выбирает читатель.
 */
function diagLeast() {
  const head = dom["diagnostics-head"]?.offsetHeight ?? 0;
  const line = dom.diagnostics?.firstElementChild?.offsetHeight ?? 22;
  const least = head + line;
  document.documentElement.style.setProperty("--diag-min", `${Math.round(least)}px`);
  return least;
}

/**
 * Показывает либо прячет структуру проекта.
 *
 * Правило смотрит на состояние страницы (`body[data-tree]`), а не прячет узлы
 * по одному: иначе ручка ширины осталась бы висеть у края и тянула бы долю
 * области, которой на экране нет.
 */
function showTree(show) {
  document.body.dataset.tree = show ? "on" : "off";
  dom.showtree.setAttribute("aria-pressed", String(show));
  shell.remember(localStorage, shell.UI_KEYS.treeShown, show ? "1" : "0");
}

/**
 * Показывает в области кода то, чем открытый файл смотрят.
 *
 * Область одна на все роды файлов: читатель работает с тем, что выбрал в
 * структуре проекта, и вторая область под сценарий или схему заставляла бы его
 * помнить, где что живёт.
 */
function showSource(what) {
  const scheme = panel("scheme");
  state.shown = what;
  // "none" - показывать нечего (проект закрыт): все показы гаснут разом, и
  // подпись области молчит вместе с ними.
  // Подпись области и имя рядом с ней говорят о показанном, а не об открытом
  // файле: сценарий и раскладку открывают, не меняя рода, и подпись "Модель" над
  // сценарием читалась бы как потерянная модель.
  const own = state.kind === "markdown" ? "source.titleDoc" : "source.title";
  const shownKey = what === "scenario" ? "source.titleScenario" : "source.titleScheme";
  const key = what === "code" || what === "none" ? own : shownKey;
  dom.sourcetitle.dataset.i18n = key;
  dom.sourcetitle.textContent = t(key);
  dom.openfilename.textContent = what === "scenario" ? state.scenarioFile : state.file;
  dom.editor.hidden = what !== "code";
  dom.scenario.hidden = what !== "scenario";
  if (scheme) scheme.hidden = what !== "scheme";
  if (what === "scheme") drawScheme(true);
  // Канон применяется к тому, что показано, а не к роду открытого файла: сценарий
  // и раскладку открывают, не меняя рода (щелчок по сценарию не подменяет модель),
  // и по роду кнопка судила бы о невидимом тексте.
  if (dom.format) dom.format.disabled = !HAS_CANON.has(canonOf());
}

/** Канон того, что показано: `takt` - модель, `scenario` - JSON, иначе - никакого. */
function canonOf() {
  if (state.shown === "scenario") return "scenario";
  if (state.shown === "code" && state.kind !== "markdown") return "takt";
  return "";
}

function selectPanel(name) {
  state.panel = name;
  dom.showgen.setAttribute("aria-pressed", String(name === "output"));
  // Пояснение компилировать нечем: вкладка "Ключи сборки" говорила бы о сборке,
  // которой не будет, и область вывода у пояснения закрыта.
  const doc = state.kind === "markdown";
  dom.tabs.hidden = name !== "output" || doc;
  if (name === "output" && !doc) selectTab(state.tab === "flags" ? "flags" : "output");
  if (name !== "output" || doc) {
    for (const hidden of panels("output", "flags")) hidden.hidden = true;
  }
  // У пояснения место вывода цели занимает показ разметки: компилировать его
  // нечем, а видеть результат рядом с текстом надо.
  dom.doc.hidden = !(doc && name === "output");
  if (doc && name === "output") showDoc();
  document.body.dataset.panel = name ?? "none";
  // Погашенная панель пишется словом, а не пустой строкой: пустую запись
  // `setting` читает как "выбора не было" и отвечает умолчанием - и погашенный
  // вывод цели возвращался при каждой перезагрузке.
  shell.remember(localStorage, shell.UI_KEYS.panel, name ?? "none");
  // Открыли генерацию - вывод обязан быть свежим: пока панель была закрыта, правки
  // модели в него не печатались. Схема - по тому же правилу, и вдобавок вид листа
  // проверяется: закрыть панель могли с отведённым в сторону холстом.
  if (name === "output") compile();
}

/**
 * Выбирает область на узком экране: модель, вывод или прогон.
 *
 * Слушателя изменения размера окна здесь нет и не нужно: какая раскладка
 * действует, решает CSS по ширине. Расширив окно, автор видит обе области при
 * любом выбранном режиме - а `resize`-обработчик пришлось бы держать
 * согласованным с медиазапросом, то есть завести второй носитель порога.
 */
function selectMode(name) {
  document.body.dataset.mode = name;
  for (const mode of dom.modes.querySelectorAll("[data-mode]")) {
    const active = mode.dataset.mode === name;
    mode.classList.toggle("active", active);
    mode.setAttribute("aria-selected", String(active));
  }
  // Область на узком экране открывается вместе со своим режимом: выбрав её в
  // полке, читатель просит показать именно её, и отжатая кнопка показа спорила
  // бы с этой просьбой.
  if (name === "output") selectPanel("output");
  if (name === "tree") showTree(true);
}

/**
 * Строка списка: диагностика, замечание прогона, строка трассы.
 *
 * У записи журнала есть колонка рода - слово, а не только цвет: цветом одним
 * состояние не передаётся (правило доступности книги оформления), да и рода у
 * прогона три - шаг, замечание, вывод модели. У шага колонка пуста: строка сама
 * называет себя тактом, и слово "шаг" стояло бы дважды.
 */
function row(text, kind) {
  const node = document.createElement("div");
  node.className = `row row-${kind}`;
  const known = ROW_KINDS[kind];
  if (known) {
    const mark = document.createElement("span");
    mark.className = "row-kind";
    mark.textContent = t(known.label);
    node.appendChild(mark);
  }
  const body = document.createElement("span");
  body.className = "row-text";
  body.textContent = text;
  node.appendChild(body);
  return node;
}

/**
 * Помечает запись списка как прочитанную; повторный щелчок отметку снимает.
 *
 * Отметка всегда одна: это место, к которому вернулись, а не отбор строк.
 */
function markRow(list, line) {
  const was = line.getAttribute("aria-selected") === "true";
  for (const other of list.querySelectorAll('[aria-selected="true"]')) {
    other.removeAttribute("aria-selected");
  }
  if (!was) line.setAttribute("aria-selected", "true");
}

/** Слово рода записи; рода без слова колонки не получают. */
const ROW_KINDS = {
  warning: { label: "trace.kindWarning" },
  output: { label: "trace.kindOutput" },
  error: { label: "trace.kindError" },
};

/**
 * Системное сообщение: сбой инструмента, а не замечание к модели.
 *
 * Замечания компилятора и эталона идут в свои области (список диагностик,
 * трасса) - они относятся к тексту автора и живут, пока живёт ошибка в нём.
 * Сюда попадает то, о чём автору сказать нечего: не загрузился модуль, отказал
 * запрос, упал обработчик. Молчать об этом нельзя - неработающий инструмент
 * выглядит работающим, пока о нём не сказано.
 */
function alarm(error) {
  const text = alerts.textOf(error);
  if (!text) return;
  alerts.show(dom.alerts, t("alerts.system", { error: text }), t("alerts.dismiss"));
}

/**
 * Доступность кнопок прогона.
 *
 * Кнопки ищутся признаком: их место - строка уровня схемы, но правило "какая
 * кнопка сейчас погашена" принадлежит прогону, а не месту. Заведи кнопку в
 * другой области - она получит то же состояние, не требуя правки здесь.
 *
 * @param {{run: boolean, step: boolean, stop: boolean}} off какие погасить
 */
function setRunButtons(off) {
  for (const [name, disabled] of Object.entries(off)) {
    for (const button of document.querySelectorAll(`[data-run="${name}"]`)) button.disabled = disabled;
  }
}

/**
 * Показывает одну из двух записей области: диагностики либо журнал прогона.
 *
 * Записи об одном и том же - что случилось с моделью, - и место у них одно:
 * читать их порознь значило бы держать в голове два экрана вместо одного.
 */
function showDiagTab(which) {
  state.diagTab = which === "trace" ? "trace" : "diagnostics";
  const trace = state.diagTab === "trace";
  dom.diagnostics.hidden = trace;
  dom.trace.hidden = !trace;
  dom["showdiag-tab"].setAttribute("aria-pressed", String(!trace));
  dom["showtrace-tab"].setAttribute("aria-pressed", String(trace));
  shell.remember(localStorage, shell.UI_KEYS.diagTab, state.diagTab);
}

/**
 * Говорит автору, что инструмент сделал и чего не смог.
 *
 * Место одно - полоса внизу страницы: сообщение застаёт автора в любой из
 * областей, и строка в шапке одной из них оставалась бы незамеченной в двух
 * других. Текст короток по правилу книги оформления: полоса постоянной высоты
 * не переносит строку, а обрезает её.
 *
 * Замечания к модели сюда не попадают: их место - панель диагностики.
 */
function say(text, kind) {
  dom.say.textContent = text;
  dom.say.className = `say say-${kind}`;
}

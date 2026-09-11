// Вход, список проектов и явное сохранение.
//
// # Чего здесь нет
//
// Правил доступа: что автор может с проектом, говорит сервер словом (`level`), и
// страница только показывает. Второй список правил разошёлся бы с первым молча - и
// разошёлся бы в сторону "кнопка есть, а сохранить нельзя".
//
// # Конфликт
//
// Расхождение ревизий показывается **обеими датами и обоими числами**, выбор - за
// автором: "перечитать" либо "перезаписать". Молчаливого выбора нет ни в одну сторону:
// перезаписать чужую работу и потерять свою - одинаково плохо.

import { pickScenario } from "./project.js";
import * as api from "./api.js";
import * as draft from "./draft.js";
import * as layoutFile from "./layout.js";
import { layoutName, modelName } from "./layout.js";
import * as shell from "./shell.js";
import { SAMPLE } from "./sample.js";
import { t } from "./i18n.js";

/**
 * Как называется первый файл нового проекта.
 *
 * Имя судится сервером как имя модели: латиница, цифры, `_`, `-` и
 * расширение. Переименование файла - задача не этой страницы.
 */
const DEFAULT_FILE = "model.takt";

/**
 * Имя модели нового проекта: сам проект и даёт его.
 *
 * Файлы проекта носят его имя (модель, раскладка, сценарии, пояснение), и
 * первый файл - не исключение. Имя проекта шире имени файла (пробелы,
 * кириллица), поэтому негодное для файла имя сводится к общему `model.takt`:
 * отказать в заведении проекта из-за имени файла было бы подменой предмета.
 */
function firstFileName(project) {
  return /^[A-Za-z0-9_-]+$/.test(project) ? `${project}.takt` : DEFAULT_FILE;
}

/** Что открыто и чем это можно править. */
const state = {
  /** Метаданные открытого проекта либо `null`. */
  project: null,
  /** Имя открытого файла. */
  file: null,
  /**
   * Имя файла, выбранного в структуре проекта.
   *
   * Не то же, что открытый: сценарий и раскладку открывают, не меняя открытого
   * файла, и действия над файлом обращены именно к выбранному.
   */
  picked: null,
  /** Проект, назначенный к удалению: строка списка, а не открытый проект. */
  doomed: null,
  /**
   * Проект, выбранный в списке панели.
   *
   * Не то же, что открытый: список стоит на экране всегда, и действия над
   * проектом (открыть, переименовать, удалить) обращены к выбранному.
   */
  chosen: null,
  /** Ревизия проекта на момент чтения файла. */
  revision: null,
  /** Мой уровень доступа к открытому проекту. */
  level: "none",
  /** Ждущий решения конфликт: `{seen, actual, text}`. */
  conflict: null,
  /** Выбранный сценарий прогона: их в проекте бывает несколько. */
  scenarioFile: null,
  /** Текст сценария, каким его отдал сервер: по нему видно, правил ли автор. */
  scenarioRead: "",
};

/**
 * Лента витрины: чем спросить следующую страницу, знает она.
 *
 * Курсор здесь не считается и не хранится россыпью по обработчикам: правило
 * "спросить тем, что дал сервер, и остановиться, когда он молчит" живёт одним
 * носителем и проверяется без браузера.
 */

/** Узлы страницы и обратные вызовы, которые даёт `app.js`. */
let dom = null;
let host = null;

/**
 * Подключает панель.
 *
 * @param {object} nodes узлы страницы
 * @param {{source: () => string, scenario: () => string,
 *          open: (state: object) => void, say: (text: string, kind: string) => void}} callbacks
 */
export function attach(nodes, callbacks) {
  dom = nodes;
  host = callbacks;
  // Список проектов - часть панели, а не окна: он рисуется сразу, и без входа
  // говорит, что для списка нужен вход.
  list();
  dom["whoami-bar"].addEventListener("click", () => toggle());
  // Кнопка одна на вход и выход: пока не вошли - открывает панель со формой, после
  // входа - выходит. Двух кнопок, из которых всегда видна одна, читателю не нужно
  //
  dom.session.addEventListener("click", () => (api.who() ? leave() : openSignin()));
  dom["signin-cancel"].addEventListener("click", closeSignin);
  // Окно закрывается щелчком по затемнению и клавишей Escape - как всякое модальное: из
  // разговора обязан быть выход, не требующий попадания в кнопку.
  dom["signin-modal"].addEventListener("click", (event) => {
    if (event.target === dom["signin-modal"]) closeSignin();
  });
  document.addEventListener("keydown", (event) => {
    if (event.key === "Escape" && !dom["signin-modal"].hidden) closeSignin();
  });
  dom.signin.addEventListener("click", () => enter(api.signIn));
  dom.signup.addEventListener("click", () => enter(api.register));
  dom.signout.addEventListener("click", () => leave());
  dom.newproject.addEventListener("click", () => openCreate());
  dom.createproject.addEventListener("click", () => make());
  dom.createcancel.addEventListener("click", () => closeModal(dom["project-modal"]));
  dom.newname.addEventListener("keydown", (event) => {
    if (event.key === "Enter") make();
  });
  // Проект выбирают в списке, а действия над ним стоят над списком: открыть,
  // переименовать, удалить.
  dom.openproject.addEventListener("click", () => openChosen());
  dom.renameproject.addEventListener("click", () => openProjectRename());
  dom.projectnameok.addEventListener("click", () => renameProject());
  dom.projectnamecancel.addEventListener("click", () => closeModal(dom["projectname-modal"]));
  dom.projectname.addEventListener("keydown", (event) => {
    if (event.key === "Enter") renameProject();
  });
  dom.dropproject.addEventListener("click", () => openDropChosen());
  dom.dropok.addEventListener("click", () => drop());
  dom.dropcancel.addEventListener("click", () => closeModal(dom["drop-modal"]));
  dom.closeproject.addEventListener("click", () => closeProject());
  dom.newfile.addEventListener("click", () => openNewFile());
  dom.fileok.addEventListener("click", () => makeFile());
  dom.filecancel.addEventListener("click", () => closeModal(dom["file-modal"]));
  dom.filename.addEventListener("input", () => showFilePreview());
  dom.filename.addEventListener("keydown", (event) => {
    if (event.key === "Enter") makeFile();
  });
  dom.pickscenario.addEventListener("click", () => openScenarioPick());
  dom.scenarioload.addEventListener("click", () => loadScenario());
  dom.scenariocancel.addEventListener("click", () => closeModal(dom["scenario-modal"]));
  dom.renamefile.addEventListener("click", () => openRenameFile());
  dom.renameok.addEventListener("click", () => renameFile());
  dom.renamecancel.addEventListener("click", () => closeModal(dom["rename-modal"]));
  dom.renamename.addEventListener("input", () => showRenamePreview());
  dom.renamename.addEventListener("keydown", (event) => {
    if (event.key === "Enter") renameFile();
  });
  dom.dropfile.addEventListener("click", () => openDropFile());
  dom.dropfileok.addEventListener("click", () => dropFile());
  dom.dropfilecancel.addEventListener("click", () => closeModal(dom["dropfile-modal"]));
  // Выход из разговора не должен требовать попадания в кнопку: щелчок по
  // затемнению и Escape закрывают любое из трёх окон.
  const MODALS = [
    "project-modal", "drop-modal", "file-modal", "dropfile-modal",
    "rename-modal", "scenario-modal", "projectname-modal",
  ];
  for (const id of MODALS) {
    dom[id].addEventListener("click", (event) => {
      if (event.target === dom[id]) closeModal(dom[id]);
    });
  }
  document.addEventListener("keydown", (event) => {
    if (event.key !== "Escape") return;
    for (const id of MODALS) {
      if (!dom[id].hidden) closeModal(dom[id]);
    }
  });
  dom.save.addEventListener("click", () => save());
  dom.reread.addEventListener("click", () => resolveConflict("reread"));
  dom.overwrite.addEventListener("click", () => resolveConflict("overwrite"));
  dom.download.addEventListener("click", () => download());
  // Поле выбора файла спрятано, а открывает его кнопка: ряд значков не должен
  // разрываться чужим контролом.
  dom.importproject.addEventListener("click", () => dom.upload.click());
  dom.exportproject.addEventListener("click", () => download(state.chosen?.id, state.chosen?.name));
  dom.upload.addEventListener("change", (event) => upload(event.target.files?.[0]));
  dom.uploadfile.addEventListener("click", () => dom.filepick.click());
  dom.filepick.addEventListener("change", (event) => uploadFile(event.target.files?.[0]));
  dom.downloadfile.addEventListener("click", () => downloadFile());
  dom.setpass.addEventListener("click", () => setPassword());
  dom.links.addEventListener("click", (event) => {
    const row = event.target.closest("[data-unlink]");
    if (row) unlinkProvider(row.dataset.unlink);
  });
  dom.pickok.addEventListener("click", () => {
    const login = dom.picklogin.value.trim();
    if (!login) {
      host.say(t("oauth.pickLogin"), "warning");
      return;
    }
    exchange(state.ticket, login);
  });
  dom.projects.addEventListener("click", (event) => {
    const row = event.target.closest("[data-project]");
    if (row) chooseProject(row.dataset.project);
  });
  // Двойной щелчок открывает: тот же приём, что у списка файлов на машине.
  dom.projects.addEventListener("dblclick", (event) => {
    const row = event.target.closest("[data-project]");
    if (row) openProject(row.dataset.project);
  });
  dom.tree.addEventListener("click", (event) => {
    const row = event.target.closest("[data-file]");
    if (row) openFile(state.project?.id, row.dataset.file);
  });
  // Ctrl+S / Cmd+S - как в редакторе на машине; браузерное "сохранить страницу" здесь
  // заведомо не то, чего хочет автор.
  window.addEventListener("keydown", (event) => {
    if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === "s") {
      event.preventDefault();
      save();
    }
  });
  refresh();
  fillProviders();
}

/**
 * Принимает проект, открытый по адресу `/p/<id>`.
 *
 * Нужно ради архива: читатель видит чужой открытый проект и вправе его
 * скачать - текст всё равно уже у него в браузере. Уровень при этом остаётся
 * читательским: сохранять такой проект нельзя, и кнопки сохранения не будет.
 */
export async function adopt(project) {
  // Читателю нужен состав, а не одна ссылка: в проекте бывают пояснение и несколько
  // сценариев, и без списка файлов он видит только модель - то есть меньше, чем автор
  // ему показывает. Уровень при этом остаётся читательским: кнопки сохранения не будет.
  try {
    await openProjectFiles(project.id);
  } catch {
    // Состав не прочитался - страница всё равно показывает модель, которую уже принесла
    // ссылка: список файлов её не заменяет.
    state.project = { id: project.id, name: project.name };
  }
  state.file = null;
  state.level = "view";
  refresh();
}

/** Открыт ли файл проекта (а не безымянный буфер). */
export function editing() {
  return state.project !== null && state.file !== null;
}

/**
 * Записывает черновик открытого файла.
 *
 * Цель и ключи сборки едут вместе с текстом: проект задаёт
 * умолчание, а черновик перекрывает его для своего автора. Не запиши их -
 * несохранённый выбор терялся бы при каждой перезагрузке, тогда как текст
 * переживал бы её.
 */
export function keepDraft(source, scenario, target, args, layout) {
  if (!editing()) return null;
  return draft.saveFile(localStorage, {
    project: state.project.id,
    file: state.file,
    revision: state.revision,
    source,
    scenario,
    // Имя сценария хранится вместе с его текстом: сценариев несколько, и текст одного
    // под именем другого - это не черновик, а подмена. Разошлись - черновик сценария
    // просто не берётся.
    scenarioFile: state.scenarioFile,
    target,
    args,
    // Раскладка схемы - авторская работа того же рода, что текст: она переживает
    // перезагрузку вместе с ним.
    layout,
  });
}

/**
 * Строит кнопки площадок по ответу сервера.
 *
 * Имён площадок в коде страницы нет: идентификатор и **ключ подписи**
 * приходят от сервера. Заведи здесь свой список - он разошёлся бы с настройкой
 * стенда молча, и кнопка вела бы в никуда.
 */
async function fillProviders() {
  let list = [];
  try {
    list = await api.oauthProviders();
  } catch {
    // Площадки не настроены либо сервера нет: вход паролем работает, и сообщать тут не
    // о чем.
    list = [];
  }
  dom.oauth.replaceChildren();
  dom.oauth.hidden = list.length === 0;
  for (const item of list) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "brand-btn";
    // Значок приходит именем файла от сервера, путь строит страница: имён площадок в её
    // коде нет - тем же приёмом, что и подписи.
    if (item.icon) {
      const mark = document.createElement("img");
      mark.className = "brand-mark";
      // Адрес считается от модуля (`import.meta.url`), а не от документа: страница
      // живёт и по адресу `/p/<id>`, и относительный путь увёл бы в корень - файл
      // приехал бы 404 при пустой на вид кнопке.
      mark.src = new URL(`brand/${encodeURIComponent(item.icon)}`, import.meta.url).href;
      mark.alt = "";
      button.appendChild(mark);
    }
    button.appendChild(document.createTextNode(t(item.label)));
    button.addEventListener("click", () => leaveTo(item));
    dom.oauth.appendChild(button);
  }
}

/**
 * Уходит на площадку.
 *
 * Черновик пишется немедленно, до перехода: отложенная на 400 мс запись до
 * ухода со страницы не доживёт, а `beforeunload` встретил бы вопросом "уйти?"
 * каждого, кто нажал "Войти".
 */
function leaveTo(item) {
  host.keep();
  const path = `${api.apiRoot()}api/oauth/${encodeURIComponent(item.id)}/start`;
  const query = new URLSearchParams({ return_to: location.pathname });
  if (item.via) query.set("via", item.via);
  // Обычная навигация, а не `fetch`: cookie потока и перенаправление на площадку бывают
  // только у неё.
  location.assign(`${path}?${query}`);
}

/**
 * Разбирает возврат с площадки: `#login=...`, `#login_error=...`, `#linked=1`.
 *
 * Фрагмент стирается сразу и до первого запроса: ticket - секрет этого шага,
 * и оставлять его в адресной строке и в истории браузера незачем.
 */
export async function handleReturn() {
  const raw = location.hash.replace(/^#/, "");
  if (!raw) return false;
  const params = new URLSearchParams(raw);
  const ticket = params.get("login");
  const failure = params.get("login_error");
  const linked = params.get("linked");
  if (!ticket && !failure && !linked) return false;
  history.replaceState(null, "", location.pathname + location.search);

  if (failure) {
    // Причина приходит ключом, а не текстом площадки: чужой текст не переводится и не
    // всегда предназначен читателю.
    host.say(oauthError(failure), "error");
    return true;
  }
  if (linked) {
    host.say(t("oauth.linked"), "ok");
    refresh();
    return true;
  }
  await exchange(ticket, null);
  return true;
}

/** Меняет ticket на пару; при нужде спрашивает логин. */
async function exchange(ticket, login) {
  try {
    const pair = await api.oauthComplete(ticket, login);
    hideLoginPrompt();
    host.say(t("account.hello", { login: pair.login }), "ok");
    refresh();
    await list();
  } catch (error) {
    if (error?.message_text === "login_required") {
      // Первый вход: логин выбирает человек - имени площадки мы не читаем.
      showLoginPrompt(ticket);
      return;
    }
    fail(error);
  }
}

/** Показывает окно "Выберите логин". */
function showLoginPrompt(ticket) {
  state.ticket = ticket;
  dom.pick.hidden = false;
  dom.picklogin.value = "";
  dom.picklogin.focus();
  host.say(t("oauth.pickLogin"), "warning");
}

function hideLoginPrompt() {
  state.ticket = null;
  dom.pick.hidden = true;
}

/** Текст отказа входа через площадку - по конечному словарю ключей. */
function oauthError(key) {
  if (key === "denied") return t("oauth.error.denied");
  if (key === "expired") return t("oauth.error.expired");
  if (key === "csrf") return t("oauth.error.csrf");
  if (key === "unavailable") return t("oauth.error.unavailable");
  if (key === "identity_taken") return t("oauth.error.identityTaken");
  return t("oauth.error.failed");
}

/**
 * Скачивает открытый проект архивом.
 *
 * Байты приходят запросом, а файл отдаётся временной ссылкой: у закрытого
 * проекта архив требует токена, а обычная ссылка заголовков не несёт.
 */
async function download(id = state.project?.id, name = state.project?.name) {
  if (!id) {
    host.say(t("account.nothingToSave"), "warning");
    return;
  }
  try {
    const bytes = await api.archive(id, host.target());
    const url = URL.createObjectURL(new Blob([bytes], { type: "application/zip" }));
    const link = document.createElement("a");
    link.href = url;
    link.download = `${name || "takt-project"}.zip`;
    link.click();
    // Ссылка живёт до конца загрузки: снятая сразу, она отменила бы её.
    setTimeout(() => URL.revokeObjectURL(url), 10_000);
    host.say(t("archive.downloaded"), "ok");
  } catch (error) {
    fail(error);
  }
}

/** Загружает проект из архива. */
async function upload(file) {
  if (!file) return;
  try {
    const bytes = await file.arrayBuffer();
    const created = await api.importArchive(bytes);
    host.say(t("archive.imported", { name: created.name }), "ok");
    await list();
    await openProject(created.id);
  } catch (error) {
    fail(error);
  } finally {
    // Тот же файл выбирают дважды: без сброса второе "выбрать" молчит.
    dom.upload.value = "";
  }
}

/**
 * Кладёт файл с диска в открытый проект.
 *
 * Имя берётся у самого файла - его же видит автор у себя на машине; род решает
 * расширение, как и при заведении со страницы. Занятое имя - отказ страницы, а
 * не молчаливая перезапись: правка чужого текста своим файлом хуже отказа.
 */
async function uploadFile(file) {
  if (!file) return;
  try {
    if (!state.project) {
      host.say(t("file.needProject"), "warning");
      return;
    }
    const name = file.name;
    if (state.project.files?.some((item) => item.name === name)) {
      host.say(t("file.exists", { name }), "warning");
      return;
    }
    const text = await file.text();
    await api.write(state.project.id, name, text, null);
    await openProjectFiles(state.project.id);
    await openFile(state.project.id, name);
    host.say(t("file.uploaded", { name }), "ok");
  } catch (error) {
    fail(error);
  } finally {
    // Тот же файл выбирают дважды: без сброса второе "выбрать" молчит.
    dom.filepick.value = "";
  }
}

/**
 * Выгружает выбранный файл проекта на диск.
 *
 * Текст берётся у сервера, а не у страницы: в области кода лежит правка автора,
 * а выгружается файл проекта - то, что в нём сохранено.
 */
async function downloadFile() {
  if (!state.project || !picked()) {
    host.say(t("file.nothingOpen"), "warning");
    return;
  }
  const name = picked();
  try {
    const body = await api.file(state.project.id, name);
    const url = URL.createObjectURL(new Blob([body.text], { type: "text/plain" }));
    const link = document.createElement("a");
    link.href = url;
    link.download = name;
    link.click();
    // Ссылка живёт до конца загрузки: снятая сразу, она отменила бы её.
    setTimeout(() => URL.revokeObjectURL(url), 10_000);
    host.say(t("file.downloaded", { name }), "ok");
  } catch (error) {
    fail(error);
  }
}

/** Задаёт пароль записи, у которой его не было. */
async function setPassword() {
  const password = dom.newpass.value;
  if (!password) {
    host.say(t("account.needBoth"), "warning");
    return;
  }
  try {
    await api.setPassword(password);
    dom.newpass.value = "";
    // Пароль гасит живые сеансы: вход придётся повторить, и сказать об этом надо здесь,
    // а не оставить человека гадать, почему всё отказывает.
    host.say(t("profile.passwordSet"), "ok");
    await api.signOut();
    refresh();
    fillProviders();
  } catch (error) {
    fail(error);
  }
}

/** Отвязывает площадку. */
async function unlinkProvider(provider) {
  try {
    await api.oauthUnlink(provider);
    host.say(t("profile.unlinked"), "ok");
    await fillProfile();
  } catch (error) {
    fail(error);
  }
}

/** Наполняет раздел профиля: связанные площадки и "задать пароль". */
async function fillProfile() {
  if (!api.signed()) {
    dom.profile.hidden = true;
    return;
  }
  dom.profile.hidden = false;
  try {
    // "Задать пароль" предлагается только тому, у кого пароля нет: иначе человек
    // узнавал бы об отказе нажатием, а не видом страницы.
    const me = await api.refreshMe();
    const needs = me !== null && me.has_password === false;
    dom.newpass.hidden = !needs;
    dom.setpass.hidden = !needs;
    const list = await api.oauthIdentities();
    dom.links.replaceChildren();
    for (const item of list) {
      const node = document.createElement("div");
      node.className = "row";
      node.dataset.unlink = item.provider;
      node.textContent = t("profile.linkedRow", { provider: item.provider });
      dom.links.appendChild(node);
    }
    if (list.length === 0) {
      const empty = document.createElement("div");
      empty.className = "row row-ok";
      empty.textContent = t("profile.noLinks");
      dom.links.appendChild(empty);
    }
  } catch (error) {
    fail(error);
  }
}

/** Показывает или прячет панель. */
/**
 * Открывает окно входа.
 *
 * Фокус уходит в поле логина: модальное окно, забирающее внимание, но не
 * фокус, для клавиатуры не открылось вовсе.
 */
function openSignin() {
  dom["signin-modal"].hidden = false;
  dom.login.focus();
}

/** Закрывает окно входа, не трогая набранное: вернуться в него - обычное дело. */
function closeSignin() {
  dom["signin-modal"].hidden = true;
  dom.session.focus();
}

function toggle(force) {
  const show = force ?? dom.panel.hidden;
  dom.panel.hidden = !show;
  if (show && api.signed()) {
    list();
    fillProfile();
  }
}

/** Вход или регистрация: обе ручки отвечают одинаково. */
async function enter(how) {
  const login = dom.login.value.trim();
  const password = dom.password.value;
  if (!login || !password) {
    host.say(t("account.needBoth"), "warning");
    return;
  }
  try {
    const me = await how(login, password);
    dom.password.value = "";
    // Разговор окончен ответом "вошёл" - окно закрывается само.
    closeSignin();
    host.say(t("account.hello", { login: me.login }), "ok");
    refresh();
    await list();
    await fillProfile();
  } catch (error) {
    fail(error);
  }
}

/** Выход: сессия забывается, открытый проект закрывается. */
async function leave() {
  await api.signOut();
  state.project = null;
  state.file = null;
  state.picked = null;
  state.chosen = null;
  state.revision = null;
  state.level = "none";
  // Вышли - список чужой: он рисуется заново и говорит, что для него нужен вход.
  await list();
  refresh();
}

/** Закрывает окно, возвращая внимание туда, откуда его позвали. */
function closeModal(box) {
  box.hidden = true;
}

/** Открывает окно заведения проекта: имя и отметка образца. */
function openCreate() {
  if (!signedIn()) return;
  dom["project-modal"].hidden = false;
  dom.newname.focus();
}

/** Открывает окно выбора: список читается заново - его мог пополнить другой. */
function openChosen() {
  if (!signedIn()) return;
  if (!state.chosen) {
    host.say(t("account.needChoice"), "warning");
    return;
  }
  openProject(state.chosen.id);
}

/** Отмечает выбранный проект в списке; выбор один - это место работы. */
function chooseProject(id) {
  const row = [...dom.projects.querySelectorAll("[data-project]")].find(
    (node) => node.dataset.project === id
  );
  state.chosen = row ? { id, name: row.dataset.name, level: row.dataset.level } : null;
  markChosen();
  refresh();
}

/** Переносит отметку выбора на строку выбранного проекта. */
function markChosen() {
  for (const row of dom.projects.querySelectorAll("[data-project]")) {
    row.setAttribute("aria-pressed", String(row.dataset.project === state.chosen?.id));
  }
}

/** Открывает окно переименования выбранного проекта. */
function openProjectRename() {
  if (!signedIn()) return;
  if (!state.chosen) {
    host.say(t("account.needChoice"), "warning");
    return;
  }
  dom.projectname.value = state.chosen.name ?? "";
  dom["projectname-modal"].hidden = false;
  dom.projectname.focus();
  dom.projectname.select();
}

/** Переименовывает выбранный проект. */
async function renameProject() {
  const name = dom.projectname.value.trim();
  if (!state.chosen) return;
  if (!name) {
    host.say(t("account.needName"), "warning");
    return;
  }
  try {
    await api.patch(state.chosen.id, { name });
    closeModal(dom["projectname-modal"]);
    // Открытый проект носит то же имя: подпись в шапке обязана его догнать.
    if (state.project?.id === state.chosen.id) state.project = { ...state.project, name };
    await list();
    chooseProject(state.chosen.id);
    refresh();
    host.say(t("account.projectRenamed", { name }), "ok");
  } catch (error) {
    fail(error);
  }
}

/** Открывает окно удаления выбранного проекта. */
function openDropChosen() {
  if (!signedIn()) return;
  if (!state.chosen) {
    host.say(t("account.needChoice"), "warning");
    return;
  }
  openDrop(state.chosen.id, state.chosen.name);
}

/** Открывает окно удаления проекта: он назван по имени. */
function openDrop(id, name) {
  state.doomed = { id, name };
  dom.droptext.textContent = t("account.dropAsk", { name });
  dom["drop-modal"].hidden = false;
}

/** Открывает окно заведения файла: род и имя без расширения. */
function openNewFile() {
  if (!state.project) return;
  paintFileKinds();
  dom.filename.value = "";
  showFilePreview();
  dom["file-modal"].hidden = false;
  dom.filename.focus();
}

/**
 * Открывает окно переименования открытого файла: имя без расширения.
 *
 * Расширение в поле не показывается и не правится: его ставит род файла - как и
 * при заведении. Дай править его руками - и автор сменил бы род, не сменив
 * содержимого.
 */
function openRenameFile() {
  if (!state.project) return;
  if (!picked()) {
    host.say(t("file.nothingOpen"), "warning");
    return;
  }
  dom.renamename.value = stemOf(picked());
  showRenamePreview();
  dom["rename-modal"].hidden = false;
  dom.renamename.focus();
  dom.renamename.select();
}

/** Открывает окно удаления открытого файла. */
function openDropFile() {
  if (!state.project) return;
  if (!picked()) {
    host.say(t("file.nothingOpen"), "warning");
    return;
  }
  dom.dropfiletext.textContent = t("file.dropAsk", { name: picked() });
  dom["dropfile-modal"].hidden = false;
}

/** Вошли ли; не вошли - сказано словами, а окно не открывается. */
function signedIn() {
  if (api.signed()) return true;
  host.say(t("account.needSignIn"), "warning");
  return false;
}

/**
 * Заводит проект и сразу его открывает.
 *
 * Пустой проект хорош тому, кто знает язык; отметка "по образцу" кладёт в него
 * рабочую модель - она компилируется и прогоняется с первого нажатия. Файл
 * пишется до открытия: открытый проект читает состав с сервера, и образец,
 * положенный после, не попал бы в дерево.
 */
async function make() {
  const name = dom.newname.value.trim();
  if (!name) {
    host.say(t("account.needName"), "warning");
    return;
  }
  try {
    const created = await api.create(name);
    if (dom.fromsample.checked) {
      await api.write(created.id, firstFileName(name), SAMPLE, null);
    }
    dom.newname.value = "";
    closeModal(dom["project-modal"]);
    await list();
    await openProject(created.id);
  } catch (error) {
    fail(error);
  }
}

/**
 * Удаляет открытый проект.
 *
 * Спрошено уже было - здесь только исполнение. После удаления страница остаётся
 * без проекта: показывать состав того, чего нет, нельзя, а открытый текст
 * автору всё ещё виден.
 */
async function drop() {
  const doomed = state.doomed;
  if (!doomed) return;
  try {
    await api.remove(doomed.id);
    closeModal(dom["drop-modal"]);
    state.doomed = null;
    // Удалённый проект мог быть открытым: страница возвращается к безымянному
    // буферу, иначе она показывала бы состав того, чего нет.
    // Удалённый проект закрывается без сохранения: писать в него уже некуда.
    if (state.project?.id === doomed.id) {
      state.level = "none";
      await closeProject(false);
    }
    host.say(t("account.dropped", { name: doomed.name }), "ok");
    refresh();
    await list();
  } catch (error) {
    fail(error);
  }
}

/**
 * Закрывает открытый проект: страница возвращается к безымянному буферу.
 *
 * Текст не трогается: закрытие проекта - не потеря работы, и то, что автор
 * набрал, остаётся у него на экране и в черновике.
 */
async function closeProject(say = true) {
  // Выход из проекта - с сохранением: автор уходит из него, а не выбрасывает
  // работу. Молча терять набранное нельзя, а спрашивать "сохранить ли" на каждый
  // выход значит спрашивать о том, чего никто не хочет иначе.
  if (editing() && (state.level === "edit" || state.level === "owner")) {
    try {
      await save();
    } catch {
      // Отказ записи назван самим сохранением; проект всё равно закрывается -
      // иначе автор оказался бы заперт в нём отказом сервера.
    }
  }
  state.project = null;
  state.file = null;
  state.picked = null;
  state.revision = null;
  state.level = "none";
  state.scenarioFile = null;
  state.scenarioRead = "";
  state.doomed = null;
  // Проект закрыт - закрыто и всё, что о нём говорило: текст, сценарий, схема,
  // состав файлов. Остаться на экране им нельзя: показанное принадлежит проекту,
  // и после закрытия оно врало бы о том, что открыто.
  shell.forget(localStorage, shell.UI_KEYS.project);
  host.projectTexts?.({});
  paintTree([]);
  hideConflict();
  host.open({ source: "", scenario: "", layout: "", file: "", kind: "takt" });
  host.closed();
  if (say) host.say(t("account.closed"), "ok");
  refresh();
}

/**
 * Возвращает страницу к последнему открытому проекту.
 *
 * Спрашивается при заходе: автор работает в проекте, и начинать каждый раз с
 * выбора из списка значит спрашивать его о том, что уже известно. Проекта нет
 * или он больше не читается - страница остаётся пустой, а не показывает образец.
 *
 * @returns {Promise<boolean>} открылся ли проект
 */
export async function restoreLast() {
  if (!api.signed()) return false;
  const id = shell.setting(localStorage, shell.UI_KEYS.project, "");
  if (!id) return false;
  try {
    await openProject(id);
    return state.project !== null;
  } catch {
    // Проект удалён, права сняты, сервер молчит - забываем и открываемся пустыми.
    shell.forget(localStorage, shell.UI_KEYS.project);
    return false;
  }
}

/** Роды файлов, которые автор вправе завести, и расширение каждого. */
const FILE_KINDS = [
  { kind: "takt", label: "file.kind.takt", extension: ".takt" },
  { kind: "layout", label: "file.kind.layout", extension: layoutFile.EXTENSION },
  { kind: "scenario", label: "file.kind.scenario", extension: ".json" },
  { kind: "markdown", label: "file.kind.markdown", extension: ".md" },
  { kind: "address_map", label: "file.kind.addressMap", extension: ".takt-map" },
];

/**
 * Рисует ряд родов файла.
 *
 * Раскладка стоит в ряду наравне с прочими: она появляется и сама - при первом
 * сохранении схемы, - но завести её заранее автор вправе. Файл раскладки парен
 * модели по имени, и без такой модели схема ему нечего показывать: об этом
 * страница говорит, а не отказывает - имя модели автор допишет следом.
 */
function paintFileKinds() {
  dom.filekinds.replaceChildren();
  for (const [index, item] of FILE_KINDS.entries()) {
    const button = document.createElement("button");
    button.type = "button";
    button.dataset.kind = item.kind;
    button.textContent = t(item.label);
    button.setAttribute("role", "radio");
    button.setAttribute("aria-checked", String(index === 0));
    button.addEventListener("click", () => {
      for (const other of dom.filekinds.children) other.setAttribute("aria-checked", "false");
      button.setAttribute("aria-checked", "true");
      showFilePreview();
    });
    dom.filekinds.appendChild(button);
  }
}

/** Выбранный род файла. */
function chosenFileKind() {
  const picked = [...dom.filekinds.children].find(
    (node) => node.getAttribute("aria-checked") === "true"
  );
  return FILE_KINDS.find((item) => item.kind === picked?.dataset.kind) ?? FILE_KINDS[0];
}

/** Показывает имя, которое получится: расширение ставит род, а не автор. */
function showFilePreview() {
  const name = dom.filename.value.trim();
  dom.filepreview.textContent = name ? name + chosenFileKind().extension : "";
}

/**
 * Заводит файл в открытом проекте и открывает его.
 *
 * Имя судит сервер (латиница, цифры, `_`, `-`), но пустое имя и занятое имя
 * страница ловит сама: отказ, который она может назвать заранее, не стоит
 * рейса.
 */
async function makeFile() {
  if (!state.project) return;
  const raw = dom.filename.value.trim();
  if (!raw) {
    host.say(t("file.needName"), "warning");
    return;
  }
  if (!/^[A-Za-z0-9_-]+$/.test(raw)) {
    host.say(t("file.badName"), "warning");
    return;
  }
  const name = raw + chosenFileKind().extension;
  if (state.project.files?.some((file) => file.name === name)) {
    host.say(t("file.exists", { name }), "warning");
    return;
  }
  // Раскладка парна модели: без неё схеме нечего показывать. Это не отказ -
  // автор вправе завести раскладку заранее, - но сказать об этом надо.
  const pairless =
    name.endsWith(layoutFile.EXTENSION) &&
    !state.project.files?.some((file) => file.name === modelName(name));
  try {
    await api.write(state.project.id, name, "", null);
    closeModal(dom["file-modal"]);
    await openProjectFiles(state.project.id);
    await openFile(state.project.id, name);
    host.say(
      pairless ? t("file.layoutWithoutModel", { name: modelName(name) }) : t("file.created", { name }),
      pairless ? "warning" : "ok"
    );
  } catch (error) {
    fail(error);
  }
}

/**
 * Открывает выбор сценария прогона для открытой модели.
 *
 * Сценарии названы по модели: файл `.json`, чьё имя начинается с её имени, - её
 * сценарий. Правило одно на страницу и на автора: `heater.json`,
 * `heater-cold.json` принадлежат `heater.takt`, а `probe.json` - нет.
 */
function openScenarioPick() {
  const model = modelOf();
  if (!model) {
    host.say(t("scenario.needModel"), "warning");
    return;
  }
  const own = scenariosOf(model);
  dom.scenarios.replaceChildren();
  if (own.length === 0) {
    // Пустой список говорит словами: молчащее окно читалось бы как поломка.
    const empty = document.createElement("div");
    empty.className = "row row-ok";
    empty.textContent = t("scenario.none", { name: model });
    dom.scenarios.appendChild(empty);
  }
  for (const name of own) {
    const row = document.createElement("button");
    row.type = "button";
    row.className = "row row-pick";
    row.dataset.scenario = name;
    row.textContent = name;
    row.setAttribute("aria-pressed", String(name === state.scenarioFile));
    row.addEventListener("click", () => {
      for (const other of dom.scenarios.children) other.setAttribute?.("aria-pressed", "false");
      row.setAttribute("aria-pressed", "true");
    });
    dom.scenarios.appendChild(row);
  }
  dom["scenario-modal"].hidden = false;
}

/** Имя открытой модели: сам файл либо модель, парная открытой раскладке. */
function modelOf() {
  const name = picked();
  if (name.endsWith(".takt")) return name;
  if (name.endsWith(layoutFile.EXTENSION)) return modelName(name);
  return state.file && state.file.endsWith(".takt") ? state.file : "";
}

/** Сценарии модели: файлы рода "сценарий", названные по её имени. */
function scenariosOf(model) {
  const stem = model.slice(0, -".takt".length);
  return (state.project?.files ?? [])
    .filter((file) => file.kind === "scenario" && file.name.startsWith(stem))
    .map((file) => file.name)
    .sort((a, b) => a.localeCompare(b));
}

/** Назначает выбранный сценарий прогоном модели. */
async function loadScenario() {
  const chosen = [...dom.scenarios.children].find(
    (node) => node.getAttribute?.("aria-pressed") === "true"
  );
  if (!chosen) {
    closeModal(dom["scenario-modal"]);
    return;
  }
  const name = chosen.dataset.scenario;
  await chooseScenario(name);
  closeModal(dom["scenario-modal"]);
  host.say(t("scenario.chosen", { name }), "ok");
}

/** Расширение открытого файла: род задаёт его, а не автор. */
function extensionOf(name) {
  const kind = FILE_KINDS.find((item) => name.endsWith(item.extension));
  return kind ? kind.extension : "";
}

/** Имя файла без расширения. */
function stemOf(name) {
  const extension = extensionOf(name);
  return extension ? name.slice(0, -extension.length) : name;
}

/**
 * Файл, над которым совершаются действия: выбранный в дереве, а без выбора -
 * открытый в области кода (проект могли открыть с назначенным файлом).
 */
function picked() {
  return state.picked || state.file || "";
}

/** Показывает имя, которое получится при переименовании. */
function showRenamePreview() {
  const raw = dom.renamename.value.trim();
  dom.renamepreview.textContent = raw ? raw + extensionOf(picked()) : "";
}

/**
 * Переименовывает открытый файл.
 *
 * Вместе с моделью переименовывается её раскладка: файл `.takt-ui` парен модели
 * по имени, и оставленный под прежним именем он показывал бы схему того, чего
 * нет. Пару знает страница - сервер видит два независимых файла.
 */
async function renameFile() {
  if (!state.project || !picked()) return;
  const raw = dom.renamename.value.trim();
  if (!raw) {
    host.say(t("file.needName"), "warning");
    return;
  }
  if (!/^[A-Za-z0-9_-]+$/.test(raw)) {
    host.say(t("file.badName"), "warning");
    return;
  }
  const was = picked();
  const name = raw + extensionOf(was);
  if (name === was) {
    closeModal(dom["rename-modal"]);
    return;
  }
  if (state.project.files?.some((file) => file.name === name)) {
    host.say(t("file.exists", { name }), "warning");
    return;
  }
  try {
    await api.renameFile(state.project.id, was, name);
    if (was.endsWith(".takt")) {
      const layout = layoutName(was);
      if (state.project.files?.some((file) => file.name === layout)) {
        await api.renameFile(state.project.id, layout, layoutName(name));
      }
    }
    closeModal(dom["rename-modal"]);
    await openProjectFiles(state.project.id);
    await openFile(state.project.id, name);
    host.say(t("file.renamed", { was, name }), "ok");
  } catch (error) {
    fail(error);
  }
}

/**
 * Удаляет открытый файл проекта.
 *
 * Вместе с моделью уходит её раскладка: файл `.takt-ui` описывает именно эту
 * модель, и осиротевший он показывал бы схему того, чего нет.
 */
async function dropFile() {
  if (!state.project || !picked()) return;
  const doomed = picked();
  try {
    await api.removeFile(state.project.id, doomed);
    if (doomed.endsWith(".takt")) {
      const layout = layoutName(doomed);
      if (state.project.files?.some((file) => file.name === layout)) {
        await api.removeFile(state.project.id, layout);
      }
    }
    closeModal(dom["dropfile-modal"]);
    host.say(t("file.dropped", { name: doomed }), "ok");
    await openProjectFiles(state.project.id);
    const next = state.project.files?.[0]?.name ?? null;
    if (next) {
      await openFile(state.project.id, next);
    } else {
      state.file = DEFAULT_FILE;
      state.revision = null;
      host.open({ source: "", scenario: "", layout: "" });
      refresh();
    }
  } catch (error) {
    fail(error);
  }
}

/** Наполняет список проектов. */
async function list() {
  if (!api.signed()) {
    // Без входа списка нет вовсе: страница говорит об этом словами, а не пустой
    // областью - пустая читалась бы как поломка.
    dom.projects.replaceChildren(row(t("account.needSignIn"), "ok"));
    return;
  }
  try {
    const rows = await api.projects();
    dom.projects.replaceChildren();
    for (const item of rows) {
      const node = document.createElement("button");
      node.type = "button";
      node.className = "row row-pick project-row";
      node.dataset.project = item.id;
      node.dataset.name = item.name;
      node.dataset.level = item.level;
      node.textContent = t("account.projectRow", {
        name: item.name,
        level: levelName(item.level),
      });
      node.setAttribute("aria-pressed", String(item.id === state.chosen?.id));
      // Описание проекта - подсказкой при наведении: в строке места ему нет, а
      // выбирать проект по одному имени приходится вслепую. Пустое описание
      // подсказки не даёт - пустая всплывашка читалась бы как поломка.
      const about = typeof item.description === "string" ? item.description.trim() : "";
      if (about) node.dataset.tip = about;
      dom.projects.appendChild(node);
    }
    if (rows.length === 0) dom.projects.appendChild(row(t("account.noProjects"), "ok"));
    // Выбор мог указывать на проект, которого больше нет: чужой список ему не
    // хозяин, и отметка на пустом месте обманывала бы кнопки.
    if (state.chosen && !rows.some((item) => item.id === state.chosen.id)) state.chosen = null;
    refresh();
  } catch (error) {
    fail(error);
  }
}

/** Строка списка словами: пустой список и запрет входа говорятся, а не молчат. */
function row(text, kind) {
  const node = document.createElement("div");
  node.className = `row row-${kind}`;
  node.textContent = text;
  return node;
}

/**
 * Рисует структуру проекта: файлы по родам, в порядке рода и имени.
 *
 * Род - не украшение: файлы открываются по-разному (модель правится кодом,
 * сценарий своим полем, раскладка показывается схемой), и читателю нужно видеть
 * это до щелчка. Подпись рода - ключ словаря: текст здесь завёл бы второй
 * словарь.
 */
function paintTree(files) {
  // Роды перечислены ключами словаря, а не собраны строкой: собранный ключ
  // сверке невидим, и подпись рода пропала бы молча.
  const KINDS = [
    { kind: "takt", label: "tree.kind.takt" },
    { kind: "layout", label: "tree.kind.layout" },
    { kind: "scenario", label: "tree.kind.scenario" },
    { kind: "markdown", label: "tree.kind.markdown" },
    { kind: "address_map", label: "tree.kind.addressMap" },
  ];
  dom.tree.replaceChildren();
  if (files.length === 0) {
    // Пусто по двум разным причинам, и путать их нельзя: у нового проекта
    // файлов ещё нет, но он открыт, и "проект не открыт" здесь было бы ложью -
    // автор как раз в нём и пишет.
    const key = state.project ? "tree.noFiles" : "tree.empty";
    const empty = document.createElement("div");
    empty.className = "tree-kind";
    empty.dataset.i18n = key;
    empty.textContent = t(key);
    dom.tree.appendChild(empty);
    host.treeChanged?.();
    return;
  }
  for (const { kind, label } of KINDS) {
    const own = files.filter((file) => file.kind === kind).sort((a, b) => a.name.localeCompare(b.name));
    if (own.length === 0) continue;
    const group = document.createElement("div");
    group.className = "tree-group";
    const title = document.createElement("div");
    title.className = "tree-kind";
    title.dataset.i18n = label;
    title.textContent = t(label);
    group.appendChild(title);
    for (const file of own) {
      const node = document.createElement("button");
      node.type = "button";
      node.className = "tree-file";
      node.dataset.file = file.name;
      node.dataset.kind = file.kind;
      node.textContent = file.name;
      group.appendChild(node);
    }
    dom.tree.appendChild(group);
  }
  markPicked();
  // Состав дерева сменился - сменилась и его мерка: нижнюю границу ширины
  // страница считает по именам файлов, и считать её надо по нарисованным.
  host.treeChanged?.();
}

/** Отмечает выбранный файл в дереве: отметка одна, это место работы. */
function markPicked() {
  for (const node of dom.tree.querySelectorAll(".tree-file")) {
    node.setAttribute("aria-pressed", String(node.dataset.file === state.picked));
  }
}

/** Рисует пустую структуру: страница открыта без проекта. */
export function paintEmptyTree() {
  if (!state.project) paintTree([]);
}

/** Перечитывает состав файлов открытого проекта. */
async function openProjectFiles(id) {
  const opened = await api.project(id);
  state.project = opened;
  // Открытый проект запоминается: заход на страницу возвращает автора туда, где
  // он работал, - иначе каждый заход начинался бы с выбора из списка.
  shell.remember(localStorage, shell.UI_KEYS.project, id);
  state.level = opened.level;
  paintTree(opened.files);
  await readProjectTexts();
  // Сценариев бывает несколько: проект называет свой, и он же становится умолчанием. Не
  // назови - прогон шёл бы по первому по имени, то есть не по тому, на котором автор
  // показывает работу модели.
  const scenarios = opened.files
    .filter((file) => file.kind === "scenario")
    .map((file) => file.name);
  const chosen = scenarios.includes(state.scenarioFile)
    ? state.scenarioFile
    : (scenarios.includes(opened.main_scenario) ? opened.main_scenario : scenarios[0] ?? null);
  host.scenarios(scenarios, chosen);
  if (chosen !== state.scenarioFile) await chooseScenario(chosen);
}

/**
 * Читает тексты моделей и карт адресов открытого проекта и отдаёт их странице.
 *
 * Нужны они для `import` и `--address-map`: у модуля в браузере диска нет, и
 * подключаемый файл и карту он находит только в составе проекта. Читаются модели
 * и карты - подключать можно только модели, а сборку с адресами ведёт карта;
 * отказ чтения одного файла не валит открытие проекта, а оставляет файл вне
 * состава - подключение его ответит `SE-013` словами.
 */
async function readProjectTexts() {
  const texts = {};
  const project = state.project;
  if (!project) {
    host.projectTexts?.(texts);
    return;
  }
  const models = (project.files ?? []).filter((file) => file.kind === "takt" || file.kind === "address_map");
  await Promise.all(
    models.map(async (file) => {
      try {
        texts[file.name] = (await api.file(project.id, file.name)).text ?? "";
      } catch {
        // Файл остаётся вне состава - см. выше.
      }
    })
  );
  // Проект могли сменить, пока тексты ехали: чужой состав странице не нужен.
  if (state.project?.id === project.id) host.projectTexts?.(texts);
}

/**
 * Выбирает сценарий прогона и читает его текст.
 *
 * Текст запоминается таким, каким его отдал сервер: по нему видно, правил ли
 * его автор, и надо ли записывать сценарий при сохранении.
 */
export async function chooseScenario(name) {
  state.scenarioFile = name;
  if (!name || !state.project) {
    state.scenarioRead = "";
    host.openScenario("", null);
    return;
  }
  try {
    const body = await api.file(state.project.id, name);
    state.scenarioRead = body.text ?? "";
    host.openScenario(state.scenarioRead, name);
  } catch (error) {
    fail(error);
  }
}

/** Род файла по составу проекта; `takt` - если состав ещё не прочитан. */
function kindOf(name) {
  return state.project?.files?.find((file) => file.name === name)?.kind ?? "takt";
}

/** Открывает проект: состав файлов и активный файл. */
async function openProject(id) {
  try {
    await openProjectFiles(id);
    const opened = state.project;
    // Без назначенного активного файла открывается модель, а не первый файл по
    // алфавиту: `test-cold.json` стоит раньше `test.takt` (`-` меньше `.`), и
    // проект открывался сценарием при пустом редакторе модели.
    const first =
      opened.main_file ??
      opened.files.find((file) => file.kind === "takt")?.name ??
      opened.files[0]?.name ??
      null;
    if (first) {
      await openFile(id, first);
    } else {
      // У нового проекта файлов ещё нет, но писать автор начинает сразу. Не назови мы
      // файл здесь - кнопки сохранения не было бы вовсе, и первый же набранный текст
      // оставался бы только в черновике.
      state.file = firstFileName(opened.name);
      state.revision = null;
      hideConflict();
      // Пустой проект открывается пустым. Оставь мы текст прежнего - он выглядел
      // бы содержимым нового проекта и ушёл бы в него первым же сохранением.
      host.open({ source: "", scenario: "", layout: "" });
      refresh();
    }
  } catch (error) {
    fail(error);
  }
}

/**
 * Открывает файл проекта.
 *
 * Черновик сильнее сервера **не молча**: разошлись - показываются обе даты
 * и обе ревизии, и выбор делает автор. Сам подставить черновик нельзя (он
 * может быть вчерашним), сам выбросить - тем более.
 */
async function openFile(id, name) {
  if (!id || !name) return;
  // Выбранный в дереве файл - не то же, что открытый в области кода: сценарий и
  // раскладку открывают, не меняя открытого файла (щелчок по сценарию не
  // подменяет модель). Действия над файлом обращены к выбранному - иначе
  // мусорка и переименование трогали бы не тот файл, на который смотрит автор.
  state.picked = name;
  markPicked();
  // Сценарий открывается своей областью: он живёт во вкладке прогона, и подмена им
  // модели означала бы, что автор потерял модель из виду, щёлкнув по списку файлов.
  if (kindOf(name) === "scenario") {
    await chooseScenario(name);
    host.showTrace();
    // Полоса действий обновляется и здесь: открытие сценария - такое же
    // открытие файла, и уйди мы отсюда молча, кнопки открытого проекта не
    // появились бы вовсе - у проекта, чей активный файл сценарий, полоса
    // осталась бы полосой закрытого.
    refresh();
    return;
  }
  // Раскладка - не самостоятельный документ: щелчок по ней открывает парную модель со
  // схемой, а не JSON текстом в редакторе.
  if (kindOf(name) === "layout") {
    const pair = modelName(name);
    if (pair && state.project?.files?.some((file) => file.name === pair)) {
      await openFile(id, pair);
      host.showScheme();
    }
    // Раскладка без своей модели тоже открытие: полоса обязана ответить.
    refresh();
    return;
  }
  try {
    const body = await api.file(id, name);
    state.file = name;
    state.revision = body.revision;
    const kept = draft.loadFile(localStorage, id, name);
    // Раскладка схемы читается парой к модели: файла нет - лист пуст.
    const layoutPair = layoutName(name);
    const hasLayout = Boolean(layoutPair) && state.project?.files?.some((file) => file.name === layoutPair && file.kind === "layout");
    state.layoutFile = hasLayout ? layoutPair : null;
    state.layoutRead = hasLayout ? ((await api.file(id, layoutPair)).text ?? "") : "";
    if (kept && kept.source !== body.text) {
      state.conflict = {
        kind: "draft",
        seen: kept.revision,
        actual: body.revision,
        text: body.text,
        draft: kept,
      };
      showConflict(
        t("account.draftDiffers", {
          saved: when(kept.savedAt),
          revision: body.revision,
        }),
      );
      // Черновик сильнее проекта: он и есть незавершённая работа автора.
      host.open({
        source: kept.source,
        file: name,
        kind: kindOf(name),
        target: kept.target || build().target,
        args: kept.args ?? build().args,
        layout: kept.layout ?? state.layoutRead,
        layoutFile: state.layoutFile,
      });
      // Черновик сценария берётся, только если он от того же файла: текст одного
      // сценария под именем другого - подмена, а не сохранность.
      if (kept.scenarioFile && kept.scenarioFile === state.scenarioFile) {
        host.openScenario(kept.scenario ?? "", state.scenarioFile);
      }
    } else {
      hideConflict();
      // Текст модели тот же, а раскладка черновика своя: автор двигал узлы и не
      // сохранил. Без этой ветви черновик читался только при правленом тексте, и
      // перезагрузка стирала расстановку, хотя черновик её хранил.
      const layout = layoutFile.preferDraft(kept?.layout, state.layoutRead);
      host.open({
        source: body.text,
        file: name,
        kind: kindOf(name),
        layout: layout.text,
        layoutFile: state.layoutFile,
        ...build(),
      });
      if (layout.fromDraft) host.say(t("scheme.layoutFromDraft"), "warning");
    }
    // Сценарий - сценарий открытой модели: чужой к ней не применяется. Выбор при
    // открытии проекта смотрел на все сценарии проекта, и модель без своих
    // получала сценарий соседней - прогон кончался отказом `SIM-030`.
    if (kindOf(name) === "takt") {
      const own = scenariosOf(name);
      const chosen = pickScenario(own, state.scenarioFile, state.project?.main_scenario ?? null);
      if (chosen !== state.scenarioFile) await chooseScenario(chosen);
    }
    refresh();
  } catch (error) {
    fail(error);
  }
}

/**
 * Записывает выбранный сценарий, если автор его правил.
 *
 * Возвращает новую ревизию проекта либо `null` - сценарий не менялся.
 *
 * Признак - расхождение с текстом, отданным сервером, а не "трогали ли
 * область": набранное и стёртое обратно - это не правка, и запись ради неё
 * подняла бы ревизию у всех, кто держит проект открытым.
 *
 * # Ошибки
 * Отказ записи поднимается вызывающему.
 */
async function keepScenario(revision) {
  const text = host.scenario();
  if (!state.scenarioFile || text === state.scenarioRead) return null;
  const written = await api.write(state.project.id, state.scenarioFile, text, revision);
  state.revision = written.revision;
  state.scenarioRead = text;
  return written.revision;
}

/**
 * Записывает раскладку схемы парным файлом, если она изменилась.
 *
 * Пустая раскладка при отсутствующем файле не записывается: файла нет и не
 * нужно. Записанный текст запоминается, чтобы следующая запись шла только по
 * новым правкам.
 *
 * Запись в существующий файл несёт ревизию проекта - ту же, что у модели и
 * сценария: без неё сервер отвечает конфликтом, и правка схемы пропадала бы при
 * каждом сохранении, кроме первого, заводящего файл.
 */
async function keepLayout() {
  const pair = layoutName(state.file);
  if (!pair) return;
  const current = host.layout();
  if (current === state.layoutRead) return;
  if (!state.layoutFile && current === layoutFile.canonical(layoutFile.empty())) return;
  const written = await api.write(state.project.id, pair, current, state.layoutFile ? state.revision : null);
  state.revision = written.revision;
  state.layoutFile = pair;
  state.layoutRead = current;
}

/**
 * Цель и ключи сборки открытого проекта.
 *
 * Пусто - проект прежней выгрузки либо страница без проекта: тогда остаётся
 * выбранное на странице, и решает это её сторона.
 */
function build() {
  return {
    target: state.project?.build_target ?? "",
    args: state.project?.build_args ?? "",
  };
}

/**
 * Записывает выбор сборки в проект, если он изменился.
 *
 * Запись идёт вместе с явным сохранением, а не на каждый щелчок по
 * вкладке: сервер не должен видеть перебор целей, которым автор просто
 * смотрит вывод. Метаданные правит только владелец (правило сервера): у
 * уровня `edit` попытка кончилась бы отказом, о котором автор не просил.
 * Отказ поднимается вызывающему - он решает, чем это считать.
 */
async function keepBuild() {
  if (state.level !== "owner") return;
  const target = host.target();
  const args = host.args();
  const kept = build();
  const fields = {};
  if (target !== kept.target || args !== kept.args) {
    fields.build_target = target;
    fields.build_args = args;
  }
  // Выбранный сценарий - тот же род величины (09n): проект называет свой, и читатель,
  // открывший проект, начинает прогон с него.
  if (state.scenarioFile && state.scenarioFile !== state.project?.main_scenario) {
    fields.main_scenario = state.scenarioFile;
  }
  if (Object.keys(fields).length === 0) return;
  const updated = await api.patch(state.project.id, fields);
  state.project = { ...state.project, ...updated };
}

/**
 * Карта адресов открытого проекта: файл носит имя проекта, как и первая модель
 * (`firstFileName`), - `ports16.takt-map`. `null` - проекта нет.
 *
 * @returns {{name: string, present: boolean}|null}
 */
export function addressMap() {
  if (!state.project) return null;
  const name = firstFileName(state.project.name).replace(/\.takt$/, ".takt-map");
  const present = Boolean(state.project.files?.some((file) => file.name === name && file.kind === "address_map"));
  return { name, present };
}

/** Задержка между тактами прогона у сценария, секунд; записи нет - без задержки. */
export function delayOf(file) {
  return (file && state.project?.run_delays?.[file]) || 0;
}

/**
 * Ставит задержку сценарию и записывает её в проект.
 *
 * Записывает только владелец - метаданные правит он (правило сервера); у прочих
 * задержка живёт до перезагрузки страницы. Без сценария записывать не к чему:
 * задержка - свойство сценария.
 */
export async function setDelay(file, seconds) {
  if (!state.project || !file) return;
  const delays = { ...(state.project.run_delays ?? {}) };
  if (seconds > 0) delays[file] = seconds;
  else delete delays[file];
  state.project = { ...state.project, run_delays: delays };
  if (state.level !== "owner") return;
  try {
    const updated = await api.patch(state.project.id, { run_delays: delays });
    state.project = { ...state.project, ...updated };
  } catch (error) {
    host.say(text(error), "warning");
  }
}

/** Сохраняет открытый файл на сервер. */
async function save() {
  if (!editing()) {
    host.say(t("account.nothingToSave"), "warning");
    return;
  }
  if (state.level !== "edit" && state.level !== "owner") {
    host.say(t("account.readOnly"), "warning");
    return;
  }
  try {
    const written = await api.write(
      state.project.id,
      state.file,
      host.source(),
      state.revision,
    );
    state.revision = written.revision;
    draft.clearFile(localStorage, state.project.id, state.file);
    hideConflict();
    // Правленый сценарий записывается вместе с файлом и называется в сообщении: он
    // живёт в другой области экрана, и молча записанный чужой файл - это работа, о
    // которой автор не просил.
    const also = await keepScenario(written.revision);
    // Раскладка схемы пишется своим файлом рядом с моделью - и только когда она
    // изменилась: пустой файл ради пустой раскладки проекту не нужен.
    try {
      await keepLayout();
    } catch (error) {
      host.say(t("scheme.layoutNotSaved", { error: text(error) }), "warning");
    }
    host.say(
      also
        ? t("account.savedWith", { revision: also, file: state.scenarioFile })
        : t("account.saved", { revision: written.revision }),
      "ok",
    );
    // Выбор сборки пишется после текста и своим отказом: сервер вправе отвергнуть
    // ключи, а текст к тому времени уже сохранён - общий отказ сказал бы автору, что
    // работа не записана, и он записал бы её ещё раз поверх своей же.
    try {
      await keepBuild();
    } catch (error) {
      host.say(text(error), "warning");
    }
    // Состав файлов мог измениться (первое сохранение заводит файл): список
    // перечитывается, иначе он остался бы вчерашним.
    await openProjectFiles(state.project.id);
    refresh();
  } catch (error) {
    if (error?.code === "revision_conflict" && typeof error.revision === "number") {
      // Числа взяты полями ответа, а не разобраны из текста: текст отказа переводится и
      // правится, а протокол - нет.
      state.conflict = { kind: "server", seen: state.revision, actual: error.revision };
      showConflict(
        t("account.conflict", { mine: state.revision, theirs: error.revision }),
      );
      return;
    }
    fail(error);
  }
}

/** Разрешает конфликт выбором автора. */
async function resolveConflict(choice) {
  if (!state.conflict || !editing()) return;
  try {
    if (choice === "reread") {
      const body = await api.file(state.project.id, state.file);
      state.revision = body.revision;
      draft.clearFile(localStorage, state.project.id, state.file);
      host.open({ source: body.text, file: state.file, kind: kindOf(state.file) });
      host.say(t("account.rereadDone", { revision: body.revision }), "ok");
    } else {
      // Перезаписать - это записать поверх той ревизии, что у сервера сейчас: автор
      // увидел оба числа и решил.
      state.revision = state.conflict.actual;
      hideConflict();
      await save();
      return;
    }
    hideConflict();
    refresh();
  } catch (error) {
    fail(error);
  }
}

function showConflict(message) {
  dom.conflict.hidden = false;
  dom.conflicttext.textContent = message;
}

function hideConflict() {
  state.conflict = null;
  dom.conflict.hidden = true;
  dom.conflicttext.textContent = "";
}

/** Приводит панель и шапку в согласие с тем, что открыто. */
function refresh() {
  const me = api.who();
  host.signedIn?.(me !== null);
  dom.signedout.hidden = me !== null;
  dom.signedin.hidden = me === null;
  dom.whoami.textContent = me ? me.login : "";
  // Кнопки шапки - Значки, и текста не несут: логин показывается подписью рядом, а в
  // саму кнопку уходит подпись для диктора и всплывающая.
  const label = me ? t("account.signOut") : t("account.enter");
  dom.session.dataset.tip = label;
  dom.session.setAttribute("aria-label", label);
  // Значок следует за действием: не вошёл - стрелка внутрь, вошёл - наружу.
  dom["icon-enter"].hidden = me !== null;
  dom["icon-leave"].hidden = me === null;
  dom["whoami-bar"].textContent = me ? me.login : "";
  dom["whoami-bar"].hidden = me === null;
  // Длинное имя обрезается по пятнадцати знакам, и хвост его затеняется.
  // Признак ставится по факту обрезки, а не правилом:
  // правило блёкло бы и на коротком имени, которое влезло целиком.
  for (const node of [dom.whoami, dom["whoami-bar"]]) {
    node.classList.toggle("clipped", node.scrollWidth > node.clientWidth + 1);
  }
  // Полоса структуры отвечает на один вопрос за раз: пока проекта нет - как его
  // завести или открыть; когда открыт - что делать с ним и его файлами. Обе
  // группы разом заставляли бы искать нужную среди ненужных.
  const opened = state.project !== null;
  const writes = opened && (state.level === "edit" || state.level === "owner");
  // Панель показывает одно из двух, и полоса отвечает тому, что показано: пока
  // проект не открыт - список проектов и действия над ними; открыли - его состав
  // и действия над файлами. Обе группы разом заставляли бы искать нужную среди
  // ненужных.
  const chosen = state.chosen;
  const mine = chosen?.level === "owner";
  dom.projects.hidden = opened;
  dom.tree.hidden = !opened;
  dom.newproject.hidden = opened;
  dom.openproject.hidden = opened || !chosen;
  // Переименование и удаление - право владельца: предлагать действие, которое
  // сервер отвергнет, нельзя.
  dom.renameproject.hidden = opened || !mine;
  dom.dropproject.hidden = opened || !mine;
  dom.importproject.hidden = opened;
  dom.exportproject.hidden = opened || !chosen;
  dom.download.hidden = !opened;
  dom.closeproject.hidden = !opened;
  // Заводить и удалять файлы вправе тот, кто вправе писать: чужой проект
  // открывается на чтение, и предлагать ему правку значит обещать отказ сервера.
  dom.newfile.hidden = !writes;
  dom.pickscenario.hidden = !opened;
  dom.uploadfile.hidden = !writes;
  dom.downloadfile.hidden = !opened;
  dom.renamefile.hidden = !writes;
  dom.dropfile.hidden = !writes;
  const writable = editing() && (state.level === "edit" || state.level === "owner");
  dom.save.hidden = !writable;
  dom.openfile.hidden = !editing();
  if (editing()) {
    // У нового файла ревизии нет вовсе, и печатать "ревизия null" нельзя: подпись
    // читает человек, а `null` в ней - сообщение об ошибке, которой не было (нашлось
    // прогоном страницы).
    dom.openfile.textContent =
      state.revision === null
        ? t("account.openNew", { project: state.project.name, file: state.file })
        : t("account.openFile", {
            project: state.project.name,
            file: state.file,
            revision: state.revision,
          });
  }
}

/**
 * Имя уровня словом.
 *
 * Ключи перечислены **буквально**, а не собраны из строки уровня: ключ,
 * собранный на ходу, невидим сверке словаря - и пропавший перевод обнаружился
 * бы у читателя, а не у проверки.
 */
function levelName(level) {
  if (level === "owner") return t("account.level.owner");
  if (level === "edit") return t("account.level.edit");
  if (level === "fork") return t("account.level.fork");
  if (level === "view") return t("account.level.view");
  return t("account.level.none");
}

/** Человеческая дата черновика: без неё "разошлось" ничего не говорит. */
function when(savedAt) {
  if (!savedAt) return "—";
  return new Date(savedAt).toLocaleString();
}

/**
 * Показывает отказ и приводит панель в согласие с сессией.
 *
 * Отказ `401` означает, что сохранённая пара больше не годится (сервер
 * перезапущен с чистой базой, токен отозван, семейство погашено). Без этого шага
 * панель продолжала бы показывать "вошли как ..." и кнопку "Выйти" у сессии,
 * которой нет - нашлось прогоном страницы.
 */
function fail(error) {
  host.say(text(error), "error");
  if (error?.status === 401) {
    state.project = null;
    state.file = null;
    state.level = "none";
    refresh();
    fillProviders();
  }
}

/** Текст отказа: свой - из словаря, чужой - как прислал сервер. */
function text(error) {
  return t(error?.key ?? "api.failed", error?.params ?? { message: String(error) });
}

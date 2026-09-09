// Панели кнопок холста: место и видимость.
//
// # Что здесь решается
//
// Куда встала панель и показывать ли её. Место - имя одного из четырёх углов
// холста, а не координата: смена размера окна, масштаба и выхода в композицию
// такое место не рушит, а раскладка остаётся опрятной без усилий читателя.
//
// # Где живёт расположение
//
// Настройки схемы едут с проектом: читатель видит лист таким, каким его оформил
// автор. Расположение панелей - другое: это привычка того, кто сидит за
// экраном, и в публикацию ей ходу нет. Отсюда хранилище браузера, рядом с
// черновиком и долями разделителей.
//
// # Видимость называется словом, а не выводится из ширины
//
// У каждой панели три ступени: показывать всегда, только на широком экране, не
// показывать. Кнопка, снятая правилом вёрстки по ширине окна, не находится и не
// возвращается - о ней попросту не знают, и решать это вправе только читатель.

/** Ключ хранилища. Версия в ключе: состав панелей ещё будет меняться. */
const KEY = "takt:panels:v1";

/** Ширина, ниже которой экран считается узким; та же граница, что у вёрстки. */
const NARROW = "(max-width: 900px)";

/** Ступени видимости: первая - умолчание набора. */
export const WHEN = ["always", "wide", "hidden"];

/**
 * Панели страницы: место по умолчанию и видимость по умолчанию.
 *
 * Легенда стоит в этом же списке, хотя углов холста не занимает: читатель
 * спрашивает про неё то же самое - показывать или нет, - и второй список
 * ответил бы ему в другом окне.
 */
export const PANELS = [
  // Дома панелей разведены по углам так, чтобы ни одна пара не встретилась в
  // одной строке: холст рядом с легендой узок, и две панели в одном углу
  // накрывают друг друга - переставить их читатель уже не может.
  { id: "run", home: "tl", when: "always", label: "scheme.panel.run" },
  { id: "view", home: "bl", when: "always", label: "scheme.panel.view" },
  { id: "sheet", home: "br", when: "wide", label: "scheme.panel.sheet" },
  { id: "legend", home: null, when: "always", label: "scheme.panel.legend" },
];

/** Места на холсте в порядке обхода. */
export const DOCKS = ["tl", "tr", "bl", "br"];

/** Панель по имени; `undefined` - имени в списке нет. */
export function panelOf(id) {
  return PANELS.find((panel) => panel.id === id);
}

/**
 * Чистит прочитанное состояние: чужие имена и ступени вне набора отбрасываются.
 *
 * Хранилище переживает выкладки, а состав панелей меняется - запись о панели,
 * которой больше нет, не должна ни падать, ни оживать при возврате имени.
 *
 * @param {unknown} raw прочитанное значение
 * @returns {Record<string, {dock?: string, when?: string}>} состояние
 */
export function cleanState(raw) {
  const out = {};
  if (!raw || typeof raw !== "object") return out;
  for (const panel of PANELS) {
    const item = raw[panel.id];
    if (!item || typeof item !== "object") continue;
    const kept = {};
    if (panel.home && DOCKS.includes(item.dock)) kept.dock = item.dock;
    if (WHEN.includes(item.when)) kept.when = item.when;
    if (Object.keys(kept).length > 0) out[panel.id] = kept;
  }
  return out;
}

/**
 * Видна ли панель при такой ступени и такой ширине.
 *
 * @param {string} when ступень видимости
 * @param {boolean} narrow узкий ли экран
 */
export function visible(when, narrow) {
  if (when === "hidden") return false;
  if (when === "wide") return !narrow;
  return true;
}

/** Состояние панелей: место и видимость каждой, с умолчаниями. */
export function stateOf(saved) {
  const clean = cleanState(saved);
  const out = {};
  for (const panel of PANELS) {
    out[panel.id] = {
      dock: clean[panel.id]?.dock ?? panel.home,
      when: clean[panel.id]?.when ?? panel.when,
    };
  }
  return out;
}

/**
 * Панели области схемы: расстановка, перемещение, видимость.
 *
 * Хранилище передаётся снаружи - тесты гоняют носитель без браузера, а окно
 * приватного просмотра отвечает отказом на запись, и падать из-за этого
 * страница не вправе.
 */
export class Panels {
  /**
   * @param {object} dom узлы: холст, панели по имени, легенда и её разделители
   * @param {object} options `store` - хранилище, `onChange` - после правки
   */
  constructor(dom, options = {}) {
    this.dom = dom;
    this.store = options.store ?? safeStorage();
    this.onChange = options.onChange ?? (() => {});
    this.narrowQuery = options.media ?? window.matchMedia(NARROW);
    this.state = stateOf(this.read());
    this.wire();
    this.apply();
  }

  read() {
    try {
      return JSON.parse(this.store.getItem(KEY) || "{}");
    } catch {
      return {};
    }
  }

  save() {
    try {
      this.store.setItem(KEY, JSON.stringify(this.state));
    } catch {
      // Приватное окно отказывает в записи: расстановка живёт до перезагрузки.
    }
    this.onChange();
  }

  get narrow() {
    return Boolean(this.narrowQuery?.matches);
  }

  /** Ступень видимости панели. */
  whenOf(id) {
    return this.state[id]?.when ?? panelOf(id)?.when ?? WHEN[0];
  }

  /** Ставит ступень видимости; чужое имя и ступень вне набора игнорируются. */
  setWhen(id, when) {
    if (!panelOf(id) || !WHEN.includes(when)) return;
    this.state[id].when = when;
    this.apply();
    this.save();
  }

  /** Снимок расстановки: окно настроек обещает отмену и обязана её сдержать. */
  snapshot() {
    return JSON.parse(JSON.stringify(this.state));
  }

  /** Возвращает расстановку к снимку. */
  restore(state) {
    this.state = stateOf(state);
    this.apply();
    this.save();
  }

  /** Возвращает панели по домашним местам и показывает все. */
  reset() {
    this.state = stateOf({});
    this.apply();
    this.save();
  }

  /** Расставляет панели по местам и применяет видимость. */
  apply() {
    for (const panel of PANELS) {
      const node = this.dom.panels?.[panel.id];
      if (!node) continue;
      const { dock, when } = this.state[panel.id];
      if (panel.home) {
        const place = this.dom.docks?.[dock] ?? this.dom.docks?.[panel.home];
        if (place && node.parentElement !== place) place.appendChild(node);
      }
      node.hidden = !visible(when, this.narrow);
    }
    // У легенды, кроме неё самой, есть разделители: скрытая легенда, чья ручка
    // осталась на экране, тянулась бы в пустоту.
    const legendHidden = !visible(this.whenOf("legend"), this.narrow);
    for (const split of this.dom.legendSplits ?? []) split.hidden = legendHidden;
  }

  /** Подключает перемещение и слежение за шириной. */
  wire() {
    for (const panel of PANELS) {
      const node = this.dom.panels?.[panel.id];
      const grip = node?.querySelector(".tool-grip");
      if (grip) grip.addEventListener("pointerdown", (event) => this.drag(event, panel, node));
      // Панель стоит на холсте, и холст возит лист нажатием: без этого нажатие
      // мимо кнопки внутри панели уводило бы рисунок.
      if (node) node.addEventListener("pointerdown", (event) => event.stopPropagation());
    }
    this.narrowQuery?.addEventListener?.("change", () => this.apply());
  }

  /** Перемещение панели: летит за указателем, встаёт в ближайшее место. */
  drag(event, panel, node) {
    if (event.button) return;
    event.preventDefault();
    const scheme = this.dom.scheme;
    const area = scheme.getBoundingClientRect();
    const box = node.getBoundingClientRect();
    const grab = { x: event.clientX - box.left, y: event.clientY - box.top };
    scheme.appendChild(node);
    scheme.classList.add("docking");
    node.classList.add("dragging");
    node.style.position = "absolute";

    const move = (moved) => {
      node.style.left = `${moved.clientX - area.left - grab.x}px`;
      node.style.top = `${moved.clientY - area.top - grab.y}px`;
      const near = this.nearestDock(moved);
      for (const name of DOCKS) this.dom.docks[name]?.classList.toggle("hot", this.dom.docks[name] === near);
    };
    const stop = (ended) => {
      window.removeEventListener("pointermove", move);
      window.removeEventListener("pointerup", stop);
      node.classList.remove("dragging");
      scheme.classList.remove("docking");
      node.style.position = node.style.left = node.style.top = "";
      const near = this.nearestDock(ended);
      for (const name of DOCKS) this.dom.docks[name]?.classList.remove("hot");
      const dock = near?.dataset.dock ?? panel.home;
      this.state[panel.id].dock = dock;
      this.apply();
      this.save();
    };
    window.addEventListener("pointermove", move);
    window.addEventListener("pointerup", stop);
  }

  /** Ближайшее к указателю место: считается по середине площадки. */
  nearestDock(event) {
    let best = null;
    let dist = Infinity;
    for (const name of DOCKS) {
      const dock = this.dom.docks?.[name];
      if (!dock) continue;
      const box = dock.getBoundingClientRect();
      const dx = event.clientX - (box.left + box.width / 2);
      const dy = event.clientY - (box.top + box.height / 2);
      const value = dx * dx + dy * dy;
      if (value < dist) {
        dist = value;
        best = dock;
      }
    }
    return best;
  }
}

/** Хранилище браузера, а при отказе - пустое: страница обязана работать и так. */
function safeStorage() {
  try {
    const probe = window.localStorage;
    probe.getItem(KEY);
    return probe;
  } catch {
    return { getItem: () => null, setItem: () => {} };
  }
}

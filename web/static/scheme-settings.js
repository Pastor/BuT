// Настройки отрисовки схемы: окно с вкладками и кнопками-образцами.
//
// # Что здесь решается
//
// Как нарисован лист: толщина линий, форма наконечника, скругление углов, место
// знака условия, гарнитура и кегль, гамма, шаг сетки и привязка. Настройки живут
// в файле раскладки (`layout.js`), то есть едут с проектом: читатель видит схему
// такой, какой её оформил автор.
//
// # Образцы вместо списков
//
// Выбор здесь зрительный: "тонкая линия" словами не отличить от "обычной", пока
// их не увидишь рядом. Каждая ступень нарисована на своей кнопке тем же
// оформлением, что и лист, - кнопка показывает результат, а не называет его.
//
// # Живой предпросмотр
//
// Правка видна на схеме позади окна сразу: выбор ступени зовёт `onPick`, а
// хозяин пишет её в раскладку и перерисовывает лист. "Отменить" возвращает
// раскладку к снимку, снятому при открытии, - откат целиком, а не по шагам.

import { PANELS } from "./panels.js";

const NS = "http://www.w3.org/2000/svg";

/** Узел SVG с атрибутами. */
function mk(tag, attrs = {}) {
  const node = document.createElementNS(NS, tag);
  for (const [key, value] of Object.entries(attrs)) node.setAttribute(key, String(value));
  return node;
}

/**
 * Состав окна: вкладки, поля, ступени.
 *
 * Ступени перечислены здесь в порядке показа, а не в порядке набора `layout.VIEW`
 * (там первое значение - умолчание). Порядок показа - от тонкого к жирному, от
 * начала к концу: ряд кнопок читается как шкала.
 */
const TABS = [
  {
    id: "lines",
    label: "scheme.settings.lines",
    fields: [
      {
        id: "edgeWidth",
        label: "scheme.settings.edgeWidth",
        sample: edgeSample,
        steps: [
          { value: "thin", label: "scheme.step.thin" },
          { value: "normal", label: "scheme.step.normal" },
          { value: "bold", label: "scheme.step.bold" },
        ],
      },
      {
        id: "arrow",
        label: "scheme.settings.arrow",
        sample: arrowSample,
        steps: [
          { value: "open", label: "scheme.arrow.open" },
          { value: "solid", label: "scheme.arrow.solid" },
          { value: "line", label: "scheme.arrow.line" },
        ],
      },
      {
        id: "nodeWidth",
        label: "scheme.settings.nodeWidth",
        sample: nodeSample,
        steps: [
          { value: "thin", label: "scheme.step.thin" },
          { value: "normal", label: "scheme.step.normal" },
          { value: "bold", label: "scheme.step.bold" },
        ],
      },
      {
        id: "corners",
        label: "scheme.settings.corners",
        sample: cornerSample,
        steps: [
          { value: "square", label: "scheme.cornersSquare" },
          { value: "round", label: "scheme.cornersRound" },
        ],
      },
      {
        id: "labelPlace",
        label: "scheme.settings.labelPlace",
        sample: placeSample,
        steps: [
          { value: "start", label: "scheme.markStart" },
          { value: "center", label: "scheme.markCenter" },
          { value: "end", label: "scheme.markEnd" },
        ],
      },
    ],
  },
  {
    id: "font",
    label: "scheme.settings.font",
    fields: [
      {
        id: "stateFont",
        label: "scheme.settings.stateFont",
        sample: fontSample,
        steps: [
          { value: "gost", label: "scheme.font.gost" },
          { value: "mono", label: "scheme.font.mono" },
        ],
      },
      {
        id: "stateSize",
        label: "scheme.settings.stateSize",
        sample: sizeSample,
        steps: [
          { value: "sm", label: "scheme.size.sm" },
          { value: "md", label: "scheme.size.md" },
          { value: "lg", label: "scheme.size.lg" },
        ],
      },
      {
        id: "condFont",
        label: "scheme.settings.condFont",
        sample: condFontSample,
        steps: [
          { value: "gost", label: "scheme.font.gost" },
          { value: "mono", label: "scheme.font.mono" },
        ],
      },
      {
        id: "condSize",
        label: "scheme.settings.condSize",
        sample: condSizeSample,
        steps: [
          { value: "xs", label: "scheme.size.xs" },
          { value: "sm", label: "scheme.size.sm" },
          { value: "md", label: "scheme.size.md" },
        ],
      },
    ],
  },
  {
    id: "color",
    label: "scheme.settings.color",
    fields: [
      {
        id: "gamma",
        label: "scheme.settings.gamma",
        sample: gammaSample,
        steps: [
          { value: "color", label: "scheme.gamma.color" },
          { value: "draft", label: "scheme.gamma.draft" },
          { value: "contrast", label: "scheme.gamma.contrast" },
        ],
      },
    ],
  },
  {
    id: "grid",
    label: "scheme.settings.grid",
    fields: [
      {
        id: "grid",
        label: "scheme.settings.gridStep",
        sample: gridSample,
        steps: [
          { value: "small", label: "scheme.grid.small" },
          { value: "medium", label: "scheme.grid.medium" },
          { value: "large", label: "scheme.grid.large" },
          { value: "off", label: "scheme.grid.off" },
        ],
      },
      {
        id: "snap",
        label: "scheme.settings.snap",
        sample: snapSample,
        steps: [
          { value: true, label: "scheme.snap.on" },
          { value: false, label: "scheme.snap.off" },
        ],
      },
      {
        id: "marks",
        label: "scheme.settings.marks",
        sample: marksSample,
        steps: [
          { value: true, label: "scheme.marks.on" },
          { value: false, label: "scheme.marks.off" },
        ],
      },
    ],
  },
  {
    // Панели холста: показывать ли каждую. Образцов здесь нет - выбор словесный,
    // и рисовать "всегда" нечем. Состав вкладки строится по списку панелей:
    // второй список разошёлся бы с ним молча.
    id: "panels",
    label: "scheme.settings.panels",
    fields: PANELS.map((panel) => ({
      id: `panel:${panel.id}`,
      label: panel.label,
      steps: [
        { value: "always", label: "scheme.when.always" },
        { value: "wide", label: "scheme.when.wide" },
        { value: "hidden", label: "scheme.when.hidden" },
      ],
    })),
  },
];

/** Толщина линии ступенью: те же числа, что у листа (`app.css`). */
const WIDTHS = { thin: 1, normal: 1.5, bold: 2.5 };

/**
 * Строит окно настроек.
 *
 * @param {object} dom узлы: `modal`, `tabs`, `body`, `save`, `cancel`
 * @param {object} options `{t, values(), onPick(key, value), onSave(), onCancel()}`
 */
export class Settings {
  constructor(dom, options) {
    this.dom = dom;
    this.t = options.t;
    this.values = options.values;
    this.onPick = options.onPick ?? (() => {});
    this.onSave = options.onSave ?? (() => {});
    this.onCancel = options.onCancel ?? (() => {});
    this.tab = TABS[0].id;
    this.wire();
  }

  wire() {
    const { modal, tabs, save, cancel } = this.dom;
    tabs.addEventListener("click", (event) => {
      const button = event.target.closest("[data-tab]");
      if (!button) return;
      this.tab = button.dataset.tab;
      this.paint();
    });
    save.addEventListener("click", () => {
      this.close();
      this.onSave();
    });
    cancel.addEventListener("click", () => {
      this.close();
      this.onCancel();
    });
    // Уход мимо кнопок - тоже отказ: окно правит раскладку на глазах, и закрытие
    // без слова "сохранить" не вправе оставлять правку.
    modal.addEventListener("pointerdown", (event) => {
      if (event.target === modal) {
        this.close();
        this.onCancel();
      }
    });
    modal.addEventListener("keydown", (event) => {
      if (event.key === "Escape") {
        event.stopPropagation();
        this.close();
        this.onCancel();
      }
    });
  }

  /** Открывает окно; текущие ступени спрашиваются у хозяина. */
  open() {
    this.dom.modal.hidden = false;
    this.paint();
    this.dom.save.focus();
  }

  close() {
    this.dom.modal.hidden = true;
  }

  get isOpen() {
    return !this.dom.modal.hidden;
  }

  /** Перерисовывает полосу вкладок и поля открытой вкладки. */
  paint() {
    const { tabs, body } = this.dom;
    const values = this.values();
    tabs.replaceChildren();
    for (const tab of TABS) {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "tab";
      button.dataset.tab = tab.id;
      button.setAttribute("role", "tab");
      button.setAttribute("aria-selected", String(tab.id === this.tab));
      button.classList.toggle("active", tab.id === this.tab);
      button.textContent = this.t(tab.label);
      tabs.appendChild(button);
    }
    body.replaceChildren();
    const open = TABS.find((tab) => tab.id === this.tab) ?? TABS[0];
    for (const field of open.fields) body.appendChild(this.fieldRow(field, values));
  }

  /** Ряд одного поля: подпись и кнопки-образцы. */
  fieldRow(field, values) {
    const row = document.createElement("div");
    row.className = "set-row";
    const title = document.createElement("span");
    title.className = "set-name";
    title.id = `set-${field.id}`;
    title.textContent = this.t(field.label);
    row.appendChild(title);
    const group = document.createElement("div");
    group.className = "set-steps";
    group.setAttribute("role", "radiogroup");
    group.setAttribute("aria-labelledby", title.id);
    for (const step of field.steps) {
      const label = this.t(step.label);
      const button = document.createElement("button");
      button.type = "button";
      button.className = "set-step";
      button.setAttribute("role", "radio");
      button.setAttribute("aria-checked", String(values[field.id] === step.value));
      button.setAttribute("aria-label", label);
      button.dataset.tip = label;
      // Поле без образца - словесный выбор: рисовать нечего, и кнопка остаётся
      // подписью. Пустой образец на его месте занимал бы место обещанием.
      if (field.sample) button.appendChild(field.sample(step.value));
      else button.classList.add("set-step-plain");
      const caption = document.createElement("span");
      caption.className = "set-step-name";
      caption.textContent = label;
      button.appendChild(caption);
      button.addEventListener("click", () => {
        this.onPick(field.id, step.value);
        this.paint();
      });
      group.appendChild(button);
    }
    row.appendChild(group);
    return row;
  }
}

// ── Образцы ступеней ──────────────────────────────────────────────────────
//
// Каждый образец рисуется теми же классами, что и лист: сменится оформление -
// сменится и картинка на кнопке. Второй набор правил рисования завёл бы кнопку,
// обещающую не то, что получится.

/** Холст образца: ширина по числу ступеней, высота одна на все. */
function box(width = 56) {
  return mk("svg", { class: "set-sample", viewBox: `0 0 ${width} 28`, width, height: 28, "aria-hidden": "true" });
}

function edgeSample(step) {
  const svg = box();
  svg.appendChild(mk("path", { class: "edge", d: "M6 14h44", "stroke-width": WIDTHS[step] }));
  return svg;
}

function nodeSample(step) {
  const svg = box();
  svg.appendChild(mk("circle", { class: "node-body", cx: 28, cy: 14, r: 10, "stroke-width": WIDTHS[step] }));
  return svg;
}

function arrowSample(step) {
  const svg = box();
  svg.appendChild(mk("path", { class: "edge", d: "M6 14h32", "stroke-width": 1.5 }));
  const head = { open: "M38 8L50 14L38 20", solid: "M38 8L50 14L38 20z", line: "M40 9L50 14L40 19" }[step];
  svg.appendChild(mk("path", { class: step === "solid" ? "arrow-solid" : "arrow-open", d: head, "stroke-width": 1.5 }));
  return svg;
}

function cornerSample(step) {
  const svg = box();
  const d = step === "round" ? "M8 22V16a6 6 0 0 1 6-6h34" : "M8 22V10h40";
  svg.appendChild(mk("path", { class: "edge", d, "stroke-width": 1.5 }));
  return svg;
}

function placeSample(step) {
  const svg = box();
  svg.appendChild(mk("path", { class: "edge", d: "M6 14h44", "stroke-width": 1.5 }));
  const x = { start: 14, center: 28, end: 42 }[step];
  svg.appendChild(mk("rect", { class: "set-mark-bg", x: x - 7, y: 2, width: 14, height: 12, rx: 2 }));
  const text = mk("text", { class: "edge-mark", x, y: 12 });
  text.textContent = "K";
  svg.appendChild(text);
  return svg;
}

function fontSample(step) {
  // Кегль образца гарнитуры постоянен: сравнивают форму знака, а не его величину -
  // её выбирают соседним полем.
  const font = step === "mono" ? "font-family: var(--font-code); " : "";
  return markBox("node-mark", "node-num", "S", "1", `${font}font-size: 15px`);
}

function sizeSample(step) {
  const size = { xs: 10, sm: 13, md: 16, lg: 20 }[step];
  return markBox("node-mark", "node-num", "S", "1", `font-size: ${size}px`);
}

/** Знак с номером: та же пара классов, что на листе, - индекс едет за кеглем. */
function markBox(cls, numCls, letter, number, style) {
  const svg = box();
  const text = mk("text", { class: cls, x: 28, y: 19, style });
  text.textContent = letter;
  const num = mk("tspan", { class: numCls, dy: "0.3em" });
  num.textContent = number;
  text.appendChild(num);
  svg.appendChild(text);
  return svg;
}

function condFontSample(step) {
  const font = step === "mono" ? "font-family: var(--font-code); " : "";
  return markBox("edge-mark", "edge-num", "K", "1", `${font}font-size: 15px`);
}

function condSizeSample(step) {
  const size = { xs: 10, sm: 13, md: 16 }[step];
  return markBox("edge-mark", "edge-num", "K", "1", `font-size: ${size}px`);
}

/** Легенда значков: три строки расшифровки либо их отсутствие. */
function marksSample(step) {
  const svg = box();
  if (!step) {
    svg.appendChild(mk("path", { class: "edge", d: "M14 14h28", "stroke-width": 1.5 }));
    svg.appendChild(mk("circle", { class: "node-body", cx: 10, cy: 14, r: 4, "stroke-width": 1.5 }));
    return svg;
  }
  for (let i = 0; i < 3; i += 1) {
    svg.appendChild(mk("circle", { class: "node-body", cx: 10, cy: 6 + i * 8, r: 3, "stroke-width": 1.2 }));
    svg.appendChild(mk("path", { class: "edge", d: `M18 ${6 + i * 8}h26`, "stroke-width": 1.2 }));
  }
  return svg;
}

function gammaSample(step) {
  const svg = box();
  // Кружки рисуются в обёртке узла: вид состояния задают правила `.node.running
  // .node-body`, и кружок без родителя-узла остаётся бесцветным.
  const states = step === "draft" ? ["", "", ""] : ["", "running", "expected"];
  states.forEach((state, i) => {
    const group = mk("g", { class: `node ${state}`.trim() });
    group.appendChild(mk("circle", { class: "node-body", cx: 14 + i * 14, cy: 14, r: 6 }));
    svg.appendChild(group);
  });
  svg.dataset.gamma = step;
  return svg;
}

function gridSample(step) {
  const svg = box();
  if (step === "off") {
    const text = mk("text", { class: "edge-mark", x: 28, y: 18 });
    text.textContent = "—";
    svg.appendChild(text);
    return svg;
  }
  const gap = { small: 6, medium: 9, large: 13 }[step];
  for (let x = 8; x < 50; x += gap) {
    for (let y = 5; y < 25; y += gap) {
      svg.appendChild(mk("circle", { class: "set-dot", cx: x, cy: y, r: 1 }));
    }
  }
  return svg;
}

function snapSample(step) {
  const svg = box();
  for (let x = 10; x < 48; x += 9) {
    for (let y = 5; y < 25; y += 9) svg.appendChild(mk("circle", { class: "set-dot", cx: x, cy: y, r: 1 }));
  }
  // Привязка показана положением узла: точно на точке сетки либо между точками.
  svg.appendChild(mk("circle", { class: "node-body", cx: step ? 28 : 32, cy: step ? 14 : 17, r: 6, "stroke-width": 1.5 }));
  return svg;
}

// Окно настроек сборки: цель и ключи.
//
// # Что здесь решается
//
// Панель генерации показывает вывод, а что строить - цель и ключи сборки - выбирается
// в окне того же вида, что основные настройки: вкладки, кнопки-образцы, "Отменить" и
// "Сохранить". Цель - кнопка-образец с фрагментом вывода: по нему цель узнают быстрее,
// чем по имени. Ключи - те же карточки, что строит страница (`drawFlags`).
//
// # Носитель величины один
//
// Своего состояния у окна нет: цель живёт в списке `#target`, ключи - в строке
// `#args`, и окно правит их тем же путём, что прежние вкладки. "Отменить" возвращает
// пару, снятую при открытии, - правка видна в выводе позади окна сразу, как у
// настроек схемы, и уход без "Сохранить" не вправе её оставить.

/**
 * Группы целей в порядке показа.
 *
 * Имена целей приходят от модуля (`version.targets`); группа лишь раскладывает их
 * по назначению. Цель, которой здесь нет, не пропадает: она уходит в группу
 * "Прочие" - список модуля сильнее списка страницы.
 */
const GROUPS = [
  { id: "mcu", label: "build.group.mcu", targets: ["c", "c-hal"] },
  { id: "plc", label: "build.group.plc", targets: ["st", "st-at"] },
  { id: "rust", label: "build.group.rust", targets: ["rust"] },
  { id: "fpga", label: "build.group.fpga", targets: ["sv", "sv-mmio"] },
  { id: "diagram", label: "build.group.diagram", targets: ["plantuml"] },
];

/**
 * Опись целей для кнопок окна: пояснение (ключ словаря) и фрагмент вывода - по
 * нему цель узнают быстрее, чем по имени. Подписи объявлены описью, как у ключей
 * сборки (`flags.js`), и в текст приходят через `t(spec.label)`.
 */
const TARGETS = {
  c: { label: "build.why.c", sample: "void tick(M *m)\n{ switch (…) }" },
  "c-hal": { label: "build.why.cHal", sample: "write_u8(PORT, …);\nread_bit(…)" },
  st: { label: "build.why.st", sample: "CASE state OF\n  1: …" },
  "st-at": { label: "build.why.stAt", sample: "x AT %QX0.1\n  : BOOL;" },
  rust: { label: "build.why.rust", sample: "match self.state\n{ … }" },
  sv: { label: "build.why.sv", sample: "always_ff @(posedge\n  clk) …" },
  "sv-mmio": { label: "build.why.svMmio", sample: "reg_wdata,\nreg_wen …" },
  plantuml: { label: "build.why.plantuml", sample: "@startuml\n[*] --> Idle" },
};

/**
 * Раскладывает цели модуля по группам: известные - по месту, прочие - последней
 * группой; пустые группы не показываются.
 *
 * @param {string[]} targets имена целей от модуля
 */
export function groupTargets(targets) {
  const known = new Set(GROUPS.flatMap((group) => group.targets));
  const out = GROUPS.map((group) => ({ ...group, targets: group.targets.filter((name) => targets.includes(name)) }))
    .filter((group) => group.targets.length > 0);
  const rest = targets.filter((name) => !known.has(name));
  if (rest.length > 0) out.push({ id: "other", label: "build.group.other", targets: rest });
  return out;
}

/**
 * Подключает окно настроек сборки.
 *
 * @param {object} dom узлы окна: `modal`, `tabs`, `targetPage`, `flagsPage`, `line`,
 *   `save`, `cancel`, `opener`
 * @param {object} host хозяин величин: `t`, `targets()`, `target()`, `args()`,
 *   `pick(target)`, `restore({target, args})`, `tab()`, `rememberTab(name)`
 * @returns {{open: Function, paint: Function}}
 */
export function attachBuildSettings(dom, host) {
  let before = null;
  let tab = host.tab() === "flags" ? "flags" : "target";

  const close = (keep) => {
    if (dom.modal.hidden) return;
    dom.modal.hidden = true;
    if (!keep && before) host.restore(before);
    before = null;
    dom.opener?.focus();
  };

  const paintTargets = () => {
    const page = dom.targetPage;
    page.replaceChildren();
    const chosen = host.target();
    for (const group of groupTargets(host.targets())) {
      const row = document.createElement("div");
      row.className = "set-row";
      const title = document.createElement("span");
      title.className = "set-name";
      title.id = `build-${group.id}`;
      title.textContent = host.t(group.label);
      const steps = document.createElement("div");
      steps.className = "set-steps";
      steps.setAttribute("role", "radiogroup");
      steps.setAttribute("aria-labelledby", title.id);
      for (const name of group.targets) {
        const button = document.createElement("button");
        button.type = "button";
        button.className = "set-step build-step";
        button.setAttribute("role", "radio");
        button.setAttribute("aria-checked", String(name === chosen));
        button.dataset.target = name;
        const sample = document.createElement("span");
        sample.className = "set-sample build-sample";
        sample.setAttribute("aria-hidden", "true");
        sample.textContent = TARGETS[name]?.sample ?? name;
        const caption = document.createElement("span");
        caption.className = "set-step-name build-name";
        caption.textContent = name;
        button.append(sample, caption);
        const spec = TARGETS[name];
        if (spec) {
          const why = document.createElement("span");
          why.className = "build-why";
          why.textContent = host.t(spec.label);
          button.append(why);
          button.setAttribute("aria-label", `${name} - ${why.textContent}`);
        } else {
          button.setAttribute("aria-label", name);
        }
        button.addEventListener("click", () => {
          host.pick(name);
          paint();
        });
        steps.append(button);
      }
      row.append(title, steps);
      page.append(row);
    }
  };

  /** Перерисовывает вкладки, открытую страницу и итоговую строку ключей. */
  function paint() {
    for (const button of dom.tabs.querySelectorAll("[data-tab]")) {
      const active = button.dataset.tab === tab;
      button.classList.toggle("active", active);
      button.setAttribute("aria-selected", String(active));
    }
    dom.targetPage.hidden = tab !== "target";
    dom.flagsPage.hidden = tab !== "flags";
    if (tab === "target") paintTargets();
    const args = host.args();
    dom.line.textContent = args.trim() === "" ? host.t("build.noFlags") : args;
  }

  dom.tabs.addEventListener("click", (event) => {
    const button = event.target.closest("[data-tab]");
    if (!button) return;
    tab = button.dataset.tab;
    host.rememberTab(tab);
    paint();
  });
  dom.save.addEventListener("click", () => close(true));
  dom.cancel.addEventListener("click", () => close(false));
  // Уход мимо кнопок - тоже отказ, как у основных настроек: правка видна сразу, и
  // закрытие без "Сохранить" не вправе её оставлять.
  dom.modal.addEventListener("pointerdown", (event) => {
    if (event.target === dom.modal) close(false);
  });
  dom.modal.addEventListener("keydown", (event) => {
    if (event.key === "Escape") {
      event.stopPropagation();
      close(false);
    }
  });

  return {
    open() {
      before = { target: host.target(), args: host.args() };
      dom.modal.hidden = false;
      paint();
      dom.save.focus();
    },
    paint() {
      if (!dom.modal.hidden) paint();
    },
  };
}

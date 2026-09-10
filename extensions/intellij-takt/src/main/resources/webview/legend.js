// Легенда обозначений и навигатор схемы.
//
// Легенда объясняет знаки текущего листа: две таблицы - состояния (`S`+номер) и
// условия переходов (`K`+номер), каждая "знак -> текст из модели -> подпись автора ->
// примета". Третьей идёт расшифровка обозначений - что значат цвет узла и вид
// линии; её показ - настройка схемы: чертёж, знаки которого читатель уже знает,
// расшифровки не требует. Имя из модели только читается (модель правится текстом), подпись автора -
// поле ввода: она живёт в файле раскладки рядом с координатами.
//
// Навигатор ведёт по уровням модели: строка листа и строки состояний с обозначением
// и видом. Это разные контролы: слив их, мы получили бы дерево, в котором правят
// подписи, - и правка уезжала бы вместе с уровнем.
//
// Узлы строятся построением, а не разметкой из данных: имена состояний и подписи
// пишет автор проекта, а видят их читатели витрины.

/** Текстовый узел с классом. */
function span(cls, text) {
  const node = document.createElement("span");
  node.className = cls;
  node.textContent = text;
  return node;
}

/** Строка таблицы легенды из ячеек. */
function row(cells, cls = "legend-row") {
  const node = document.createElement("div");
  node.className = cls;
  for (const cell of cells) node.appendChild(cell);
  return node;
}

/** Подсказка знака: латиница из модели и подпись автора, если она есть. */
export function tipOf(mark, source, alias) {
  return alias ? `${mark} · ${source} · ${alias}` : `${mark} · ${source}`;
}

/** Поле подписи автора; изменение уходит вызывающему по вводу. */
function aliasField(value, placeholder, label, onInput) {
  const input = document.createElement("input");
  input.className = "legend-alias";
  input.type = "text";
  input.spellcheck = false;
  input.value = value ?? "";
  input.placeholder = placeholder;
  input.setAttribute("aria-label", label);
  input.addEventListener("input", () => onInput(input.value));
  return input;
}

/**
 * Рисует легенду листа.
 *
 * @param {HTMLElement} container узел легенды (очищается)
 * @param {object} ctx `{t, sheet, kinds, selectedNode, selectedEdge, floating, title,
 *   onNode(name), onEdge(key), onAlias(name, text), onEdgeAlias(key, text),
 *   onTitleDown(event)}`
 */
export function paintLegend(container, ctx) {
  const { t, sheet } = ctx;
  container.replaceChildren();
  if (ctx.floating) {
    const title = document.createElement("div");
    title.className = "legend-title";
    title.textContent = t("scheme.legend.move", { sheet: ctx.title });
    title.addEventListener("pointerdown", ctx.onTitleDown);
    container.appendChild(title);
  }

  const states = document.createElement("div");
  states.className = "legend-states";
  const statesTitle = document.createElement("div");
  statesTitle.className = "legend-title";
  statesTitle.textContent = t("scheme.legend.states");
  states.appendChild(statesTitle);
  states.appendChild(
    row(
      [
        span("", t("scheme.legend.mark")),
        span("", t("scheme.legend.name")),
        span("", t("scheme.legend.alias")),
        span("", t("scheme.legend.kind")),
      ],
      "legend-row legend-head",
    ),
  );
  for (const node of sheet.nodes) {
    // Шаг композиции называется моделью, которая его реализует: имя узла листа -
    // служебное (`Heater#1`), автору оно ничего не говорит.
    const source = node.model ?? node.name;
    const line = row([
      span("legend-mark", node.mark),
      span("legend-name", source),
      // Подпись ставится и на листе композиции: он строится из выражения, но
      // подписи его квадратов живут в раскладке, как у состояний.
      sheet.namesAt
        ? aliasField(
            node.alias,
            t("scheme.legend.aliasEmpty"),
            t("scheme.legend.aliasOf", { name: source }),
            (text) => ctx.onAlias(node.name, text),
          )
        : span("legend-name", node.alias ?? ""),
      span("legend-kind", ctx.kinds[node.kind] ?? node.kind),
    ]);
    if (node.name === ctx.selectedNode) line.setAttribute("aria-selected", "true");
    line.setAttribute("data-tip", tipOf(node.mark, source, node.alias));
    line.addEventListener("click", (event) => {
      if (event.target.tagName !== "INPUT") ctx.onNode(node.name);
    });
    states.appendChild(line);
  }
  container.appendChild(states);

  const conds = document.createElement("div");
  conds.className = "legend-conds";
  const condsTitle = document.createElement("div");
  condsTitle.className = "legend-title";
  condsTitle.textContent = t("scheme.legend.conditions");
  conds.appendChild(condsTitle);
  conds.appendChild(
    row(
      [
        span("", t("scheme.legend.mark")),
        span("", t("scheme.legend.condition")),
        span("", t("scheme.legend.alias")),
        span("", t("scheme.legend.transition")),
      ],
      "legend-row legend-head",
    ),
  );
  const marks = new Map(sheet.nodes.map((n) => [n.name, n.mark]));
  let any = false;
  for (const edge of sheet.edges) {
    if (!edge.cond) continue;
    any = true;
    const cond = span("legend-cond", edge.cond);
    cond.setAttribute("data-tip", tipOf(edge.mark, edge.cond, edge.alias));
    const line = row([
      span("legend-mark", edge.mark),
      cond,
      sheet.editable
        ? aliasField(
            edge.alias,
            t("scheme.legend.condEmpty"),
            t("scheme.legend.aliasOf", { name: edge.cond }),
            (text) => ctx.onEdgeAlias(edge.key, text),
          )
        : span("legend-name", edge.alias ?? ""),
      span("legend-edge", `${marks.get(edge.from) ?? edge.from} → ${marks.get(edge.to) ?? edge.to}`),
    ]);
    if (edge.key === ctx.selectedEdge) line.setAttribute("aria-selected", "true");
    line.addEventListener("click", (event) => {
      if (event.target.tagName !== "INPUT") ctx.onEdge(edge.key);
    });
    conds.appendChild(line);
  }
  // Условий на листе нет - нет и пункта: заголовок с пустой таблицей занимал место
  // и ничего не называл.
  if (any) container.appendChild(conds);

  // Расшифровка обозначений идёт последней и по настройке: она объясняет знаки, а
  // не называет содержимое листа, и автору, который их знает, только занимает место.
  if (ctx.marks) container.appendChild(marksBlock(t));
}

/**
 * Расшифровка обозначений: цвета состояний и виды линий.
 *
 * Образцы рисуются теми же классами, что и лист: сменится оформление - сменится и
 * образец. Второй набор правил рисования обещал бы не то, что нарисовано на схеме.
 */
function marksBlock(t) {
  const NS = "http://www.w3.org/2000/svg";
  const shape = (tag, attrs) => {
    const node = document.createElementNS(NS, tag);
    for (const [key, value] of Object.entries(attrs)) node.setAttribute(key, String(value));
    return node;
  };
  const box = (draw) => {
    const svg = document.createElementNS(NS, "svg");
    svg.setAttribute("class", "legend-sample");
    svg.setAttribute("viewBox", "0 0 44 24");
    svg.setAttribute("aria-hidden", "true");
    draw(svg);
    return svg;
  };
  // Узел рисуется в той же обёртке, что на листе: вид состояния задают правила
  // `.node.running .node-body`, и кружок без родителя-узла остаётся бесцветным -
  // все пять образцов выглядели одинаково серыми.
  const node = (state) => box((svg) => {
    const group = shape("g", { class: `node ${state}`.trim() });
    group.appendChild(shape("circle", { class: "node-body", cx: 22, cy: 12, r: 8 }));
    svg.appendChild(group);
  });
  // Наконечник рисуется здесь же, а не берётся маркером листа: маркер объявлен в
  // `defs` того SVG и в чужом недоступен - стрелка приезжала голой линией.
  const line = (cls, head) => box((svg) => {
    svg.appendChild(shape("path", { class: cls, d: "M6 12h24" }));
    if (head === "solid") svg.appendChild(shape("path", { class: "arrow-solid", d: "M30 7l8 5-8 5z" }));
    if (head === "open") svg.appendChild(shape("path", { class: "arrow-open", d: "M30 7l8 5-8 5" }));
  });
  const loop = () => box((svg) => {
    svg.appendChild(shape("path", { class: "edge edge-loop", d: "M14 16a8 8 0 1 1 14 0" }));
    svg.appendChild(shape("path", { class: "arrow-open", d: "M24 11l4 5-6 1" }));
  });

  const block = document.createElement("div");
  block.className = "legend-marks";
  const title = document.createElement("div");
  title.className = "legend-title";
  title.textContent = t("scheme.legend.marks");
  block.appendChild(title);
  const items = [
    { draw: () => node(""), label: "scheme.legend.markState" },
    { draw: () => node("running"), label: "scheme.legend.markRunning" },
    { draw: () => node("expected"), label: "scheme.legend.markExpected" },
    { draw: () => node("reachable"), label: "scheme.legend.markReachable" },
    { draw: () => node("unplaced"), label: "scheme.legend.markUnplaced" },
    { draw: () => line("edge", "open"), label: "scheme.legend.markEdgeRef" },
    { draw: () => line("edge", "solid"), label: "scheme.legend.markEdgeNext" },
    { draw: () => loop(), label: "scheme.legend.markEdgeLoop" },
  ];
  for (const item of items) {
    block.appendChild(row([item.draw(), span("legend-name", t(item.label))], "legend-row legend-mark-row"));
  }
  return block;
}

/**
 * Рисует навигатор: лист и его состояния.
 *
 * @param {HTMLElement} container узел навигатора (очищается)
 * @param {object} ctx `{t, sheet, kinds, title, selectedNode, onNode(name), onEnter(name)}`
 */
export function paintNav(container, ctx) {
  const { t, sheet } = ctx;
  container.replaceChildren();
  const head = document.createElement("button");
  head.className = "nav-row nav-model";
  head.type = "button";
  head.appendChild(span("nav-mark", ""));
  head.appendChild(document.createTextNode(ctx.title));
  head.appendChild(span("nav-kind", t("scheme.kind.model")));
  container.appendChild(head);
  for (const node of sheet.nodes) {
    const line = document.createElement("button");
    line.className = "nav-row nav-state";
    line.type = "button";
    line.setAttribute("aria-selected", String(node.name === ctx.selectedNode));
    line.setAttribute("data-tip", tipOf(node.mark, node.model ?? node.name, node.alias));
    line.appendChild(span("nav-mark", node.mark));
    line.appendChild(document.createTextNode(node.alias || node.model || node.name));
    line.appendChild(span("nav-kind", ctx.kinds[node.kind] ?? node.kind));
    line.addEventListener("click", () => ctx.onNode(node.name));
    line.addEventListener("dblclick", () => ctx.onEnter(node.name));
    container.appendChild(line);
  }
}

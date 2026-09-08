// Легенда обозначений и навигатор схемы.
//
// Легенда объясняет знаки текущего листа: две таблицы - состояния (`S`+номер) и
// условия переходов (`K`+номер), каждая "знак -> текст из модели -> подпись автора ->
// примета". Имя из модели только читается (модель правится текстом), подпись автора -
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
    const line = row([
      span("legend-mark", node.mark),
      span("legend-name", node.name),
      sheet.editable
        ? aliasField(
            node.alias,
            t("scheme.legend.aliasEmpty"),
            t("scheme.legend.aliasOf", { name: node.name }),
            (text) => ctx.onAlias(node.name, text),
          )
        : span("legend-name", node.alias ?? ""),
      span("legend-kind", ctx.kinds[node.kind] ?? node.kind),
    ]);
    if (node.name === ctx.selectedNode) line.setAttribute("aria-selected", "true");
    line.setAttribute("data-tip", tipOf(node.mark, node.name, node.alias));
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
  if (!any) conds.appendChild(row([span("legend-kind", t("scheme.legend.noConditions"))]));
  container.appendChild(conds);
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
    line.setAttribute("data-tip", tipOf(node.mark, node.name, node.alias));
    line.appendChild(span("nav-mark", node.mark));
    line.appendChild(document.createTextNode(node.alias || node.name));
    line.appendChild(span("nav-kind", ctx.kinds[node.kind] ?? node.kind));
    line.addEventListener("click", () => ctx.onNode(node.name));
    line.addEventListener("dblclick", () => ctx.onEnter(node.name));
    container.appendChild(line);
  }
}

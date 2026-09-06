/**
 * Подсказка при наведении — своя, поверх страницы (задача `0531-09l`).
 *
 * ⚠️ Нативную подсказку (`title`) браузер рисует САМ: своим шрифтом, своим
 * фоном, со своей задержкой и мимо темы страницы. У кнопок-значков подпись
 * ушла в подсказку целиком, и чужой её вид стал заметен — просьба заказчика
 * 2026-09-05 «почему тултипы нативные? измени везде».
 *
 * **Устройство взято у референса** (`tamagotchi`, его `web/static/app.js`,
 * прочитан 2026-09-05), и вместе с ним три приёма, до которых своим умом
 * дошли не сразу:
 *
 * 1. **`title` перехватывается и СНИМАЕТСЯ** при первом показе. В разметке
 *    его нет вовсе (сторож грепом), но узел с `title` может прийти из кода —
 *    и тогда рядом со своей подсказкой браузер нарисует вторую, свою.
 * 2. **Без наведения подсказок нет** (`hover: hover`): на сенсорном экране
 *    наведение не кончается, и панель осталась бы висеть.
 * 3. **Панель меряется НЕВИДИМОЙ** (`visibility`, а не `hidden`): размеры
 *    нужны до того, как ей назначено место, иначе она мелькает в углу.
 *
 * ⚠️ Своё имя элементу подсказка НЕ даёт: `aria-describedby` заставил бы
 * диктора прочесть тот же текст дважды — доступное имя уже несёт `aria-label`
 * (кнопка-значок без текста иначе для диктора пуста). Панель помечена
 * `aria-hidden`: она — оформление того, что доступности уже сказано.
 */

/** Зазор между элементом и панелью, px. */
export const GAP = 8;

/** Отступ от края окна, px: панель не касается границы. */
export const EDGE = 8;

/**
 * Считает место панели: под элементом, а если под ним не помещается — над.
 * По горизонтали панель центрируется и прижимается к окну.
 *
 * Чистая функция — её и проверяют тесты: место панели иначе видно только
 * глазом, а глаз не смотрит на узкое окно и на элемент у самого края.
 *
 * @param {{left:number, top:number, width:number, height:number, bottom:number}} anchor
 * @param {{width:number, height:number}} tip
 * @param {{width:number, height:number}} view
 * @returns {{left:number, top:number, side:"below"|"above"}}
 */
export function place(anchor, tip, view) {
  const bottom = anchor.bottom ?? anchor.top + anchor.height;
  const below = bottom + GAP;
  // Под элементом - если панель туда влезает; иначе над ним, но не выше края.
  const fits = below + tip.height <= view.height - EDGE;
  const top = fits ? below : Math.max(EDGE, anchor.top - tip.height - GAP);

  const centred = anchor.left + anchor.width / 2 - tip.width / 2;
  const limit = Math.max(EDGE, view.width - tip.width - EDGE);
  const left = Math.min(Math.max(centred, EDGE), limit);

  return { left, top, side: fits ? "below" : "above" };
}

/** Ближайший предок с подсказкой (или сам узел). */
export function target(node) {
  return node && node.closest ? node.closest("[title], [data-tip]") : null;
}

/**
 * Забирает подпись у узла, снимая нативный `title`.
 * Возвращает текст подсказки либо пустую строку.
 */
export function claim(node) {
  if (node.hasAttribute("title")) {
    node.dataset.tip = node.getAttribute("title");
    node.removeAttribute("title");
  }
  return node.dataset.tip ?? "";
}

/**
 * Включает подсказки на всей странице. Слушает документ, а не каждый узел:
 * кнопки появляются и исчезают (вход, список файлов), и подписка на узел
 * пережила бы не всякую перерисовку.
 */
export function attach(doc = document, view = window) {
  const hoverable = view.matchMedia("(hover: hover)");
  const panel = doc.createElement("div");
  panel.className = "tip";
  panel.setAttribute("role", "tooltip");
  panel.setAttribute("aria-hidden", "true");
  panel.hidden = true;
  doc.body.appendChild(panel);

  let shown = null;

  function hide() {
    shown = null;
    panel.hidden = true;
  }

  function show(node) {
    const text = claim(node);
    if (!text) return;
    panel.textContent = text;
    // Меряем невидимой: размеры нужны раньше, чем известно место.
    panel.style.visibility = "hidden";
    panel.hidden = false;
    const spot = place(node.getBoundingClientRect(), panel.getBoundingClientRect(), {
      width: doc.documentElement.clientWidth,
      height: doc.documentElement.clientHeight,
    });
    panel.style.left = `${Math.round(spot.left)}px`;
    panel.style.top = `${Math.round(spot.top)}px`;
    panel.dataset.side = spot.side;
    panel.style.visibility = "";
    shown = node;
  }

  doc.addEventListener("mouseover", (event) => {
    if (!hoverable.matches) return;
    const node = target(event.target);
    if (!node) {
      if (shown) hide();
      return;
    }
    if (node !== shown) show(node);
  });
  doc.addEventListener("mouseout", (event) => {
    if (shown && !shown.contains(event.relatedTarget)) hide();
  });
  doc.addEventListener("focusin", (event) => {
    if (!hoverable.matches) return;
    const node = target(event.target);
    if (node) show(node);
  });
  doc.addEventListener("focusout", hide);
  doc.addEventListener("keydown", (event) => {
    if (event.key === "Escape") hide();
  });
  // Прокрутка уводит элемент из-под панели; ловим с перехватом - прокручиваются
  // внутренние области, а не окно.
  doc.addEventListener("scroll", hide, true);
  view.addEventListener("resize", hide);
  hoverable.addEventListener("change", hide);

  return { hide, panel };
}

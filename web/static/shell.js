// Ширина оболочки: читатель задаёт её сам.
//
// # Границы
//
//   - **наибольшая - размер окна**: шире монитора оболочки не бывает;
//   - наименьшая - `MIN_WIDTH`: уже двух колонок кода раскладка всё равно
//     схлопывается в одну (порог 900 px), и сужать дальше нечего;
//   - на узком экране ручки нет вовсе: там область одна на экран.
//
// Значение помнится в `localStorage`: это настройка удобства читателя, и спрашивать её
// заново на каждом заходе незачем. В ссылку-снимок она не входит - ширина монитора у
// получателя своя.
//
// # Две ручки, одно правило
//
// Ручек здесь две, и предметы у них разные: `attach` задаёт ширину оболочки (сколько
// места занимает страница в окне), `attachPanes` - Доли двух областей внутри неё
// (сколько из них отдано исходнику, сколько выводу). Общего у них - границы, память и
// обязательная клавиатура, и потому они лежат рядом: заведи вторую отдельно, и правила
// разошлись бы на первой же правке.

/** Ключ хранилища ширины оболочки. */
export const KEY = "takt.shell";

/** Ключ хранилища долей областей по горизонтали. */
export const PANES_KEY = "takt.panes";

/** Ключ хранилища долей рядов области (редактор и диагностики). */
export const ROWS_KEY = "takt.rows";

/** Доля высоты, отданная легенде-полке, и доля ширины у легенды-колонки. */
export const LEGEND_ROWS_KEY = "takt.rows.legend";
export const LEGEND_COLS_KEY = "takt.panes.legend";

/** Какими стрелками двигается разделитель каждой оси. */
const ARROWS = { x: ["ArrowLeft", "ArrowRight"], y: ["ArrowUp", "ArrowDown"] };

/**
 * Наименьшая доля области: `0.2` - пятая часть.
 *
 * Ноль сюда не годится: область, сжатая в полосу, выглядит пропавшей, и
 * вернуть её мышью уже не за что - разделитель уезжает под край.
 */
export const MIN_RATIO = 0.2;

/** Умолчание колонок - равные половины: пока читатель не тронул ручку, ничего не меняется. */
export const HALF = 0.5;

/**
 * Умолчание рядов: семь десятых - редактору.
 *
 * Не половина: до появления разделителя список диагностик занимал 30 %
 * высоты, и умолчание обязано оставить вид прежним - читатель, ничего не
 * тронувший, не должен обнаружить, что редактор ужался вдвое.
 */
export const ROWS_DEFAULT = 0.7;

export const LEGEND_ROWS_DEFAULT = 0.7;
export const LEGEND_COLS_DEFAULT = 0.7;

/**
 * Приводит долю к допустимой и отбрасывает мусор.
 *
 * Отдельной функцией по той же причине, что и [`clamp`]: DOM в проверках
 * нет, а правило есть. `NaN` из испорченной записи хранилища обязан давать
 * умолчание, а не "ширину NaN" - вторая область тогда исчезает молча.
 */
export function clampRatio(ratio, fallback = HALF) {
  return clampWithin(ratio, fallback, MIN_RATIO, 1 - MIN_RATIO);
}

/**
 * Доля в названных границах.
 *
 * Границы приходят от того, кто их знает: у областей это общий предел "область,
 * сжатая в полосу, выглядит пропавшей", а у структуры проекта - ширина самого
 * длинного имени файла. Общий предел ей не годится: дерево из трёх коротких
 * имён держало бы пятую часть экрана, а дерево длинных имён не разворачивалось
 * бы на весь.
 */
export function clampWithin(ratio, fallback, min, max) {
  if (!Number.isFinite(ratio)) return fallback;
  if (max < min) return min;
  return Math.min(max, Math.max(min, ratio));
}

/**
 * Доля по месту указателя внутри рабочей области.
 *
 * @param {number} x координата указателя
 * @param {{left: number, width: number}} rect место рабочей области
 */
export function ratioAt(point, rect, axis = "x") {
  const size = axis === "y" ? rect?.height : rect?.width;
  const start = axis === "y" ? rect?.top : rect?.left;
  if (!size || size <= 0) return HALF;
  return clampRatio((point - start) / size);
}

/** Наименьшая ширина оболочки: уже неё две колонки кода не имеют смысла. */
export const MIN_WIDTH = 640;

/**
 * Приводит запрошенную ширину к допустимой.
 *
 * Отдельной функцией, потому что проверяется в `node`: DOM там нет, а
 * правило границ есть, и ошибка в нём делает оболочку либо неуправляемо узкой,
 * либо шире окна.
 */
export function clamp(width, windowWidth) {
  const most = Math.max(MIN_WIDTH, windowWidth);
  return Math.round(Math.min(most, Math.max(MIN_WIDTH, width)));
}

/** Читает запомненную ширину; `null` - её нет либо запись испорчена. */
export function stored(storage) {
  try {
    const value = Number(storage.getItem(KEY));
    return Number.isFinite(value) && value > 0 ? value : null;
  } catch {
    return null;
  }
}

/**
 * Заводит ручку ширины.
 *
 * @param {HTMLElement} grip элемент-разделитель
 * @param {Storage} storage хранилище настройки
 */
export function attach(grip, storage) {
  const root = grip.ownerDocument.documentElement;
  let width = stored(storage) ?? window.innerWidth;

  const apply = (next) => {
    width = clamp(next, window.innerWidth);
    root.style.setProperty("--shell-w", `${width}px`);
    grip.setAttribute("aria-valuenow", String(width));
    grip.setAttribute("aria-valuemin", String(MIN_WIDTH));
    grip.setAttribute("aria-valuemax", String(Math.max(MIN_WIDTH, window.innerWidth)));
  };

  const remember = () => {
    try {
      storage.setItem(KEY, String(width));
    } catch {
      // Приватный режим либо запрет сайту: ширина действует до перезагрузки.
    }
  };

  // Тяга мышью и пальцем - одним обработчиком: указатель у браузера один. Оболочка
  // стоит по центру, поэтому сдвиг края меняет ширину вдвое: считать её от одной
  // стороны - значит уводить содержимое вбок.
  grip.addEventListener("pointerdown", (event) => {
    event.preventDefault();
    grip.setPointerCapture(event.pointerId);
    const startX = event.clientX;
    const startWidth = width;
    const move = (moved) => apply(startWidth + (moved.clientX - startX) * 2);
    const stop = () => {
      grip.removeEventListener("pointermove", move);
      grip.removeEventListener("pointerup", stop);
      grip.removeEventListener("pointercancel", stop);
      remember();
    };
    grip.addEventListener("pointermove", move);
    grip.addEventListener("pointerup", stop);
    grip.addEventListener("pointercancel", stop);
  });

  // Клавиатура: разделитель без неё недоступен вовсе.
  grip.addEventListener("keydown", (event) => {
    const step = event.shiftKey ? 160 : 32;
    switch (event.key) {
      case "ArrowLeft": apply(width - step); remember(); break;
      case "ArrowRight": apply(width + step); remember(); break;
      case "Home": apply(MIN_WIDTH); remember(); break;
      case "End": apply(window.innerWidth); remember(); break;
      default: return;
    }
    event.preventDefault();
  });

  // Двойной щелчок возвращает во всю ширину: сузив оболочку случайно, вернуть её надо
  // одним движением.
  grip.addEventListener("dblclick", () => {
    apply(window.innerWidth);
    remember();
  });

  // Окно уменьшили - оболочка обязана поместиться; запомненное при этом не портится:
  // вернут окно, вернётся и ширина.
  window.addEventListener("resize", () => apply(width));

  apply(width);
}

/**
 * Заводит разделитель областей.
 *
 * Доли считаются от места рабочей области, а не от окна: оболочка стоит по
 * центру и бывает уже окна, и счёт от края уводил бы разделитель из-под
 * указателя тем сильнее, чем уже оболочка.
 *
 * @param {HTMLElement} split элемент-разделитель
 * @param {Storage} storage хранилище настройки
 */
export function attachPanes(split, storage) {
  attachDivider(split, {
    storage,
    key: PANES_KEY,
    axis: "x",
    // Доли считаются от рабочей области - вместилища разделителя.
    box: () => split.parentElement.getBoundingClientRect(),
    apply: (ratio, root) => {
      // Доли задаются обе: `fr` делит остаток, и оставь мы вторую единицей - области
      // перестали бы быть долями друг друга.
      root.style.setProperty("--panes-l", `${ratio}fr`);
      root.style.setProperty("--panes-r", `${1 - ratio}fr`);
    },
  });
}

/**
 * Заводит ряды области: разделитель между редактором и диагностиками.
 *
 * Та же ручка правил, что у колонок: разделитель - один вид контрола, и
 * второй набор границ, памяти и клавиатуры разошёлся бы с первым на первой же
 * правке. Разница только в оси и в том, что именно ставится в стилях.
 */
export function attachRows(split, storage, plan = {}) {
  // Доля считается от рабочей области целиком: ручка живёт в обёртке журнала, а
  // процент дорожки сетки - от высоты области. Мерка от обёртки гоняла бы долю по
  // кругу: обёртка растёт от доли, доля - от обёртки.
  const box = () => (split.closest(plan.within ?? ".work") ?? split.parentElement).getBoundingClientRect();
  attachDivider(split, {
    storage,
    key: ROWS_KEY,
    axis: "y",
    fallback: ROWS_DEFAULT,
    box,
    // Журнал диагностик ужимается до одной записи, а не до общей пятой части
    // высоты: читателю, занятому кодом, довольно видеть
    // последнюю строку - остальное скажет затенение. Сверху граница прежняя:
    // область кода, сжатая в полосу, выглядит пропавшей.
    bounds: (rect) => {
      const room = rect?.height;
      const least = plan.least?.() ?? 0;
      if (!room || room <= 0) return {};
      return { min: MIN_RATIO, max: Math.max(MIN_RATIO, 1 - least / room) };
    },
    // Доля - часть высоты, отданная верхней области (редактору); нижней достаётся
    // остаток, и он же задаёт высоту списка диагностик.
    apply: (ratio, root) => {
      root.style.setProperty("--rows-b", `${(1 - ratio) * 100}%`);
    },
  });
}

/**
 * Ручка разделителя: границы, память, клавиатура - на обе оси.
 *
 * @param {HTMLElement} split элемент-разделитель
 * @param {{storage: Storage, key: string, axis: "x"|"y", fallback?: number,
 *          box: () => DOMRect, apply: (ratio: number, root: HTMLElement) => void}} plan
 *        чем меряем, где помним, что ставим и к чему возвращаемся
 */
/** Разделитель легенды-полки: её высота под холстом. */
/**
 * Ручка ширины структуры проекта.
 *
 * Доля своя, а не общая с областями кода и вывода: дерево читают, а не
 * растягивают под содержимое, и делить его ширину с выводом значило бы менять
 * два размера одной ручкой.
 */
export function attachTree(split, storage, plan = {}) {
  return attachTreeDivider(split, storage, plan);
}

function attachTreeDivider(split, storage, plan) {
  // Сторона решает и ось, и то, какая величина ставится: слева и справа
  // структура делит ширину, сверху и снизу - высоту. Величины две, а не одна:
  // читатель, переставивший дерево сверху вниз, ждёт прежней высоты, а не
  // высоты, пересчитанной из ширины.
  const sideOf = plan.side ?? (() => "right");
  const vertical = () => sideOf() === "top" || sideOf() === "bottom";
  const first = () => sideOf() === "left" || sideOf() === "top";
  return attachDivider(split, {
    storage,
    key: TREE_KEY,
    axis: () => (vertical() ? "y" : "x"),
    fallback: TREE_DEFAULT,
    box: () => split.parentElement.getBoundingClientRect(),
    // Нижняя граница - размер, при котором видны имена файлов; верхней нет:
    // читатель вправе развернуть структуру во всю область. Доля считается от
    // начала области, поэтому у стороны "слева" и "сверху" она обратная.
    bounds: (rect) => {
      const room = vertical() ? rect?.height : rect?.width;
      const need = plan.least?.() ?? 0;
      // Область не разложена либо уже самых длинных имён: граница не спасёт
      // имена, а распахнула бы дерево во всю область.
      if (!room || room <= 0 || need >= room) return { min: 0, max: 1 };
      const share = Math.min(1, need / room);
      return first() ? { min: share, max: 1 } : { min: 0, max: 1 - share };
    },
    apply: (ratio, root) => {
      const share = first() ? ratio : 1 - ratio;
      root.style.setProperty(vertical() ? "--tree-h" : "--tree-w", `${share * 100}%`);
    },
  });
}

export function attachLegendRows(split, storage) {
  attachDivider(split, {
    storage,
    key: LEGEND_ROWS_KEY,
    axis: "y",
    fallback: LEGEND_ROWS_DEFAULT,
    box: () => split.parentElement.getBoundingClientRect(),
    apply: (ratio, root) => {
      root.style.setProperty("--legend-h", `${(1 - ratio) * 100}dvh`);
    },
  });
}

/** Разделитель легенды-колонки: её ширина справа от холста. */
export function attachLegendCols(split, storage) {
  attachDivider(split, {
    storage,
    key: LEGEND_COLS_KEY,
    axis: "x",
    fallback: LEGEND_COLS_DEFAULT,
    box: () => split.parentElement.getBoundingClientRect(),
    apply: (ratio, root) => {
      root.style.setProperty("--legend-w", `${(1 - ratio) * 100}%`);
    },
  });
}

/** Ключ доли структуры проекта в памяти читателя. */
const TREE_KEY = "takt.ui.tree";

/** Умолчание доли: структура занимает шестую часть рабочей области. */
const TREE_DEFAULT = 0.84;

function attachDivider(split, plan) {
  const root = split.ownerDocument.documentElement;
  // Ось спрашивается на каждое движение, а не запоминается: у структуры проекта
  // сторону выбирает читатель, и сверху она делит высоту, а слева - ширину.
  const axisOf = () => (typeof plan.axis === "function" ? plan.axis() : plan.axis);
  const fallback = plan.fallback ?? HALF;
  // Границы тоже спрашиваются: у структуры проекта нижняя граница - ширина
  // самого длинного имени, и она меняется вместе с составом проекта.
  const limits = () => {
    const own = plan.bounds?.(plan.box());
    return {
      min: Number.isFinite(own?.min) ? own.min : MIN_RATIO,
      max: Number.isFinite(own?.max) ? own.max : 1 - MIN_RATIO,
    };
  };
  // Выбор читателя и показанная доля - разные величины. Границы зависят от
  // содержимого и размера области, а при загрузке они ещё не устоялись: урежь
  // граница первого мгновения сам выбор - и после каждой перезагрузки область
  // оставалась бы такой, какой её сжала эта граница. Выбор меняют только руки
  // читателя; показ урезается границами на каждое переприменение.
  let wanted = panes(plan.storage, plan.key, fallback);
  let ratio = wanted;

  const show = () => {
    const { min, max } = limits();
    ratio = clampWithin(wanted, fallback, min, max);
    plan.apply(ratio, root);
    split.setAttribute("aria-orientation", axisOf() === "y" ? "horizontal" : "vertical");
    split.setAttribute("aria-valuenow", String(Math.round(ratio * 100)));
    split.setAttribute("aria-valuemin", String(Math.round(min * 100)));
    split.setAttribute("aria-valuemax", String(Math.round(max * 100)));
  };

  /** Движение читателя: выбор становится тем, что он видит. */
  const apply = (next) => {
    wanted = next;
    show();
    wanted = ratio;
  };

  const remember = () => {
    try {
      plan.storage.setItem(plan.key, String(wanted));
    } catch {
      // Приватный режим либо запрет сайту: доли действуют до перезагрузки.
    }
  };

  split.addEventListener("pointerdown", (event) => {
    event.preventDefault();
    split.setPointerCapture(event.pointerId);
    const move = (moved) => {
      const axis = axisOf();
      apply(ratioAt(axis === "y" ? moved.clientY : moved.clientX, plan.box(), axis));
    };
    const stop = () => {
      split.removeEventListener("pointermove", move);
      split.removeEventListener("pointerup", stop);
      split.removeEventListener("pointercancel", stop);
      remember();
    };
    split.addEventListener("pointermove", move);
    split.addEventListener("pointerup", stop);
    split.addEventListener("pointercancel", stop);
  });

  // Клавиатура обязательна - разделитель без неё недоступен вовсе. Стрелки берутся по
  // Оси таблицей: у горизонтального разделителя "влево" не значит ничего, а тернарник
  // из двух литералов сверка ключей словаря принимает за подписи (нашлось её же
  // прогоном).
  split.addEventListener("keydown", (event) => {
    const [less, more] = ARROWS[axisOf()];
    const { min, max } = limits();
    const step = event.shiftKey ? 0.1 : 0.02;
    switch (event.key) {
      case less: apply(ratio - step); remember(); break;
      case more: apply(ratio + step); remember(); break;
      case "Home": apply(min); remember(); break;
      case "End": apply(max); remember(); break;
      default: return;
    }
    event.preventDefault();
  });

  // Двойной щелчок возвращает умолчание: сдвинув разделитель случайно, вернуть его надо
  // одним движением - и вернуть именно к тому, что было.
  split.addEventListener("dblclick", () => {
    apply(fallback);
    remember();
  });

  show();
  // Границы сменились (дерево перерисовано, окно изменило размер) - показ
  // пересчитывается из выбора читателя, а не из значения, урезанного границей.
  return { refresh: show };
}

/**
 * Читает запомненную долю; умолчание - равные половины.
 *
 * Пустая запись отвечает умолчанием, а не разбором пустоты: `Number(null)`
 * даёт ноль, ноль - доля, и первый же заход на страницу схлопывал бы исходник
 * в пятую часть. Нашлось прогоном страницы 2026-09-04.
 */
export function panes(storage, key = PANES_KEY, fallback = HALF) {
  try {
    const raw = storage.getItem(key);
    if (raw === null || raw === undefined || raw === "") return fallback;
    return clampRatio(Number(raw), fallback);
  } catch {
    return fallback;
  }
}

/** Ключи хранилища переноса строк: у каждой области свой. */
/**
 * Ключ переноса длинных строк - один на страницу.
 *
 * Настройка общая для всех областей кода: кнопка переноса действует и на
 * исходник, и на вывод целей. Перенос есть свойство того, как человек читает
 * код, а не свойство отдельной панели.
 */
export const WRAP_KEY = "takt.wrap";

/**
 * Читает настройку переноса строк.
 *
 * Умолчание - нет переноса: код читают столбцом, и включённый по умолчанию
 * перенос менял бы вид всякой модели у всякого читателя.
 */
export function wrapped(storage, key) {
  try {
    return storage.getItem(key) === "1";
  } catch {
    return false;
  }
}

/**
 * Заводит переключатель переноса строк для одной области кода.
 *
 * @param {HTMLElement} button кнопка-переключатель
 * @param {HTMLElement} area область кода
 * @param {Storage} storage хранилище настройки
 * @param {string} key ключ хранилища этой области
 */
export function applyWrap(areas, on) {
  // Класс ставится области, а не строкам: строки перестраивает покраска (`paintCode`)
  // на каждую правку, и настройка исчезала бы с первым же нажатием клавиши.
  for (const box of [].concat(areas).filter(Boolean)) box.classList.toggle("wrap", on);
}

/**
 * Ставит перенос строк и запоминает выбор.
 *
 * Своей кнопки у переноса нет: это настройка чтения, и живёт она в окне
 * настроек рядом с языком - у полосы действий области предмет другой, там правят
 * открытый текст, а не то, как его читают.
 */
export function setWrap(areas, on, storage, key) {
  applyWrap(areas, on);
  try {
    // Не тернарник из двух литералов: сверка ключей словаря принимает такую форму за
    // подписи (второй случай за задачу - см. стрелки осей).
    storage.setItem(key, String(Number(on)));
  } catch {
    // Приватный режим: настройка действует до перезагрузки.
  }
}

/** Ключ хранилища кегля страницы. */
export const FONT_KEY = "takt.font";

/** Кегль страницы по умолчанию, px: тот же, что стоит в стилях. */
export const FONT_DEFAULT = 16;

/** Границы кегля, px. */
export const FONT_MIN = 10;
export const FONT_MAX = 28;

/**
 * Приводит кегль к допустимому.
 *
 * Границы названы обе: ниже нижней страница нечитаема, выше верхней в
 * области кода перестают помещаться даже короткие строки, и читатель, нажавший
 * "крупнее" двадцать раз, остаётся один на один с двумя словами на экран.
 */
export function clampFont(size) {
  if (!Number.isFinite(size)) return FONT_DEFAULT;
  return Math.min(FONT_MAX, Math.max(FONT_MIN, Math.round(size)));
}

/** Читает запомненный кегль; умолчание - [`FONT_DEFAULT`]. */
export function fontSize(storage) {
  try {
    const raw = storage.getItem(FONT_KEY);
    if (raw === null || raw === undefined || raw === "") return FONT_DEFAULT;
    return clampFont(Number(raw));
  } catch {
    return FONT_DEFAULT;
  }
}

/**
 * Заводит выбор кегля страницы: "мельче", "крупнее" и текущее число.
 *
 * Меняется корневой кегль, а не кегль области: все ступени шкалы заданы в
 * `rem`, поэтому страница растёт целиком и пропорции шкалы сохраняются. Свой
 * кегль "только для кода" развалил бы шкалу на два набора.
 *
 * @param {HTMLElement} less кнопка "мельче"
 * @param {HTMLElement} more кнопка "крупнее"
 * @param {HTMLElement} label узел с текущим числом
 * @param {Storage} storage хранилище настройки
 */
export function attachFontSize(less, more, label, storage) {
  const root = less.ownerDocument.documentElement;
  let size = fontSize(storage);

  const apply = () => {
    root.style.setProperty("--text-root", `${size}px`);
    label.textContent = String(size);
    // Кнопка у предела гасится: нажатие, которое ничего не меняет, читается как
    // поломка.
    less.disabled = size <= FONT_MIN;
    more.disabled = size >= FONT_MAX;
  };

  const step = (delta) => {
    size = clampFont(size + delta);
    apply();
    try {
      storage.setItem(FONT_KEY, String(size));
    } catch {
      // Приватный режим: кегль действует до перезагрузки.
    }
  };

  less.addEventListener("click", () => step(-1));
  more.addEventListener("click", () => step(1));
  apply();
}

/**
 * Простые настройки интерфейса, которые переживают перезагрузку.
 *
 * Ключи собраны В одном месте: настройка, чей ключ придуман по месту,
 * однажды разойдётся с тем, кто её читает, - и читатель получит умолчание там,
 * где сам выбирал (принятое решение 2026-09-05: настройки UI хранятся в
 * `localStorage`).
 *
 * Что сюда не входит: текст модели, сценарий и выбранная цель - они
 * принадлежат работе, а не интерфейсу, и живут в черновике (`draft.js`),
 * который ключуется проектом и файлом.
 */
export const UI_KEYS = {
  /** Раздел справки, на котором читатель остановился: якорь заголовка. */
  helpAt: "takt.ui.helpAt",
  /** Открытая вкладка области вывода: `output` либо `trace`. */
  tab: "takt.ui.tab",
  /** Бюджет прогона, тактов. */
  budget: "takt.ui.budget",
  /** Открытая панель правой области: `output` либо пусто. */
  panel: "takt.ui.panel",
  /** Какая запись показана в области диагностик: `diagnostics` либо `trace`. */
  diagTab: "takt.ui.diagTab",
  /** Видна ли область диагностик под кодом. */
  diagnostics: "takt.ui.diagnostics",
  /** Доля структуры проекта в рабочей области. */
  tree: "takt.ui.tree",
  /** Видна ли структура проекта. */
  treeShown: "takt.ui.treeShown",
  /** С какой стороны стоит структура проекта: `left`, `right`, `top`, `bottom`. */
  treeSide: "takt.ui.treeSide",
  /** Сколько записей держит журнал диагностик до ротации. */
  diagKeep: "takt.ui.diagKeep",
  /** Видны ли путь открытого файла и ревизия в шапке. */
  crumbs: "takt.ui.crumbs",
  /** Последний открытый проект: страница возвращается к нему при заходе. */
  project: "takt.ui.project",
};

/** Стороны, на которых может стоять структура проекта. */
export const TREE_SIDES = ["left", "right", "top", "bottom"];

/** Сторона по умолчанию: справа - там она и стояла до появления выбора. */
export const TREE_SIDE_DEFAULT = "right";

/**
 * Сколько записей журнал диагностик держит без ротации.
 *
 * Предел нужен потому, что журнал накапливает:
 * страница живёт часами, компиляция идёт на каждую правку, и список без предела
 * растёт, пока браузер не начнёт спотыкаться на его отрисовке.
 */
export const DIAG_KEEP_DEFAULT = 500;

/** Границы объёма журнала: меньше десятка бесполезно, больше десяти тысяч - тяжело. */
export const DIAG_KEEP_MIN = 10;
export const DIAG_KEEP_MAX = 10000;

/** Читает объём журнала из памяти читателя, приводя к границам. */
export function diagKeep(storage) {
  const raw = Number(setting(storage, UI_KEYS.diagKeep, String(DIAG_KEEP_DEFAULT)));
  if (!Number.isFinite(raw)) return DIAG_KEEP_DEFAULT;
  return Math.min(DIAG_KEEP_MAX, Math.max(DIAG_KEEP_MIN, Math.round(raw)));
}

/** Читает сторону структуры проекта; незнакомая запись - умолчание. */
export function treeSide(storage) {
  const raw = setting(storage, UI_KEYS.treeSide, TREE_SIDE_DEFAULT);
  return TREE_SIDES.includes(raw) ? raw : TREE_SIDE_DEFAULT;
}

/** Читает настройку; `fallback` - если её нет либо хранилище недоступно. */
export function setting(storage, key, fallback) {
  try {
    const raw = storage.getItem(key);
    return raw === null || raw === "" ? fallback : raw;
  } catch {
    return fallback;
  }
}

/** Запоминает настройку. Отказ хранилища не роняет страницу. */
export function remember(storage, key, value) {
  try {
    storage.setItem(key, String(value));
  } catch {
    // Приватный режим либо запрет сайту: настройка действует до перезагрузки.
  }
}

/**
 * Забывает настройку.
 *
 * Пустая запись не годится: `setting` отвечает на неё умолчанием, а забытое
 * значит "выбора нет" - у последнего открытого проекта это разные вещи.
 */
export function forget(storage, key) {
  try {
    storage.removeItem(key);
  } catch {
    // То же, что у записи: без хранилища настройка живёт до перезагрузки.
  }
}

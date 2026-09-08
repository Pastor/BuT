// Системные сообщения страницы: сбои инструмента, а не замечания к модели.
//
// # Что сюда попадает
//
// Отказ загрузки модуля, необработанное исключение и отклонённое обещание, сбой
// запроса к серверу - всё, о чём автору сказать нечего, кроме "инструмент не
// сработал". Диагностики компилятора и эталона сюда **не** попадают: у них своя
// область (список диагностик и трасса), они относятся к модели и живут ровно
// столько, сколько живёт ошибка в тексте.
//
// # Место полосы
//
// Сбой инструмента застаёт автора где угодно: он мог смотреть вывод цели, прогон
// или схему. Строка в одной из областей осталась бы незамеченной в двух других -
// а неработающий инструмент выглядит как работающий, пока о нём не сказано.
//
// # Узлы, а не разметка строкой
//
// Текст берётся у источника ошибки, а он бывает чужим (ответ сервера, сообщение
// среды). `innerHTML` здесь означал бы чужую разметку в странице автора, поэтому
// узлы строятся, а текст кладётся в `textContent`.

/**
 * Показывает системное сообщение.
 *
 * Повтор не плодится: одно и то же сообщение приходит очередями (обещание
 * отклоняется на каждый неудачный запрос), и стопка из десяти одинаковых полос
 * закрыла бы страницу целиком.
 *
 * @param {object} host узел-хозяин (`#alerts`)
 * @param {string} text текст сообщения
 * @param {string} dismiss подпись кнопки закрытия (из словаря)
 * @returns {object|null} узел сообщения; `null` - такое уже показано
 */
export function show(host, text, dismiss) {
  if (!host || !text) return null;
  for (const shown of host.children ?? []) {
    if (shown.dataset?.text === text) return null;
  }
  const box = host.ownerDocument.createElement("div");
  box.className = "alert";
  box.dataset.text = text;
  box.setAttribute("role", "alert");
  const message = host.ownerDocument.createElement("span");
  message.className = "alert-text";
  message.textContent = text;
  box.appendChild(message);
  const close = host.ownerDocument.createElement("button");
  close.className = "alert-close";
  close.type = "button";
  close.setAttribute("aria-label", dismiss);
  close.dataset.tip = dismiss;
  // Крестик нарисован рамками, а не набран символом: покрытие шрифта решает за
  // нас, и промах даёт системный глиф на чужой базовой линии (правило крошек).
  close.appendChild(host.ownerDocument.createElement("span"));
  close.addEventListener("click", () => box.remove());
  box.appendChild(close);
  host.appendChild(box);
  return box;
}

/** Снимает все показанные сообщения. */
export function clear(host) {
  host?.replaceChildren();
}

/**
 * Ставит перехват сбоев, о которых иначе не узнает никто.
 *
 * Оба события обязательны: `error` ловит исключение обработчика, а
 * `unhandledrejection` - отказ обещания, и второй случай у страницы, где всё
 * идёт через `await`, куда чаще первого.
 *
 * @param {object} target окно (`window`)
 * @param {(text: string) => void} say показать сообщение
 */
export function watch(target, say) {
  target.addEventListener("error", (event) => {
    say(textOf(event.error ?? event.message));
  });
  target.addEventListener("unhandledrejection", (event) => {
    say(textOf(event.reason));
  });
}

/** Текст ошибки: сообщение, если оно есть, иначе сама величина. */
export function textOf(error) {
  if (error === null || error === undefined) return "";
  if (typeof error === "string") return error;
  return String(error.message ?? error);
}

// Прогон модели в отдельном потоке.

import { Bridge } from "./bridge.js";

let bridge = null;
let session = null;
// Ключ открытой сессии: текст, сценарий и период такта. Шаг и прогон продолжают
// открытую сессию, пока эти трое не изменились; изменились - сессия открывается
// заново, и страница узнаёт об этом сообщением `opened`.
let sessionKey = null;
// Сделано тактов с открытия сессии: бюджет прогона считается от него.
let done = 0;
let stopped = false;

self.onmessage = async (event) => {
  const message = event.data ?? {};
  try {
    switch (message.type) {
      case "run":
        await run(message);
        break;
      case "step":
        await step(message);
        break;
      case "reset":
        // Сброс - не останов: сессия закрывается, и следующий пуск открывает её
        // заново, то есть автомат начинает с первого такта. Трассу сброс не трогает:
        // её показывает страница, и старый прогон рядом с новым - её решение.
        close_();
        post({ type: "reset" });
        break;
      case "stop":
        // Останов - не ошибка прогона: автор попросил, и прогон обязан прекратиться на
        // ближайшей границе порции.
        stopped = true;
        break;
      default:
        post({ type: "failed", key: "trace.unknownCommand", params: { type: message.type } });
    }
  } catch (error) {
    // Отказ среды исполнения - не строка словаря: он приходит от браузера.
    post({ type: "failed", message: String(error?.message ?? error) });
  }
};

/**
 * Открывает сессию прогона либо продолжает открытую.
 *
 * @returns {Promise<boolean>} сессия готова к тактам
 */
async function ensure({ wasmUrl, source, scenario, tickMs }) {
  if (!bridge) bridge = await Bridge.load(wasmUrl);
  const key = JSON.stringify([source, scenario ?? "", tickMs ?? 0]);
  if (session !== null && sessionKey === key) return true;
  close_();
  const opened = bridge.simOpen(source, scenario ?? "", tickMs ?? 0);
  if (!opened.ok) {
    post({ type: "failed", message: opened.error?.message, key: "trace.notOpened", error: opened.error });
    return false;
  }
  session = opened.id;
  sessionKey = key;
  done = 0;
  post({ type: "opened" });
  if (opened.warnings?.length) post({ type: "warnings", items: opened.warnings });
  return true;
}

/**
 * Делает такты открытой сессии и отправляет их странице.
 *
 * @returns {{ok: boolean, finished: boolean}} исход порции
 */
function advance(count) {
  const ticked = bridge.simTick(session, count);
  if (!ticked.ok) {
    post({ type: "failed", message: ticked.error?.message, key: "trace.tickFailed", error: ticked.error });
    close_();
    return { ok: false, finished: false };
  }
  done += ticked.lines.length;
  // Предупреждения такта и вывод модели отправляются до строк трассы своей
  // порции: место сообщения в потоке совпадает с тем, что даёт эталон в консоли.
  if (ticked.warnings?.length) post({ type: "warnings", items: ticked.warnings });
  if (ticked.output?.length) post({ type: "output", lines: ticked.output });
  post({ type: "lines", lines: ticked.lines, states: ticked.states ?? [], next: ticked.next ?? [], done });
  if (ticked.done) {
    post({ type: "finished", info: ticked.info, errors: ticked.errors, steps: done });
    close_();
    return { ok: true, finished: true };
  }
  return { ok: true, finished: false };
}

async function run(message) {
  stopped = false;
  if (!(await ensure(message))) return;
  // Бюджет всего прогона - свойство прогона, а не модели: автор просит столько тактов,
  // сколько готов ждать, и остановка называется словами. Такты, сделанные шагами до
  // прогона, в бюджет входят: сессия одна.
  const limit = message.budget ?? 10_000;
  const portion = Math.max(1, Math.min(message.chunk ?? 256, limit));
  while (!stopped && done < limit) {
    const outcome = advance(Math.min(portion, limit - done));
    if (!outcome.ok || outcome.finished) return;
    // Отдаём поток: пришедшая команда "стоп" разбирается между порциями.
    await Promise.resolve();
  }
  // Остановленная сессия остаётся открытой: автор вправе продолжить её шагами.
  post({
    type: "halted",
    key: stopped ? "trace.stoppedByAuthor" : "trace.budgetSpent",
    params: { steps: limit },
    steps: done,
  });
}

/** Один такт: сессия открывается по нужде и остаётся открытой для следующего шага. */
async function step(message) {
  stopped = false;
  if (!(await ensure(message))) return;
  const outcome = advance(1);
  if (outcome.ok && !outcome.finished) post({ type: "stepped", steps: done });
}

function close_() {
  if (session !== null) {
    bridge.simClose(session);
    session = null;
    sessionKey = null;
    done = 0;
  }
}

function post(message) {
  self.postMessage(message);
}

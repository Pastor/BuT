// Прогон модели в отдельном потоке.

import { Bridge } from "./bridge.js";

let bridge = null;
let session = null;
let stopped = false;

self.onmessage = async (event) => {
  const message = event.data ?? {};
  try {
    switch (message.type) {
      case "run":
        await run(message);
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

async function run({ wasmUrl, source, scenario, tickMs, budget, chunk }) {
  stopped = false;
  if (!bridge) bridge = await Bridge.load(wasmUrl);
  if (session !== null) {
    bridge.simClose(session);
    session = null;
  }

  const opened = bridge.simOpen(source, scenario ?? "", tickMs ?? 0);
  if (!opened.ok) {
    post({ type: "failed", message: opened.error?.message, key: "trace.notOpened", error: opened.error });
    return;
  }
  session = opened.id;
  if (opened.warnings?.length) post({ type: "warnings", lines: opened.warnings });

  // Бюджет всего прогона - свойство прогона, а не модели: автор просит столько тактов,
  // сколько готов ждать, и остановка называется словами.
  const limit = budget ?? 10_000;
  const portion = Math.max(1, Math.min(chunk ?? 256, limit));
  let done = 0;
  while (!stopped && done < limit) {
    const ticked = bridge.simTick(session, Math.min(portion, limit - done));
    if (!ticked.ok) {
      post({ type: "failed", message: ticked.error?.message, key: "trace.tickFailed", error: ticked.error });
      close_();
      return;
    }
    done += ticked.lines.length;
    post({ type: "lines", lines: ticked.lines, done });
    if (ticked.done) {
      post({ type: "finished", info: ticked.info, errors: ticked.errors, steps: done });
      close_();
      return;
    }
    // Отдаём поток: пришедшая команда "стоп" разбирается между порциями.
    await Promise.resolve();
  }
  post({
    type: "halted",
    key: stopped ? "trace.stoppedByAuthor" : "trace.budgetSpent",
    params: { steps: limit },
    steps: done,
  });
  close_();
}

function close_() {
  if (session !== null) {
    bridge.simClose(session);
    session = null;
  }
}

function post(message) {
  self.postMessage(message);
}

// Проверка тождественности модуля WebAssembly.
//
// # Что доказывает
//
// Модуль в браузере обязан отвечать то же, что инструменты на машине:
//
// 1. компиляция - файлы каждой цели байт В байт равны файлам `taktc`,
//      а отказ равен по коду и по позиции;
// 2. прогон - трасса равна выводу `takt-sim` строка в строку;
// 3. редакторский слой - на корпусе матрицы операция отвечает,
//      в том числе на недописанном файле;
// 4. экспорт - картинки и видео проекта байт в байт равны выводу
//      `takt-sim export`.
//
// Проверка сверяет вывод, а не устройство. Модуль зовёт те же функции
// библиотек, но одного этого мало: между библиотекой и страницей лежит мост
// (JSON, буфер, UTF-8), и потерять в нём хвост файла или строку трассы можно
// молча - вывод останется валидным и будет другим.
//
// Запуск: node scripts/check-wasm-identity.mjs <модуль.wasm> <taktc> <takt-sim> [модуль-экспорта.wasm]
//
// Экспорт сверяется по модулю экспорта: картинки рисует он, а не ядро. Без
// четвёртого аргумента путь берётся рядом с ядром.

import { readFile, writeFile, mkdtemp, readdir, rm } from "node:fs/promises";
import { requireInput } from "./gatelib.mjs";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { tmpdir } from "node:os";
import { join, basename } from "node:path";

const run = promisify(execFile);

const [wasmPath, taktcPath, taktSimPath, exportArg] = process.argv.slice(2);
const exportPath = exportArg ?? wasmPath?.replace(/takt_wasm\.wasm$/, "takt_wasm_export.wasm");
if (!wasmPath || !taktcPath || !taktSimPath) {
  console.error("Использование: node check-wasm-identity.mjs <модуль.wasm> <taktc> <takt-sim>");
  process.exit(2);
}

const TARGETS = ["c", "c-hal", "st", "st-at", "rust", "sv", "sv-mmio"];

const decoder = new TextDecoder();
const encoder = new TextEncoder();
let wasm;
/** Модуль экспорта: его операцию `takt_export` сверяет четвёртая часть. */
let exporter;

/** Загружает модуль и возвращает его экспорты. */
async function loadModule(path) {
  const bytes = await readFile(path);
  const { instance } = await WebAssembly.instantiate(bytes, {});
  return instance.exports;
}

/**
 * Зовёт операцию модуля: запрос JSON в буфер, ответ JSON из него же.
 *
 * Адрес буфера перечитывается после вызова: модуль растит его под ответ, и
 * старый указатель после роста ведёт в чужую память.
 */
function call(operation, request) {
  return callOn(wasm, operation, request);
}

/** То же, что [`call`], у названного модуля: буфер у каждого модуля свой. */
function callOn(module, operation, request) {
  const bytes = encoder.encode(JSON.stringify(request));
  module.takt_io_reserve(bytes.length);
  new Uint8Array(module.memory.buffer, module.takt_io_ptr(), bytes.length).set(bytes);
  const length = operation(bytes.length);
  const text = decoder.decode(new Uint8Array(module.memory.buffer, module.takt_io_ptr(), length));
  return JSON.parse(text);
}

const failures = [];
function fail(what, detail) {
  failures.push(`${what}: ${detail}`);
}

/** Компиляция цели инструментом: файлы вывода и диагностика отказа. */
async function compileWithTaktc(target, file, outputDir) {
  try {
    const { stderr } = await run(taktcPath, ["compile", "-t", target, file, "-o", outputDir]);
    const names = (await readdir(outputDir)).sort();
    const files = {};
    for (const name of names) {
      files[name] = await readFile(join(outputDir, name), "utf8");
    }
    return { ok: true, files, stderr };
  } catch (error) {
    // Отказ: `taktc` печатает диагностику в stderr - сверяем её код и позицию.
    return { ok: false, stderr: error.stderr ?? "" };
  }
}

/**
 * Строка отказа в выводе `taktc`.
 *
 * Не первая строка stderr: перед отказом печатаются предупреждения, и
 * первый же прогон проверки поймал на этом сам проверка - он сверял код `SE-036`
 * (неиспользуемая переменная) с кодом отказа модуля.
 */
function errorLine(text) {
  return text.split("\n").find((line) => line.includes("Ошибка")) ?? "";
}

/** Код диагностики из строки вывода `taktc` (`... [SE-034] ...`). */
function codeOf(text) {
  const match = /\[([A-Z]{2,4}-\d{3})\]/.exec(errorLine(text));
  return match ? match[1] : null;
}

/** Позиция `файл:строка:колонка` из строки вывода `taktc`. */
function positionOf(text) {
  const match = /:(\d+):(\d+):/.exec(errorLine(text));
  return match ? { line: Number(match[1]), column: Number(match[2]) } : null;
}

/**
 * Опирается ли пример на `import`.
 *
 * Такой пример из сверки исключается, но не пропускается молча: в браузере
 * файловой системы нет, и подключение файла - названная граница (
 * фичи). Проверка требует, чтобы модуль отвечал отказом с диагностикой, а не
 * рапортовал об успехе и не падал.
 */
function usesImport(source) {
  return /^\s*import\s/m.test(source);
}

/** Сверяет компиляцию одного примера одной целью. */
async function checkCompile(target, file, source, workDir) {
  const outputDir = join(workDir, `${target}-${basename(file, ".takt")}`);
  await rm(outputDir, { recursive: true, force: true });
  const expected = await compileWithTaktc(target, file, outputDir);
  const actual = call(wasm.takt_compile, {
    target,
    args: basename(file),
    source,
  });

  const what = `${target} / ${basename(file)}`;

  if (usesImport(source)) {
    // Граница A7: модель с `import` в браузере не собирается. Проверяется не
    // совпадение с `taktc`, а то, что отказ назван.
    if (actual.ok) {
      fail(what, "модель с `import` принята, хотя файловой системы в модуле нет");
    } else if (!actual.error?.message) {
      fail(what, "отказ на `import` без сообщения");
    }
    return;
  }

  if (expected.ok !== actual.ok) {
    fail(what, `taktc ${expected.ok ? "принял" : "отверг"}, модуль ${actual.ok ? "принял" : "отверг"}: ${actual.error?.message ?? ""}`);
    return;
  }

  if (!expected.ok) {
    // Отказ обязан совпасть по коду и позиции: разный код означает, что в
    // браузере автор увидит не ту ошибку, а разная позиция - не то место.
    const expectedCode = codeOf(expected.stderr);
    if (expectedCode && expectedCode !== actual.error?.code) {
      fail(what, `код отказа: taktc ${expectedCode}, модуль ${actual.error?.code}`);
    }
    const expectedPosition = positionOf(expected.stderr);
    if (expectedPosition && actual.error?.line != null) {
      if (expectedPosition.line !== actual.error.line || expectedPosition.column !== actual.error.column) {
        fail(
          what,
          `позиция отказа: taktc ${expectedPosition.line}:${expectedPosition.column}, ` +
            `модуль ${actual.error.line}:${actual.error.column}`
        );
      }
    }
    return;
  }

  const produced = new Map((actual.files ?? []).map((f) => [f.name, f.text]));
  const expectedNames = Object.keys(expected.files).sort();
  const producedNames = [...produced.keys()].sort();
  if (expectedNames.join(",") !== producedNames.join(",")) {
    fail(what, `состав файлов: taktc [${expectedNames}], модуль [${producedNames}]`);
    return;
  }
  for (const name of expectedNames) {
    if (expected.files[name] !== produced.get(name)) {
      const a = expected.files[name];
      const b = produced.get(name);
      let at = 0;
      while (at < a.length && at < b.length && a[at] === b[at]) at += 1;
      fail(what, `файл ${name} расходится на байте ${at}: taktc «${a.slice(at, at + 40)}», модуль «${b.slice(at, at + 40)}»`);
    }
  }
}

/** Строки трассы из вывода `takt-sim` (сводка исхода в них не входит). */
function traceLines(stdout) {
  return stdout.split("\n").filter((line) => line.startsWith("Шаг "));
}

/** Сверяет трассу одного сценария. */
async function checkTrace(modelFile, scenarioFile, source) {
  const scenario = await readFile(scenarioFile, "utf8");
  const steps = JSON.parse(scenario).length;
  const what = `прогон / ${basename(scenarioFile)}`;

  let expected;
  try {
    const { stdout } = await run(taktSimPath, [modelFile, "-s", scenarioFile]);
    expected = traceLines(stdout);
  } catch (error) {
    fail(what, `takt-sim отказал: ${(error.stderr ?? "").split("\n")[0]}`);
    return;
  }

  const opened = call(wasm.takt_sim_open, { source, scenario, tick_ms: 0 });
  if (!opened.ok) {
    fail(what, `модуль не открыл прогон: ${opened.error?.message}`);
    return;
  }
  const lines = [];
  let guard = 0;
  for (;;) {
    const ticked = call(wasm.takt_sim_tick, { id: opened.id, budget: 64 });
    if (!ticked.ok) {
      fail(what, `модуль отказал на такте: ${ticked.error?.message}`);
      call(wasm.takt_sim_close, { id: opened.id });
      return;
    }
    lines.push(...ticked.lines);
    if (ticked.done) break;
    guard += 1;
    if (lines.length >= steps || guard > 64) break;
  }
  call(wasm.takt_sim_close, { id: opened.id });

  const produced = lines.slice(0, expected.length);
  if (produced.length !== expected.length) {
    fail(what, `тактов: takt-sim ${expected.length}, модуль ${produced.length}`);
    return;
  }
  for (let i = 0; i < expected.length; i += 1) {
    if (expected[i] !== produced[i]) {
      fail(what, `шаг ${i + 1}:\n    takt-sim: ${expected[i]}\n    модуль:   ${produced[i]}`);
      return;
    }
  }
}

/**
 * Сверяет экспорт проекта: файлы модуля байт в байт равны файлам `takt-sim export`.
 *
 * @returns {Promise<number>} сколько файлов сверено
 */
async function checkExport(projectDir, workDir) {
  const manifest = JSON.parse(await readFile(join(projectDir, "takt-project.json"), "utf8"));
  const files = {};
  for (const file of manifest.files) files[file.name] = await readFile(join(projectDir, file.name), "utf8");
  const sets = [
    { args: ["--format", "svg", "--format", "png"], request: { formats: ["svg", "png"] } },
    {
      args: ["--view", "run", "--format", "svg", "--format", "png", "--background", "none", "--legend", "off"],
      request: { view: "run", formats: ["svg", "png"], background: "none", legend: false },
    },
    { args: ["--format", "gif", "--format", "mp4"], request: { formats: ["gif", "mp4"] } },
  ];
  let compared = 0;
  for (const [i, set] of sets.entries()) {
    const what = `экспорт / ${set.args.join(" ")}`;
    const out = join(workDir, `export-${i}`);
    try {
      await run(taktSimPath, ["export", projectDir, "-o", out, ...set.args]);
    } catch (error) {
      fail(what, `takt-sim отказал: ${(error.stderr ?? "").split("\n")[0]}`);
      continue;
    }
    const reply = callOn(exporter, exporter.takt_export, {
      files,
      main_file: manifest.main_file ?? null,
      main_scenario: manifest.main_scenario ?? null,
      ...set.request,
    });
    if (!reply.ok) {
      fail(what, `модуль отказал: ${reply.error?.message}`);
      continue;
    }
    const written = (await readdir(out)).sort();
    const produced = reply.files.map((file) => file.name).sort();
    if (JSON.stringify(written) !== JSON.stringify(produced)) {
      fail(what, `файлы: takt-sim ${written.join(", ")}; модуль ${produced.join(", ")}`);
      continue;
    }
    for (const file of reply.files) {
      const expected = await readFile(join(out, file.name));
      if (!expected.equals(Buffer.from(file.data, "base64"))) fail(what, `${file.name} разошёлся байтами`);
      compared += 1;
    }
  }
  return compared;
}

/** Проверяет, что редакторская операция отвечает на любом входе корпуса. */
function checkEditorAnswers(name, source) {
  const operations = [
    ["diagnostics", () => call(wasm.takt_diagnostics, { source })],
    ["tokens", () => call(wasm.takt_tokens, { source })],
    ["symbols", () => call(wasm.takt_symbols, { source })],
    ["completion", () => call(wasm.takt_completion, { source })],
    ["hover", () => call(wasm.takt_hover, { source, line: 0, character: 0 })],
    ["goto", () => call(wasm.takt_goto, { source, line: 0, character: 0 })],
    ["references", () => call(wasm.takt_references, { source, line: 0, character: 0 })],
    ["format", () => call(wasm.takt_format, { source })],
  ];
  for (const [operation, invoke] of operations) {
    let reply;
    try {
      reply = invoke();
    } catch (error) {
      fail(`редактор / ${name}`, `${operation} не ответила: ${error}`);
      continue;
    }
    if (typeof reply.ok !== "boolean") {
      fail(`редактор / ${name}`, `${operation} вернула ответ без поля ok`);
    }
  }
}

async function main() {
  wasm = await loadModule(wasmPath);
  exporter = await loadModule(exportPath);
  const workDir = await mkdtemp(join(tmpdir(), "takt-wasm-identity-"));

  // 1. Компиляция: корпус `examples/` x семь целей.
  const examples = (await readdir("examples"))
    .filter((name) => name.endsWith(".takt"))
    .sort();
  requireInput("примеры корпуса", examples.length, 1, "examples/");
  let compiled = 0;
  for (const name of examples) {
    const file = join("examples", name);
    const source = await readFile(file, "utf8");
    for (const target of TARGETS) {
      await checkCompile(target, file, source, workDir);
      compiled += 1;
    }
  }

  // 2. Прогон: сценарии `examples/simulations/`.
  const scenarios = (await readdir("examples/simulations"))
    .filter((name) => name.endsWith(".json"))
    .sort();
  requireInput("сценарии корпуса", scenarios.length, 1, "examples/simulations/");
  let traced = 0;
  for (const name of scenarios) {
    // Модель сценария называет правило принадлежности крейта проекта - через
    // `takt-sim project --owner`: своей копии правила у проверки нет.
    const models = examples.map((model) => join("examples", model));
    let modelFile = null;
    try {
      const { stdout } = await run(taktSimPath, ["project", "--owner", name, ...models]);
      modelFile = stdout.trim() || null;
    } catch {
      // Ничей сценарий: код 1 и пустая строка.
    }
    if (!modelFile) continue;
    const source = await readFile(modelFile, "utf8");
    await checkTrace(modelFile, join("examples/simulations", name), source);
    traced += 1;
  }

  // 3. Редакторский слой на корпусе матрицы, если он выгружен.
  let answered = 0;
  try {
    const dirs = await readdir("target/matrix-corpus", { withFileTypes: true });
    for (const dir of dirs) {
      if (!dir.isDirectory()) continue;
      const file = join("target/matrix-corpus", dir.name, "probe.takt");
      let source;
      try {
        source = await readFile(file, "utf8");
      } catch {
        continue;
      }
      checkEditorAnswers(dir.name, source);
      // Недописанный файл: редактор видит такой чаще всего.
      checkEditorAnswers(`${dir.name} (обрезан)`, source.slice(0, Math.floor(source.length / 2)));
      answered += 2;
    }
  } catch {
    // Корпуса нет - набор матрицы не выгружался; это не отказ проверки.
  }

  // 4. Экспорт: проект-фикстура экспорта в трёх наборах форматов.
  const exported = await checkExport("takt-sim/tests/data/export/project", workDir);

  await rm(workDir, { recursive: true, force: true });

  const note = requireInput("компиляции корпуса под целями", compiled, 1, "examples/");
  console.log(
    `  Тождественность модуля: ${note}, трасс ${traced}, ` +
      `редакторских входов ${answered}, файлов экспорта ${exported}.`
  );
  if (failures.length > 0) {
    console.error(`  РАСХОЖДЕНИЙ: ${failures.length}`);
    for (const failure of failures.slice(0, 20)) {
      console.error(`    ${failure}`);
    }
    if (failures.length > 20) {
      console.error(`    … и ещё ${failures.length - 20}`);
    }
    process.exit(1);
  }
}

await main();

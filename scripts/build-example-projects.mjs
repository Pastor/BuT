// Собирает модели из `examples/` в архивы проектов онлайн-редактора.
//
// # Что кладётся в архив
//
// Каждая модель верхнего уровня `examples/*.takt` становится проектом своего имени:
//
// - сама модель - активный файл проекта;
// - подключаемый код - файлы, которые модель импортирует (рекурсивно), из
//   `examples/` и `examples/include/`: без них проект на странице не соберётся;
// - описание - `<имя>.md` из ведущего комментария модели: страница показывает
//   пояснение рядом с моделью, а автор модели уже написал его в шапке файла;
// - схема - `<имя>.takt-ui`, раскладка состояний по ярусам: ровно та, что страница
//   строит сама при первом показе схемы, - той же функцией (`autoPlace`);
// - сценарии прогона - `examples/simulations/<имя>.json` и `<имя>_*.json`: по
//   правилу имени они принадлежат модели и выбираются в окне сценариев.
//
// Библиотека (файл без состояний) своим проектом не становится - она едет
// подключаемым кодом у тех, кто её импортирует: открыть её на странице нечем.
//
// # Почему node, а не свой разбор
//
// Граф модели отвечает тот же модуль WebAssembly, что стоит на странице, а
// раскладку строят те же модули страницы. Своего разбора языка здесь нет: второй
// носитель разошёлся бы со страницей молча, и схема архива отличалась бы от той,
// что автор увидит, открыв проект.
//
// # Куда
//
// `.temporary/example-projects/` - каталог в `.gitignore`: архивы воспроизводимы и
// в репозитории не хранятся. Формат - тот же, что у выгрузки сервиса (версия 3):
// `takt-project.json` и исходники в `src/`.
//
// Запуск: `node scripts/build-example-projects.mjs [модуль.wasm]`. Без аргумента
// берётся модуль из `web/dist` (его собирает `scripts/build-web.sh`) либо из
// переменной `TAKT_WASM`.

import { existsSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { crc32, deflateRawSync, inflateRawSync } from "node:zlib";

import { Bridge } from "../web/static/bridge.js";
import * as layout from "../web/static/layout.js";
import * as geo from "../web/static/scheme-geometry.js";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const EXAMPLES = join(ROOT, "examples");
const INCLUDE = join(EXAMPLES, "include");
const SIMULATIONS = join(EXAMPLES, "simulations");
const OUT = join(ROOT, ".temporary", "example-projects");

/** Версия формата архива сервиса (`web/server/src/archive.rs::FORMAT`). */
const FORMAT = 3;

/** Пределы сервиса (`web/server/src/limits.rs`): архив сверх них сервис отвергнет. */
const FILE_BYTES = 64 * 1024;
const PROJECT_BYTES = 512 * 1024;
const FILES_PER_PROJECT = 32;

/** Имя файла проекта: латиница, цифры, `_`, `-` - то же правило, что у сервиса. */
const STEM = /^[A-Za-z0-9_-]+$/;

main().catch((error) => {
  console.error(`  ОШИБКА: ${error?.message ?? error}`);
  process.exit(1);
});

async function main() {
  const wasmPath = findWasm();
  const bridge = await loadBridge(wasmPath);
  const version = versions();

  rmSync(OUT, { recursive: true, force: true });
  mkdirSync(OUT, { recursive: true });

  const models = readdirSync(EXAMPLES)
    .filter((name) => name.endsWith(".takt"))
    .sort();
  // Имена всех моделей нужны правилу принадлежности сценария: одно имя бывает
  // префиксом другого (`elevator` и `elevator_mini`).
  const stems = models.map((name) => basename(name, ".takt"));
  let built = 0;
  const skipped = [];
  for (const file of models) {
    const result = buildProject(bridge, file, version, stems);
    if (result.skipped) {
      skipped.push(`${file}: ${result.skipped}`);
      continue;
    }
    const target = join(OUT, `${result.name}.zip`);
    writeFileSync(target, zip(result.entries));
    verify(target, result);
    built += 1;
    console.log(`  ${result.name}.zip - ${result.files.map((f) => f.name).join(", ")}`);
  }
  for (const line of skipped) console.log(`  пропуск ${line}`);
  // Нижняя граница выборки: ноль собранных при непустом каталоге - сломанный
  // скрипт, а не пустой результат, и молчать о нём нельзя.
  if (built === 0) throw new Error(`не собрано ни одного проекта из ${models.length} моделей`);
  console.log(`Проекты примеров: разобрано ${models.length} — модели examples/, собрано ${built}, в ${OUT}`);
}

/** Модуль WebAssembly: аргумент, переменная, либо собранный `web/dist`. */
function findWasm() {
  const given = process.argv[2] ?? process.env.TAKT_WASM;
  if (given) return given;
  const index = join(ROOT, "web", "dist", "wasm", "index.json");
  if (existsSync(index)) {
    const latest = JSON.parse(readFileSync(index, "utf8"));
    const version = latest.latest ?? latest.version;
    const candidate = join(ROOT, "web", "dist", "wasm", String(version), "takt.wasm");
    if (existsSync(candidate)) return candidate;
  }
  throw new Error("модуль не найден: соберите страницу (scripts/build-web.sh) либо передайте путь к takt.wasm");
}

async function loadBridge(path) {
  const { instance } = await WebAssembly.instantiate(readFileSync(path), {});
  return new Bridge(instance.exports);
}

/** Версии модуля и языка - из тех же источников, что у выкладки. */
function versions() {
  const cargo = readFileSync(join(ROOT, "takt-lang", "Cargo.toml"), "utf8");
  const lib = readFileSync(join(ROOT, "takt-lang", "src", "version.rs"), "utf8");
  const module = /^version\s*=\s*"([^"]+)"/m.exec(cargo)?.[1];
  const language = /LANGUAGE_VERSION: &str = "([^"]+)"/.exec(lib)?.[1];
  if (!module || !language) throw new Error("версии модуля или языка не прочитаны");
  return { module, language };
}

/**
 * Собирает состав одного проекта.
 *
 * @returns {{name, files, entries} | {skipped: string}}
 */
function buildProject(bridge, file, version, stems) {
  const stem = basename(file, ".takt");
  if (!STEM.test(stem)) return { skipped: "имя модели негодно для имени файла проекта" };
  const source = readFileSync(join(EXAMPLES, file), "utf8");

  const graph = bridge.graph(source);
  const sheets = graph.ok ? graph.sheets ?? [] : [];
  const states = sheets.reduce((count, sheet) => count + (sheet.nodes?.length ?? 0), 0);
  if (states === 0) return { skipped: "библиотека без состояний - едет подключаемым кодом у импортёров" };

  const files = [{ name: file, kind: "takt", text: source }];
  for (const lib of imports(source, new Set([file]))) {
    files.push({ name: lib.name, kind: "takt", text: lib.text });
  }
  const description = describe(stem, source);
  if (description) files.push({ name: `${stem}.md`, kind: "markdown", text: description });
  files.push({ name: `${stem}${layout.EXTENSION}`, kind: "layout", text: scheme(sheets) });
  const scenarios = scenariosOf(stem, stems);
  for (const scenario of scenarios) files.push(scenario);

  const problem = limits(files);
  if (problem) return { skipped: problem };

  const manifest = {
    format: FORMAT,
    name: stem,
    description: firstLine(source) ?? "",
    takt_lang: version.module,
    language_version: version.language,
    main_file: file,
    main_scenario: scenarios[0]?.name ?? null,
    files: files.map((f) => ({ name: f.name, kind: f.kind })),
    // Метка выгрузки нулевая намеренно: архив воспроизводим, и два прогона
    // скрипта обязаны давать одинаковые байты.
    exported_at: 0,
    generated_target: null,
    build_target: "c",
    build_args: "",
  };
  const entries = [
    { name: "takt-project.json", text: `${JSON.stringify(manifest, null, 2)}\n` },
    ...files.map((f) => ({ name: `src/${f.name}`, text: f.text })),
  ];
  return { name: stem, files, entries, manifest };
}

/**
 * Подключаемые файлы модели - рекурсивно.
 *
 * Ищутся там же, где их ищет компилятор для примеров: рядом с моделью и в
 * `examples/include`. Не найденный файл - не повод молчать: проект без него на
 * странице не соберётся, и сказать об этом надо сейчас.
 */
function imports(source, seen) {
  const found = [];
  for (const match of source.matchAll(/^\s*import\b[^"\n]*"([^"]+)"/gm)) {
    const name = match[1];
    if (seen.has(name)) continue;
    seen.add(name);
    const path = [join(EXAMPLES, name), join(INCLUDE, name)].find((p) => existsSync(p));
    if (!path) throw new Error(`подключаемый файл '${name}' не найден ни в examples/, ни в examples/include/`);
    const text = readFileSync(path, "utf8");
    found.push({ name: basename(name), text });
    found.push(...imports(text, seen));
  }
  return found;
}

/** Описание модели из ведущего комментария: заголовок - первая строка. */
function describe(stem, source) {
  const lines = [];
  for (const line of source.split("\n")) {
    const comment = /^\s*\/\/\s?(.*)$/.exec(line);
    if (!comment) {
      if (line.trim() === "" && lines.length === 0) continue;
      break;
    }
    lines.push(comment[1].replace(/\s+$/, ""));
  }
  while (lines.length && lines[lines.length - 1] === "") lines.pop();
  if (lines.length === 0) return null;
  const [title, ...rest] = lines;
  const body = rest.join("\n").replace(/^\n+/, "");
  return `# ${stem}\n\n${title}\n${body ? `\n${body}\n` : ""}`;
}

/** Первая строка ведущего комментария - краткое описание проекта. */
function firstLine(source) {
  const match = /^\s*\/\/\s?(.+)$/m.exec(source);
  return match ? match[1].trim().slice(0, 200) : null;
}

/** Схема: раскладка каждого листа по ярусам той же функцией, что у страницы. */
function scheme(sheets) {
  const result = layout.empty();
  for (const sheet of sheets) {
    const placed = geo.autoPlace(sheet.nodes);
    for (const node of sheet.nodes) {
      const at = placed[node.name];
      if (at) layout.place(result, sheet.path, node.name, at.x, at.y);
    }
  }
  return layout.canonical(result);
}

/**
 * Сценарии модели по правилу имени: `<имя>.json` и `<имя>_*.json`.
 *
 * Сценарий достаётся модели с **самым длинным** подходящим именем: одно имя бывает
 * префиксом другого, и `elevator_mini_floor2.json` принадлежит `elevator_mini`, а
 * не `elevator` - иначе проект лифта получил бы сценарий чужой модели, чьих портов
 * у него нет.
 */
function scenariosOf(stem, stems) {
  if (!existsSync(SIMULATIONS)) return [];
  const fits = (name, candidate) =>
    name === `${candidate}.json` || (name.startsWith(`${candidate}_`) && name.endsWith(".json"));
  return readdirSync(SIMULATIONS)
    .filter((name) => fits(name, stem))
    .filter((name) => !stems.some((other) => other.length > stem.length && fits(name, other)))
    .filter((name) => STEM.test(basename(name, ".json")))
    .sort()
    .map((name) => ({ name, kind: "scenario", text: readFileSync(join(SIMULATIONS, name), "utf8") }));
}

/** Пределы сервиса: архив сверх них сервис отвергнет, и собирать его незачем. */
function limits(files) {
  if (files.length > FILES_PER_PROJECT) return `файлов ${files.length} при пределе ${FILES_PER_PROJECT}`;
  let total = 0;
  for (const file of files) {
    const size = Buffer.byteLength(file.text, "utf8");
    if (size > FILE_BYTES) return `файл ${file.name} - ${size} байт при пределе ${FILE_BYTES}`;
    total += size;
  }
  if (total > PROJECT_BYTES) return `проект ${total} байт при пределе ${PROJECT_BYTES}`;
  return null;
}

// ── Zip без зависимостей ────────────────────────────────────────────────────
// Формат: локальные заголовки, центральный каталог, запись конца каталога.
// Сжатие - deflate (метод 8); время файлов нулевое (1980-01-01), чтобы архив был
// воспроизводим байт в байт.

function zip(entries) {
  const locals = [];
  const central = [];
  let offset = 0;
  for (const entry of entries) {
    const name = Buffer.from(entry.name, "utf8");
    const data = Buffer.from(entry.text, "utf8");
    const packed = deflateRawSync(data);
    const sum = crc32(data) >>> 0;
    const local = Buffer.alloc(30);
    local.writeUInt32LE(0x04034b50, 0);
    local.writeUInt16LE(20, 4); // версия для распаковки
    local.writeUInt16LE(0x0800, 6); // имена в UTF-8
    local.writeUInt16LE(8, 8); // deflate
    local.writeUInt16LE(0, 10); // время
    local.writeUInt16LE(0x21, 12); // дата 1980-01-01
    local.writeUInt32LE(sum, 14);
    local.writeUInt32LE(packed.length, 18);
    local.writeUInt32LE(data.length, 22);
    local.writeUInt16LE(name.length, 26);
    local.writeUInt16LE(0, 28);
    locals.push(local, name, packed);

    const head = Buffer.alloc(46);
    head.writeUInt32LE(0x02014b50, 0);
    head.writeUInt16LE(20, 4);
    head.writeUInt16LE(20, 6);
    head.writeUInt16LE(0x0800, 8);
    head.writeUInt16LE(8, 10);
    head.writeUInt16LE(0, 12);
    head.writeUInt16LE(0x21, 14);
    head.writeUInt32LE(sum, 16);
    head.writeUInt32LE(packed.length, 20);
    head.writeUInt32LE(data.length, 24);
    head.writeUInt16LE(name.length, 28);
    head.writeUInt32LE(offset, 42);
    central.push(head, name);
    offset += local.length + name.length + packed.length;
  }
  const directory = Buffer.concat(central);
  const end = Buffer.alloc(22);
  end.writeUInt32LE(0x06054b50, 0);
  end.writeUInt16LE(entries.length, 8);
  end.writeUInt16LE(entries.length, 10);
  end.writeUInt32LE(directory.length, 12);
  end.writeUInt32LE(offset, 16);
  return Buffer.concat([...locals, directory, end]);
}

/**
 * Читает собранный архив обратно и сверяет с задуманным.
 *
 * Запись без чтения не доказывает ничего: ошибка в заголовке даёт файл, который
 * откроется у автора, но не у сервиса. Сверяются состав, тексты и манифест.
 */
function verify(path, expected) {
  const bytes = readFileSync(path);
  const endAt = bytes.lastIndexOf(Buffer.from([0x50, 0x4b, 0x05, 0x06]));
  if (endAt < 0) throw new Error(`${path}: нет записи конца каталога`);
  const count = bytes.readUInt16LE(endAt + 10);
  let at = bytes.readUInt32LE(endAt + 16);
  const read = new Map();
  for (let i = 0; i < count; i += 1) {
    const size = bytes.readUInt32LE(at + 20);
    const nameLength = bytes.readUInt16LE(at + 28);
    const extra = bytes.readUInt16LE(at + 30);
    const comment = bytes.readUInt16LE(at + 32);
    const local = bytes.readUInt32LE(at + 42);
    const name = bytes.subarray(at + 46, at + 46 + nameLength).toString("utf8");
    const localName = bytes.readUInt16LE(local + 26);
    const localExtra = bytes.readUInt16LE(local + 28);
    const start = local + 30 + localName + localExtra;
    read.set(name, inflateRawSync(bytes.subarray(start, start + size)).toString("utf8"));
    at += 46 + nameLength + extra + comment;
  }
  for (const entry of expected.entries) {
    if (read.get(entry.name) !== entry.text) throw new Error(`${path}: '${entry.name}' не сошёлся при чтении`);
  }
  const manifest = JSON.parse(read.get("takt-project.json"));
  if (manifest.format !== FORMAT || manifest.main_file !== expected.manifest.main_file) {
    throw new Error(`${path}: манифест не сошёлся при чтении`);
  }
}

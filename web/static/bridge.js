// Мост к модулю WebAssembly: буфер, JSON, операции.
//
// Здесь нет ни одного знания о языке Takt - только протокол обмена. Всё, что умеет
// редактор, считает модуль; эта прослойка лишь
// кладёт запрос в буфер и читает ответ.
//
// Протокол (см. `takt-wasm/src/lib.rs`):
//   1. `takt_io_reserve(len)` - растит буфер под запрос;
//   2. запрос UTF-8 JSON пишется по адресу `takt_io_ptr()`;
//   3. операция зовётся с длиной запроса и возвращает длину ответа;
//   4. ответ читается оттуда же.
//
// Адрес буфера перечитывается после каждого шага: модуль растит буфер под ответ, а
// `Vec` при росте переезжает - старый указатель ведёт в чужую память.

const encoder = new TextEncoder();
const decoder = new TextDecoder();

/** Загруженный модуль: экспорты и его память. */
export class Bridge {
  constructor(exports) {
    this.wasm = exports;
  }

  /** Загружает модуль по URL. */
  static async load(url) {
    const response = await fetch(url);
    // `instantiateStreaming` требует `Content-Type: application/wasm`; статика может
    // отдавать иное, поэтому байты берутся явно - ошибка загрузки должна быть про
    // модуль, а не про заголовок.
    const bytes = await response.arrayBuffer();
    const { instance } = await WebAssembly.instantiate(bytes, {});
    return new Bridge(instance.exports);
  }

  /** Зовёт операцию модуля и возвращает разобранный ответ. */
  call(operation, request) {
    const wasm = this.wasm;
    const bytes = encoder.encode(JSON.stringify(request ?? {}));
    wasm.takt_io_reserve(bytes.length);
    new Uint8Array(wasm.memory.buffer, wasm.takt_io_ptr(), bytes.length).set(bytes);
    const length = wasm[operation](bytes.length);
    const text = decoder.decode(
      new Uint8Array(wasm.memory.buffer, wasm.takt_io_ptr(), length)
    );
    return JSON.parse(text);
  }

  /** Версия языка, крейта и список целей. */
  version() {
    const wasm = this.wasm;
    const length = wasm.takt_version();
    const text = decoder.decode(
      new Uint8Array(wasm.memory.buffer, wasm.takt_io_ptr(), length)
    );
    return JSON.parse(text);
  }

  /**
   * Компилирует исходник целью `target`.
   *
   * Имя файла едет вместе с текстом: имя корневой модели берётся из него, и
   * вывод открытого проекта обязан нести имя его файла, а не имя буфера.
   *
   * @param {string} filename имя файла; пусто - умолчание моста
   */
  /**
   * @param {Record<string, string>} files состав проекта (имя - текст): по нему
   *   модуль разрешает `import`, диска у него нет
   */
  compile(target, args, source, filename = "", files = {}) {
    return this.call("takt_compile", { target, args, source, filename, files });
  }

  diagnostics(source, files = {}) {
    return this.call("takt_diagnostics", { source, files });
  }

  tokens(source) {
    return this.call("takt_tokens", { source });
  }

  /**
   * Подсветка вывода цели: те же пятёрки, что у `tokens`.
   *
   * Цель, а не язык: соответствие "цель -> язык вывода" знает модуль
   * (`c-hal` печатает C, `st-at` - Structured Text), и повторять его здесь
   * значило бы завести вторую таблицу целей.
   */
  highlight(target, text) {
    return this.call("takt_highlight", { target, text });
  }

  hover(source, line, character) {
    return this.call("takt_hover", { source, line, character });
  }

  goto(source, line, character) {
    return this.call("takt_goto", { source, line, character });
  }

  references(source, line, character) {
    return this.call("takt_references", { source, line, character });
  }

  rename(source, line, character, newName) {
    return this.call("takt_rename", { source, line, character, new_name: newName });
  }

  format(source) {
    return this.call("takt_format", { source });
  }

  symbols(source) {
    return this.call("takt_symbols", { source });
  }

  completion(source) {
    return this.call("takt_completion", { source });
  }

  /** Граф модели для схемы: листы, узлы, рёбра, ярусы - всё считает модуль. */
  graph(source) {
    return this.call("takt_graph", { source });
  }

  simOpen(source, scenario, tickMs, files = {}, steps = null) {
    return this.call("takt_sim_open", { source, scenario, tick_ms: tickMs ?? 0, files, steps });
  }

  simTick(id, budget) {
    return this.call("takt_sim_tick", { id, budget });
  }

  simClose(id) {
    return this.call("takt_sim_close", { id });
  }
}

/**
 * Разворачивает данные семантических токенов в отрезки по строкам.
 *
 * Модуль отдаёт их в форме LSP: пятёрки `(Δстрока, Δколонка, длина, тип,
 * модификаторы)`, где дельты считаются от предыдущего токена. Разворачивать их
 * обязан потребитель - иначе каждая страница делала бы это по-своему.
 *
 * Тип токена - индекс в списке `token_types` того же ответа, а не имя:
 * список ведёт слой LSP, и свой словарь у страницы завёлся бы ровно тем
 * дублем знания, который фича запрещает.
 */
export function spans(tokens) {
  const out = [];
  const types = tokens.token_types ?? [];
  const data = tokens.data ?? [];
  let line = 0;
  let column = 0;
  for (let i = 0; i + 4 < data.length; i += 5) {
    const deltaLine = data[i];
    const deltaStart = data[i + 1];
    const length = data[i + 2];
    const type = data[i + 3];
    line += deltaLine;
    column = deltaLine === 0 ? column + deltaStart : deltaStart;
    out.push({ line, column, length, type: types[type] ?? "" });
  }
  return out;
}

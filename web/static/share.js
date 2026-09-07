// Обмен по ссылке: состояние редактора во фрагменте URL.
//
// # Форма
//
// `{v, src, scn, t, args}` - версия модуля, исходник, сценарий, цель, ключи сборки.
// Кодирование: JSON -> `deflate-raw` -> base64url. Версия хранится затем, чтобы
// публикация открывалась модулем своей версии (решение A5): вывод целей меняется вместе
// с компилятором, и старая ссылка обязана показывать то же, что показывала.
//
// Сжатие - `CompressionStream`: он есть и в браузере, и в Node (18+), поэтому круговой
// рейс проверяется проверкой, а не глазом.

/** Собирает состояние в строку фрагмента (без `#`). */
export async function encodeState(state) {
  const json = JSON.stringify(compact(state));
  const compressed = await deflate(new TextEncoder().encode(json));
  return base64UrlEncode(compressed);
}

/** Восстанавливает состояние из строки фрагмента; `null` - фрагмент не наш. */
export async function decodeState(fragment) {
  const text = (fragment ?? "").replace(/^#/, "");
  if (!text) return null;
  try {
    const inflated = await inflate(base64UrlDecode(text));
    const parsed = JSON.parse(new TextDecoder().decode(inflated));
    return expand(parsed);
  } catch {
    // Чужой или испорченный фрагмент - не ошибка приложения: страница просто
    // открывается с умолчаниями. Молчать нельзя лишь о своём сбое, а этот случай
    // неотличим от "сюда пришли по обычной ссылке с якорем".
    return null;
  }
}

/** Короткие имена полей: ссылка тем короче, чем меньше в ней служебного. */
function compact(state) {
  const out = { v: state.version ?? "", src: state.source ?? "" };
  if (state.scenario) out.scn = state.scenario;
  if (state.target) out.t = state.target;
  if (state.args) out.args = state.args;
  return out;
}

/** Обратное преобразование имён. */
function expand(parsed) {
  return {
    version: parsed.v ?? "",
    source: parsed.src ?? "",
    scenario: parsed.scn ?? "",
    target: parsed.t ?? "",
    args: parsed.args ?? "",
  };
}

async function deflate(bytes) {
  return await through(bytes, new CompressionStream("deflate-raw"));
}

async function inflate(bytes) {
  return await through(bytes, new DecompressionStream("deflate-raw"));
}

/** Пропускает байты через поток преобразования и собирает результат. */
async function through(bytes, transform) {
  const stream = new Blob([bytes]).stream().pipeThrough(transform);
  const chunks = [];
  const reader = stream.getReader();
  for (;;) {
    const { done, value } = await reader.read();
    if (done) break;
    chunks.push(value);
  }
  let total = 0;
  for (const chunk of chunks) total += chunk.length;
  const out = new Uint8Array(total);
  let at = 0;
  for (const chunk of chunks) {
    out.set(chunk, at);
    at += chunk.length;
  }
  return out;
}

/**
 * base64url: алфавит URL-безопасный, набивка снята.
 *
 * Обычный base64 в ссылке негоден: `+`, `/` и `=` там значат другое, и
 * половина ссылок ломалась бы при копировании через мессенджер.
 */
function base64UrlEncode(bytes) {
  let binary = "";
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

function base64UrlDecode(text) {
  const padded = text.replace(/-/g, "+").replace(/_/g, "/");
  const binary = atob(padded + "=".repeat((4 - (padded.length % 4)) % 4));
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) bytes[i] = binary.charCodeAt(i);
  return bytes;
}

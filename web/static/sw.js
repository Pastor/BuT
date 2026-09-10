// Служебный воркер: страница открывается и работает без сети.
//
// # Что и как кешируется
//
// Воркер лежит в корне страницы (сборка переносит его туда из бандла): область
// воркера - каталог его адреса, и из `b/<отпечаток>/` он не видел бы ни страницы,
// ни модуля компилятора. Ответ выбирается по форме адреса относительно области:
//
// - `b/<отпечаток>/...` и `wasm/<версия>/...` - сначала кеш. Эти адреса
//   неизменны (сервер отдаёт их `immutable`), и кеш по ним никогда не врёт;
// - сама страница (переход) и `version.json` - сначала сеть, кеш - когда её нет.
//   Страница в сети обязана быть свежей: её отдают `no-cache`, и новый бандл
//   читатель получает прежним путём, а не после закрытия всех вкладок;
// - `api/...` - только сеть. Проекты, права и вход - данные сервера, и ответ из
//   кеша показал бы чужое состояние как нынешнее; без сети страница говорит об
//   отказе, а работа автора живёт в черновике браузера.
//
// Предзагрузка - всё, что нужно странице без сети: вход, опись сборки, модуль и
// каждый файл бандла. Список и идентификатор бандла подставляет сборка
// (`scripts/build-web.sh`), поэтому текст воркера меняется с каждым бандлом, и
// браузер сам ставит новый воркер. Кеш прежнего бандла снимается при его
// активации.

/** Идентификатор бандла: подставляет сборка. */
const BUNDLE = "__TAKT_BUNDLE__";
/** Адреса предзагрузки относительно области воркера: подставляет сборка. */
const PRECACHE = ["__TAKT_PRECACHE__"];
const CACHE = `takt-${BUNDLE}`;
/**
 * Собрана ли страница. Несобранная (модули прямо из `web/static`) подстановки не
 * получала, и воркер тогда ничего не перехватывает: кешировать ему нечего.
 */
const BUILT = !BUNDLE.startsWith("__");

/**
 * Как отвечать на запрос.
 *
 * @param {string} path адрес относительно области воркера, без ведущей косой черты
 * @param {boolean} navigate запрос - переход на страницу
 * @returns {"page"|"network"|"cache"|"fresh"}
 */
function strategyOf(path, navigate) {
  if (navigate) return "page";
  if (path.startsWith("api/")) return "network";
  if (path.startsWith("b/") || path.startsWith("wasm/")) return "cache";
  return "fresh";
}

self.addEventListener("install", (event) => {
  if (!BUILT) return;
  event.waitUntil(
    caches
      .open(CACHE)
      .then((cache) => cache.addAll(PRECACHE))
      .then(() => self.skipWaiting()),
  );
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    (async () => {
      for (const key of await caches.keys()) {
        if (key.startsWith("takt-") && key !== CACHE) await caches.delete(key);
      }
      await self.clients.claim();
    })(),
  );
});

self.addEventListener("fetch", (event) => {
  if (!BUILT) return;
  const request = event.request;
  if (request.method !== "GET") return;
  const url = new URL(request.url);
  const scope = new URL(self.registration.scope);
  if (url.origin !== scope.origin || !url.pathname.startsWith(scope.pathname)) return;
  const kind = strategyOf(url.pathname.slice(scope.pathname.length), request.mode === "navigate");
  if (kind === "network") return;
  event.respondWith(answer(kind, request, scope));
});

/**
 * Ответ по стратегии.
 *
 * Страница хранится под адресом области: корень и `/p/<id>` - одна и та же
 * разметка (сервер собирает её из одного `index.html`), и без сети любой переход
 * получает её.
 */
async function answer(kind, request, scope) {
  const cache = await caches.open(CACHE);
  if (kind === "cache") {
    const hit = await cache.match(request);
    if (hit) return hit;
    const response = await fetch(request);
    if (response.ok) await cache.put(request, response.clone());
    return response;
  }
  const key = kind === "page" ? scope.href : request;
  try {
    const response = await fetch(request);
    if (response.ok) await cache.put(key, response.clone());
    return response;
  } catch (error) {
    const hit = await cache.match(key);
    if (hit) return hit;
    throw error;
  }
}

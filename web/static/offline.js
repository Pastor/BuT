// Регистрация служебного воркера: страница открывается и без сети.
//
// Воркер лежит в корне страницы, а не в бандле: область воркера - каталог его
// адреса, и из `b/<отпечаток>/` он не видел бы ни страницы, ни модуля. Адрес
// считается от `document.baseURI`: под префиксом стенда (`<base href="/takt/">`)
// это `/takt/sw.js`, и область - вся страница под префиксом.

/**
 * Ставит воркер, если страница собрана и открыта по http(s).
 *
 * Несобранная страница (модули прямо из `web/static`) воркера не получает:
 * бандл и список предзагрузки ему подставить некому. Отказ регистрации странице
 * не мешает - она работает с сетью, как и без воркера, - но пишется в консоль:
 * молчаливый отказ выглядел бы работой без сети, которой нет.
 *
 * @param {object} win окно страницы (в тестах - подставное)
 * @param {string} moduleUrl адрес этого модуля: по нему видно, собрана ли страница
 * @returns {Promise<ServiceWorkerRegistration|null>|null}
 */
export function registerOffline(win = globalThis, moduleUrl = import.meta.url) {
  const workers = win.navigator?.serviceWorker;
  if (!workers || !/^https?:$/.test(win.location?.protocol ?? "")) return null;
  if (!/\/b\/[0-9a-f]+\//.test(moduleUrl)) return null;
  const url = new URL("sw.js", win.document.baseURI);
  return workers.register(url.href).catch((error) => {
    console.warn("service worker:", error);
    return null;
  });
}

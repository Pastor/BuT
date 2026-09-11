# Образ сервиса проектов Takt.
#
# Многостадийная сборка: инструменты сборки в готовый образ не едут.
#
# Собирается двое, и оба обязательны:
#   1. модуль `takt-wasm` и статика - их отдаёт браузеру сервер;
#   2. сам сервер - он же исполняет тот же модуль при выгрузке архива (09g).
# Разъедься они по версиям - вывод целей в архиве разошёлся бы с браузером
# молча. Здесь этого не случится: обе половины собираются из одного дерева.

FROM rust:1.97-bookworm AS build
WORKDIR /src

# Толчейн задаётся деревом (`rust-toolchain.toml`), а не строкой здесь: второй
# носитель версии разошёлся бы с первым.
COPY rust-toolchain.toml ./
RUN rustup target add wasm32-unknown-unknown
# Typst собирает справку страницы из документа `book/` (выгрузка HTML). Ставится
# из crates.io с `--locked` - версия и зависимости закреплены, двоичных файлов со
# стороны нет; слой стоит до `COPY . .` и пересобирается только при смене версии.
ARG TYPST_VERSION=0.15.1
RUN --mount=type=cache,target=/usr/local/cargo/registry,sharing=locked \
    --mount=type=cache,target=/usr/local/cargo/git,sharing=locked \
    cargo install typst-cli --version ${TYPST_VERSION} --locked

COPY . .

# Статика: модули + страница. Она же нужна серверу - из неё он берёт
# `wasm/<версия>/takt.wasm` для архива. Ядро и модуль экспорта собираются
# отдельными вызовами cargo: общий вызов объединил бы фичи, и ядро получило бы
# графику эталона.
# Профиль - `wasm`, а не `release`: `build-web.sh` ищет модуль по
# `TAKT_WASM_PROFILE` (умолчание `wasm`), и собранный `release` он не находит -
# "Модуль не собран", сборка образа падает. Профиль назван один раз, здесь же и
# передаётся скрипту: два носителя одного знания разошлись бы молча.
# Кеш cargo живёт между сборками (`--mount=type=cache`). Дерево копируется
# целиком и раньше сборки, поэтому строка в журнале рушит слой и заставляет
# собирать всё заново: реестр, `lalrpop` из git и порождённую им грамматику в
# 68 015 строк. Кеш этого не отменяет, но пересобирает только изменившееся.
# Каталог `target` кешируется отдельно от серверного (`sharing=locked`):
# профили у них разные, и общий каталог заставлял бы их вытеснять друг друга.
ARG WASM_PROFILE=wasm
RUN --mount=type=cache,target=/usr/local/cargo/registry,sharing=locked \
    --mount=type=cache,target=/usr/local/cargo/git,sharing=locked \
    --mount=type=cache,target=/src/target,sharing=locked \
    cargo build --profile ${WASM_PROFILE} --target wasm32-unknown-unknown -p takt-wasm \
 && cargo build --profile ${WASM_PROFILE} --target wasm32-unknown-unknown -p takt-wasm-export \
 && TAKT_WASM_PROFILE=${WASM_PROFILE} TAKT_WEB_DIST=/out/static TAKT_HELP_REQUIRED=1 scripts/build-web.sh

# Сервер - свой workspace (полторы сотни зависимостей не должны собираться на
# каждой правке компилятора).
# Каталог сборки задаётся явно: `.cargo/config.toml` корня переносит его в
# `target/precheck`, и конфигурация достаёт до `web/server` - тот же класс, что у
# расширения Zed. Без этого `cargo` собирает успешно, а `cp` ищет бинарник по
# несуществующему пути и отвечает "No such file or directory" после полутора
# сотен скомпилированных крейтов.
# Переменная окружения сильнее конфигурации - этим и пользуемся.
ARG SERVER_TARGET_DIR=/build/web-server
RUN --mount=type=cache,target=/usr/local/cargo/registry,sharing=locked \
    --mount=type=cache,target=/usr/local/cargo/git,sharing=locked \
    --mount=type=cache,target=/build/web-server,sharing=locked \
    cd web/server \
 && CARGO_TARGET_DIR=${SERVER_TARGET_DIR} cargo build --release \
 && cp ${SERVER_TARGET_DIR}/release/takt-web-server /out/

FROM debian:bookworm-slim AS runtime
# `ca-certificates` - для исходящих обращений при входе через площадки;
# `curl` - для проверки живости из `docker-compose`.
RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates curl \
 && rm -rf /var/lib/apt/lists/*

# Служба работает не от root: у неё есть каталог с чужими исходниками.
RUN useradd --system --home /srv/takt --create-home takt
WORKDIR /srv/takt

COPY --from=build /out/takt-web-server /usr/local/bin/takt-web-server
COPY --from=build /out/static /srv/takt/static

# Каталог исходников - том: он переживает пересборку образа.
RUN mkdir -p /srv/takt/projects && chown -R takt:takt /srv/takt
VOLUME ["/srv/takt/projects"]
USER takt

ENV TAKT_WEB_HOST=0.0.0.0 \
    TAKT_WEB_PORT=8730 \
    TAKT_WEB_STATIC=/srv/takt/static \
    TAKT_WEB_PROJECTS=/srv/takt/projects
EXPOSE 8730

# Живость спрашивает базу (`/health` ходит в неё): сервис, у которого базы
# нет, живым не является, и оркестратор обязан узнать об этом сам.
# Префикс входит в адрес проверки: сервер вкладывает под него весь роутер, и
# при `TAKT_WEB_BASE_PATH=/takt` ручка живёт по `/takt/health`. Проверка без
# префикса отвечала 404, и живой контейнер вечно числился нездоровым - замечено
# разбором "долгой выкладки" 2026-09-05. Корневой префикс `/` даёт `//health`,
# поэтому одиночная косая срезается.
HEALTHCHECK --interval=30s --timeout=5s --start-period=20s --retries=3 \
  CMD curl -fsS "http://127.0.0.1:8730${TAKT_WEB_BASE_PATH%/}/health" || exit 1

ENTRYPOINT ["takt-web-server"]

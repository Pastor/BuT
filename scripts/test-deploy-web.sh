#!/usr/bin/env bash
# Тест выкладки и скриптов стенда.
#
# Что доказывает:
#
# E1 выкладка кладёт на стенд то, на что ссылается разметка;
# E2 **подмена модуля под тем же адресом - отказ**, а не перезапись:
#       адрес `wasm/<версия>/` обещает неизменность, и молчаливая замена
#       портит страницу у каждого, кто уже кешировал;
# E3 прежние бандлы копятся не бесконечно, а свежий не снимается никогда;
# E4 скрипт стенда знает ровно четыре действия и **не удаляет тома**;
# E5 нет `node` либо модуля - мягкий пропуск, под `PRECHECK_STRICT=1` ошибка.
#
# Настройка nginx судится отдельным тестом (`test-setup-nginx-takt.sh`):
# у неё другой предмет - совместная жизнь с соседним сервисом на одном стенде.
#
# Docker здесь не запускается: предмет проверки - скрипты и раскладка, а
# подъём стенда проверяется человеком на стенде (в проекте нет исполняемого CI).

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
STRICT="${PRECHECK_STRICT:-0}"
NODE="${TAKT_NODE:-node}"
BIN_DIR="$("$ROOT/scripts/target-dir.sh")"
TARGET_DIR="$(dirname "$BIN_DIR")"
PROFILE="${TAKT_WASM_PROFILE:-wasm}"
WASM="$TARGET_DIR/wasm32-unknown-unknown/$PROFILE/takt_wasm.wasm"

skip_or_fail() {
  if [[ "$STRICT" == "1" ]]; then
    echo "  ОШИБКА: $1 (PRECHECK_STRICT=1)"
    exit 1
  fi
  echo "  пропуск: $1"
  exit 0
}

echo "Сторож выкладки статики (фича 0531)..."

# -- E4 не требует сборки: проверяется текстом --------------------------------
# Шаблона nginx здесь больше нет: он был вторым носителем правил прокси и
# уехал в генератор `setup-nginx-takt.sh` - его судит свой тест
# (`test-setup-nginx-takt.sh`). Две копии одних правил разошлись бы молча.

STAND_SH="$ROOT/scripts/stand.sh"
for action in up down status restart; do
  grep -qE "^  $action\)" "$STAND_SH" || {
    echo "  ПРОВАЛ: E4 у стенда нет действия '$action'"
    exit 1
  }
done
# Ключ `-v` уносит тома - базу и чужие исходники. Такое делают руками.
# Строки комментариев отброшены: сам запрет описан словами и именно словом
# `down -v` - грепом по всему файлу тест поймал бы собственное объяснение.
if grep -vE '^[[:space:]]*#' "$STAND_SH" | grep -qE 'down[^|]*-v( |$)'; then
  echo "  ПРОВАЛ: E4 скрипт стенда удаляет тома"
  exit 1
fi
grep -q '/health' "$STAND_SH" || {
  echo "  ПРОВАЛ: E4 состояние стенда не спрашивает /health"
  exit 1
}
echo "  OK: E4 стенд знает четыре действия, томов не трогает, спрашивает /health"

# -- Остальное требует собранного модуля --------------------------------------
command -v "$NODE" >/dev/null 2>&1 || skip_or_fail "не найден node (сборка статики его требует)"
[[ -f "$WASM" ]] || skip_or_fail "модуль не собран (см. check-wasm.sh)"

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
STAND="$WORK/stand"

# -- E1: выкладка кладёт то, на что ссылается разметка ------------------------
"$ROOT/scripts/deploy-web.sh" -d "$STAND" >/dev/null || {
  echo "  ПРОВАЛ: E1 выкладка не отработала"
  exit 1
}
missing=0
while read -r asset; do
  [[ -z "$asset" ]] && continue
  case "$asset" in http*|"#"*|data:*|/) continue ;; esac
  [[ -f "$STAND/$asset" ]] || { echo "    нет '$asset'"; missing=1; }
done < <(sed 's/<base [^>]*>//g' "$STAND/index.html" \
         | grep -oE '(href|src)="[^"]+"' | sed 's/.*="//; s/"//')
[[ "$missing" == "0" ]] || { echo "  ПРОВАЛ: E1 на стенде нет того, на что ссылается разметка"; exit 1; }
[[ -f "$STAND/wasm/index.json" ]] || { echo "  ПРОВАЛ: E1 нет описи версий модуля"; exit 1; }
echo "  OK: E1 выкладка разложила статику целиком"

# -- E2: подмена модуля отвергается -------------------------------------------
VERSION="$(sed -n 's/.*"takt_lang": "\([^"]*\)".*/\1/p' "$STAND/version.json" | head -1)"
printf 'подмена' >> "$STAND/wasm/$VERSION/takt.wasm"
if "$ROOT/scripts/deploy-web.sh" -d "$STAND" --dry-run >/dev/null 2>&1; then
  echo "  ПРОВАЛ: E2 подмена модуля под тем же адресом прошла"
  exit 1
fi
echo "  OK: E2 подмена модуля под адресом с версией отвергнута"

# -- E3: прежние бандлы копятся не бесконечно ---------------------------------
rm -rf "$STAND"
"$ROOT/scripts/deploy-web.sh" -d "$STAND" >/dev/null
CURRENT="$(sed -n 's/.*"bundle": "\([^"]*\)".*/\1/p' "$STAND/version.json" | head -1)"
# Подкладываем прежние бандлы - так же, как их оставили бы прошлые выкладки.
for old in aaaaaaaaaaaa bbbbbbbbbbbb cccccccccccc dddddddddddd; do
  mkdir -p "$STAND/b/$old"
  printf 'старое' > "$STAND/b/$old/app.js"
done
"$ROOT/scripts/deploy-web.sh" -d "$STAND" -k 2 >/dev/null
COUNT="$(find "$STAND/b" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"
if [[ "$COUNT" -gt 3 ]]; then
  echo "  ПРОВАЛ: E3 бандлов осталось $COUNT при пределе 2"
  exit 1
fi
[[ -d "$STAND/b/$CURRENT" ]] || {
  echo "  ПРОВАЛ: E3 снят СВЕЖИЙ бандл — страница перестала бы открываться"
  exit 1
}
echo "  OK: E3 прежние бандлы снимаются, свежий остаётся"

# -- E6: политика пропуска ----------------------------------------------------
if TAKT_NODE=такого-нет "$0" 2>&1 | grep -q "пропуск"; then
  echo "  OK: E5 без node — мягкий пропуск"
else
  echo "  ПРОВАЛ: E5 без node пропуск не назван"
  exit 1
fi
if TAKT_NODE=такого-нет PRECHECK_STRICT=1 "$0" >/dev/null 2>&1; then
  echo "  ПРОВАЛ: E5 под PRECHECK_STRICT=1 отсутствие node прошло"
  exit 1
fi
echo "  OK: E5 под PRECHECK_STRICT=1 отсутствие node — ошибка"

# -- E6: стек разбирается -----------------------------------------------------
# Предмет - не "файл на месте", а "docker его читает". Значение с двоеточием
# и пробелом (адрес базы, текст отказа `:?`) без кавычек YAML читает как
# отображение, и стек не поднимается вовсе: "mapping values are not allowed in
# this context". Класс нашла выкладка на стенд 2026-09-05 - дома стек не
# поднимали ни разу, и проверки его не читали.
if docker compose version >/dev/null 2>&1; then
  if TAKT_WEB_JWT_SECRET=проба-сторожа docker compose -p takt-guard \
       --project-directory "$ROOT/web/deploy" \
       -f "$ROOT/web/deploy/docker-compose.yml" config -q >/dev/null 2>&1; then
    echo "  OK: E6 стек разбирается docker compose"
  else
    echo "  ПРОВАЛ: E6 стек не разбирается — на стенде он не поднимется:"
    TAKT_WEB_JWT_SECRET=проба-сторожа docker compose -p takt-guard \
      --project-directory "$ROOT/web/deploy" \
      -f "$ROOT/web/deploy/docker-compose.yml" config -q 2>&1 | head -3 | sed 's/^/    /'
    exit 1
  fi
else
  echo "  пропуск: E6 нет docker compose — разбор стека не проверен"
fi

# -- E7: образ собирает модуль тем же профилем, что ищет сборка статики -------
# `build-web.sh` ищет модуль по `TAKT_WASM_PROFILE` (умолчание `wasm`), а
# образ собирал его `--release` - и сборка падала на "Модуль не собран" уже на
# Стенде, после полутора сотен скомпилированных крейтов. Два носителя одного
# знания разошлись молча.
DOCKER_PROFILE="$(grep -oE 'cargo build --profile \$\{WASM_PROFILE\}|cargo build --release --target wasm32' "$ROOT/web/deploy/Dockerfile" | head -1)"
if [[ "$DOCKER_PROFILE" != 'cargo build --profile ${WASM_PROFILE}' ]]; then
  echo "  ПРОВАЛ: E7 образ собирает модуль не тем профилем, что ищет build-web.sh"
  echo "          (нашлось: '${DOCKER_PROFILE:-ничего}')"
  exit 1
fi
grep -q 'TAKT_WASM_PROFILE=\${WASM_PROFILE}' "$ROOT/web/deploy/Dockerfile" || {
  echo "  ПРОВАЛ: E7 профиль не передан скрипту сборки статики — он возьмёт своё умолчание"
  exit 1
}
echo "  OK: E7 профиль модуля назван один раз и доезжает до сборки статики"

# -- E8: сервер собирается в названный каталог -------------------------------
# `.cargo/config.toml` корня переносит каталог сборки (`target/precheck`,
# ) и достаёт до `web/server` - тот же класс, что у расширения Zed
# . Без явного `CARGO_TARGET_DIR` сборка проходит, а копирование
# бинарника падает: "No such file or directory" уже на стенде.
grep -q 'CARGO_TARGET_DIR=\${SERVER_TARGET_DIR} cargo build --release' "$ROOT/web/deploy/Dockerfile" || {
  echo "  ПРОВАЛ: E8 сервер в образе собирается без явного CARGO_TARGET_DIR"
  echo "          конфигурация корня уведёт бинарник, и cp его не найдёт"
  exit 1
}
grep -q 'cp \${SERVER_TARGET_DIR}/release/takt-web-server /out/' "$ROOT/web/deploy/Dockerfile" || {
  echo "  ПРОВАЛ: E8 копирование берёт бинарник не из того каталога, куда собирали"
  exit 1
}
echo "  OK: E8 каталог сборки сервера назван один раз и оттуда же берётся бинарник"

# -- E9: том базы смонтирован по раскладке образа ----------------------------
# `postgres:18+` держит данные в подкаталоге с номером версии и при
# монтировании тома в `/var/lib/postgresql/data` не стартует вовсе: "there
# appears to be PostgreSQL data in /var/lib/postgresql/data (unused
# mount/volume)". Раскладка от 17-й версии доживает до стенда - дома стек не
# поднимали ни разу.
if grep -qE '^\s+- db:/var/lib/postgresql/data' "$ROOT/web/deploy/docker-compose.yml"; then
  echo "  ПРОВАЛ: E9 том базы смонтирован в /var/lib/postgresql/data"
  echo "          образ 18+ с такой раскладкой не стартует — стек не поднимется"
  exit 1
fi
grep -qE '^\s+- db:/var/lib/postgresql$' "$ROOT/web/deploy/docker-compose.yml" || {
  echo "  ПРОВАЛ: E9 том базы не смонтирован в /var/lib/postgresql"
  exit 1
}
echo "  OK: E9 том базы смонтирован по раскладке образа"

# -- E10: проверка живости знает префикс -------------------------------------
# Сервер вкладывает под префикс весь роутер: при `TAKT_WEB_BASE_PATH=/takt`
# ручка живёт по `/takt/health`, и проверка без префикса отвечает 404. Живой
# контейнер вечно числился нездоровым - замечено разбором "долгой выкладки"
# 2026-09-05, когда на вопрос "почему долго" ответ оказался "выкладка давно
# кончилась".
if grep -qE 'CMD curl[^|]*127\.0\.0\.1:8730/health' "$ROOT/web/deploy/Dockerfile"; then
  echo "  ПРОВАЛ: E10 проверка живости образа не знает префикса"
  echo "          под TAKT_WEB_BASE_PATH ручка живёт по <префикс>/health"
  exit 1
fi
grep -qE 'TAKT_WEB_BASE_PATH%?/?\}?/health' "$ROOT/web/deploy/Dockerfile" || {
  echo "  ПРОВАЛ: E10 в проверке живости нет префикса"
  exit 1
}
echo "  OK: E10 проверка живости знает префикс"

# -- E11: сборка кеширует cargo между выкладками -----------------------------
# Дерево копируется целиком и раньше сборки, поэтому строка в журнале рушит
# слой: без кеша каждая выкладка собирает `lalrpop` из git и порождённую им
# грамматику заново. Кеш этого не отменяет, но пересобирает изменившееся.
# Кеш нужен обеим сборкам - модуля и сервера: без него любая из них
# собирает свою половину зависимостей заново, и выигрыш съедается наполовину.
for target in /src/target /build/web-server; do
  grep -q "mount=type=cache,target=$target" "$ROOT/web/deploy/Dockerfile" || {
    echo "  ПРОВАЛ: E11 кеш каталога сборки '$target' не смонтирован"
    exit 1
  }
done
registries="$(grep -c 'mount=type=cache,target=/usr/local/cargo/registry' "$ROOT/web/deploy/Dockerfile")"
if [[ "$registries" -lt 2 ]]; then
  echo "  ПРОВАЛ: E11 реестр cargo кешируется не у обеих сборок ($registries из 2)"
  exit 1
fi
echo "  OK: E11 сборка кеширует загруженное и собранное"

# -- E12: ssh выкладки не висит вечно ----------------------------------------
# Оборванный канал без "живых" проб висит бесконечно, и выкладка выглядит
# вечной сборкой: на стенде уже ничего не считается, а клиент ждёт.
grep -q 'ServerAliveInterval' "$ROOT/scripts/deploy-stand.sh" || {
  echo "  ПРОВАЛ: E12 у ssh выкладки нет живых проб"
  exit 1
}
echo "  OK: E12 ssh выкладки не висит вечно"

echo "  Сторож выкладки: все проверки пройдены (E1…E12)."

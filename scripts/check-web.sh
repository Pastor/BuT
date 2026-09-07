#!/usr/bin/env bash
# Проверка веб-части.
#
# Что доказывает:
#
# 1. страница собирается - `build-web.sh` кладёт всё, на что ссылается
#      разметка (пропавший файл иначе обнаружился бы только в браузере);
# 2. её код работает - тесты в `node`: круговой рейс ссылки, перевод
#      координат, черновик, форма ответов моста;
# 3. в `web/` нет списка ключевых слов Takt - знание о языке живёт в лексере,
# и вторая его копия разошлась бы молча (критерий 2 фичи);
# 4. каждый скрипт страницы разбирается - `node --check` вместо браузера;
# 5. словари оболочки лежат в собранной статике - без них подписи вырождаются
#      в ключи, а разметка на них не ссылается (их берёт JavaScript);
# 6. Раскладка собранного верна - отпечаток бандла, описи версий, предсжатие
#      (проверки в `node` получают путь к собранному дереву).
#
# Политика внешнего инструмента - как у ST-арбитра: нет `node` - мягкий
# пропуск, под `PRECHECK_STRICT=1` - ошибка.

set -euo pipefail

. "$(dirname "${BASH_SOURCE[0]}")/gatelib.sh"

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

STRICT="${PRECHECK_STRICT:-0}"
NODE="${TAKT_NODE:-node}"
BIN_DIR="$("$(dirname "${BASH_SOURCE[0]}")/target-dir.sh")"
TARGET_DIR="$(dirname "$BIN_DIR")"
PROFILE="${TAKT_WASM_PROFILE:-wasm}"
WASM="$TARGET_DIR/wasm32-unknown-unknown/$PROFILE/takt_wasm.wasm"

skip_or_fail() {  # $1 = причина
  if [[ "$STRICT" == "1" ]]; then
    echo "  ОШИБКА: $1 (PRECHECK_STRICT=1)"
    exit 1
  fi
  echo "  пропуск: $1"
  exit 0
}

echo "Гейт веб-части (фича 0531)..."

command -v "$NODE" >/dev/null 2>&1 || skip_or_fail "не найден node"
[[ -f "$WASM" ]] || skip_or_fail "модуль не собран (см. check-wasm.sh)"

# -- 1. Сборка статики --------------------------------------------------------
DIST="$(mktemp -d)/dist"
TAKT_WEB_DIST="$DIST" "$ROOT/scripts/build-web.sh"

# Разметка ссылается только на то, что собрано: пропавший файл - белая страница
# в браузере и ни слова в консоли сборки.
#
# `<base>` из разбора выброшен: он называет корень адресов, а не файл, и его
# `href="/"` искали бы на диске.
missing=0
while read -r asset; do
  [[ -z "$asset" ]] && continue
  case "$asset" in
    http*|"#"*|data:*) continue ;;
  esac
  if [[ ! -f "$DIST/$asset" ]]; then
    echo "  ОШИБКА: разметка ссылается на '$asset', которого нет в собранной статике"
    missing=1
  fi
done < <(sed 's/<base [^>]*>//g' "$DIST/index.html" \
           | grep -oE '(href|src)="[^"]+"' | sed 's/.*="//; s/"//')
[[ "$missing" == "0" ]] || exit 1

# -- 2. Словари оболочки собраны ----------------------------------------------
# На словарь разметка не ссылается - его запрашивает `i18n.js`, и проверка
# выше его не видит. Пропавший словарь даёт страницу, подписанную ключами.
BUNDLE_DIR="$(find "$DIST/b" -mindepth 1 -maxdepth 1 -type d | head -1)"
dicts=0
for dict in "$ROOT"/web/static/i18n/*.json; do
  name="$(basename "$dict")"
  dicts=$((dicts + 1))
  if [[ ! -f "$BUNDLE_DIR/i18n/$name" ]]; then
    echo "  ОШИБКА: словарь '$name' не попал в собранную статику"
    exit 1
  fi
done

DICTS_NOTE="$(require_input "словари оболочки" "$dicts" 1 "web/static/i18n")" || exit 1

# -- 3. Разбор каждого скрипта ------------------------------------------------
scripts_seen=0
for script in "$ROOT"/web/static/*.js; do
  scripts_seen=$((scripts_seen + 1))
  "$NODE" --check "$script" || {
    echo "  ОШИБКА: не разбирается $script"
    exit 1
  }
done
SCRIPTS_NOTE="$(require_input "модули веб-части" "$scripts_seen" 1 "web/static")" || exit 1
echo "  Веб-часть: $DICTS_NOTE, $SCRIPTS_NOTE."

# -- 4. Списка ключевых слов Takt в вебе нет ----------------------------------
# Признак - набор слов языка рядом друг с другом. Ищутся те, которые нигде,
# кроме словаря, вместе не встретятся: страница красит по ответу модуля и
# знать их не должна.
if grep -REn '"(start|state|model|invariant)"[[:space:]]*,[[:space:]]*"(start|state|model|invariant|always|enter|exit)"' \
     "$ROOT/web/static" "$ROOT/web/tests" >/dev/null 2>&1; then
  echo "  ОШИБКА: в web/ появился список ключевых слов Takt — знание о языке живёт в лексере"
  grep -REn '"(start|state|model|invariant)"[[:space:]]*,' "$ROOT/web/static" "$ROOT/web/tests" | head -5
  exit 1
fi

# -- 5. Проверки в node -------------------------------------------------------
"$NODE" "$ROOT/web/tests/web-tests.mjs" "$WASM" "$DIST"

rm -rf "$(dirname "$DIST")"

#!/usr/bin/env bash
# Сборка статики онлайн-редактора.
#
# # Раскладка собранного
#
# index.html вход; кешу не отдаётся (`no-cache`)
# version.json идентификатор бандла и адрес модуля; `no-cache`
# b/<отпечаток>/... страница целиком: скрипты, стили, шрифт, словари
# wasm/index.json какая версия модуля последняя; `no-cache`
# wasm/<версия>/... модуль, его опись и контрольная сумма
#
# **Содержимое задаёт адрес, адрес задаёт срок** (правило образца, замер
# 2026-09-04). Помеченное отпечатком неизменно и живёт год; непомеченное -
# `no-cache`, иначе после выкладки браузер ещё десять минут показывал бы старую
# страницу, подтягивая к ней новые стили.
#
# Отпечаток - на бандл, а не на каждый файл. у образца он пофайловый,
# потому что понятия бандла у него нет; у нас пофайловый потребовал бы
# переписывать спецификаторы `import` в порядке зависимостей - работу
# сборщика, от которого проект отказался.
# Внутри каталога бандла все ссылки относительные и переписывать нечего.
# Цена: правка одного файла обновляет весь бандл - 60 КиБ текста против
# 3,3 МиБ модуля рядом, который версионируется отдельно.
#
# Модуль лежит по адресу с версией: публикация открывается
# своим модулем и через год. Рядом `manifest.json` с контрольной суммой - по
# ней выкладка отказывает на подмене уже выложенного.
#
# Бандлера здесь нет: страница написана модулями браузера и грузится как
# есть. Минификации тоже нет - цена принята ради того, чтобы предкоммит не
# требовал ни сети, ни `npm`.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

BIN_DIR="$("$(dirname "${BASH_SOURCE[0]}")/target-dir.sh")"
TARGET_DIR="$(dirname "$BIN_DIR")"
PROFILE="${TAKT_WASM_PROFILE:-wasm}"
WASM="$TARGET_DIR/wasm32-unknown-unknown/$PROFILE/takt_wasm.wasm"
DIST="${TAKT_WEB_DIST:-$ROOT/web/dist}"
STATIC="$ROOT/web/static"

if [[ ! -f "$WASM" ]]; then
  echo "Модуль не собран: $WASM"
  echo "Соберите: cargo build -p takt-wasm --profile $PROFILE --target wasm32-unknown-unknown"
  exit 1
fi

# Версия крейта модуля - из его манифеста: второй записи версии в проекте быть
# не должно.
VERSION="$(awk -F'"' '/^version = /{print $2; exit}' "$ROOT/takt-lang/Cargo.toml")"
# Константа живёт в `takt-lang/src/version.rs` (там же её ищет своя проверка), а
# `lib.rs` её только реэкспортирует. Пока читали `lib.rs`, поле `language`
# описей выходило пустым, и заметить это можно было лишь заглянув в
# `version.json`: страница берёт версию языка у самого модуля.
LANGUAGE="$(awk -F'"' '/^pub const LANGUAGE_VERSION/{print $2; exit}' "$ROOT/takt-lang/src/version.rs")"
if [[ -z "$VERSION" || -z "$LANGUAGE" ]]; then
  echo "  ОШИБКА: версия крейта ('$VERSION') либо версия языка ('$LANGUAGE') не прочитана"
  exit 1
fi
BUILT_AT="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

sha256() { shasum -a 256 "$1" | awk '{print $1}'; }

# -- Отпечаток бандла ---------------------------------------------------------
# Считается по содержимому всех файлов страницы в устойчивом порядке: правка
# любого из них даёт новый адрес, а перезапуск сборки без правок - тот же.
# Дата и версия модуля в отпечаток не входят: иначе он менялся бы на каждой
# сборке, и кеш читателя обесценивался бы без единой правки.
BUNDLE="$(
  find "$STATIC" -type f ! -name ".*" -print0 \
    | LC_ALL=C sort -z \
    | xargs -0 shasum -a 256 \
    | sed "s|$STATIC/||" \
    | shasum -a 256 \
    | cut -c1-12
)"

rm -rf "$DIST"
mkdir -p "$DIST/b/$BUNDLE" "$DIST/wasm/$VERSION"

# Страница целиком - в каталог бандла. Ссылки внутри относительные, поэтому
# копируется дерево `web/static` как есть.
( cd "$STATIC" && tar cf - . ) | ( cd "$DIST/b/$BUNDLE" && tar xf - )

# Вход остаётся в корне: он `no-cache`, и адрес его меняться не должен -
# именно им делятся. Ссылки в нём переписываются на каталог бандла.
mv "$DIST/b/$BUNDLE/index.html" "$DIST/index.html"
# Переписываются только относительные адреса без схемы: `#`, `data:` и
# внешние остаются как есть (их в разметке и нет - внешних CDN у страницы нет).
sed -i.bak -E 's%(href|src)="([^"#:/][^"]*)"%\1="b/'"$BUNDLE"'/\2"%g' "$DIST/index.html"
rm -f "$DIST/index.html.bak"

cp "$WASM" "$DIST/wasm/$VERSION/takt.wasm"
WASM_SHA="$(sha256 "$DIST/wasm/$VERSION/takt.wasm")"
WASM_SIZE="$(wc -c < "$DIST/wasm/$VERSION/takt.wasm" | tr -d ' ')"

# -- Номер сборки сервиса -----------------------------------------------------
# Инкрементальный номер, как у образца: читателю он говорит "свежее или
# старее", а версия языка и версия модуля отвечают на другой вопрос.
#
# Номер считается числом коммитов ветки, а не файлом-счётчиком: файл пришлось
# бы коммитить каждой сборкой (шум в истории и гонка при двух сборках подряд), а
# счёт коммитов монотонен, воспроизводим и не требует записи в дерево.
#
# Вне git (сборка из архива, без истории) номер не выдумывается: пустое поле
# честнее придуманного, и страница тогда показывает время сборки.
if BUILD_NUMBER="${TAKT_BUILD_NUMBER:-$(git -C "$ROOT" rev-list --count HEAD 2>/dev/null)}"; then :; fi
BUILD_NUMBER="${BUILD_NUMBER:-}"
BUILD_COMMIT="$(git -C "$ROOT" rev-parse --short=12 HEAD 2>/dev/null || echo "")"
BUILD_BRANCH="$(git -C "$ROOT" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "")"

# Опись модуля: по ней выкладка отказывает, если под уже занятым адресом
# `wasm/<версия>/` лежит другой файл. Адрес обещает неизменность, и подмена
# под ним - молчаливая порча у всех, кто уже кешировал.
cat > "$DIST/wasm/$VERSION/manifest.json" <<JSON
{
  "takt_lang": "$VERSION",
  "language": "$LANGUAGE",
  "sha256": "$WASM_SHA",
  "size": $WASM_SIZE,
  "built_at": "$BUILT_AT"
}
JSON

# Какая версия последняя. Список версий ведёт выкладка: здесь сборка
# знает только про свою.
cat > "$DIST/wasm/index.json" <<JSON
{
  "latest": "$VERSION",
  "versions": ["$VERSION"]
}
JSON

# Опись сборки: по ней открытая вкладка узнаёт, что вышла новая.
# Идентификатор бандла здесь один и тот же, что в адресе `b/<отпечаток>/`:
# страница читает свой из собственного адреса модуля (`import.meta.url`), и
# второго носителя у него нет.
cat > "$DIST/version.json" <<JSON
{
  "bundle": "$BUNDLE",
  "takt_lang": "$VERSION",
  "language": "$LANGUAGE",
  "wasm": "wasm/$VERSION/takt.wasm",
  "built_at": "$BUILT_AT",
  "build": "$BUILD_NUMBER",
  "commit": "$BUILD_COMMIT",
  "branch": "$BUILD_BRANCH"
}
JSON

# -- Предсжатие ---------------------------------------------------------------
# Стенд ничего не считает на лету: модуль 3,3 мб, и сжимать его каждому первому
# заходу - лишняя работа. `brotli` берётся,
# если он есть, - мягкий пропуск, как у внешних инструментов проекта.
compressed=0
while IFS= read -r file; do
  gzip -9 -k -f "$file"
  compressed=$((compressed + 1))
  if command -v brotli >/dev/null 2>&1; then
    brotli -f -q 11 -o "$file.br" "$file"
  fi
done < <(find "$DIST" -type f \( -name '*.js' -o -name '*.css' -o -name '*.html' \
           -o -name '*.json' -o -name '*.svg' -o -name '*.wasm' \) ! -name 'version.json' \
           ! -name 'index.json')

size_kib=$(( WASM_SIZE / 1024 ))
files=$(find "$DIST" -type f | wc -l | tr -d ' ')
echo "  статика собрана: $DIST ($files файлов, модуль ${size_kib} КиБ, версия $VERSION)"
echo "  бандл b/$BUNDLE, предсжато файлов: $compressed$(command -v brotli >/dev/null 2>&1 && echo ' (gzip + brotli)' || echo ' (gzip; brotli не найден)')"

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
#                (адрес модуля в `version.json` несёт отпечаток содержимого
#                запросом: версия крейта между выкладками не меняется, а файл
#                меняется, и кеш иначе отдаёт вчерашний модуль новой странице)
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

# -- Справка ------------------------------------------------------------------
# Описание языка из документа `book/` - тем же Typst, что собирает PDF, но
# выгрузкой HTML; фрагмент для окна справки готовит `build-web-help.py`.
# Справка собирается раньше отпечатка бандла и входит в него: правка документа без
# правки страницы иначе оставила бы прежний адрес `b/<отпечаток>/`, помеченный
# неизменным, и читатель видел бы вчерашнюю справку.
# Нет `typst` либо самого документа (сборка в урезанной копии дерева) - справки
# нет, и страница говорит об этом при открытии окна. Стенд обязан её иметь: там
# сборка идёт с `TAKT_HELP_REQUIRED=1`.
HELP_DIR="$(mktemp -d)"
trap 'rm -rf "$HELP_DIR"' EXIT
if command -v typst >/dev/null 2>&1 && [[ -f "$ROOT/book/src/main.typ" ]]; then
  if ! typst compile --features html --format html --root "$ROOT/book" \
      "$ROOT/book/src/main.typ" "$HELP_DIR/book.html" 2> "$HELP_DIR/typst.log"; then
    cat "$HELP_DIR/typst.log"
    echo "  ОШИБКА: документ book/ не собран в HTML"
    exit 1
  fi
  python3 "$ROOT/scripts/build-web-help.py" "$HELP_DIR/book.html" "$HELP_DIR/help.html"
elif [[ "${TAKT_HELP_REQUIRED:-0}" == "1" ]]; then
  echo "  ОШИБКА: нет typst либо документа book/, а справка обязательна (TAKT_HELP_REQUIRED=1)"
  exit 1
else
  echo "  пропуск: нет typst либо документа book/ - справка не собрана"
fi

# -- Отпечаток бандла ---------------------------------------------------------
# Считается по содержимому всех файлов страницы в устойчивом порядке: правка
# любого из них даёт новый адрес, а перезапуск сборки без правок - тот же.
# Дата и версия модуля в отпечаток не входят: иначе он менялся бы на каждой
# сборке, и кеш читателя обесценивался бы без единой правки.
BUNDLE="$(
  {
    find "$STATIC" -type f ! -name ".*" -print0 \
      | LC_ALL=C sort -z \
      | xargs -0 shasum -a 256 \
      | sed "s|$STATIC/||"
    if [[ -f "$HELP_DIR/help.html" ]]; then (cd "$HELP_DIR" && shasum -a 256 help.html); fi
  } | shasum -a 256 | cut -c1-12
)"

rm -rf "$DIST"
mkdir -p "$DIST/b/$BUNDLE" "$DIST/wasm/$VERSION"

# Страница целиком - в каталог бандла. Ссылки внутри относительные, поэтому
# копируется дерево `web/static` как есть.
( cd "$STATIC" && tar cf - . ) | ( cd "$DIST/b/$BUNDLE" && tar xf - )
if [[ -f "$HELP_DIR/help.html" ]]; then cp "$HELP_DIR/help.html" "$DIST/b/$BUNDLE/help.html"; fi

# Вход остаётся в корне: он `no-cache`, и адрес его меняться не должен -
# именно им делятся. Ссылки в нём переписываются на каталог бандла.
mv "$DIST/b/$BUNDLE/index.html" "$DIST/index.html"
# Переписываются только относительные адреса: `#`, `data:` и адреса со схемой
# (ссылка на репозиторий) остаются как есть - в относительном адресе двоеточия нет.
sed -i.bak -E 's%(href|src)="([^"#:/][^":]*)"%\1="b/'"$BUNDLE"'/\2"%g' "$DIST/index.html"
rm -f "$DIST/index.html.bak"

# Холст для панели редактора: та же разметка и те же модули, данные снаружи.
# Страница собирается из `index.html`, чтобы второго носителя разметки не было.
python3 "$ROOT/scripts/build-scheme-host.py" "$DIST/b/$BUNDLE"

cp "$WASM" "$DIST/wasm/$VERSION/takt.wasm"
WASM_SHA="$(sha256 "$DIST/wasm/$VERSION/takt.wasm")"
WASM_SIZE="$(wc -c < "$DIST/wasm/$VERSION/takt.wasm" | tr -d ' ')"
# Отпечаток содержимого в адресе модуля. Версия крейта отвечает на вопрос "какой
# это компилятор", но не на вопрос "тот же ли это файл": между двумя выкладками
# модуль меняется, а версия остаётся, и адрес `wasm/<версия>/takt.wasm`, помеченный
# `immutable` на год, начинает врать - у читателя со вчерашней вкладкой остаётся
# вчерашний модуль под новой страницей, и он отвечает "operation is not a function"
# на операции, которой в нём нет (замер 2026-09-08: `takt_graph` при неизменной
# 0.60.0). Путь на диске прежний - его знает и сервер (`module.rs`), - меняется
# запрос: для кеша это другой ресурс, для файловой системы тот же файл.
WASM_TAG="${WASM_SHA:0:12}"

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
  "wasm": "wasm/$VERSION/takt.wasm?$WASM_TAG",
  "built_at": "$BUILT_AT",
  "build": "$BUILD_NUMBER",
  "commit": "$BUILD_COMMIT",
  "branch": "$BUILD_BRANCH"
}
JSON

# -- Служебный воркер ---------------------------------------------------------
# Воркер переезжает из бандла в корень: область воркера - каталог его адреса, и из
# `b/<отпечаток>/` он не видел бы страницы. В корне он `no-cache`, как вход, и
# браузер сверяет его при каждом заходе. Сборка подставляет бандл и список
# предзагрузки: вход, опись, модуль с отпечатком и каждый файл бандла. Текст
# воркера меняется с каждым бандлом - браузер сам ставит новый, а тот снимает
# кеш прежнего.
mv "$DIST/b/$BUNDLE/sw.js" "$DIST/sw.js"
python3 - "$DIST" "$BUNDLE" "wasm/$VERSION/takt.wasm?$WASM_TAG" <<'PY'
import json
import pathlib
import sys

dist, bundle, wasm = pathlib.Path(sys.argv[1]), sys.argv[2], sys.argv[3]
files = sorted(
    p.relative_to(dist).as_posix()
    for p in (dist / "b" / bundle).rglob("*")
    if p.is_file() and p.suffix not in (".gz", ".br")
)
sw = dist / "sw.js"
text = sw.read_text(encoding="utf-8")
for mark in ('"__TAKT_BUNDLE__"', '["__TAKT_PRECACHE__"]'):
    if mark not in text:
        sys.exit(f"  ОШИБКА: в sw.js нет места подстановки {mark}")
text = text.replace('"__TAKT_BUNDLE__"', json.dumps(bundle))
text = text.replace('["__TAKT_PRECACHE__"]', json.dumps(["./", "version.json", wasm, *files], indent=2))
sw.write_text(text, encoding="utf-8")
PY

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

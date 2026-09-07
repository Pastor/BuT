#!/bin/sh
# Проверка снимков порождённого кода в book/.
#
# Правило: файл в `book/src/**/generated/<цель>/` - это вывод инструмента на
# соседнем примере (`../examples/<имя>.takt`), и он обязан совпадать с тем, что
# инструмент печатает сегодня. Приложение "Порождённый код примера" читает эти
# файлы напрямую (`read(...)`), то есть показывает их читателю как настоящий
# вывод компилятора.
#
# Повод - не теория. Замер 2026-08-19 нашёл три отставших снимка,
# все с расхождением по существу:
# rust - константа `DWELL_TICKS` вместо `LIFT_DWELL_TICKS` (квалификация
# владельцем): документ показывал имя, которого цель не
#           печатает;
# sv - то же (`DWELL_TICKS` против `lift_DWELL_TICKS`);
# st - отсутствовал блок первого скана `IF state = 0`, то есть
#           снимок описывал автомат, сдвинутый на такт.
# Без сверки расхождение находят только глазами: снимок цели обновляют
# `c`, - его же, а соседние цели не смотрел никто.
#
# Проверка опирается на детерминированность генерации: без неё diff
# был бы шумом. Проверка детерминированности стоит в том же предкоммите.
#
# Контракт частоты: модель с объявлением `clock` требует
# совпадающего `--tick-hz`, иначе цель `c` отвечает `SE-069`. Частота
# извлекается из текста примера - тем же приёмом, что у проверки примеров book/.
#
# Использование:
# scripts/check-book-generated.sh # проверить (шаг precheck.sh)
# scripts/check-book-generated.sh --update # пересобрать снимки на месте
#
# POSIX sh, без внешних зависимостей.
set -eu

. "$(dirname "$0")/gatelib.sh"

ROOT="${BG_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
TAKTC="${TAKTC:-$ROOT/target/precheck/debug/taktc}"
UPDATE=0
[ "${1:-}" = "--update" ] && UPDATE=1

if [ ! -x "$TAKTC" ]; then
    echo "check-book-generated: не найден компилятор $TAKTC" >&2
    exit 1
fi

echo "Гейт снимков порождённого кода book/ (фича 0274)..."

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

BAD=0
CHECKED=0

# Частота такта из объявления `clock` примера (пусто - объявления нет).
tick_flag_of() {
    LIT="$(grep -oE 'clock[[:space:]]+[0-9]+[kMG]?Hz' "$1" | head -1 | sed -E 's/clock[[:space:]]+//' || true)"
    [ -z "$LIT" ] && return 0
    NUM="$(printf '%s' "$LIT" | sed -E 's/([0-9]+).*/\1/')"
    case "$LIT" in
        *kHz) HZ=$((NUM * 1000)) ;;
        *MHz) HZ=$((NUM * 1000000)) ;;
        *GHz) HZ=$((NUM * 1000000000)) ;;
        *) HZ=$NUM ;;
    esac
    printf -- '--tick-hz=%s' "$HZ"
}

# Обход: каждый каталог `generated/<цель>` рядом с `examples/`.
for GEN_DIR in $(find "$ROOT/book/src" -type d -name generated | sort); do
    SECTION="$(dirname "$GEN_DIR")"
    EXAMPLES="$SECTION/examples"
    if [ ! -d "$EXAMPLES" ]; then
        echo "  ОШИБКА: $GEN_DIR без соседнего examples/ — источник снимка неизвестен" >&2
        BAD=1
        continue
    fi
    for TARGET_DIR in "$GEN_DIR"/*; do
        [ -d "$TARGET_DIR" ] || continue
        TARGET="$(basename "$TARGET_DIR")"
        # Имя примера - по имени файла снимка (`lift.c` -> `lift.takt`).
        SRC=""
        for SNAP in "$TARGET_DIR"/*; do
            [ -f "$SNAP" ] || continue
            STEM="$(basename "$SNAP")"
            STEM="${STEM%.*}"
            if [ -f "$EXAMPLES/$STEM.takt" ]; then
                SRC="$EXAMPLES/$STEM.takt"
                break
            fi
        done
        if [ -z "$SRC" ]; then
            echo "  ОШИБКА: для снимков $TARGET_DIR нет примера в $EXAMPLES" >&2
            BAD=1
            continue
        fi
        OUT="$TMP/$(basename "$SECTION")-$TARGET"
        mkdir -p "$OUT"
        FLAG="$(tick_flag_of "$SRC")"
        # shellcheck disable=SC2086 - $FLAG либо пуст, либо один аргумент.
        if ! "$TAKTC" compile "$SRC" -t "$TARGET" $FLAG -o "$OUT" >"$OUT/.log" 2>&1; then
            echo "  ОШИБКА: $(basename "$SRC") целью '$TARGET' не компилируется:" >&2
            sed 's/^/    /' "$OUT/.log" | head -3 >&2
            BAD=1
            continue
        fi
        rm -f "$OUT/.log"
        CHECKED=$((CHECKED + 1))
        if [ "$UPDATE" -eq 1 ]; then
            cp "$OUT"/* "$TARGET_DIR"/
            echo "  обновлено: ${TARGET_DIR#"$ROOT"/}"
            continue
        fi
        if ! diff -r "$TARGET_DIR" "$OUT" >"$TMP/diff.txt" 2>&1; then
            echo "  ОШИБКА: снимок отстал от вывода: ${TARGET_DIR#"$ROOT"/}" >&2
            sed 's/^/    /' "$TMP/diff.txt" | head -8 >&2
            BAD=1
        fi
    done
done

if [ "$BAD" -ne 0 ]; then
    echo "  Снимки в book/ расходятся с выводом инструмента." >&2
    echo "  Приложение «Порождённый код примера» читает их напрямую — читатель" >&2
    echo "  видит код, которого компилятор не печатает." >&2
    echo "  Обновить: scripts/check-book-generated.sh --update" >&2
    exit 1
fi

NOTE="$(require_input "снимки целей в разделах документа" "$CHECKED" 1 "book/src")" || exit 1
if [ "$UPDATE" -eq 1 ]; then
    echo "  Снимки пересобраны: $NOTE."
else
    echo "  OK: снимки совпадают с выводом; $NOTE."
fi

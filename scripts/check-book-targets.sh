#!/bin/sh
# Проверка примеров документа book/ под инструментами целей.
#
# Правило: пример раздела - образец для читателя, и порождённый из него код
# обязан приниматься инструментом целевой платформы. Проверка 0133 проверяет
# компиляцию целью `c` и симуляцию, проверка 0274 сверяет снимки - но ни один не
# спрашивает `cc`, `iec2c`, `rustc`/`clippy` и `verilator`.
#
# Повод - не теория. Замер 2026-09-03 (33 примера x четыре цели) нашёл пять
# классов, каждый при нулевом коде возврата `taktc`: потерянная обёртка
# переполнения и лишнее приведение у `rust`, мнемоника варианта рядом с
# целым (0508, 0512), пустая ветвь `match`, вложенный `if`,
# ключевые слова IEC в именах. Проверки целей их не видели по устройству:
# они гоняют корпус `examples/`, а перечисленных форм там нет.
#
# Отказ самой цели (`SV-002`, `ST-011`, `CC-015`, ...) проверка не считает
# ошибкой: это названные границы цели, и пример вправе их задевать. Предмет
# проверки - "цель напечатала файл, а её инструмент этот файл отверг".
#
# Библиотечные примеры (импортируемые другими) пропускаются: у них нет
# верхнеуровневого входа, и проверяет их импортёр. Признак выводится грепом -
# тем же приёмом, что у проверки 0133.
#
# Использование:
# scripts/check-book-targets.sh # шаг precheck.sh
#
# Переменные: BT_ROOT - корень дерева (для теста), TAKTC - путь к компилятору.
# POSIX sh.
set -eu

ROOT="${BT_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
TAKTC="${TAKTC:-$ROOT/target/precheck/debug/taktc}"
IEC_LIB="${IEC2C_LIB:-$HOME/.local/share/matiec/lib}"
STRICT="${PRECHECK_STRICT:-0}"

if [ ! -x "$TAKTC" ]; then
    echo "check-book-targets: не найден компилятор $TAKTC" >&2
    exit 1
fi

echo "Гейт примеров book/ под инструментами целей (фича 0513)..."

have() { command -v "$1" >/dev/null 2>&1; }

# Инструмент отсутствует - шаг по нему пропускается (в CI это ошибка).
missing() {
    if [ "$STRICT" = "1" ]; then
        echo "  ОШИБКА: инструмент '$1' не найден, а PRECHECK_STRICT=1" >&2
        exit 1
    fi
    echo "  [пропуск] $1 не найден — цель '$2' не проверена"
}

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

BAD=0
CHECKED=0
SKIPPED=0

# Частота такта из объявления `clock` (контракт 0134): без неё цель `c`
# отвечает `SE-069`, и проверка выродилась бы в "пример не компилируется".
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

for SRC in $(find "$ROOT/book/src" -name '*.takt' | sort); do
    BASE="$(basename "$SRC")"
    NAME="${BASE%.takt}"
    if grep -rql "from \"$BASE\"" "$ROOT/book/src" --include='*.takt' 2>/dev/null; then
        continue
    fi
    TICK="$(tick_flag_of "$SRC")"

    for TARGET in c st rust sv; do
        DIR="$TMP/$NAME-$TARGET"
        mkdir -p "$DIR"
        # shellcheck disable=SC2086
        if ! "$TAKTC" compile "$SRC" -t "$TARGET" $TICK -o "$DIR" >"$DIR/gen.log" 2>&1; then
            # Отказ самой цели - законная граница, не предмет проверки.
            SKIPPED=$((SKIPPED + 1))
            continue
        fi
        CHECKED=$((CHECKED + 1))
        case "$TARGET" in
            c)
                have cc || { missing cc c; CHECKED=$((CHECKED - 1)); continue; }
                if ! cc -std=c11 -Wall -Wextra -Wno-unused-parameter -Werror \
                        -I "$DIR" -c "$DIR/$NAME.c" -o "$DIR/$NAME.o" 2>"$DIR/tool.log"; then
                    echo "  $BASE [c] → cc ОТВЕРГ:"
                    sed 's/^/    /' "$DIR/tool.log" | head -4
                    BAD=1
                fi
                ;;
            st)
                have iec2c || { missing iec2c st; CHECKED=$((CHECKED - 1)); continue; }
                mkdir -p "$DIR/iec"
                if ! iec2c -I "$IEC_LIB" -T "$DIR/iec" "$DIR/$NAME.st" >"$DIR/tool.log" 2>&1; then
                    echo "  $BASE [st] → iec2c ОТВЕРГ:"
                    sed 's/^/    /' "$DIR/tool.log" | head -4
                    BAD=1
                fi
                ;;
            rust)
                have clippy-driver || { missing clippy-driver rust; CHECKED=$((CHECKED - 1)); continue; }
                {
                    echo "#![no_std]"
                    echo "#[path = \"$DIR/$NAME.rs\"]"
                    echo "pub mod generated;"
                } > "$DIR/gate.rs"
                if ! clippy-driver --edition 2021 --crate-type=lib -D warnings \
                        "$DIR/gate.rs" --out-dir "$DIR/out" 2>"$DIR/tool.log"; then
                    echo "  $BASE [rust] → clippy ОТВЕРГ:"
                    sed 's/^/    /' "$DIR/tool.log" | head -6
                    BAD=1
                fi
                ;;
            sv)
                have verilator || { missing verilator sv; CHECKED=$((CHECKED - 1)); continue; }
                if ! verilator --lint-only -Wall "$DIR/$NAME.sv" >"$DIR/tool.log" 2>&1; then
                    echo "  $BASE [sv] → verilator ОТВЕРГ:"
                    sed 's/^/    /' "$DIR/tool.log" | head -6
                    BAD=1
                fi
                ;;
        esac
    done
done

echo "  Проверено пар пример×цель: $CHECKED; отказов самой цели (границы): $SKIPPED."
if [ "$BAD" -ne 0 ]; then
    echo "  Пример документа порождает код, который отвергает инструмент цели" \
         "— предкоммит провален (фича 0513)." >&2
    exit 1
fi

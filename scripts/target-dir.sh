#!/bin/sh
# Каталог отладочных бинарников - Один носитель знания.
#
# Повод: перенесла сборку в `target/precheck` через `.cargo/config.toml`,
# а `scripts/run_simulations.sh` продолжал искать бинарник в `target/debug`. Он
# сломался в тот же день и не был замечен, потому что его никто не запускает:
# в `precheck.sh` его нет. Инструмент, который никто не гоняет, неотличим от
# сломанного - поэтому знание о каталоге вынесено сюда, а не скопировано.
#
# Печатает каталог с отладочными бинарниками. Порядок поиска:
# 1) CARGO_TARGET_DIR - переменная окружения сильнее конфига;
# 2) [build] target-dir из.cargo/config.toml;
# 3) target - умолчание cargo.
#
# Использование: BIN_DIR="$("$(dirname "$0")/target-dir.sh")"
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

if [ -n "${CARGO_TARGET_DIR:-}" ]; then
    printf '%s/debug\n' "$CARGO_TARGET_DIR"
    exit 0
fi

CONFIG="$ROOT/.cargo/config.toml"
if [ -f "$CONFIG" ]; then
    DIR="$(grep -oE '^[[:space:]]*target-dir[[:space:]]*=[[:space:]]*"[^"]+"' "$CONFIG" \
        | head -n1 | sed 's/.*"\(.*\)"/\1/')"
    if [ -n "${DIR:-}" ]; then
        case "$DIR" in
            /*) printf '%s/debug\n' "$DIR" ;;
            *)  printf '%s/%s/debug\n' "$ROOT" "$DIR" ;;
        esac
        exit 0
    fi
fi

printf '%s/target/debug\n' "$ROOT"

#!/usr/bin/env bash
# Проверка модуля WebAssembly: сборка под `wasm32-unknown-unknown` и
# сверка вывода модуля с инструментами (`check-wasm-identity.mjs`).
#
# Что доказывает: то, что показывает браузер, равно тому, что печатают `taktc`
# и `takt-sim`. Одной сборки мало - модуль собирается и тогда, когда мост теряет
# хвост файла или строку трассы: такой вывод остаётся валидным и оказывается
# Другим.
#
# Политика внешних инструментов - как у ST-арбитра: нет `node` или
# не установлен таргет - мягкий пропуск; под `PRECHECK_STRICT=1` это ошибка.
# Причина: предкоммит обязан идти на машине без веб-оснастки, а CI обязан
# требовать полноты.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

STRICT="${PRECHECK_STRICT:-0}"
# `target-dir.sh` печатает каталог отладочных бинарников (`.../debug`), а модуль
# лежит рядом - в каталоге своего профиля под таргетом. Корень берётся отсечением
# последнего сегмента: знание о каталоге сборки остаётся у одного носителя
# а профиль знает этот проверка.
BIN_DIR="$("$(dirname "${BASH_SOURCE[0]}")/target-dir.sh")"
TARGET_DIR="$(dirname "$BIN_DIR")"
# Профиль модуля: свой, а не общий `release` (см. `[profile.wasm]` корневого
# Cargo.toml) - браузеру важен размер, а `taktc` собирается для машины.
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

echo "Гейт модуля WebAssembly (фича 0531)..."

# Имя интерпретатора - переменная: так тест проверки может проверить политику
# "нет инструмента" (мягкий пропуск против ошибки под PRECHECK_STRICT), не
# вычищая PATH целиком, - иначе вместе с `node` исчезли бы `bash` и `cargo`.
NODE="${TAKT_NODE:-node}"
command -v "$NODE" >/dev/null 2>&1 || skip_or_fail "не найден node"
rustup target list --installed 2>/dev/null | grep -qx "wasm32-unknown-unknown" \
  || skip_or_fail "не установлен таргет wasm32-unknown-unknown (rustup target add wasm32-unknown-unknown)"

CARGO_CMD="${CARGO_CMD:-cargo}"
$CARGO_CMD build -p takt-wasm --profile "$PROFILE" --target wasm32-unknown-unknown

[[ -f "$WASM" ]] || {
  echo "  ОШИБКА: модуль не собран: $WASM"
  exit 1
}
size_kib=$(( $(wc -c < "$WASM") / 1024 ))
echo "  модуль собран: $WASM (${size_kib} КиБ)"

TAKTC="${TAKTC:-$BIN_DIR/taktc}"
TAKT_SIM="${TAKT_SIM:-$BIN_DIR/takt-sim}"
for tool in "$TAKTC" "$TAKT_SIM"; do
  [[ -x "$tool" ]] || {
    echo "  ОШИБКА: не найден инструмент сверки: $tool"
    exit 1
  }
done

"$NODE" "$ROOT/scripts/check-wasm-identity.mjs" "$WASM" "$TAKTC" "$TAKT_SIM"

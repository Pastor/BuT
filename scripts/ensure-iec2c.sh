#!/usr/bin/env bash
# Обеспечивает наличие MatIEC `iec2c` - арбитра валидности порождённого
# Structured Text.
#
# Если локальной сборки нет - собирает из исходников и устанавливает в
# ~/.local (бинарник -> ~/.local/bin/iec2c). Если есть - ничего не делает.
#
# Скрипт не провальный: при любой беде (нет сети, нет bison, не собралось) он
# печатает причину и выходит с кодом 0. Причина - `iec2c` нужен только для
# проверки порождённого ST, а не для сборки и тестов `taktc`; ронять предкоммит
# из-за отсутствия внешнего инструмента нельзя.
#
# Использование:
# scripts/ensure-iec2c.sh # проверить и при нужде собрать
# IEC2C_SKIP=1 scripts/ensure-iec2c.sh # пропустить (для CI/оффлайна)
#
# Выход: 0 всегда. Признак успеха - существование "$IEC2C_BIN".

set -uo pipefail

PREFIX="${IEC2C_PREFIX:-$HOME/.local}"
IEC2C_BIN="$PREFIX/bin/iec2c"
# `iec2c` требует библиотеку стандартных функций (`ieclib.txt`) и по умолчанию
# ищет её в `./lib` относительно текущего каталога. `make install` MatIEC ставит
# Только бинарники (`install-data-am` пуст), поэтому библиотеку кладём сами и
# указываем путь через `-I`.
IEC2C_LIB="$PREFIX/share/matiec/lib"
SRC_DIR="${IEC2C_SRC:-$HOME/.cache/matiec}"
REPO="https://github.com/beremiz/matiec.git"

say() { echo "  $*"; }

if [ "${IEC2C_SKIP:-0}" = "1" ]; then
  say "[пропуск] IEC2C_SKIP=1 — проверка ST-арбитра отключена"
  exit 0
fi

# Установка считается пригодной, только если есть И бинарник, И библиотека:
# без `ieclib.txt` iec2c падает на любом входе ("Error opening library file").
if [ -x "$IEC2C_BIN" ] && [ -f "$IEC2C_LIB/ieclib.txt" ]; then
  say "iec2c: $IEC2C_BIN (библиотека: $IEC2C_LIB)"
  exit 0
fi

if [ -x "$IEC2C_BIN" ] && [ ! -f "$IEC2C_LIB/ieclib.txt" ]; then
  say "[внимание] $IEC2C_BIN есть, но библиотеки нет ($IEC2C_LIB) — пересобираю"
fi

echo "MatIEC iec2c не найден — сборка из исходников (нужен один раз)..."

# -- Предпосылки -------------------------------------------------------------
# bison >= 2.4: у MatIEC это жёсткое требование `configure`. Системный bison
# macOS - 2.3, поэтому homebrew-версия (если есть) ставится в начало PATH.
if [ -d /opt/homebrew/opt/bison/bin ]; then
  export PATH="/opt/homebrew/opt/bison/bin:$PATH"
elif [ -d /usr/local/opt/bison/bin ]; then
  export PATH="/usr/local/opt/bison/bin:$PATH"
fi

for tool in git autoreconf bison flex make; do
  if ! command -v "$tool" &>/dev/null; then
    say "[пропуск] нет '$tool' — iec2c не собран; проверка ST будет пропущена"
    say "          macOS: brew install git autoconf automake libtool bison flex"
    exit 0
  fi
done

BISON_VER="$(bison --version | head -1 | grep -oE '[0-9]+\.[0-9]+' | head -1)"
if [ -z "$BISON_VER" ]; then
  # Версию не разобрать - не гадаем. Пусть решает configure: он всё равно
  # проверяет bison сам и внятно ругается, а сборка у нас не фатальна.
  say "[внимание] не удалось определить версию bison — полагаюсь на configure"
  BISON_VER="0.0"
fi
BISON_MAJOR="${BISON_VER%%.*}"
BISON_MINOR="${BISON_VER##*.}"
if [ "$BISON_VER" != "0.0" ] &&
   { [ "$BISON_MAJOR" -lt 2 ] || { [ "$BISON_MAJOR" -eq 2 ] && [ "$BISON_MINOR" -lt 4 ]; }; }; then
  say "[пропуск] bison $BISON_VER < 2.4 — MatIEC не соберётся"
  say "          macOS: brew install bison && export PATH=/opt/homebrew/opt/bison/bin:\$PATH"
  exit 0
fi

# -- Исходники ---------------------------------------------------------------
if [ -d "$SRC_DIR/.git" ]; then
  say "исходники: $SRC_DIR (уже склонированы)"
else
  say "клонирую $REPO → $SRC_DIR"
  mkdir -p "$(dirname "$SRC_DIR")"
  if ! git clone --depth 1 "$REPO" "$SRC_DIR" >/dev/null 2>&1; then
    say "[пропуск] не удалось склонировать MatIEC (нет сети?)"
    exit 0
  fi
fi

# -- Сборка ------------------------------------------------------------------
LOG="${TMPDIR:-/tmp}/matiec-build.log"
(
  cd "$SRC_DIR" || exit 1
  [ -f configure ] || autoreconf -i
  # --prefix даёт `make install` -> $PREFIX/bin/iec2c
  ./configure --prefix="$PREFIX" && make -j"$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)"
) >"$LOG" 2>&1

if [ ! -x "$SRC_DIR/iec2c" ]; then
  say "[пропуск] сборка MatIEC не удалась — лог: $LOG"
  say "          $(tail -1 "$LOG" 2>/dev/null)"
  exit 0
fi

# -- Установка ---------------------------------------------------------------
mkdir -p "$PREFIX/bin" "$PREFIX/share/matiec"
if ! (cd "$SRC_DIR" && make install >>"$LOG" 2>&1); then
  say "[пропуск] make install не удался — лог: $LOG"
  exit 0
fi
# Библиотека standard-функций: `make install` её не ставит, копируем вручную.
# Каталог `lib/C` тоже нужен - на него смотрит MATIEC_C_INCLUDE_PATH при сборке
# порождённого C.
rm -rf "$IEC2C_LIB"
cp -R "$SRC_DIR/lib" "$PREFIX/share/matiec/"

if [ -x "$IEC2C_BIN" ] && [ -f "$IEC2C_LIB/ieclib.txt" ]; then
  say "установлен: $IEC2C_BIN"
  say "библиотека: $IEC2C_LIB"
  case ":$PATH:" in
    *":$PREFIX/bin:"*) ;;
    *) say "[совет] добавьте в PATH: export PATH=\"$PREFIX/bin:\$PATH\"" ;;
  esac
else
  say "[пропуск] установка не завершилась — лог: $LOG"
fi

exit 0

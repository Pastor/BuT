#!/usr/bin/env bash
# Собирает релизные бинарники Takt и устанавливает их.
#
# Собираются три инструмента:
# taktc - компилятор (takt-lang)
# takt-lsp - языковой сервер (takt-lang, фича `lsp`)
# takt-sim - симулятор (takt-sim)
#
# Использование:
# scripts/install.sh # собрать и поставить в ~/.local/bin
#   scripts/install.sh --prefix /usr/local # другой преисправление (bin/ внутри него)
# scripts/install.sh --no-lsp # без языкового сервера
# scripts/install.sh --build-only # только собрать, не устанавливать
# scripts/install.sh --check # проверить установленное и выйти
# scripts/install.sh --dry-run # показать, что было бы сделано
#
# Переменные окружения:
# TAKT_PREFIX преисправление установки (по умолчанию ~/.local)
# CARGO_TARGET_DIR каталог сборки (по умолчанию <репозиторий>/target/install)
#
# Каталог сборки по умолчанию - Свой (`target/install`), а не общий `target`.
# Причина измерена: накопленный рабочий каталог (десятки гигабайт,
# сотни тысяч файлов в `debug/deps`) превращает минутную сборку в часы, потому
# что cargo обходит его перед каждой единицей компиляции. Скрипт, который может
# зависнуть на чужом мусоре, бесполезен; свой каталог - свойство инструмента, а
# не дисциплина автора.
#
# Код возврата: 0 - успех, иначе - причина напечатана.

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

PREFIX="${TAKT_PREFIX:-$HOME/.local}"
WITH_LSP=1
INSTALL=1
DRY_RUN=0
CHECK_ONLY=0

usage() {
    sed -n '2,26p' "$0" | sed 's/^# \{0,1\}//'
}

while [ $# -gt 0 ]; do
    case "$1" in
        --prefix) PREFIX="${2:?--prefix требует каталог}"; shift 2 ;;
        --prefix=*) PREFIX="${1#*=}"; shift ;;
        --no-lsp) WITH_LSP=0; shift ;;
        --build-only) INSTALL=0; shift ;;
        --check) CHECK_ONLY=1; shift ;;
        --dry-run) DRY_RUN=1; shift ;;
        -h|--help) usage; exit 0 ;;
        *) echo "install: неизвестный аргумент: $1 (см. --help)" >&2; exit 2 ;;
    esac
done

BIN_DIR="$PREFIX/bin"

# Инструменты: имя бинарника | крейт | дополнительные флаги cargo.
# `takt-lsp` собирается только с фичей `lsp` - без неё бинарника не существует.
tools() {
    printf '%s\n' \
        "taktc|takt-lang|" \
        "takt-sim|takt-sim|"
    if [ "$WITH_LSP" -eq 1 ]; then
        printf '%s\n' "takt-lsp|takt-lang|--features lsp"
    fi
}

# -- Проверка установленного (--check) ----------------------------------------
if [ "$CHECK_ONLY" -eq 1 ]; then
    missing=0
    while IFS='|' read -r bin _crate _flags; do
        path="$BIN_DIR/$bin"
        if [ -x "$path" ]; then
            printf '  %-9s %s\n' "$bin" "$path"
        else
            printf '  %-9s НЕ УСТАНОВЛЕН (%s)\n' "$bin" "$path"
            missing=1
        fi
    done <<EOF
$(tools)
EOF
    if [ "$missing" -eq 1 ]; then
        echo "install --check: установлены не все инструменты" >&2
        exit 1
    fi
    echo "install --check: все инструменты на месте ($BIN_DIR)"
    exit 0
fi

# -- Требования ---------------------------------------------------------------
if ! command -v cargo >/dev/null 2>&1; then
    echo "install: не найден cargo. Поставьте Rust: https://rustup.rs" >&2
    exit 1
fi

# Каталог сборки: свой, если вызывающий не назвал другой (см. врезку выше).
export CARGO_TARGET_DIR="${CARGO_TARGET_DIR:-$ROOT/target/install}"

run() {
    if [ "$DRY_RUN" -eq 1 ]; then
        echo "  [dry-run] $*"
    else
        "$@"
    fi
}

echo "Сборка релизных бинарников Takt"
echo "  каталог сборки: $CARGO_TARGET_DIR"
echo "  толчейн:        $(cargo --version)"

# -- Сборка -------------------------------------------------------------------
while IFS='|' read -r bin crate flags; do
    echo "  собираю $bin ($crate)${flags:+ [$flags]}"
    # shellcheck disable=SC2086 # $flags - намеренно несколько аргументов
    run cargo build --release --manifest-path "$ROOT/$crate/Cargo.toml" \
        --bin "$bin" $flags
done <<EOF
$(tools)
EOF

if [ "$INSTALL" -eq 0 ]; then
    echo "Готово (--build-only): бинарники в $CARGO_TARGET_DIR/release"
    exit 0
fi

# -- Установка ----------------------------------------------------------------
run mkdir -p "$BIN_DIR"

while IFS='|' read -r bin _crate _flags; do
    src="$CARGO_TARGET_DIR/release/$bin"
    if [ "$DRY_RUN" -eq 0 ] && [ ! -x "$src" ]; then
        echo "install: сборка не дала $src" >&2
        exit 1
    fi
    # `install` затирает работающий бинарник безопаснее, чем `cp` (замена
    # inode вместо записи поверх), и сразу ставит права.
    run install -m 0755 "$src" "$BIN_DIR/$bin"
    echo "  установлен $bin → $BIN_DIR/$bin"
done <<EOF
$(tools)
EOF

echo "Готово: инструменты в $BIN_DIR"

# Предупреждение о PATH - не украшение: молча установленный инструмент,
# которого не видно в оболочке, выглядит как несработавший скрипт.
case ":$PATH:" in
    *":$BIN_DIR:"*) ;;
    *)
        echo
        echo "⚠️  $BIN_DIR отсутствует в PATH. Добавьте в ~/.zshrc (или ~/.bashrc):"
        echo "      export PATH=\"$BIN_DIR:\$PATH\""
        ;;
esac

# Признак того, что установлено именно собранное сейчас: время файла и размер.
# Версию у `taktc` не спрашиваем: подкоманды `version` у него ещё нет
# (незакрытая ), а `--version` он отвергает как неизвестную команду.
if [ "$DRY_RUN" -eq 0 ]; then
    echo
    echo "Установлено:"
    while IFS='|' read -r bin _crate _flags; do
        ls -lh "$BIN_DIR/$bin" | awk '{ printf "  %-9s %6s  %s %s %s\n", "'"$bin"'", $5, $6, $7, $8 }'
    done <<EOF
$(tools)
EOF
fi

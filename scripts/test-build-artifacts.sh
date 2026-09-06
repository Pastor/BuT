#!/bin/sh
# Тест проверки артефактов сборки.
#
# Проверка проверяет отслеживаемые файлы, поэтому тест строит временный
# репозиторий: копия рабочего дерева тут не годится - нужен свой индекс.
#
# Условий три (: "плохое ловится" и "хорошее проходит", а
# вырожденный вход обязан давать отказ, а не рапорт об успехе).
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-build-artifacts.py"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

fail() { printf '  ОШИБКА: %s\n' "$1" >&2; exit 1; }

git -C "$WORK" init -q
git -C "$WORK" config user.email t@e.st
git -C "$WORK" config user.name test
printf 'fn main() {}\n' > "$WORK/main.rs"
git -C "$WORK" add main.rs
git -C "$WORK" commit -qm init

# 1. Чистый репозиторий принимается.
BA_ROOT="$WORK" python3 "$GATE" >/dev/null || fail "чистый репозиторий обязан приниматься"
echo "  OK: чистый репозиторий принимается"

# 2. Отслеживаемый артефакт ловится и называется.
printf 'binary\n' > "$WORK/libprobe.rlib"
git -C "$WORK" add -f libprobe.rlib
OUT="$(BA_ROOT="$WORK" python3 "$GATE" 2>&1)" && fail "артефакт обязан ловиться"
printf '%s' "$OUT" | grep -q 'libprobe.rlib' || fail "отказ обязан назвать файл"
echo "  OK: отслеживаемый артефакт ловится и назван"

# 3. НЕотслеживаемый артефакт проверка не роняет: его отсекает.gitignore.
git -C "$WORK" rm -q --cached libprobe.rlib
BA_ROOT="$WORK" python3 "$GATE" >/dev/null || fail "неотслеживаемый файл ронять гейт не должен"
echo "  OK: неотслеживаемый артефакт гейт не роняет"

# 4. Каталог без репозитория - Отказ, а не рапорт об успехе.
EMPTY="$WORK/empty"
mkdir -p "$EMPTY"
BA_ROOT="$EMPTY" python3 "$GATE" >/dev/null 2>&1 && fail "не-репозиторий обязан давать отказ"
echo "  OK: вырожденный вход даёт отказ"

echo "  Сторож гейта артефактов сборки: все проверки пройдены"

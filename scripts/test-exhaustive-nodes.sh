#!/bin/sh
# Тест проверки исчерпаемости разборов.
#
# Проверка `check-exhaustive-nodes.sh` требует атрибута
# `deny(clippy::wildcard_enum_match_arm)` у модулей, разбирающих узлы языка
# . Проверялось это ничем: проверка, который никогда не падал,
# неотличим от проверки, который не смотрит - а у самого проверки дыра
# уже случалась: он искал атрибут не с начала строки и находил его в
# док-комментарии.
#
# Работает на копии дерева (`EN_ROOT`), рабочие файлы не трогаются.
#
# POSIX sh.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TOOL="$ROOT/scripts/check-exhaustive-nodes.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

echo "Сторож гейта исчерпаемости разборов (фича 0315)..."

# Копия только нужных файлов: проверка читает пять путей.
copy_tree() {
    rm -rf "$TMP/tree"
    for rel in takt-lang/src/semantic/mod.rs \
               takt-sim/src/eval/mod.rs \
               takt-sim/src/unit/initial.rs \
               takt-lang/src/parser/depth/children.rs \
               takt-lang/src/parser/depth/dismantle.rs; do
        mkdir -p "$TMP/tree/$(dirname "$rel")"
        cp "$ROOT/$rel" "$TMP/tree/$rel"
    done
}

# --- 1. Здоровое дерево принимается ------------------------------------------
copy_tree
if EN_ROOT="$TMP/tree" sh "$TOOL" >"$TMP/out" 2>&1; then
    ok "здоровое дерево принимается"
else
    fail "здоровое дерево отвергнуто: $(cat "$TMP/out")"
fi

# --- 2. Снятый атрибут ловится -----------------------------------------------
# Это и есть предмет правила: модуль без `deny` снова пропустит ветку `_` по
# узлу языка, а такая ветка молча не исполняет конструкцию.
copy_tree
sed -i.bak 's/^#!\[deny(clippy::wildcard_enum_match_arm)\]//' "$TMP/tree/takt-sim/src/eval/mod.rs"
sed -i.bak 's/^#!\[deny(clippy::wildcard_enum_match_arm)\]//' "$TMP/tree/takt-sim/src/unit/initial.rs"
if ! EN_ROOT="$TMP/tree" sh "$TOOL" >"$TMP/out" 2>&1; then
    ok "снятый атрибут ловится"
else
    fail "снятый атрибут НЕ пойман: $(cat "$TMP/out")"
fi

# --- 3. Атрибут в комментарии не считается ------------------------------------
# Ровно этой дырой проверка болел до: искал подстроку где угодно в
# строке и находил её в док-комментарии.
copy_tree
sed -i.bak 's|^#!\[deny(clippy::wildcard_enum_match_arm)\]|// #![deny(clippy::wildcard_enum_match_arm)]|' \
    "$TMP/tree/takt-sim/src/eval/mod.rs"
if ! EN_ROOT="$TMP/tree" sh "$TOOL" >"$TMP/out" 2>&1; then
    ok "атрибут в комментарии не засчитывается"
else
    fail "комментарий принят за атрибут: $(cat "$TMP/out")"
fi

# --- 4. Пропавший файл - отказ, а не молчание ---------------------------------
copy_tree
rm "$TMP/tree/takt-lang/src/parser/depth/children.rs"
if ! EN_ROOT="$TMP/tree" sh "$TOOL" >"$TMP/out" 2>&1; then
    ok "пропавший файл ловится"
else
    fail "пропавший файл принят за успех: $(cat "$TMP/out")"
fi

[ "$FAILED" -eq 0 ] || { echo "  Сторож гейта исчерпаемости: ПРОВАЛ" >&2; exit 1; }
echo "  Сторож гейта исчерпаемости: все проверки пройдены"

#!/bin/sh
# Тест проверки конфига Zed (правило "у каждого проверки есть тест",
# правилу проекта о контроле у каждой проверки.
#
# Проверяет предмет проверки, а не факт запуска: каждая из четырёх проверок
# обязана ловить своё нарушение, а согласованное дерево - проходить. Проверка
# гоняется на копии дерева (переменная корня `ZC_ROOT`), поэтому рабочее
# дерево не трогается.
set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

copy_tree() {
    mkdir -p "$1/extensions/zed-takt/languages/takt" "$1/takt-lang/src/parser" "$1/scripts"
    cp "$ROOT/extensions/zed-takt/languages/takt/config.toml" "$1/extensions/zed-takt/languages/takt/"
    cp "$ROOT/extensions/zed-takt/extension.toml" "$1/extensions/zed-takt/"
    cp "$ROOT/takt-lang/src/parser/lexer.rs" "$1/takt-lang/src/parser/"
    cp "$ROOT/scripts/check-zed-config.py" "$ROOT/scripts/gatelib.py" "$1/scripts/"
}

run_gate() {
    (cd "$1" && python3 scripts/check-zed-config.py >/dev/null 2>&1)
}

# 1. Согласованное дерево принимается.
GOOD="$TMP/good"
copy_tree "$GOOD"
if run_gate "$GOOD"; then
    echo "  OK: согласованный конфиг принимается"
else
    echo "  ПРОВАЛ: согласованный конфиг отвергнут"
    exit 1
fi

# 2. Пропавший суффикс ловится (Z1).
CASE="$TMP/z1"
copy_tree "$CASE"
sed -i.bak 's/path_suffixes = \["takt"\]/path_suffixes = ["other"]/' \
    "$CASE/extensions/zed-takt/languages/takt/config.toml"
if run_gate "$CASE"; then
    echo "  ПРОВАЛ: пропавший суффикс не пойман (Z1)"
    exit 1
fi
echo "  OK: пропавший суффикс ловится (Z1)"

# 3. Пропавший doc-комментарий ловится (Z2).
CASE="$TMP/z2"
copy_tree "$CASE"
sed -i.bak 's|line_comments = \["// ", "/// "\]|line_comments = ["// "]|' \
    "$CASE/extensions/zed-takt/languages/takt/config.toml"
if run_gate "$CASE"; then
    echo "  ПРОВАЛ: пропавший doc-комментарий не пойман (Z2)"
    exit 1
fi
echo "  OK: пропавший doc-комментарий ловится (Z2)"

# 4. Пропавшая пара скобок ловится (Z3).
CASE="$TMP/z3"
copy_tree "$CASE"
grep -v '{ start = "\[", end = "\]"' \
    "$ROOT/extensions/zed-takt/languages/takt/config.toml" \
    > "$CASE/extensions/zed-takt/languages/takt/config.toml"
if run_gate "$CASE"; then
    echo "  ПРОВАЛ: пропавшая пара скобок не поймана (Z3)"
    exit 1
fi
echo "  OK: пропавшая пара скобок ловится (Z3)"

# 5. Пропавший языковой сервер ловится (Z4).
CASE="$TMP/z4"
copy_tree "$CASE"
sed -i.bak 's/language_servers = \["takt-lsp"\]/language_servers = ["none"]/' \
    "$CASE/extensions/zed-takt/languages/takt/config.toml"
if run_gate "$CASE"; then
    echo "  ПРОВАЛ: пропавший языковой сервер не пойман (Z4)"
    exit 1
fi
echo "  OK: пропавший языковой сервер ловится (Z4)"

# 6. Пропавший блочный комментарий ловится (Z5).
CASE="$TMP/z5"
copy_tree "$CASE"
grep -v '^block_comment' "$ROOT/extensions/zed-takt/languages/takt/config.toml" \
    > "$CASE/extensions/zed-takt/languages/takt/config.toml"
if run_gate "$CASE"; then
    echo "  ПРОВАЛ: пропавший блочный комментарий не пойман (Z5)"
    exit 1
fi
echo "  OK: пропавший блочный комментарий ловится (Z5)"

echo "Сторож гейта конфига Zed: все проверки пройдены"

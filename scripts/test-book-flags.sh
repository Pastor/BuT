#!/bin/sh
# Тест проверки ключей сборки.
#
# Проверяются оба направления: документ, называющий все ключи, принимается, а
# пропущенный ключ ловится и назван. Отдельно - протухшая запись долга и
# вырожденный вход: дерево, где разбор аргументов не найден, обязано ронять
# прогон, иначе проверка молча начнёт проверять пустоту.
#
# Прогон идёт на копии дерева (`BF_ROOT`): рабочие исходники не трогаются.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-book-flags.py"

echo "Сторож гейта ключей сборки (фича 0527)..."

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

TREE="$TMP/tree"
mkdir -p "$TREE/takt-lang/src/compile_cli" "$TREE/book/src/01-probe" "$TREE/scripts"
BASE="$TREE/scripts/book-flags-baseline.txt"

# Дерево-проба: разбор двух ключей и раздел, называющий их.
cat > "$TREE/takt-lang/src/compile_cli/mod.rs" <<'RS'
match arg {
    "--probe-one" => one = true,
    a if a.starts_with("--probe-two=") => two = parse(a),
    _ => {}
}
RS

chapter() {
    cat > "$TREE/book/src/01-probe/index.typ" <<CHAPTER
= Проба

Ключи сборки: $1
CHAPTER
}

run_gate() {
    BF_ROOT="$TREE" python3 "$GATE" >"$TMP/log" 2>&1
}

# 1. Все ключи названы - принимается.
chapter '\`--probe-one\` и \`--probe-two=x\`'
: > "$BASE"
if run_gate; then
    echo "  OK: документ, называющий все ключи, принимается"
else
    echo "  ОШИБКА: гейт отверг документ, где названы все ключи:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
fi

# 2. Пропущенный ключ ловится и назван.
chapter '\`--probe-one\`'
if run_gate; then
    echo "  ОШИБКА: гейт принял документ без одного ключа" >&2
    exit 1
fi
grep -q -- "--probe-two" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал пропущенный ключ:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: пропущенный ключ ловится и назван"

# 3. Протухшая запись долга ловится.
chapter '\`--probe-one\` и \`--probe-two=x\`'
echo "--probe-two | запись долга, которой не место" > "$BASE"
if run_gate; then
    echo "  ОШИБКА: гейт принял долг, который назван в документе" >&2
    exit 1
fi
grep -q "протухла" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал протухшую запись:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: протухшая запись долга ловится"

# 4. Дерево без разбора аргументов - отказ, а не успех.
: > "$BASE"
printf 'fn main() {}\n' > "$TREE/takt-lang/src/compile_cli/mod.rs"
if run_gate; then
    echo "  ОШИБКА: гейт принял дерево без единого ключа" >&2
    exit 1
fi
echo "  OK: дерево без ключей отвергнуто"

echo "  Сторож гейта ключей сборки пройден."

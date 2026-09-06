#!/bin/sh
# Тест проверки чисел README.
#
# Проверяются оба направления: согласованный README принимается, а каждое из
# трёх расхождений ловится - съехавшее число наборов, съехавшее число разделов
# и тема, которой в дереве нет. Отдельно - вырожденный вход: README без
# проверяемых утверждений обязан ронять прогон, иначе смена разметки превратит
# проверка в тривиальный успех.
#
# Прогон идёт на копии дерева (`RN_ROOT`): рабочий README не трогается.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-readme-numbers.py"

echo "Сторож гейта чисел README (фича 0526)..."

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

TREE="$TMP/tree"
mkdir -p "$TREE/probe/tests/sim" "$TREE/book/src"

# Дерево-проба: одна тема с двумя наборами и документ из трёх глав.
printf 'mod one_tests;\nmod two_tests;\n' > "$TREE/probe/tests/sim/main.rs"
printf '#include "01-a/index.typ"\n#include "02-b/index.typ"\n#include "03-c/index.typ"\n#include "appendix-x/index.typ"\n' \
    > "$TREE/book/src/main.typ"

readme() {
    cat > "$TREE/README.md" <<TEXT
# Проба

> Описание языка — в документе (${2} раздела и приложения, включая грамматику).

| Крейт | Цель | Что внутри | Наборов |
|---|---|---|---|
| \`${3:-probe}\` | \`sim\` | наборы пробы | ${1} |
TEXT
}

run_gate() {
    RN_ROOT="$TREE" python3 "$GATE" >"$TMP/log" 2>&1
}

# 1. Согласованный README принимается.
readme 2 3
if run_gate; then
    echo "  OK: согласованный README принимается"
else
    echo "  ОШИБКА: гейт отверг числа, совпадающие с деревом:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
fi

# 2. Съехавшее число наборов ловится.
readme 9 3
if run_gate; then
    echo "  ОШИБКА: гейт принял чужое число наборов" >&2
    exit 1
fi
grep -q "наборов" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал расхождение:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: съехавшее число наборов ловится"

# 3. Съехавшее число разделов ловится.
readme 2 9
if run_gate; then
    echo "  ОШИБКА: гейт принял чужое число разделов" >&2
    exit 1
fi
grep -q "разделов документа" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал число разделов:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: съехавшее число разделов ловится"

# 4. Тема, которой нет в дереве, ловится.
readme 2 3 ghost
if run_gate; then
    echo "  ОШИБКА: гейт принял тему, которой нет в дереве" >&2
    exit 1
fi
grep -q "нет в дереве" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал пропавшую тему:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: несуществующая тема ловится"

# 5. README без проверяемых утверждений - отказ, а не успех.
printf '# Проба\n\nЧисел здесь нет вовсе.\n' > "$TREE/README.md"
if run_gate; then
    echo "  ОШИБКА: гейт принял README без проверяемых чисел" >&2
    exit 1
fi
echo "  OK: README без проверяемых чисел отвергнут"

echo "  Сторож гейта чисел README пройден."

#!/bin/sh
# Тест проверки разбора примеров в главах.
#
# Проверка без проверенной ловушки доказывает лишь свой запуск. Здесь мутацией
# проверяются все четыре отказа - неразбираемый пример, обещание кода, которого
# инструмент не даёт, протухшая запись долга, вырожденное дерево, - и обратное
# условие: согласованная глава принимается.
#
# Прогон идёт на копии дерева (`BCE_ROOT`): рабочие главы не трогаются.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-book-chapter-examples.py"
TAKTC="${TAKTC:-$ROOT/target/precheck/debug/taktc}"

if [ ! -x "$TAKTC" ]; then
    echo "test-book-chapter-examples: не найден компилятор $TAKTC" >&2
    exit 1
fi

echo "Сторож гейта разбора примеров в главах (фича 0521)..."

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

TREE="$TMP/tree"
mkdir -p "$TREE/book/src/01-probe" "$TREE/scripts"
BASE="$TREE/scripts/book-chapter-examples-baseline.txt"

chapter() {
    cat > "$TREE/book/src/01-probe/index.typ" <<CHAPTER
= Проба

Пример раздела:
\`\`\`takt
$1
\`\`\`
CHAPTER
}

run_gate() {
    BCE_ROOT="$TREE" TAKTC="$TAKTC" python3 "$GATE" >"$TMP/log" 2>&1
}

# 1. Согласованная глава принимается.
chapter 'var speed: u8 := 0;'
: > "$BASE"
if run_gate; then
    echo "  OK: разбираемый пример принимается"
else
    echo "  ОШИБКА: гейт отверг пример, который разбирается:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
fi

# 2. Неразбираемый пример ловится (`next` - ключевое слово, именем быть не может).
chapter 'fn next(x: u8) -> u8 { return x + 1; }'
if run_gate; then
    echo "  ОШИБКА: гейт принял пример, который не разбирается" >&2
    exit 1
fi
grep -q "не разбирается" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал причину:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: неразбираемый пример ловится"

# 3. Обещание кода, которого инструмент не даёт, ловится.
chapter 'var speed: u8 := 0;   // ошибка SE-999: такого кода нет'
if run_gate; then
    echo "  ОШИБКА: гейт принял обещание несуществующего кода" >&2
    exit 1
fi
grep -q "SE-999" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал обещанный код:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: обещание кода, которого нет, ловится"

# 4. Протухшая запись долга ловится.
chapter 'var speed: u8 := 0;'
echo "01-probe/index.typ#1 | запись долга, которой не место" > "$BASE"
if run_gate; then
    echo "  ОШИБКА: гейт принял долг, который проходит" >&2
    exit 1
fi
grep -q "протухла" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал протухшую запись:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: протухшая запись долга ловится"

# 5. Дерево без блоков - отказ, а не тривиальный успех.
: > "$BASE"
cat > "$TREE/book/src/01-probe/index.typ" <<'EMPTY'
= Проба

Примеров здесь нет вовсе.
EMPTY
if run_gate; then
    echo "  ОШИБКА: гейт принял главы без единого примера" >&2
    exit 1
fi
echo "  OK: главы без примеров отвергнуты"

echo "  Сторож гейта разбора примеров в главах пройден."

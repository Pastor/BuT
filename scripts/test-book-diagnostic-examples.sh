#!/bin/sh
# Тест проверки воспроизводимости примеров приложения.
#
# Проверка без теста доказывает лишь то, что он запустился. Здесь проверяются оба
# условия правила: "согласованное приложение принимается" и "расхождение
# ловится", причём расхождений три вида - обещанный код не приходит, позиция в
# цитате разошлась с фактической, запись долга протухла. Отдельно проверяется
# вырожденный вход: приложение без единой пары обязано ронять прогон, иначе при
# смене разметки проверка молча начнёт проверять пустоту.
#
# Прогон идёт на копии дерева (`BDE_ROOT`): рабочее приложение не трогается.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-book-diagnostic-examples.py"
TAKTC="${TAKTC:-$ROOT/target/precheck/debug/taktc}"

if [ ! -x "$TAKTC" ]; then
    echo "test-book-diagnostic-examples: не найден компилятор $TAKTC" >&2
    exit 1
fi

echo "Сторож гейта воспроизводимости примеров приложения (фича 0520)..."

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

TREE="$TMP/tree"
mkdir -p "$TREE/book/src/appendix-errors" "$TREE/scripts"

# Копия дерева - одна пара: тесту нужен предмет, а не объём приложения.
write_appendix() {
    cat > "$TREE/book/src/appendix-errors/index.typ" <<APPENDIX
= Ошибки и предупреждения

=== \`SE-034\` — неизвестный тип
Тип с таким именем языку неизвестен.
\`\`\`takt
var x: u9 := 0;
start S;
\`\`\`
\`\`\`text
model.takt:1:8: Ошибка компиляции [$1]: Локальный тип 'u9' не найден
\`\`\`
APPENDIX
}

run_gate() {
    BDE_ROOT="$TREE" TAKTC="$TAKTC" python3 "$GATE" >"$TMP/log" 2>&1
}

# 1. Согласованное приложение принимается.
write_appendix "SE-034"
: > "$TREE/scripts/book-diagnostic-examples-baseline.txt"
if run_gate; then
    echo "  OK: согласованный разбор принимается"
else
    echo "  ОШИБКА: гейт отверг разбор, который воспроизводится:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
fi

# 2. Обещанный код не приходит - ловится.
write_appendix "SE-999"
if run_gate; then
    echo "  ОШИБКА: гейт принял разбор, обещающий код, которого нет" >&2
    exit 1
fi
grep -q "SE-999" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал протухший код:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: обещанный, но не приходящий код ловится"

# 3. Позиция в цитате разошлась с фактической - ловится.
write_appendix "SE-034"
sed -i.bak 's/model.takt:1:8:/model.takt:9:9:/' \
    "$TREE/book/src/appendix-errors/index.typ"
rm -f "$TREE/book/src/appendix-errors/index.typ.bak"
if run_gate; then
    echo "  ОШИБКА: гейт принял цитату с чужой позицией" >&2
    exit 1
fi
grep -q "позиция" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал расхождение позиции:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: расхождение позиции в цитате ловится"

# 4. Протухшая запись долга - ловится.
write_appendix "SE-034"
echo "SE-034 | запись долга, которой не место" \
    > "$TREE/scripts/book-diagnostic-examples-baseline.txt"
if run_gate; then
    echo "  ОШИБКА: гейт принял долг, который воспроизводится" >&2
    exit 1
fi
grep -q "протухла" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал протухшую запись долга:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: протухшая запись долга ловится"

# 5. Разбор `SIM` проверяется прогоном эталона, а не компилятора.
cat > "$TREE/book/src/appendix-errors/index.typ" <<'SIMCASE'
= Ошибки и предупреждения

=== `SIM-001` — деление на ноль
Делитель оказался нулём.
```takt
var a: u8 := 1;
var b: u8 := 0;
var c: u8 := 0;

start Run {
    always { c := a / b; }
    ref Run;
}
```
```text
ОШИБКА вычисления на шаге 1: деление на ноль (SIM-001)
```
SIMCASE
: > "$TREE/scripts/book-diagnostic-examples-baseline.txt"
if run_gate; then
    echo '  OK: разбор SIM проверяется прогоном эталона'
else
    echo "  ОШИБКА: гейт отверг разбор SIM, который воспроизводится:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
fi

sed -i.bak 's/деление на ноль (SIM-001)/деление на ноль (SIM-018)/' \
    "$TREE/book/src/appendix-errors/index.typ"
rm -f "$TREE/book/src/appendix-errors/index.typ.bak"
if run_gate; then
    echo "  ОШИБКА: гейт принял разбор SIM с чужим кодом" >&2
    exit 1
fi
echo '  OK: чужой код в разборе SIM ловится'

# 6. Приложение без пар - отказ, а не тривиальный успех.
: > "$TREE/scripts/book-diagnostic-examples-baseline.txt"
cat > "$TREE/book/src/appendix-errors/index.typ" <<'EMPTY'
= Ошибки и предупреждения

Разбора здесь нет вовсе.
EMPTY
if run_gate; then
    echo "  ОШИБКА: гейт принял приложение без единой пары" >&2
    exit 1
fi
echo "  OK: приложение без пар отвергнуто"

echo "  Сторож гейта воспроизводимости примеров пройден."

#!/bin/sh
# Тест проверки трасс прогонов.
#
# Ловушек у проверки четыре, и каждая проверяется мутацией: съехавшее значение,
# чужое состояние, переменная, которой у модели нет, и протухшая запись долга.
# Отдельно проверяется вырожденный вход - главы без единой трассы обязаны
# ронять прогон, иначе при смене разметки проверка начнёт проверять пустоту.
#
# Прогон идёт на копии дерева (`BTR_ROOT`): рабочие главы не трогаются.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-book-traces.py"
TAKT_SIM="${TAKT_SIM:-$ROOT/target/precheck/debug/takt-sim}"

if [ ! -x "$TAKT_SIM" ]; then
    echo "test-book-traces: не найден симулятор $TAKT_SIM" >&2
    exit 1
fi

echo "Сторож гейта трасс прогонов (фича 0522)..."

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

TREE="$TMP/tree"
mkdir -p "$TREE/book/src/01-probe/examples" "$TREE/scripts"
BASE="$TREE/scripts/book-traces-baseline.txt"

cat > "$TREE/book/src/01-probe/examples/probe.takt" <<'TAKT'
var t: u8 := 0;

start Run {
    always { t := t + 1; }
    ref Run;
}
TAKT

chapter() {
    cat > "$TREE/book/src/01-probe/index.typ" <<CHAPTER
= Проба

Прогон:
\`\`\`bash
takt-sim probe.takt -n 2
\`\`\`
\`\`\`text
$1
$2
\`\`\`
CHAPTER
}

run_gate() {
    BTR_ROOT="$TREE" TAKT_SIM="$TAKT_SIM" python3 "$GATE" >"$TMP/log" 2>&1
}

# 1. Верная трасса принимается.
chapter 'Шаг 1: [Run]  t=1' 'Шаг 2: [Run]  t=2'
: > "$BASE"
if run_gate; then
    echo "  OK: верная трасса принимается"
else
    echo "  ОШИБКА: гейт отверг трассу, которую даёт симулятор:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
fi

# 2. Съехавшее значение ловится.
chapter 'Шаг 1: [Run]  t=1' 'Шаг 2: [Run]  t=9'
if run_gate; then
    echo "  ОШИБКА: гейт принял трассу с чужим значением" >&2
    exit 1
fi
grep -q "t=9" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал расхождение:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: съехавшее значение ловится"

# 3. Чужое состояние ловится.
chapter 'Шаг 1: [Idle]  t=1' 'Шаг 2: [Run]  t=2'
if run_gate; then
    echo "  ОШИБКА: гейт принял трассу с чужим состоянием" >&2
    exit 1
fi
grep -q "состояния" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал состояние:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: чужое состояние ловится"

# 4. Переменная, которой у модели нет, ловится.
chapter 'Шаг 1: [Run]  t=1  ghost=0' 'Шаг 2: [Run]  t=2'
if run_gate; then
    echo "  ОШИБКА: гейт принял трассу с несуществующей переменной" >&2
    exit 1
fi
grep -q "ghost" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал переменную:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: несуществующая переменная ловится"

# 5. Протухшая запись долга ловится.
chapter 'Шаг 1: [Run]  t=1' 'Шаг 2: [Run]  t=2'
echo "01-probe/index.typ#1 | запись долга, которой не место" > "$BASE"
if run_gate; then
    echo "  ОШИБКА: гейт принял долг, который сходится" >&2
    exit 1
fi
grep -q "протухла" "$TMP/log" || {
    echo "  ОШИБКА: отказ не назвал протухшую запись:" >&2
    sed 's/^/    /' "$TMP/log" >&2
    exit 1
}
echo "  OK: протухшая запись долга ловится"

# 6. Главы без трасс - отказ, а не тривиальный успех.
: > "$BASE"
cat > "$TREE/book/src/01-probe/index.typ" <<'EMPTY'
= Проба

Трасс здесь нет вовсе.
EMPTY
if run_gate; then
    echo "  ОШИБКА: гейт принял главы без единой трассы" >&2
    exit 1
fi
echo "  OK: главы без трасс отвергнуты"

echo "  Сторож гейта трасс прогонов пройден."

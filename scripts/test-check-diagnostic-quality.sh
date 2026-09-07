#!/bin/sh
# Тест проверки качества диагностик.
#
# Проверяет предмет проверки, а не факт запуска: каждое из пяти правил обязано
# ловить своё нарушение, а согласованное состояние - проходить.
#
# Правила D3 (язык сообщения) и D4 (внутреннее представление) тестом не
# проверяются: чтобы их нарушить, нужен дефект В коде компилятора, а не вход.
# Граница названа; сами правила держатся на прогоне полного корпуса.
#
# Проверка гоняется на своём корпусе из двух входов и своих реестрах
# (переменные `TAKT_DIAG_CORPUS`, `TAKT_DIAG_POSITION_DEBT`,
# `TAKT_DIAG_COVERAGE_DEBT`): предмет проверки - правила, и им довольно
# нескольких файлов, а полный корпус стоил бы 11 с на каждый случай.
set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

CORPUS="$TMP/corpus"
mkdir -p "$CORPUS"

# Вход, дающий отказ С позицией: имя не объявлено.
cat > "$CORPUS/named.takt" <<'EOF'
var x: bit := 0;
start S {
    always {
        x := ghost();
    }
}
EOF

# Вход, дающий диагностику без позиции: модель объявила такт устройства, а
# флаг не передан.
# Именно `SE-069` и осталась единственной записью долга: её
# позиция `Implicit` осознанно - контракт относится ко всей модели, а не к
# строке (решение 0134).
cat > "$CORPUS/faceless.takt" <<'EOF'
model Timed {
    clock 1kHz;
    out o: u8;
    var n: u8 := 0;
    start Run {
        always { n := n + 1; o := n; }
        ref Run: n < 3;
    }
}

start Main = Timed;
EOF

run() {
    TAKT_DIAG_CORPUS="$CORPUS" \
    TAKT_DIAG_POSITION_DEBT="$1" \
    TAKT_DIAG_COVERAGE_DEBT="$2" \
        python3 "$ROOT/scripts/check-diagnostic-quality.py" >"$TMP/out" 2>&1
}

# Какие коды даёт наш корпус - спрашиваем сам проверка (режим отчёта).
: > "$TMP/empty"
printf 'SE-069 # сторож\n' > "$TMP/pos"
TAKT_DIAG_CORPUS="$CORPUS" TAKT_DIAG_POSITION_DEBT="$TMP/pos" \
    TAKT_DIAG_COVERAGE_DEBT="$TMP/empty" \
    python3 "$ROOT/scripts/check-diagnostic-quality.py" --emit-reached > "$TMP/seen.txt"
grep -oE '`[A-Z]{2,3}-[0-9]{3}`' "$ROOT/docs/diagnostics/README.md" | tr -d '`' | sort -u |
    grep -E '^(LE|SY|SE|CC|RS|SV|ST|AM|DF|PU)-' > "$TMP/all.txt"

# Полный реестр покрытия: всё, кроме кодов, которые даёт наш корпус.
comm -23 "$TMP/all.txt" "$TMP/seen.txt" | sed 's/$/ # сторож/' > "$TMP/cov_ok"

# 1. Согласованное состояние принимается.
if run "$TMP/pos" "$TMP/cov_ok"; then
    echo "  OK: согласованное состояние принимается"
else
    echo "  ПРОВАЛ: согласованное состояние отвергнуто"
    cat "$TMP/out"
    exit 1
fi

# 2. Диагностика без позиции, чей код не в долге, ловится.
if run "$TMP/empty" "$TMP/cov_ok"; then
    echo "  ПРОВАЛ: диагностика без позиции не поймана (D2)"
    exit 1
fi
grep -q 'D2 ' "$TMP/out" || { echo "  ПРОВАЛ: отказ не назвал правило D2"; cat "$TMP/out"; exit 1; }
echo "  OK: диагностика без позиции ловится (D2)"

# 3. Недостижимый код без записи ловится.
# Убираем одну запись - недостижимый код без записи обязан быть пойман.
sed '$d' "$TMP/cov_ok" > "$TMP/cov_short"
if run "$TMP/pos" "$TMP/cov_short"; then
    echo "  ПРОВАЛ: недостижимый код без записи не пойман (D5)"
    exit 1
fi
grep -q 'D5 ' "$TMP/out" || { echo "  ПРОВАЛ: отказ не назвал правило D5"; exit 1; }
echo "  OK: недостижимый код без записи ловится (D5)"

# 4. Протухшая запись покрытия ловится: код, который корпус даёт.
cp "$TMP/cov_ok" "$TMP/cov_stale"
head -1 "$TMP/seen.txt" | sed 's/$/ # протухшая/' >> "$TMP/cov_stale"
if run "$TMP/pos" "$TMP/cov_stale"; then
    echo "  ПРОВАЛ: протухшая запись покрытия не поймана (D5)"
    exit 1
fi
grep -q 'протухла' "$TMP/out" || { echo "  ПРОВАЛ: отказ не назвал протухшую запись"; exit 1; }
echo "  OK: протухшая запись покрытия ловится (D5)"

# 5. Протухшая запись долга позиций ловится: код, который
#    корпус печатает С координатой, в долге стоять не вправе.
printf 'SE-069 # сторож\nSE-004 # протухшая: корпус сторожа даёт её С координатой\n' > "$TMP/pos_stale"
if run "$TMP/pos_stale" "$TMP/cov_ok"; then
    echo "  ПРОВАЛ: протухшая запись долга позиций не поймана (D2)"
    exit 1
fi
grep -q 'запись протухла' "$TMP/out" || {
    echo "  ПРОВАЛ: отказ не назвал протухшую запись долга позиций"
    cat "$TMP/out"
    exit 1
}
echo "  OK: протухшая запись долга позиций ловится (D2)"

echo "Сторож гейта качества диагностик: все проверки пройдены"

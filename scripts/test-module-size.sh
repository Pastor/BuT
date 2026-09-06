#!/bin/sh
# Тест проверки размера модулей.
#
# У проверки 0027 самопроверки не было вовсе: он падал только на настоящем дереве,
# и "зелено" ничем не отличалось от "не смотрит". Здесь каждое из его условий
# проверяется мутацией на временном дереве (MS_ROOT), а рабочее не трогается.
#
# Отдельно проверяется жёлтая зона: она обязана печататься и обязана не
# влиять на код возврата - проверка, роняющий прогон за 30 строк до лимита,
# заставлял бы дробить модуль в чужой правке.
#
# POSIX sh (образец - scripts/test-legacy-names.sh).
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-module-size.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

echo "Сторож гейта размера модулей (фича 0261)..."

TREE="$TMP/tree"
mkdir -p "$TREE/takt-lang/src" "$TREE/takt-sim/src" \
         "$TREE/takt-lang/tests" "$TREE/takt-sim/tests" "$TREE/scripts"

# Файл заданной длины.
make_file() {
    : > "$2"
    i=0
    while [ "$i" -lt "$1" ]; do
        echo "// строка $i" >> "$2"
        i=$((i + 1))
    done
}

run_gate() { MS_ROOT="$TREE" sh "$GATE" >"$TMP/out" 2>&1; }

# --- 1. Контроль: маленький файл и пустой реестр принимаются ------------------
printf '# реестр\n' > "$TREE/scripts/module-size-baseline.txt"
make_file 100 "$TREE/takt-lang/src/small.rs"
if run_gate; then
    ok "маленький файл принимается"
else
    fail "маленький файл отвергнут: $(cat "$TMP/out")"
fi

# --- 2. Новый нарушитель ловится ---------------------------------------------
make_file 1200 "$TREE/takt-lang/src/big.rs"
if ! run_gate && grep -q 'при лимите' "$TMP/out"; then
    ok "новый нарушитель ловится"
else
    fail "новый нарушитель НЕ пойман: $(cat "$TMP/out")"
fi

# --- 3. Рост из реестра ловится ----------------------------------------------
printf '# реестр\n1100 takt-lang/src/big.rs\n' > "$TREE/scripts/module-size-baseline.txt"
if ! run_gate && grep -q 'вырос' "$TMP/out"; then
    ok "рост сверх записанного ловится"
else
    fail "рост НЕ пойман: $(cat "$TMP/out")"
fi

# --- 4. Ужатие требует правки записи -----------------------------------------
printf '# реестр\n1300 takt-lang/src/big.rs\n' > "$TREE/scripts/module-size-baseline.txt"
if ! run_gate && grep -q 'ужат' "$TMP/out"; then
    ok "ужатие требует обновить запись"
else
    fail "ужатие НЕ поймано: $(cat "$TMP/out")"
fi

# --- 5. Закрытая запись ловится ----------------------------------------------
rm "$TREE/takt-lang/src/big.rs"
make_file 500 "$TREE/takt-lang/src/big.rs"
printf '# реестр\n1300 takt-lang/src/big.rs\n' > "$TREE/scripts/module-size-baseline.txt"
if ! run_gate && grep -q 'уложился в лимит' "$TMP/out"; then
    ok "закрытая запись ловится (храповик не проворачивается назад)"
else
    fail "закрытая запись НЕ поймана: $(cat "$TMP/out")"
fi

# --- 6. Запись на несуществующий файл ловится --------------------------------
printf '# реестр\n1300 takt-lang/src/gone.rs\n' > "$TREE/scripts/module-size-baseline.txt"
if ! run_gate && grep -q 'несуществующий файл' "$TMP/out"; then
    ok "запись на несуществующий файл ловится"
else
    fail "несуществующий файл НЕ пойман: $(cat "$TMP/out")"
fi

# --- 7. Жёлтая зона печатается и не роняет прогон ----------------
printf '# реестр\n' > "$TREE/scripts/module-size-baseline.txt"
rm "$TREE/takt-lang/src/big.rs"
make_file 985 "$TREE/takt-lang/src/near.rs"
if run_gate && grep -q 'Жёлтая зона' "$TMP/out" && grep -q 'near.rs' "$TMP/out"; then
    ok "жёлтая зона печатается и не роняет прогон"
else
    fail "жёлтая зона не сработала (или уронила прогон): $(cat "$TMP/out")"
fi

# --- 8. Контроль: файл вне зоны в предупреждение не попадает -----------------
# Без этого "зона печатается" означало бы "печатается всё подряд".
rm "$TREE/takt-lang/src/near.rs"
make_file 900 "$TREE/takt-lang/src/mid.rs"
if run_gate && ! grep -q 'Жёлтая зона' "$TMP/out"; then
    ok "файл вне зоны предупреждения не даёт"
else
    fail "файл вне зоны попал в жёлтую зону: $(cat "$TMP/out")"
fi

if [ "$FAILED" -ne 0 ]; then
    echo "  Сторож гейта размера модулей: ПРОВАЛ" >&2
    exit 1
fi
echo "  Сторож гейта размера модулей: все проверки пройдены"

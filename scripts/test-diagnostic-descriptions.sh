#!/bin/sh
# Тест проверки описаний диагностик.
#
# Проверка без самопроверки - это проверка, о котором известно лишь то, что он
# зелёный (: четыре условия прожили без проверки, и дырявым оказалось
# то, которое никто не пробовал сломать).
#
# Проверяется на копии дерева: реестр и долг подменяются, рабочие файлы не
# трогаются (переменная `DD_ROOT`).
#
# POSIX sh.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TOOL="$ROOT/scripts/check-diagnostic-descriptions.py"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

mkdir -p "$TMP/docs/diagnostics" "$TMP/scripts"
: > "$TMP/scripts/diagnostic-description-baseline.txt"

write_registry() {
    cat > "$TMP/docs/diagnostics/README.md" <<EOF
| Код | Значение | Место |
|---|---|---|
$1
EOF
}

echo "Сторож гейта описаний диагностик (фича 0311)..."

# --- 1. Годный реестр принимается ---------------------------------------------
write_registry '| `XX-001` | внятное описание длиной заведомо сверх порога | `src/x.rs` |'
if DD_ROOT="$TMP" python3 "$TOOL" >"$TMP/out" 2>&1; then
    ok "годный реестр принимается"
else
    fail "годный реестр отвергнут: $(cat "$TMP/out")"
fi

# --- 2. Служебное слово ловится (D1) ------------------------------------------
write_registry '| `XX-002` | ошибка | `src/x.rs` |'
if ! DD_ROOT="$TMP" python3 "$TOOL" >"$TMP/out" 2>&1 && grep -q 'D1' "$TMP/out"; then
    ok "служебное слово ловится"
else
    fail "служебное слово НЕ поймано: $(cat "$TMP/out")"
fi

# --- 3. Служебное слово не спасается долгом ------------------------------------
# Именно этот случай и был предметом фичи: внести "ошибку" в долг значило бы
# узаконить запись, не несущую смысла по построению.
echo "XX-002" > "$TMP/scripts/diagnostic-description-baseline.txt"
if ! DD_ROOT="$TMP" python3 "$TOOL" >"$TMP/out" 2>&1 && grep -q 'D1' "$TMP/out"; then
    ok "долг не покрывает служебное слово"
else
    fail "служебное слово узаконено долгом: $(cat "$TMP/out")"
fi
: > "$TMP/scripts/diagnostic-description-baseline.txt"

# --- 4. Короткое описание ловится и покрывается долгом (D2) --------------------
write_registry '| `XX-003` | коротко | `src/x.rs` |'
if ! DD_ROOT="$TMP" python3 "$TOOL" >"$TMP/out" 2>&1 && grep -q 'D2' "$TMP/out"; then
    ok "короткое описание ловится"
else
    fail "короткое описание НЕ поймано: $(cat "$TMP/out")"
fi
echo "XX-003" > "$TMP/scripts/diagnostic-description-baseline.txt"
if DD_ROOT="$TMP" python3 "$TOOL" >"$TMP/out" 2>&1; then
    ok "узаконенный долг пропускается"
else
    fail "долг не пропущен: $(cat "$TMP/out")"
fi

# --- 5. Протухшая запись долга ловится (D3) ------------------------------------
write_registry '| `XX-003` | внятное описание длиной заведомо сверх порога | `src/x.rs` |'
if ! DD_ROOT="$TMP" python3 "$TOOL" >"$TMP/out" 2>&1 && grep -q 'D3' "$TMP/out"; then
    ok "протухшая запись долга ловится"
else
    fail "храповик проворачивается назад: $(cat "$TMP/out")"
fi

# --- 6. Пустой реестр - вырожденная проверка ----------------------------------
write_registry ''
if ! DD_ROOT="$TMP" python3 "$TOOL" >"$TMP/out" 2>&1 && grep -q 'вырожден' "$TMP/out"; then
    ok "пустой реестр отвергается"
else
    fail "пустой реестр принят за успех: $(cat "$TMP/out")"
fi

# --- 7. Собственная самопроверка проверки -----------------------------------------
if python3 "$TOOL" --self-test >"$TMP/out" 2>&1; then
    ok "самопроверка гейта проходит"
else
    fail "самопроверка гейта провалена: $(cat "$TMP/out")"
fi

[ "$FAILED" -eq 0 ] || { echo "  Сторож гейта описаний: ПРОВАЛ" >&2; exit 1; }
echo "  Сторож гейта описаний: все проверки пройдены"

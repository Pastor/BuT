#!/bin/sh
# Тест проверки снимков book/: мутацией доказывает, что расхождение
# ловится, а согласованное дерево принимается.
#
# Повод: проверка, который никогда не падал, неотличим от проверки, который не
# смотрит, - а именно этим и был прежний порядок, когда снимки сверяли глазами
# (три из шести отстали, и никто не заметил).
#
# Гоняется на копии дерева через BG_ROOT: рабочие снимки не трогаются.
#
# POSIX sh (образец - scripts/test-legacy-names.sh).
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-book-generated.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

echo "Сторож гейта снимков book/ (фича 0274)..."

# Копия дерева: только то, что читает проверка.
mkdir -p "$TMP/tree/book"
cp -R "$ROOT/book/src" "$TMP/tree/book/src"

run_gate() { BG_ROOT="$TMP/tree" TAKTC="$ROOT/target/precheck/debug/taktc" sh "$GATE" >"$TMP/out" 2>&1; }

# --- 1. Согласованное дерево принимается ------------------------------------
if run_gate; then
    ok "согласованные снимки принимаются"
else
    fail "согласованные снимки отвергнуты: $(cat "$TMP/out")"
fi

# --- 2. Мутация снимка ловится ----------------------------------------------
# Правится один символ в одном файле: проверка обязан заметить и назвать каталог.
SNAP="$TMP/tree/book/src/18-showcase/generated/rust/lift.rs"
if [ ! -f "$SNAP" ]; then
    fail "нет снимка $SNAP — проверка вырождена"
else
    printf '\n// мутация сторожа\n' >> "$SNAP"
    if ! run_gate && grep -q 'снимок отстал' "$TMP/out"; then
        ok "изменённый снимок ловится"
    else
        fail "изменённый снимок НЕ пойман: $(cat "$TMP/out")"
    fi
    if grep -q '18-showcase/generated/rust' "$TMP/out"; then
        ok "отказ называет каталог снимка"
    else
        fail "отказ не называет каталог — автору негде искать"
    fi
fi

# --- 3. Обновление приводит дерево в порядок --------------------------------
if BG_ROOT="$TMP/tree" TAKTC="$ROOT/target/precheck/debug/taktc" sh "$GATE" --update >/dev/null 2>&1 && run_gate; then
    ok "--update возвращает согласие"
else
    fail "--update не восстановил согласие: $(cat "$TMP/out")"
fi

# --- 4. Снимок без примера - отказ, а не молчание ---------------------------
# Каталог `generated/<цель>` осмыслен только рядом с `examples/`: снимок,
# источник которого неизвестен, сверить не с чем, и молчать об этом нельзя.
mkdir -p "$TMP/tree/book/src/99-orphan/generated/c"
printf '// снимок без примера\n' > "$TMP/tree/book/src/99-orphan/generated/c/ghost.c"
if ! run_gate && grep -q 'без соседнего examples' "$TMP/out"; then
    ok "снимок без примера отвергнут"
else
    fail "снимок без источника принят молча: $(cat "$TMP/out")"
fi

[ "$FAILED" -eq 0 ] || { echo "  Сторож гейта снимков провален." >&2; exit 1; }
echo "  Сторож гейта снимков пройден."

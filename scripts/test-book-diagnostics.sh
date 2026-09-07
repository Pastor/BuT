#!/bin/sh
# Тест проверки кодов документа: мутацией доказывает, что расхождение
# ловится в обе стороны, а согласованное дерево принимается.
#
# Повод: проверка, который никогда не падал, неотличим от проверки, который не
# смотрит. Именно этим и был прежний порядок - приложение "Ошибки" сверяли
# глазами, и замер 2026-08-20 нашёл 32 кода вне сводной таблицы и один код,
# описанный после того, как он выведен (`SE-066`).: четыре условия
# проверки диагностик прожили без самопроверки, и дырявым оказалось то, которое
# никто не пробовал сломать.
#
# Гоняется на копии дерева через BD_ROOT: рабочие документы не трогаются.
#
# POSIX sh (образец - scripts/test-book-generated.sh).
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-book-diagnostics.py"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

echo "Сторож гейта кодов документа (фича 0290)..."

# Копия дерева: только то, что читает проверка.
mkdir -p "$TMP/tree/docs/diagnostics" "$TMP/tree/book/src/appendix-errors"
cp "$ROOT/docs/diagnostics/README.md" "$TMP/tree/docs/diagnostics/README.md"
cp "$ROOT/book/src/appendix-errors/index.typ" "$TMP/tree/book/src/appendix-errors/index.typ"

APPENDIX="$TMP/tree/book/src/appendix-errors/index.typ"
BACKUP="$TMP/appendix.orig"
cp "$APPENDIX" "$BACKUP"

run_gate() { BD_ROOT="$TMP/tree" python3 "$GATE" >"$TMP/out" 2>&1; }
restore()  { cp "$BACKUP" "$APPENDIX"; }

# --- 1. Контроль: согласованное дерево принимается ---------------------------
# Без этой проверки "проверка падает" означало бы лишь, что он падает всегда.
if run_gate; then
    ok "согласованное дерево принимается"
else
    fail "согласованное дерево отвергнуто: $(cat "$TMP/out")"
fi

# --- 2. Код реестра, пропавший из приложения, ловится ------------------------
grep -v '\[`SE-115`\]' "$APPENDIX" > "$TMP/mut" && mv "$TMP/mut" "$APPENDIX"
if ! run_gate && grep -q 'Нет в приложении' "$TMP/out" && grep -q 'SE-115' "$TMP/out"; then
    ok "пропавший код ловится и назван"
else
    fail "пропавший код НЕ пойман: $(cat "$TMP/out")"
fi
restore

# --- 3. Код приложения, которого нет в реестре, ловится ----------------------
# Ровно случай `SE-066`: справочник обещает диагностику, которой инструмент не
# выдаёт. Это направление кандидат не называл вовсе, а оно дороже пробела.
python3 - "$APPENDIX" <<'PY'
import sys
path = sys.argv[1]
text = open(path, encoding="utf-8").read()
anchor = "    [`SE-115`],"
assert anchor in text, "нет якоря вставки — проверка вырождена"
text = text.replace(anchor, "    [`SE-999`], [Мутация сторожа],\n" + anchor, 1)
open(path, "w", encoding="utf-8").write(text)
PY
if ! run_gate && grep -q 'Нет в реестре' "$TMP/out" && grep -q 'SE-999' "$TMP/out"; then
    ok "лишний код ловится и назван"
else
    fail "лишний код НЕ пойман: $(cat "$TMP/out")"
fi
restore

# --- 4. Пустая разметка - ошибка, а не успех --------------------------------
# Смена разметки не вправе молча превратить сверку в проверку пустого множества
#
printf '= Ошибки\nПроза без таблиц.\n' > "$APPENDIX"
if ! run_gate && grep -q 'Сводная таблица кодов' "$TMP/out"; then
    ok "документ без сводной таблицы валит гейт"
else
    fail "документ без сводной таблицы НЕ пойман: $(cat "$TMP/out")"
fi
restore

# --- 5. Реестр без строк таблиц - тоже ошибка -------------------------------
printf '# Реестр\nПроза без таблиц.\n' > "$TMP/tree/docs/diagnostics/README.md"
if ! run_gate && grep -q 'строки таблиц реестра' "$TMP/out"; then
    ok "реестр без таблиц валит гейт"
else
    fail "реестр без таблиц НЕ пойман: $(cat "$TMP/out")"
fi

if [ "$FAILED" -ne 0 ]; then
    echo "Сторож гейта кодов документа: ПРОВАЛ" >&2
    exit 1
fi
echo "Сторож гейта кодов документа: все проверки пройдены"

#!/bin/sh
# Тест проверки разбора диагностик.
#
# Проверка гоняется на копии дерева (переменная DA_ROOT), потому что проверяет он
# состав документа: испортить рабочий каталог ради проверки нельзя.
#
# Условий три, и все обязательны:
#   1. согласованное дерево принимается;
#   2. код, подпадающий под признак и лишённый разбора, ловится и называется;
# 3. вырожденный вход (приложение без разборов) даёт отказ, а не успех -
# проверка, который на пустоте рапортует "всё разобрано", не проверка.
set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
GATE="$ROOT/scripts/check-diagnostic-analysis.py"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

mkdir -p "$TMP/docs/diagnostics" "$TMP/book/src/appendix-errors"
cp "$ROOT/docs/diagnostics/README.md" "$TMP/docs/diagnostics/README.md"
cp "$ROOT/book/src/appendix-errors/index.typ" "$TMP/book/src/appendix-errors/index.typ"

# 1. Согласованное дерево принимается.
if ! DA_ROOT="$TMP" "$GATE" >/dev/null 2>&1; then
    echo "  ПРОВАЛ: согласованное дерево отвергнуто"
    exit 1
fi
echo "  OK: согласованное дерево принимается"

# 2. Пропавший разбор ловится и назван.
python3 - "$TMP" <<'PY'
import re, sys
path = sys.argv[1] + "/book/src/appendix-errors/index.typ"
text = open(path, encoding="utf-8").read()
# Убираем заголовок разбора SE-125 - код подпадает под признак (verilator).
text = text.replace("=== `SE-125` — разряд за объявленной шириной", "=== разряд за объявленной шириной")
open(path, "w", encoding="utf-8").write(text)
PY
OUT=$(DA_ROOT="$TMP" "$GATE" 2>&1 || true)
case "$OUT" in
    *"SE-125"*) echo "  OK: пропавший разбор ловится и назван" ;;
    *) echo "  ПРОВАЛ: пропавший разбор не пойман:"; echo "$OUT"; exit 1 ;;
esac

# 3. Вырожденный вход - отказ, а не успех.
cp "$ROOT/book/src/appendix-errors/index.typ" "$TMP/book/src/appendix-errors/index.typ"
python3 - "$TMP" <<'PY'
import re, sys
path = sys.argv[1] + "/book/src/appendix-errors/index.typ"
text = open(path, encoding="utf-8").read()
text = "\n".join(l for l in text.splitlines() if not l.startswith("=== "))
open(path, "w", encoding="utf-8").write(text)
PY
if DA_ROOT="$TMP" "$GATE" >/dev/null 2>&1; then
    echo "  ПРОВАЛ: приложение без разборов принято"
    exit 1
fi
echo "  OK: вырожденный вход даёт отказ"

echo "  Сторож гейта разбора диагностик: все проверки пройдены"

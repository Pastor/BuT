#!/usr/bin/env bash
# Контрольная проба scripts/gatelib.py и scripts/gatelib.sh: помощник отказывает
# на выборке ниже границы и молчит на достаточной. Обе стороны обязательны:
# помощник, отвергающий всё, прошёл бы набор с одной.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

fail() { echo "  - $1" >&2; exit 1; }

# Python: достаточный вход описывается для фразы успеха.
note="$(python3 - "$ROOT" <<'PY'
import sys
sys.path.insert(0, sys.argv[1] + "/scripts")
from gatelib import require_input
print(require_input("записи реестра", 539, source="docs/features/README.md"))
PY
)"
[ "$note" = "разобрано 539 — записи реестра" ] || fail "P1 описание входа неверно: $note"
echo "  + P1 достаточный вход описан числом разобранных записей"

# Python: пустая выборка отвергается, и отказ называет вход.
if out="$(python3 - "$ROOT" 2>&1 <<'PY'
import sys
sys.path.insert(0, sys.argv[1] + "/scripts")
from gatelib import require_input
require_input("записи реестра", 0, source="docs/features/README.md")
PY
)"; then
    fail "P2 пустая выборка принята"
fi
grep -q "записи реестра" <<<"$out" || fail "P2 отказ не называет вход: $out"
echo "  + P2 пустая выборка отвергнута"

# Python: недобор до объявленной границы - тоже отказ, а не только ноль.
if python3 - "$ROOT" >/dev/null 2>&1 <<'PY'
import sys
sys.path.insert(0, sys.argv[1] + "/scripts")
from gatelib import require_input
require_input("строки таблицы", 3, minimum=10)
PY
then
    fail "P3 недобор до границы принят"
fi
echo "  + P3 недобор до границы отвергнут"

# Оболочка: та же пара условий.
note="$(sh -c ". '$ROOT/scripts/gatelib.sh'; require_input 'просмотренные файлы' 636")"
[ "$note" = "разобрано 636 - просмотренные файлы" ] || fail "P4 описание входа неверно: $note"
echo "  + P4 достаточный вход описан и в оболочке"

if sh -c ". '$ROOT/scripts/gatelib.sh'; N=\"\$(require_input 'записи' 0)\" || exit 1" >/dev/null 2>&1; then
    fail "P5 пустая выборка принята оболочкой"
fi
echo "  + P5 пустая выборка отвергнута оболочкой"

# Отказ обязан пережить подстановку команд: без хвоста `|| exit 1` выход из
# подоболочки оставил бы вызывающего с пустым описанием и нулевым кодом.
if sh -c "set -eu; . '$ROOT/scripts/gatelib.sh'; N=\"\$(require_input 'записи' 0)\"; echo \"\$N\"" >/dev/null 2>&1; then
    fail "P6 отказ не пережил подстановку под set -e"
fi
echo "  + P6 отказ переживает подстановку команд"

echo "Контрольная проба помощника границ: все пробы пройдены"

#!/usr/bin/env bash
# Тест проверки дизайн-системы.
#
# Мутациями доказывается, что `check-design.py` ловит то, ради чего заведён:
#
# G0 - согласованное дерево принимается (иначе проверка красен всегда);
# G1 - цвет числом в правиле;
# G2 - готовый цвет в `:hover`;
# G3 - пара "заливка / чернила" вне реестра книги;
# G4 - пара реестра, которая нигде не применяется (сверка в обе стороны);
# G5 - кегль числом;
# G6 - высота числом;
# G7 - контрол книги, которого нет в витрине;
# G7b - пара книги, которой нет в витрине (третья сторона сверки);
# G7c - переменная, употреблённая без объявления;
# G8 - вырожденный вход: пустой CSS отвергается, а не проходит молча.
#
# Мутации ставятся на копии дерева (`DESIGN_ROOT`), рабочие файлы не
# трогаются: тест, который правит проект, однажды оставит правку после
# падения.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

echo "Сторож гейта дизайн-системы (фича 0531)..."

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
TREE="$WORK/tree"
mkdir -p "$TREE/scripts" "$TREE/web/static" "$TREE/web/design"
cp "$ROOT/scripts/check-design.py" "$TREE/scripts/"
cp "$ROOT/web/static/app.css" "$TREE/web/static/"
cp "$ROOT/web/design/BOOK.md" "$ROOT/web/design/controls.html" "$TREE/web/design/"

run_gate() { ( cd "$TREE" && python3 scripts/check-design.py 2>&1 ); }

restore() {
  cp "$ROOT/web/static/app.css" "$TREE/web/static/app.css"
  cp "$ROOT/web/design/BOOK.md" "$TREE/web/design/BOOK.md"
  cp "$ROOT/web/design/controls.html" "$TREE/web/design/controls.html"
}

expect_caught() {  # $1 = метка, $2 = ожидаемый код правила
  local label="$1" code="$2" out
  if out="$(run_gate)"; then
    echo "  ПРОВАЛ: $label не пойман"
    restore
    exit 1
  fi
  if grep -q "$code" <<< "$out"; then
    echo "  OK: $label ловится ($code)"
  else
    echo "  ПРОВАЛ: $label пойман не тем правилом:"
    echo "$out" | sed 's/^/    /'
    restore
    exit 1
  fi
  restore
}

# -- G0: согласованное дерево -------------------------------------------------
if out="$(run_gate)"; then
  echo "  OK: G0 согласованное дерево принимается"
else
  echo "  ПРОВАЛ: G0 согласованное дерево отвергнуто:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi

# -- G1: цвет числом ----------------------------------------------------------
printf '\n.brand { color: #ff0000; }\n' >> "$TREE/web/static/app.css"
expect_caught "G1 цвет числом" "D1"

# -- G2: готовый цвет в отклике -----------------------------------------------
printf '\n.tab:hover { background: var(--surface-accent); }\n' >> "$TREE/web/static/app.css"
expect_caught "G2 готовый цвет в :hover" "D2"

# -- G3: пара вне реестра -----------------------------------------------------
printf '\n.brand { background: var(--surface-accent); color: var(--on-surface-soft); }\n' \
  >> "$TREE/web/static/app.css"
expect_caught "G3 пара вне реестра" "D3"

# -- G4: пара реестра без применения ------------------------------------------
# Сверка обязана идти в обе стороны: реестр, в который дописали строку "на
# будущее", перестаёт описывать вёрстку и начинает её выдумывать.
printf '| `--surface-sunken` | `--on-surface-accent` | нигде |\n' >> "$TREE/web/design/BOOK.md"
expect_caught "G4 пара реестра без применения" "D3"

# -- G5: кегль числом ---------------------------------------------------------
printf '\n.brand { font-size: 13px; }\n' >> "$TREE/web/static/app.css"
expect_caught "G5 кегль числом" "D4"

# -- G6: высота числом --------------------------------------------------------
printf '\n.brand { height: 37px; }\n' >> "$TREE/web/static/app.css"
expect_caught "G6 высота числом" "D5"

# -- G7: контрол книги без витрины --------------------------------------------
printf '\n### Ползунок (`.slider`)\n\nЕго нет в витрине.\n' >> "$TREE/web/design/BOOK.md"
expect_caught "G7 контрол книги без витрины" "D6"

# -- G7b: пара книги без образца в витрине ------------------------------------
# Сверка идёт по трём сторонам: вёрстка, книга, витрина. Отставшая витрина
# показывает не то оформление, которое описано, и заметить это можно только
# сличением двух текстов.
python3 - "$TREE/web/design/controls.html" <<'PYEOF'
import re, sys
from pathlib import Path
p = Path(sys.argv[1])
s = p.read_text(encoding="utf-8")
p.write_text(re.sub(r'\s*<span class="swatch" data-pair="--surface-yes[^\n]*\n', "\n", s, count=1), encoding="utf-8")
PYEOF
expect_caught "G7b пара книги без образца" "D3"

# -- G7c: переменная без объявления -------------------------------------------
# Неизвестную переменную браузер не считает ошибкой - он молча отбрасывает
# объявление, и правило пропадает целиком (так модальное окно вышло без полей).
printf '\n.brand { padding: var(--gap-huge); }\n' >> "$TREE/web/static/app.css"
expect_caught "G7c переменная без объявления" "D8"

# -- G8: вырожденный вход -----------------------------------------------------
# Пустой CSS обязан отвергаться: проверка, который на пустоте отвечает "нарушений
# нет", доказывает не то, что вёрстка чиста, а то, что он ничего не прочёл.
: > "$TREE/web/static/app.css"
if out="$(run_gate)"; then
  echo "  ПРОВАЛ: G8 пустой app.css принят"
  restore
  exit 1
fi
echo "  OK: G8 вырожденный вход отвергается"
restore

echo "  Сторож гейта дизайн-системы: все проверки пройдены (G0…G8, включая G7b и G7c)."

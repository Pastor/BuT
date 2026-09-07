#!/usr/bin/env bash
# Самопроверка scripts/check-docs-style.py: проверка ловит нарушение и
# пропускает исправный текст. Работа идёт на копии дерева, боевые документы
# не затрагиваются.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/takt-docs-style.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

mkdir -p "$WORK/docs" "$WORK/scripts"
cp "$ROOT/scripts/check-docs-style.py" "$WORK/scripts/"

run() {
    ( cd "$WORK" && DS_ROOT="$WORK" python3 scripts/check-docs-style.py >"$WORK/out" 2>&1 )
}

fail() { echo "  - $1" >&2; exit 1; }

# Исправный текст принимается: без этого условия проверка, отвергающая всё,
# тоже прошла бы набор.
cat >"$WORK/docs/ok.md" <<'DOC'
# Пример

Проверка называет предмет: генератор цели `c` печатает объявление, а АСД
хранит позицию узла. Статус карточки — ГОТОВО.
DOC
run || fail "D0 исправный документ отвергнут: $(cat "$WORK/out")"
echo "  + D0 исправный документ принят"

probe() {
    local code="$1" text="$2"
    printf '%s\n' "$text" >"$WORK/docs/probe.md"
    if run; then
        fail "$code нарушение не поймано: $text"
    fi
    grep -q "^  $code " "$WORK/out" || fail "$code ожидался код $code, получено: $(cat "$WORK/out")"
    echo "  + $code поймано"
    rm -f "$WORK/docs/probe.md"
}

probe D1 'Внимание ⚠ здесь сказано важное.'
probe D2 'Правило проверяет гейт предкоммита.'
probe D3 'Значение берётся ТОЛЬКО у объявления.'
probe D4 'Разбор описан в фиче 0533 подробно.'

# Долг снимает находку, но только свою.
printf 'Правило проверяет гейт предкоммита.\n' >"$WORK/docs/probe.md"
printf 'docs/probe.md D2\n' >"$WORK/scripts/docs-style-baseline.txt"
run || fail "D5 долг не снял находку: $(cat "$WORK/out")"
echo "  + D5 долг снимает свою находку"
printf 'Значение берётся ТОЛЬКО у объявления.\n' >>"$WORK/docs/probe.md"
if run; then fail "D6 долг снял чужую находку"; fi
echo "  + D6 долг не снимает чужую находку"
rm -f "$WORK/docs/probe.md" "$WORK/scripts/docs-style-baseline.txt"

# Пример в блоке кода принадлежит примеру и не судится.
cat >"$WORK/docs/probe.md" <<'DOC'
Текст описания.

```text
Здесь ГЕЙТ и ⚠ живут внутри примера.
```
DOC
run || fail "D7 содержимое примера осуждено: $(cat "$WORK/out")"
echo "  + D7 содержимое примера не судится"
rm -f "$WORK/docs/probe.md"

# Пустое дерево - отказ: проверка, не нашедшая ни одного документа, ничего
# не прочла.
rm -f "$WORK/docs/ok.md"
if run; then fail "D8 пустое дерево принято"; fi
echo "  + D8 нет документов - отказ"

echo "Самопроверка стиля документов: все пробы пройдены"

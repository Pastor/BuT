#!/usr/bin/env bash
# Контрольная проба scripts/check-gate-inputs.py: проверка ловит проверку без
# границы, устаревшую и висячую записи реестра, рост долга и собственный пустой
# вход, а согласованное дерево пропускает. Работа идёт на копии, боевой реестр
# не затрагивается.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/takt-gate-inputs.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

mkdir -p "$WORK/scripts"
cp "$ROOT/scripts/check-gate-inputs.py" "$ROOT/scripts/gatelib.py" "$ROOT/scripts/gatelib.sh" "$WORK/scripts/"

BASE="$WORK/scripts/gate-input-baseline.txt"

run() { ( cd "$WORK" && GI_ROOT="$WORK" python3 scripts/check-gate-inputs.py >"$WORK/out" 2>&1 ); }
fail() { echo "  - $1" >&2; exit 1; }

# Подопытные проверки: одна с границей, одна без неё.
printf 'from gatelib import require_input\nrequire_input("вход", 1)\n' >"$WORK/scripts/check-with.py"
printf 'echo "гоняет чужой инструмент"\n' >"$WORK/scripts/check-tool.sh"

# Согласованное дерево принимается. Без этого условия проверка, отвергающая
# всё, тоже прошла бы набор.
cat >"$BASE" <<'REG'
исключение check-tool.sh - судит по коду возврата чужого инструмента
REG
run || fail "G0 согласованное дерево отвергнуто: $(cat "$WORK/out")"
echo "  + G0 согласованное дерево принято"

probe() {
    local code="$1" what="$2"
    if run; then fail "$code $what не поймано"; fi
    grep -q "  $code " "$WORK/out" || fail "$code ожидался код $code, получено: $(cat "$WORK/out")"
    echo "  + $code $what поймано"
}

# G1: проверка без вызова помощника и без записи в реестре.
printf 'echo "ничего не читаю"\n' >"$WORK/scripts/check-mute.sh"
probe G1 "проверка без границы"
rm -f "$WORK/scripts/check-mute.sh"

# G2: запись реестра пережила заведение границы.
printf 'долг check-with.py - устаревшая запись\n' >>"$BASE"
probe G2 "устаревшая запись реестра"

# G3: запись реестра указывает на исчезнувшую проверку.
cat >"$BASE" <<'REG'
исключение check-tool.sh - судит по коду возврата чужого инструмента
долг check-gone.py - проверки с таким именем нет
REG
probe G3 "висячая запись реестра"

# G4: долг вырос сверх потолка. Потолок опускается в копии проверки - так же,
# как он опускается в боевой при переводе очередной проверки.
cat >"$BASE" <<'REG'
исключение check-tool.sh - судит по коду возврата чужого инструмента
долг check-debt-one.py - вход не назван
долг check-debt-two.py - вход не назван
REG
printf 'print("нечего")\n' >"$WORK/scripts/check-debt-one.py"
printf 'print("нечего")\n' >"$WORK/scripts/check-debt-two.py"
sed -i.bak 's/^DEBT_CEILING = .*/DEBT_CEILING = 1/' "$WORK/scripts/check-gate-inputs.py"
probe G4 "рост долга сверх потолка"
# Потолок возвращается из копии, а не вписывается числом: второе место с той же
# величиной разошлось бы с боевым при первом же понижении долга.
mv "$WORK/scripts/check-gate-inputs.py.bak" "$WORK/scripts/check-gate-inputs.py"
rm -f "$WORK/scripts/check-debt-one.py" "$WORK/scripts/check-debt-two.py"

# Строка реестра неизвестного вида - отказ: молчаливый пропуск такой строки
# снял бы обязанность с названной в ней проверки.
cat >"$BASE" <<'REG'
исключение check-tool.sh - судит по коду возврата чужого инструмента
разрешено check-with.py - вид, которого нет
REG
if run; then fail "G5 строка неизвестного вида принята"; fi
grep -q "не разобрана" "$WORK/out" || fail "G5 отказ не назвал причину: $(cat "$WORK/out")"
echo "  + G5 строка реестра неизвестного вида отвергнута"

# Проверка подчиняется собственному правилу: дерево без проверок - её отказ.
# Корень берётся отдельный и пустой, а сама проверка запускается из своего
# каталога: иначе она нашла бы в перечне себя, и вход перестал бы быть пустым.
mkdir -p "$WORK/empty/scripts"
: >"$WORK/empty/scripts/gate-input-baseline.txt"
if ( cd "$WORK" && GI_ROOT="$WORK/empty" python3 scripts/check-gate-inputs.py >"$WORK/out" 2>&1 ); then
    fail "G6 дерево без проверок принято"
fi
grep -q "проверки дерева" "$WORK/out" || fail "G6 отказ не назвал свой вход: $(cat "$WORK/out")"
echo "  + G6 пустой перечень проверок - отказ"

echo "Контрольная проба границ входа: все пробы пройдены"

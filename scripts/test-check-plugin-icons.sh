#!/usr/bin/env bash
# Проба сверки значков плагина со значком страницы.
#
# Сверка, молчащая на испорченном входе, ничего не проверяет: здесь она гоняется
# на копии дерева (PI_ROOT) - на правке значка руками, на пропаже значка и на
# значке страницы без корня, а согласованные значки обязана принять.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

echo "Проба сверки значков плагина..."

RES="$WORK/extensions/intellij-takt/src/main/resources"
mkdir -p "$WORK/web/static" "$RES"
cp "$ROOT/web/static/favicon.svg" "$WORK/web/static/favicon.svg"

fail() { echo "  ПРОВАЛ: $1"; exit 1; }
check() { PI_ROOT="$WORK" python3 "$ROOT/scripts/build-plugin-icons.py" --check >/dev/null 2>&1; }

PI_ROOT="$WORK" python3 "$ROOT/scripts/build-plugin-icons.py" >/dev/null
check || fail "I0 согласованные значки отвергнуты"
grep -q 'width="16" height="16"' "$RES/icons/takt.svg" || fail "I0 значок файла не 16 точек"
grep -q 'width="40" height="40"' "$RES/META-INF/pluginIcon.svg" || fail "I0 значок плагина не 40 точек"
echo "  I0: согласованные значки приняты, стороны 16 и 40"

cp "$RES/icons/takt.svg" "$WORK/keep.svg"
sed -i.bak 's/#1f6f4a/#1E88E5/' "$RES/icons/takt.svg"
check && fail "I1 правка значка руками не поймана"
echo "  I1: правка значка руками ловится"
cp "$WORK/keep.svg" "$RES/icons/takt.svg"

rm "$RES/META-INF/pluginIcon.svg"
check && fail "I2 пропажа значка не поймана"
echo "  I2: пропажа значка ловится"
PI_ROOT="$WORK" python3 "$ROOT/scripts/build-plugin-icons.py" >/dev/null

printf '<!-- без корня -->\n' > "$WORK/web/static/favicon.svg"
check && fail "I3 значок страницы без корня принят"
echo "  I3: значок страницы без корня отвергнут"

echo "Проба сверки значков плагина: все случаи пройдены"

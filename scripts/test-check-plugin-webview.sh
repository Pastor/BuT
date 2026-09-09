#!/usr/bin/env bash
# Проба проверки ресурсов панели плагина.
#
# Проверка, молчащая на испорченном входе, ничего не проверяет: здесь она гоняется
# на правке файла руками, на пропаже файла и на лишнем файле, а согласованное
# дерево обязана принять.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEST="$ROOT/extensions/intellij-takt/src/main/resources/webview"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"; cp -R "$WORK.keep/." "$DEST" 2>/dev/null || true; rm -rf "$WORK.keep"' EXIT

echo "Проба проверки ресурсов панели плагина..."

# Снимок ресурсов: проба правит их на месте и обязана вернуть как было.
mkdir -p "$WORK.keep"
cp -R "$DEST/." "$WORK.keep/"

fail() { echo "  ПРОВАЛ: $1"; exit 1; }

python3 "$ROOT/scripts/check-plugin-webview.py" >/dev/null \
  || fail "W0 согласованное дерево отвергнуто"
echo "  W0: согласованное дерево принято"

printf '\n/* правка руками */\n' >> "$DEST/app.css"
if python3 "$ROOT/scripts/check-plugin-webview.py" >/dev/null 2>&1; then
  fail "W1 правка файла руками не поймана"
fi
echo "  W1: правка файла руками ловится"
cp "$WORK.keep/app.css" "$DEST/app.css"

rm "$DEST/panels.js"
if python3 "$ROOT/scripts/check-plugin-webview.py" >/dev/null 2>&1; then
  fail "W2 пропажа файла не поймана"
fi
echo "  W2: пропажа файла ловится"
cp "$WORK.keep/panels.js" "$DEST/panels.js"

printf '// чужой файл\n' > "$DEST/stray.js"
if python3 "$ROOT/scripts/check-plugin-webview.py" >/dev/null 2>&1; then
  fail "W3 лишний файл не пойман"
fi
echo "  W3: лишний файл ловится"
rm "$DEST/stray.js"

python3 "$ROOT/scripts/check-plugin-webview.py" >/dev/null \
  || fail "W4 восстановленное дерево отвергнуто"
echo "  W4: восстановленное дерево принято"

echo "Проба проверки ресурсов панели плагина: все случаи пройдены"

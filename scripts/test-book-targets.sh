#!/bin/sh
# Тест проверки примеров book/ под инструментами целей.
#
# Проверка без теста доказывает лишь то, что он запустился. Здесь проверяются оба
# условия: "согласованное дерево принимается" и "вывод, отвергнутый
# инструментом, ловится". Прогон идёт на копии дерева (переменная `BT_ROOT`),
# рабочие файлы не трогаются.
#
# Порча вносится не в исходник, а в вывод: обёртка над `taktc` дописывает в
# порождённый C заведомо неверную строку. Так проверяется ровно предмет проверки -
# "цель напечатала файл, а её инструмент этот файл отверг", - а не поведение
# самой цели на кривом входе (там был бы законный отказ цели, который проверка
# пропускает намеренно).
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-book-targets.sh"
TAKTC="${TAKTC:-$ROOT/target/precheck/debug/taktc}"

if [ ! -x "$TAKTC" ]; then
    echo "test-book-targets: не найден компилятор $TAKTC" >&2
    exit 1
fi
command -v cc >/dev/null 2>&1 || {
    echo "[ПРОПУСК] сторож гейта примеров book/: `cc` не найден"
    exit 0
}

echo "Сторож гейта примеров book/ (фича 0513)..."

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Копия дерева: один пример - тесту нужен предмет, а не объём.
mkdir -p "$TMP/tree/book/src/probe/examples"
cat > "$TMP/tree/book/src/probe/examples/probe.takt" <<'TAKT'
out value: u8 at 0x100;
var t: u8 := 0;

start Run {
    always {
        t := t + 1;
        value := t;
    }
    ref Run;
}
TAKT

# 1. Согласованное дерево принимается.
if BT_ROOT="$TMP/tree" TAKTC="$TAKTC" "$GATE" >"$TMP/ok.log" 2>&1; then
    echo "  OK: согласованное дерево принимается"
else
    echo "  ОШИБКА: гейт отверг дерево, которое инструменты принимают:" >&2
    sed 's/^/    /' "$TMP/ok.log" >&2
    exit 1
fi

# 2. Вывод, отвергнутый инструментом, ловится.
cat > "$TMP/spoil-taktc" <<SPOIL
#!/bin/sh
"$TAKTC" "\$@" || exit \$?
for f in \$(find "\$TMP" -name '*.c' 2>/dev/null); do :; done
# Каталог вывода - аргумент после -o.
OUT=""
prev=""
for a in "\$@"; do
    [ "\$prev" = "-o" ] && OUT="\$a"
    prev="\$a"
done
[ -n "\$OUT" ] || exit 0
for f in "\$OUT"/*.c; do
    [ -e "\$f" ] || continue
    printf 'int takt_gate_probe(void) { int x; return x; }\n' >> "\$f"
done
SPOIL
chmod +x "$TMP/spoil-taktc"

if BT_ROOT="$TMP/tree" TAKTC="$TMP/spoil-taktc" "$GATE" >"$TMP/bad.log" 2>&1; then
    echo "  ОШИБКА: гейт принял вывод, который отвергает cc" >&2
    sed 's/^/    /' "$TMP/bad.log" >&2
    exit 1
fi
if grep -q "ОТВЕРГ" "$TMP/bad.log"; then
    echo "  OK: вывод, отвергнутый инструментом, ловится"
else
    echo "  ОШИБКА: отказ гейта не назвал инструмент:" >&2
    sed 's/^/    /' "$TMP/bad.log" >&2
    exit 1
fi

# 3. Отказ самой цели проверка не считает ошибкой (границы цели названы).
mkdir -p "$TMP/limits/book/src/probe/examples"
cat > "$TMP/limits/book/src/probe/examples/limits.takt" <<'TAKT'
extern fn read_sensor() -> u8;

out value: u8 at 0x100;

start Run {
    always {
        value := read_sensor();
    }
    ref Run;
}
TAKT
if BT_ROOT="$TMP/limits" TAKTC="$TAKTC" "$GATE" >"$TMP/lim.log" 2>&1; then
    echo "  OK: отказ самой цели гейт пропускает (границы цели)"
else
    echo "  ОШИБКА: гейт принял отказ цели за дефект:" >&2
    sed 's/^/    /' "$TMP/lim.log" >&2
    exit 1
fi

echo "  Сторож гейта примеров book/ пройден."

#!/bin/sh
# Тест проверки команд README: мутацией доказывает, что негодная
# команда ловится, а годная принимается.
#
# Повод: проверка, который никогда не падал, неотличим от проверки, который не
# смотрит. Первый же прогон настоящего проверки нашёл в README команду, роняющую
# компилятор паникой, - но это находка, а не доказательство работоспособности.
#
# Гоняется на копии дерева (RC_ROOT): рабочий README не трогается.
#
# POSIX sh (образец - scripts/test-book-generated.sh).
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-readme-commands.sh"
TAKTC="$ROOT/target/precheck/debug/taktc"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

echo "Сторож гейта команд README (фича 0275)..."

# Копия дерева: README, примеры и скрипты - всё, что читает проверка.
mkdir -p "$TMP/tree/scripts" "$TMP/tree/examples"
cp "$ROOT/README.md" "$TMP/tree/README.md"
cp "$ROOT/examples"/*.takt "$TMP/tree/examples/" 2>/dev/null || true
cp "$ROOT/examples"/*.map "$TMP/tree/examples/" 2>/dev/null || true
cp "$ROOT/scripts"/*.sh "$TMP/tree/scripts/" 2>/dev/null || true
cp "$ROOT/scripts"/*.py "$TMP/tree/scripts/" 2>/dev/null || true
# Манифест порождённого Rust-проекта: README предлагает `cargo run
# --manifest-path examples/generated/rust/Cargo.toml`, и без него копия дерева
# была бы беднее рабочего - тест ругался бы на настоящий README.
mkdir -p "$TMP/tree/examples/generated/rust"
cp "$ROOT/examples/generated/rust/Cargo.toml" "$TMP/tree/examples/generated/rust/" 2>/dev/null || true

run_gate() { RC_ROOT="$TMP/tree" TAKTC="$TAKTC" sh "$GATE" >"$TMP/out" 2>&1; }

# --- 1. Настоящий README принимается ----------------------------------------
if run_gate; then
    ok "команды README работают"
else
    fail "рабочий README отвергнут: $(cat "$TMP/out")"
fi

# --- 2. Несуществующий путь крейта ловится ----------------------------------
# Ровно тот класс, ради которого фича заведена: `--path grammar` пережил
# переименование крейта на полтора десятка фич.
printf '\n```sh\ncargo install --path grammar --bin taktc\n```\n' >> "$TMP/tree/README.md"
if ! run_gate && grep -q 'пути нет в дереве' "$TMP/out"; then
    ok "устаревший путь крейта ловится"
else
    fail "устаревший --path принят: $(cat "$TMP/out")"
fi
cp "$ROOT/README.md" "$TMP/tree/README.md"

# --- 3. Неизвестная подкоманда ловится --------------------------------------
printf '\n```sh\ntaktc translate examples/stacker.takt -o out/\n```\n' >> "$TMP/tree/README.md"
if ! run_gate && grep -q 'неизвестная подкоманда' "$TMP/out"; then
    ok "неизвестная подкоманда ловится"
else
    fail "неизвестная подкоманда принята: $(cat "$TMP/out")"
fi
cp "$ROOT/README.md" "$TMP/tree/README.md"

# --- 4. Неработающая команда компилятора ловится ----------------------------
# Файл существует, значит команда прогоняется - и падает на несуществующей цели.
printf '\n```sh\ntaktc compile -t nosuchtarget examples/stacker.takt -o out/\n```\n' >> "$TMP/tree/README.md"
if ! run_gate && grep -q 'команда README не работает' "$TMP/out"; then
    ok "неработающая команда компилятора ловится"
else
    fail "неработающая команда принята: $(cat "$TMP/out")"
fi
cp "$ROOT/README.md" "$TMP/tree/README.md"

# --- 5. Несуществующий скрипт ловится ---------------------------------------
printf '\n```sh\nscripts/nosuch-script.sh --full\n```\n' >> "$TMP/tree/README.md"
if ! run_gate && grep -q 'скрипта нет в дереве' "$TMP/out"; then
    ok "несуществующий скрипт ловится"
else
    fail "несуществующий скрипт принят: $(cat "$TMP/out")"
fi
cp "$ROOT/README.md" "$TMP/tree/README.md"

# --- 6. Контроль: команда с плейсхолдером не прогоняется ---------------------
# `model.takt` в дереве нет - такую строку исполнить нечем, и проверка обязан
# ограничиться проверкой подкоманды, а не падать.
printf '\n```sh\ntaktc compile -t c model.takt -o build/\n```\n' >> "$TMP/tree/README.md"
if run_gate; then
    ok "команда с плейсхолдером не роняет гейт"
else
    fail "плейсхолдер принят за настоящий файл: $(cat "$TMP/out")"
fi

[ "$FAILED" -eq 0 ] || { echo "  Сторож гейта команд README провален." >&2; exit 1; }
echo "  Сторож гейта команд README пройден."

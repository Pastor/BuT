#!/bin/sh
# Тест проверки расширения Zed.
#
# Проверяет предмет проверки, а не запуск: каждое из двух условий ловится по
# отдельности, и здоровое дерево проходит. Проверка, который никогда не падал,
# неотличим от проверки, который не смотрит.
#
# Работает на копии дерева (`ZE_ROOT`), рабочие файлы не трогает.
#
# POSIX sh.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TOOL="$ROOT/scripts/check-zed-extension.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

echo "Сторож гейта расширения Zed (фича 0414)..."

# Синтетическое дерево: только то, что читает проверка.
make_tree() {
    targets="$1"; target_dir="$2"
    rm -rf "$TMP/tree"
    mkdir -p "$TMP/tree/scripts" "$TMP/tree/extensions/zed-takt/.cargo"
    {
        echo '[toolchain]'
        echo 'channel = "1.97.1"'
        [ -n "$targets" ] && echo "targets = [$targets]"
    } > "$TMP/tree/rust-toolchain.toml"
    if [ -n "$target_dir" ]; then
        printf '[build]\ntarget-dir = "%s"\n' "$target_dir" \
            > "$TMP/tree/extensions/zed-takt/.cargo/config.toml"
    fi
}

run() { ZE_ROOT="$TMP/tree" "$TOOL" >"$TMP/out" 2>&1; }

# --- Условие 1: здоровое дерево проходит -------------------------------------
make_tree '"wasm32-wasip1", "wasm32-wasip2"' 'target'
if run; then ok "здоровое дерево принимается"
else fail "здоровое дерево отвергнуто:$(printf '\n%s' "$(cat "$TMP/out")")"; fi

# --- Условие 2: пропавший таргет ловится (Z1) --------------------------------
make_tree '"wasm32-wasip1"' 'target'
if run; then fail "отсутствие wasm32-wasip2 не поймано"
else ok "Z1: отсутствие таргета ловится"; fi

# --- Условие 3: таргетов нет вовсе (вырожденный вход) ------------------------
make_tree '' 'target'
if run; then fail "пин без таргетов не пойман"
else ok "Z1: пин без таргетов ловится"; fi

# --- Условие 4: нет своего каталога сборки (Z2) ------------------------------
make_tree '"wasm32-wasip1", "wasm32-wasip2"' ''
if run; then fail "отсутствие .cargo/config.toml расширения не поймано"
else ok "Z2: отсутствие своего каталога сборки ловится"; fi

# --- Условие 5: чужой каталог сборки ловится (Z2) ----------------------------
# Именно эта форма и была на диске: корневой конфиг уводил wasm в target/precheck.
make_tree '"wasm32-wasip1", "wasm32-wasip2"' 'target/precheck'
if run; then fail "чужой target-dir не пойман"
else ok "Z2: чужой target-dir ловится"; fi

# --- Условие 6: без расширения проверка молчит -----------------------------------
make_tree '"wasm32-wasip1", "wasm32-wasip2"' 'target'
rm -rf "$TMP/tree/extensions"
if run; then ok "дерево без расширения пропускается"
else fail "дерево без расширения отвергнуто"; fi

if [ "$FAILED" -ne 0 ]; then
    echo "Сторож гейта расширения Zed: есть провалы." >&2
    exit 1
fi
echo "Сторож гейта расширения Zed: все проверки пройдены."

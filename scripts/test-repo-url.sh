#!/bin/sh
# Тест проверки адреса репозитория.
#
# `check-repo-url.sh` сверяет адрес во всех местах, где он исполняется (фича
# Проверялось это ничем: проверка, который никогда не падал, неотличим от
# проверки, который не смотрит - а его собственный повод был именно
# таким: строка `cd <имя>` после `git clone` ломается независимо от редиректа
# GitHub, и сломанной она может прожить долго.
#
# Работает на копии дерева (`RU_ROOT`).
#
# POSIX sh.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TOOL="$ROOT/scripts/check-repo-url.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

echo "Сторож гейта адреса репозитория (фича 0315)..."

# Синтетическое дерево: манифесты, README и файлы расширений, которые читает проверка.
make_tree() {
    slug="$1"; other_slug="$2"; cd_name="$3"
    rm -rf "$TMP/tree"
    mkdir -p "$TMP/tree/takt-lang/src" "$TMP/tree/takt-sim/src" \
             "$TMP/tree/extensions/zed-takt/scripts" \
             "$TMP/tree/extensions/intellij-takt/src/main/resources/META-INF"
    printf 'repository = "https://github.com/%s"\n' "$slug" > "$TMP/tree/takt-lang/Cargo.toml"
    printf 'repository = "https://github.com/%s"\n' "$other_slug" > "$TMP/tree/takt-sim/Cargo.toml"
    printf 'git clone https://github.com/%s\ncd %s\n' "$slug" "$cd_name" > "$TMP/tree/README.md"
    printf 'repository = "https://github.com/%s"\n' "$slug" \
        > "$TMP/tree/extensions/zed-takt/extension.toml"
    printf '# https://github.com/%s\n' "$slug" \
        > "$TMP/tree/extensions/zed-takt/scripts/install.sh"
    printf '<url>https://github.com/%s</url>\n' "$slug" \
        > "$TMP/tree/extensions/intellij-takt/src/main/resources/META-INF/plugin.xml"
    : > "$TMP/tree/takt-lang/src/lib.rs"
    : > "$TMP/tree/takt-sim/src/lib.rs"
}

# --- 1. Согласованное дерево принимается --------------------------------------
make_tree "Owner/Repo" "Owner/Repo" "Repo"
if RU_ROOT="$TMP/tree" sh "$TOOL" >"$TMP/out" 2>&1; then
    ok "согласованное дерево принимается"
else
    fail "согласованное дерево отвергнуто: $(cat "$TMP/out")"
fi

# --- 2. Разошедшийся адрес ловится --------------------------------------------
make_tree "Owner/Repo" "Owner/Stale" "Repo"
if ! RU_ROOT="$TMP/tree" sh "$TOOL" >"$TMP/out" 2>&1; then
    ok "разошедшийся адрес ловится"
else
    fail "разошедшийся адрес НЕ пойман: $(cat "$TMP/out")"
fi

# --- 3. Отставший `cd` после клонирования ловится ------------------------------
# Ровно та строка, ради которой проверка и заводился: она ломается независимо от
# редиректа GitHub - клон нового URL создаёт каталог с новым именем.
make_tree "Owner/Repo" "Owner/Repo" "OldName"
if ! RU_ROOT="$TMP/tree" sh "$TOOL" >"$TMP/out" 2>&1; then
    ok "отставший 'cd <имя>' ловится"
else
    fail "отставший cd НЕ пойман: $(cat "$TMP/out")"
fi

# --- 4. Адрес в doc-ссылке исходников тоже сверяется ---------------------------
make_tree "Owner/Repo" "Owner/Repo" "Repo"
printf '//! См. https://github.com/Owner/Stale\n' > "$TMP/tree/takt-lang/src/lib.rs"
if ! RU_ROOT="$TMP/tree" sh "$TOOL" >"$TMP/out" 2>&1; then
    ok "doc-ссылка исходников сверяется"
else
    fail "doc-ссылка не проверена: $(cat "$TMP/out")"
fi

# --- 5. Пропавший проверяемый файл - отказ, а не молчание ----------------------
make_tree "Owner/Repo" "Owner/Repo" "Repo"
rm "$TMP/tree/extensions/zed-takt/extension.toml"
if ! RU_ROOT="$TMP/tree" sh "$TOOL" >"$TMP/out" 2>&1; then
    ok "пропавший файл ловится"
else
    fail "пропавший файл принят за успех: $(cat "$TMP/out")"
fi

[ "$FAILED" -eq 0 ] || { echo "  Сторож гейта адреса: ПРОВАЛ" >&2; exit 1; }
echo "  Сторож гейта адреса: все проверки пройдены"

#!/bin/sh
# Тест проверки временных каталогов тестов.
#
# Проверяет предмет проверки: каждый из четырёх классов ловится по отдельности, и
# здоровое дерево проходит. Работает на копии дерева (`TD_ROOT`).
#
# Каталог процесса (`takt_pid{}`) общий у всех файлов по замыслу - он и
# разводит два прогона. Условие 8 проверяет именно это: проверка не
# вправе объявить пересечением собственное лекарство.
#
# POSIX sh.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TOOL="$ROOT/scripts/check-test-temp-dirs.py"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

echo "Сторож гейта временных каталогов тестов (фикс 0190-01)..."

tree() {
    rm -rf "$TMP/tree"
    mkdir -p "$TMP/tree/takt-sim/tests/conformance"
}

run() { TD_ROOT="$TMP/tree" python3 "$TOOL" >"$TMP/out" 2>&1; }

# --- Условие 1: здоровое дерево принимается --------------------------------
tree
cat > "$TMP/tree/takt-sim/tests/conformance/a_tests.rs" <<'RS'
fn build_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current().name().unwrap_or("t").replace(':', "_");
    std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_a_{tag}_{thread}"))
}
RS
cat > "$TMP/tree/takt-sim/tests/conformance/b_tests.rs" <<'RS'
fn dir() -> std::path::PathBuf {
    std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join("takt_b_only")
}
RS
if run; then ok "здоровое дерево принимается"
else fail "здоровое дерево отвергнуто:$(printf '\n%s' "$(cat "$TMP/out")")"; fi

# --- Условие 2: общий литеральный каталог (D1) ------------------------------
tree
for f in a b; do
    printf 'fn d() -> std::path::PathBuf { std::env::temp_dir().join(format!("takt_pid{}", std::process::id())).join("takt_same") }\n' \
        > "$TMP/tree/takt-sim/tests/conformance/${f}_tests.rs"
done
if run; then fail "D1: общий литеральный каталог не пойман"
else ok "D1: общий литеральный каталог ловится"; fi

# --- Условие 3: общий шаблон без имени потока (D2) --------------------------
tree
for f in a b; do
    printf 'fn d(tag: &str) -> std::path::PathBuf { std::env::temp_dir().join(format!("takt_pid{}", std::process::id())).join(format!("takt_sv_{tag}")) }\n' \
        > "$TMP/tree/takt-sim/tests/conformance/${f}_tests.rs"
done
if run; then fail "D2: общий шаблон не пойман"
else ok "D2: общий шаблон без имени потока ловится"; fi

# --- Условие 4: тот же шаблон, но С именем потока - законен -----------------
tree
for f in a b; do
    cat > "$TMP/tree/takt-sim/tests/conformance/${f}_tests.rs" <<'RS'
fn d(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current().name().unwrap_or("t").replace(':', "_");
    std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_sv_{tag}_{thread}"))
}
RS
done
if run; then ok "имя потока снимает пересечение шаблонов"
else fail "шаблон с именем потока отвергнут:$(printf '\n%s' "$(cat "$TMP/out")")"; fi

# --- Условие 5: дубль тега в файле без имени потока (D3) --------------------
tree
cat > "$TMP/tree/takt-sim/tests/conformance/a_tests.rs" <<'RS'
fn build_dir(tag: &str) -> std::path::PathBuf {
    std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_a_{tag}"))
}
fn one() { let _ = build_dir("trace"); }
fn two() { let _ = build_dir("trace"); }
RS
if run; then fail "D3: дубль тега не пойман"
else ok "D3: дубль тега без имени потока ловится"; fi

# --- Условие 6: каталог без идентификатора процесса (D4) --------------------
#
# Класс воспроизведён 2026-08-23 двумя копиями тестового бинарника: имя потока
# уникально внутри процесса, и второй прогон сносит каталог первого прямо во
# время сборки verilator.
tree
cat > "$TMP/tree/takt-sim/tests/conformance/a_tests.rs" <<'RS'
fn build_dir(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current().name().unwrap_or("t").replace(':', "_");
    std::env::temp_dir().join(format!("takt_a_{tag}_{thread}"))
}
RS
if run; then fail "D4: каталог без идентификатора процесса не пойман"
else ok "D4: каталог без идентификатора процесса ловится"; fi

# --- Условие 7: отказ D4 называет класс и показывает лекарство --------------
if grep -q "D4:" "$TMP/out" && grep -q "takt_pid" "$TMP/out"; then
    ok "отказ D4 называет класс и форму лекарства"
else
    fail "отказ D4 не называет класс либо не показывает форму"
fi

# --- Условие 8: общий каталог процесса пересечением не считается ------------
#
# Он общий у всех файлов по замыслу: проверка, объявивший его пересечением, запретил
# бы собственное лекарство.
tree
for f in a b; do
    cat > "$TMP/tree/takt-sim/tests/conformance/${f}_tests.rs" <<RS
fn d(tag: &str) -> std::path::PathBuf {
    let thread = std::thread::current().name().unwrap_or("t").replace(':', "_");
    std::env::temp_dir()
        .join(format!("takt_pid{}", std::process::id()))
        .join(format!("takt_${f}_{tag}_{thread}"))
}
RS
done
if run; then ok "общий каталог процесса пересечением не считается"
else fail "каталог процесса объявлен пересечением:$(printf '\n%s' "$(cat "$TMP/out")")"; fi

# --- Условие 9: пустое дерево не роняет проверка --------------------------------
tree
if run; then ok "дерево без тестов пропускается"
else fail "пустое дерево отвергнуто"; fi

if [ "$FAILED" -ne 0 ]; then
    echo "Сторож гейта временных каталогов: есть провалы." >&2
    exit 1
fi
echo "Сторож гейта временных каталогов: все проверки пройдены."

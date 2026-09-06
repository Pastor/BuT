#!/bin/sh
# Тест проверки метки версии: проверяет, что
# scripts/check-version-tag.sh ловит пропуск тега, различает аннотированный и
# лёгкий, требует достижимости из HEAD и - главное - Молчит в момент подъёма.
#
# Последнее не формальность: тег ставится на коммит, а предкоммит идёт до
# коммита. Проверка, падающий при незакоммиченном подъёме, мешал бы ровно тем
# фичам, которые исполняют верно, - и его бы отключили.
#
# Гоняется в temp-репозитории через VT_ROOT; рабочее дерево не трогает
# (образец - scripts/test-new-feature.sh с NF_ROOT).
#
# POSIX sh.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-version-tag.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

echo "Сторож гейта метки версии (фича 0162)..."

G="git -C $TMP -c user.email=t@t -c user.name=t -c commit.gpgsign=false -c init.defaultBranch=main"

set_version() {   # $1 — версия
    mkdir -p "$TMP/takt-lang/src"
    printf 'pub const LANGUAGE_VERSION: &str = "%s";\n' "$1" > "$TMP/takt-lang/src/version.rs"
}

run_gate() {      # печатает вывод, возвращает код
    ( cd "$TMP" && VT_ROOT="$TMP" PRECHECK_STRICT="${1:-0}" "$GATE" 2>&1 )
}

$G init -q .
set_version 0.1.0
$G add -A >/dev/null
$G commit -q -m "версия 0.1.0"

# --- 1. Тега нет - проверка падает и называет его -------------------------------
# В репозитории обязан быть хоть один тег, иначе сработает ветвь "клон без
# тегов" (проверка 6): различить "тег потерян" и "теги не выкачаны" нечем, и
# проверка по построению выбирает мягкий пропуск. Поэтому ставим посторонний тег -
# так проверяется именно отсутствие тега версии, а не пустота списка.
$G tag -a probe-base -m "посторонний тег" HEAD
OUT="$(run_gate || true)"
if printf '%s' "$OUT" | grep -q 'тега v0.1.0 нет' && ! run_gate >/dev/null 2>&1; then
    ok "пропуск тега пойман, тег назван"
else
    fail "пропуск тега НЕ пойман: $OUT"
fi

# --- 2. Аннотированный тег на коммите - проверка зелен --------------------------
$G tag -a v0.1.0 -m "версия 0.1.0" HEAD
if run_gate >/dev/null 2>&1; then
    ok "аннотированный тег на коммите принимается"
else
    fail "верное состояние отвергнуто: $(run_gate || true)"
fi

# --- 3. Лёгкий тег - отказ (метка границы без автора и даты) ----------------
$G tag -d v0.1.0 >/dev/null
$G tag v0.1.0 HEAD
OUT="$(run_gate || true)"
if printf '%s' "$OUT" | grep -q 'лёгкий тег' && ! run_gate >/dev/null 2>&1; then
    ok "лёгкий тег отвергнут"
else
    fail "лёгкий тег принят: $OUT"
fi
$G tag -d v0.1.0 >/dev/null
$G tag -a v0.1.0 -m "версия 0.1.0" HEAD

# --- 4. Подъём не закоммичен - проверка молчит ----------------------------------
set_version 0.2.0
OUT="$(run_gate || true)"
if printf '%s' "$OUT" | grep -q 'ПРОПУСК: подъём 0.1.0 → 0.2.0' && run_gate >/dev/null 2>&1; then
    ok "незакоммиченный подъём не мешает (гейт молчит, код 0)"
else
    fail "гейт мешает подъёму: $OUT"
fi
# ...и подсказывает команду тега
printf '%s' "$OUT" | grep -q 'git tag -a v0.2.0' \
    && ok "подсказка называет команду тега" \
    || fail "подсказки с командой тега нет"
set_version 0.1.0

# --- 5. Тег на коммите вне HEAD - отказ -------------------------------------
$G tag -d v0.1.0 >/dev/null
$G checkout -q -b side
printf 'побочная правка\n' > "$TMP/side.txt"
$G add -A >/dev/null
$G commit -q -m "побочный коммит"
$G tag -a v0.1.0 -m "версия 0.1.0" HEAD
$G checkout -q main
OUT="$(run_gate || true)"
if printf '%s' "$OUT" | grep -q 'не достижим' && ! run_gate >/dev/null 2>&1; then
    ok "тег вне истории HEAD отвергнут"
else
    fail "тег вне истории принят: $OUT"
fi
$G tag -d v0.1.0 >/dev/null
$G tag -a v0.1.0 -m "версия 0.1.0" main

# --- 6. Клон без тегов: мягкий пропуск, а под строгим режимом - ошибка -------
$G tag -d v0.1.0 >/dev/null
$G tag -d probe-base >/dev/null
if run_gate 0 >/dev/null 2>&1; then
    ok "без тегов — мягкий пропуск"
else
    fail "без тегов гейт упал в нестрогом режиме"
fi
OUT="$(run_gate 1 || true)"
if printf '%s' "$OUT" | grep -q 'нет ни одного тега' && ! run_gate 1 >/dev/null 2>&1; then
    ok "без тегов под PRECHECK_STRICT=1 — ошибка"
else
    fail "строгий режим не отличается от мягкого: $OUT"
fi

[ "$FAILED" -eq 0 ] || { echo "  Сторож гейта метки версии провален." >&2; exit 1; }
echo "  Сторож гейта метки версии пройден."

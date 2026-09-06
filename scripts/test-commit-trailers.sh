#!/bin/sh
# Тест проверки трейлеров: проверяет, что проверка ловит трейлер в
# любом написании, молчит на чистой истории и падает списком.
#
# Гоняется в temp-репозитории через CT_REV и `git -C`; рабочая история не
# трогается.
#
# POSIX sh.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-commit-trailers.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0
ok()   { echo "  OK: $1"; }
fail() { echo "  ПРОВАЛ: $1" >&2; FAILED=1; }

echo "Сторож гейта трейлеров коммита (правило 31)..."

G="git -C $TMP -c user.email=t@t -c user.name=t -c commit.gpgsign=false -c init.defaultBranch=main"
$G init -q .
printf 'x\n' > "$TMP/a.txt"; $G add -A >/dev/null
$G commit -q -m "первый коммит

Feature: 0001"

run() { ( cd "$TMP" && "$GATE" 2>&1 ); }
code() { ( cd "$TMP" && "$GATE" >/dev/null 2>&1 ); }

# --- 1. Чистая история - проверка зелен -----------------------------------------
if code; then ok "чистая история принимается"; else fail "ложная тревога: $(run)"; fi

# --- 2. Трейлер ловится ------------------------------------------------------
printf 'y\n' >> "$TMP/a.txt"; $G add -A >/dev/null
$G commit -q -m "второй коммит

Feature: 0002

Co-Authored-By: Кто-то <someone@example.com>"
if ! code && run | grep -q 'второй коммит'; then
    ok "трейлер пойман, коммит назван"
else
    fail "трейлер НЕ пойман: $(run)"
fi

# --- 3. Написание вразнобой ловится тоже ------------------------------------
# Инструменты пишут его по-разному; проверка, чувствительная к регистру,
# пропустила бы половину.
printf 'z\n' >> "$TMP/a.txt"; $G add -A >/dev/null
$G commit -q -m "третий коммит

co-authored-by: другой <other@example.com>"
if ! code; then ok "нижний регистр ловится"; else fail "нижний регистр пропущен"; fi

# --- 4. Падение списком ------------------------------------------------------
HITS="$(run | grep -cE '^    [0-9a-f]{40} ' || true)"
if [ "$HITS" -ge 2 ]; then
    ok "падает списком (названо коммитов: $HITS)"
else
    fail "названо коммитов: $HITS, ожидалось не менее 2"
fi

# --- 5. Похожий, но законный текст тревоги не даёт ---------------------------
# Контрольный вход: без него правило можно было бы "исполнить"
# запретом любого упоминания слова "author".
$G checkout -q --orphan clean
$G rm -rqf . >/dev/null 2>&1 || true
printf 'x\n' > "$TMP/b.txt"; $G add -A >/dev/null
$G commit -q -m "коммит про авторство

В теле упомянуты authors и co-author как слова, но трейлера нет.

Feature: 0003"
if code; then ok "похожий текст без трейлера тревоги не даёт"; else fail "ложная тревога: $(run)"; fi

[ "$FAILED" -eq 0 ] || { echo "  Сторож гейта трейлеров провален." >&2; exit 1; }
echo "  Сторож гейта трейлеров пройден."

#!/bin/sh
# Проверка единственности адреса репозитория.
#
# Правило: адрес репозитория в дереве один. Все вхождения
# `github.com/<владелец>/<имя>` в рабочих файлах обязаны называть то же имя, что
# и эталон - поле `repository` манифеста `takt-lang/Cargo.toml`.
#
# Повод: переезд `BuT` -> `Takt`  оставил старое имя в 16 местах -
# метаданных обоих крейтов, команде `git clone` из README, doc-ссылках
# исходников и манифестах обоих плагинов. Ничего не сломалось только потому, что
# GitHub держит редирект 301; исключение - `cd BuT` сразу после `git clone`,
# который не работал независимо от редиректа. Прецедент: правило,
# которое нельзя проверить командой, - не правило.
#
# Граница Проверки (A-2 ): проверяется согласованность, а не
# Правильность. Если переименовать репозиторий во всех местах разом на
# несуществующее имя, проверка промолчит - правильность адреса есть знание вне
# репозитория, и установить её машинно можно только походом в сеть, а
# предкоммит обязан работать офлайн. Тест этой границы - проверка A4
# тест-плана 0179.
#
# Проверяются рабочие файлы, а не "всё, кроме исключений". планировал
# denylist исторических артефактов, но реализация показала: цитируют старый
# адрес все повествовательные файлы - карточки и самой фичи, CHANGES.md,
# FEATURES.md, - и denylist пришлось бы дополнять при каждом упоминании. Поэтому
# взят allowlist: перечислены места, где адрес исполняется (манифесты,
# инструкции, doc-ссылки, скрипты), а `docs/` и журналы не проверяются - там
# старый адрес есть свидетельство, а не ошибка.
#
# Сверяются только адреса владельца эталона. Первая редакция проверки сравнивала
# любой `github.com/*/*` и закономерно упала на `lalrpop/lalrpop` - git-зависимости
# в том же манифесте. Чужой репозиторий называется как называется; правило
# "адрес один" касается своего репозитория, поэтому владелец берётся из эталона.
#
# POSIX sh, без внешних зависимостей (образец - scripts/check-language-version.sh).
set -eu

# Корень переопределяется переменной - для теста проверки.
ROOT="${RU_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
MANIFEST="$ROOT/takt-lang/Cargo.toml"
README="$ROOT/README.md"

[ -f "$MANIFEST" ] || { echo "check-repo-url: не найден $MANIFEST" >&2; exit 1; }
[ -f "$README" ]   || { echo "check-repo-url: не найден $README" >&2; exit 1; }

echo "Гейт адреса репозитория: единственность (фича 0179)..."

# --- Эталон: поле repository манифеста takt-lang ---------------------------
REF_URL="$(grep -oE '^repository *= *"https://github\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+"' "$MANIFEST" \
    | grep -oE 'https://github\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+' | head -n1)"
if [ -z "${REF_URL:-}" ]; then
    echo "  ОШИБКА: в $MANIFEST нет поля repository вида" >&2
    echo "    repository = \"https://github.com/<владелец>/<имя>\"" >&2
    exit 1
fi
REF_SLUG="${REF_URL#https://github.com/}"   # владелец/имя
REF_OWNER="${REF_SLUG%%/*}"                  # владелец
REF_NAME="${REF_SLUG#*/}"                    # имя каталога после клонирования

# --- Рабочие файлы: там адрес исполняется ----------------------------------
# Расширять список при появлении нового места, где адрес работает, а не
# упоминается. `docs/` и журналы (CHANGES.md, FEATURES.md) сюда не входят -
# см. заголовок.
FILES="
takt-lang/Cargo.toml
takt-sim/Cargo.toml
README.md
extensions/zed-takt/extension.toml
extensions/zed-takt/scripts/install.sh
extensions/intellij-takt/src/main/resources/META-INF/plugin.xml
"

BAD=0
for REL in $FILES; do
    F="$ROOT/$REL"
    [ -f "$F" ] || { echo "  ОШИБКА: проверяемый файл отсутствует: $REL" >&2; BAD=1; continue; }
    FOUND="$(grep -oE "github\.com/$REF_OWNER/[A-Za-z0-9_.-]+" "$F" | sort -u || true)"
    for SLUG in $FOUND; do
        SLUG="${SLUG#github.com/}"
        SLUG="${SLUG%.git}"
        [ "$SLUG" = "$REF_SLUG" ] && continue
        echo "  ОШИБКА: $REL называет '$SLUG', эталон — '$REF_SLUG'." >&2
        BAD=1
    done
done

# Исходники крейтов: doc-ссылки на файлы репозитория.
for REL in takt-lang/src takt-sim/src; do
    D="$ROOT/$REL"
    [ -d "$D" ] || continue
    FOUND="$(grep -rhoE "github\.com/$REF_OWNER/[A-Za-z0-9_.-]+" "$D" | sort -u || true)"
    for SLUG in $FOUND; do
        SLUG="${SLUG#github.com/}"
        SLUG="${SLUG%.git}"
        [ "$SLUG" = "$REF_SLUG" ] && continue
        echo "  ОШИБКА: doc-ссылка в $REL называет '$SLUG', эталон — '$REF_SLUG'." >&2
        BAD=1
    done
done

# --- Имя каталога в инструкции клонирования --------------------------------
# `cd <имя>` сразу после `git clone ...` обязано совпасть с именем репозитория:
# эта строка ломается независимо от редиректа GitHub (клон нового URL создаёт
# каталог с новым именем) - ровно так она и была сломана до 0179.
CD_AFTER_CLONE="$(grep -A1 -E '^git clone https://github\.com/' "$README" \
    | grep -oE '^cd [A-Za-z0-9_.-]+' | head -n1 | sed 's/^cd //')"
if [ -z "${CD_AFTER_CLONE:-}" ]; then
    echo "  ПРЕДУПРЕЖДЕНИЕ: в $README не найдена пара 'git clone …' + 'cd <имя>' — якорь не проверен." >&2
elif [ "$CD_AFTER_CLONE" != "$REF_NAME" ]; then
    echo "  ОШИБКА: README после 'git clone' делает 'cd $CD_AFTER_CLONE', а клон создаёт '$REF_NAME'." >&2
    BAD=1
fi

if [ "$BAD" -ne 0 ]; then
    echo "  Приведите адрес к эталону из takt-lang/Cargo.toml либо смените эталон (ADR 0179)." >&2
    exit 1
fi

echo "  OK: адрес репозитория один — $REF_SLUG (каталог после клонирования: $REF_NAME)"

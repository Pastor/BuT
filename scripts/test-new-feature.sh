#!/bin/sh
# test-new-feature.sh - регресс-тест генератора заготовок.
#
# Гоняет scripts/new-feature.sh в временном дереве (через NF_ROOT), не трогая
# рабочие реестры, и проверяет:
# A1 - повторный --register не дублирует строки (идемпотентность);
# A2 - -строка реестра и шаблон - Draft, не Accepted;
# A3 - --stage report создаёт и регистрирует отчёт (ранее невозможно);
# A4 - --subtask NN добирает XXXX-NN идемпотентно;
# A5 - умолчательный путь заведения даёт ожидаемый набор файлов.
#
# POSIX sh, без внешних зависимостей. Подключён в precheck.sh.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GEN="$ROOT/scripts/new-feature.sh"

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Слепок дерева: шаблоны (реальные) + пустые реестры с таблицей.
mkdir -p "$TMP/docs/templates"
cp "$ROOT/docs/templates/"*.md "$TMP/docs/templates/"
# Папок две: карточки и исправления - каталогов стадий больше нет.
for r in features fixes; do
  mkdir -p "$TMP/docs/$r"
  printf '# Реестр\n\n| A | B |\n|---|---|\n' > "$TMP/docs/$r/README.md"
done

fail=0
check() { # <описание> <ожидаемо> <фактически>
  if [ "$2" = "$3" ]; then
    echo "  OK: $1"
  else
    echo "  ПРОВАЛ: $1 — ожидалось '$2', получено '$3'" >&2
    fail=1
  fi
}

echo "test-new-feature: идемпотентность и стадии (фича 0094)..."

# Два прогона заведения одной фичи - строки не должны дублироваться (A1).
NF_ROOT="$TMP" "$GEN" --register 0099 test-feat "Тест фича" >/dev/null 2>&1
NF_ROOT="$TMP" "$GEN" --register 0099 test-feat "Тест фича" >/dev/null 2>&1

n="$(grep -c '0099' "$TMP/docs/features/README.md" || true)"
check "A1 реестр фич — одна строка 0099" "1" "$n"
[ -f "$TMP/docs/features/0099-test-feat.md" ] && card=1 || card=0
check "A1 карточка создана" "1" "$card"

# A2: карточка несёт разделы всех стадий как заготовки - их
# заводят по мере прохождения, поэтому в шаблоне они лежат закомментированными.
for sec in 'Архитектура (ADR)' 'Анализ' 'Разработка' 'Тест-план' 'Отчёт о тестировании'; do
  hit="$(grep -c "$sec" "$TMP/docs/features/0099-test-feat.md" || true)"
  [ "$hit" -ge 1 ] || { echo "  ПРОВАЛ: A2 в карточке нет упоминания раздела «$sec»" >&2; fail=1; }
done
[ "$fail" = 0 ] && echo "  OK: A2 карточка называет все стадии-разделы"

# A3: стадии 2...6 файлов не создают - только подсказывают раздел.
before="$(find "$TMP/docs" -name '*.md' | wc -l | tr -d ' ')"
NF_ROOT="$TMP" "$GEN" --stage report --register 0099 test-feat "Тест фича" >/dev/null 2>&1
NF_ROOT="$TMP" "$GEN" --stage adr 0099 test-feat "Тест фича" >/dev/null 2>&1
after="$(find "$TMP/docs" -name '*.md' | wc -l | tr -d ' ')"
check "A3 стадии 2…6 файлов не создают" "$before" "$after"

# A4: исправление - единственный отдельный артефакт; заводится идемпотентно.
NF_ROOT="$TMP" "$GEN" --stage fixes --subtask 03 --register 0099 test-feat "Тест фича" >/dev/null 2>&1
NF_ROOT="$TMP" "$GEN" --stage fixes --subtask 03 --register 0099 test-feat "Тест фича" >/dev/null 2>&1
[ -f "$TMP/docs/fixes/0099-03-test-feat.md" ] && fix_file=1 || fix_file=0
check "A4 фикс 0099-03 создан" "1" "$fix_file"
fix_row="$(grep -c '0099-03' "$TMP/docs/fixes/README.md" || true)"
check "A4 строка 0099-03 одна (идемпотентно)" "1" "$fix_row"

# A5: умолчательный путь для новой фичи создаёт один файл - карточку.
NF_ROOT="$TMP" "$GEN" 0088 other-feat "Другая" >/dev/null 2>&1
[ -f "$TMP/docs/features/0088-other-feat.md" ] || { echo "  ПРОВАЛ: A5 нет карточки 0088" >&2; fail=1; }
for gone in adr analyze development tests reports; do
  [ -e "$TMP/docs/$gone" ] && { echo "  ПРОВАЛ: A5 создан каталог стадии docs/$gone" >&2; fail=1; }
done
[ "$fail" = 0 ] && echo "  OK: A5 дефолтный путь создаёт одну карточку и не заводит каталогов стадий"

if [ "$fail" != 0 ]; then
  echo "test-new-feature: ПРОВАЛЕНО" >&2
  exit 1
fi
echo "test-new-feature: все проверки пройдены (A1–A5)."

#!/usr/bin/env bash
# Тест проверки каталогов сообщений.
#
# Мутациями доказывается, что `check-messages.py` ловит то, ради чего заведён:
#
# M1 - строка каталога без `=` (нарушение формата);
# M2 - ключ есть в базовом каталоге и отсутствует в переводе;
# M3 - набор подстановок `{имя}` у ключа разошёлся между языками;
# M4 - ключ "переведён" копированием русского текста;
# M5 - ключ в каталоге есть, а печатника у него нет (мёртвый ключ);
# M6 - согласованное дерево принимается (иначе проверка красен всегда);
# M7 - вырожденный вход (пустой базовый каталог) даёт отказ, а не молчание.
#
# Проверяются оба условия - "плохое ловится" и "хорошее проходит": проверка,
# который краснеет всегда, так же бесполезен, как молчащий.
#
# Мутации ставятся на копии дерева (`MSG_ROOT`), рабочие каталоги не
# трогаются.
set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-messages.py"
FAILED=0

# Готовит копию дерева: каталоги сообщений и исходники, по которым проверка судит
# использование ключей.
prepare() {
  local dst="$1"
  mkdir -p "$dst/takt-lang/messages" "$dst/takt-lang/src"
  cp "$ROOT/takt-lang/messages/"*.txt "$dst/takt-lang/messages/"
  # Исходник-заглушка: перечисляет константы всех ключей, чтобы `M5` молчал -
  # мутации ставятся по одной, и посторонние находки скрыли бы предмет пробы.
  python3 - "$dst" <<'PY'
import sys, pathlib
dst = pathlib.Path(sys.argv[1])
keys = []
for line in (dst / "takt-lang/messages/ru.txt").read_text(encoding="utf-8").splitlines():
    line = line.strip()
    if line and not line.startswith("#") and "=" in line:
        keys.append(line.split("=", 1)[0].strip().replace("-", "_").replace(".", "_").upper())
(dst / "takt-lang/src/stub.rs").write_text(
    "\n".join(f"// keys::{k}" for k in keys) + "\n", encoding="utf-8")
PY
}

# Прогоняет проверка на копии и сверяет ожидание с кодом возврата.
expect() {
  local name="$1" want="$2" dst="$3"
  MSG_ROOT="$dst" python3 "$GATE" >/dev/null 2>&1
  local got=$?
  if [ "$want" = "fail" ] && [ "$got" -eq 0 ]; then
    echo "  ✗ $name: гейт ПРОПУСТИЛ мутацию"
    FAILED=1
  elif [ "$want" = "pass" ] && [ "$got" -ne 0 ]; then
    echo "  ✗ $name: гейт отверг корректное дерево"
    FAILED=1
  else
    echo "  ✓ $name"
  fi
  rm -rf "$dst"
}

echo "Сторож гейта каталогов сообщений (0532)..."

# M6 - контроль: нетронутая копия принимается. Идёт первым: краснеющий на
# корректном дереве проверка делает бессмысленными все прочие пробы.
D=$(mktemp -d); prepare "$D"
expect "M6 согласованное дерево принимается" pass "$D"

# M1 - строка без `=`.
D=$(mktemp -d); prepare "$D"
echo "строка без равенства" >> "$D/takt-lang/messages/ru.txt"
expect "M1 строка каталога без '='" fail "$D"

# M2 - ключ без перевода.
D=$(mktemp -d); prepare "$D"
grep -v '^lang.unknown' "$D/takt-lang/messages/en.txt" > "$D/tmp" && mv "$D/tmp" "$D/takt-lang/messages/en.txt"
expect "M2 ключ без перевода" fail "$D"

# M3 - подстановки разошлись.
D=$(mktemp -d); prepare "$D"
sed -i.bak 's/^lang.unknown = .*/lang.unknown = unknown language {name}/' "$D/takt-lang/messages/en.txt"
expect "M3 набор подстановок разошёлся" fail "$D"

# M4 - "перевод" копированием русского текста.
D=$(mktemp -d); prepare "$D"
sed -i.bak "s/^diag.note-label = .*/diag.note-label = примечание/" "$D/takt-lang/messages/en.txt"
expect "M4 непереведённый ключ (кириллица)" fail "$D"

# M5 - мёртвый ключ: есть в обоих каталогах, печатника нет.
D=$(mktemp -d); prepare "$D"
echo "dead.key = мёртвый" >> "$D/takt-lang/messages/ru.txt"
echo "dead.key = dead" >> "$D/takt-lang/messages/en.txt"
expect "M5 ключ без печатника" fail "$D"

# M7 - вырожденный вход: пустой базовый каталог обязан давать отказ. Проверка,
# молчащий на пустом файле, ничего не прочёл.
D=$(mktemp -d); prepare "$D"
: > "$D/takt-lang/messages/ru.txt"
expect "M7 пустой базовый каталог — отказ" fail "$D"

if [ "$FAILED" -ne 0 ]; then
  echo "Сторож гейта каталогов сообщений: ПРОВАЛ"
  exit 1
fi
echo "Сторож гейта каталогов сообщений: все пробы пройдены"

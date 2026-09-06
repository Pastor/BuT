#!/usr/bin/env bash
# Тест проверки комментариев.
#
# Мутациями доказывается, что `check-comments.py` ловит то, ради чего заведён:
#
# C1 - пиктограмма или типографский знак в комментарии; C2 - ссылка на номер фичи,
# задачи или правила; C3 - упоминание того, по чьей просьбе сделано; C4 - раздел истории
# правки; C5 - чистое дерево принимается (иначе проверка красен всегда); C6 - вырожденный
# вход (нет файлов с кодом) даёт отказ, а не молчание.
#
# Мутации ставятся на копии дерева (`CC_ROOT`), рабочие файлы не трогаются.
set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-comments.py"
FAILED=0

prepare() {
  local dst="$1"
  mkdir -p "$dst/takt-lang/src"
  printf '// Разбор аргументов командной строки.\npub fn main() {}\n' \
    > "$dst/takt-lang/src/lib.rs"
}

expect() {
  local name="$1" want="$2" dst="$3"
  CC_ROOT="$dst" python3 "$GATE" >/dev/null 2>&1
  local got=$?
  if [ "$want" = "fail" ] && [ "$got" -eq 0 ]; then
    echo "  x $name: проверка пропустила мутацию"
    FAILED=1
  elif [ "$want" = "pass" ] && [ "$got" -ne 0 ]; then
    echo "  x $name: проверка отвергла чистое дерево"
    FAILED=1
  else
    echo "  + $name"
  fi
  rm -rf "$dst"
}

echo "Самопроверка проверки комментариев..."

# C5 идёт первым: краснеющий на чистом дереве проверка обесценивает прочие пробы.
D=$(mktemp -d); prepare "$D"
expect "C7 чистое дерево принимается" pass "$D"

D=$(mktemp -d); prepare "$D"
printf '// Внимание: тут тире - и стрелка ->\n' | sed 's/ - / \xe2\x80\x94 /' \
  >> "$D/takt-lang/src/lib.rs"
expect "C1 типографский знак" fail "$D"

D=$(mktemp -d); prepare "$D"
printf '// Форма выбрана фичей 0448.\n' >> "$D/takt-lang/src/lib.rs"
expect "C2 ссылка на номер" fail "$D"

D=$(mktemp -d); prepare "$D"
printf '// Так решил заказчик.\n' >> "$D/takt-lang/src/lib.rs"
expect "C3 упоминание просьбы" fail "$D"

D=$(mktemp -d); prepare "$D"
printf '//! # Что было\n' >> "$D/takt-lang/src/lib.rs"
expect "C4 раздел истории правки" fail "$D"

D=$(mktemp -d); prepare "$D"
printf '// Проверку гоняет сторож.\n' >> "$D/takt-lang/src/lib.rs"
expect "C5 жаргон вместо термина" fail "$D"

D=$(mktemp -d); prepare "$D"
printf '// Значение берётся ТОЛЬКО из карты.\n' >> "$D/takt-lang/src/lib.rs"
expect "C6 выделение прописными" fail "$D"

D=$(mktemp -d); prepare "$D"
printf '// Разбор АСД идёт одним проходом.\n' >> "$D/takt-lang/src/lib.rs"
expect "C6 аббревиатура прописными законна" pass "$D"

# C8: пустое дерево - проверка, молчащий на нём, ничего не прочёл.
D=$(mktemp -d); mkdir -p "$D/takt-lang/src"
expect "C8 нет файлов с кодом - отказ" fail "$D"

if [ "$FAILED" -ne 0 ]; then
  echo "Самопроверка проверки комментариев: провал"
  exit 1
fi
echo "Самопроверка проверки комментариев: все пробы пройдены"

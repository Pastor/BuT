#!/usr/bin/env bash
# Тест проверки комментариев.
#
# Мутациями доказывается, что `check-comments.py` ловит то, ради чего заведён:
# каждый класс C1..C14 - пробой, которая обязана упасть, и там, где у правила
# есть названная граница, - пробой, которая обязана пройти (стандарт, дробь,
# "прежде чем", имена классов проверок в `scripts/`). Отдельно проверяются
# область (конфигурация проверяется, `docs/` нет, заголовок Markdown не
# комментарий) и реестр долга (долг не растёт, протухшая запись есть отказ,
# `--update-baseline` переписывает реестр).
#
# Мутации ставятся на копии дерева (`CC_ROOT`), рабочие файлы не трогаются.
set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GATE="$ROOT/scripts/check-comments.py"
FAILED=0

prepare() {
  local dst="$1"
  mkdir -p "$dst/takt-lang/src" "$dst/scripts"
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

# Проба на одну строку комментария в `lib.rs` копии.
probe() {
  local name="$1" want="$2" line="$3"
  local d; d=$(mktemp -d); prepare "$d"
  printf '%s\n' "$line" >> "$d/takt-lang/src/lib.rs"
  expect "$name" "$want" "$d"
}

echo "Самопроверка проверки комментариев..."

# Чистое дерево идёт первым: краснеющая на нём проверка обесценивает прочие пробы.
D=$(mktemp -d); prepare "$D"
expect "чистое дерево принимается" pass "$D"

D=$(mktemp -d); prepare "$D"
printf '// Внимание: тут тире - и стрелка ->\n' | sed 's/ - / \xe2\x80\x94 /' \
  >> "$D/takt-lang/src/lib.rs"
expect "C1 типографский знак" fail "$D"

probe "C2 ссылка на номер со словом" fail '// Форма выбрана фичей 0448.'
probe "C3 упоминание просьбы" fail '// Так решил заказчик.'
probe "C4 раздел истории правки" fail '//! # Что было'
probe "C5 жаргон вместо термина" fail '// Проверку гоняет сторож.'
probe "C6 выделение прописными" fail '// Значение берётся ТОЛЬКО из карты.'
probe "C6 аббревиатура прописными законна" pass '// Разбор АСД идёт одним проходом.'
probe "C7 отсылка к решению карточки" fail '// Форма выбрана решением A5 разбора.'
probe "C7 отсылка к задаче в кавычках" fail '// Список ключей заводит задача `09b`.'
probe "C8 отсылка к образцу заимствования" fail '// Устройство взято у референса.'
probe "C9 датированный замер" fail '// Форма выбрана прогоном (замер 2026-09-04).'
probe "C9 ссылка на стандарт законна" pass '// Ширина слова задана стандартом (IEEE 1800 §23.2).'
probe "C10 голый номер работы" fail '// Ключ карты с 0084 квалифицирован моделью.'
probe "C10 номер через дробь" fail '// Результат вызова поднимает свой проход (0431/0432).'
probe "C10 номер задачи после косой черты" fail '// Хелперы из родителя (приём /08).'
probe "C10 дробь и путь к карточке законны" pass '// Шаг сетки 0.0125; см. docs/features/0029-c-type-mapping.md'
probe "C11 код карточки без слова" fail '// Круговой рейс остаётся тождеством, R4.'
probe "C11 код теста в заголовке" fail '//! **T1.** Одиночная модель получает typedef корня.'
probe "C11 имя состояния S1 законно" pass '// Из состояния S1 автомат уходит в S2.'
probe "C12 история словом прежде" fail '// Прежде здесь печаталась голая строка.'
probe "C12 первая редакция" fail '// Первая редакция покрыла лишь сравнение.'
probe "C12 оборот прежде чем законен" pass '// Прежде чем печатать, тип приводится к приёмнику.'
probe "C13 происхождение модуля" fail '//! Вынесено из `mod.rs` по правилу размера модуля.'
probe "C14 висячий дефис" fail '/// Границы -.'
probe "C14 пустая строка документации последней" fail '//! Заголовок.
//!
use x;'

D=$(mktemp -d); prepare "$D"
printf '// Хвостовой пробел \n' >> "$D/takt-lang/src/lib.rs"
expect "C14 хвостовой пробел" fail "$D"

D=$(mktemp -d); prepare "$D"
printf '# T1 - вырожденный вход даёт отказ.\n' > "$D/scripts/probe.sh"
expect "C11 имя класса проверки в scripts законно" pass "$D"

# Область: конфигурация проверяется, docs/ нет, заголовок Markdown не комментарий.
D=$(mktemp -d); prepare "$D"
printf '# Каталог сборки задан фичей 0251.\n[build]\n' > "$D/Cargo.toml"
expect "область: конфигурация проверяется" fail "$D"

D=$(mktemp -d); prepare "$D"; mkdir -p "$D/docs"
printf '// Форма выбрана фичей 0448.\n' > "$D/docs/probe.rs"
expect "область: docs исключён" pass "$D"

D=$(mktemp -d); prepare "$D"
printf '# Что было\n\n* Пункт — с тире.\n' > "$D/README.md"
expect "область: разметка Markdown не комментарий" pass "$D"

D=$(mktemp -d); prepare "$D"
printf '<!-- Форма выбрана фичей 0448. -->\n' > "$D/README.md"
expect "область: комментарий разметки проверяется" fail "$D"

# Реестр долга: запись разрешает ровно своё число находок.
D=$(mktemp -d); prepare "$D"
printf '// Ключ карты с 0084 квалифицирован.\n' >> "$D/takt-lang/src/lib.rs"
printf 'takt-lang/src/lib.rs C10 1\n' > "$D/scripts/comment-baseline.txt"
expect "реестр: долг в пределах записи" pass "$D"

D=$(mktemp -d); prepare "$D"
printf '// Ключ карты с 0084 квалифицирован.\n// Ключ карты с 0085 тоже.\n' >> "$D/takt-lang/src/lib.rs"
printf 'takt-lang/src/lib.rs C10 1\n' > "$D/scripts/comment-baseline.txt"
expect "реестр: долг вырос - отказ" fail "$D"

D=$(mktemp -d); prepare "$D"
printf '// Ключ карты с 0084 квалифицирован.\n' >> "$D/takt-lang/src/lib.rs"
printf 'takt-lang/src/lib.rs C10 2\n' > "$D/scripts/comment-baseline.txt"
expect "реестр: протухшая запись - отказ" fail "$D"

D=$(mktemp -d); prepare "$D"
printf '// Ключ карты с 0084 квалифицирован.\n' >> "$D/takt-lang/src/lib.rs"
CC_ROOT="$D" python3 "$GATE" --update-baseline >/dev/null 2>&1
if ! grep -q '^takt-lang/src/lib.rs C10 1$' "$D/scripts/comment-baseline.txt"; then
  echo "  x реестр: --update-baseline не записал долг"; FAILED=1
fi
expect "реестр: после --update-baseline проверка зелёная" pass "$D"

D=$(mktemp -d); prepare "$D"
printf '// Ключ карты с 0084 квалифицирован.\n' >> "$D/takt-lang/src/lib.rs"
printf 'takt-lang/src/lib.rs C10 1\n' > "$D/scripts/comment-baseline.txt"
if CC_ROOT="$D" python3 "$GATE" --list takt-lang | grep -q '^C10 takt-lang/src/lib.rs:3'; then
  echo "  + реестр: --list печатает находку под долгом"
else
  echo "  x реестр: --list не печатает находку под долгом"; FAILED=1
fi
rm -rf "$D"

# Пустое дерево: проверка, молчащая на нём, ничего не прочла.
D=$(mktemp -d); mkdir -p "$D/takt-lang/src"
expect "пустое дерево - отказ" fail "$D"

if [ "$FAILED" -ne 0 ]; then
  echo "Самопроверка проверки комментариев: провал"
  exit 1
fi
echo "Самопроверка проверки комментариев: все пробы пройдены"

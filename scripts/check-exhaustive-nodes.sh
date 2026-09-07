#!/bin/sh
# check-exhaustive-nodes.sh - защита инварианта исчерпывающего разбора
# семантических узлов.
#
# Правило (docs/CODE.md): семантические узлы-диспетчеры ExpressionNode /
# ConditionNode / StatementNode разбираются исчерпывающе (без `_ =>`), а модуль
# семантики вычислений закрепляет это `#![deny(clippy::wildcard_enum_match_arm)]`.
# Именно `_ => None`/необязательный разбор позволил двум вычислителям симулятора
# разойтись молча (восемь дефектов при зелёных тестах).
#
# Два пути "тихой смерти" инварианта - их и ловит проверка:
# 1. Пометить узел `#[non_exhaustive]` - атрибут отключает проверку
#      исчерпываемости для зависимых крейтов: добавление варианта перестанет
# ломать сборку, адаптеры вернутся к "молча не обработано".
#   2. Снять `#![deny(clippy::wildcard_enum_match_arm)]` в одном из модулей,
# разбирающих узлы языка, - исчезнет компиляторное принуждение перечислять
#      варианты. Таких модулей два, и оба обязательны:
# - takt-sim/src/eval/ - полный интерпретатор такта;
# - takt-sim/src/unit/initial.rs - Второй вычислитель, начальные значения
# . Он жил под `_ => None` и потому был вне действия правила:
#          добавление варианта не ломало сборку, новый узел молча получал
# "значения нет";
# - takt-lang/src/parser/depth/{children,dismantle}.rs - счёт глубины АСД и
# утилизация отвергнутого дерева. Узел, выпавший из счёта,
#          Не измеряется: дерево произвольной глубины прошло бы предел и уронило
#          бы первого же рекурсивного потребителя; узел, выпавший из утилизации,
# уничтожался бы рекурсивным `Drop` - то есть падение вернулось бы
#          молча, на самом глубоком вводе.
# Оба пути проходят сборку молча - поэтому нужен именно проверка (прецедент /
# Правило без команды - не правило).
#
# POSIX sh, без внешних зависимостей (образец - scripts/check-diagnostic-codes.sh).
set -eu

# Корень переопределяется переменной: тест гоняет проверка на
# Копии дерева, не трогая рабочие файлы.
ROOT="${EN_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
NODES_FILE="$ROOT/takt-lang/src/semantic/mod.rs"
EVAL_MOD="$ROOT/takt-sim/src/eval/mod.rs"
INITIAL_MOD="$ROOT/takt-sim/src/unit/initial.rs"
DEPTH_CHILDREN="$ROOT/takt-lang/src/parser/depth/children.rs"
DEPTH_DISMANTLE="$ROOT/takt-lang/src/parser/depth/dismantle.rs"
DENY='#![deny(clippy::wildcard_enum_match_arm)]'

for f in "$NODES_FILE" "$EVAL_MOD" "$INITIAL_MOD" "$DEPTH_CHILDREN" "$DEPTH_DISMANTLE"; do
  [ -f "$f" ] || { echo "check-exhaustive-nodes: не найден $f" >&2; exit 1; }
done

echo "Гейт исчерпывающего разбора семантических узлов (фича 0093)..."

fail=0

# Условие 1: ни один из трёх узлов не помечен `#[non_exhaustive]`. Атрибут
# привязан к узлу через его блок атрибутов (только строки-атрибуты/док/пустые
# между `#[non_exhaustive]` и `pub enum X` - любая строка кода сбрасывает связь).
BAD_NODES="$(awk '
  /#\[non_exhaustive\]/ { pending = 1; next }
  /^pub enum (ExpressionNode|ConditionNode|StatementNode)[ {]/ {
    if (pending) { match($0, /ExpressionNode|ConditionNode|StatementNode/); print substr($0, RSTART, RLENGTH) }
    pending = 0; next
  }
  /^[[:space:]]*$/ { next }        # пустая строка — часть блока атрибутов
  /^[[:space:]]*#/ { next }        # другой атрибут (#[derive], …) — часть блока
  /^[[:space:]]*\/\// { next }     # док-/обычный комментарий — часть блока
  { pending = 0 }                  # строка кода — связь с атрибутом сброшена
' "$NODES_FILE")"

if [ -n "$BAD_NODES" ]; then
  echo "  ОШИБКА: семантический узел помечен #[non_exhaustive] (фича 0093):" >&2
  echo "$BAD_NODES" | sed 's/^/    /' >&2
  echo "  Это ТИХО отключает инвариант исчерпывающего разбора (ADR 0025):" >&2
  echo "  добавление варианта перестанет ломать сборку, вычислители разойдутся молча." >&2
  echo "  Снимите #[non_exhaustive] с этих узлов (docs/CODE.md, «Расширяемость и API»)." >&2
  fail=1
fi

# Условие 2: Все модули, разбирающие узлы языка, хранят
# `#![deny(clippy::wildcard_enum_match_arm)]`. Проверять только `eval/` мало:
# ровно так второй вычислитель и оставался вне правила.
# Поиск привязан к началу строки. Простой `grep -F` находил атрибут и внутри
# док-комментария, объясняющего, зачем он нужен, - то есть модуль без атрибута,
# но с рассказом о нём, проверка проходил. Вскрыто мутацией.
for mod_file in "$EVAL_MOD" "$INITIAL_MOD" "$DEPTH_CHILDREN" "$DEPTH_DISMANTLE"; do
  if ! grep -q '^#!\[deny(clippy::wildcard_enum_match_arm)\]' "$mod_file"; then
    echo "  ОШИБКА: в $mod_file снят '$DENY' (фичи 0093, 0163, 0156)." >&2
    echo "  Без него компилятор перестаёт требовать явного разбора вариантов —" >&2
    echo "  вычислители смогут разойтись молча (ADR 0025). Верните атрибут." >&2
    fail=1
  fi
done

if [ "$fail" != 0 ]; then
  exit 1
fi
echo "  OK: узлы не #[non_exhaustive]; вычислители симулятора и обходы глубины АСД хранят deny(wildcard_enum_match_arm)."

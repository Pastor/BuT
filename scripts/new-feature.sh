#!/bin/sh
# new-feature.sh - генератор заготовок артефактов фичи из docs/templates/
# . POSIX sh + sed + awk, без
# внешних зависимостей.
#
# Использование:
#   scripts/new-feature.sh [--with-dev] [--register] XXXX slug "Заголовок"
#   scripts/new-feature.sh --stage NAME [--register] [--subtask NN] XXXX slug "Заголовок"
#
# --with-dev совместимость: ничего не меняет - все стадии живут разделами
# одной карточки
# --register дописать строки в реестры README соответствующих папок
# --stage NAME feature|fixes создают файл; adr|analyze|dev|tests|report -
# подсказка, какой раздел карточки заполняется (:
#                  стадии файлов не заводят).
# --subtask NN номер dev-подзадачи для --stage dev (по умолчанию 01)
#
# Идемпотентность: --register не дублирует строки - если строка с
# ключом-номером фичи в реестре уже есть, вставка пропускается. Повторный прогон
# безопасен.
#
# Переменная окружения NF_ROOT переопределяет корень репозитория (по умолчанию -
# каталог скрипта/..). Нужна для тестируемости: scripts/test-new-feature.sh гоняет
# генератор в temp-дереве, не трогая рабочие реестры.
#
# Примеры:
#   scripts/new-feature.sh 0032 my-feature "Моя фича"
#   scripts/new-feature.sh --with-dev --register 0032 my-feature "Моя фича"
#   scripts/new-feature.sh --stage report --register 0032 my-feature "Моя фича"
#   scripts/new-feature.sh --stage dev --subtask 03 --register 0032 my-feature "Моя фича"
set -eu

WITH_DEV=0
REGISTER=0
STAGE=""
SUBTASK="01"
while [ $# -gt 0 ]; do
  case "$1" in
    --with-dev) WITH_DEV=1; shift ;;
    --register) REGISTER=1; shift ;;
    --stage) STAGE="${2:-}"; shift 2 ;;
    --subtask) SUBTASK="${2:-}"; shift 2 ;;
    --) shift; break ;;
    -*) echo "Неизвестный флаг: $1" >&2; exit 2 ;;
    *) break ;;
  esac
done

if [ $# -ne 3 ]; then
  echo "Использование: $0 [--with-dev] [--register] [--stage NAME] [--subtask NN] XXXX slug \"Заголовок\"" >&2
  exit 2
fi

NUM="$1"; SLUG="$2"; TITLE="$3"

# Валидация
case "$NUM" in
  [0-9][0-9][0-9][0-9]) : ;;
  *) echo "Ошибка: XXXX должен быть 4 цифры (напр. 0032), получено: '$NUM'" >&2; exit 2 ;;
esac
case "$SLUG" in
  *[!a-z0-9-]*|"" ) echo "Ошибка: slug - kebab-case латиницей [a-z0-9-], получено: '$SLUG'" >&2; exit 2 ;;
  *) : ;;
esac
case "$SUBTASK" in
  [0-9][0-9]) : ;;
  *) echo "Ошибка: --subtask NN - две цифры (напр. 03), получено: '$SUBTASK'" >&2; exit 2 ;;
esac
case "$STAGE" in
  ""|feature|adr|analyze|dev|tests|report|fixes) : ;;
  *) echo "Ошибка: --stage NAME ∈ {feature,adr,analyze,dev,tests,report,fixes}, получено: '$STAGE'" >&2; exit 2 ;;
esac

# Корень репозитория: NF_ROOT (для тестов) либо каталог скрипта/..
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT="${NF_ROOT:-$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)}"
TPL="$ROOT/docs/templates"
DATE=$(date +%F)

[ -d "$TPL" ] || { echo "Не найден каталог шаблонов: $TPL" >&2; exit 1; }

# Экранирование заголовка для replacement-части sed (делимитер '|')
TITLE_ESC=$(printf '%s' "$TITLE" | sed -e 's/[\\&|]/\\\&/g')

# render <шаблон> <куда> <repl_yy:0|1>: заполнить шаблон плейсхолдерами. При
# repl_yy=1 подставляет номер подзадачи ($SUBTASK) вместо YY. Существующий файл
# Не перезаписывается (идемпотентно).
render() {
  tpl="$1"; dest="$2"; repl_yy="$3"
  [ -f "$TPL/$tpl" ] || { echo "Нет шаблона: $TPL/$tpl" >&2; exit 1; }
  if [ -e "$dest" ]; then
    echo "Пропуск (уже существует): $dest" >&2
    return 0
  fi
  mkdir -p "$(dirname -- "$dest")"
  # Порядок важен: YYYY-MM-DD до YY (иначе YY затрёт часть даты)
  if [ "$repl_yy" = "1" ]; then
    sed -e "s|YYYY-MM-DD|$DATE|g" \
        -e "s|XXXX|$NUM|g" \
        -e "s|YY|$SUBTASK|g" \
        -e "s|slug|$SLUG|g" \
        -e "s|<заголовок>|$TITLE_ESC|g" \
        "$TPL/$tpl" > "$dest"
  else
    sed -e "s|YYYY-MM-DD|$DATE|g" \
        -e "s|XXXX|$NUM|g" \
        -e "s|slug|$SLUG|g" \
        -e "s|<заголовок>|$TITLE_ESC|g" \
        "$TPL/$tpl" > "$dest"
  fi
  echo "Создано: ${dest#$ROOT/}"
}

# insert_row <README> <key> <row>: идемпотентно вставить строку после последней
# строки таблицы. `key` - литерал первого столбца, однозначно определяющий строку
# фичи (напр. "| [0094]", "| 0094 |", "| |"). Если строка с ключом уже
# есть - вставка пропускается.
insert_row() {
  readme="$1"; key="$2"; row="$3"
  [ -f "$readme" ] || { echo "Нет реестра: $readme (пропуск)" >&2; return 0; }
  if grep -Fq "$key" "$readme"; then
    echo "Реестр уже содержит запись (пропуск): ${readme#$ROOT/} [$key]" >&2
    return 0
  fi
  tmp="$readme.tmp.$$"
  awk -v row="$row" '
    /^\|/ { last = NR }
    { lines[NR] = $0 }
    END {
      for (i = 1; i <= NR; i++) {
        print lines[i]
        if (i == last) print row
      }
    }
  ' "$readme" > "$tmp" && mv "$tmp" "$readme"
  echo "Реестр обновлён: ${readme#$ROOT/}"
}

# do_stage <name>: рендер заготовки стадии и (при --register) идемпотентная
# регистрация её строки в реестре. Обе операции безопасны при повторе.
# Регистрация - через `if ... then ... fi`, а не `[ ... ] && insert_row`: под
# `set -e` последняя форма при REGISTER=0 возвращает 1 и обрывает do_stage
# (ломало умолчательный путь без --register; поймано регресс-тестом 0094).
do_stage() {
  case "$1" in
    feature)
      render feature.md "$ROOT/docs/features/$NUM-$SLUG.md" 0
      if [ "$REGISTER" = 1 ]; then
        insert_row "$ROOT/docs/features/README.md" "| [$NUM]" \
          "| [$NUM](./$NUM-$SLUG.md) | $TITLE_ESC | СОЗДАНА | СОЗДАНА |"
      fi
      ;;
    # Стадии 2...6 файлов не создают: их результат - раздел той же
    # карточки. Флаг `--stage` остался подсказкой: он называет раздел, который
    # надо заполнить, и ничего не пишет на диск.
    adr)      stage_hint "Архитектура (ADR)" ;;
    analyze)  stage_hint "Анализ" ;;
    dev)      stage_hint "Разработка → ### Задача $NUM-$SUBTASK" ;;
    tests)    stage_hint "Тест-план" ;;
    report)   stage_hint "Отчёт о тестировании" ;;
    fixes)
      render fixes.md "$ROOT/docs/fixes/$NUM-$SUBTASK-$SLUG.md" 1
      if [ "$REGISTER" = 1 ]; then
        insert_row "$ROOT/docs/fixes/README.md" "| $NUM-$SUBTASK |" \
          "| $NUM-$SUBTASK | $NUM | $TITLE_ESC | [$NUM-$SUBTASK-$SLUG.md]($NUM-$SUBTASK-$SLUG.md) |"
      fi
      ;;
  esac
}

# stage_hint <раздел>: напоминание, какой раздел карточки заполняется.
# Файлов не создаёт - стадии живут разделами.
stage_hint() {
  card="docs/features/$NUM-$SLUG.md"
  if [ -f "$ROOT/$card" ]; then
    echo "Стадия пишется в раздел «$1» карточки $card (правило 32)."
  else
    echo "Карточки $card нет: заведите её (scripts/new-feature.sh --register $NUM $SLUG \"$TITLE\")." >&2
    exit 1
  fi
}

if [ -n "$STAGE" ]; then
  # Режим одной стадии: `feature`/`fixes` создают файл, прочие - подсказка.
  do_stage "$STAGE"
else
  # Умолчательный режим: заводится одна карточка со всеми разделами-заготовками
  # . Флаг --with-dev сохранён для совместимости вызовов и ничего
  # не меняет: разделы разработки и тест-плана живут в той же карточке.
  do_stage feature
fi

echo "Готово. Не забудьте: заполнить разделы карточки, обновить статус в FEATURES.md и внести запись в CHANGES.md."

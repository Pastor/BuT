#!/bin/sh
# measure-precheck.sh - обвязка замера времени предкоммита.
#
# # Что делает обвязка
#
# 1. Прогрев - один прогон, время которого выбрасывается. Он наполняет кэш
#      файловой системы и каталог сборки.
#   2. Два замерных прогона.
#   3. Вердикт: если они разошлись больше чем на порог (по умолчанию 20 %),
# замер объявляется негодным - система не устоялась, цифру записывать
#      нельзя.
#
# Это не проверка: в `precheck.sh` обвязка не вызывается (три прогона стоят
# минуты). Она нужна фиче, которая собирается **записать число** в карточку.
#
# Переменные:
# MEASURE_CMD - что мерить (по умолчанию./scripts/precheck.sh)
# MEASURE_SPREAD - допустимый разброс в процентах (по умолчанию 20)
# MEASURE_WARMUP - 0 отключает прогрев (для теста)
#
# Использование:
#   scripts/measure-precheck.sh
#   MEASURE_CMD='sleep 1' scripts/measure-precheck.sh
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CMD="${MEASURE_CMD:-$ROOT/scripts/precheck.sh}"
SPREAD="${MEASURE_SPREAD:-20}"
WARMUP="${MEASURE_WARMUP:-1}"

# Секунды с начала эпохи - переносимо (BSD и GNU date понимают %s).
now() { date +%s; }

run_once() {
    start="$(now)"
    if ! sh -c "$CMD" >/dev/null 2>&1; then
        echo "ОШИБКА: измеряемая команда завершилась неуспешно: $CMD" >&2
        echo "        Замер бессмыслен, пока прогон красный." >&2
        exit 1
    fi
    end="$(now)"
    echo $((end - start))
}

echo "Замер времени прогона (фича 0272): $CMD"

if [ "$WARMUP" = "1" ]; then
    echo "  прогрев (время выбрасывается)…"
    warm="$(run_once)"
    echo "    прогрев: ${warm} с"
fi

echo "  замер 1…"
first="$(run_once)"
echo "    ${first} с"
echo "  замер 2…"
second="$(run_once)"
echo "    ${second} с"

# Разброс считается от меньшего: так "120 с против 100 с" даёт 20 %, а не 16 %.
if [ "$first" -le "$second" ]; then
    lo="$first"; hi="$second"
else
    lo="$second"; hi="$first"
fi
if [ "$lo" -eq 0 ]; then
    lo=1
fi
delta=$(( (hi - lo) * 100 / lo ))

echo ""
echo "  меньший прогон: ${lo} с, больший: ${hi} с, разброс: ${delta} %"

if [ "$delta" -gt "$SPREAD" ]; then
    echo ""
    echo "  ЗАМЕР НЕГОДЕН: разброс ${delta} % больше допустимых ${SPREAD} %." >&2
    echo "  Система не устоялась — записывать это число как свойство сборки" >&2
    echo "  нельзя (прецедент 0242: 1633.8 с против 233.2 с, разница в 70 раз)." >&2
    echo "  Дайте файловой системе успокоиться и повторите." >&2
    exit 1
fi

echo "  ЗАМЕР ГОДЕН: берите меньшее — ${lo} с (разброс ${delta} % ≤ ${SPREAD} %)."

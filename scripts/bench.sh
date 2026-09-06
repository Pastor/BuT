#!/bin/sh
# bench.sh - прогон бенчмарков производительности.
#
# Это не Проверка. Скрипт не вызывается из precheck.sh, порога нет (решение
# ): время на общей машине шумит - фоновая нагрузка, тепловой
# троттлинг, - и порог давал бы ложные срабатывания. Проверка, которому не верят,
# хуже отсутствующего.
#
# Ценность бенчей - увидеть смену класса сложности (линия -> квадрат), а не
# проценты. Именно этот класс дефекта чинили и 0068.
#
# Использование:
# scripts/bench.sh # быстрый прогон (сокращённая выборка)
# scripts/bench.sh --full # полный прогон criterion (дольше, точнее)
# scripts/bench.sh --save NAME # сохранить baseline под именем NAME
# scripts/bench.sh --compare NAME # сравнить с сохранённым baseline
#
# Быстро != верно: бенч ловит замедление, а не ошибку. Правильность проверяют
# тесты, и подменять их бенчем нельзя.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

case "${1:-}" in
  --full)
    echo "Бенчмарки: полный прогон criterion…"
    cargo bench --workspace --benches
    ;;
  --save)
    NAME="${2:?укажите имя baseline: scripts/bench.sh --save NAME}"
    echo "Бенчмарки: сохранение baseline «$NAME»…"
    cargo bench --workspace --benches -- --save-baseline "$NAME"
    ;;
  --compare)
    NAME="${2:?укажите имя baseline: scripts/bench.sh --compare NAME}"
    echo "Бенчмарки: сравнение с baseline «$NAME»…"
    cargo bench --workspace --benches -- --baseline "$NAME"
    ;;
  *)
    # Сокращённая выборка: числа грубее, зато прогон укладывается в минуты.
    # Для сравнения "до/после" этого достаточно - ищем порядок, а не проценты.
    echo "Бенчмарки: быстрый прогон (сокращённая выборка; --full для точного)…"
    cargo bench --workspace --benches -- --quick
    ;;
esac

#!/bin/sh
# coverage.sh - измерение покрытия тестами.
#
# Это не Проверка. Скрипт не вызывается из precheck.sh и не должен: сборка с
# инструментацией не переиспользует обычные артефакты и стоит минут, а покрытие
# - справочная величина, а не цель. Порога нет намеренно (
# ): покрытая строка не значит проверенное поведение - проект не раз
# ловил дефекты именно в покрытом коде (цель `sv` компилировалась и считала не
# то; `c-hal` собиралась и молча теряла биты).
#
# Ценность прогона - не процент, а список мест, которых не касается ни один
# тест.
#
# Использование:
# scripts/coverage.sh # сводка по крейтам
# scripts/coverage.sh --html # плюс HTML-отчёт в target/llvm-cov/html
# scripts/coverage.sh --files # построчная таблица по файлам (длинная)
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

if ! cargo llvm-cov --version >/dev/null 2>&1; then
  echo "Покрытие не измерено: нет cargo-llvm-cov."
  echo "Установка:"
  echo "  rustup component add llvm-tools-preview"
  echo "  cargo install cargo-llvm-cov --locked"
  echo "(мягкий пропуск — как у ensure-iec2c.sh; это не гейт)"
  exit 0
fi

# Фича `lsp` обязательна: без неё модули языкового сервера не собираются и
# выпадают из измерения целиком - та же ловушка, что с их тестами.
COMMON="--workspace --features lsp"

# Тесты идут параллельно. Флаг `--test-threads=1` здесь не нужен,
# он остался от времён, когда его требовал предкоммит.
# гонки были в каталогах тестов, а не в параллелизме, и ключ уникальности
# каталога - имя потока, которого в однопоточном режиме нет.


case "${1:-}" in
  --html)
    echo "Покрытие: HTML-отчёт (target/llvm-cov/html/index.html)…"
    # shellcheck disable=SC2086
    cargo llvm-cov $COMMON --html
    ;;
  --files)
    echo "Покрытие: таблица по файлам…"
    # shellcheck disable=SC2086
    cargo llvm-cov $COMMON
    ;;
  *)
    echo "Покрытие: сводка (это не гейт — порога нет)…"
    # shellcheck disable=SC2086
    cargo llvm-cov $COMMON --summary-only
    ;;
esac

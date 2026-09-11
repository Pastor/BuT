#!/usr/bin/env bash
# Запускает все симуляции из examples/simulations/ по очереди.
# Для каждого файла вида <модель>_<сценарий>.json модель в examples/ называет
# `takt-sim project --owner`.
# Запускать из любого каталога.

set -euo pipefail

# Корень репозитория = каталог этого скрипта /..
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SIM_DIR="$ROOT/examples/simulations"
# Каталог бинарников - у одного носителя. Свой путь здесь
# "$ROOT/target/debug", тогда как перенесла сборку в target/precheck
# через.cargo/config.toml: скрипт сломался в тот же день и не был замечен,
# потому что его никто не запускает (в precheck.sh его нет).
BINARY="$("$(dirname "$0")/target-dir.sh")/takt-sim"

if [[ ! -x "$BINARY" ]]; then
  echo "Бинарник не найден: $BINARY"
  echo "Запустите: cargo build --bin takt-sim"
  exit 1
fi

pass=0
fail=0
skip=0

for sim_file in "$SIM_DIR"/*.json; do
  [[ -f "$sim_file" ]] || continue

  # Имя файла без пути и расширения: stacker_loading
  base="$(basename "$sim_file" .json)"

  # Модель сценария называет правило принадлежности крейта проекта: из моделей,
  # чья основа совпадает с именем сценария либо начинает его с `_`, побеждает
  # самая длинная (`elevator_mini_floor2` -> `elevator_mini`, а не `elevator`).
  # Своей копии правила у скрипта нет - она разошлась бы с правилом страницы.
  takt_file="$("$BINARY" project --owner "$sim_file" "$ROOT"/examples/*.takt)" || takt_file=""
  if [[ -z "$takt_file" ]]; then
    echo "[ ПРОПУСК ] $base  (ни одна модель examples/ сценарию не хозяин)"
    ((skip++)) || true
    continue
  fi

  # Количество шагов из JSON (опционально: ограничивает сценарий снаружи)
  n_steps="$(python3 -c "import json,sys; print(len(json.load(open('$sim_file'))))" 2>/dev/null || echo "")"
  step_arg=""
  [[ -n "$n_steps" ]] && step_arg="-n $n_steps"

  # Запуск симуляции. Кадров скрипт не пишет: GIF прогона рисуется по файлу
  # раскладки `.takt-ui`, а у примеров его в дереве нет (`takt-sim export`
  # откажет без него).
  # shellcheck disable=SC2086
  if output="$("$BINARY" "$takt_file" -s "$sim_file" $step_arg 2>&1)"; then
    echo "[  OK  ] $base"
    ((pass++)) || true
  else
    echo "[ FAIL ] $base"
    echo "$output" | sed 's/^/         /'
    ((fail++)) || true
  fi
done

echo ""
echo "Итого: $pass прошло, $fail упало, $skip пропущено."
[[ $fail -eq 0 ]]

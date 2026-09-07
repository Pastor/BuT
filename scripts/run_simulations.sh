#!/usr/bin/env bash
# Запускает все симуляции из examples/simulations/ по очереди.
# Для каждого файла вида <модель>_<сценарий>.json ищет examples/<модель>.takt.
# Запускать из любого каталога.

set -euo pipefail

# Корень репозитория = каталог этого скрипта /..
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SIM_DIR="$ROOT/examples/simulations"
# Каталог бинарников - у одного носителя. Прежде здесь стояло
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

  # Имя модели - самый длинный префикс `base` (по `_`), для которого есть.takt.
  # Прежде бралась часть до первого `_` (`${base%%_*}`), что ломалось на именах
  # моделей с подчёркиванием: `elevator_mini_floor2` -> `elevator` вместо
  # `elevator_mini`. Отсекаем суффикс справа, пока не найдём.takt.
  candidate="$base"
  takt_file=""
  model="$candidate"
  while :; do
    if [[ -f "$ROOT/examples/${candidate}.takt" ]]; then
      model="$candidate"
      takt_file="$ROOT/examples/${candidate}.takt"
      break
    fi
    [[ "$candidate" == *_* ]] || break
    candidate="${candidate%_*}"
  done
  output_path="$ROOT/examples/simulations/graphics"
  config_file="$ROOT/examples/graphics-configs/default_svg.json"

  if [[ -z "$takt_file" ]]; then
    echo "[ ПРОПУСК ] $base  (не найден ${model}.takt)"
    ((skip++)) || true
    continue
  fi

  # Количество шагов из JSON (опционально: ограничивает сценарий снаружи)
  n_steps="$(python3 -c "import json,sys; print(len(json.load(open('$sim_file'))))" 2>/dev/null || echo "")"
  step_arg=""
  [[ -n "$n_steps" ]] && step_arg="-n $n_steps"

  # Запуск симуляции
  # shellcheck disable=SC2086
  if output="$("$BINARY" "$takt_file" -s "$sim_file" -o "$output_path" --graphics-config $config_file $step_arg 2>&1)"; then
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

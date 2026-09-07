#!/bin/sh
# Проверка команд README: то, что документ предлагает набрать, обязано
# работать.
#
# Повод - не теория. Замер при закрытии: раздел сборки
# предлагал `cargo install --path grammar --bin taktc` - команду, не работающую
# с (крейт переименован в `takt-lang`); полтора десятка фич подряд
# документ вёл читателя в тупик. Проверка ссылок этого не ловит по
# Устройству: он проверяет пути в markdown-ссылках, а не работоспособность
# вызовов в блоках кода.
#
# Что проверяется (три вещи, по возрастанию строгости):
#   1. Пути, названные флагами cargo (`--path`, `--manifest-path`) и вызовами
#      скриптов (`scripts/*.sh`), существуют;
#   2. Подкоманда `taktc` известна инструменту;
# 3. Команды `taktc`, все входные файлы которых существуют, прогоняются -
#      вывод перенаправляется во временный каталог.
#
# Прогоняется не всё и намеренно: команды с плейсхолдерами (`model.takt`,
# `hello.takt`) исполнить нельзя - таких файлов нет; команды внешних
# инструментов (`verilator`, `yosys`, `gtkwave`, `cargo install`) требуют сети
# либо ставят софт. Для них работает проверка 1, для `taktc` - ещё и 2.
#
# Использование: scripts/check-readme-commands.sh (шаг precheck.sh)
# Переменные: RC_ROOT - корень дерева, TAKTC - путь к компилятору.
#
# POSIX sh, без внешних зависимостей.
set -eu

. "$(dirname "$0")/gatelib.sh"

ROOT="${RC_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
TAKTC="${TAKTC:-$ROOT/target/precheck/debug/taktc}"
README="$ROOT/README.md"

echo "Гейт команд README (фича 0275)..."

[ -f "$README" ] || { echo "  нет $README" >&2; exit 1; }

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
BAD=0
RUN=0
CHECKED=0

# Строки внутри блоков ```sh, без комментариев и пустых.
awk '/^```sh$/{f=1;next} /^```$/{f=0} f' "$README" \
    | sed 's/[[:space:]]*#.*$//' \
    | grep -vE '^[[:space:]]*$' > "$TMP/cmds"

# Известные подкоманды берутся у самого инструмента: второй список разошёлся бы
# с первым.
SUBCOMMANDS="compile fmt verify address-map version"

while IFS= read -r LINE; do
    CHECKED=$((CHECKED + 1))

    # --- 1. Пути, названные флагами cargo и вызовами скриптов ---------------
    for FLAG in --path --manifest-path; do
        VAL="$(printf '%s' "$LINE" | sed -nE "s/.*$FLAG[= ]+([^ ]+).*/\\1/p")"
        [ -z "$VAL" ] && continue
        if [ ! -e "$ROOT/$VAL" ]; then
            echo "  ОШИБКА: $FLAG $VAL — пути нет в дереве:" >&2
            echo "    $LINE" >&2
            BAD=1
        fi
    done
    SCRIPT="$(printf '%s' "$LINE" | grep -oE '(^|[[:space:]])scripts/[A-Za-z0-9._-]+\.(sh|py)' | head -1 | tr -d ' ' || true)"
    if [ -n "$SCRIPT" ] && [ ! -f "$ROOT/$SCRIPT" ]; then
        echo "  ОШИБКА: $SCRIPT — скрипта нет в дереве:" >&2
        echo "    $LINE" >&2
        BAD=1
    fi

    # --- 2 и 3. Вызовы самого компилятора -----------------------------------
    case "$LINE" in
        taktc\ *|./target/release/taktc\ *)
            ARGS="${LINE#*taktc }"
            SUB="$(printf '%s' "$ARGS" | awk '{print $1}')"
            case " $SUBCOMMANDS " in
                *" $SUB "*) : ;;
                --version|-V|--help) : ;;
                *)
                    echo "  ОШИБКА: неизвестная подкоманда '$SUB':" >&2
                    echo "    $LINE" >&2
                    BAD=1
                    continue
                    ;;
            esac
            # Все ли входные файлы существуют? Плейсхолдеры (model.takt,
            # hello.takt) прогонять нечем - такие строки проверкой 2 и
            # ограничиваются.
            RUNNABLE=1
            for TOK in $ARGS; do
                case "$TOK" in
                    *.takt|*.map|*.json)
                        # Выходные пути (`-o ports.json`) исключаются: их не
                        # должно существовать до прогона.
                        case " $ARGS " in *" -o $TOK "*|*" --output $TOK "*) continue ;; esac
                        [ -e "$ROOT/$TOK" ] || RUNNABLE=0
                        ;;
                esac
            done
            [ "$RUNNABLE" -eq 1 ] || continue
            # Прогон: выход - во временный каталог.
            OUT="$TMP/run$RUN"
            mkdir -p "$OUT"
            CMD="$(printf '%s' "$ARGS" | sed -E "s#-o [^ ]+#-o $OUT#; s#--output [^ ]+#--output $OUT#")"
            case "$CMD" in *" -o "*|*" --output "*) : ;; *) CMD="$CMD -o $OUT" ;; esac
            # `fmt` без --check правит файлы на месте - такую команду не гоняем.
            case "$SUB" in
                fmt)
                    case "$CMD" in *--check*|*--stdin*) : ;; *) continue ;; esac
                    CMD="$(printf '%s' "$ARGS")"
                    ;;
            esac
            # shellcheck disable=SC2086 - аргументы разбираются намеренно.
            if ! (cd "$ROOT" && "$TAKTC" $CMD) >"$OUT/log" 2>&1; then
                echo "  ОШИБКА: команда README не работает:" >&2
                echo "    $LINE" >&2
                sed 's/^/      /' "$OUT/log" | head -3 >&2
                BAD=1
            fi
            RUN=$((RUN + 1))
            ;;
    esac
done < "$TMP/cmds"

if [ "$BAD" -ne 0 ]; then
    echo "  README предлагает команды, которые не работают." >&2
    echo "  Документ ведёт читателя в тупик — правьте README, а не гейт." >&2
    exit 1
fi

NOTE="$(require_input "проверенные строки README" "$CHECKED" 1 "README.md")" || exit 1
  echo "  OK: $NOTE, команд компилятора прогнано $RUN."

#!/bin/sh
# Снятие замера расхождения - эталон и все семь целей одной командой.
#
set -eu

STEPS=1
INCLUDE=""
MODEL=""
while [ $# -gt 0 ]; do
    case "$1" in
        -n) STEPS="${2:-1}"; shift 2 ;;
        -I) INCLUDE="${2:-}"; shift 2 ;;
        -h|--help) sed -n '2,32p' "$0"; exit 0 ;;
        -*) echo "Неизвестный флаг: $1" >&2; exit 2 ;;
        *) MODEL="$1"; shift ;;
    esac
done
[ -n "$MODEL" ] || { echo "Использование: $0 [-n ШАГОВ] [-I КАТАЛОГ] МОДЕЛЬ.takt" >&2; exit 2; }
[ -f "$MODEL" ] || { echo "Не найден файл модели: $MODEL" >&2; exit 2; }

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BIN_DIR="$("$SCRIPT_DIR/target-dir.sh")"
TAKTC="$BIN_DIR/taktc"
SIM="$BIN_DIR/takt-sim"
for b in "$TAKTC" "$SIM"; do
    [ -x "$b" ] || {
        echo "Бинарник не найден: $b" >&2
        echo "Соберите: cargo build --bin taktc --bin takt-sim" >&2
        exit 1
    }
done

INC_ARGS=""
[ -n "$INCLUDE" ] && INC_ARGS="-I $INCLUDE"

OUT="$(mktemp -d)"
trap 'rm -rf "$OUT"' EXIT

STEM="$(basename "$MODEL" .takt)"

echo "Проба: $MODEL (эталон + семь целей)"
echo

# -- 1. Проверка самой пробы --------------------------------------------------
echo "ПРОВЕРКА ПРОБЫ"
PROBE_BAD=0

# 1а. Объявлено, но не используется: такая переменная не попадёт в вывод целей,
# и замер покажет "расхождения нет".
# shellcheck disable=SC2086
UNUSED="$("$TAKTC" compile "$MODEL" -t c -o "$OUT/warm" $INC_ARGS 2>&1 | grep 'SE-036' || true)"
if [ -n "$UNUSED" ]; then
    echo "$UNUSED" | while IFS= read -r line; do
        echo "  ✗ $(echo "$line" | sed 's/.*Предупреждение //')"
    done
    echo "    → неиспользуемое объявление ВЫБРАСЫВАЕТСЯ из вывода целей;"
    echo "      проба измеряет не то. Используйте его в теле состояния."
    PROBE_BAD=1
else
    echo "  ✓ объявленные переменные используются"
fi

# 1б. Имя файла становится именем корневой модели: столкновение с
#     объявленным типом даёт постороннюю ошибку в цели `c`.
#
# `model` в список не входит, и это замер, а не забывчивость:
# `model Regulator` в `regulator.takt` - идиома корпуса, так написан почти
#     каждый пример, и цель `c` принимает такой вход (проверено `cc -pedantic`).
#     Разбор, этого не различающий, объявлял бы негодной каждую
# вторую пробу - тест, кричащий всегда, замолкает навсегда.
#
# Тело цикла обязано кончаться успехом: под `set -e` статус
#     последней команды `while` становится статусом подстановки, и вариант
# `[ ... ] && printf` роняет весь скрипт, когда совпадения нет, - то есть на
#     каждой пробе с объявленным типом. Тест этого не видел: во всех его
#     входах либо типов не было вовсе, либо совпадение было.
CLASH="$(grep -oE '^[[:space:]]*(struct|enum|type)[[:space:]]+[A-Za-z_][A-Za-z0-9_]*' "$MODEL" \
    | awk '{print $NF}' | while IFS= read -r name; do
        lower_name="$(printf '%s' "$name" | tr '[:upper:]' '[:lower:]')"
        lower_stem="$(printf '%s' "$STEM" | tr '[:upper:]' '[:lower:]')"
        if [ "$lower_name" = "$lower_stem" ]; then
            printf '%s\n' "$name"
        fi
    done)"
if [ -n "$CLASH" ]; then
    echo "  ✗ имя файла '$STEM' совпадает с именем объявленного ТИПА: $CLASH"
    echo "    → корневая модель берёт имя из имени файла, и цель \`c\` даст"
    echo "      постороннюю коллизию (класс 0195). Переименуйте файл или тип."
    PROBE_BAD=1
else
    echo "  ✓ имя файла не сталкивается с именами объявленных типов"
fi

[ "$PROBE_BAD" -eq 0 ] || echo "  ⚠️ проба негодна: замер ниже измеряет не предмет."
echo

# -- 2. Эталон ----------------------------------------------------------------
echo "ЭТАЛОН (takt-sim, шагов: $STEPS)"
# shellcheck disable=SC2086
if REF="$("$SIM" "$MODEL" -n "$STEPS" $INC_ARGS 2>&1)"; then
    printf '%s\n' "$REF" | grep -E '^Шаг' | tail -n 3 | sed 's/^/  /'
else
    printf '%s\n' "$REF" | head -n 2 | sed 's/^/  ОТКАЗ: /'
fi
echo

# -- 3. Цели ------------------------------------------------------------------
# Отказ общей диагностикой (SE-/SY-/LE-) приходит раньше цели: её поведение
# таким прогоном не измерено. Отказ кодом цели (CC-/ST-/RS-/SV-) - измерен.
#
# Вердикт трансляции запоминается в файле `$OUT/gen.<цель>`: пустой - вывод
# порождён, иначе там код отказа. Раздел 4 читает его, потому что инструменту
# цели нечего проверять там, где файла нет.
echo "ЦЕЛИ"
for t in c c-hal st st-at rust sv sv-mmio; do
    # shellcheck disable=SC2086
    if MSG="$("$TAKTC" compile "$MODEL" -t "$t" -o "$OUT/$t" $INC_ARGS 2>&1)"; then
        printf '  %-9s OK\n' "$t"
        : > "$OUT/gen.$t"
    else
        CODE="$(printf '%s' "$MSG" | grep -oE '\[[A-Z]{2,3}-[0-9]{3}\]' | head -n1 | tr -d '[]')"
        printf '%s\n' "${CODE:-без кода}" > "$OUT/gen.$t"
        case "$CODE" in
            SE-*|SY-*|LE-*)
                printf '  %-9s ОТКАЗ %s ⚠️ ОБЩИЙ — поведение цели НЕ измерено\n' "$t" "$CODE" ;;
            "")
                printf '  %-9s ОТКАЗ (без кода)\n' "$t" ;;
            *)
                printf '  %-9s ОТКАЗ %s (отказ цели)\n' "$t" "$CODE" ;;
        esac
    fi
done
echo

# -- 4. Инструменты целей ----------------------------------------
# Нулевой код возврата `taktc` не означает, что вывод валиден: порождённый файл
# принимает или отвергает чужой инструмент. Класс "цель рапортует об успехе, а
# `cc`/`iec2c`/`rustc`/`verilator` вывод отвергают" находили вручную одиннадцать
# фич подряд, и вывод одной из них записан прямо: "probe.sh этот класс не
# показывает - он сводит
# коды возврата, а все три цели возвращали ноль".
#
# Флаги те же, что У Проверок `precheck.sh`. Мягче проверки - замер объявит годным
# вывод, который предкоммит отвергнет; строже - даст ложную тревогу. Тест
# `scripts/test-probe.sh` требует, чтобы строгость здесь не терялась.
#
# У цели `st` шага два, и это намеренно строже проверки: `iec2c` форму
# разбирает, а порождённый им C может не собраться ( - `VAR_INPUT` с
# массивом). Сборка `POUS.c` идёт с `-w`: 307 предупреждений даёт сам
# `iec_std_lib.h` MatIEC, к порождённому коду отношения не имеющий, - гасится
# чужой шум, а не свои ошибки.
#
# Отсутствие инструмента печатается словом "пропуск" и никогда не выглядит
# как "принял": `iec2c`, `verilator` и `yosys` внешние, и молчание при их
# отсутствии повторило бы ровно тот класс, ради которого раздел заведён.
IEC2C_BIN="${IEC2C_PREFIX:-$HOME/.local}/bin/iec2c"
IEC2C_LIB="${IEC2C_PREFIX:-$HOME/.local}/share/matiec/lib"

# Единица трансляции порождённого MatIEC кода. У цели `st-at` iec2c выпускает
# CONFIGURATION отдельным файлом `<Имя>Config.c`, который сам включает `POUS.c`
# после объявления глобальных размещённых переменных; собирать вместо него
# `POUS.c` напрямую значит получить "call to undeclared function
# __GET_GLOBAL_..." - ложную тревогу на любом входе. Контроль (`examples/regulator.takt`,
# проходящий проверка) её и показал: тест, кричащий всегда, замолкает навсегда.
iec_c_unit() {
    for cfg in "$1"/*Config.c; do
        [ -e "$cfg" ] || continue
        printf '%s\n' "$cfg"
        return 0
    done
    printf '%s\n' "-include POUS.h $1/POUS.c"
}

tool_say()  { printf '  %-9s %s\n' "$1" "$2"; }
tool_show() { grep -vE '^[[:space:]]*$' "$1" | head -n 4 | sed 's/^/             /'; }

echo "ИНСТРУМЕНТЫ ЦЕЛЕЙ (те же команды и флаги, что у гейтов предкоммита)"
for t in c c-hal st st-at rust sv sv-mmio; do
    GEN="$(cat "$OUT/gen.$t" 2>/dev/null || printf '')"
    if [ -n "$GEN" ]; then
        tool_say "$t" "— вывода нет (taktc: $GEN)"
        continue
    fi
    ERR="$OUT/tool.$t.err"
    case "$t" in
        c|c-hal)
            if ! command -v cc >/dev/null 2>&1; then
                tool_say "$t" "пропуск: нет cc"
            elif cc -std=c11 -Wall -Wextra -Werror -c "$OUT/$t"/*.c -o /dev/null 2>"$ERR"; then
                tool_say "$t" "cc принял"
            else
                tool_say "$t" "cc ОТВЕРГ:"; tool_show "$ERR"
            fi ;;
        st|st-at)
            if [ ! -x "$IEC2C_BIN" ] || [ ! -f "$IEC2C_LIB/ieclib.txt" ]; then
                tool_say "$t" "пропуск: нет iec2c ($IEC2C_BIN)"
            else
                IEC_OUT="$OUT/iec.$t"
                mkdir -p "$IEC_OUT"
                if ! "$IEC2C_BIN" -I "$IEC2C_LIB" -T "$IEC_OUT" "$OUT/$t"/*.st >"$ERR" 2>&1; then
                    tool_say "$t" "iec2c ОТВЕРГ:"; tool_show "$ERR"
                elif [ ! -f "$IEC_OUT/POUS.c" ] || [ ! -d "$IEC2C_LIB/C" ]; then
                    tool_say "$t" "iec2c принял (сборка C пропущена: нет $IEC2C_LIB/C)"
                elif cc -c -w -I "$IEC2C_LIB/C" -I "$IEC_OUT" $(iec_c_unit "$IEC_OUT") \
                        -o /dev/null 2>"$ERR"; then
                    tool_say "$t" "iec2c принял · cc собрал"
                else
                    tool_say "$t" "iec2c принял, но cc НЕ СОБРАЛ порождённый C:"
                    grep -i 'error' "$ERR" > "$ERR.only" 2>/dev/null || : > "$ERR.only"
                    tool_show "$ERR.only"
                fi
            fi ;;
        rust)
            RS="$(ls "$OUT/$t"/*.rs 2>/dev/null | head -n 1)"
            if [ -z "$RS" ]; then
                tool_say "$t" "— вывода нет"
            elif ! command -v rustc >/dev/null 2>&1; then
                tool_say "$t" "пропуск: нет rustc"
            else
                # Обёртка `#![no_std]` - как в проверке: атрибут допустим только в
                # корне крейта, и модуль проверяется так, как будет использован.
                printf '#![no_std]\n#[path = "%s"]\npub mod generated;\n' "$RS" > "$OUT/gate_rust.rs"
                if ! rustc --edition 2021 --crate-type=lib -D warnings \
                        "$OUT/gate_rust.rs" --out-dir "$OUT/rsout" 2>"$ERR"; then
                    tool_say "$t" "rustc ОТВЕРГ:"; tool_show "$ERR"
                elif clippy-driver --edition 2021 --crate-type=lib -D warnings \
                        "$OUT/gate_rust.rs" --out-dir "$OUT/rsout" 2>"$ERR"; then
                    tool_say "$t" "rustc принял · clippy принял"
                else
                    tool_say "$t" "rustc принял, clippy ОТВЕРГ:"; tool_show "$ERR"
                fi
            fi ;;
        sv|sv-mmio)
            SVF="$(ls "$OUT/$t"/*.sv 2>/dev/null | head -n 1)"
            TOP="$(basename "${SVF:-x.sv}" .sv)"
            if [ -z "$SVF" ]; then
                tool_say "$t" "— вывода нет"
            elif ! command -v verilator >/dev/null 2>&1; then
                tool_say "$t" "пропуск: нет verilator"
            elif ! verilator --lint-only -Wall --top-module "$TOP" "$SVF" >"$ERR" 2>&1; then
                tool_say "$t" "verilator ОТВЕРГ:"; tool_show "$ERR"
            elif ! command -v yosys >/dev/null 2>&1; then
                tool_say "$t" "verilator принял (пропуск: нет yosys)"
            elif yosys -q -p "read_verilog -sv $SVF; synth -top $TOP" >"$ERR" 2>&1; then
                tool_say "$t" "verilator принял · yosys синтезировал"
            else
                tool_say "$t" "verilator принял, yosys НЕ СИНТЕЗИРОВАЛ:"; tool_show "$ERR"
            fi ;;
    esac
done
echo
echo "Сравнивает человек: скрипт приводит ответы в одну таблицу, но не судит."
echo "Цели, отказавшие ОБЩИМ кодом, из замера исключены — снимите причину и"
echo "повторите. Чек-лист пробы — правило 30 свода docs/RULE.md."

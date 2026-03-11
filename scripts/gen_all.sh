#!/usr/bin/env bash
# =============================================================================
# gen_all.sh — Пакетная генерация кода для всех .but файлов в директории.
#
# Использование:
#   gen_all.sh <исходная_директория> <директория_назначения> [ОПЦИИ]
#
# Аргументы:
#   <исходная_директория>   Директория с .but файлами (поиск рекурсивный).
#   <директория_назначения> Директория, куда будет помещён сгенерированный код.
#
# Опции:
#   --indent-size N         Отступ в N пробелов (по умолчанию: 4).
#   --indent-tab            Использовать символ табуляции вместо пробелов.
#   --generators LIST       Список генераторов через запятую.
#                           Возможные значения: c,verilog,st,lc3,thumb,all
#                           По умолчанию: all (все генераторы).
#   -I DIR, --include-dir DIR
#                           Добавить директорию в пути поиска include-файлов.
#                           Можно указывать несколько раз.
#   --but PATH              Путь к исполняемому файлу but
#                           (по умолчанию: ищется в PATH, затем в target/release).
#   --dry-run               Вывести команды без их выполнения.
#   -h, --help              Показать эту справку.
#
# Структура выходной директории:
#   <директория_назначения>/
#     <относительный_путь_файла>/   # Совпадает со структурой источника
#       c/                          # C99 заголовок + исходник
#       verilog/                    # Verilog RTL-модуль
#       st/                         # Structured Text (МЭК 61131-3)
#       lc3/                        # Ассемблер LC-3
#       thumb/                      # Ассемблер ARM Thumb
#
# Примеры:
#   gen_all.sh ./examples ./gen
#   gen_all.sh ./src ./out --indent-size 2
#   gen_all.sh ./src ./out --indent-tab --generators c,verilog
#   gen_all.sh ./src ./out -I ./include -I ./lib/types
#   gen_all.sh ./src ./out --but ./target/release/but
# =============================================================================

set -euo pipefail

# ── Цвета для вывода ──────────────────────────────────────────────────────────
if [[ -t 1 ]]; then
    CLR_RESET='\033[0m'
    CLR_BOLD='\033[1m'
    CLR_GREEN='\033[0;32m'
    CLR_YELLOW='\033[0;33m'
    CLR_RED='\033[0;31m'
    CLR_CYAN='\033[0;36m'
else
    CLR_RESET='' CLR_BOLD='' CLR_GREEN='' CLR_YELLOW='' CLR_RED='' CLR_CYAN=''
fi

# ── Вспомогательные функции ───────────────────────────────────────────────────

info()    { echo -e "${CLR_CYAN}[INFO]${CLR_RESET}  $*"; }
ok()      { echo -e "${CLR_GREEN}[ОК]${CLR_RESET}    $*"; }
warn()    { echo -e "${CLR_YELLOW}[WARN]${CLR_RESET}  $*" >&2; }
error()   { echo -e "${CLR_RED}[ОШИБКА]${CLR_RESET} $*" >&2; }
fatal()   { error "$*"; exit 1; }

usage() {
    sed -n '2,/^# ={10}/p' "$0" | grep '^#' | sed 's/^# \?//'
    exit 0
}

# ── Значения по умолчанию ─────────────────────────────────────────────────────
SRC_DIR=""
DST_DIR=""
INDENT_ARGS=""
GENERATORS="all"
BUT_BIN=""
DRY_RUN=false
# Массив дополнительных директорий поиска include-файлов
INCLUDE_DIRS=()

# ── Разбор аргументов ─────────────────────────────────────────────────────────
while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            usage ;;
        --indent-size)
            [[ -z "${2:-}" ]] && fatal "--indent-size требует аргумент"
            INDENT_ARGS="--indent-size $2"
            shift 2 ;;
        --indent-tab)
            INDENT_ARGS="--indent-tab"
            shift ;;
        --generators)
            [[ -z "${2:-}" ]] && fatal "--generators требует аргумент"
            GENERATORS="$2"
            shift 2 ;;
        -I|--include-dir)
            [[ -z "${2:-}" ]] && fatal "$1 требует аргумент"
            INCLUDE_DIRS+=("$2")
            shift 2 ;;
        --but)
            [[ -z "${2:-}" ]] && fatal "--but требует аргумент"
            BUT_BIN="$2"
            shift 2 ;;
        --dry-run)
            DRY_RUN=true
            shift ;;
        -*)
            fatal "Неизвестный флаг: $1" ;;
        *)
            if [[ -z "$SRC_DIR" ]]; then
                SRC_DIR="$1"
            elif [[ -z "$DST_DIR" ]]; then
                DST_DIR="$1"
            else
                fatal "Слишком много позиционных аргументов: $1"
            fi
            shift ;;
    esac
done

# ── Проверка обязательных аргументов ─────────────────────────────────────────
[[ -z "$SRC_DIR" ]] && fatal "Не указана исходная директория.\nИспользование: $0 <исходная> <назначение>"
[[ -z "$DST_DIR" ]] && fatal "Не указана директория назначения.\nИспользование: $0 <исходная> <назначение>"
[[ -d "$SRC_DIR" ]] || fatal "Исходная директория не найдена: $SRC_DIR"

# Нормализуем пути
SRC_DIR="$(cd "$SRC_DIR" && pwd)"
DST_DIR="$(cd "$(dirname "$DST_DIR")" && pwd)/$(basename "$DST_DIR")"

# ── Поиск исполняемого файла but ──────────────────────────────────────────────
find_but() {
    # 1. Явно указанный путь
    if [[ -n "$BUT_BIN" ]]; then
        [[ -x "$BUT_BIN" ]] || fatal "Исполняемый файл не найден или не исполняем: $BUT_BIN"
        echo "$BUT_BIN"
        return
    fi
    # 2. В PATH
    if command -v but &>/dev/null; then
        echo "but"
        return
    fi
    # 3. Собранный cargo (release)
    local script_dir
    script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    local root_dir
    root_dir="$(cd "$script_dir/.." && pwd)"
    local release_bin="$root_dir/target/release/but"
    if [[ -x "$release_bin" ]]; then
        echo "$release_bin"
        return
    fi
    # 4. Собранный cargo (debug)
    local debug_bin="$root_dir/target/debug/but"
    if [[ -x "$debug_bin" ]]; then
        warn "Используется отладочная сборка: $debug_bin"
        echo "$debug_bin"
        return
    fi
    fatal "Исполняемый файл 'but' не найден.\nУстановите его через 'cargo install --path cli' или укажите путь через --but."
}

BUT_BIN="$(find_but)"

# ── Формирование флагов генераторов ──────────────────────────────────────────
build_gen_flags() {
    local gens="$1"
    if [[ "$gens" == "all" ]]; then
        echo "--gen-c --gen-verilog --gen-st --gen-lc3 --gen-thumb"
        return
    fi
    local flags=""
    IFS=',' read -ra parts <<< "$gens"
    for g in "${parts[@]}"; do
        g="${g// /}" # убрать пробелы
        case "$g" in
            c)       flags="$flags --gen-c" ;;
            verilog) flags="$flags --gen-verilog" ;;
            st)      flags="$flags --gen-st" ;;
            lc3)     flags="$flags --gen-lc3" ;;
            thumb)   flags="$flags --gen-thumb" ;;
            *)       warn "Неизвестный генератор: '$g' (допустимые: c,verilog,st,lc3,thumb,all)" ;;
        esac
    done
    [[ -z "$flags" ]] && fatal "Не указан ни один допустимый генератор"
    echo "$flags"
}

GEN_FLAGS="$(build_gen_flags "$GENERATORS")"

# ── Основной цикл ─────────────────────────────────────────────────────────────
echo -e "${CLR_BOLD}BuT — пакетная генерация кода${CLR_RESET}"
echo "  Источник    : $SRC_DIR"
echo "  Назначение  : $DST_DIR"
echo "  Генераторы  : $GENERATORS"
echo "  Отступ      : ${INDENT_ARGS:-по умолчанию (4 пробела)}"
if [[ ${#INCLUDE_DIRS[@]} -gt 0 ]]; then
    echo "  Include-пути: ${INCLUDE_DIRS[*]}"
fi
echo "  Исполняемый : $BUT_BIN"
$DRY_RUN && echo -e "  ${CLR_YELLOW}Режим dry-run: команды не выполняются${CLR_RESET}"
echo ""

# Счётчики
count_total=0
count_ok=0
count_err=0
declare -a failed_files=()

# Найти все .but файлы рекурсивно
while IFS= read -r -d '' but_file; do
    count_total=$((count_total + 1))

    # Относительный путь от источника (без расширения .but)
    rel_path="${but_file#"$SRC_DIR/"}"
    rel_base="${rel_path%.but}"

    # Выходная директория для этого файла
    out_dir="$DST_DIR/$rel_base"

    echo -e "  ${CLR_CYAN}▶${CLR_RESET} $rel_path"

    # Собираем команду
    # Добавляем флаги путей поиска include-файлов
    include_flags=()
    for inc_dir in "${INCLUDE_DIRS[@]}"; do
        include_flags+=("-I" "$inc_dir")
    done
    # shellcheck disable=SC2086
    cmd=("$BUT_BIN" "$but_file" $GEN_FLAGS --output-dir "$out_dir" ${INDENT_ARGS} "${include_flags[@]}")

    if $DRY_RUN; then
        echo "    ${CLR_YELLOW}[dry-run]${CLR_RESET} ${cmd[*]}"
        count_ok=$((count_ok + 1))
        continue
    fi

    # Создать выходную директорию
    mkdir -p "$out_dir"

    # Выполнить генерацию
    if output=$("${cmd[@]}" 2>&1); then
        ok "$rel_path → $out_dir"
        count_ok=$((count_ok + 1))
    else
        error "Ошибка при обработке: $rel_path"
        echo "$output" | sed 's/^/    /' >&2
        failed_files+=("$rel_path")
        count_err=$((count_err + 1))
        # Продолжаем обработку остальных файлов
    fi
done < <(find "$SRC_DIR" -name "*.but" -type f -print0 | sort -z)

# ── Итоговый отчёт ────────────────────────────────────────────────────────────
echo ""
echo -e "${CLR_BOLD}── Результат ────────────────────────────────${CLR_RESET}"
echo -e "  Всего файлов : $count_total"
echo -e "  ${CLR_GREEN}Успешно      : $count_ok${CLR_RESET}"
if [[ $count_err -gt 0 ]]; then
    echo -e "  ${CLR_RED}С ошибками   : $count_err${CLR_RESET}"
    echo -e "  ${CLR_RED}Файлы с ошибками:${CLR_RESET}"
    for f in "${failed_files[@]}"; do
        echo -e "    ${CLR_RED}✗${CLR_RESET} $f"
    done
    exit 1
else
    echo -e "  ${CLR_GREEN}Все файлы обработаны успешно.${CLR_RESET}"
fi

#!/usr/bin/env bash
# Тест скрипта установки `scripts/install.sh`.
#
# Настоящую релизную сборку тест не запускает: она занимает минуты и от
# скрипта не зависит - её делает cargo. Проверяется то, что принадлежит
# **скрипту**: разбор аргументов, состав команд, выбор каталога сборки, отказы.
# Образец - `scripts/test-new-feature.sh` и `scripts/test-precheck-hygiene.sh`.
#
# Проверки:
#   A1 --help печатает использование и выходит 0
#   A2 неизвестный аргумент отвергается (код 2), а не игнорируется молча
#   A3 --dry-run печатает три сборки и три установки, ничего не создавая
#   A4 --no-lsp убирает языковой сервер из состава
#   A5 --prefix уважается (и в путях установки, и в проверке PATH)
#   A6 --build-only не устанавливает
#   A7 --check отвергает пустой префикс (код 1) и называет недостающее
# A8 каталог сборки по умолчанию - Свой (`target/install`), не общий `target`
#
# Код возврата: 0 - все проверки пройдены.

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SCRIPT="$ROOT/scripts/install.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

fail=0
ok() { echo "  OK: $1"; }
bad() { echo "  ПРОВАЛ: $1" >&2; fail=1; }

# A1 - справка
out="$("$SCRIPT" --help 2>&1)"; code=$?
if [ $code -eq 0 ] && printf '%s' "$out" | grep -q "Использование:"; then
    ok "A1 --help печатает использование"
else
    bad "A1 --help: код $code, вывод: $out"
fi

# A2 - неизвестный аргумент
out="$("$SCRIPT" --нет-такого-флага 2>&1)"; code=$?
if [ $code -eq 2 ] && printf '%s' "$out" | grep -q "неизвестный аргумент"; then
    ok "A2 неизвестный аргумент отвергнут"
else
    bad "A2 неизвестный аргумент: код $code, вывод: $out"
fi

# A3 - сухой прогон: три сборки, три установки, ничего не создано
out="$("$SCRIPT" --dry-run --prefix "$TMP/pfx" 2>&1)"; code=$?
builds=$(printf '%s\n' "$out" | grep -c "cargo build --release")
installs=$(printf '%s\n' "$out" | grep -c "install -m 0755")
if [ $code -eq 0 ] && [ "$builds" -eq 3 ] && [ "$installs" -eq 3 ] && [ ! -e "$TMP/pfx" ]; then
    ok "A3 --dry-run: 3 сборки, 3 установки, каталог не создан"
else
    bad "A3 --dry-run: код $code, сборок $builds, установок $installs, каталог $( [ -e "$TMP/pfx" ] && echo создан || echo нет)"
fi

# A4 - без языкового сервера
out="$("$SCRIPT" --dry-run --no-lsp --prefix "$TMP/pfx" 2>&1)"
if ! printf '%s' "$out" | grep -q "takt-lsp"; then
    ok "A4 --no-lsp убирает языковой сервер"
else
    bad "A4 --no-lsp: takt-lsp остался в составе"
fi

# A5 - префикс уважается
out="$("$SCRIPT" --dry-run --prefix "$TMP/pfx" 2>&1)"
if printf '%s' "$out" | grep -q "$TMP/pfx/bin/taktc"; then
    ok "A5 --prefix уважается"
else
    bad "A5 --prefix: путь установки не под префиксом"
fi

# A6 - только сборка
out="$("$SCRIPT" --dry-run --build-only --prefix "$TMP/pfx" 2>&1)"
if printf '%s' "$out" | grep -q "build-only" && ! printf '%s' "$out" | grep -q "install -m 0755"; then
    ok "A6 --build-only не устанавливает"
else
    bad "A6 --build-only: в выводе есть установка"
fi

# A7 - проверка пустого префикса
out="$("$SCRIPT" --check --prefix "$TMP/empty" 2>&1)"; code=$?
if [ $code -eq 1 ] && printf '%s' "$out" | grep -q "НЕ УСТАНОВЛЕН"; then
    ok "A7 --check отвергает пустой префикс и называет недостающее"
else
    bad "A7 --check: код $code, вывод: $out"
fi

# A8 - свой каталог сборки (урок: общий `target` бывает раздут до
# состояния, в котором cargo идёт часами)
out="$("$SCRIPT" --dry-run --prefix "$TMP/pfx" 2>&1)"
if printf '%s' "$out" | grep -q "каталог сборки: $ROOT/target/install"; then
    ok "A8 каталог сборки по умолчанию — target/install"
else
    bad "A8 каталог сборки: $(printf '%s\n' "$out" | grep 'каталог сборки')"
fi

if [ $fail -eq 0 ]; then
    echo "test-install: все проверки пройдены (A1–A8)."
else
    echo "test-install: ЕСТЬ ПРОВАЛЫ" >&2
fi
exit $fail

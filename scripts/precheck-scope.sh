#!/usr/bin/env bash
# Проверка, соразмерная правке: гоняет то, чей предмет затронут.
#
# # Предмет
#
# Полный предкоммит собирает дерево Rust и гоняет сверки целей - пять минут и
# сотни тестов. Правке, не касавшейся компилятора (страница, плагины, документ,
# скрипты процесса), эти минуты ничего не доказывают, а проверка, которую не
# гоняют из-за её цены, хуже быстрой.
#
# # Как решает
#
# Область берётся у самой правки: список изменённых файлов против ветки, а не
# догадка автора. Задет код на Rust, грамматика, примеры или манифесты - зовётся
# полный `precheck.sh`, и спорить не о чем. Не задет - гоняются проверки тех
# частей, что правились.
#
# # Границы
#
# Скрипт не заменяет предкоммит перед закрытием фичи: правило свода требует
# зелёного полного прогона там, где правился компилятор. Он отвечает на вопрос
# "что проверять сейчас", а не "можно ли закрывать".
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

# Что считать правкой: незакоммиченное плюс коммиты ветки против её начала.
BASE="${PRECHECK_SCOPE_BASE:-}"
if [ -z "$BASE" ]; then
  BASE="$(git merge-base HEAD origin/v2 2>/dev/null || echo "")"
fi
CHANGED="$(
  {
    git diff --name-only HEAD 2>/dev/null || true
    git diff --name-only --cached 2>/dev/null || true
    git ls-files --others --exclude-standard 2>/dev/null || true
    [ -n "$BASE" ] && git diff --name-only "$BASE"..HEAD 2>/dev/null || true
  } | sort -u
)"

if [ -z "$CHANGED" ]; then
  echo "Правок нет: проверять нечего."
  exit 0
fi

has() { echo "$CHANGED" | grep -qE "$1"; }

# Компилятор, эталон, грамматика, примеры и манифесты: предмет полного прогона.
# Их задели - выборочная проверка не вправе отвечать за целое.
if has '^(takt-lang|takt-sim|takt-wasm)/' || has '^examples/' || has '^Cargo\.(toml|lock)$' \
   || has '^rust-toolchain\.toml$' || has '\.rs$'; then
  echo "Правка задела компилятор или эталон: полный предкоммит."
  exec "$ROOT/scripts/precheck.sh"
fi

run() {
  echo
  echo "→ $*"
  "$@"
}

echo "Правка компилятора не касается; проверяются затронутые части."

if has '^web/'; then
  run "$ROOT/scripts/check-web.sh"
  run python3 "$ROOT/scripts/check-design.py"
fi

if has '^web/static/font/' || has '^takt-scheme/fonts/'; then
  run python3 "$ROOT/scripts/check-scheme-fonts.py" --self-test
  run python3 "$ROOT/scripts/check-scheme-fonts.py"
fi

if has '^extensions/' || has '^web/static/' ; then
  run python3 "$ROOT/scripts/check-zed-config.py"
  run "$ROOT/scripts/check-zed-extension.sh"
  run "$ROOT/scripts/test-check-plugin-webview.sh"
  run python3 "$ROOT/scripts/check-plugin-webview.py"
fi

if has '^book/'; then
  run python3 "$ROOT/scripts/check-book-diagnostics.py"
  run python3 "$ROOT/scripts/check-book-chapter-examples.py"
  run make -C "$ROOT/book" build
fi

if has '^scripts/'; then
  run python3 "$ROOT/scripts/check-precheck-comments.py"
  run python3 "$ROOT/scripts/check-gate-inputs.py"
fi

# Документы правятся почти всякой фичей: реестры, статусы и живой контекст
# сверяются всегда - они дёшевы и ловят расхождение, которое иначе доедет
# до чужой сессии.
run python3 "$ROOT/scripts/check-docs-style.py"
run python3 "$ROOT/scripts/check-registries.py"
run python3 "$ROOT/scripts/check-feature-status.py"
run python3 "$ROOT/scripts/check-claude-md.py"
run python3 "$ROOT/scripts/check-comments.py"
run "$ROOT/scripts/check-commit-trailers.sh"

echo
echo "Выборочная проверка пройдена. Полный предкоммит остаётся обязательным"
echo "перед закрытием фичи, задевшей компилятор."

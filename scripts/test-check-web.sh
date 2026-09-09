#!/usr/bin/env bash
# Тест проверки веб-части.
#
# Мутациями доказывается, что `check-web.sh` ловит то, ради чего заведён:
#
# B1 - разметка ссылается на файл, которого сборка не кладёт;
# B2 - скрипт страницы не разбирается (сломанный синтаксис);
# B3 - в `web/` появился список ключевых слов Takt;
# B4 - согласованное дерево принимается (иначе проверка красен всегда);
# B5 - без `node` мягкий пропуск, под `PRECHECK_STRICT=1` - ошибка;
# B6 - роли подсветки кода разошлись с темой документа `book/takt.tmTheme`;
# B7 - словарь языка неполон (ключ есть в базовом и нет в переводе);
# B8 - текст оболочки написан в коде мимо словаря.
#
# Мутации ставятся на копии дерева (`WEB_ROOT`), рабочие файлы не трогаются:
# тест, который правит проект, однажды оставит правку после падения.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

echo "Сторож гейта веб-части (фича 0531)..."

NODE="${TAKT_NODE:-node}"
if ! command -v "$NODE" >/dev/null 2>&1; then
  if [[ "${PRECHECK_STRICT:-0}" == "1" ]]; then
    echo "  ОШИБКА: не найден node (PRECHECK_STRICT=1)"
    exit 1
  fi
  echo "  пропуск: не найден node"
  exit 0
fi

BIN_DIR="$("$(dirname "${BASH_SOURCE[0]}")/target-dir.sh")"
TARGET_DIR="$(dirname "$BIN_DIR")"
PROFILE="${TAKT_WASM_PROFILE:-wasm}"
WASM="$TARGET_DIR/wasm32-unknown-unknown/$PROFILE/takt_wasm.wasm"
if [[ ! -f "$WASM" ]]; then
  if [[ "${PRECHECK_STRICT:-0}" == "1" ]]; then
    echo "  ОШИБКА: модуль не собран (PRECHECK_STRICT=1)"
    exit 1
  fi
  echo "  пропуск: модуль не собран"
  exit 0
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Копия дерева: только то, что проверке нужно.
TREE="$WORK/tree"
mkdir -p "$TREE/scripts" "$TREE/web" "$TREE/takt-lang" "$TREE/book" "$TREE/target/precheck/wasm32-unknown-unknown/$PROFILE"
cp -R "$ROOT/web/static" "$ROOT/web/tests" "$TREE/web/"
# Подписи кнопок площадок объявляет сервер, и сверка словаря читает его
# исходник. Копия дерева - не "весь проект": файл, который тесты
# читают, а тест не кладёт, роняет B4 с чужой причиной (тот же класс, что у
# `version.rs`).
mkdir -p "$TREE/web/server/src/oauth"
cp "$ROOT/web/server/src/oauth/api.rs" "$TREE/web/server/src/oauth/"
cp "$ROOT/scripts/check-web.sh" "$ROOT/scripts/build-web.sh" "$ROOT/scripts/target-dir.sh" \
   "$ROOT/scripts/build-scheme-host.py" "$ROOT/scripts/gatelib.sh" "$TREE/scripts/"
cp "$ROOT/takt-lang/Cargo.toml" "$TREE/takt-lang/"
# Версия языка для описи сборки берётся из константы, а не повторяется.
# Живёт она в `version.rs`; `lib.rs` её только реэкспортирует. Пока сборка
# читала `lib.rs`, поле `language` описей выходило пустым.
mkdir -p "$TREE/takt-lang/src"
cp "$ROOT/takt-lang/src/lib.rs" "$ROOT/takt-lang/src/version.rs" "$TREE/takt-lang/src/"
# Разбор аргументов: по нему сверяется опись ключей сборки.
# Копия дерева - не "весь проект", и файл, который тесты читают, а тест не
# кладёт, роняет B4 с чужой причиной (нашлось прогоном 2026-09-04 и повторилось
# 2026-09-05 на этих самых ключах).
mkdir -p "$TREE/takt-lang/src/compile_cli"
cp "$ROOT/takt-lang/src/compile_cli/mod.rs" "$ROOT/takt-lang/src/compile_cli/target_flags.rs" \
   "$TREE/takt-lang/src/compile_cli/"
# Тема документа: по ней проверяется реестр ролей подсветки.
# Копия дерева - не "весь проект": файл, который тесты читают, а тест не
# кладёт, роняет B4 с чужой причиной.
cp "$ROOT/book/takt.tmTheme" "$TREE/book/"
# Пример лифта: по нему проверяется граф модуля и сверка раскладки.
# Копия дерева - не "весь проект": файл, который тесты читают, а тест не
# кладёт, роняет B4 с чужой причиной.
mkdir -p "$TREE/examples"
cp "$ROOT/examples/elevator.takt" "$TREE/examples/"
cp "$WASM" "$TREE/target/precheck/wasm32-unknown-unknown/$PROFILE/"

run_gate() {  # запускает гейт на копии дерева
  ( cd "$TREE" && CARGO_TARGET_DIR="$TREE/target/precheck" bash scripts/check-web.sh 2>&1 )
}

# -- B4: согласованное дерево принимается -------------------------------------
if out="$(run_gate)"; then
  echo "  OK: B4 согласованное дерево принимается"
else
  echo "  ПРОВАЛ: B4 согласованное дерево отвергнуто:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi

# -- B1: ссылка на несобранный файл ловится -----------------------------------
cp "$TREE/web/static/index.html" "$WORK/index.html.bak"
sed -i.bak 's|<link rel="stylesheet" href="app.css">|<link rel="stylesheet" href="theme.css">|' \
  "$TREE/web/static/index.html"
if out="$(run_gate)"; then
  echo "  ПРОВАЛ: B1 ссылка на отсутствующий файл не поймана"
  exit 1
fi
if grep -q "theme.css" <<< "$out"; then
  echo "  OK: B1 ссылка на несобранный файл ловится и названа"
else
  echo "  ПРОВАЛ: B1 отказ не называет файл:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi
cp "$WORK/index.html.bak" "$TREE/web/static/index.html"

# -- B2: неразбираемый скрипт ловится -----------------------------------------
cp "$TREE/web/static/app.js" "$WORK/app.js.bak"
printf '\nfunction( {\n' >> "$TREE/web/static/app.js"
if out="$(run_gate)"; then
  echo "  ПРОВАЛ: B2 сломанный скрипт не пойман"
  exit 1
fi
if grep -q "не разбирается" <<< "$out"; then
  echo "  OK: B2 неразбираемый скрипт ловится"
else
  echo "  ПРОВАЛ: B2 отказ не про разбор:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi
cp "$WORK/app.js.bak" "$TREE/web/static/app.js"

# -- B3: список ключевых слов Takt ловится ------------------------------------
# Самый дорогой класс: свой словарь красит текст правдоподобно и расходится с
# лексером молча - ровно то, чем платили параллельные списки LSP.
cat > "$TREE/web/static/words.js" <<'EOF'
export const KEYWORDS = ["start", "state", "model", "invariant", "always"];
EOF
if out="$(run_gate)"; then
  echo "  ПРОВАЛ: B3 список ключевых слов Takt в web/ не пойман"
  exit 1
fi
if grep -q "ключевых слов" <<< "$out"; then
  echo "  OK: B3 список ключевых слов ловится"
else
  echo "  ПРОВАЛ: B3 отказ не про словарь:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi
rm "$TREE/web/static/words.js"

# -- B6: роли подсветки разошлись с темой документа ---------------------------
# Реестр ролей - `book/takt.tmTheme`: блоки кода в PDF и
# вкладка цели красят одни и те же виды токенов. Роль, заведённая только с одной
# стороны, - это документ и редактор, разошедшиеся глазами; сличить их может
# только человек, и только положив две картинки рядом.
cp "$TREE/web/static/app.css" "$WORK/app.css.bak"
# Роль дописывается после строки с любым значением: мутация, привязанная к
# конкретному цвету, перестаёт мутировать при первой же правке палитры - и
# тест начинает докладывать об успехе, ничего не проверив (нашлось правкой
# палитры 2026-09-04).
python3 - "$TREE/web/static/app.css" <<'PYEOF'
import sys
from pathlib import Path
p = Path(sys.argv[1])
lines = p.read_text(encoding="utf-8").splitlines(keepends=True)
at = next(i for i, l in enumerate(lines) if l.strip().startswith("--tok-comment:"))
lines.insert(at + 1, "  --tok-macro: #123456;\n")
p.write_text("".join(lines), encoding="utf-8")
PYEOF
if out="$(run_gate)"; then
  echo "  ПРОВАЛ: B6 роль вне темы документа не поймана"
  exit 1
fi
if grep -q "роли кода" <<< "$out"; then
  echo "  OK: B6 роль вне темы документа ловится"
else
  echo "  ПРОВАЛ: B6 отказ не про роли:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi
cp "$WORK/app.css.bak" "$TREE/web/static/app.css"

# -- B7: неполный словарь языка -----------------------------------------------
# Замер образца 2026-09-04: у него 163 ключа есть только в `ru`, и
# непереведённое молча падает на русский. Правило проекта иное - язык либо
# полон, либо не заведён; держать его обязана машина, а не дисциплина.
cp "$TREE/web/static/i18n/en.json" "$WORK/en.json.bak"
"$NODE" -e '
  const fs = require("fs");
  const path = process.argv[1];
  const dict = JSON.parse(fs.readFileSync(path, "utf8"));
  delete dict["bar.format"];
  fs.writeFileSync(path, JSON.stringify(dict, null, 2));
' "$TREE/web/static/i18n/en.json"
if out="$(run_gate)"; then
  echo "  ПРОВАЛ: B7 неполный словарь не пойман"
  exit 1
fi
if grep -q "состав" <<< "$out"; then
  echo "  OK: B7 неполный словарь ловится"
else
  echo "  ПРОВАЛ: B7 отказ не про состав словаря:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi
cp "$WORK/en.json.bak" "$TREE/web/static/i18n/en.json"

# -- B8: текст оболочки мимо словаря ------------------------------------------
# Самый тихий класс a: строка, написанная в коде, не переводится
# никогда и ничем не обнаруживается - страница выглядит рабочей, а одна подпись
# остаётся на чужом языке.
cp "$TREE/web/static/app.js" "$WORK/app.js.i18n.bak"
printf '\nexport const ЗАГОЛОВОК = "Сохранить";\n' >> "$TREE/web/static/app.js"
if out="$(run_gate)"; then
  echo "  ПРОВАЛ: B8 текст мимо словаря не пойман"
  exit 1
fi
if grep -q "мимо словаря" <<< "$out"; then
  echo "  OK: B8 текст мимо словаря ловится"
else
  echo "  ПРОВАЛ: B8 отказ не про словарь:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi
cp "$WORK/app.js.i18n.bak" "$TREE/web/static/app.js"

# -- B5: политика внешнего инструмента ----------------------------------------
if out="$(TAKT_NODE="$WORK/нет-такого-node" bash "$ROOT/scripts/check-web.sh" 2>&1)"; then
  if grep -q "пропуск" <<< "$out"; then
    echo "  OK: B5 без node — мягкий пропуск"
  else
    echo "  ПРОВАЛ: B5 без node гейт не сказал о пропуске:"
    echo "$out" | sed 's/^/    /'
    exit 1
  fi
else
  echo "  ПРОВАЛ: B5 без node гейт уронил прогон:"
  echo "$out" | sed 's/^/    /'
  exit 1
fi
if out="$(PRECHECK_STRICT=1 TAKT_NODE="$WORK/нет-такого-node" bash "$ROOT/scripts/check-web.sh" 2>&1)"; then
  echo "  ПРОВАЛ: B5 под PRECHECK_STRICT=1 отсутствие node принято"
  exit 1
fi
echo "  OK: B5 под PRECHECK_STRICT=1 отсутствие node — ошибка"

echo "  Сторож гейта веб-части: все проверки пройдены (B1…B8)."

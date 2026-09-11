#!/usr/bin/env bash
# Тест проверки сервера проектов.
#
# Мутациями доказывается, что `check-web-server.sh` ловит то, ради чего заведён:
#
# S0 - согласованное дерево принимается (иначе проверка красен всегда);
# S1 - несобирающийся сервер;
# S2 - красный тест без базы;
# S3 - колонка с персональными данными в схеме (ловится только с базой);
# S4 - политика внешнего инструмента: без базы мягкий пропуск, под
# `PRECHECK_STRICT=1` - ошибка.
#
# Мутации ставятся на копии дерева, рабочие файлы не трогаются: тест,
# который правит проект, однажды оставит правку после падения.
#
# S3 требует базы и без неё пропускается - это и есть цена перехода на
# PostgreSQL: половина предмета проверки проверяема только там, где база есть.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

echo "Сторож гейта сервера проектов (фича 0531)..."

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
TREE="$WORK/tree"
mkdir -p "$TREE/scripts"
cp -R "$ROOT/web" "$TREE/"
# Крейт проекта - зависимость сервера по пути (`../../takt-project`): без него
# в копии не разбирается даже манифест сервера.
cp -R "$ROOT/takt-project" "$TREE/"
cp "$ROOT/scripts/check-web-server.sh" "$TREE/scripts/"
# Сборка мутанта идёт в общий каталог: пересобирать полторы сотни крейтов на
# каждый случай - минуты, а мутации трогают только свой крейт.
export TAKT_WEB_SERVER_TARGET="${TAKT_WEB_SERVER_TARGET:-$ROOT/target/web-server}"

run_gate() { ( cd "$TREE" && bash scripts/check-web-server.sh 2>&1 ); }
restore() { cp -R "$ROOT/web/server/src" "$ROOT/web/server/tests" "$TREE/web/server/"; }

# -- S0: согласованное дерево -------------------------------------------------
if out="$(run_gate)"; then
  echo "  OK: S0 согласованное дерево принимается"
else
  echo "  ПРОВАЛ: S0 согласованное дерево отвергнуто:"
  echo "$out" | tail -20 | sed 's/^/    /'
  exit 1
fi

# -- S1: несобирающийся сервер ------------------------------------------------
printf '\nfn это_не_код( {\n' >> "$TREE/web/server/src/lib.rs"
if out="$(run_gate)"; then
  echo "  ПРОВАЛ: S1 несобирающийся сервер принят"
  exit 1
fi
echo "  OK: S1 несобирающийся сервер ловится"
restore

# -- S2: красный тест без базы ------------------------------------------------
python3 - "$TREE/web/server/src/rate.rs" <<'PYEOF'
import sys
from pathlib import Path
p = Path(sys.argv[1])
s = p.read_text(encoding="utf-8")
# Окно перестаёт останавливать поток: проверка обязана покраснеть.
s = s.replace("if entry.1 >= self.limit {", "if false {")
p.write_text(s, encoding="utf-8")
PYEOF
if out="$(run_gate)"; then
  echo "  ПРОВАЛ: S2 красный тест без базы не пойман"
  exit 1
fi
if grep -q "без базы красны" <<< "$out"; then
  echo "  OK: S2 красный тест без базы ловится"
else
  echo "  ПРОВАЛ: S2 отказ не про проверки без базы:"
  echo "$out" | tail -20 | sed 's/^/    /'
  exit 1
fi
restore

# -- S3: персональные данные в схеме ------------------------------------------
if [[ -n "${TAKT_WEB_TEST_DB:-}" ]]; then
  python3 - "$TREE/web/server/src/db.rs" <<'PYEOF'
import sys
from pathlib import Path
p = Path(sys.argv[1])
s = p.read_text(encoding="utf-8")
s = s.replace("    created_at BIGINT NOT NULL\n);\nCREATE UNIQUE INDEX users_login_lower",
              "    created_at BIGINT NOT NULL,\n    email      TEXT\n);\nCREATE UNIQUE INDEX users_login_lower")
p.write_text(s, encoding="utf-8")
PYEOF
  if out="$(run_gate)"; then
    echo "  ПРОВАЛ: S3 колонка почты в схеме принята"
    exit 1
  fi
  if grep -q "хранилища и HTTP красны" <<< "$out"; then
    echo "  OK: S3 персональные данные в схеме ловятся"
  else
    echo "  ПРОВАЛ: S3 отказ не про хранилище:"
    echo "$out" | tail -20 | sed 's/^/    /'
    exit 1
  fi
  restore
else
  echo "  пропуск: S3 требует базы (TAKT_WEB_TEST_DB не задан)"
fi

# -- S4: политика внешнего инструмента ----------------------------------------
if out="$(env -u TAKT_WEB_TEST_DB bash "$ROOT/scripts/check-web-server.sh" 2>&1)"; then
  if grep -q "пропуск" <<< "$out"; then
    echo "  OK: S4 без базы — мягкий пропуск"
  else
    echo "  ПРОВАЛ: S4 без базы гейт не сказал о пропуске:"
    echo "$out" | tail -10 | sed 's/^/    /'
    exit 1
  fi
else
  echo "  ПРОВАЛ: S4 без базы гейт уронил прогон:"
  echo "$out" | tail -10 | sed 's/^/    /'
  exit 1
fi
if out="$(PRECHECK_STRICT=1 env -u TAKT_WEB_TEST_DB bash "$ROOT/scripts/check-web-server.sh" 2>&1)"; then
  echo "  ПРОВАЛ: S4 под PRECHECK_STRICT=1 отсутствие базы принято"
  exit 1
fi
echo "  OK: S4 под PRECHECK_STRICT=1 отсутствие базы — ошибка"

echo "  Сторож гейта сервера проектов: все проверки пройдены (S0…S4)."

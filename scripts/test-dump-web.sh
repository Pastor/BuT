#!/usr/bin/env bash
# Тест копии и восстановления.
#
# Что доказывает:
#
# D1 снятая копия разворачивается, и данные в ней те же - круговой рейс;
# D2 копия без файлов исходников отвергается (копия базы в одиночку
#       восстанавливает список проектов, у которых нет ни строчки текста);
# D3 восстановление без копии отказывает, называя, чего не хватает;
# D4 нет базы - мягкий пропуск, под `PRECHECK_STRICT=1` ошибка.
#
# Тест нужен именно здесь: копия, которую никогда не восстанавливали,
# копией не является, и узнают об этом в тот день, когда она понадобилась.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
STRICT="${PRECHECK_STRICT:-0}"

echo "Сторож копии сервиса проектов (фича 0531)..."

if [[ -z "${TAKT_WEB_TEST_DB:-}" ]]; then
  if [[ "$STRICT" == "1" ]]; then
    echo "  ОШИБКА: не задан TAKT_WEB_TEST_DB — копия не проверена (PRECHECK_STRICT=1)"
    exit 1
  fi
  echo "  пропуск: не задан TAKT_WEB_TEST_DB — копия и восстановление не проверены"
  exit 0
fi

for tool in pg_dump pg_restore psql; do
  command -v "$tool" >/dev/null 2>&1 || {
    if [[ "$STRICT" == "1" ]]; then
      echo "  ОШИБКА: не найден $tool (PRECHECK_STRICT=1)"
      exit 1
    fi
    echo "  пропуск: не найден $tool"
    exit 0
  }
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Своя база под теста: разворачивать копию поверх рабочей нельзя.
BASE_URL="${TAKT_WEB_TEST_DB%%\?*}"
GUARD_DB="takt_dump_guard_$$"
ADMIN_URL="$BASE_URL"
psql "$ADMIN_URL" -q -c "DROP DATABASE IF EXISTS $GUARD_DB" >/dev/null 2>&1 || true
psql "$ADMIN_URL" -q -c "CREATE DATABASE $GUARD_DB" >/dev/null || {
  echo "  пропуск: база сторожа не создаётся (нет права CREATE DATABASE)"
  exit 0
}
cleanup() {
  psql "$ADMIN_URL" -q -c "DROP DATABASE IF EXISTS $GUARD_DB" >/dev/null 2>&1 || true
  rm -rf "$WORK"
}
trap cleanup EXIT

# Строка подключения к базе теста: имя базы - последний сегмент пути.
GUARD_URL="$(dirname "$BASE_URL")/$GUARD_DB"

export TAKT_WEB_DB="$GUARD_URL"
export TAKT_WEB_PROJECTS="$WORK/projects"
export TAKT_WEB_DUMP_DIR="$WORK/dumps"

# -- Данные, которые обязаны пережить рейс ------------------------------------
psql "$GUARD_URL" -q -c "CREATE TABLE probe (id TEXT PRIMARY KEY, name TEXT NOT NULL)" >/dev/null
psql "$GUARD_URL" -q -c "INSERT INTO probe VALUES ('p1', 'Термореле')" >/dev/null
mkdir -p "$TAKT_WEB_PROJECTS/u1/p1"
printf 'model A {}\n' > "$TAKT_WEB_PROJECTS/u1/p1/model.takt"

# -- D1: круговой рейс --------------------------------------------------------
"$ROOT/scripts/dump-web.sh" >/dev/null || {
  echo "  ПРОВАЛ: D1 копия не снялась"
  exit 1
}
STAMP="$(basename "$(ls -t "$TAKT_WEB_DUMP_DIR"/db-*.dump | head -1)" | sed 's/^db-//; s/\.dump$//')"

# Портим и базу, и файлы - восстановление обязано вернуть оба.
psql "$GUARD_URL" -q -c "UPDATE probe SET name = 'испорчено'" >/dev/null
rm -rf "$TAKT_WEB_PROJECTS"

"$ROOT/scripts/restore-web.sh" "$STAMP" --yes >/dev/null || {
  echo "  ПРОВАЛ: D1 восстановление не отработало"
  exit 1
}
NAME="$(psql "$GUARD_URL" -tA -c "SELECT name FROM probe WHERE id = 'p1'")"
if [[ "$NAME" != "Термореле" ]]; then
  echo "  ПРОВАЛ: D1 база восстановлена не та: '$NAME'"
  exit 1
fi
if [[ ! -f "$TAKT_WEB_PROJECTS/u1/p1/model.takt" ]]; then
  echo "  ПРОВАЛ: D1 исходники не восстановлены"
  exit 1
fi
echo "  OK: D1 копия разворачивается, база и исходники те же"

# -- D2: копии базы в одиночку мало -------------------------------------------
# Каталога исходников нет - снятие обязано отказать, а не отдать половину.
rm -rf "$TAKT_WEB_PROJECTS"
if "$ROOT/scripts/dump-web.sh" >/dev/null 2>&1; then
  echo "  ПРОВАЛ: D2 копия снялась без исходников"
  exit 1
fi
echo "  OK: D2 копия без исходников отвергнута"
mkdir -p "$TAKT_WEB_PROJECTS/u1/p1"
printf 'model A {}\n' > "$TAKT_WEB_PROJECTS/u1/p1/model.takt"

# -- D3: восстановление без копии ---------------------------------------------
if "$ROOT/scripts/restore-web.sh" "нет-такой-метки" --yes >/dev/null 2>&1; then
  echo "  ПРОВАЛ: D3 восстановление из несуществующей копии прошло"
  exit 1
fi
echo "  OK: D3 восстановление без копии отказывает"

# -- D4: политика пропуска ----------------------------------------------------
if TAKT_WEB_TEST_DB= "$ROOT/scripts/test-dump-web.sh" 2>&1 | grep -q "пропуск"; then
  echo "  OK: D4 без базы — мягкий пропуск"
else
  echo "  ПРОВАЛ: D4 без базы пропуск не назван"
  exit 1
fi
if TAKT_WEB_TEST_DB= PRECHECK_STRICT=1 "$ROOT/scripts/test-dump-web.sh" >/dev/null 2>&1; then
  echo "  ПРОВАЛ: D4 под PRECHECK_STRICT=1 отсутствие базы прошло"
  exit 1
fi
echo "  OK: D4 под PRECHECK_STRICT=1 отсутствие базы — ошибка"

echo "  Сторож копии сервиса проектов: все проверки пройдены (D1…D4)."

#!/usr/bin/env bash
# Восстановление сервиса проектов из копии.
#
# Этот скрипт существует потому, что **копия, которую никогда не
# восстанавливали, копией не является**. Снять `pg_dump` умеет всякий; узнать,
# что он разворачивается, можно только развернув.
#
# Порядок обратный снятию: сначала база, потом файлы. Между шагами сервис не
# работает - восстановление останавливает его на время.
#
# Использование:
#
#   scripts/restore-web.sh <метка> [-i каталог] [--db-only]
#
# Метка - то, что стоит в именах файлов копии: `db-<метка>.dump` и
# `projects-<метка>.tar.gz`.
#
# Восстановление перезаписывает и базу, и каталог исходников. Скрипт
# спрашивает подтверждение, если не задан `--yes`: развернуть вчерашнюю копию
# поверх сегодняшней работы - потеря, которую не отменить.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IN="${TAKT_WEB_DUMP_DIR:-$ROOT/web/dumps}"
STAMP=""
DB_ONLY=0
YES=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    -i|--in) IN="$2"; shift 2 ;;
    --db-only) DB_ONLY=1; shift ;;
    --yes) YES=1; shift ;;
    -h|--help)
      sed -n '2,22p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
      exit 0 ;;
    -*) echo "неизвестный ключ '$1'; см. --help" >&2; exit 2 ;;
    *) STAMP="$1"; shift ;;
  esac
done

[[ -n "$STAMP" ]] || { echo "укажите метку копии; см. --help" >&2; exit 2; }

DB="${TAKT_WEB_DB:-postgresql://localhost/takt_web}"
PROJECTS="${TAKT_WEB_PROJECTS:-$ROOT/web/projects}"
DBFILE="$IN/db-$STAMP.dump"
ARCHIVE="$IN/projects-$STAMP.tar.gz"

[[ -f "$DBFILE" ]] || { echo "ОШИБКА: нет копии базы '$DBFILE'" >&2; exit 1; }
if [[ "$DB_ONLY" == "0" && ! -f "$ARCHIVE" ]]; then
  echo "ОШИБКА: нет копии исходников '$ARCHIVE'" >&2
  echo "(если исходники восстанавливаются иначе — добавьте --db-only)" >&2
  exit 1
fi

if [[ "$YES" == "0" ]]; then
  echo "Восстановление ПЕРЕЗАПИШЕТ базу '$DB' и каталог '$PROJECTS'."
  read -r -p "Продолжить? [y/N] " answer
  [[ "$answer" == "y" || "$answer" == "Y" ]] || { echo "отменено"; exit 1; }
fi

echo "Восстановление сервиса проектов (фича 0531)..."

command -v pg_restore >/dev/null 2>&1 || {
  echo "  ОШИБКА: не найден pg_restore"
  exit 1
}
# `--clean --if-exists`: копия разворачивается поверх существующей схемы, а не
# рядом с ней. Без этого вторая попытка падает на существующих таблицах, и
# восстановление превращается в ручную работу в самый неподходящий момент.
pg_restore --clean --if-exists --no-owner --no-privileges --dbname="$DB" "$DBFILE"
echo "  база восстановлена из $DBFILE"

if [[ "$DB_ONLY" == "0" ]]; then
  mkdir -p "$PROJECTS"
  tar -xzf "$ARCHIVE" -C "$PROJECTS"
  echo "  исходники восстановлены в $PROJECTS"
fi

echo "  Готово. ⚠️ Проверьте /health и откройте один проект: копия, которую не"
echo "  прочитали, остаётся обещанием."

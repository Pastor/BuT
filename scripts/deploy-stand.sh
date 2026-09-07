#!/usr/bin/env bash
#
# Выкатка на стенд: `ssh стенд 'git pull && make up'`.
#
# Что делает:
#   1. проверяет, что выкатывать есть что и что рабочее дерево чисто;
#   2. толкает текущую ветку в `origin` (иначе `git pull` на стенде возьмёт
#      вчерашнее, а выкатка отрапортует об успехе);
#   3. на стенде: `git pull`, подъём стека (`scripts/stand.sh up`);
# 4. спрашивает `<адрес><префикс>/health` снаружи - через nginx.
#
# Шаг 4 обязателен и идёт через прокси, а не по `127.0.0.1`. Стек,
# поднявшийся на стенде, и сервис, доступный по адресу, - разные утверждения:
# между ними стоит nginx, и именно он ломается при смене префикса.
#
# Сборка идёт на стенде: образ собирается там же, где
# работает, и промежуточных артефактов между машинами не существует. Цена -
# на стенде нужен Docker и минуты на сборку Rust.
#
# Использование:
#   scripts/deploy-stand.sh [хост-из-ssh-config] [-p префикс] [--dirty]
#
# Настройки: TAKT_STAND (хост), TAKT_STAND_DIR (каталог клона на стенде),
# TAKT_STAND_URL (внешний адрес для проверки), TAKT_WEB_BASE_PATH (префикс).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
STAND="${TAKT_STAND:-takt-stand}"
DIR="${TAKT_STAND_DIR:-~/takt}"
PREFIX="${TAKT_WEB_BASE_PATH:-/takt}"
ALLOW_DIRTY=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    -p|--prefix) PREFIX="$2"; shift 2 ;;
    --dirty) ALLOW_DIRTY=1; shift ;;
    -h|--help) sed -n '2,26p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
    -*) echo "неизвестный ключ '$1'; см. --help" >&2; exit 2 ;;
    *) STAND="$1"; shift ;;
  esac
done

cd "$ROOT"
BRANCH="$(git rev-parse --abbrev-ref HEAD)"
URL="${TAKT_STAND_URL:-}"

echo "Выкатка на стенд '$STAND' (ветка $BRANCH, префикс $PREFIX)..."

# -- 1. Дерево чисто ----------------------------------------------------------
# Выкатка НЕсохранённого - это выкатка того, чего нет в истории: на стенде
# окажется код, который никто не сможет воспроизвести.
if [[ -n "$(git status --porcelain)" ]]; then
  if [[ "$ALLOW_DIRTY" == "1" ]]; then
    echo "  ВНИМАНИЕ: дерево грязное, но задан --dirty — на стенд уедет то, что в origin"
  else
    echo "  ОШИБКА: рабочее дерево грязное — закоммитьте либо уберите правки"
    git status --short | head -10
    echo "  (осознанно выкатываете последний коммит поверх правок? добавьте --dirty)"
    exit 1
  fi
fi

# -- 2. Ветка в origin --------------------------------------------------------
echo "  Отправляем ветку в origin..."
git push origin "$BRANCH"

# -- 3. Подъём на стенде ------------------------------------------------------
echo "  Поднимаем на стенде..."
# У ssh стоят "живые" пробы: оборванный канал иначе висит бесконечно, и
# выкладка выглядит вечной сборкой - так и было 2026-09-05, когда удалённая
# сторона умерла, а локальный клиент этого не заметил.
SSH=(ssh -o ServerAliveInterval=30 -o ServerAliveCountMax=6 -o ConnectTimeout=15)

"${SSH[@]}" "$STAND" "set -euo pipefail
  cd $DIR
  git fetch --prune origin
  git checkout $BRANCH
  git reset --hard origin/$BRANCH
  TAKT_WEB_BASE_PATH=$PREFIX scripts/stand.sh up" || {
  echo "  ОШИБКА: подъём на стенде не удался"
  echo "  Журнал: ssh $STAND 'cd $DIR && scripts/stand.sh status'"
  exit 1
}

# -- 4. Проверка снаружи ------------------------------------------------------
if [[ -z "$URL" ]]; then
  # Адрес не задан - берём хост из ssh-конфига: он же и есть имя стенда.
  HOSTNAME_OF_STAND="$(ssh -G "$STAND" | awk '/^hostname /{print $2; exit}')"
  URL="https://${HOSTNAME_OF_STAND}"
fi
echo "  Проверяем снаружи: ${URL}${PREFIX}/health"
CODE="$(curl -s -o /dev/null -w '%{http_code}' -m 15 "${URL}${PREFIX}/health" || echo '---')"
if [[ "$CODE" != "200" ]]; then
  # Пробуем http: сертификата может не быть - это названный случай, а не сбой.
  CODE="$(curl -s -o /dev/null -w '%{http_code}' -m 15 "http://${URL#https://}${PREFIX}/health" || echo '---')"
fi
if [[ "$CODE" != "200" ]]; then
  echo "  ОШИБКА: снаружи сервис не отвечает (код $CODE)"
  echo "  Стек мог подняться — между ним и адресом стоит nginx:"
  echo "    sudo PREFIX=$PREFIX scripts/setup-nginx-takt.sh"
  exit 1
fi

echo "  Выкачено: ${URL}${PREFIX}/"

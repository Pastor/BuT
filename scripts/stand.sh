#!/usr/bin/env bash
# Управление стендом сервиса проектов.
#
# Четыре действия и ни одним больше - запуск, гашение, проверка, перезапуск:
#
# scripts/stand.sh up поднять (собрать образ и запустить)
# scripts/stand.sh down погасить (данные остаются в томах)
# scripts/stand.sh status проверить: что запущено и жив ли сервис
# scripts/stand.sh restart перезапустить сервер, не трогая базу
#
# Скрипт **не удаляет тома**: `docker compose down -v` уносит и базу, и
# исходники пользователей. Такое делают руками и осознанно, а не командой,
# которую набирают по привычке.
#
# `status` спрашивает `/health`, а не "поднят ли контейнер": контейнер,
# который поднят и не видит базы, живым не является - ровно за этим `/health`
# и ходит в базу.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEPLOY="$ROOT/web/deploy"
# Префикс входит в адрес проверки живости: сервер вкладывает под него весь
# роутер, и при `TAKT_WEB_BASE_PATH=/takt` `/health` живёт по `/takt/health`.
# Без этого `status` спрашивал бы несуществующий адрес и объявлял живой стенд
# мёртвым (нашлось при разборе выкатки под префиксом, d).
# Префикс и адрес привязки берутся из файла окружения, если их нет в самом
# окружении: `docker compose` читает `web/deploy/.env` сам, а скрипт - нет, и
# проверка живости стучалась в `/health` вместо `/takt/health`. Сервер отвечал
# `404`, скрипт объявлял подъём неудавшимся и печатал журнал - при живом
# стенде. Класс найден выкаткой 2026-09-06: "ошибка: сервис не ответил" пришла
# сразу после "Started", и сервис при этом работал.
#
# Читается ровно две переменные, а не весь файл: рядом с ними лежат пароль
# базы и секрет подписи, и втягивать их в окружение ради адреса незачем.
env_value() {
  [[ -f "$DEPLOY/.env" ]] || return 0
  sed -n "s/^$1=//p" "$DEPLOY/.env" | tail -1 | tr -d '"'"'"'"'
}
BIND="${TAKT_WEB_BIND:-$(env_value TAKT_WEB_BIND)}"
BIND="${BIND:-127.0.0.1:8730}"
PREFIX="${TAKT_WEB_BASE_PATH:-$(env_value TAKT_WEB_BASE_PATH)}"
PREFIX="${PREFIX:-/}"
[[ "$PREFIX" == "/" ]] && PREFIX=""
HEALTH="http://$BIND${PREFIX}/health"

compose() {
  # `docker compose` (плагин) либо `docker-compose` (старый бинарник): на
  # стендах встречаются оба, и падать из-за этого незачем.
  #
  # Имя проекта - `takt`, и оно задано и здесь, и в файле стека: на стенде
  # рядом работает другой сервис, а имя по умолчанию берётся у каталога.
  if docker compose version >/dev/null 2>&1; then
    docker compose -p takt --project-directory "$DEPLOY" -f "$DEPLOY/docker-compose.yml" "$@"
  elif command -v docker-compose >/dev/null 2>&1; then
    docker-compose -p takt --project-directory "$DEPLOY" -f "$DEPLOY/docker-compose.yml" "$@"
  else
    echo "ОШИБКА: не найден ни 'docker compose', ни 'docker-compose'" >&2
    exit 1
  fi
}

# Отдаётся ли сервис наружу: включение локейшенов Takt в сайте соседа.
#
# Проверка нужна потому, что включение живёт в чужом файле. Домен занят
# соседним сервисом, Takt приносит только локейшены, и его
# `include` дописывается в сайт соседа. Собственный скрипт соседа переписывает
# этот файл целиком - и включение исчезает. 2026-09-06 так и случилось: сервер
# работал, `/health` изнутри отвечал `200`, а снаружи приходил `404`, и понять
# это можно было только чтением конфигурации nginx.
#
# Проверка мягкая: без nginx (своя машина, контейнерный прокси) она молчит -
# отсутствие снаружи-прокси не делает стенд сломанным. Она говорит лишь тогда,
# когда сниппет положен, а включения на него нет: это и есть та самая пропажа.
check_nginx_include() {
  local snippet=/etc/nginx/snippets/takt-locations.conf
  [[ -r "$snippet" ]] || return 0
  if grep -rqs "takt-locations.conf" /etc/nginx/sites-enabled/ /etc/nginx/conf.d/ 2>/dev/null; then
    echo "  nginx: включение локейшенов Takt на месте"
    return 0
  fi
  echo "  nginx: локейшены Takt НЕ включены — снаружи сервис отдаваться не будет."
  echo "         Сниппет положен ($snippet), а include на него нет: скорее всего"
  echo "         сайт соседа переписан его собственной настройкой."
  echo "         Вернуть: sudo scripts/setup-nginx-takt.sh"
  return 1
}

case "${1:-}" in
  up)
    echo "Стенд Takt: подъём..."
    compose up -d --build
    echo "  Ждём готовности сервиса..."
    for _ in $(seq 1 60); do
      if curl -fsS "$HEALTH" >/dev/null 2>&1; then
        echo "  Стенд поднят: http://$BIND${PREFIX}/"
        exit 0
      fi
      sleep 2
    done
    # Молчаливого "наверное, поднялся" здесь нет: подъём, о котором нельзя
    # сказать, удался ли он, - это отказ, о котором узнают позже и хуже.
    echo "  ОШИБКА: сервис не ответил на /health за две минуты"
    compose logs --tail=40 server
    exit 1
    ;;
  down)
    echo "Стенд Takt: гашение..."
    compose down
    echo "  Погашен. Тома (база и исходники) целы — их снимают руками."
    ;;
  status)
    compose ps
    echo
    if curl -fsS "$HEALTH" >/dev/null 2>&1; then
      echo "  /health: сервис отвечает и видит базу"
    else
      echo "  /health: НЕ отвечает (контейнер может быть поднят — этого мало)"
      exit 1
    fi
    check_nginx_include
    ;;
  restart)
    echo "Стенд Takt: перезапуск сервера..."
    # Только сервер: база перезапуска не требует, а лишний её останов - это
    # разорванные соединения на ровном месте.
    compose restart server
    for _ in $(seq 1 60); do
      if curl -fsS "$HEALTH" >/dev/null 2>&1; then
        echo "  Перезапущен."
        exit 0
      fi
      sleep 2
    done
    echo "  ОШИБКА: после перезапуска сервис не отвечает"
    exit 1
    ;;
  *)
    sed -n '2,20p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
    exit 2
    ;;
esac

#!/usr/bin/env bash
# Тест настройки nginx на общем стенде.
#
# Что доказывает:
#
# N1 сниппет несёт префикс во всех местах (локейшен, редирект, заголовок);
# N2 **завершающей косой в `proxy_pass` нет** - префикс обязан доехать до
#       сервера: он вкладывает под него весь роутер;
# N3 **своего `server`-блока нет и локейшенов от корня нет**: домен занят
#       соседом, второй блок с тем же именем nginx игнорирует, а корневой
#       `/api/` отобрал бы у соседа адрес;
# N4 заголовок кеша не переопределяется: правило живёт в сервисе;
# N5 вставка включения **идемпотентна** и попадает в обслуживающий блок, а
#       не в тот, что перенаправляет на https;
# N6 негодный префикс отвергается;
# N7 скрипт стенда знает про префикс (`<префикс>/health`);
# N8 стек изолирован именем проекта: тома соседа не делятся с нашими.
#
# Ни nginx, ни прав root не нужно: скрипт умеет печатать (`--print`) и
# вставлять во временный файл (`--insert-only`). Иначе правило проверялось бы
# только на стенде - то есть уже после того, как соседа уронили.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SETUP="$ROOT/scripts/setup-nginx-takt.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

echo "Сторож настройки nginx на общем стенде (фича 0531)..."

render() {
  PREFIX="${1:-/takt}" SERVER_NAME=stand.example.org BACKEND=127.0.0.1:8730 \
    "$SETUP" --print
}
render > "$WORK/conf"

# -- N1 -----------------------------------------------------------------------
for needle in "location = /takt {" "location /takt/ {" "X-Forwarded-Prefix /takt" "return 301 /takt/;"; do
  grep -qF "$needle" "$WORK/conf" || { echo "  ПРОВАЛ: N1 нет '$needle'"; exit 1; }
done
echo "  OK: N1 префикс стоит в локейшене, редиректе и заголовке"

# -- N2 -----------------------------------------------------------------------
# Косая в `proxy_pass` срезает префикс, а сервер вкладывает под него весь
# роутер: со срезанием сервис получает адрес, которого у него нет. Наружу тогда
# работает только редирект, всё остальное отвечает 404 при здоровом стеке
#
if grep -qF "proxy_pass http://takt_backend/;" "$WORK/conf"; then
  echo "  ПРОВАЛ: N2 в proxy_pass завершающая косая — префикс срезается,"
  echo "          и сервис под префиксом отвечает 404 на собственные адреса"
  exit 1
fi
grep -qF "proxy_pass http://takt_backend;" "$WORK/conf" || {
  echo "  ПРОВАЛ: N2 нет проксирования на апстрим"
  exit 1
}
echo "  OK: N2 префикс доезжает до сервера — косой в proxy_pass нет"

# -- N3 -----------------------------------------------------------------------
# Своего server-блока быть не должно вовсе: домен занят соседом.
if grep -qE '^\s*server\s*\{' "$WORK/conf"; then
  echo "  ПРОВАЛ: N3 скрипт создаёт свой server-блок — он отберёт домен у соседа"
  exit 1
fi
BAD="$(grep -oE '^\s*location [^{]*\{' "$WORK/conf" \
      | sed 's/^[[:space:]]*location //; s/[[:space:]]*{//' \
      | grep -vE '^(= )?/takt' | grep -vE '^@takt' || true)"
if [[ -n "$BAD" ]]; then
  echo "  ПРОВАЛ: N3 есть локейшены вне префикса:"
  printf '    %s\n' $BAD
  exit 1
fi
echo "  OK: N3 своего server-блока нет, локейшены только под префиксом"

# -- N4 -----------------------------------------------------------------------
if grep -qE 'add_header[[:space:]]+Cache-Control|proxy_hide_header[[:space:]]+Cache-Control' "$WORK/conf"; then
  echo "  ПРОВАЛ: N4 конфиг переопределяет заголовок кеша — правило живёт в сервисе"
  exit 1
fi
echo "  OK: N4 заголовок кеша проходит от сервиса как есть"

# -- N5 -----------------------------------------------------------------------
# Сайт соседа: два блока, как у него, - 80 перенаправляет, 443 обслуживает.
cat > "$WORK/site" <<'SITE'
server {
    listen 80;
    server_name stand.example.org;
    location ^~ /.well-known/acme-challenge/ { root /var/www/certbot; }
    location / { return 301 https://$host$request_uri; }
}

server {
    listen 443 ssl;
    server_name stand.example.org;
    location / { return 404; }
    location /api/ { proxy_pass http://kjuru_backend; }
}
SITE
cp "$WORK/site" "$WORK/site.orig"
"$SETUP" --insert-only "$WORK/site" >/dev/null
"$SETUP" --insert-only "$WORK/site" >/dev/null
COUNT="$(grep -c 'takt-locations.conf' "$WORK/site" || true)"
if [[ "$COUNT" != "1" ]]; then
  echo "  ПРОВАЛ: N5 включений $COUNT — вставка не идемпотентна"
  exit 1
fi
# Включение обязано попасть в обслуживающий блок: в блоке-перенаправлении оно
# отдавало бы Takt по голому http в обход редиректа.
if ! awk '/listen 443/,0' "$WORK/site" | grep -q 'takt-locations.conf'; then
  echo "  ПРОВАЛ: N5 включение попало не в обслуживающий блок"
  sed -n '1,30p' "$WORK/site"
  exit 1
fi
# Чужого не тронули: строки соседа на месте.
while read -r original; do
  [[ -z "$original" ]] && continue
  grep -qF "$original" "$WORK/site" || {
    echo "  ПРОВАЛ: N5 из сайта соседа пропала строка: $original"
    exit 1
  }
done < "$WORK/site.orig"
echo "  OK: N5 включение одно, в обслуживающем блоке, чужое не тронуто"

# -- N6 -----------------------------------------------------------------------
for bad in "takt" "/takt/" "/такт" "/a b" "/"; do
  if render "$bad" >/dev/null 2>&1; then
    echo "  ПРОВАЛ: N6 негодный префикс '$bad' принят"
    exit 1
  fi
done
echo "  OK: N6 негодный префикс отвергнут"

# -- N7 -----------------------------------------------------------------------
grep -q 'TAKT_WEB_BASE_PATH' "$ROOT/scripts/stand.sh" || {
  echo "  ПРОВАЛ: N7 скрипт стенда не знает про префикс — живой стенд объявлялся"
  echo "          бы мёртвым: /health под префиксом живёт по <префикс>/health"
  exit 1
}
echo "  OK: N7 скрипт стенда спрашивает /health с учётом префикса"

# -- N8 -----------------------------------------------------------------------
# Без явного имени проекта `docker compose` берёт имя каталога (`deploy`), и
# тома соседа с тем же именем каталога делились бы с нашими молча.
grep -qE '^name: takt$' "$ROOT/web/deploy/docker-compose.yml" || {
  echo "  ПРОВАЛ: N8 у стека нет явного имени проекта"
  exit 1
}
grep -q -- '-p takt' "$ROOT/scripts/stand.sh" || {
  echo "  ПРОВАЛ: N8 скрипт стенда не задаёт имя проекта"
  exit 1
}
if grep -qE '^\s+- "\$\{[A-Z_]*DB_PORT' "$ROOT/web/deploy/docker-compose.yml"; then
  echo "  ПРОВАЛ: N8 стек публикует порт базы — на стенде 5432 занят соседом"
  exit 1
fi
echo "  OK: N8 стек изолирован именем проекта, порт базы наружу не публикуется"

# -- N9 -----------------------------------------------------------------------
# Наш файл включается внутрь server-блока соседа, и директива уровня блока в
# нём - это директива В его блоке. Повтор client_max_body_size nginx считает
# ошибкой ("directive is duplicate"), и конфигурация перестаёт приниматься
# Целиком: сосед не переживёт следующий reload. Замер 2026-09-05: у соседа на
# стенде client_max_body_size 32m в его же snippet, включённом в оба блока.
OUTSIDE="$(awk '
  /^### \/etc\/nginx\/snippets\/takt-locations\.conf/ { inside = 1; next }
  /^### / { inside = 0 }
  inside {
    line = $0
    sub(/#.*/, "", line)
    gsub(/^[ \t]+|[ \t]+$/, "", line)
    if (line == "") next
    if (depth == 0 && line !~ /^location/ && line !~ /^}/) print line
    depth += gsub(/{/, "{", line) - gsub(/}/, "}", line)
  }
' "$WORK/conf")"
if [[ -n "$OUTSIDE" ]]; then
  echo "  ПРОВАЛ: N9 директива уровня блока в файле локейшенов:"
  echo "$OUTSIDE" | sed 's/^/          /'
  echo "          она попадёт в server-блок СОСЕДА — повтор ломает весь конфиг"
  exit 1
fi
echo "  OK: N9 директив уровня блока нет — чужой server-блок не задет"

# -- N10 ----------------------------------------------------------------------
# Тело конфигурации печатается heredoc-ом с подстановкой (иначе не доедет
# ${PREFIX}), и потому bash исполняет обратные кавычки - даже в комментариях.
# Замер выкладки 2026-09-05: комментарий с `/takt/health` дал на стенде
# "/takt/health: No such file or directory", а в положенный файл вместо пути
# попала пустота. Печать обязана быть молчаливой: сообщение в stderr означает,
# что часть конфигурации выполнилась как команда.
render_err="$(PREFIX=/takt SERVER_NAME=stand.example.org BACKEND=127.0.0.1:8730 \
  "$SETUP" --print 2>&1 >/dev/null)"
if [[ -n "$render_err" ]]; then
  echo "  ПРОВАЛ: N10 печать конфигурации не молчит — heredoc что-то исполнил:"
  echo "$render_err" | head -3 | sed 's/^/    /'
  exit 1
fi
echo "  OK: N10 печать конфигурации молчалива — подстановок в тексте нет"

echo "  Сторож настройки nginx: все проверки пройдены (N1…N10)."

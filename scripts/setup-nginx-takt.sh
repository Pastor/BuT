#!/usr/bin/env bash
#
# Настройка nginx под локейшеном /takt на общем стенде (d;
# приём референса `setup_nginx_tamagotchi.sh`).
#
# **Своего `server`-блока этот скрипт не создаёт, и это главное решение.**
# На стенде уже живёт другой сервис (tamagotchi), и его блок занимает
# `server_name` на 80 и 443. Второй блок с тем же именем nginx **игнорирует**
# ("conflicting server name"), а `sites-enabled` читается по алфавиту - `takt`
# идёт раньше `tamagotchi`, и наш блок забрал бы домен себе, уронив уже
# развёрнутый сервис. Поэтому Takt приносит **локейшены**, а домен остаётся за
# тем, кто его занял.
#
# Что кладётся:
# /etc/nginx/conf.d/takt-upstream.conf - апстрим (контекст `http`)
# /etc/nginx/snippets/takt-proxy.conf - заголовки проксирования
# /etc/nginx/snippets/takt-locations.conf - локейшены `<ПРЕФИКС>`
# и в сайт соседа дописывается одна строка `include` - идемпотентно.
#
# **Цена решения названа:** собственный скрипт соседа переписывает свой сайт
# целиком, и наше включение при его следующем прогоне исчезнет. Этот скрипт
# такое обнаруживает и говорит; повторный прогон возвращает включение.
#
# Локейшенов от корня здесь нет. У референса они появились оттого, что его
# клиент ходит в API абсолютными путями; у нас страница строит адреса
# относительно себя (`<base href>` переписывает сервер по `TAKT_WEB_BASE_PATH`).
# Корневой `/api/` дал бы второй вход к сервису мимо преисправления - и отобрал бы
# адрес у соседа.
#
# Заголовки кеша не переопределяются: правило "форма адреса задаёт срок"
# живёт в сервисе, и второй его носитель разошёлся бы с первым
# молча. Заголовок апстрима проходит как есть, а smoke-тест показывает, что
# пришло.
#
# Запуск: sudo./setup-nginx-takt.sh
# Настройки:
#   SERVER_NAME=pastor.axatel.ru BACKEND=127.0.0.1:8730 PREFIX=/takt \
# SITE_TARGET=/etc/nginx/sites-available/tamagotchi sudo -E./setup-nginx-takt.sh
#
# Для теста: `--print` печатает всё, что положил бы, и ничего не трогает;
# `--insert-only ФАЙЛ` только дописывает включение в названный файл.
set -euo pipefail

SERVER_NAME="${SERVER_NAME:-pastor.axatel.ru}"
BACKEND="${BACKEND:-127.0.0.1:8730}"
PREFIX="${PREFIX:-/takt}"                 # без завершающей косой черты
BODY_LIMIT="${BODY_LIMIT:-2m}"
# Сайт соседа, в чей `server`-блок дописывается включение.
SITE_TARGET="${SITE_TARGET:-/etc/nginx/sites-available/tamagotchi}"

UPSTREAM_CONF=/etc/nginx/conf.d/takt-upstream.conf
PROXY_SNIPPET=/etc/nginx/snippets/takt-proxy.conf
LOC_SNIPPET=/etc/nginx/snippets/takt-locations.conf
INCLUDE_LINE="    include ${LOC_SNIPPET}; # Takt (scripts/setup-nginx-takt.sh)"

MODE=install
case "${1:-}" in
  --print) MODE=print ;;
  --insert-only) MODE=insert; SITE_TARGET="${2:?укажите файл: --insert-only ФАЙЛ}" ;;
  "") ;;
  *) echo "неизвестный ключ '$1'" >&2; exit 2 ;;
esac

log() { printf '\n== %s\n' "$*"; }
die() { printf '[x] %s\n' "$*" >&2; exit 1; }

# Преисправление попадает и в адрес, и в локейшен: чужая строка сломала бы конфиг.
[[ "$PREFIX" =~ ^/[A-Za-z0-9_-]+$ ]] \
  || die "префикс '$PREFIX' негоден: ожидается вида /takt, без завершающей косой"

upstream_text() {
cat <<CONF
# Апстрим сервиса Takt. Контекст \http\, поэтому файл лежит в
# conf.d, а не в snippets: внутри \server\ объявить апстрим нельзя.
# Файл создан scripts/setup-nginx-takt.sh - правьте скрипт, не файл.
upstream takt_backend {
    server ${BACKEND};
    keepalive 16;
}
CONF
}

proxy_text() {
cat <<CONF
# Заголовки проксирования к сервису Takt.
# Файл создан scripts/setup-nginx-takt.sh - правьте скрипт, не файл.
proxy_http_version 1.1;
proxy_set_header Host              \$host;
proxy_set_header X-Real-IP         \$remote_addr;
proxy_set_header X-Forwarded-For   \$proxy_add_x_forwarded_for;
proxy_set_header X-Forwarded-Proto \$scheme;
# Преисправление сообщается сервису: он знает его и сам (TAKT_WEB_BASE_PATH), но
# заголовок нужен тому, кто читает журналы.
proxy_set_header X-Forwarded-Prefix ${PREFIX};

# Модуль WebAssembly - три мегабайта: короткий таймаут рвал бы его загрузку.
proxy_connect_timeout 5s;
proxy_send_timeout    60s;
proxy_read_timeout    60s;
CONF
}

locations_text() {
cat <<CONF
# Локейшены сервиса Takt под преисправлением ${PREFIX}.
# Файл создан scripts/setup-nginx-takt.sh - правьте скрипт, не файл.
#
# Подключается внутрь чужого server-блока: домен занят соседним сервисом, и
# второй блок с тем же server_name nginx игнорирует.

# Директив уровня блока здесь нет ни одной, и это не стиль, а условие
# работоспособности соседа: файл включается внутрь его server-блока, а nginx
# считает повтор client_max_body_size в одном контексте ошибкой
# ("directive is duplicate") - конфигурация перестала бы приниматься целиком.
# Замер 2026-09-05: у соседа на стенде client_max_body_size 32m в его же
# snippet, включённом в оба блока. Поэтому предел тела задаётся внутри нашего
# локейшена: он и должен действовать только на наши адреса.

location = ${PREFIX} {
    return 301 ${PREFIX}/;
}

# Без завершающей косой в proxy_pass: преисправление доезжает до сервера как есть.
# Сервер поднят с TAKT_WEB_BASE_PATH и вкладывает под преисправление весь роутер -
# срежь его здесь, и сервис получит адрес, которого у него нет: /takt/health
# отвечает 200, а /health - 404. Замер выкладки 2026-09-05: со срезанием
# наружу работал только редирект, всё остальное отвечало 404 при здоровом
# стеке.
#
# Заголовок кеша не переопределяется: правило живёт в сервисе.
location ${PREFIX}/ {
    # Предел тела - тот же, что у сервиса (TAKT_WEB_BODY_LIMIT), и только для
    # наших адресов: у соседа свой, и менять его мы не вправе.
    client_max_body_size ${BODY_LIMIT};

    proxy_pass http://takt_backend;
    include ${PROXY_SNIPPET};

    # Сервис лежит - короткая заставка вместо страницы прокси. Заставка соседа
    # не трогается: она его.
    proxy_intercept_errors on;
    error_page 502 503 504 = @takt_maintenance;
}

location @takt_maintenance {
    default_type text/plain;
    return 503 "Takt: сервис недоступен, попробуйте позже\\n";
}
CONF
}

# Дописывает включение в server-блок чужого сайта. Идемпотентно.
#
# Блок выбирается осмысленно: у соседа их два (80 и 443), и блок, который
# лишь перенаправляет на https, обслуживать Takt не должен - иначе сервис
# оказался бы доступен по голому http в обход перенаправления.
insert_include() {
  local file="$1"
  [[ -f "$file" ]] || die "сайта '$file' нет: сначала разверните соседний сервис либо задайте SITE_TARGET"
  command -v python3 >/dev/null 2>&1 || die "нужен python3 для точной вставки; строка для ручной вставки:
${INCLUDE_LINE}"
  python3 - "$file" "$INCLUDE_LINE" <<'PY'
import sys

path, line = sys.argv[1], sys.argv[2]
text = open(path, encoding="utf-8").read()
if line.strip() in text:
    print("включение уже стоит — не дублируем")
    raise SystemExit(0)

lines = text.splitlines()
# Границы server-блоков: считаем фигурные скобки.
blocks, depth, start = [], 0, None
for index, raw in enumerate(lines):
    stripped = raw.split("#", 1)[0]
    if start is None and stripped.strip().startswith("server") and "{" in stripped:
        start, depth = index, 0
    if start is not None:
        depth += stripped.count("{") - stripped.count("}")
        if depth == 0:
            blocks.append((start, index))
            start = None

if not blocks:
    sys.exit("в файле нет ни одного server-блока — вставлять некуда")

def serving(block):
    """Блок, который ОБСЛУЖИВАЕТ, а не перенаправляет на https."""
    body = "\n".join(lines[block[0]:block[1] + 1])
    return "return 301 https://" not in body

candidates = [b for b in blocks if serving(b)]
if not candidates:
    sys.exit("все server-блоки только перенаправляют — Takt обслуживать негде")
# Последний обслуживающий: у соседа это блок 443, когда сертификат есть.
begin, _ = candidates[-1]
at = next(
    (i for i in range(begin, candidates[-1][1]) if lines[i].strip().startswith("server_name")),
    begin,
)
lines.insert(at + 1, line)
open(path, "w", encoding="utf-8").write("\n".join(lines) + "\n")
print(f"включение добавлено после строки {at + 1}")
PY
}

case "$MODE" in
  print)
    echo "### ${UPSTREAM_CONF}"; upstream_text
    echo "### ${PROXY_SNIPPET}"; proxy_text
    echo "### ${LOC_SNIPPET}";   locations_text
    echo "### include"; echo "$INCLUDE_LINE"
    exit 0 ;;
  insert)
    insert_include "$SITE_TARGET"
    exit 0 ;;
esac

[ "$(id -u)" -eq 0 ] || die "нужны права root: sudo $0"

log "Апстрим: ${UPSTREAM_CONF}"
install -d -m 755 /etc/nginx/conf.d /etc/nginx/snippets
upstream_text > "$UPSTREAM_CONF"

log "Заголовки проксирования: ${PROXY_SNIPPET}"
proxy_text > "$PROXY_SNIPPET"

log "Локейшены: ${LOC_SNIPPET}"
locations_text > "$LOC_SNIPPET"

log "Включение в сайт соседа: ${SITE_TARGET}"
insert_include "$SITE_TARGET"

log "Проверка конфигурации"
nginx -t || die "конфигурация не принята — nginx НЕ перезагружен"
systemctl reload nginx || service nginx reload || die "перезагрузка не удалась"

# -- Smoke --------------------------------------------------------------------
SCHEME="https"
curl -s -o /dev/null -m 5 "https://${SERVER_NAME}/" || SCHEME="http"
BASE="${SCHEME}://${SERVER_NAME}"
code() { curl -s -o /dev/null -w '%{http_code}' -m 10 "$1" || echo "---"; }
header() { curl -sI -m 10 "$1" | tr -d '\r' | sed -n 's/^[Cc]ache-[Cc]ontrol: //p' | head -1; }

log "Проверка"
printf '  %-26s %s (ожидается 301)\n' "${PREFIX}"        "$(code "${BASE}${PREFIX}")"
printf '  %-26s %s (ожидается 200)\n' "${PREFIX}/"       "$(code "${BASE}${PREFIX}/")"
printf '  %-26s %s (ожидается 200)\n' "${PREFIX}/health" "$(code "${BASE}${PREFIX}/health")"
printf '  %-26s %s\n' "кеш страницы"     "$(header "${BASE}${PREFIX}/")"
printf '  %-26s %s\n' "кеш version.json" "$(header "${BASE}${PREFIX}/version.json")"
# Сосед обязан продолжать работать: проверка стоит здесь, потому что именно
# её отсутствие превращает "поставил себе" в "уронил чужое".
printf '  %-26s %s (сосед жив)\n' "/health" "$(code "${BASE}/health")"

echo
echo "  адрес: ${BASE}${PREFIX}/"
echo "  ⚠️ Если сосед перезапустит свою настройку nginx, включение Takt"
echo "     исчезнет вместе с его сайтом — прогоните этот скрипт снова."

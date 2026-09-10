#!/usr/bin/env bash
#
# Иконки установленного приложения (PWA) из знака страницы.
#
# Растровые копии лежат в дереве, а не собираются при выкладке: стенд собирает
# образ без растеризатора, и зависимость ради четырёх неизменных картинок
# была бы дороже самих картинок. Знак правят в SVG и перезапускают скрипт:
#
#   favicon.svg             - знак со скруглённым фоном: иконки "any";
#   icons/icon-maskable.svg - фон во весь квадрат, знак в безопасной зоне (80 %):
#                             система вырезает из него круг либо скруглённый
#                             квадрат, и знак не обрезается.
#
# Нужен `rsvg-convert` (librsvg).
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
STATIC="$ROOT/web/static"
command -v rsvg-convert >/dev/null || { echo "нужен rsvg-convert (librsvg)" >&2; exit 1; }
rsvg-convert -w 192 -h 192 "$STATIC/favicon.svg" -o "$STATIC/icons/icon-192.png"
rsvg-convert -w 512 -h 512 "$STATIC/favicon.svg" -o "$STATIC/icons/icon-512.png"
rsvg-convert -w 512 -h 512 "$STATIC/icons/icon-maskable.svg" -o "$STATIC/icons/icon-maskable-512.png"
rsvg-convert -w 180 -h 180 "$STATIC/icons/icon-maskable.svg" -o "$STATIC/icons/apple-touch-icon.png"
echo "иконки собраны: $STATIC/icons"

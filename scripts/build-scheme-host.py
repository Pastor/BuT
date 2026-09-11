#!/usr/bin/env python3
"""Страница холста для панели редактора: вырезка из разметки страницы.

Второй разметки холста в проекте нет и быть не должно: разойдись она с первой,
панель редактора показывала бы вчерашний холст, и сказать об этом было бы
некому. Поэтому страница панели собирается отсюда - из `index.html`.

Берётся блок области схемы и окно настроек; всё остальное (редактор, вывод,
прогон, шапка, полка режимов) панели не нужно - схему в ней смотрят рядом с
текстом, который открыт в самом редакторе.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

# Узлы, без которых холст не соберётся. Список - обещание вырезки: пропал узел
# из разметки страницы - сборка панели падает здесь, а не в чужом редакторе.
REQUIRED = [
    'id="scheme"', 'id="sheet"', 'id="map"', 'id="stage"', 'id="legend"',
    'id="crumbs"', 'id="scheme-up"', 'id="nav"', 'id="scheme-empty"',
    'id="scheme-modal"', 'id="scheme-tabs"', 'id="scheme-settings"',
    'id="panel-run"', 'id="panel-view"', 'id="panel-sheet"',
    'data-dock="tl"', 'data-dock="tr"', 'data-dock="bl"', 'data-dock="br"',
]

PAGE = """<!doctype html>
<html lang="ru">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Takt</title>
<link rel="stylesheet" href="app.css">
</head>
<body class="scheme-host">
{body}
<script type="module" src="scheme-host-boot.js"></script>
</body>
</html>
"""

BOOT = """// Запуск холста в панели редактора: тот же приём, что у страницы.
//
// Отдельным файлом, а не встроенным скриптом: относительный `import` из
// разметки считался бы от документа, а страница лежит в каталоге бандла.

import { main } from "./scheme-host.js";

main();
"""


def block(html: str, opening: str) -> str:
    """Вырезает блок разметки от `opening` до парного закрытия."""
    start = html.index(opening)
    depth = 0
    for match in re.finditer(r"<(/?)div\b[^>]*?(/?)>", html[start:]):
        if match.group(2) == "/":
            continue
        depth += -1 if match.group(1) else 1
        if depth == 0:
            return html[start : start + match.end()]
    raise SystemExit(f"разметка не закрыта: {opening}")


def build(static: Path) -> str:
    html = (static / "index.html").read_text(encoding="utf-8")
    scheme = block(html, '<div data-panel="scheme"')
    modal = block(html, '<div id="scheme-modal"')
    # Область схемы на странице скрыта до выбора вкладки; в панели она одна.
    scheme = scheme.replace('class="panel panel-scheme" hidden', 'class="panel panel-scheme"', 1)
    # Кнопки экспорта у панели нет: модуля в панели нет, рисует экспорт модуль
    # страницы, а панель выгружает командной строкой `takt-sim export`. Кнопка без
    # обработчика молчала бы на щелчок.
    scheme, cut = re.subn(
        r'\s*<!-- Экспорт:[^>]*?-->\s*<button id="scheme-export".*?</button>', "", scheme, count=1, flags=re.S
    )
    if cut != 1 or 'id="scheme-export"' in scheme:
        raise SystemExit("кнопка экспорта не вырезана из холста панели")
    body = f"<main class=\"work\">\n{scheme}\n</main>\n{modal}\n"
    missing = [node for node in REQUIRED if node not in body]
    if missing:
        raise SystemExit("в вырезке нет узлов холста: " + ", ".join(missing))
    return PAGE.format(body=body)


def main() -> int:
    root = Path(__file__).resolve().parent.parent
    static = root / "web" / "static"
    out = Path(sys.argv[1]) if len(sys.argv) > 1 else static.parent / "dist"
    out.mkdir(parents=True, exist_ok=True)
    (out / "scheme-host.html").write_text(build(static), encoding="utf-8")
    (out / "scheme-host-boot.js").write_text(BOOT, encoding="utf-8")
    print(f"  холст панели редактора собран: {out}/scheme-host.html")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

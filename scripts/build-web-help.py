#!/usr/bin/env python3
"""Справка страницы из документа `book/`: HTML Typst -> фрагмент для окна справки.

Typst выгружает документ HTML-файлом целиком (`typst compile --format html`).
Окну справки нужно другое: тело документа без обвязки, заголовки с устойчивыми
якорями (по ним работают оглавление и память раздела) и подсветка кода ролями
страницы, а не цветами печати.

Что делает:

- берёт содержимое `<body>`;
- снимает служебные пустые `span` вёрстки Typst и пробелы-`span`;
- цвет подсветки (палитра `book/takt.tmTheme`) заменяет классом роли `tok-*`:
  код в справке красится так же, как в редакторе, и в тёмной теме тоже;
- даёт каждому заголовку `h2`...`h5` якорь `h-<слова заголовка>` - по тексту, а не
  по номеру: вставка главы перенумерует разделы, и память читателя уехала бы на
  чужой раздел;
- внешние ссылки открывает в новой вкладке: справка живёт поверх рабочего места,
  и уход со страницы унёс бы несохранённую работу.

⚠️ Цвет, которого нет в таблице, - отказ, а не пропуск: новая роль в палитре
печати иначе доехала бы в справку цветом, который на тёмной теме не читается.

Использование: build-web-help.py ДОКУМЕНТ.html СПРАВКА.html
"""

import html
import re
import sys
from pathlib import Path

# Палитра `book/takt.tmTheme` -> роль страницы. Ключевое слово и тип делят цвет,
# а различаются начертанием: ключевое слово полужирное (`<strong>`).
ROLES = {
    "#204a87": "tok-type",
    "#ce5c00": "tok-operator",
    "#0000cf": "tok-number",
    "#75507b": "tok-constant",
    "#4e9a06": "tok-string",
    "#8f5902": "tok-comment",
    "#000000": "tok-function",
}


def slug(text):
    """Якорь заголовка по его словам: буквы и цифры любого алфавита, дефисы."""
    words = re.findall(r"[^\W_]+", text.lower())
    return "h-" + "-".join(words)[:80] if words else "h"


def convert(source):
    match = re.search(r"<body>(.*)</body>", source, re.S)
    if not match:
        raise SystemExit("  ОШИБКА: в выгрузке Typst нет <body>")
    body = match.group(1)
    body = body.replace('<span style="display: inline-block"></span>', "")
    body = re.sub(r'<span style="white-space: pre-wrap">([^<]*)</span>', r"\1", body)
    body = re.sub(r'<span style="display: inline-block">([^<]*)</span>', r"\1", body)
    body = re.sub(
        r'<strong><span style="color: #204a87">',
        '<strong><span class="tok-keyword">',
        body,
        flags=re.I,
    )

    def role(found):
        color = found.group(1).lower()
        if color not in ROLES:
            raise SystemExit(f"  ОШИБКА: цвет подсветки {color} не сопоставлен роли страницы")
        return f'class="{ROLES[color]}"'

    body = re.sub(r'style="color: (#[0-9a-fA-F]{6})"', role, body)

    used = set(re.findall(r' id="([^"]+)"', body))

    def anchor(found):
        level, attrs, inner = found.group(1), found.group(2), found.group(3)
        if " id=" in attrs:
            return found.group(0)
        text = html.unescape(re.sub(r"<[^>]+>", "", inner))
        base = slug(text)
        name, n = base, 2
        while name in used:
            name, n = f"{base}-{n}", n + 1
        used.add(name)
        return f'<h{level}{attrs} id="{name}">{inner}</h{level}>'

    body = re.sub(r"<h([2-5])([^>]*)>(.*?)</h\1>", anchor, body, flags=re.S)
    body = re.sub(r'<a href="(https?://[^"]+)"', r'<a href="\1" target="_blank" rel="noopener"', body)
    left = len(re.findall(r'style="', body))
    return body, left


def main():
    if len(sys.argv) != 3:
        raise SystemExit(__doc__)
    source = Path(sys.argv[1]).read_text(encoding="utf-8")
    body, left = convert(source)
    heads = len(re.findall(r"<h[2-5][^>]* id=", body))
    if heads == 0:
        raise SystemExit("  ОШИБКА: в справке нет ни одного заголовка - оглавление было бы пустым")
    Path(sys.argv[2]).write_text(body, encoding="utf-8")
    print(f"  справка: заголовков {heads}, {len(body) // 1024} КиБ, встроенных стилей осталось {left}")


if __name__ == "__main__":
    main()

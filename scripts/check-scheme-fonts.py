#!/usr/bin/env python3
"""check-scheme-fonts.py — шрифты чертежа совпадают со шрифтами страницы.

Чертёж схемы (`takt-scheme`) набирается теми же начертаниями, что лист на
странице, но в TTF: растеризатор WOFF2 не читает. Файлов два набора, и второй
разошёлся бы с первым молча: картинка экспорта набиралась бы другими глифами, чем
холст, а заметить это можно только глазом на двух картинках рядом.

Проверка разворачивает каждый `web/static/font/<имя>.woff2` в TTF и сравнивает с
`takt-scheme/fonts/<имя>.ttf` таблицы символов (`cmap`), метрик (`hmtx`) и
порядок глифов. Нужен `fontTools` с `brotli`; нет их - мягкий пропуск, под
`PRECHECK_STRICT=1` - ошибка.

Использование:

    python3 scripts/check-scheme-fonts.py
    python3 scripts/check-scheme-fonts.py --self-test
"""

import io
import os
import shutil
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from gatelib import require_input  # noqa: E402  (путь к помощнику известен только здесь)

ROOT = Path(os.environ.get("FONTS_ROOT", Path(__file__).resolve().parent.parent))
NAMES = ("FiraCode-Regular", "GOST2.304-81TypeA-Slanted")


def tables(font):
    """То, что видит набор текста: символы, метрики, глифы."""
    return {
        "cmap": dict(font.getBestCmap()),
        "hmtx": dict(font["hmtx"].metrics),
        "glyphs": list(font.getGlyphOrder()),
    }


def check(root):
    """Список расхождений и число сверенных пар."""
    from fontTools.ttLib import TTFont

    problems = []
    checked = 0
    for name in NAMES:
        page = root / "web/static/font" / f"{name}.woff2"
        own = root / "takt-scheme/fonts" / f"{name}.ttf"
        if not page.is_file() or not own.is_file():
            problems.append(f"{name}: нет пары файлов ({page.name}, {own.name})")
            continue
        woff = TTFont(page)
        woff.flavor = None
        buffer = io.BytesIO()
        woff.save(buffer)
        buffer.seek(0)
        a, b = tables(TTFont(buffer)), tables(TTFont(own))
        checked += 1
        for key in a:
            if a[key] != b[key]:
                problems.append(f"{name}: таблица {key} чертежа расходится со страницей")
    return problems, checked


def main():
    strict = os.environ.get("PRECHECK_STRICT") == "1"
    try:
        import fontTools  # noqa: F401
        import brotli  # noqa: F401
    except ImportError:
        if strict:
            sys.exit("ОШИБКА: нет fontTools/brotli — шрифты чертежа не сверены (PRECHECK_STRICT=1)")
        print("  пропуск: нет fontTools/brotli — шрифты чертежа не сверены")
        return
    if "--self-test" in sys.argv:
        return self_test()
    problems, checked = check(ROOT)
    note = require_input("пары шрифтов страницы и чертежа", checked, minimum=len(NAMES))
    if problems:
        sys.exit("ОШИБКА: шрифты чертежа:\n  " + "\n  ".join(problems))
    print(f"Шрифты чертежа: {note}, глифы и метрики совпадают со страницей.")


def self_test():
    """Порченый шрифт ловится, согласованный проходит."""
    from fontTools.ttLib import TTFont

    with tempfile.TemporaryDirectory() as tmp:
        tree = Path(tmp)
        for sub in ("web/static/font", "takt-scheme/fonts"):
            shutil.copytree(ROOT / sub, tree / sub)
        problems, checked = check(tree)
        assert not problems and checked == len(NAMES), f"согласованное дерево отвергнуто: {problems}"
        own = tree / "takt-scheme/fonts" / f"{NAMES[1]}.ttf"
        font = TTFont(own)
        glyph = next(iter(font["hmtx"].metrics))
        width, lsb = font["hmtx"].metrics[glyph]
        font["hmtx"].metrics[glyph] = (width + 10, lsb)
        font.save(own)
        problems, _ = check(tree)
        assert any("hmtx" in p for p in problems), "изменённая метрика не поймана"
        (tree / "takt-scheme/fonts" / f"{NAMES[0]}.ttf").unlink()
        problems, _ = check(tree)
        assert any("нет пары" in p for p in problems), "пропавший файл не пойман"
    print("  Самопроверка шрифтов чертежа пройдена: согласованное принято, метрика и пропажа пойманы.")


if __name__ == "__main__":
    main()

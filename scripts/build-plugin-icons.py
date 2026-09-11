#!/usr/bin/env python3
"""Значки плагина IntelliJ из значка страницы: один рисунок у всех.

Носитель значка Takt один - `web/static/favicon.svg`. Плагин получает его копии
нужного размера: значок файла `.takt` (16 точек) и значок плагина в списке
установленных и на витрине (`META-INF/pluginIcon.svg`, 40 точек). Рисунок не
меняется - копия получает только размер корня и строку о происхождении; правится
значок страницы, а плагин пересобирается этим скриптом.

С ключом `--check` скрипт ничего не пишет, а сверяет лежащие в плагине значки с
тем, что собрал бы: так проверка не держит второй копии правила сборки.

Расширению Zed значок не нужен: его описание расширения поля значка не имеет, а
значок файла в Zed задаёт тема значков целиком, а не язык.
"""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from gatelib import require_input  # noqa: E402

ROOT = Path(os.environ.get("PI_ROOT") or Path(__file__).resolve().parent.parent)
FAVICON = ROOT / "web" / "static" / "favicon.svg"
RESOURCES = ROOT / "extensions" / "intellij-takt" / "src" / "main" / "resources"

# Копии: путь внутри ресурсов плагина и сторона в точках.
ICONS = [("icons/takt.svg", 16), ("META-INF/pluginIcon.svg", 40)]

ORIGIN = "<!-- Собрано из web/static/favicon.svg скриптом scripts/build-plugin-icons.py: правится значок страницы. -->"


def derive(favicon: str, side: int) -> str:
    """Копия значка страницы стороной `side`: размер корня и строка происхождения."""
    root = re.search(r"<svg\b[^>]*>", favicon)
    if not root:
        raise ValueError("в значке страницы нет корня <svg>")
    tag = re.sub(r'\s(?:width|height)="[^"]*"', "", root.group(0))
    tag = tag.replace("<svg", f'<svg width="{side}" height="{side}"', 1)
    return favicon[: root.start()] + tag + "\n  " + ORIGIN + favicon[root.end() :]


def main() -> int:
    check = "--check" in sys.argv[1:]
    if not FAVICON.is_file():
        print(f"  ОШИБКА: значка страницы нет: {FAVICON}")
        return 1
    favicon = FAVICON.read_text(encoding="utf-8")
    problems = []
    for name, side in ICONS:
        try:
            want = derive(favicon, side)
        except ValueError as error:
            print(f"  ОШИБКА: {error}")
            return 1
        path = RESOURCES / name
        if check:
            if not path.is_file():
                problems.append(f"нет в плагине: {name}")
            elif path.read_text(encoding="utf-8") != want:
                problems.append(f"разошёлся со значком страницы: {name}")
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(want, encoding="utf-8")
    if not check:
        print(f"  значки плагина собраны из значка страницы: {len(ICONS)}")
        return 0
    described = require_input("значки плагина", len(ICONS), minimum=2)
    if problems:
        print("  ОШИБКА: значки плагина разошлись со значком страницы:")
        for problem in problems:
            print(f"    {problem}")
        print("  Пересоберите: scripts/build-plugin-icons.py")
        return 1
    print(f"  Значки плагина совпадают со значком страницы: {described}.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

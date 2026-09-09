#!/usr/bin/env python3
"""Ресурсы панели схемы для плагина IntelliJ: сборка из `web/`.

Копии страницы в плагине не хранится: она собирается отсюда. Иначе панель
редактора и страница разошлись бы молча - плагин собирается вне предкоммита, и
сказать об этом было бы некому.

Берётся ровно то, до чего дотягивается точка входа панели: модули по графу
импортов, лист стилей, шрифты и словари. Модуль под браузер сюда не входит -
граф панели даёт языковой сервер, который у плагина уже есть.
"""

from __future__ import annotations

import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
STATIC = ROOT / "web" / "static"
DEST = ROOT / "extensions" / "intellij-takt" / "src" / "main" / "resources" / "webview"

# Точка входа панели и то, что тянется не импортом: стили, шрифты, словари.
ENTRY = "scheme-host-boot.js"
EXTRA = ["app.css"]
TREES = ["font", "i18n"]

IMPORT = re.compile(r'^\s*(?:import|export)\s[^\n]*?from\s+"\./([\w./-]+)"', re.M)


def modules(entry: Path, built: dict[str, str]) -> set[str]:
    """Модули, до которых дотягивается точка входа: обход по импортам."""
    seen: set[str] = set()
    stack = [entry.name]
    while stack:
        name = stack.pop()
        if name in seen:
            continue
        seen.add(name)
        text = built.get(name) or (STATIC / name).read_text(encoding="utf-8")
        for match in IMPORT.finditer(text):
            stack.append(match.group(1))
    return seen


def build(dest: Path) -> list[str]:
    """Собирает ресурсы панели в `dest`; возвращает список положенных файлов."""
    # Страница панели - производная от разметки страницы: её строит соседний
    # скрипт, и второго носителя разметки в проекте нет.
    tmp = dest.parent / ".webview-page"
    tmp.mkdir(parents=True, exist_ok=True)
    subprocess.run([sys.executable, str(ROOT / "scripts" / "build-scheme-host.py"), str(tmp)], check=True)
    built = {path.name: path.read_text(encoding="utf-8") for path in tmp.iterdir()}

    if dest.exists():
        shutil.rmtree(dest)
    dest.mkdir(parents=True)
    placed: list[str] = []
    for name, text in built.items():
        (dest / name).write_text(text, encoding="utf-8")
        placed.append(name)
    for name in sorted(modules(Path(ENTRY), built)):
        if name in built:
            continue
        shutil.copy2(STATIC / name, dest / name)
        placed.append(name)
    for name in EXTRA:
        shutil.copy2(STATIC / name, dest / name)
        placed.append(name)
    for tree in TREES:
        shutil.copytree(STATIC / tree, dest / tree)
        placed.extend(f"{tree}/{item.name}" for item in sorted((STATIC / tree).iterdir()))
    shutil.rmtree(tmp)
    return sorted(placed)


def main() -> int:
    dest = Path(sys.argv[1]) if len(sys.argv) > 1 else DEST
    placed = build(dest)
    print(f"  ресурсы панели плагина собраны: {len(placed)} файлов в {dest}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

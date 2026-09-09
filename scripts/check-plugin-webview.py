#!/usr/bin/env python3
"""Ресурсы панели плагина против страницы: сверка, а не доверие.

Плагин собирается вне предкоммита, и расхождение между страницей и панелью
пришло бы молча: панель показывала бы вчерашний холст, а инструменты промолчали
бы. Поэтому ресурсы плагина не правятся руками - они собираются из `web/`, а эта
проверка пересобирает их во временный каталог и сверяет с тем, что лежит в
плагине.

Граница названа: сверка держит форму ресурсов, а не поведение панели. Что панель
показывает то же, что страница, проверяет человек: сборка плагина идёт вне
предкоммита.
"""

from __future__ import annotations

import filecmp
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from gatelib import require_input  # noqa: E402

ROOT = Path(__file__).resolve().parent.parent
DEST = ROOT / "extensions" / "intellij-takt" / "src" / "main" / "resources" / "webview"


def files(root: Path) -> dict[str, Path]:
    return {str(path.relative_to(root)): path for path in sorted(root.rglob("*")) if path.is_file()}


def main() -> int:
    if not DEST.exists():
        print("  ОШИБКА: ресурсов панели плагина нет; соберите scripts/build-plugin-webview.py")
        return 1
    with tempfile.TemporaryDirectory() as tmp:
        fresh = Path(tmp) / "webview"
        subprocess.run(
            [sys.executable, str(ROOT / "scripts" / "build-plugin-webview.py"), str(fresh)],
            check=True,
            stdout=subprocess.DEVNULL,
        )
        want, have = files(fresh), files(DEST)
        problems = []
        for name in sorted(set(want) | set(have)):
            if name not in have:
                problems.append(f"нет в плагине: {name}")
            elif name not in want:
                problems.append(f"лишний в плагине: {name}")
            elif not filecmp.cmp(want[name], have[name], shallow=False):
                problems.append(f"разошёлся со страницей: {name}")

        # Выборка обязана иметь нижнюю границу: пустая сверка молчит так же, как
        # согласованная, и отличить их по коду возврата нельзя. Десять файлов -
        # это страница, точка входа, лист стилей и модули холста; меньше их не
        # бывает даже при урезанной сборке.
        described = require_input("сверенные файлы панели", len(want), minimum=10)

        if problems:
            print("  ОШИБКА: ресурсы панели плагина разошлись со страницей:")
            for problem in problems:
                print(f"    {problem}")
            print("  Пересоберите: scripts/build-plugin-webview.py")
            return 1
        print(f"  Ресурсы панели плагина: {described} - расхождений со страницей нет.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

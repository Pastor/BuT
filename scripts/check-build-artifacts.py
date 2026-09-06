#!/usr/bin/env python3
"""Артефакты сборки не попадают в репозиторий (фича 0377).

# Повод

Коммит `3c6bec19` (фича 0334) принёс в **корень** репозитория три файла
`libshl.rlib`, `libvarshift.rlib`, `libvarshift32.rlib` — 84 КБ вывода ручных
проб `rustc`, запущенных без `--out-dir`. Их никто не заметил: `git status`
после `git add -A` показывает такие файлы наравне с исходниками, а замер фичи
делается в спешке.

Класс шире одного случая: пробы целей запускают `rustc`, `cc`, `verilator`,
`yosys` и `iec2c`, и каждый охотно кладёт вывод в текущий каталог.

# Что проверяется

Отслеживаемые гитом файлы против списка расширений артефактов. Гейт смотрит
именно на **отслеживаемые** (`git ls-files`), а не на рабочее дерево: файл,
лежащий рядом и не добавленный в индекс, вреда не делает — его отсекает
`.gitignore`.

⚠️ Проверка идёт **по расширению**, а не по содержимому: `file(1)` на 4000
файлов стоит секунды, а расширение artefact'а известно точно.

Использование:
    scripts/check-build-artifacts.py            # проверка репозитория
    BA_ROOT=<путь> scripts/check-build-artifacts.py   # проверка копии (сторож)
"""

import os
import subprocess
import sys

# Расширения, которых в репозитории быть не должно. Список закрытый: каждая
# запись - вывод известного инструмента, а не догадка.
ARTIFACT_SUFFIXES = (
    ".rlib",  # rustc --crate-type=lib
    ".rmeta",  # rustc, метаданные крейта
    ".o",  # cc -c
    ".obj",  # cc -c (windows)
    ".so",  # разделяемая библиотека
    ".dylib",  # то же, macOS
    ".d",  # файлы зависимостей cc/rustc
)

# Исключения - файлы, чьё расширение совпадает случайно. Пусто: если запись
# появится, она обязана нести причину рядом.
ALLOWED: tuple[str, ...] = ()


def tracked_files(root: str) -> list[str]:
    """Список отслеживаемых файлов; пустой, если каталог не репозиторий."""
    try:
        out = subprocess.run(
            ["git", "ls-files"],
            cwd=root,
            capture_output=True,
            text=True,
            check=True,
        )
    except (subprocess.CalledProcessError, FileNotFoundError):
        return []
    return [line for line in out.stdout.splitlines() if line]


def main() -> int:
    root = os.environ.get("BA_ROOT") or os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    files = tracked_files(root)
    if not files:
        print(f"ОТКАЗ: в {root} нет отслеживаемых файлов — это не репозиторий.")
        return 1
    found = [
        path
        for path in files
        if path.endswith(ARTIFACT_SUFFIXES) and path not in ALLOWED
    ]
    if found:
        print("Артефакты сборки в репозитории (фича 0377):")
        for path in found:
            print(f"  {path}")
        print()
        print("Эти файлы — вывод инструментов (rustc/cc/verilator), а не исходники.")
        print("Удалите их (`git rm --cached <файл>`) и проверьте, что расширение")
        print("закрыто в .gitignore: пробы целей кладут вывод в текущий каталог.")
        return 1
    print(f"Артефакты сборки: проверено {len(files)} отслеживаемых файлов, артефактов нет.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

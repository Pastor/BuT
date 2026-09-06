#!/usr/bin/env python3
"""snapshot-book-font.py — съёмка покрытия шрифта документа `book/` (фича 0146).

Печатает в stdout снимок таблицы `cmap` шрифта, которым набирается документ, —
кодовые точки, сжатые в диапазоны. Снимок кладётся в
`scripts/book-font-charset.txt` и служит критерием гейту
`scripts/check-book-glyphs.py`.

Зачем снимок, а не опрос шрифта на месте: гейт обязан работать в CI и на машине
без TeX, без установленного шрифта и без сторонних библиотек (ADR 0146,
Option C). Этот скрипт — единственное место, где нужен `fontTools`, и зовут его
только при пересъёмке (смена шрифта или его версии).

    pip install fonttools
    python3 scripts/snapshot-book-font.py > scripts/book-font-charset.txt

Имя семейства берётся из `book/book.toml` (`mainfont`), а не задаётся здесь:
иначе съёмщик и документ разъехались бы молча. Файлы начертаний ищутся через
`fc-list`; если его нет — по каталогам шрифтов пользователя и системы.
"""

import os
import re
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BOOK_TOML = os.path.join(ROOT, "book", "book.toml")

FONT_DIRS = [
    os.path.expanduser("~/Library/Fonts"),
    "/Library/Fonts",
    "/System/Library/Fonts",
    os.path.expanduser("~/.local/share/fonts"),
    os.path.expanduser("~/.fonts"),
    "/usr/share/fonts",
    "/usr/local/share/fonts",
]


def main_font_family():
    """Имя семейства шрифта текста документа — из `book.toml`."""
    with open(BOOK_TOML, encoding="utf-8") as handle:
        for line in handle:
            match = re.match(r'\s*mainfont\s*=\s*"([^"]+)"', line)
            if match:
                return match.group(1)
    sys.exit("ОШИБКА: в book/book.toml нет поля mainfont")


def font_files(family):
    """Пути к файлам начертаний семейства."""
    try:
        out = subprocess.run(
            ["fc-list", family, "file"],
            capture_output=True,
            text=True,
            check=True,
        ).stdout
        paths = {line.split(":")[0].strip() for line in out.splitlines() if line.strip()}
        if paths:
            return sorted(paths)
    except (FileNotFoundError, subprocess.CalledProcessError):
        pass

    # Запасной путь: `fc-list` есть не везде. Имя файла у начертаний семейства
    # строится из имени семейства без пробелов (`Fira Code` -> `FiraCode-*.ttf`).
    stem = family.replace(" ", "")
    found = []
    for directory in FONT_DIRS:
        if not os.path.isdir(directory):
            continue
        for entry in os.listdir(directory):
            if entry.startswith(stem) and entry.lower().endswith((".ttf", ".otf")):
                found.append(os.path.join(directory, entry))
    if not found:
        sys.exit(f"ОШИБКА: файлы шрифта '{family}' не найдены (искал в {FONT_DIRS})")
    return sorted(found)


def to_ranges(codepoints):
    """Сжимает отсортированные кодовые точки в непрерывные диапазоны."""
    ranges = []
    start = prev = codepoints[0]
    for point in codepoints[1:]:
        if point == prev + 1:
            prev = point
        else:
            ranges.append((start, prev))
            start = prev = point
    ranges.append((start, prev))
    return ranges


def main():
    try:
        from fontTools.ttLib import TTFont
    except ImportError:
        sys.exit("ОШИБКА: нужен fontTools (pip install fonttools) — только для пересъёмки")

    family = main_font_family()
    paths = font_files(family)

    faces = {}
    for path in paths:
        font = TTFont(path, fontNumber=0)
        version = next(
            (r.toUnicode() for r in font["name"].names if r.nameID == 5 and r.platformID == 3),
            "<версия не указана>",
        )
        faces[os.path.basename(path)] = (set(font.getBestCmap().keys()), version)

    names = sorted(faces)
    sets = [faces[n][0] for n in names]
    # Пересечение, а не объединение: документ печатается несколькими начертаниями
    # (полужирный - заголовки и врезки), и глиф, который есть только в Regular,
    # выпадет из полужирного текста ровно так же молча.
    covered = set.intersection(*sets)
    identical = covered == set.union(*sets)
    version = faces.get(f"{family.replace(' ', '')}-Regular.ttf", (None, faces[names[0]][1]))[1]

    ranges = to_ranges(sorted(covered))

    print("# Снимок покрытия шрифта документа book/ (фича 0146).")
    print("#")
    print("# НАЗНАЧЕНИЕ. Символ, которого нет в шрифте, PDF не роняет: xelatex печатает")
    print("# предупреждение Missing character, а глиф МОЛЧА выпадает из вывода. Сборка")
    print("# документа в предкоммит не входит — нужны mdbook, mdbook-pandoc, pandoc,")
    print("# xelatex и сам шрифт. Снимок позволяет проверять покрытие ТЕКСТОМ, без них:")
    print("# scripts/check-book-glyphs.py сверяет с ним каждый символ документа.")
    print("#")
    print("# ЧТО ЭТО. Кодовые точки таблицы cmap, сжатые в диапазоны. Это ЗАМЕР, а не")
    print("# редполитика: файл говорит, что шрифт УМЕЕТ нарисовать, а не что уместно в")
    print("# тексте. Уместность — дело автора и ревью.")
    print("#")
    print(f"# ШРИФТ: {family}")
    print(f"# ВЕРСИЯ: {version}")
    print(f"# НАЧЕРТАНИЙ: {len(names)} ({', '.join(names)})")
    print(f"# CMAP НАЧЕРТАНИЙ ИДЕНТИЧНЫ: {'да' if identical else 'НЕТ — взято пересечение'}")
    print(f"# КОДОВЫХ ТОЧЕК: {len(covered)}   ДИАПАЗОНОВ: {len(ranges)}")
    print("#")
    print("# ПЕРЕСЪЁМКА (при смене шрифта или его версии):")
    print("#")
    print("#   pip install fonttools")
    print("#   python3 scripts/snapshot-book-font.py > scripts/book-font-charset.txt")
    print("#")
    print("# ГРАНИЦА. Снимок фиксирует покрытие НАЗВАННОЙ версии. Если на машине сборки")
    print("# стоит другая версия с меньшим покрытием, гейт промолчит, а xelatex")
    print("# предупредит: конечный арбитр — `make -C book build`. Это та же граница")
    print("# «согласованность, а не правильность», что у гейта адреса репозитория.")
    print("#")
    print("# Формат строки: <начало>-<конец> шестнадцатерично, границы включительно.")
    print()
    for start, end in ranges:
        print(f"{start:04X}-{end:04X}")


if __name__ == "__main__":
    main()

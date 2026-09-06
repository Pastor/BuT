#!/usr/bin/env python3
"""check-book-glyphs.py — символы документа `book/` вне шрифта (фича 0146).

Ловит класс дефекта, стоивший фикса 0132-01: символ, которого нет в шрифте
документа, **не роняет** сборку PDF — `xelatex` печатает предупреждение
`Missing character`, а глиф молча выпадает из вывода. Так `⚠️` (U+26A0 + U+FE0F)
прожил во врезке до ручной сборки, оставив врезку без пометки.

Проверка идёт **по тексту**, без TeX и без установленного шрифта: критерий —
снимок таблицы `cmap` (`scripts/book-font-charset.txt`, снимается
`scripts/snapshot-book-font.py`). Так гейт работает и в CI, где нет ни шрифта,
ни `mdbook`, ни `pandoc`.

Область — **то, что попадает в PDF**, не больше и не меньше:

- `book/src/**/*.typ`  — текст документа (кроме шаблона и оглавления сборки:
  `template.typ`/`main.typ` несут только код и комментарии, как Makefile);
- `book/src/**/*.takt` — примеры, вставляемые `#example(read(…))` в блоки кода;
- поля `doc-title`/`doc-authors`/`doc-description` шаблона — титульный лист.

Прочие файлы `book/` не проверяются: они не рендерятся. Это не придирка —
`book/Makefile` **уже** содержит `⚠` в комментарии, и проверка каталога целиком
дала бы ложный отказ с первого прогона.

Использование:

    python3 scripts/check-book-glyphs.py              # проверка дерева
    python3 scripts/check-book-glyphs.py --self-test  # проверка самой ловушки
"""

import os
import re
import sys
import unicodedata

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BOOK_SRC = os.path.join(ROOT, "book", "src")
BOOK_TEMPLATE = os.path.join(ROOT, "book", "src", "template.typ")

# Файлы сборки документа: код и комментарии, а не текст PDF. Исключены по той же
# причине, по какой не проверяется `book/Makefile`, - он тоже несёт `` в
# комментарии, и проверка "всего каталога" дала бы ложный отказ.
NOT_RENDERED = ("template.typ", "main.typ")
CHARSET = os.path.join(ROOT, "scripts", "book-font-charset.txt")

# Управляющие символы разметки: в PDF они не глифы, а разбиение текста.
IGNORED = {"\n", "\r", "\t"}

# Поля шаблона, попадающие на титульный лист. Файл берётся не целиком: его
# комментарии - такой же служебный текст, как в Makefile, и вправе содержать
# любые символы.
TITLE_FIELDS = ("doc-title", "doc-authors", "doc-description")


def load_charset(path=CHARSET):
    """Диапазоны кодовых точек снимка + имя шрифта из его шапки."""
    ranges = []
    family = None
    try:
        with open(path, encoding="utf-8") as handle:
            for line in handle:
                line = line.strip()
                if line.startswith("#"):
                    match = re.match(r"#\s*ШРИФТ:\s*(.+)$", line)
                    if match:
                        family = match.group(1).strip()
                    continue
                if not line:
                    continue
                start, _, end = line.partition("-")
                ranges.append((int(start, 16), int(end, 16)))
    except OSError as error:
        sys.exit(f"ОШИБКА: не прочитать снимок {path}: {error}")
    if not ranges:
        sys.exit(f"ОШИБКА: снимок {path} не содержит ни одного диапазона")
    return ranges, family


def covered(codepoint, ranges):
    for start, end in ranges:
        if start <= codepoint <= end:
            return True
    return False


def book_font_family():
    """Шрифт текста документа — из шаблона (`set text(font: …)`)."""
    with open(BOOK_TEMPLATE, encoding="utf-8") as handle:
        for line in handle:
            match = re.search(r'set text\(font:\s*"([^"]+)"', line)
            if match:
                return match.group(1)
    return None


def rendered_files():
    """Файлы, чей текст попадает в PDF."""
    result = []
    for directory, _, names in os.walk(BOOK_SRC):
        for name in sorted(names):
            if name in NOT_RENDERED:
                continue
            if name.endswith((".typ", ".takt")):
                result.append(os.path.join(directory, name))
    return sorted(result)


def title_lines():
    """Строки шаблона, попадающие на титульный лист, с их номерами."""
    lines = []
    with open(BOOK_TEMPLATE, encoding="utf-8") as handle:
        for number, line in enumerate(handle, start=1):
            if line.lstrip().startswith("//"):
                continue
            for field in TITLE_FIELDS:
                if re.match(rf'\s*#let\s+{field}\s*=', line):
                    lines.append((number, line.rstrip("\n")))
                    break
    return lines


def scan_text(path, text, ranges, problems):
    for number, line in enumerate(text.splitlines(), start=1):
        scan_line(path, number, line, ranges, problems)


def scan_line(path, number, line, ranges, problems):
    for column, char in enumerate(line, start=1):
        if char in IGNORED or covered(ord(char), ranges):
            continue
        try:
            name = unicodedata.name(char)
        except ValueError:
            name = "<символ без имени в Unicode>"
        problems.append((path, number, column, char, name))


def check(ranges, family):
    """Возвращает список находок; пустой список — гейт пройден."""
    problems = []
    for path in rendered_files():
        with open(path, encoding="utf-8") as handle:
            scan_text(os.path.relpath(path, ROOT), handle.read(), ranges, problems)
    for number, line in title_lines():
        scan_line("book/src/template.typ", number, line, ranges, problems)
    return problems


def report(problems):
    print("Символы вне шрифта документа book/ (фича 0146):", file=sys.stderr)
    for path, number, column, char, name in problems:
        print(
            f"  {path}:{number}:{column}: U+{ord(char):04X} {name}",
            file=sys.stderr,
        )
    print(
        "\nЭти символы шрифт документа нарисовать не умеет: PDF соберётся, но глиф\n"
        "МОЛЧА выпадет из вывода. Замените символ на принятый в документе приём\n"
        "(например, врезку «> **Осторожно.** …» вместо значка) либо, если символ\n"
        "шрифтом всё же покрыт, пересоберите снимок:\n"
        "  python3 scripts/snapshot-book-font.py > scripts/book-font-charset.txt",
        file=sys.stderr,
    )


def self_test(ranges):
    """Проверяет, что ловушка взведена: подложенный символ обязан быть пойман.

    Без этой проверки зелёный гейт неотличим от гейта, который пропускает всё:
    сам по себе он говорит «находок нет», и на пустом снимке сказал бы то же.
    """
    planted = "⚠️"  # ⚠️ — ровно тот символ, что дал фикс 0132-01
    problems = []
    scan_line("<self-test>", 1, f"Проба {planted} проба", ranges, problems)
    if len(problems) != 2:
        sys.exit(
            f"САМОПРОВЕРКА ПРОВАЛЕНА: подложенный ⚠️ дал {len(problems)} находок "
            "вместо 2 — ловушка не взведена, гейт зеленеет по случайности"
        )
    codes = {f"U+{ord(p[3]):04X}" for p in problems}
    if codes != {"U+26A0", "U+FE0F"}:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: найдены {sorted(codes)}, ожидались U+26A0 и U+FE0F")
    # Обратная сторона: обычный текст документа ложных находок давать не должен -
    # иначе "ловушка ловит" означало бы лишь "ловушка ловит всё подряд".
    clean = []
    scan_line("<self-test>", 1, "Модель — «Takt», φ ≥ 0, шаг → далее…", ranges, clean)
    if clean:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: обычный текст дал ложные находки: {clean}")
    print("  самопроверка гейта: ловушка взведена (⚠️ ловится, обычный текст — нет)")


def main():
    ranges, snapshot_family = load_charset()

    # A-2: снимок сделан для конкретного шрифта. Сменили шрифт документа -
    # снимок стал неверным, и без этой проверки он стал бы неверным молча.
    book_family = book_font_family()
    if book_family and snapshot_family and book_family != snapshot_family:
        sys.exit(
            f"ОШИБКА: шаблон набирает документ шрифтом '{book_family}', а снимок\n"
            f"scripts/book-font-charset.txt снят с '{snapshot_family}'. Пересоберите снимок:\n"
            "  python3 scripts/snapshot-book-font.py > scripts/book-font-charset.txt"
        )

    if "--self-test" in sys.argv[1:]:
        self_test(ranges)
        return 0

    problems = check(ranges, book_family)
    if problems:
        report(problems)
        return 1
    print(
        f"Символы документа book/: проверено {len(rendered_files())} файлов "
        f"+ титульные поля шаблона, символов вне шрифта нет."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

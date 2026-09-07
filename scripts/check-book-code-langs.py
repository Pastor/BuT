#!/usr/bin/env python3
"""check-book-code-langs.py — языки блоков кода документа против подсветки (0269).

Блок ```ЯЗЫК в документе подсвечивает syntect (движок Typst). Его встроенных
определений хватает не на всё: Structured Text и EBNF среди них нет, и до фичи
0269 такие блоки печатались **чёрным** — при том что соседний ```c подсвечен.
Замечает это только глаз, и то на цветной странице: `typst compile` на
неизвестном языке блока даёт **код 0** и ни слова.

Поэтому язык блока обязан быть **объявлен** здесь, в одном из двух списков:

- `BUILTIN` — определение есть у syntect (проверено рендером);
- `LOCAL` — определение лежит рядом с документом (`book/*.sublime-syntax`) и
  подключено в `book/src/template.typ`.

Новый язык роняет гейт: это не запрет, а требование сказать, откуда берётся его
подсветка. Молчание неотличимо от «забыли».

⚠️ Гейт проверяет **объявленность и подключённость**, а не то, как выглядит
подсветка. Цвет — на человеке (правило 4): его смотрят рендером страницы.

Использование:

    python3 scripts/check-book-code-langs.py
    python3 scripts/check-book-code-langs.py --self-test
"""

import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gatelib import require_input  # noqa: E402  (путь к помощнику известен только здесь)

ROOT = os.environ.get(
    "BCL_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)
BOOK_SRC = os.path.join(ROOT, "book", "src")
TEMPLATE = os.path.join(BOOK_SRC, "template.typ")

# Языки, чьё определение есть у syntect. Список проверен рендером пробной
# страницы: у каждого блок выходит цветным.
BUILTIN = {"c", "rust", "systemverilog", "json", "bash", "sh", "text", "diff", "yaml"}

# Языки, чьё определение лежит рядом с документом. Значение - имя файла: проверка
# требует, чтобы файл существовал И был подключён в шаблоне (иначе определение
# есть, а подсветки нет - самая тихая из возможных поломок).
LOCAL = {
    "takt": "takt.sublime-syntax",
    "st": "st.sublime-syntax",
    "ebnf": "ebnf.sublime-syntax",
}


def block_languages(src_dir):
    """Языки блоков ```ЯЗЫК во всех `.typ` документа."""
    found = {}
    for base, _, files in os.walk(src_dir):
        for name in sorted(files):
            if not name.endswith(".typ"):
                continue
            path = os.path.join(base, name)
            with open(path, encoding="utf-8") as handle:
                for number, line in enumerate(handle, 1):
                    match = re.match(r"^```([A-Za-z][A-Za-z0-9_+-]*)\s*$", line)
                    if match:
                        found.setdefault(match.group(1), []).append(
                            f"{os.path.relpath(path, ROOT)}:{number}"
                        )
    return found


def linked_syntaxes(template_text):
    """Файлы подсветки, подключённые в шаблоне (`#set raw(syntaxes: …)`)."""
    return set(re.findall(r'"/([A-Za-z0-9_.-]+\.sublime-syntax)"', template_text))


def check(languages, linked, book_dir):
    """Список находок `(класс, язык, пояснение)`; пустой — гейт пройден."""
    problems = []
    for lang, places in sorted(languages.items()):
        if lang in BUILTIN:
            continue
        if lang not in LOCAL:
            problems.append(
                (
                    "C1",
                    lang,
                    f"язык блока не объявлен: {places[0]} (и ещё {len(places) - 1})"
                    if len(places) > 1
                    else f"язык блока не объявлен: {places[0]}",
                )
            )
            continue
        syntax = LOCAL[lang]
        if not os.path.isfile(os.path.join(book_dir, syntax)):
            problems.append(("C2", lang, f"нет файла подсветки book/{syntax}"))
        elif syntax not in linked:
            problems.append(
                ("C3", lang, f"book/{syntax} не подключён в src/template.typ")
            )
    # Обратная сторона: объявленный локально язык, которого в документе нет, -
    # мёртвая запись. Она обещает подсветку, которой никто не пользуется.
    for lang in sorted(LOCAL):
        if lang not in languages:
            problems.append(("C4", lang, "объявлен в гейте, но блоков этого языка нет"))
    return problems


def self_test():
    """Ловушки взведены: каждый класс обязан быть пойман, согласное — принято."""
    book = os.path.join(ROOT, "book")
    all_linked = set(LOCAL.values())
    full = {lang: [f"{lang}.typ:1"] for lang in LOCAL} | {"c": ["c.typ:1"]}

    # C1 - язык блока не объявлен ни встроенным, ни своим.
    found = {k for k, _, _ in check(full | {"pascal": ["d.typ:4"]}, all_linked, book)}
    if found != {"C1"}:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: C1 не пойман, получено {found}")

    # C2 - определение объявлено, а файла нет (каталог без файлов подсветки).
    found = {k for k, _, _ in check(full, all_linked, os.path.join(book, "src"))}
    if found != {"C2"}:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: C2 не пойман, получено {found}")

    # C3 - файл есть, но в шаблоне не подключён: определение мёртвое, и это
    # самая тихая поломка (подсветки нет, ошибки нет).
    found = {k for k, _, _ in check(full, {"takt.sublime-syntax"}, book)}
    if found != {"C3"}:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: C3 не пойман, получено {found}")

    # C4 - язык объявлен в проверке, а блоков этого языка в документе нет.
    found = {k for k, _, _ in check({"takt": ["a.typ:1"]}, all_linked, book)}
    if found != {"C4"}:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: C4 не пойман, получено {found}")

    # Контроль: согласованный вход находок давать не должен - иначе "ловится"
    # означало бы "ругается на всё".
    clean = check(full, all_linked, book)
    if clean:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: согласованный вход дал находки: {clean}")

    print("  самопроверка гейта: ловушка взведена (4 класса, согласованный вход — нет)")


def main():
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    try:
        template_text = open(TEMPLATE, encoding="utf-8").read()
    except OSError as error:
        sys.exit(f"ОШИБКА: не прочитать шаблон документа: {error}")

    languages = block_languages(BOOK_SRC)
    note = require_input("языки блоков кода", len(languages), source="book/src")
    linked = linked_syntaxes(template_text)
    problems = check(languages, linked, os.path.join(ROOT, "book"))
    if problems:
        print("ОШИБКА: подсветка блоков кода документа (фича 0269):", file=sys.stderr)
        for kind, lang, why in problems:
            print(f"  [{kind}] ```{lang} — {why}", file=sys.stderr)
        print(
            "\nЯзык блока обязан быть объявлен в scripts/check-book-code-langs.py:\n"
            "  BUILTIN — определение есть у syntect (проверьте рендером страницы);\n"
            "  LOCAL   — определение лежит в book/*.sublime-syntax и подключено\n"
            "            в book/src/template.typ.\n"
            "Иначе блок печатается ЧЁРНЫМ, а `typst compile` молчит с кодом 0.",
            file=sys.stderr,
        )
        return 1

    print(
        f"Блоки кода документа: {note} "
        f"({len(LOCAL)} своих определений); подсветка объявлена у всех."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

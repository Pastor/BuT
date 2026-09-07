#!/usr/bin/env python3
"""check-readme-numbers.py — числа README СВЕРЯЮТСЯ с деревом (фича 0526).

README называет проект числами: сколько наборов тестов в каждой теме, сколько
разделов в документе. Числа эти пересчитываются по дереву за секунду, но никем
не пересчитывались — и протухали молча:

- замер 2026-09-03 (фича 0523): таблица наборов интеграционных тестов отстала по
  **всем** строкам, `conformance` - 111 наборов против записанных 27, `semantic`
  — 81 против 51;
- замер 2026-09-03 (эта фича): «17 разделов и приложения» при 21 главе и пяти
  приложениях.

Класс тот же, ради которого заведены гейты документа 0520…0522: утверждение
живёт отдельно от предмета и расходится с ним без единого сигнала. Разница в
том, что здесь предмет — само дерево, а не вывод инструмента.

Проверки:

- **R1 — таблица наборов интеграционных тестов.** Строка
  `| \\`крейт\\` | \\`тема\\` | … | N |` сверяется с числом `mod` в
  `<крейт>/tests/<тема>/main.rs` (правило «одна цель на тему, набор внутри —
  файл», фичи 0244 и 0271).
- **R2 — число разделов документа.** Фраза «(N разделов и приложения» сверяется
  с числом нумерованных глав в `book/src/main.typ`.

⚠️ Проверяются **не все** числа README, а те, у которых есть однозначный предмет
в дереве. Замеры («предкоммит целиком — 4 мин 50 с», «32 кода не значились в
сводной») датированы и принадлежат своей фиче: пересчитывать их нельзя, они
описывают прошлое.

⚠️ Правило, не нашедшее в README ни одной строки, — **ошибка**: разметка
изменилась, и проверка выродилась бы в тривиальный успех (урок фикса 0202-01).

Корень дерева переопределяется `RN_ROOT` — для сторожа
`scripts/test-readme-numbers.sh`, который гоняет гейт на копии.

Использование:

    python3 scripts/check-readme-numbers.py
"""

import os
import re
import sys

ROOT = os.environ.get(
    "RN_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)
README = os.path.join(ROOT, "README.md")
MAIN_TYP = os.path.join(ROOT, "book", "src", "main.typ")

SUITE_ROW = re.compile(
    r"^\|\s*`([\w-]+)`\s*\|\s*`([\w-]+)`\s*\|[^|]*\|\s*(\d+)\s*\|\s*$"
)
SECTIONS = re.compile(r"\((\d+) раздел\w* и приложения")
CHAPTER = re.compile(r'^#include "\d')


def count_mods(path):
    if not os.path.exists(path):
        return None
    with open(path, encoding="utf-8") as handle:
        return sum(1 for line in handle if line.startswith("mod "))


def check_suites(text, problems):
    """R1: число наборов темы = число `mod` в её `main.rs`."""
    seen = 0
    for number, line in enumerate(text.split("\n"), 1):
        row = SUITE_ROW.match(line)
        if not row:
            continue
        crate, theme, shown = row.group(1), row.group(2), int(row.group(3))
        main = os.path.join(ROOT, crate, "tests", theme, "main.rs")
        actual = count_mods(main)
        if actual is None:
            problems.append(
                f"строка {number}: темы `{crate}/{theme}` нет в дереве "
                f"({os.path.relpath(main, ROOT)})"
            )
            continue
        seen += 1
        if actual != shown:
            problems.append(
                f"строка {number}: `{crate}` / `{theme}` — в README {shown} "
                f"наборов, в дереве {actual}"
            )
    if not seen:
        problems.append(
            "таблица наборов интеграционных тестов не найдена: проверять нечего "
            "(сменилась разметка?)"
        )
    return seen


def paragraphs(text):
    """Абзацы с номером первой строки: утверждение переносится и живёт в цитате,
    поэтому строчный поиск его не видит (тот же урок, что у гейта 0297)."""
    out = []
    start = None
    buffer = []
    for number, line in enumerate(text.split("\n"), 1):
        stripped = line.lstrip("> ").rstrip()
        if not stripped:
            if buffer:
                out.append((start, " ".join(buffer)))
                buffer = []
            continue
        if not buffer:
            start = number
        buffer.append(stripped)
    if buffer:
        out.append((start, " ".join(buffer)))
    return out


def check_sections(text, problems):
    """R2: число разделов документа = число нумерованных глав в main.typ."""
    found = [
        (line, match)
        for line, body in paragraphs(text)
        for match in SECTIONS.finditer(body)
    ]
    if not found:
        problems.append(
            "утверждение о числе разделов документа не найдено: проверять нечего "
            "(сменилась разметка?)"
        )
        return 0
    if not os.path.exists(MAIN_TYP):
        problems.append(f"не найден состав документа: {MAIN_TYP}")
        return 0
    with open(MAIN_TYP, encoding="utf-8") as handle:
        actual = sum(1 for line in handle if CHAPTER.match(line))
    for line, match in found:
        shown = int(match.group(1))
        if shown != actual:
            problems.append(
                f"строка {line}: в README {shown} разделов документа, "
                f"в `book/src/main.typ` — {actual}"
            )
    return len(found)


def main():
    if not os.path.exists(README):
        print(f"ОШИБКА: не найден README: {README}", file=sys.stderr)
        return 1
    with open(README, encoding="utf-8") as handle:
        text = handle.read()

    print("Числа README против дерева (фича 0526)...")
    problems = []
    suites = check_suites(text, problems)
    sections = check_sections(text, problems)

    if problems:
        print(f"  ОШИБКА: число в README разошлось с деревом ({len(problems)}):",
              file=sys.stderr)
        for problem in problems:
            print(f"    {problem}", file=sys.stderr)
        return 1

    print(f"  Сверено: строк таблицы наборов {suites}, утверждений о разделах {sections}.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

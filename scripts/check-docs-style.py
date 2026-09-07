#!/usr/bin/env python3
"""Проверка стиля документов проекта.

Правило записано в `docs/RULE.md` (правило «Текст проекта пишется по-русски и без служебного шума»); здесь оно проверяется машиной.
Предмет - текст документа, а не код: содержимое блоков примеров и вставок в
обратных кавычках принадлежит примеру и не судится.

Проверки (падают списком, а не первой находкой):

* `D1` - пиктограммы. Значок не несёт смысла, которого нет в предложении.
  Знаки вердикта таблиц пропускаются: их читает машина.
* `D2` - жаргон и англицизмы там, где есть русский термин.
* `D3` - выделение прописными. Прописные - у аббревиатур, единиц и слов
  процесса, которые читает машина.
* `D4` - номер фичи, задачи, правила или архитектурного решения в тексте.
  Номер живёт в имени файла и в адресе ссылки, а в предложении устаревает.

Проверяются `*.md` и `*.typ` дерева документов. Долг ведётся файлом
`scripts/docs-style-baseline.txt`: строка "путь код" разрешает одну находку.

Самопроверка - `scripts/test-check-docs-style.sh`.
Переменная `DS_ROOT` переопределяет корень дерева.
"""

import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gatelib import require_input  # noqa: E402  (путь к помощнику известен только здесь)
from pathlib import Path

ROOTS = ["docs", "book/src", "CLAUDE.md", "README.md", "FEATURES.md", "CHANGES.md"]
SUFFIXES = {".md", ".typ"}
BASELINE = "scripts/docs-style-baseline.txt"

# Вставка, блок примера и формула: их содержимое принадлежит примеру.
SPAN = re.compile(r"```.*?```|`[^`\n]*`|\$[^$\n]*\$", re.S)
# Ссылка: её адрес несёт номер по праву, а текст судится наравне с прочим.
LINK_URL = re.compile(r"\]\([^)]*\)")

PICTO = re.compile(r"[⚠✓✗★⌘⚡•▪◆]")
JARGON = re.compile(
    r"\b(?:гейт\w*|сторож\w*|дефолт\w*|бэкенд\w*|рантайм\w*|оверлей\w*|"
    r"фикс|фикса|фиксу|фиксе|фиксом|фиксы|фиксов|фиксам|фиксами|фиксах)\b",
    re.I,
)
# Номер фичи, задачи и решения - четырёхзначный, номер правила свода - одно-
# или двузначный.
NUMBER = re.compile(
    r"\b(?:(?:фич\w*|задач\w*|подзадач\w*|инвариант\w*|ADR)\s+№?\s*\d{3,4}"
    r"(?:-\d{2}[a-zа-я]*)?"
    r"|правил(?:о|а|у|е|ом|ам|ах|ами)[ \t]+№?[ \t]*\d{1,2}[а-я]?(?!\d))",
    re.I,
)
CAPS = re.compile(r"\b[А-ЯЁ]{2,}\b")

# Прописными пишутся аббревиатуры, единицы и слова процесса, которые читают
# `check-feature-status.py` и `check-registry-verdicts.py`.
ABBR = {
    "АСД", "ПЛК", "МК", "СУС", "ЦП", "ФС", "ПИД", "ОЗУ", "ПЗУ", "ЭВМ", "ГОСТ",
    "БД", "ПЛИС", "СИ", "ЧПУ", "ФБ", "ОС", "СУ", "АСУ", "ТП", "ШИМ", "АЦП",
    "ЦАП", "КБ", "МБ", "ГБ", "ТБ", "МС", "НС",
    "ГОТОВО", "СОЗДАНА", "ОТМЕНА", "ОТМЕНЕНА", "ЗАМОРОЖЕНА", "ЗАБЛОКИРОВАНА",
    "АНАЛИЗ", "АРХИТЕКТУРА", "РАЗРАБОТКА", "ТЕСТИРОВАНИЕ", "ИСПРАВЛЕНИЕ",
    "ПРОЙДЕН", "ПРОЙДЕНО", "ИСПРАВЛЕН", "ОТВЕРГ", "ВЫПОЛНЕНО", "ГОДНО",
    "НЕГОДНО", "ОК", "ДА", "НЕТ", "ЧАСТИЧНО", "ОТКАЗ", "ЗАКРЫТА", "ЗАКРЫТ",
}


def check(path: Path, rel: str) -> list[str]:
    """Находки в одном файле."""
    try:
        text = path.read_text(encoding="utf-8")
    except (UnicodeDecodeError, OSError):
        return []
    plain = SPAN.sub(lambda m: "\n" * m.group(0).count("\n"), text)
    plain = LINK_URL.sub("](url)", plain)

    found = []
    for rx, code, what in (
        (PICTO, "D1", "пиктограмма"),
        (JARGON, "D2", "жаргон вместо термина"),
        (NUMBER, "D4", "номер в тексте"),
    ):
        for m in rx.finditer(plain):
            line = plain.count("\n", 0, m.start()) + 1
            found.append(f"{code} {rel}:{line}: {what} - {m.group(0)[:40]}")
    for m in CAPS.finditer(plain):
        if m.group(0) in ABBR:
            continue
        line = plain.count("\n", 0, m.start()) + 1
        found.append(f"D3 {rel}:{line}: выделение прописными - {m.group(0)}")
    return found


def load_baseline(root: Path) -> set[str]:
    path = root / BASELINE
    if not path.is_file():
        return set()
    allowed = set()
    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.split("#", 1)[0].strip()
        if line:
            allowed.add(line)
    return allowed


def main() -> int:
    root = Path(os.environ.get("DS_ROOT", Path(__file__).resolve().parent.parent))
    allowed = load_baseline(root)
    errors: list[str] = []
    files = 0
    for name in ROOTS:
        base = root / name
        if base.is_file():
            targets = [base]
        elif base.is_dir():
            targets = sorted(p for p in base.rglob("*") if p.suffix in SUFFIXES)
        else:
            continue
        for path in targets:
            if not path.is_file() or path.suffix not in SUFFIXES:
                continue
            files += 1
            rel = str(path.relative_to(root))
            for line in check(path, rel):
                code = line.split(" ", 1)[0]
                if f"{rel} {code}" in allowed:
                    continue
                errors.append(line)

    if not files:
        print("ОШИБКА: не найдено ни одного документа", file=sys.stderr)
        return 1
    if errors:
        print("Стиль документов: нарушения", file=sys.stderr)
        for line in errors[:40]:
            print(f"  {line}", file=sys.stderr)
        if len(errors) > 40:
            print(f"  ... и ещё {len(errors) - 40}", file=sys.stderr)
        print(
            "\nТекст документа пишется терминами, без пиктограмм, выделения "
            "прописными и номеров (docs/RULE.md, правило «Текст проекта пишется по-русски и без служебного шума»).",
            file=sys.stderr,
        )
        return 1

    note = require_input("просмотренные файлы документов", files, source="docs, book")
    print(f"  стиль документов: {note}, нарушений нет")
    return 0


if __name__ == "__main__":
    sys.exit(main())

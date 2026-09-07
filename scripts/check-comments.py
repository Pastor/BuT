#!/usr/bin/env python3
"""Проверка комментариев исходного кода.

Комментарий описывает назначение кода. Правило записано в `docs/CODE.md`,
раздел "Комментарии"; здесь оно проверяется машиной.

Проверки (падают списком, а не первой находкой):

* `C1` - пиктограммы и типографские знаки: только ASCII.
* `C2` - номера фич, задач, правил, инвариантов и архитектурных решений.
  Ссылка живёт вне кода и протухает молча.
* `C3` - упоминание того, по чьей просьбе сделано.
* `C4` - разделы истории правки: "Что было", "Зачем", "Замер", "Цена".
* `C5` - жаргон и англицизмы там, где есть русский термин: "гейт", "сторож",
  "фикс", "дефолт", "бэкенд", "рантайм".
* `C6` - выделение прописными. Прописные - только у аббревиатур; список
  кириллических ведётся здесь же.

Проверяется код: `*.rs`, `*.py`, `*.sh`, `*.js`, `*.mjs`. Каталоги сборки и
данные тестов пропускаются: в фикстурах текст принадлежит их автору. Блоки
примеров (```) внутри комментария тоже пропускаются: там знаки принадлежат
примеру, а не описанию.

Самопроверка - `scripts/test-check-comments.sh`.
Переменная `CC_ROOT` переопределяет корень дерева.
"""

import os
import re
import sys
from pathlib import Path

ROOTS = [
    "takt-lang/src",
    "takt-lang/tests",
    "takt-sim/src",
    "takt-sim/tests",
    "takt-wasm/src",
    "web/server/src",
    "web/server/tests",
    "web/static",
    "scripts",
]
SUFFIXES = {".rs", ".py", ".sh", ".js", ".mjs"}

# Комментарий строки: маркер и текст после него.
COMMENT = re.compile(r"^\s*(?://+!?|#)\s?(.*)$")
SHEBANG = re.compile(r"^#!")

PICTO = re.compile(r"[⚠✓✗★⌘⚡•▪◆—–→←≡≤≥«»…×≠]")
NUMBER = re.compile(
    r"\b(?:фич\w*|задач\w*|подзадач\w*|правил\w*|инвариант\w*|ADR|фикс)\s+№?\s*\d{2,4}",
    re.I,
)
CUSTOMER = re.compile(r"\bзаказчик\w*", re.I)
# Формы жаргонного слова перечислены поимённо: "фиксированный", "фиксация" и
# "фикстура" - обычные слова, и под правило не подпадают.
JARGON = re.compile(
    r"\b(?:гейт\w*|сторож\w*|дефолт\w*|бэкенд\w*|рантайм\w*|оверлей\w*|"
    r"фикс|фикса|фиксу|фиксе|фиксом|фиксы|фиксов|фиксам|фиксами|фиксах)\b",
    re.I,
)
# Кириллические аббревиатуры: прописные у них законны.
ABBR = {"АСД", "ПЛК", "МК", "СУС", "ЦП", "ФС", "ПИД", "ОЗУ", "ПЗУ", "ЭВМ", "ГОСТ"}
CAPS = re.compile(r"\b[А-ЯЁ]{2,}\b")
CODE = re.compile(r"`[^`]*`")
HISTORY = re.compile(
    r"^#+\s*(что было|зачем|почему|цена|история|замер|что нашёл|что нашла|"
    r"как нашли|предыстория)\b",
    re.I,
)


def check(path: Path) -> list[str]:
    """Находки в одном файле."""
    found = []
    try:
        text = path.read_text(encoding="utf-8")
    except (UnicodeDecodeError, OSError):
        return found
    in_example = False
    for no, raw in enumerate(text.splitlines(), 1):
        if SHEBANG.match(raw.lstrip()):
            continue
        m = COMMENT.match(raw)
        if not m:
            continue
        body = m.group(1)
        if body.strip().startswith("```"):
            in_example = not in_example
            continue
        if in_example:
            continue
        where = f"{path}:{no}"
        if PICTO.search(body):
            found.append(f"C1 {where}: пиктограмма или типографский знак")
        if NUMBER.search(body):
            found.append(f"C2 {where}: ссылка на номер")
        if CUSTOMER.search(body):
            found.append(f"C3 {where}: упоминание просьбы")
        if HISTORY.match(body.strip()):
            found.append(f"C4 {where}: раздел истории правки")
        plain = CODE.sub("", body)
        if JARGON.search(plain):
            found.append(f"C5 {where}: жаргон вместо термина")
        caps = [w for w in CAPS.findall(plain) if w not in ABBR]
        if caps:
            found.append(f"C6 {where}: выделение прописными - {caps[0]}")
    return found


def main() -> int:
    root = Path(os.environ.get("CC_ROOT", Path(__file__).resolve().parent.parent))
    errors: list[str] = []
    files = 0
    for name in ROOTS:
        base = root / name
        if not base.is_dir():
            continue
        for path in sorted(base.rglob("*")):
            if not path.is_file() or path.suffix not in SUFFIXES:
                continue
            if "/target/" in str(path) or "/data/" in str(path):
                continue
            files += 1
            errors.extend(check(path))

    if not files:
        print("ОШИБКА: не найдено ни одного файла с кодом", file=sys.stderr)
        return 1
    if errors:
        print("Проверка комментариев: нарушения", file=sys.stderr)
        for line in errors[:40]:
            print(f"  {line}", file=sys.stderr)
        if len(errors) > 40:
            print(f"  ... и ещё {len(errors) - 40}", file=sys.stderr)
        print(
            "\nКомментарий описывает назначение кода (docs/CODE.md, "
            '"Комментарии"): без номеров, истории правки и пиктограмм.',
            file=sys.stderr,
        )
        return 1

    print(f"  комментарии: проверено {files} файлов, нарушений нет")
    return 0


if __name__ == "__main__":
    sys.exit(main())

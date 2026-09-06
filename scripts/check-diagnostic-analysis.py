#!/usr/bin/env python3
"""Гейт: код, чьё описание ссылается на устройство или называет чужой
инструмент, обязан иметь подробный разбор в приложении (фича 0398).

Признак отбора выбран заказчиком 2026-08-23 (Option C ADR 0398) и он
**проверяемый**: сообщение такого кода без примера понять нельзя, даже зная
язык. Прочие коды объясняет их собственный текст, и разбора им не нужно —
именно поэтому вторая часть приложения прежде называлась «основных ошибок»,
не определяя, что такое «основные».

Условия отказа:
  A1 — код подпадает под признак, но разбора не имеет;
  A2 — реестр диагностик или приложение не читаются (вырожденный вход).

Корень дерева берётся из переменной DA_ROOT (для самопроверки на копии).
"""

import os
import re
import sys

ROOT = os.environ.get("DA_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
REGISTRY = os.path.join(ROOT, "docs", "diagnostics", "README.md")
APPENDIX = os.path.join(ROOT, "book", "src", "appendix-errors", "index.typ")

# Слова, по которым сообщение опознаётся как "ссылающееся на устройство",
# и имена чужих инструментов. Список короткий намеренно: широкий признак
# потребовал бы разбора почти для всего и был бы снят при первом неудобстве.
MARKERS = (
    "представлен",
    "понижен",
    "раскладк",
    "упакован",
    "iec2c",
    "verilator",
    "clippy",
    "yosys",
    "rustc",
)


def read(path: str) -> str:
    try:
        with open(path, encoding="utf-8") as f:
            return f.read()
    except OSError as e:
        print(f"ОТКАЗ (A2): не читается {path}: {e}")
        sys.exit(1)


def main() -> int:
    registry = read(REGISTRY)
    appendix = read(APPENDIX)

    # Коды с разбором - включая составные заголовки (`SV-018` / `ST-020`).
    analysed = set()
    for line in appendix.splitlines():
        if line.startswith("=== "):
            analysed.update(re.findall(r"`([A-Z]{2}-\d{3})`", line))
    if not analysed:
        print("ОТКАЗ (A2): в приложении нет ни одного разбора — проверять нечего")
        return 1

    rows = re.findall(r"^\| `([A-Z]{2}-\d{3})` \| (.+?) \|", registry, re.M)
    if not rows:
        print("ОТКАЗ (A2): реестр диагностик не разобран — таблиц не найдено")
        return 1

    missing = []
    matched = 0
    for code, desc in rows:
        if "RETIRED" in desc or "RESERVED" in desc:
            continue
        if not any(m in desc.lower() for m in MARKERS):
            continue
        matched += 1
        if code not in analysed:
            missing.append(code)

    if missing:
        print("ОТКАЗ (A1): коды подпадают под признак, но разбора не имеют:")
        for code in missing:
            print(f"   {code}")
        print()
        print("Признак (решение заказчика, фича 0398): описание ссылается на")
        print("устройство инструмента либо называет чужой инструмент — такое")
        print("сообщение без примера понять нельзя. Добавьте разбор в")
        print("book/src/appendix-errors/ либо перепишите описание по существу.")
        return 1

    print(
        f"Разбор диагностик: {len(analysed)} кодов разобрано, "
        f"под признак подпадает {matched} — все разобраны."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

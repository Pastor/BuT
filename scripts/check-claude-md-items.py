#!/usr/bin/env python3
"""Размер пункта живого контекста (фича 0408).

`CLAUDE.md` — контекст, который читается ПЕРЕД работой, и место в нём стоит
дорого. Правило 9 свода и формат фичи 0180 задают пункту строгую форму:
**инвариант -> чем платим за нарушение -> контроль -> ссылка на разбор**. История
находки, отвергнутые варианты и замеры живут в артефактах фичи (правило 32),
а не здесь.

Форма проверяема по длине: пункт, уложенный в предел, физически не вмещает
пересказ карточки. Замер 2026-08-23 (фича 0408): 221 пункт, 3263 строки (89 %
файла), медиана 14. После диеты 2026-08-31 (фича 0436): 239 пунктов, 1237 строк,
медиана 5 — предел опущен с 15 до 8.

Классы нарушений:

  C1 — пункт длиннее предела и не значится в реестре узаконенного долга;
  C2 — запись реестра протухла: пункт уложился в предел (или исчез), а строка
       осталась — храповик обязан крутиться только в одну сторону;
  C3 — запись реестра занижает длину: пункт вырос сверх записанного.

Переменная окружения `CM_ROOT` переопределяет корень репозитория — её
использует сторож `scripts/test-claude-md-items.sh`, гоняющий гейт на копии
дерева.
"""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path

# Предел выбран замером: он оставляет место инварианту,
# цене, тесту и одной-двум ловушкам ``, но не вмещает пересказ карточки.
# Восемь - та величина, при которой долг сведён к нулю после диеты (2666 ->
# 1237 строк в пунктах, медиана 5): реестр ниже пуст, и это его нормальное
# состояние, а не поблажка.
LIMIT = 8

ROOT = Path(os.environ.get("CM_ROOT", Path(__file__).resolve().parent.parent))
CONTEXT = ROOT / "CLAUDE.md"
BASELINE = ROOT / "scripts" / "claude-md-item-baseline.txt"


def items(text: str) -> dict[str, int]:
    """Пункты контекста: имя → длина в строках.

    Пункт — блок, начинающийся с `- **Имя**` на нулевом отступе; он кончается
    следующим таким блоком, заголовком раздела или пунктом другого вида.
    """
    found: dict[str, int] = {}
    name: str | None = None
    length = 0
    lines = text.split("\n")
    for index, line in enumerate(lines):
        if line.startswith("- **"):
            if name:
                found[name] = length
            name = title(lines, index)
            length = 1
            continue
        # Пункт кончается заголовком, пунктом другого вида или врезкой-цитатой:
        # врезка - самостоятельный элемент, и приписывать её соседу нельзя.
        ends_item = (
            line.startswith("#")
            or line.startswith("> ")
            or (line.startswith("- ") and not line.startswith("- **"))
        )
        if ends_item:
            if name:
                found[name] = length
                name = None
            continue
        if name:
            length += 1
    if name:
        found[name] = length
    return found


def title(lines: list[str], index: int) -> str:
    """Имя пункта — жирный заголовок, который может занимать ДВЕ строки.

    ⚠️ Наивная построчная регулярка такой заголовок не узнаёт, и пункт
    приписывается предыдущему — длина обоих становится неверной, а гейт
    сообщает о пунктах, которых нет. Поймано на живом файле (фича 0408).
    """
    joined = " ".join(lines[index : index + 3]).strip()
    match = re.match(r"- \*\*(.+?)\*\*", joined)
    if match:
        return " ".join(match.group(1).split())
    return " ".join(lines[index][4:].split())


def debt() -> dict[str, int]:
    """Узаконенный долг: имя пункта → разрешённая длина."""
    if not BASELINE.exists():
        return {}
    allowed: dict[str, int] = {}
    for line in BASELINE.read_text(encoding="utf-8").split("\n"):
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        size, _, name = line.partition(" ")
        allowed[name.strip()] = int(size)
    return allowed


def main() -> int:
    found = items(CONTEXT.read_text(encoding="utf-8"))
    allowed = debt()
    over = {n: k for n, k in found.items() if k > LIMIT}

    c1 = sorted((k, n) for n, k in over.items() if n not in allowed)
    c2 = sorted(n for n in allowed if found.get(n, 0) <= LIMIT)
    c3 = sorted((found[n], allowed[n], n) for n in allowed if n in over and over[n] > allowed[n])

    if c1:
        print(f"ОТКАЗ: пунктов длиннее {LIMIT} строк вне реестра долга: {len(c1)}")
        for size, name in c1:
            print(f"  {size:3} строк  {name}")
        print()
        print("Формат пункта (правило 9, фича 0180): инвариант → чем платим →")
        print("сторож → ссылка. Замеры, историю находки и отвергнутые варианты")
        print("вынесите в карточку фичи (правило 32) — здесь им не место.")
    if c3:
        print(f"ОТКАЗ: пункты выросли сверх записанного в реестре: {len(c3)}")
        for size, was, name in c3:
            print(f"  {name}: было {was}, стало {size}")
    if c2:
        print(f"ОТКАЗ: записи реестра протухли (пункт уложился в предел): {len(c2)}")
        for name in c2:
            print(f"  {name}")
        print()
        print("Удалите строку: реестр замораживает долг, а не разрешает его.")
    if c1 or c2 or c3:
        return 1

    total = sum(found.values())
    print(
        f"Размер пунктов CLAUDE.md: {len(found)} пунктов, {total} строк; "
        f"сверх предела {LIMIT} — {len(over)}."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

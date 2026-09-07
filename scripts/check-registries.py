#!/usr/bin/env python3
"""check-registries.py — реестры стадий против файлов на диске (фича 0164).

Каждая папка пофичевых артефактов — `docs/features/` (карточки: все стадии в
одном файле, правило 32) и `docs/fixes/` (исправления) — ведёт реестр
`README.md`. Реестр — **вход в проект**: по нему
смотрят, что сделано и где это лежит. Наполняется он руками, и до этой фичи
рассинхрон с диском не ловился ничем.

Замер 2026-08-20: **42** файла лежали на диске, не значась в реестре своей
папки — ADR 5, анализ 6, разработка 12, тест-планы 6, отчёты 12, фиксы 1.

⚠️ **Пропуск в реестре делает соседний гейт слепым.** `check-registry-verdicts.py`
(правило 21) требует настоящий вердикт у каждой строки стадий 5–6 — но строки,
которой нет, он не видит: шесть тест-планов и двенадцать отчётов проходили мимо
проверки вердиктов вовсе.

Два класса находок:

- **R1** — файл есть, записи в реестре нет (реестр неполон);
- **R2** — запись есть, файла нет (реестр врёт). Битую markdown-ссылку ловит и
  `check-links.py`, но здесь она названа своим именем и в своём месте.

⚠️ Гейт сверяет **состав**, а не содержание строки: верность заголовка, статуса
и вердикта — на человеке (правило 4) и на `check-registry-verdicts.py`.

Использование:

    python3 scripts/check-registries.py
    python3 scripts/check-registries.py --self-test
"""

import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gatelib import require_input  # noqa: E402  (путь к помощнику известен только здесь)

ROOT = os.environ.get(
    "REG_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)

# Папки-стадии жизненного цикла. `docs/templates/` не в списке:
# там плейсхолдеры, а не артефакты фич; `docs/diagnostics/` - реестр кодов, а не
# стадия.
# Папок две: стадии живут разделами карточки, и
# каталогов `adr/analyze/development/tests/reports` больше нет.
STAGES = ("features", "fixes")

# Ссылка на файл в строке реестра: `[0019](./0019-slug.md)` либо
# `[0019-slug.md](0019-slug.md)`.
LINK = re.compile(r"\]\((?:\./)?([0-9A-Za-z_.-]+\.md)\)")


def registry_links(text):
    """Файлы, на которые ссылается реестр."""
    return set(LINK.findall(text))


def stage_files(path):
    """Артефакты папки-стадии (сам реестр не в счёт)."""
    return sorted(
        name
        for name in os.listdir(path)
        if name.endswith(".md") and name != "README.md"
    )


def check(stages):
    """Находки `(класс, стадия, файл)`; пустой список — гейт пройден.

    `stages` — словарь `стадия → (список файлов, текст реестра)`.
    """
    problems = []
    for stage, (files, text) in sorted(stages.items()):
        linked = registry_links(text)
        if not files and not linked:
            problems.append(("R0", stage, "ни файлов, ни записей — проверка вырождена"))
            continue
        for name in files:
            if name not in linked:
                problems.append(("R1", stage, name))
        for name in sorted(linked):
            if name not in files:
                problems.append(("R2", stage, name))
    return problems


def self_test():
    """Ловушки взведены: оба класса ловятся, согласованный вход — нет."""
    ok = {"adr": (["0001-a.md", "0002-b.md"], "[0001](./0001-a.md) [0002](./0002-b.md)")}
    if check(ok):
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: согласованный вход дал находки: {check(ok)}")

    missing = {"adr": (["0001-a.md", "0002-b.md"], "[0001](./0001-a.md)")}
    found = {(k, n) for k, _, n in check(missing)}
    if found != {("R1", "0002-b.md")}:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: R1 не пойман: {found}")

    dangling = {"adr": (["0001-a.md"], "[0001](./0001-a.md) [0009](./0009-x.md)")}
    found = {(k, n) for k, _, n in check(dangling)}
    if found != {("R2", "0009-x.md")}:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: R2 не пойман: {found}")

    empty = {"adr": ([], "")}
    if {k for k, _, _ in check(empty)} != {"R0"}:
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: пустая стадия принята за успех")

    print("  самопроверка гейта: ловушка взведена (R0/R1/R2, согласованный вход — нет)")


def main():
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    stages = {}
    for stage in STAGES:
        path = os.path.join(ROOT, "docs", stage)
        if not os.path.isdir(path):
            continue
        registry = os.path.join(path, "README.md")
        if not os.path.isfile(registry):
            print(
                f"ОШИБКА: у папки-стадии docs/{stage}/ нет реестра README.md "
                "(правило 17).",
                file=sys.stderr,
            )
            return 1
        with open(registry, encoding="utf-8") as handle:
            stages[stage] = (stage_files(path), handle.read())

    note = require_input("папки-стадии", len(stages), source="docs/")

    problems = check(stages)
    if problems:
        print("ОШИБКА: реестры стадий разошлись с файлами (фича 0164):", file=sys.stderr)
        for kind, stage, name in problems:
            what = {
                "R0": "стадия пуста",
                "R1": "файл есть, записи в реестре нет",
                "R2": "запись есть, файла нет",
            }[kind]
            print(f"  [{kind}] docs/{stage}/{name} — {what}", file=sys.stderr)
        print(
            "\nРеестр стадии — вход в проект (правило 17): по нему смотрят, что\n"
            "сделано и где лежит. Допишите запись (генератор: scripts/new-feature.sh\n"
            "--stage NAME --register) либо уберите ссылку на исчезнувший файл.\n"
            "⚠️ Пропуск в реестре делает слепым check-registry-verdicts.py: строки,\n"
            "которой нет, он не проверяет.",
            file=sys.stderr,
        )
        return 1

    total = sum(len(files) for files, _ in stages.values())
    print(
        f"Реестры артефактов: {note}, артефактов {total} — "
        "расхождений с диском нет."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

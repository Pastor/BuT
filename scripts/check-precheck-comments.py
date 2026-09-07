#!/usr/bin/env python3
"""check-precheck-comments.py — комментарий шага не пересказывает гейт
(фича 0316).

У шага `precheck.sh` и у скрипта гейта разные роли:

- **заголовок скрипта** объясняет ПРАВИЛО: что проверяется, почему это важно,
  каким замером повод измерен;
- **комментарий шага** объясняет ШАГ: зачем он здесь и почему стоит именно в
  этом месте (например «после сборки — гейту нужны бинарники»).

Когда комментарий шага пересказывает заголовок, знание раздваивается — и копии
расходятся молча. Замер фичи 0253: сняв исключение в `check-legacy-names.sh`,
пришлось править **два** комментария `precheck.sh`, повторявших это исключение
своими словами. Замер 2026-08-20 по всем шагам: 34 шага с комментарием, у
**четырёх** - дословное совпадение с заголовком проверки длиной 64...85 символов.

Проверка синтаксическая: ищется общий дословный кусок длиной от `MIN_OVERLAP`
символов. Короткие совпадения (имя фичи, термин, «фича 0274») законны и не
считаются — иначе гейт запретил бы называть предмет.

Использование:

    python3 scripts/check-precheck-comments.py
    python3 scripts/check-precheck-comments.py --self-test
"""

import difflib
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gatelib import require_input  # noqa: E402  (путь к помощнику известен только здесь)

ROOT = os.environ.get(
    "PC_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)
PRECHECK = os.path.join(ROOT, "scripts", "precheck.sh")
SCRIPTS = os.path.join(ROOT, "scripts")
CALL_RE = re.compile(r"/(check|test)-([a-z0-9-]+)\.(sh|py)")
MIN_OVERLAP = 60
HEAD_LINES = 60


def steps(text):
    """Пары `(имя скрипта, комментарий шага)` из `precheck.sh`."""
    found = []
    comment = []
    for line in text.splitlines():
        stripped = line.strip()
        if stripped.startswith("#"):
            comment.append(stripped.lstrip("#").strip())
            continue
        match = CALL_RE.search(line)
        if match and comment:
            found.append(
                (f"{match.group(1)}-{match.group(2)}.{match.group(3)}", " ".join(comment))
            )
        # `echo` - часть шага, комментарий к нему относится тоже.
        if not stripped.startswith("echo"):
            comment = []
    return found


def header_of(path):
    """Заголовок скрипта: первые строки комментария/докстроки."""
    head = []
    with open(path, encoding="utf-8") as handle:
        for line in list(handle)[:HEAD_LINES]:
            stripped = line.strip()
            if stripped.startswith("#") or stripped.startswith('"""'):
                head.append(stripped.lstrip("#").lstrip('"').strip())
    return " ".join(head)


def overlap(comment, header):
    """Длина самого длинного дословного общего куска."""
    if not comment or not header:
        return 0
    matcher = difflib.SequenceMatcher(None, comment, header)
    return max((block.size for block in matcher.get_matching_blocks()), default=0)


def check(pairs):
    """Находки `(скрипт, длина)`; пустой список — гейт пройден."""
    problems = []
    for script, comment, header in pairs:
        size = overlap(comment, header)
        if size >= MIN_OVERLAP:
            problems.append((script, size))
    return problems


def self_test():
    same = "правило проверяется так-то и потому-то, а повод измерен замером"
    if not check([("x.sh", same, same)]):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: дословный пересказ не пойман")
    if check([("x.sh", "стоит здесь: нужны собранные бинарники", same)]):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: объяснение шага объявлено пересказом")
    if check([("x.sh", "фича 0274, гейт снимков", "фича 0274: правило таково")]):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: короткое совпадение объявлено пересказом")
    if check([("x.sh", "", same)]):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: пустой комментарий дал находку")
    print("  самопроверка гейта: ловушка взведена (пересказ ловится, объяснение шага — нет)")


def main():
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    if not os.path.isfile(PRECHECK):
        sys.exit(f"ОШИБКА: не найден {PRECHECK}.")
    with open(PRECHECK, encoding="utf-8") as handle:
        found = steps(handle.read())
    if not found:
        sys.exit("ОШИБКА: шагов с комментарием не найдено — проверка вырождена.")

    pairs = []
    for script, comment in found:
        path = os.path.join(SCRIPTS, script)
        if os.path.isfile(path):
            pairs.append((script, comment, header_of(path)))
    problems = check(pairs)
    if problems:
        print("ОШИБКА: комментарий шага пересказывает заголовок гейта (фича 0316):", file=sys.stderr)
        for script, size in problems:
            print(
                f"  {script}: общий дословный кусок {size} символов — замените "
                f"пересказ ссылкой на заголовок скрипта",
                file=sys.stderr,
            )
        print(
            "\nКомментарий шага объясняет ШАГ (зачем он здесь и почему в этом\n"
            "месте), правило живёт в заголовке гейта. Два носителя одного знания\n"
            "расходятся молча — класс 0084/0193/0195.",
            file=sys.stderr,
        )
        return 1

    note = require_input("шаги предкоммита", len(pairs), source="scripts/precheck.sh")
    print(f"Комментарии шагов precheck: {note}, пересказов нет.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

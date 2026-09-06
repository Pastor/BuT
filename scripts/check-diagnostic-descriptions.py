#!/usr/bin/env python3
"""check-diagnostic-descriptions.py — запись реестра диагностик НАЗЫВАЕТ СМЫСЛ
(фича 0311).

Реестр `docs/diagnostics/README.md` — единственное место, где у кода есть
человеческое объяснение: приложение «Ошибки» сверяется с ним (гейт 0290), а
разбор с примером есть не у каждого кода. Запись вида

    | `DF-001` | ошибка | `takt-lang/src/address_map/env.rs` |

не объясняет ничего: слово «ошибка» повторяет то, что видно по колонке кода.
Замер 2026-08-20 нашёл **десять** таких записей — `DF-001`…`DF-004`, `CS-001`,
`FM-001`, `ST-004`, `SV-005`, `SV-006`, `AM-001`.

Класс знакомый: диагностика, не помогающая исправить (0212 — отказ без кода,
0231 — `Debug` вместо текста, 0307 — чужой диапазон в тексте). Здесь предмет —
не сообщение инструмента, а **справочник**.

Два условия:

- **D1 — служебное слово вместо описания.** Запись, состоящая из слов
  «ошибка», «предупреждение», «отказ» (в любом падеже и регистре), отвергается
  всегда: она не несёт смысла по построению.
- **D2 — слишком короткое описание.** Запись короче `MIN_LEN` символов
  отвергается, если её нет в реестре `scripts/diagnostic-description-baseline.txt`.
  Реестр — **узаконенный долг** с ратчетом: запись из него нельзя добавлять,
  можно только удалять вместе с расширением описания.

⚠️ Короткое — не значит плохое: «Модель не найдена» объясняет исчерпывающе.
Поэтому D2 — ратчет, а не запрет; растёт список только через явное решение.

Использование:

    python3 scripts/check-diagnostic-descriptions.py
    python3 scripts/check-diagnostic-descriptions.py --self-test
"""

import os
import re
import sys

ROOT = os.environ.get(
    "DD_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)
REGISTRY = os.path.join(ROOT, "docs", "diagnostics", "README.md")
BASELINE = os.path.join(ROOT, "scripts", "diagnostic-description-baseline.txt")

ROW = re.compile(r"^\| `([A-Z]{2}-\d{3})` \| (.*?) \| ")
SERVICE = re.compile(r"^(ошибк\w*|предупрежд\w*|отказ\w*)$", re.I)
MIN_LEN = 25


def rows(text):
    """Пары `(код, описание)` из таблиц реестра."""
    out = []
    for line in text.splitlines():
        match = ROW.match(line)
        if match:
            out.append((match.group(1), match.group(2).strip()))
    return out


def check(entries, baseline):
    """Находки `(класс, код, описание)`; пустой список — гейт пройден."""
    problems = []
    seen = set()
    for code, desc in entries:
        seen.add(code)
        if SERVICE.match(desc):
            problems.append(("D1", code, desc))
        elif len(desc) < MIN_LEN and code not in baseline:
            problems.append(("D2", code, desc))
    # Ратчет: запись реестра, которой в таблице больше нет либо чьё описание
    # выросло, замораживает пустоту - храповик проворачивается назад.
    for code in sorted(baseline - seen):
        problems.append(("D3", code, "кода нет в реестре диагностик"))
    for code, desc in entries:
        if code in baseline and len(desc) >= MIN_LEN and not SERVICE.match(desc):
            problems.append(("D3", code, "описание расширено — удалите запись долга"))
    return problems


def read_baseline(path):
    if not os.path.isfile(path):
        sys.exit(f"ОШИБКА: не найден реестр долга {path} (фича 0311).")
    entries = set()
    with open(path, encoding="utf-8") as handle:
        for line in handle:
            line = line.strip()
            if line and not line.startswith("#"):
                entries.add(line)
    return entries


def self_test():
    if check([("DF-001", "ошибка")], set()) != [("D1", "DF-001", "ошибка")]:
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: служебное слово не поймано")
    if check([("XX-001", "коротко")], set()) != [("D2", "XX-001", "коротко")]:
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: короткое описание не поймано")
    if check([("XX-001", "коротко")], {"XX-001"}):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: узаконенный долг не пропущен")
    if not any(
        kind == "D3" for kind, _, _ in check([("XX-001", "а" * 40)], {"XX-001"})
    ):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: протухшая запись долга не поймана")
    if not any(kind == "D3" for kind, _, _ in check([], {"XX-002"})):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: запись о несуществующем коде не поймана")
    if check([("XX-001", "внятное описание длиной сверх порога")], set()):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: годная запись объявлена находкой")
    print("  самопроверка гейта: ловушка взведена (D1/D2/D3 и годный вход)")


def main():
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    if not os.path.isfile(REGISTRY):
        sys.exit(f"ОШИБКА: не найден реестр {REGISTRY}.")
    with open(REGISTRY, encoding="utf-8") as handle:
        entries = rows(handle.read())
    if not entries:
        sys.exit("ОШИБКА: в реестре не найдено ни одной записи — проверка вырождена.")
    baseline = read_baseline(BASELINE)
    problems = check(entries, baseline)
    if problems:
        print("ОШИБКА: описания диагностик (фича 0311):", file=sys.stderr)
        for kind, code, desc in problems:
            what = {
                "D1": "описание — служебное слово: назовите, ЧТО не так у автора",
                "D2": "описание короче порога и не значится в узаконенном долге",
                "D3": "запись долга протухла",
            }[kind]
            print(f"  [{kind}] {code}: {desc!r} — {what}", file=sys.stderr)
        return 1

    print(
        f"Описания диагностик: проверено {len(entries)} записей, "
        f"узаконенного долга {len(baseline)} — безликих нет."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

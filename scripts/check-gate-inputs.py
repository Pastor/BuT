#!/usr/bin/env python3
"""check-gate-inputs.py — у проверки есть нижняя граница входа (фича 0536).

Проверка судит выборку, извлечённую разбором, и пустая выборка выглядит у неё
благополучием: разобрано ноль записей — расхождений ноль — код возврата ноль.
Разбор объяснён в `scripts/gatelib.py`; здесь — обязанность его звать.

Обязанность требуется от КАЖДОЙ проверки `scripts/check-*`, а не от тех, кого
машина сочла читающими документ. Замер 2026-09-07: признак «читает документ»
даёт верхнюю оценку и путает чтение документа с упоминанием пути в тексте
сообщения — в разбор попала проверка комментариев, чей вход суть исходники.
Признак, который сам может ошибиться, не годится в основание обязанности.

Проверка, которой граница не нужна, называется в реестре
`scripts/gate-input-baseline.txt` с причиной; проверка, которой она нужна, но
ещё не заведена, стоит там же долгом. Долг убывает: его потолок записан
константой ниже, и дописать запись, не тронув потолок, нельзя.

Находки:

* `G1` — проверка не зовёт помощника и не названа в реестре.
* `G2` — запись реестра устарела: проверка уже зовёт помощника.
* `G3` — запись реестра указывает на файл, которого нет.
* `G4` — долг вырос сверх потолка.

Проверка судит ФОРМУ (вызов есть), а не истину (граница разумна): границу
«не меньше единицы» она пропустит. Истину держит контрольная проба самой
проверки — правило контрольных проб (фича 0315).

Использование:

    python3 scripts/check-gate-inputs.py              # проверка
    python3 scripts/check-gate-inputs.py --self-test  # проверка самих ловушек

Корень дерева переопределяется переменной `GI_ROOT` — для контрольной пробы
`scripts/test-check-gate-inputs.sh`, которая гоняет проверку на копии.
"""

import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gatelib import require_input  # noqa: E402  (путь к помощнику известен только здесь)

ROOT = os.environ.get(
    "GI_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)
SCRIPTS = os.path.join(ROOT, "scripts")
BASELINE = os.path.join(SCRIPTS, "gate-input-baseline.txt")

# Потолок долга. Уменьшается вместе с переводом проверок и не растёт: новая
# проверка обязана заводить границу сразу, а не пополнять реестр.
DEBT_CEILING = 0

# Вызов помощника в теле проверки. Имя одно на три языка с точностью до регистра
# (`requireInput` в модуле на JavaScript); собственное объявление функции в
# `gatelib.sh` под признак не подпадает.
CALL = re.compile(r"require_input[ (]|requireInput\(")
DECLARATION = re.compile(r"^require_input\(\)", re.M)

KINDS = ("долг", "исключение")


def scripts_of(root):
    """Проверки дерева: `scripts/check-*` на любом языке, в порядке имени."""
    names = []
    for name in sorted(os.listdir(os.path.join(root, "scripts"))):
        if name.startswith("check-") and name.rsplit(".", 1)[-1] in ("py", "sh", "mjs"):
            names.append(name)
    return names


def calls_helper(path):
    """Зовёт ли проверка помощника. Объявление функции вызовом не считается."""
    with open(path, encoding="utf-8") as handle:
        text = handle.read()
    if DECLARATION.search(text):
        return False
    return bool(CALL.search(text))


def read_baseline(path):
    """Реестр: имя проверки → (вид, причина). Строки-комментарии пропускаются."""
    entries = {}
    if not os.path.exists(path):
        return entries
    with open(path, encoding="utf-8") as handle:
        for line in handle:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            parts = line.split(None, 1)
            if len(parts) != 2 or parts[0] not in KINDS:
                sys.exit(
                    f"ОШИБКА: строка реестра не разобрана: «{line}».\n"
                    f"Формат: <{' | '.join(KINDS)}> <имя проверки> — <причина>."
                )
            kind = parts[0]
            rest = parts[1].split(None, 1)
            name = rest[0]
            reason = rest[1].lstrip("—- ").strip() if len(rest) > 1 else ""
            entries[name] = (kind, reason)
    return entries


def check(root, names, baseline, ceiling=None):
    """Находки: проверка без границы, устаревшая или висячая запись реестра.

    `ceiling` задаёт потолок долга; по умолчанию берётся боевой. Параметр нужен
    самопроверке: её образцы живут своим долгом, не связанным с реестром дерева.
    """
    if ceiling is None:
        ceiling = DEBT_CEILING
    problems = []
    for name in names:
        called = calls_helper(os.path.join(root, "scripts", name))
        entry = baseline.get(name)
        if called and entry:
            problems.append(("G2", name, f"зовёт помощника, а в реестре значится как «{entry[0]}»"))
        elif not called and not entry:
            problems.append(("G1", name, "нет вызова require_input и нет записи в реестре"))
    for name in sorted(baseline):
        if name not in names:
            problems.append(("G3", name, "записи соответствует не существующая проверка"))
    debt = [name for name, (kind, _) in baseline.items() if kind == "долг"]
    if len(debt) > ceiling:
        problems.append(
            ("G4", "gate-input-baseline.txt",
             f"долг {len(debt)} записей при потолке {ceiling} — реестр обязан убывать")
        )
    return problems


def report(problems):
    """Печать находок: проверка падает списком, а не первой находкой."""
    print("Проверка без нижней границы входа (фича 0536):", file=sys.stderr)
    for code, name, message in problems:
        print(f"  {code} {name}: {message}", file=sys.stderr)
    print(
        "\nПустая выборка неотличима от благополучия: разбор ничего не нашёл, и\n"
        "проверка отвечает успехом, не прочитав ни строки. Заведите границу\n"
        "вызовом require_input из scripts/gatelib.py (или gatelib.sh) либо\n"
        "назовите причину в scripts/gate-input-baseline.txt.",
        file=sys.stderr,
    )


def self_test():
    """Ловушки обязаны срабатывать, а согласованный вход — проходить молча.

    Без обратной стороны проверка, отвергающая всё, тоже прошла бы набор.
    """
    import tempfile

    with tempfile.TemporaryDirectory() as work:
        scripts = os.path.join(work, "scripts")
        os.makedirs(scripts)

        def put(name, text):
            with open(os.path.join(scripts, name), "w", encoding="utf-8") as handle:
                handle.write(text)

        put("check-with.py", 'from gatelib import require_input\nrequire_input("вход", 1)\n')
        put("check-without.py", 'print("нечего проверять")\n')
        put("check-noted.sh", 'echo "гоняет чужой инструмент"\n')

        names = scripts_of(work)
        if names != ["check-noted.sh", "check-with.py", "check-without.py"]:
            sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: перечень проверок неверен: {names}")

        baseline = {"check-noted.sh": ("исключение", "судит по коду возврата")}
        found = check(work, names, baseline)
        codes = sorted(code for code, _, _ in found)
        if codes != ["G1"]:
            sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: ожидалось G1, получено {found}")

        baseline["check-without.py"] = ("долг", "вход — записи реестра")
        if check(work, names, baseline, ceiling=1):
            sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: согласованный вход дал находки")

        baseline["check-with.py"] = ("долг", "устаревшая запись")
        codes = sorted(code for code, _, _ in check(work, names, baseline, ceiling=2))
        if codes != ["G2"]:
            sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: устаревшая запись реестра не поймана: {codes}")
        del baseline["check-with.py"]

        baseline["check-gone.py"] = ("долг", "проверки нет")
        codes = sorted(code for code, _, _ in check(work, names, baseline, ceiling=2))
        if codes != ["G3"]:
            sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: висячая запись реестра не поймана: {codes}")
        del baseline["check-gone.py"]

        # Потолок долга ловит рост реестра.
        codes = sorted(code for code, _, _ in check(work, names, baseline, ceiling=0))
        if "G4" not in codes:
            sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: рост долга не пойман: {codes}")

    print("Самопроверка check-gate-inputs: все классы срабатывают.")


def main():
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    names = scripts_of(ROOT)
    # Проверка подчиняется собственному правилу: пустой перечень проверок
    # означал бы, что она ничего не прочла.
    note = require_input("проверки дерева", len(names), source=os.path.relpath(SCRIPTS, ROOT))
    baseline = read_baseline(BASELINE)
    problems = check(ROOT, names, baseline)
    if problems:
        report(problems)
        return 1
    debt = sum(1 for kind, _ in baseline.values() if kind == "долг")
    print(
        f"Нижняя граница входа: {note}, долга {debt} при потолке {DEBT_CEILING}, "
        f"исключений {len(baseline) - debt}."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

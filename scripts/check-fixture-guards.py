#!/usr/bin/env python3
"""check-fixture-guards.py — сторожа фикстур не констатируют разбор (фича 0288).

Фикстура `valid/` что-то **обещает** — «тип выводится из функции», «локальная
переменная доступна в блоке», «перечисление несёт варианты». Сторож обязан
проверять это обещание, а не то, что файл разбирается.

Цена констатации измерена: `example_ce6_type_inference_chain_valid` объявлял
вывод типа по ссылке и был зелёным **всё время, пока вывод не работал вовсе**
(фича 0204). Тот же класс — фикс 0010-01: проверяли структуру автомата вместо
языка.

Замер 2026-08-20: из 62 тестов на фикстурах `valid/` **28** не проверяют ничего,
кроме успешного построения.

Гейт — **ратчет**: список таких тестов заморожен в
`scripts/fixture-guard-baseline.txt`, и появление новых записей роняет прогон.
Единственная допустимая правка реестра — **удаление** записи вместе с усилением
сторожа.

Два класса находок:

- **F1** — новый слабый сторож (в реестре его нет);
- **F2** — запись реестра больше не соответствует коду: тест усилен или
  переименован. Такая запись замораживает пустоту, а храповик проворачивается
  назад.

⚠️ «Слабым» считается тест, у которого нет ни `assert_eq!`/`assert_ne!`, ни
`assert!` с содержательным предикатом: только `unwrap`/`expect` либо
`assert!(… .is_ok())`. Проверка синтаксическая — она не судит о **смысле**
проверки, а лишь отличает констатацию от утверждения.

Использование:

    python3 scripts/check-fixture-guards.py
    python3 scripts/check-fixture-guards.py --self-test
"""

import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gatelib import require_input  # noqa: E402  (путь к помощнику известен только здесь)

ROOT = os.environ.get(
    "FG_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)
BASELINE = os.path.join(ROOT, "scripts", "fixture-guard-baseline.txt")
TEST_DIRS = ("takt-lang/tests", "takt-sim/tests")

TEST_HEAD = re.compile(r"#\[test\]\s*\n(?:\s*//[^\n]*\n)*\s*fn (\w+)\(\)[^{]*\{")


def body_of(text, start):
    """Тело функции от `start` (сразу после `{`) до парной скобки."""
    depth, i = 1, start
    while i < len(text) and depth:
        if text[i] == "{":
            depth += 1
        elif text[i] == "}":
            depth -= 1
        i += 1
    return text[start:i]


def is_weak(body):
    """Констатация ли это разбора (в противоположность утверждению о поведении)."""
    if re.search(r"assert(_eq|_ne)!", body):
        return False
    for predicate in re.findall(r"assert!\s*\(([^;]*)\);", body, re.S):
        if not re.search(r"is_ok\(\)|is_some\(\)|is_none\(\)|\.is_empty\(\)", predicate):
            return False
        if re.search(r"contains|matches!|==|!=|\.len\(\)", predicate):
            return False
    return True


def collect(root):
    """Слабые сторожа фикстур `valid/`: множество `путь::имя`."""
    weak = set()
    total = 0
    for rel in TEST_DIRS:
        base = os.path.join(root, rel)
        for dirpath, _, files in os.walk(base):
            for name in sorted(files):
                if not name.endswith(".rs"):
                    continue
                path = os.path.join(dirpath, name)
                with open(path, encoding="utf-8") as handle:
                    text = handle.read()
                for match in TEST_HEAD.finditer(text):
                    body = body_of(text, match.end())
                    if "/valid/" not in body:
                        continue
                    total += 1
                    if is_weak(body):
                        weak.add(f"{os.path.relpath(path, root)}::{match.group(1)}")
    return weak, total


def read_baseline(path):
    if not os.path.isfile(path):
        sys.exit(f"ОШИБКА: не найден реестр {path} (фича 0288).")
    entries = set()
    with open(path, encoding="utf-8") as handle:
        for line in handle:
            line = line.strip()
            if line and not line.startswith("#"):
                entries.add(line)
    return entries


def check(weak, baseline):
    """Находки `(класс, запись)`; пустой список — гейт пройден."""
    problems = []
    for item in sorted(weak - baseline):
        problems.append(("F1", item))
    for item in sorted(baseline - weak):
        problems.append(("F2", item))
    return problems


def self_test():
    assert is_weak('build_file("…/valid/x.takt").unwrap();'), "unwrap — констатация"
    assert is_weak('assert!(build_file("…/valid/x.takt").is_ok());'), "is_ok — констатация"
    assert not is_weak('assert_eq!(ty_of("…/valid/x.takt"), "Bool");'), "assert_eq — утверждение"
    assert not is_weak('assert!(printed.contains("Always"));'), "contains — утверждение"

    if check({"a::b"}, set()) != [("F1", "a::b")]:
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: новый слабый сторож не пойман")
    if check(set(), {"a::b"}) != [("F2", "a::b")]:
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: протухшая запись не поймана")
    if check({"a::b"}, {"a::b"}):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: согласованный вход дал находки")

    print("  самопроверка гейта: ловушка взведена (F1/F2 и разбор тела)")


def main():
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    weak, total = collect(ROOT)
    if total == 0:
        sys.exit(
            "ОШИБКА: тестов на фикстурах `valid/` не найдено — проверка вырождена."
        )
    baseline = read_baseline(BASELINE)
    problems = check(weak, baseline)
    if problems:
        print("ОШИБКА: сторожа фикстур (фича 0288):", file=sys.stderr)
        for kind, item in problems:
            what = {
                "F1": "новый сторож только констатирует разбор — проверьте ОБЕЩАНИЕ фикстуры",
                "F2": "запись реестра больше не соответствует коду — удалите её",
            }[kind]
            print(f"  [{kind}] {item} — {what}", file=sys.stderr)
        print(
            f"\nРеестр {os.path.relpath(BASELINE, ROOT)} — узаконенный долг, а не\n"
            "разрешение: единственная допустимая правка — удаление записи вместе с\n"
            "усилением сторожа. Цена констатации измерена: тест был зелёным всё\n"
            "время, пока обещанное им поведение не работало (фича 0204).",
            file=sys.stderr,
        )
        return 1

    note = require_input("тесты на фикстуры valid", total, source="takt-lang/tests")
    print(
        f"Сторожа фикстур: {note}, "
        f"констатаций в реестре {len(baseline)} — новых нет."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

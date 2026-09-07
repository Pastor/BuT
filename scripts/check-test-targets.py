#!/usr/bin/env python3
"""check-test-targets.py — устройство интеграционных тестов (фича 0271).

Фича 0244 свела 147 тестовых бинарников в **семь тем**: `tests/<тема>/main.rs`
объявляет `mod`, а наборы лежат рядом своими файлами. Это дало −98.2 с стадии
`cargo test` — счёт шёл не на исполнение, а на **первый запуск** каждого
бинарника (0.60 с при загрузке ЦП 2 %).

Правило держалось **только дисциплиной**: cargo считает целью любой
`tests/*.rs`, и файл, положенный туда, молча вернул бы свою долю стоимости —
0.42 с, незаметные поодиночке и заметные к десятому набору. Предупреждение
стояло в `CLAUDE.md` и `README.md`; машина не проверяла ничего.

Два класса находок:

- **T1** — `.rs` в `tests/` верхнего уровня: новая тестовая цель;
- **T2** — файл набора лежит в теме, но **не объявлен** `mod` в её `main.rs`.
  Этот класс тише первого: набор не собирается и не запускается **вовсе**, а
  `cargo test` рапортует об успехе — тесты, которых нет, не падают.

⚠️ Каталог `tests/data/` — фикстуры, а не наборы: он пропускается.

Использование:

    python3 scripts/check-test-targets.py
    python3 scripts/check-test-targets.py --self-test
"""

import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gatelib import require_input  # noqa: E402  (путь к помощнику известен только здесь)

ROOT = os.environ.get(
    "TT_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)

# Список крейтов ведётся здесь, и новый крейт обязан в него попасть: иначе
# его тесты проверке невидимы молча.
CRATES = ("takt-lang", "takt-sim", "takt-wasm")
# Каталоги, которые темами не являются: фикстуры и вспомогательные данные.
NOT_A_THEME = {"data"}

MOD_DECL = re.compile(r"^\s*mod\s+([A-Za-z0-9_]+)\s*;", re.M)


def scan(root):
    """Собирает устройство тестов: `(лишние цели, темы)`.

    `темы` — словарь `путь темы → (файлы наборов, объявленные модули)`.
    """
    stray = []
    themes = {}
    for crate in CRATES:
        tests = os.path.join(root, crate, "tests")
        if not os.path.isdir(tests):
            continue
        for name in sorted(os.listdir(tests)):
            path = os.path.join(tests, name)
            if os.path.isfile(path) and name.endswith(".rs"):
                stray.append(os.path.relpath(path, root))
                continue
            if not os.path.isdir(path) or name in NOT_A_THEME:
                continue
            main = os.path.join(path, "main.rs")
            files = sorted(
                f[:-3]
                for f in os.listdir(path)
                if f.endswith(".rs") and f != "main.rs"
            )
            declared = set()
            if os.path.isfile(main):
                with open(main, encoding="utf-8") as handle:
                    declared = set(MOD_DECL.findall(handle.read()))
            else:
                declared = None
            themes[os.path.relpath(path, root)] = (files, declared)
    return stray, themes


def check(stray, themes):
    """Находки `(класс, место, пояснение)`; пустой список — гейт пройден."""
    problems = []
    if not themes:
        problems.append(("T0", "tests/", "тем не найдено — проверка вырождена"))
        return problems
    for path in stray:
        problems.append(("T1", path, "файл в tests/ верхнего уровня — это новая тестовая цель"))
    for theme, (files, declared) in sorted(themes.items()):
        if declared is None:
            problems.append(("T3", theme, "у темы нет main.rs — наборы не собираются"))
            continue
        for name in files:
            if name not in declared:
                problems.append(
                    ("T2", f"{theme}/{name}.rs", "набор не объявлен `mod` в main.rs темы")
                )
    return problems


def self_test():
    """Ловушки взведены: T1, T2, T3 ловятся, согласованный вход — нет."""
    ok_themes = {"takt-lang/tests/semantic": (["a", "b"], {"a", "b"})}
    if check([], ok_themes):
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: согласованный вход дал находки: {check([], ok_themes)}")

    found = {k for k, _, _ in check(["takt-lang/tests/loose.rs"], ok_themes)}
    if found != {"T1"}:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: T1 не пойман: {found}")

    found = {k for k, _, _ in check([], {"t": (["a", "b"], {"a"})})}
    if found != {"T2"}:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: T2 не пойман: {found}")

    found = {k for k, _, _ in check([], {"t": (["a"], None)})}
    if found != {"T3"}:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: T3 не пойман: {found}")

    if {k for k, _, _ in check([], {})} != {"T0"}:
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: пустой вход принят за успех")

    print("  самопроверка гейта: ловушка взведена (T0…T3, согласованный вход — нет)")


def main():
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    stray, themes = scan(ROOT)
    problems = check(stray, themes)
    if problems:
        print("ОШИБКА: устройство интеграционных тестов (фича 0271):", file=sys.stderr)
        for kind, place, why in problems:
            print(f"  [{kind}] {place} — {why}", file=sys.stderr)
        print(
            "\nНаборы живут в темах: `tests/<тема>/main.rs` объявляет `mod`, файлы\n"
            "лежат рядом (фича 0244 — так снято 98.2 с стадии тестов).\n"
            "  T1: перенесите файл в тему и объявите его там;\n"
            "  T2: допишите `mod <имя>;` в main.rs темы — иначе набор не\n"
            "      собирается и не запускается, а `cargo test` рапортует об успехе.",
            file=sys.stderr,
        )
        return 1

    sets = sum(len(files) for files, _ in themes.values())
    note = require_input("наборы интеграционных тестов", sets, source="tests/")
    print(
        f"Устройство тестов: {len(themes)} тем, {note} — "
        "лишних целей нет, все наборы объявлены."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env python3
"""Гейт: сценарии симулятора в репозитории пользуются ИМЕНАМИ портов (фича 0150).

ЗАЧЕМ. Позиционная форма (`"in_ports": [0, 1, 0]`) привязывает значение к месту
имени в АЛФАВИТНОМ списке портов модели и её под-моделей. Проба фичи 0132: один
добавленный порт превратил «датчик этажа 2» в «верхний датчик этажа 1» — шаг
молча стал описывать другое событие. Инструмент об этой форме предупреждает
(`SIM-037`), но предупреждение можно и не заметить; репозиторий удерживает
машина.

ЧТО ПРОВЕРЯЕТСЯ. В каждом `.json` ищутся поля сценария (`in_ports`, `inout`,
`out`, `vars`) со значением-массивом. Найденный файл обязан быть в списке
исключений ниже — с причиной.

⚠️ РАТЧЕТ. Запись исключения, которой не соответствует ни один позиционный файл,
ВАЛИТ гейт (образец — `scripts/module-size-baseline.txt`). Иначе список
превращается в свалку разрешений, переживающую свои поводы.

Использование: scripts/check-positional-scenarios.py [корень]
"""

import json
import os
import sys

# Файлы, которым позиционная форма нужна по существу: путь -> причина.
ALLOWED = {
    "takt-sim/tests/data/named0132/short_positional.json": (
        "фикстура сторожа SIM-032 (длина массива не совпадает с числом портов) — "
        "именованной формой этот вход не выразить"
    ),
    "takt-sim/tests/data/named0132/positional_multi_step.json": (
        "фикстура сторожа SIM-037: несколько шагов позиционной формы, на которых "
        "проверяется, что предупреждение печатается ОДИН раз за прогон, — "
        "именованной формой такой вход не выразить"
    ),
}

# Поля шага сценария, где значение может быть массивом значений портов.
PORT_FIELDS = ("in_ports", "inout", "out", "vars")

SKIP_DIRS = {".git", "target", "node_modules", ".idea", "build"}


def has_positional(node) -> bool:
    """Есть ли в дереве JSON поле сценария со значением-массивом?"""
    if isinstance(node, dict):
        for key, value in node.items():
            if key in PORT_FIELDS and isinstance(value, list):
                return True
            if has_positional(value):
                return True
    elif isinstance(node, list):
        return any(has_positional(item) for item in node)
    return False


def scan(root: str) -> list[str]:
    """Пути (относительно корня) всех позиционных сценариев."""
    found = []
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in SKIP_DIRS]
        for name in filenames:
            if not name.endswith(".json"):
                continue
            path = os.path.join(dirpath, name)
            try:
                with open(path, encoding="utf-8") as handle:
                    data = json.load(handle)
            except (OSError, ValueError):
                # Не сценарий либо нечитаемый JSON - не наше дело.
                continue
            if has_positional(data):
                found.append(os.path.relpath(path, root))
    return sorted(found)


def self_check() -> None:
    """Ловушка взведена? Проверяем на образцах, а не на вере."""
    traps = [
        ({"in_ports": [0, 1]}, True, "массив в in_ports"),
        ([{"in_ports": {"a": 1}}, {"inout": [3]}], True, "массив в inout шага"),
        ({"guard": {"out": [0]}}, True, "массив в guard.out"),
        ({"guard": {"vars": [1]}}, True, "массив в guard.vars"),
        ({"in_ports": {"a": 1}}, False, "именованная форма"),
        ([{"in_ports": {"a": 1}, "guard": {"out": {"b": 0}}}], False, "именованный шаг"),
        ({"steps": 5, "notes": ["текст"]}, False, "массив в постороннем поле"),
    ]
    for sample, expected, what in traps:
        if has_positional(sample) != expected:
            sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: «{what}» распознано неверно")
    print("  самопроверка гейта: ловушка взведена (4 класса ловятся, 3 законные формы — нет)")


def main() -> int:
    root = sys.argv[1] if len(sys.argv) > 1 else "."
    root = os.path.abspath(root)
    self_check()

    found = scan(root)
    unexpected = [p for p in found if p not in ALLOWED]
    stale = [p for p in ALLOWED if p not in found]

    if unexpected:
        print("\nПозиционные сценарии вне списка исключений (фича 0150):", file=sys.stderr)
        for path in unexpected:
            print(f"  {path}", file=sys.stderr)
        print(
            "\nИндекс в массиве привязан к АЛФАВИТНОМУ месту имени порта: добавление\n"
            "порта сдвигает весь массив, и шаг начинает описывать другое событие —\n"
            "молча. Задайте значения именами: {\"имя_порта\": значение}.",
            file=sys.stderr,
        )
        return 1

    if stale:
        print("\nУстаревшие записи списка исключений (фича 0150, ратчет):", file=sys.stderr)
        for path in stale:
            print(f"  {path} — позиционной формы в файле больше нет", file=sys.stderr)
        print(
            "\nЗапись исключения без своего файла — разрешение, пережившее повод.\n"
            "Уберите её из ALLOWED в scripts/check-positional-scenarios.py.",
            file=sys.stderr,
        )
        return 1

    print(
        f"  Сценарии: проверено json-файлов дерева, позиционных — {len(found)} "
        f"(все названы в исключениях)."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

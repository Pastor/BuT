#!/usr/bin/env python3
"""check-registry-verdicts.py — вердикт закрытой фичи не бывает заготовкой (фича 0218).

Реестр фич (`docs/features/README.md`) несёт
последней колонкой ответ на вопрос «чем кончилось тестирование». Заготовку
строки пишет `scripts/new-feature.sh` — значением `СОЗДАНА`, и на момент
создания это верно. Стадия закрытия колонку не правила: чек-лист о ней молчал,
машина её не смотрела.

Замер при заведении гейта (2026-08-17): `docs/features/README.md` — **44** таких
строки, `docs/features/README.md` — **35**, и ВСЕ до одной принадлежат фичам в
статусе `ГОТОВО`. То есть реестр отчётов утверждал «тестирование не
проводилось» ровно там, где оно проведено и задокументировано, — а по такой
записи проверку заводят заново.

Гейт судит ФОРМУ, а не истину: доказать, что тестирование было, он не может
(это на человеке, правило 4). Он запрещает единственное — заготовке пережить
закрытие фичи.

⚠️ Статус фичи в реестре записан ПЯТЬЮ формами (`ГОТОВО`, `✅ ГОТОВО`,
`**ГОТОВО**`, плюс формы с приписками вроде «ГОТОВО (с оговоркой…)»), поэтому
сравнение по точной строке дало бы ложное «не закрыта» для четверти реестра.
Отсюда нормализация ниже — тот же приём, что в `check-claude-md.py`, но с
отбрасыванием скобочного хвоста.
"""

import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FEATURES_REGISTRY = os.path.join(ROOT, "docs", "features", "README.md")
# Реестр один: стадии живут разделами карточки, и
# реестров `docs/tests/`, `docs/reports/` больше нет. Колонок в строке фичи
# четыре: номер, название, статус, вердикт тестирования.
STAGE_REGISTRIES = {
    os.path.join("docs", "features", "README.md"): "Вердикт тестирования",
}
# Заготовка, которую пишет генератор.
PLACEHOLDER = "СОЗДАНА"
# Статусы, означающие, что работа по фиче кончена и вердикт обязан быть настоящим.
TERMINAL = ("ГОТОВО", "ОТМЕНА")


def read_lines(path):
    with open(path, encoding="utf-8") as handle:
        return handle.read().splitlines()


def normalize_status(cell):
    """Первое слово статуса без украшений: `✅ **ГОТОВО** (тег …)` → `ГОТОВО`."""
    text = re.sub(r"[*✅]", "", cell)
    text = re.sub(r"\(.*", "", text)
    return text.strip().split(" ")[0] if text.strip() else ""


def table_cells(line):
    """Ячейки строки таблицы Markdown, или None, если строка таблицей не является."""
    if not line.startswith("|"):
        return None
    return [cell.strip() for cell in line.strip().strip("|").split("|")]


FEATURE_LINK = re.compile(r"\[[^\]]*\]\((?:\./)?(\d{4})-[^)]*\)")


def feature_number(cell):
    """Номер фичи из первой ячейки реестра; `None` - её там нет.

    Номер берётся из адреса ссылки, а не из её текста: текстом служит слово, а
    номер живёт в имени файла карточки. Голый номер принимается тоже - так
    записаны ячейки реестров стадий.
    """
    match = FEATURE_LINK.search(cell)
    if match:
        return match.group(1)
    cell = cell.strip()
    return cell if re.fullmatch(r"\d{4}", cell) else None


def feature_statuses(lines):
    """Номер фичи → нормализованный статус из реестра `docs/features/README.md`."""
    statuses = {}
    for line in lines:
        cells = table_cells(line)
        if not cells:
            continue
        number = feature_number(cells[0])
        if number:
            statuses[number] = normalize_status(cells[-2])
    return statuses


def check_registry(lines, statuses, relative_path, column):
    """Находки одного реестра: заготовка у фичи с терминальным статусом."""
    problems = []
    for number, line in enumerate(lines, start=1):
        cells = table_cells(line)
        if not cells or len(cells) < 3:
            continue
        feature = feature_number(cells[0])
        if not feature:
            continue
        if normalize_status(cells[-1]) != PLACEHOLDER:
            continue
        # Статус берётся из этой же строки (предпоследняя ячейка): реестр
        # один, и второго источника статуса больше нет.
        status = normalize_status(cells[-2]) or statuses.get(feature)
        if status in TERMINAL:
            problems.append(
                (
                    f"{relative_path}:{number}",
                    f"фича {feature} — {status}, а колонка «{column}» осталась "
                    f"заготовкой «{PLACEHOLDER}»",
                )
            )
    return problems


def run_checks(statuses, registries):
    """registries: путь → (строки, имя колонки). Возвращает список находок."""
    problems = []
    for relative_path, (lines, column) in registries.items():
        problems.extend(check_registry(lines, statuses, relative_path, column))
    return problems


def self_test():
    """Ловушка обязана срабатывать на подложенной строке — и молчать на законной.

    Без этой проверки «находок нет» неотличимо от «гейт ничего не ищет»
    (образец — `check-claude-md.py`).
    """
    statuses = {"0001": "ГОТОВО", "0002": "СОЗДАНА", "0003": "ОТМЕНА"}

    planted = ["| [0001](./0001-a.md) | Проба | ГОТОВО | СОЗДАНА |"]
    if not run_checks(statuses, {"проба": (planted, "Вердикт")}):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: заготовка у закрытой фичи не поймана")

    # Отменённая фича - тоже терминальная: вердикт обязан быть настоящим.
    planted_cancelled = ["| [0003](./0003-c.md) | Проба | ОТМЕНА | СОЗДАНА |"]
    if not run_checks(statuses, {"проба": (planted_cancelled, "Вердикт")}):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: заготовка у отменённой фичи не поймана")

    # Обратная сторона - законные записи ложных находок давать не должны.
    clean = [
        "| [0002](./0002-b.md) | Фича в работе | АНАЛИЗ | СОЗДАНА |",
        "| [0001](./0001-a.md) | Закрытая фича | ГОТОВО | ✅ ГОТОВО |",
        "| [0001](./0001-a.md) | Закрытая с припиской | ГОТОВО | **ГОТОВО** (тег `v0.5.0`) |",
        "| [0004](./0004-d.md) | Ранняя фича без отчёта | ГОТОВО | — |",
        "| Фича | Наименование | Статус | Вердикт тестирования |",
        "текст вне таблицы",
    ]
    found = run_checks(statuses, {"проба": (clean, "Вердикт")})
    if found:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: ложные находки на законных строках: {found}")

    # Нормализация обязана справляться со всеми формами, встречающимися в реестре.
    for cell in ("ГОТОВО", "✅ ГОТОВО", "**ГОТОВО**", "ГОТОВО (с оговоркой: A5 в CI не проверен)"):
        if normalize_status(cell) != "ГОТОВО":
            sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: форма статуса «{cell}» не нормализуется")

    print("Самопроверка check-registry-verdicts: все классы срабатывают.")


def main():
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    statuses = feature_statuses(read_lines(FEATURES_REGISTRY))
    registries = {
        path: (read_lines(os.path.join(ROOT, path)), column)
        for path, column in STAGE_REGISTRIES.items()
    }
    problems = run_checks(statuses, registries)
    if problems:
        print(
            "Вердикт закрытой фичи остался заготовкой (фича 0218):", file=sys.stderr
        )
        for place, message in problems:
            print(f"  {place}: {message}", file=sys.stderr)
        print(
            "\nРеестр — вход в проект для новой сессии: запись «СОЗДАНА» у закрытой\n"
            "фичи читается как «тестирование не проводилось», и по ней заводят\n"
            "проверку заново. Проставьте вердикт из отчёта (стадия 8, правило 21).",
            file=sys.stderr,
        )
        return 1
    total = sum(len(lines) for lines, _ in registries.values())
    print(
        f"Вердикт тестирования в реестре фич: проверено {total} строк, "
        f"заготовок у закрытых фич нет."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

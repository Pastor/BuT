#!/usr/bin/env python3
"""check-feature-status.py — статус фичи: карточка ↔ реестр ↔ витрина (фича 0177).

Статус живёт в **двух** местах: поле `- **Статус:**` карточки
`docs/features/XXXX-*.md` и колонка «Статус» реестра `docs/features/README.md`.
Оба обновляются руками, и шаг реестра пропускался: аудит 2026-07-27 нашёл ~20
закрытых фич, показанных как `СОЗДАНА`, и синхронизировал их разово — **дрейф
вернулся за два дня** (замер 2026-07-29 нашёл фичу 0138: закрыта полным циклом,
а в реестре «СОЗДАНА, стадия 1»).

Реестр — вход в проект: по нему выбирают, что сделано и что брать. Строка
«стадия 1» у закрытой фичи заставляет заводить работу заново.

⚠️ **Сравниваются ТОКЕНЫ статуса, а не текст ячеек.** Наивное сравнение строк
даёт 48 находок из 180, и 47 — шум формы записи: `**ГОТОВО**`, `✅ ГОТОВО`,
`ГОТОВО (тег ...)`, `**ГОТОВО с оговоркой**`. Срезание скобок тоже не годится:
ячейка 0028 содержит вложенные скобки внутри кода (`S(Модель) = Состояние`), и
нежадный `\\(.*?\\)` рвёт её посередине. Токенная сверка даёт **0 ложных**.

Проверяется:

1. существование в обе стороны (предусловие сравнения; полная сверка реестров с
   диском — предмет фичи 0164, её область шире);
2. статус принадлежит известному набору (ловит опечатку);
3. токены статуса в карточке и реестре совпадают;
4. **правило «Последовательность разработки (таблица фич в `FEATURES.md`)»**: терминальный статус (`ГОТОВО`/`ОТМЕНА`) ⇒ фичи нет в таблице
   `FEATURES.md`; нетерминальный ⇒ фича в таблице есть.

⚠️ **Автопочинки нет намеренно.** Машина затёрла бы осмысленные приписки реестра
(`тег v0.5.0`, `с оговоркой: A5 в CI не проверен`), а расхождение может означать
ошибку и в **карточке** — тогда автопочинка размножила бы её в реестр.

Использование:

    python3 scripts/check-feature-status.py              # проверка
    python3 scripts/check-feature-status.py --self-test  # проверка самой ловушки
"""

import glob
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
REGISTRY = os.path.join(ROOT, "docs", "features", "README.md")
SHOWCASE = os.path.join(ROOT, "FEATURES.md")
CARDS_GLOB = os.path.join(ROOT, "docs", "features", "0*.md")

# Набор статусов плюс заморожена.
#
# `ЗАМОРОЖЕНА` в свод (docs/RULE.md) не входит: она введена
# решением для и описана только в FEATURES.md. Проверка её
# принимает - иначе отказал бы на законной строке; несоответствие свода практике
# заведено кандидатом, поскольку правка свода есть изменение процесса.
STATUSES = [
    "СОЗДАНА",
    "АРХИТЕКТУРА",
    "АНАЛИЗ",
    "РАЗРАБОТКА",
    "ТЕСТИРОВАНИЕ",
    "ИСПРАВЛЕНИЕ",
    "ЗАБЛОКИРОВАНА",
    "ЗАМОРОЖЕНА",
    "ГОТОВО",
    "ОТМЕНА",
]

# Терминальные статусы: фича с таким статусом обязана быть удалена из витрины
# .
TERMINAL = {"ГОТОВО", "ОТМЕНА"}


def status_token(cell):
    """Известный статус из ячейки; None — не найден, список — найдено несколько."""
    found = [s for s in STATUSES if re.search(r"\b" + s + r"\b", cell)]
    if not found:
        return None
    if len(found) > 1:
        return found
    return found[0]


def read(path):
    with open(path, encoding="utf-8") as handle:
        return handle.read()


def registry_statuses(text):
    """Номер фичи → (ячейка, токен) из реестра."""
    result = {}
    for line in text.splitlines():
        # Колонок стало четыре: номер, название, статус, вердикт.
        # Статус - третья ячейка, а не последняя: последняя теперь вердикт
        # тестирования.
        # Номер берётся из адреса карточки: текстом ссылки служит её название.
        match = re.match(
            r"\|\s*\[[^\]]*\]\((?:\./)?(\d{4})[^)]*\)\s*\|[^|]*\|([^|]*)\|[^|]*\|", line
        )
        if match:
            cell = match.group(2)
            result[match.group(1)] = (cell.strip(), status_token(cell))
    return result


def card_statuses(paths):
    """Номер фичи → (строка, токен) из карточек."""
    result = {}
    for path in paths:
        number = os.path.basename(path)[:4]
        match = re.search(r"-\s*\*\*Статус:\*\*\s*(.+)", read(path))
        if match is None:
            result[number] = (None, None)
        else:
            line = match.group(1).strip()
            result[number] = (line, status_token(line))
    return result


def showcase_numbers(text):
    """Номера фич, стоящих в таблице витрины FEATURES.md."""
    # Номер стоит в адресе карточки: текстом ссылки служит её название.
    return set(re.findall(r"\|\s*\[.*?\]\(docs/features/(\d{4})", text))


def run_checks(registry_text, cards, showcase_text):
    registry = registry_statuses(registry_text)
    showcase = showcase_numbers(showcase_text)
    problems = []

    for number in sorted(set(registry) | set(cards)):
        in_registry = number in registry
        in_cards = number in cards
        if not in_cards:
            problems.append((number, "строка реестра есть, карточки на диске нет"))
            continue
        if not in_registry:
            problems.append((number, "карточка есть, строки в реестре нет"))
            continue

        reg_cell, reg_token = registry[number]
        card_line, card_token = cards[number]

        if card_line is None:
            problems.append((number, "в карточке нет поля `- **Статус:**`"))
            continue
        for where, token, raw in (("реестре", reg_token, reg_cell),
                                  ("карточке", card_token, card_line)):
            if token is None:
                problems.append((number, f"в {where} неизвестный статус: {raw!r}"))
            elif isinstance(token, list):
                problems.append((number, f"в {where} несколько статусов сразу: {token}"))
        if not isinstance(reg_token, str) or not isinstance(card_token, str):
            continue

        if reg_token != card_token:
            problems.append(
                (number, f"статус разошёлся: реестр={reg_token}, карточка={card_token}")
            )
            continue

        # терминальный статус ⇒ фичи нет в витрине, и наоборот.
        if reg_token in TERMINAL and number in showcase:
            problems.append(
                (number, f"статус {reg_token}, но фича осталась в таблице FEATURES.md "
                         "(правило «Последовательность разработки (таблица фич в `FEATURES.md`)» — удалить из витрины)")
            )
        if reg_token not in TERMINAL and number not in showcase:
            problems.append(
                (number, f"статус {reg_token} (не закрыта), но фичи нет в таблице FEATURES.md")
            )
    return problems


def self_test():
    """Ловушка обязана срабатывать на каждом классе и молчать на законных формах."""
    # Колонки реестра: номер | название | статус | вердикт.
    reg_row = "| [9001](./9001-x.md) | Проба | {} | вердикт |"
    card = "# Фича 9001\n\n- **Статус:** {}\n"
    showcase_row = "| [9001](docs/features/9001-x.md) | Проба | — | proc | СОЗДАНА |"

    cases = {
        "расхождение статуса": (reg_row.format("СОЗДАНА"), card.format("ГОТОВО"), ""),
        "неизвестный статус": (reg_row.format("ГОТВО"), card.format("ГОТОВО"), ""),
        "правило «Последовательность разработки (таблица фич в `FEATURES.md`)»: закрытая в витрине": (
            reg_row.format("ГОТОВО"), card.format("ГОТОВО"), showcase_row),
        "правило «Последовательность разработки (таблица фич в `FEATURES.md`)»: открытая вне витрины": (
            reg_row.format("СОЗДАНА"), card.format("СОЗДАНА"), ""),
    }
    for name, (reg, card_text, show) in cases.items():
        found = run_checks(reg, {"9001": card_statuses_from_text(card_text)}, show)
        if not found:
            sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: класс «{name}» не сработал")

    # Законные формы записи - образцы взяты из реального реестра, а не
    # придуманы: упрощённый образец проверял бы упрощение.
    legal = [
        ("**ГОТОВО**", "ГОТОВО"),
        ("✅ ГОТОВО", "ГОТОВО"),
        ("ГОТОВО (тег `v0.5.0`)", "**ГОТОВО**"),
        ("ГОТОВО (CC-018; вскрыт дефект трансляции `S(Модель) = Состояние`)", "**ГОТОВО**"),
        ("ГОТОВО", "**ГОТОВО с оговоркой**"),
    ]
    for reg_cell, card_line in legal:
        found = run_checks(
            reg_row.format(reg_cell),
            {"9001": card_statuses_from_text(card.format(card_line))},
            "",
        )
        if found:
            sys.exit(
                f"САМОПРОВЕРКА ПРОВАЛЕНА: законная форма реестр={reg_cell!r} / "
                f"карточка={card_line!r} дала ложные находки: {found}"
            )
    print("  самопроверка гейта: ловушка взведена (4 класса ловятся, 5 законных форм — нет)")


def card_statuses_from_text(text):
    """Разбор поля статуса из текста карточки (для самопроверки)."""
    match = re.search(r"-\s*\*\*Статус:\*\*\s*(.+)", text)
    if match is None:
        return (None, None)
    line = match.group(1).strip()
    return (line, status_token(line))


def main():
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    cards = card_statuses(sorted(glob.glob(CARDS_GLOB)))
    problems = run_checks(read(REGISTRY), cards, read(SHOWCASE))
    if problems:
        print("Расхождения статуса фич (фича 0177):", file=sys.stderr)
        for number, message in problems:
            print(f"  фича {number}: {message}", file=sys.stderr)
        print(
            "\nРеестр docs/features/README.md — вход в проект: по нему выбирают, что\n"
            "сделано и что брать. Приведите статусы в соответствие ВРУЧНУЮ: гейт\n"
            "намеренно не чинит сам — расхождение может означать ошибку и в карточке.",
            file=sys.stderr,
        )
        return 1
    print(f"Статусы фич: проверено {len(cards)} карточек, расхождений нет.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

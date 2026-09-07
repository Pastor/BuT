#!/usr/bin/env python3
"""check-book-diagnostics.py — коды приложения «Ошибки» против реестра (фича 0290).

Списков кодов диагностик в проекте два: реестр `docs/diagnostics/README.md`
(сверяется с исходниками гейтом `check-diagnostic-codes.sh`, фича 0077) и
сводная таблица приложения `book/src/appendix-errors/index.typ` — справочник,
который читает **автор программы**, увидевший код в выводе инструмента. Второй
список наполнялся руками и сторожа не имел вовсе.

Замер 2026-08-20 (ADR 0290): в реестре 264 кода, в приложении 247. Расхождение
шло в **обе** стороны — 18 кодов не описаны (из них 14 эмитируются сегодня), а
`SE-066` описан, хотя выведен задачей 0134-09: справочник обещал диагностику,
которой инструмент не выдаёт никогда.

Правило (ADR 0290, Option B) — **тождественность множеств**, без исключений:
каждый код реестра описан в приложении (выведенный — с пометкой «(снят)», как
уже оформлены `SE-091`, `SE-093`, `ST-005`), кода вне реестра в приложении нет.
Исключение — место, где сверка молча выключается, поэтому их здесь ноль.

Гейт сверяет **наличие** кода, а не верность его описания: текст остаётся на
человеке (правило 4), как и у `check-claude-md.py`.

⚠️ Коды берутся **из строк таблиц** реестра, а не грепом по всему файлу: проза
реестра называет `CC-009` и `SE-024` в объяснении про пропуски номеров, и
наивный греп добавил бы к расхождению два несуществующих кода (первая редакция
замера на этом и споткнулась — счёт вышел 20 вместо 18).

⚠️ Со стороны документа читается **только сводная таблица**: вторая часть
приложения («Подробный разбор основных ошибок») — подмножество, она раскрывает
основные ошибки, а не все.

⚠️ Пустое множество — **ошибка**, а не успех: при смене разметки проверка
выполнилась бы тривиально (урок фикса 0202-01).

Корень дерева переопределяется переменной `BD_ROOT` — для сторожа
`scripts/test-book-diagnostics.sh`, который гоняет гейт на копии.

Использование:

    python3 scripts/check-book-diagnostics.py
"""

import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gatelib import require_input  # noqa: E402  (путь к помощнику известен только здесь)

ROOT = os.environ.get(
    "BD_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)
REGISTRY = os.path.join(ROOT, "docs", "diagnostics", "README.md")
APPENDIX = os.path.join(ROOT, "book", "src", "appendix-errors", "index.typ")

# Границы сводной таблицы приложения: от заголовка раздела до начала разбора.
SUMMARY_START = "== Сводная таблица кодов"
SUMMARY_END = "== Подробный разбор"

CODE = r"[A-Z]{2,4}-[0-9]{3}"


def registry_codes(text, source):
    """Коды из строк таблиц реестра: строка вида `| \\`XX-NNN\\` | … |`."""
    codes = set()
    for line in text.splitlines():
        match = re.match(rf"\|\s*`({CODE})`\s*\|", line)
        if match:
            codes.add(match.group(1))
    require_input("строки таблиц реестра", len(codes), source=source)
    return codes


def appendix_codes(text, source):
    """Коды сводной таблицы приложения: запись вида `[\\`XX-NNN\\`], [текст],`."""
    start = text.find(SUMMARY_START)
    if start < 0:
        sys.exit(
            f"ОШИБКА: в {source} нет раздела «{SUMMARY_START}».\n"
            "Гейт 0290 сверяет именно сводную таблицу: разбор ошибок — её\n"
            "подмножество. Верните раздел либо обновите гейт вместе с разметкой."
        )
    end = text.find(SUMMARY_END, start)
    summary = text[start:end if end > 0 else len(text)]
    codes = set(re.findall(rf"\[`({CODE})`\]", summary))
    require_input("коды сводной таблицы приложения", len(codes), source=source)
    return codes


def report(missing, extra):
    """Падение — списком: каждое направление лечится по-своему."""
    print("ОШИБКА: приложение «Ошибки» разошлось с реестром диагностик.", file=sys.stderr)
    if missing:
        print(
            f"\n  Нет в приложении ({len(missing)}) — код есть в реестре, "
            "а справочник о нём молчит:",
            file=sys.stderr,
        )
        for code in sorted(missing):
            print(f"    {code}", file=sys.stderr)
        print(
            "  Опишите каждый в сводной таблице book/src/appendix-errors/index.typ\n"
            "  (выведенный код — с пометкой «(снят)», как SE-091, SE-093, ST-005).",
            file=sys.stderr,
        )
    if extra:
        print(
            f"\n  Нет в реестре ({len(extra)}) — справочник обещает диагностику, "
            "которой инструмент не выдаёт:",
            file=sys.stderr,
        )
        for code in sorted(extra):
            print(f"    {code}", file=sys.stderr)
        print(
            "  Удалите запись из приложения либо верните код в реестр — молчаливое\n"
            "  обещание хуже пробела: по нему автор ищет ошибку, которой нет.",
            file=sys.stderr,
        )
    print(
        "\nПравило (ADR 0290): множества кодов реестра и приложения тождественны.",
        file=sys.stderr,
    )


def main():
    try:
        registry_text = open(REGISTRY, encoding="utf-8").read()
        appendix_text = open(APPENDIX, encoding="utf-8").read()
    except OSError as error:
        sys.exit(f"ОШИБКА: не прочитать источник: {error}")

    registry = registry_codes(registry_text, os.path.relpath(REGISTRY, ROOT))
    appendix = appendix_codes(appendix_text, os.path.relpath(APPENDIX, ROOT))

    missing = registry - appendix
    extra = appendix - registry
    if missing or extra:
        report(missing, extra)
        return 1

    print(
        f"Приложение «Ошибки»: {len(appendix)} кодов сверены с реестром диагностик, "
        "расхождений нет."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

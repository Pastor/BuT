#!/usr/bin/env python3
"""check-claude-md.py — согласованность живого контекста `CLAUDE.md` (фича 0149).

`CLAUDE.md` читается **в начале каждой сессии**: его факты становятся
предпосылкой всей последующей работы ещё до первого взгляда в код. Поэтому
ошибка здесь стоит дороже ошибки в любом другом документе — а проверялся он до
сих пор ничем.

Замер при заведении гейта нашёл в файле: два места, объявлявших **закрытые**
фичи «в работе» (35 строк текста, включая действующую инструкцию «не используй
вывод C как эталон» — при том что цель `c` компилируется гейтом, а
`conformance_c_tests` служит основным эталоном сверки проекта), один
несуществующий путь и ссылку на номер строки, указывающую не туда.

Проверяются ТОЛЬКО машинно-проверяемые классы:

1. **статус фичи** — «фича NNNN … в работе / не закрыта» против
   `docs/features/README.md`;
2. **путь к файлу** — разрешается полностью либо по хвосту
   (`semantic/tree.rs` → `takt-lang/src/semantic/tree.rs`);
3. **версия крейта** — каноническая форма ``**сейчас `X.Y.Z`**`` против
   `Cargo.toml`;
4. **номер строки** — форма `файл.rs:NNN` ЗАПРЕЩЕНА (гниёт молча).

⚠️ **Версия ЯЗЫКА здесь не проверяется** — она живёт в
`scripts/check-language-version.sh`, который сверяет её сразу в трёх источниках
(константа, README, `CLAUDE.md`). Два гейта, проверяющие один предмет,
неизбежно разъезжаются — проект платил за это дважды.

⚠️ **Истинность утверждений о поведении кода вне объёма.** Гейт не скажет, что
«`Bit` → `int`» неверно: это проверка прозы на истинность. Он лишь не даёт
соседнему факту — статусу фичи, пути — врать молча.

Использование:

    python3 scripts/check-claude-md.py              # проверка
    python3 scripts/check-claude-md.py --self-test  # проверка самой ловушки
"""

import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CLAUDE_MD = os.path.join(ROOT, "CLAUDE.md")
FEATURES_REGISTRY = os.path.join(ROOT, "docs", "features", "README.md")

# Расширения, по которым строка в обратных кавычках считается путём к файлу.
FILE_SUFFIXES = (".rs", ".toml", ".sh", ".takt", ".lalrpop", ".lua", ".xml", ".yml")

# Упоминания несуществующего, законные по смыслу. Список короткий и с причиной:
# без него проверка требовал бы завести файлы, которых в проекте нет намеренно.
LEGITIMATELY_ABSENT = {
    "TASKS.md": "плоских списков задач в проекте нет — текст об этом и говорит",
    "STATUS.md": "то же: упоминается как отсутствующее по правилу 10",
    "concat.takt": "гипотетический пример ловушки имён IEC, файла нет и не должно быть",
    "lib/ieclib.txt": "файл внутри префикса установки MatIEC, вне репозитория",
}

# Слова, которыми текст заявляет незавершённость фичи.
UNFINISHED = ("в работе", "не закрыт", "не закрыта", "незакрыт")


def read_lines(path):
    with open(path, encoding="utf-8") as handle:
        return handle.read().splitlines()


def feature_statuses():
    """Номер фичи → статус из реестра `docs/features/README.md`."""
    statuses = {}
    for line in read_lines(FEATURES_REGISTRY):
        match = re.match(r"\|\s*\[(\d{4})\]\([^)]*\)\s*\|[^|]*\|[^|]*\|([^|]*)\|", line)
        if match:
            statuses[match.group(1)] = re.sub(r"[*✅\s]", "", match.group(2))
    return statuses


def build_file_index():
    """Индекс `имя файла` → пути в дереве (для разрешения сокращённых форм)."""
    index = {}
    skip = ("/.git", "/target", "/book/book", "/node_modules")
    for directory, dirnames, names in os.walk(ROOT):
        rel = directory[len(ROOT):]
        if any(s in rel for s in skip):
            dirnames[:] = []
            continue
        for name in names:
            index.setdefault(name, []).append(
                os.path.join(directory, name)[len(ROOT) + 1:]
            )
    return index


def paragraphs(lines):
    """Абзацы как `(номер первой строки, текст одной строкой)` — фича 0297.

    ⚠️ **Построчный поиск слеп к переносу**, и это не теория: замер фикса
    0202-01 показал, что `check_crate_versions` искал пару «имя крейта … сейчас
    X.Y.Z» в одной строке, а в `CLAUDE.md` она была перенесена — совпадения не
    возникало **никогда**, и заявленная сверка не проверяла ничего. Факт тогда
    сняли (запись свели в одну строку), а причина осталась: следующая правка
    абзаца вернула бы перенос.

    Номер — **первой** строки абзаца: указывать на середину сшитого текста
    бессмысленно, а начало абзаца читатель найдёт.
    """
    result = []
    start = None
    buffer = []
    for number, line in enumerate(lines, start=1):
        if line.strip():
            if start is None:
                start = number
            buffer.append(line.strip())
            continue
        if buffer:
            result.append((start, " ".join(buffer)))
            start, buffer = None, []
    if buffer:
        result.append((start, " ".join(buffer)))
    return result


def check_feature_status(lines, statuses, problems):
    """Класс 1: закрытая фича, объявленная незавершённой.

    Номер ищется в абзаце, а не в строке: запись «(фичи\\n[0026](…), [0029](…),
    обе в работе)» переносится, и построчный поиск её пропускал — именно так
    35 строк ложного текста и прожили до замера.
    """
    for number, text in paragraphs(lines):
        if not any(word in text for word in UNFINISHED):
            continue
        for feature in sorted(set(re.findall(r"\b(0[01]\d\d)\b", text))):
            if statuses.get(feature) == "ГОТОВО":
                problems.append(
                    (number, f"фича {feature} названа незавершённой, а в реестре — ГОТОВО")
                )


def check_paths(lines, index, problems):
    """Класс 2: путь, который не разрешается ни полностью, ни по хвосту."""
    for number, text in paragraphs(lines):
        for path in re.findall(r"`([A-Za-z0-9_\-./]+\.[a-z]+)`", text):
            if not path.endswith(FILE_SUFFIXES) and not path.endswith(".md"):
                continue
            if path in LEGITIMATELY_ABSENT:
                continue
            if os.path.exists(os.path.join(ROOT, path)):
                continue
            candidates = index.get(os.path.basename(path), [])
            if any(c.endswith(path) for c in candidates):
                continue
            problems.append((number, f"путь `{path}` не найден в дереве"))


def check_crate_versions(lines, problems):
    """Класс 3: каноническая форма версии крейта против манифеста."""
    for number, text in paragraphs(lines):
        # Между именем крейта и версией законно стоят другие обратные кавычки
        # (`крейта `takt-lang` (`CARGO_PKG_VERSION`, сейчас `0.20.0`)`), поэтому
        # класс символов их не исключает - первая редакция исключала, и мутация
        # версии проходила мимо проверки.
        # Поиск идёт по абзацу: в строке пара "имя ... сейчас
        # версия" не помещалась, и построчный поиск не срабатывал никогда.
        for crate, version in re.findall(
            r"`(takt-lang|takt-sim)`.{0,120}?сейчас `(\d+\.\d+\.\d+)`", text
        ):
            manifest = os.path.join(ROOT, crate, "Cargo.toml")
            actual = None
            for manifest_line in read_lines(manifest):
                match = re.match(r'version\s*=\s*"(\d+\.\d+\.\d+)"', manifest_line)
                if match:
                    actual = match.group(1)
                    break
            if actual != version:
                problems.append(
                    (number, f"крейт {crate}: заявлено {version}, в манифесте {actual}")
                )


def check_line_references(lines, problems):
    """Класс 4: ссылка на номер строки — запрещённая форма.

    Проверять её бессмысленно: строка почти всегда существует, а указывает уже
    не туда (замер: `generator/c/mod.rs:150` — символ уехал на 291). Ссылаться
    надо именем символа: `generator/c/mod.rs::map_c_type`.
    """
    for number, text in paragraphs(lines):
        for reference in re.findall(r"`([A-Za-z0-9_\-./]+\.rs:\d+)`", text):
            problems.append(
                (number, f"ссылка на номер строки `{reference}` — форма запрещена, "
                         "ссылайтесь именем символа (`файл.rs::symbol`)")
            )


def check_invariant_checklist(lines, problems):
    """Класс 5: чек-лист критических инвариантов ↔ подробные пункты (фича 0289).

    Раздел «Критические инварианты» — **указатель**: подробный разбор живёт в
    «Технических подводных камнях» (решение фичи 0238). Значит новый
    критический инвариант вносится **дважды**, и до этой проверки забытая
    половина молчала — класс 0084/0193/0195, только списки не в коде, а в
    контексте.

    Связь двусторонняя: номер строки чек-листа ↔ метка
    `` `[критический инвариант N]` `` у подробного пункта.
    """
    listed = set()
    in_checklist = False
    for number, line in enumerate(lines, start=1):
        if line.startswith("### Критические инварианты"):
            in_checklist = True
            continue
        if in_checklist and line.startswith("### "):
            in_checklist = False
        if in_checklist:
            match = re.match(r"(\d+)\.\s+\*\*", line)
            if match:
                listed.add(int(match.group(1)))

    marked = {}
    for number, line in enumerate(lines, start=1):
        for found in re.findall(r"`\[критический инвариант (\d+)\]`", line):
            marked.setdefault(int(found), []).append(number)

    if not listed:
        problems.append((0, "раздел «Критические инварианты» не найден или пуст — "
                            "проверка чек-листа вырождена"))
        return

    for num in sorted(listed - set(marked)):
        problems.append((0, f"инвариант {num} есть в чек-листе, но подробного пункта "
                            f"с меткой `[критический инвариант {num}]` нет"))
    for num in sorted(set(marked) - listed):
        problems.append((marked[num][0], f"пункт помечен `[критический инвариант {num}]`, "
                                         "но строки с этим номером в чек-листе нет"))
    for num, places in sorted(marked.items()):
        if len(places) > 1:
            problems.append((places[1], f"метка `[критический инвариант {num}]` стоит "
                                        f"{len(places)} раз — номер обязан быть один"))


def run_checks(lines):
    statuses = feature_statuses()
    index = build_file_index()
    problems = []
    check_feature_status(lines, statuses, problems)
    check_paths(lines, index, problems)
    check_crate_versions(lines, problems)
    check_line_references(lines, problems)
    check_invariant_checklist(lines, problems)
    return sorted(set(problems))


def self_test():
    """Ловушка обязана срабатывать на подложенном тексте каждого класса.

    Без этой проверки «находок нет» неотличимо от «гейт ничего не ищет».
    """
    statuses = feature_statuses()
    closed = next((f for f, s in statuses.items() if s == "ГОТОВО"), None)
    if closed is None:
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: в реестре нет ни одной закрытой фичи")

    # У каждого случая - Ожидаемая подстрока находки, а не просто "что-то
    # нашлось". Без неё самопроверка бутафорна: класс 5 (чек-лист инвариантов)
    # даёт находку "раздел не найден" на любом коротком образце, и все случаи
    # проходили бы независимо от своего класса. Поймано мутацией при.
    cases = {
        "статус фичи": (
            [f"- **Проба** (фича {closed}, в работе). Текст."],
            "названа незавершённой",
        ),
        "путь": (
            ["- Проба `takt-lang/src/nonexistent_zzz.rs` — файла нет."],
            "не найден в дереве",
        ),
        # Образец повторяет реальную форму записи - с обратными кавычками
        # между именем крейта и версией. Первая редакция самопроверки брала
        # упрощённый образец, зеленела, и мутация настоящей строки CLAUDE.md
        # проходила мимо проверки.
        "версия крейта": (
            ["- Крейт `takt-lang` (`CARGO_PKG_VERSION`, сейчас `0.0.1`) — заведомо не тот."],
            "в манифесте",
        ),
        # та же запись, разорванная переносом. Построчный поиск её
        # пропускал - и заявленная сверка версий не проверяла ничего (замер
        # исправления ). Проверка обязана работать по абзацу.
        "версия крейта через перенос": (
            [
                "- Крейт `takt-lang` (`CARGO_PKG_VERSION`,",
                "  сейчас `0.0.1`) — заведомо не тот.",
            ],
            "в манифесте",
        ),
        # Граница, найденная при 0297: перенос внутри одного токена в
        # обратных кавычках (`takt-lang/src/` ↵ `mod.rs`) сшивкой не лечится -
        # после склейки в пути появляется пробел, и он перестаёт быть путём.
        # Сшивка лечит перенос между элементами записи (имя крейта ... версия),
        # а не внутри имени. Случая на это в самопроверке нет намеренно: он
        # утверждал бы то, чего проверка не делает.
        "номер строки": (
            ["- Проба `semantic/tree.rs:999` — запрещённая форма."],
            "форма запрещена",
        ),
        "инвариант без подробного пункта": (
            ["### Критические инварианты (НЕ нарушать)", "1. **Проба** — пункта с меткой нет."],
            "подробного пункта",
        ),
        "метка без строки чек-листа": (
            [
                "### Критические инварианты (НЕ нарушать)",
                "1. **Проба** — пункт есть.",
                "",
                "- Подробный пункт `[критический инвариант 1]`.",
                "- Лишний пункт `[критический инвариант 9]`.",
            ],
            "но строки с этим номером",
        ),
    }
    for name, (planted, expected) in cases.items():
        found = run_checks(planted)
        if not any(expected in message for _, message in found):
            sys.exit(
                f"САМОПРОВЕРКА ПРОВАЛЕНА: класс «{name}» не сработал "
                f"(ожидалось «{expected}», получено {found})"
            )

    # Обратная сторона: законный текст ложных находок давать не должен.
    clean = [
        "- Проба `semantic/tree.rs` — сокращённая форма, обязана разрешаться.",
        "- Плоских списков задач (`TASKS.md`/`STATUS.md`) нет.",
        # Чек-лист обязателен: без него проверка класса 5 вырождена, и это
        # ошибка - значит законный образец несёт согласованную пару.
        "### Критические инварианты (НЕ нарушать)",
        "1. **Проба** — подробный пункт ниже.",
        "### Прочее",
        "- Подробный пункт `[критический инвариант 1]`.",
    ]
    noise = run_checks(clean)
    if noise:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: законный текст дал ложные находки: {noise}")
    print("  самопроверка гейта: ловушка взведена (7 случаев ловятся, законный текст — нет)")


def main():
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    lines = read_lines(CLAUDE_MD)
    problems = run_checks(lines)
    if problems:
        print("Расхождения в живом контексте CLAUDE.md (фича 0149):", file=sys.stderr)
        for number, message in problems:
            print(f"  CLAUDE.md:{number}: {message}", file=sys.stderr)
        print(
            "\nЖивой контекст читается в начале каждой сессии — ложный факт здесь\n"
            "становится предпосылкой всей работы. Приведите запись в соответствие\n"
            "с реестром фич, деревом исходников и манифестами.",
            file=sys.stderr,
        )
        return 1
    print(f"Живой контекст CLAUDE.md: проверено {len(lines)} строк, расхождений нет.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

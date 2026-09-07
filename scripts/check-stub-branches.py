#!/usr/bin/env python3
"""check-stub-branches.py — заглушка не переживает свою задачу (фича 0217).

Ветвь-заглушка — разобранный узел, отвечающий отказом «пока не поддерживается»
до какой-то будущей работы. Ни один существующий гейт её не видит: гейт
исчерпаемости (0093) требует разбора всех вариантов — он выполнен; реестр
диагностик требует кода — он есть. Дефект класс дал дважды (фиксы 0134-01 и
0134-02: ветвь ссылалась на подзадачу 0134-03 и пережила её на пять месяцев).

Замер при заведении гейта (2026-08-17): формулировку несут ШЕСТЬ мест, и они
делятся пополам. Два — постоянные отказы, названные «пока» по недосмотру
(`ST-016`: переменный `PT` у `TON` — решение ADR 0183; `FM-001`: механизм
отказа форматтера). Два кода — живые заглушки, причём `SIM-013` ссылалась на
фичу 0025, закрытую давно, а `SIM-020` не называла номера вовсе; обе проверены
прогоном: `ref Done: Idle;` и `min(n, 3)` останавливают симулятор, тогда как
цель `c` тот же `min` переводит верно.

Отсюда устройство: различить законное и просроченное машина сама не может —
нужна РАЗМЕТКА. Реестр `scripts/stub-branches.txt` называет держателя каждой
заглушки (фичу, которая её снимет, либо `ПОСТОЯННЫЙ`), а гейт сверяет.

⚠️ Обнаружение идёт по ФОРМУЛИРОВКЕ: заглушка, написанная иначе, гейту не
видна. Это ограничение того же рода, что у греп-сторожей 0168 и 0213, и оно
покрывается правилом: заглушка пишется формулировкой «пока не поддерживается».
"""

import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
REGISTRY = os.path.join(ROOT, "scripts", "stub-branches.txt")
FEATURES_REGISTRY = os.path.join(ROOT, "docs", "features", "README.md")
SOURCE_DIRS = (
    os.path.join("takt-lang", "src"),
    os.path.join("takt-sim", "src"),
)
# Формулировка, которой пишется заглушка (правило `docs/CODE.md`).
MARKER = "пока не поддерж"
# Второй признак заглушки: ссылка на задачу либо фичу в тексте
# диагностики. Замер 2026-08-20 нашёл **четыре** такие ветви в `st_expr.rs`
# , и ни одну
# из них проверка не видел: они написаны не словом "пока", а обещанием работы.
# при этом закрыта - обещание пережило её на год.
#
# Ищется только в строке с кавычкой: упоминание фичи в комментарии законно и
# полезно (им объясняют решение), а в тексте, который читает автор модели, -
# нет: он про фичи проекта не знает.
TASK_REFERENCE = re.compile(r"(задач\w*|подзадач\w*|фич\w*)\s+№?\s*\d{4}")
# Держатель, означающий "отказ постоянный, снимать нечего".
PERMANENT = "ПОСТОЯННЫЙ"
# Статусы, при которых держатель больше не может отвечать за заглушку.
TERMINAL = ("ГОТОВО", "ОТМЕНА")


def read_text(path):
    with open(path, encoding="utf-8") as handle:
        return handle.read()


def normalize_status(cell):
    """Первое слово статуса без украшений: `✅ **ГОТОВО** (тег …)` → `ГОТОВО`."""
    text = re.sub(r"[*✅]", "", cell)
    text = re.sub(r"\(.*", "", text)
    return text.strip().split(" ")[0] if text.strip() else ""


def feature_statuses(text):
    statuses = {}
    for line in text.splitlines():
        if not line.startswith("|"):
            continue
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        match = re.match(r"\[(\d{4})\]", cells[0]) if cells else None
        if match:
            statuses[match.group(1)] = normalize_status(cells[-1])
    return statuses


def parse_registry(text):
    """Строки реестра → список (код, файл, держатель)."""
    entries = []
    for number, line in enumerate(text.splitlines(), start=1):
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        body = stripped.split("#", 1)[0].strip()
        parts = body.split()
        if len(parts) != 3:
            entries.append((number, None, None, None, body))
            continue
        entries.append((number, parts[0], parts[1], parts[2], body))
    return entries


def scan_sources(files):
    """files: путь → текст. Возвращает множество пар (код, файл) для заглушек.

    Код берётся у ближайшего `with_code("…")` ПОСЛЕ маркера: диагностика
    строится сверху вниз (текст, затем код), и в проекте это единственная форма.
    """
    found = set()
    for path, text in files.items():
        # Продолжения строковых литералов Rust склеиваются до поиска: длинный
        # текст диагностики переносится обратным слэшем, и маркер разрывается
        # пополам ("...пока не \" + "поддерживается..."). Первая редакция проверки
        # искала построчно и такую заглушку не видела - поймано на,
        # когда текст отказа `SIM-020` переписали в две строки.
        text = re.sub(r"\\\n\s*", "", text)
        lines = text.splitlines()
        for index, line in enumerate(lines):
            promises_task = '"' in line and TASK_REFERENCE.search(line) is not None
            if MARKER not in line and not promises_task:
                continue
            # Комментарий - не заглушка, а рассказ о ней: док-строка
            # `format/mod.rs` ("Узел АСД пока не поддержан печатью") описывает
            # механизм, а сам отказ строится ниже, в коде. Считая комментарии,
            # проверка нашёл бы место без `with_code` и потребовал регистрировать
            # прозу.
            if line.lstrip().startswith("//"):
                continue
            tail = "\n".join(lines[index : index + 12])
            code = re.search(r'with_code\("([A-Z]{2,3}-\d{3})"\)', tail)
            # Обещание задачи считается заглушкой, только если рядом строится
            # Диагностика (есть `with_code`). Иначе в улов попадают справка CLI
            # ("цель st ...") и комментарии, которые цель печатает в
            # порождённый код, - там ссылка на фичу законна и полезна.
            if code is None and MARKER not in line:
                continue
            found.add((code.group(1) if code else "?", path))
    return found


def run_checks(registry_text, statuses, files):
    problems = []
    entries = parse_registry(registry_text)
    declared = set()
    for number, code, path, holder, body in entries:
        if code is None:
            problems.append(
                (f"scripts/stub-branches.txt:{number}", f"строка не разобрана: «{body}»")
            )
            continue
        declared.add((code, path))
        if holder == PERMANENT:
            continue
        if not re.fullmatch(r"\d{4}", holder):
            problems.append(
                (
                    f"scripts/stub-branches.txt:{number}",
                    f"держатель «{holder}» — не номер фичи и не {PERMANENT}",
                )
            )
            continue
        status = statuses.get(holder)
        if status is None:
            problems.append(
                (
                    f"scripts/stub-branches.txt:{number}",
                    f"фичи {holder} нет в реестре фич — держатель не существует",
                )
            )
        elif status in TERMINAL:
            problems.append(
                (
                    f"scripts/stub-branches.txt:{number}",
                    f"заглушка {code} ({path}) пережила свою задачу: "
                    f"фича {holder} — {status}, а отказ в коде остался",
                )
            )

    found = scan_sources(files)
    for code, path in sorted(found - declared):
        problems.append(
            (path, f"заглушка {code} не объявлена в scripts/stub-branches.txt")
        )
    for code, path in sorted(declared - found):
        problems.append(
            (
                "scripts/stub-branches.txt",
                f"запись {code} {path} ничему не соответствует в коде — "
                f"снимите её (иначе реестр окаменеет)",
            )
        )
    return problems


def collect_sources():
    files = {}
    for directory in SOURCE_DIRS:
        for current, _dirs, names in os.walk(os.path.join(ROOT, directory)):
            for name in names:
                if not name.endswith(".rs"):
                    continue
                full = os.path.join(current, name)
                files[full[len(ROOT) + 1 :]] = read_text(full)
    return files


def self_test():
    """Каждое из трёх условий отказа обязано срабатывать — и молчать на законном."""
    statuses = {"0001": "ГОТОВО", "0002": "СОЗДАНА"}
    src = {"a.rs": 'msg("пока не поддерживается")\n.with_code("XX-001")'}

    clean = "XX-001 a.rs 0002\n"
    if run_checks(clean, statuses, src):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: законная запись даёт находку")

    cases = {
        "держатель закрыт": ("XX-001 a.rs 0001\n", src),
        "заглушка не объявлена": ("", src),
        "запись без места в коде": ("XX-001 a.rs 0002\nYY-002 b.rs 0002\n", src),
        "держатель не существует": ("XX-001 a.rs 9999\n", src),
        "строка не разобрана": ("XX-001 a.rs\n", src),
    }
    for name, (registry, files) in cases.items():
        if not run_checks(registry, statuses, files):
            sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: условие «{name}» не сработало")

    # Постоянный держатель законен и статус не спрашивает.
    if run_checks("XX-001 a.rs ПОСТОЯННЫЙ\n", statuses, src):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: ПОСТОЯННЫЙ даёт находку")

    # Второй признак заглушки - обещание задачи в тексте диагностики.
    promise = {"b.rs": 'msg("печать — часть 2 задачи 0041-04")\n.with_code("YY-002")'}
    if not run_checks("", statuses, promise):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: обещание задачи не поймано")

    # Упоминание фичи вне диагностики законно: так объясняют решение в
    # комментарии и печатают пояснение в порождённый код. Проверка, ловящий и это,
    # был бы снят при первом же неудобстве.
    prose = {"c.rs": '// профиль «часы» (фича 0134)\nlet x = 1;'}
    if run_checks("", statuses, prose):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: упоминание фичи в прозе объявлено заглушкой")

    printed = {"d.rs": 'p.ident("// регистровый интерфейс (фича 0062)");'}
    if run_checks("", statuses, printed):
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: печатаемый комментарий объявлен заглушкой")

    print("Самопроверка check-stub-branches: все условия срабатывают.")


def main():
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    problems = run_checks(
        read_text(REGISTRY), feature_statuses(read_text(FEATURES_REGISTRY)), collect_sources()
    )
    if problems:
        print("Ветви-заглушки разошлись с реестром (фича 0217):", file=sys.stderr)
        for place, message in problems:
            print(f"  {place}: {message}", file=sys.stderr)
        print(
            "\nЗаглушка «пока не поддерживается» обязана быть объявлена в\n"
            "scripts/stub-branches.txt с держателем — фичей, которая её снимет,\n"
            "либо словом ПОСТОЯННЫЙ, если отказ принят как решение. Закрылась\n"
            "фича-держатель — снимите заглушку вместе с записью.",
            file=sys.stderr,
        )
        return 1
    entries = [e for e in parse_registry(read_text(REGISTRY)) if e[1] is not None]
    print(f"Ветви-заглушки: {len(entries)} объявлено, расхождений с кодом нет.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

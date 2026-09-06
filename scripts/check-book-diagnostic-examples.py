#!/usr/bin/env python3
"""check-book-diagnostic-examples.py — разбор приложения «Ошибки» ВОСПРОИЗВОДИТСЯ
(фича 0520).

Приложение `book/src/appendix-errors/index.typ` — место, куда автор программы
приходит с кодом из вывода инструмента. Разбор кода показывает вход и ответ
компилятора; и то, и другое набрано **руками** и живёт отдельно от инструмента.

Гейт 0290 сверяет **множество кодов** приложения с реестром и прямо оговаривает,
что «наличие кода» — не «верность описания». Воспроизводимость примера не
проверял никто, и разбор протухал молча: замер 2026-09-03 (фича 0519) прогнал
91 пару «пример → ожидаемый вывод» и нашёл **две** протухшие — `LE-009`
описывал границу приёма литерала как `i64` (фича 0157 расширила её до `u64`) и
объявлял ошибочной маску, которую сам документ двумя разделами выше называет
законной; пример `CC-023` не срабатывал, потому что неиспользуемую константу
цель отбрасывает до печати.

Гейт берёт из приложения пары «блок `takt` → блок ожидаемого вывода с кодом» и
прогоняет каждую настоящим `taktc`:

- **E1 — обещанный код приходит.** Цель подбирается по префиксу кода (`CC` → `c`
  и `c-hal`, `ST` → `st` и `st-at`, `SV` → `sv` и `sv-mmio`, `RS` → `rust`,
  `PU` → `plantuml`; общие `SE`/`SY`/`LE` — целью `c`, затем `c-hal`), а к ней
  пробуются флаги сборки (`--parameters=specialize`). Пара засчитана, если код
  пришёл хотя бы в одной комбинации.
- **E2 — позиция в цитате верна.** Если цитата несёт префикс `файл:строка:колонка`
  и пара воспроизвелась — координата сверяется с фактической. Класс не
  теоретический: цитата `SV-020` обещала `1:1` при фактических `2:1`.

⚠️ **Не всякий разбор самодостаточен.** Часть примеров намеренно показывает
фрагмент (нет объявлений, цитата снята с более полного файла) — такой вход
отвечает `SE-003`/`SE-102` раньше предмета разбора. Эти пары перечислены в
`scripts/book-diagnostic-examples-baseline.txt` — **узаконенный долг с
ратчетом**: запись оттуда нельзя добавлять, можно только удалять, дополнив
пример до самодостаточного.

⚠️ **Протухшая запись бейзлайна — ошибка.** Пара, которая начала
воспроизводиться, обязана уйти из долга: иначе список растёт молча и перестаёт
быть долгом.

⚠️ Пары с кодами `SIM`, `CS`, `FM` не берутся: их выдают другие инструменты
(симулятор, форматтер), и вход у них другой. Их счёт печатается — молчаливого
пропуска здесь нет.

⚠️ Пустое множество пар — **ошибка**, а не успех: при смене разметки проверка
выполнилась бы тривиально (урок фикса 0202-01).

Корень дерева переопределяется `BDE_ROOT`, компилятор — `TAKTC`: обе нужны
сторожу `scripts/test-book-diagnostic-examples.sh`, который гоняет гейт на копии.

Использование:

    python3 scripts/check-book-diagnostic-examples.py
"""

import os
import re
import shutil
import subprocess
import sys
import tempfile

ROOT = os.environ.get(
    "BDE_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)
APPENDIX = os.path.join(ROOT, "book", "src", "appendix-errors", "index.typ")
# Бейзлайн ищется от корня дерева, а не от каталога скрипта: тест гоняет проверка
# на копии (`BDE_ROOT`), и долг там должен быть свой.
BASELINE = os.path.join(ROOT, "scripts", "book-diagnostic-examples-baseline.txt")

# Компилятор печатает код в скобках `[SE-034]`, симулятор - в круглых после
# текста: `... (SIM-001)`. Разбираем обе формы: предмет один - обещанный код.
CODE_RE = re.compile(r"\[([A-Z]{2,4}-[0-9]{3})\]|\((SIM-[0-9]{3})\)")
# Цитата документа несёт голое имя файла, вывод инструмента - путь прогона:
# позицию ищем одинаково, а имя сводим к basename.
POS_RE = re.compile(r"^(\S+\.takt):(\d+):(\d+): ")

# Цель подбирается по преисправлениеу кода: отказ цели виден только ей самой.
TARGETS = {
    "CC": ("c", "c-hal"),
    "RS": ("rust",),
    "ST": ("st", "st-at"),
    "SV": ("sv", "sv-mmio"),
    "PU": ("plantuml",),
}
COMMON_TARGETS = ("c", "c-hal")
# Инструменты, чей вход этот проверка не строит (форматтер и канон стиля: у них
# свой вызов и свой формат вывода).
FOREIGN = ("CS", "FM")
FLAG_SETS = ((), ("--parameters=specialize",))


def taktc_path():
    """Путь к компилятору: переменная сильнее конфига (урок 0251)."""
    explicit = os.environ.get("TAKTC")
    if explicit:
        return explicit
    target = os.environ.get("CARGO_TARGET_DIR")
    if not target:
        config = os.path.join(ROOT, ".cargo", "config.toml")
        if os.path.exists(config):
            with open(config, encoding="utf-8") as handle:
                found = re.search(
                    r'^\s*target-dir\s*=\s*"([^"]+)"', handle.read(), re.M
                )
            target = found.group(1) if found else None
        target = target or "target"
    if not os.path.isabs(target):
        target = os.path.join(ROOT, target)
    return os.path.join(target, "debug", "taktc")


def pairs(text):
    """Пары «блок takt → блок ожидаемого вывода с кодом», в порядке текста."""
    lines = text.split("\n")
    found = []
    index = 0
    while index < len(lines):
        if lines[index].strip() != "```takt":
            index += 1
            continue
        stop = index + 1
        while stop < len(lines) and lines[stop].strip() != "```":
            stop += 1
        source = "\n".join(lines[index + 1 : stop])
        probe = stop + 1
        while probe < len(lines) and probe < stop + 6 and not lines[probe].strip():
            probe += 1
        if probe < len(lines) and lines[probe].strip() == "```text":
            end = probe + 1
            while end < len(lines) and lines[end].strip() != "```":
                end += 1
            found.append((index + 1, source, "\n".join(lines[probe + 1 : end])))
            index = end + 1
            continue
        index = stop + 1
    return found


def keyed(found):
    """Ключ пары — код и номер вхождения: `SE-090#2` читается без файла."""
    seen = {}
    out = []
    for line, source, expected in found:
        found = codes_in(expected)
        if not found:
            continue
        code = sorted(found)[0]
        if code.split("-")[0] in FOREIGN:
            out.append((None, code, line, source, expected))
            continue
        seen[code] = seen.get(code, 0) + 1
        key = code if seen[code] == 1 else f"{code}#{seen[code]}"
        out.append((key, code, line, source, expected))
    return out


def read_baseline():
    known = {}
    if not os.path.exists(BASELINE):
        return known
    with open(BASELINE, encoding="utf-8") as handle:
        for raw in handle:
            line = raw.strip()
            if not line or line.startswith("#"):
                continue
            key, _, reason = line.partition("|")
            known[key.strip()] = reason.strip()
    return known


def codes_in(text):
    """Коды из текста — обе формы записи."""
    return {compiler or simulator for compiler, simulator in CODE_RE.findall(text)} - {""}


def run_simulator(work, name, source):
    """Прогон эталона: у примеров `SIM` предмет — исполнение, а не перевод."""
    path = os.path.join(work, name)
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(source + "\n")
    simulator = os.path.join(os.path.dirname(taktc_path()), "takt-sim")
    if not os.access(simulator, os.X_OK):
        return ""
    try:
        done = subprocess.run(
            [simulator, path, "-n", "5"], capture_output=True, text=True, timeout=60
        )
    except subprocess.TimeoutExpired:
        return ""
    return done.stdout + done.stderr


def run(taktc, work, name, source, target, flags):
    path = os.path.join(work, name)
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(source + "\n")
    out = os.path.join(work, "out")
    shutil.rmtree(out, ignore_errors=True)
    try:
        done = subprocess.run(
            [taktc, "compile", "-t", target, path, "-o", out, *flags],
            capture_output=True,
            text=True,
            timeout=60,
        )
    except subprocess.TimeoutExpired:
        return ""
    return done.stdout + done.stderr


def attempt(taktc, work, name, source, code):
    """Прогон комбинаций до первой, где обещанный код пришёл."""
    if code.startswith("SIM-"):
        text = run_simulator(work, name, source)
        return (code in codes_in(text)), text, codes_in(text)
    targets = TARGETS.get(code.split("-")[0], COMMON_TARGETS)
    seen = set()
    for target in targets:
        for flags in FLAG_SETS:
            text = run(taktc, work, name, source, target, flags)
            seen |= codes_in(text)
            if f"[{code}]" in text:
                return True, text, seen
    return False, "", seen


def actual_position(text, code):
    for line in text.split("\n"):
        if f"[{code}]" not in line:
            continue
        match = POS_RE.match(line.strip())
        if match:
            return os.path.basename(match.group(1)), match.group(2), match.group(3)
    return None


def expected_position(expected, code):
    for line in expected.split("\n"):
        if f"[{code}]" not in line:
            continue
        match = POS_RE.match(line.strip())
        if match:
            return match.group(1), match.group(2), match.group(3)
    return None


def main():
    if not os.path.exists(APPENDIX):
        print(f"ОШИБКА: приложение не найдено: {APPENDIX}", file=sys.stderr)
        return 1
    taktc = taktc_path()
    if not os.access(taktc, os.X_OK):
        print(f"ОШИБКА: не найден компилятор {taktc}", file=sys.stderr)
        return 1

    with open(APPENDIX, encoding="utf-8") as handle:
        found = keyed(pairs(handle.read()))
    checkable = [item for item in found if item[0] is not None]
    foreign = len(found) - len(checkable)

    if not checkable:
        print(
            "ОШИБКА: в приложении не найдено ни одной пары «пример → вывод»: "
            "проверять нечего (сменилась разметка?)",
            file=sys.stderr,
        )
        return 1

    print("Разбор приложения «Ошибки»: воспроизводимость примеров (фича 0520)...")
    baseline = read_baseline()
    work = tempfile.mkdtemp(prefix="takt_bde_")
    broken, misplaced, stale = [], [], []
    passed = 0
    try:
        for key, code, line, source, expected in checkable:
            wanted = expected_position(expected, code)
            name = wanted[0] if wanted else "model.takt"
            ok, text, seen = attempt(taktc, work, name, source, code)
            if not ok:
                if key in baseline:
                    continue
                got = ", ".join(sorted(seen)) or "нет диагностик"
                broken.append((key, line, got))
                continue
            if key in baseline:
                stale.append((key, line))
                continue
            passed += 1
            if wanted:
                actual = actual_position(text, code)
                if actual and actual[1:] != wanted[1:]:
                    misplaced.append((key, line, wanted, actual))
    finally:
        shutil.rmtree(work, ignore_errors=True)

    if broken:
        print(
            f"  ОШИБКА: разбор обещает код, которого инструмент не даёт "
            f"({len(broken)}):",
            file=sys.stderr,
        )
        for key, line, got in broken:
            print(f"    {key} (строка {line}): получено {got}", file=sys.stderr)
    if misplaced:
        print(
            f"  ОШИБКА: позиция в цитате разошлась с фактической "
            f"({len(misplaced)}):",
            file=sys.stderr,
        )
        for key, line, wanted, actual in misplaced:
            print(
                f"    {key} (строка {line}): в документе "
                f"{wanted[1]}:{wanted[2]}, инструмент даёт {actual[1]}:{actual[2]}",
                file=sys.stderr,
            )
    if stale:
        print(
            f"  ОШИБКА: запись долга протухла — пара воспроизводится ({len(stale)}), "
            f"удалите её из {os.path.basename(BASELINE)}:",
            file=sys.stderr,
        )
        for key, line in stale:
            print(f"    {key} (строка {line})", file=sys.stderr)
    if broken or misplaced or stale:
        return 1

    print(
        f"  Пар проверено: {passed}; узаконенного долга (фрагменты): "
        f"{len(baseline)}; чужих инструментов пропущено: {foreign}."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

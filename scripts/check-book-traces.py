#!/usr/bin/env python3
"""check-book-traces.py — трассы прогонов в главах СВЕРЯЮТСЯ с симулятором
(фича 0522).

Раздел документа показывает поведение модели построчно:

    Шаг 1: [Watching]  kick=1  alarm=0  idle=0
    Шаг 2: [Watching]  kick=0  alarm=0  idle=1

Это самое сильное обещание документа — ради поведения читатель его и открыл, —
и до этого гейта оно жило отдельно от модели: трассы набраны руками и сверялись
глазами. Соседи проверяют другое: 0133 — что пример компилируется и прогоняется,
0274 — снимки порождённого кода, 0513 — ответ инструментов целей, 0520 и 0521 —
разбор и диагностики. Что модель печатает **именно эти** числа, не проверял
никто.

Гейт берёт каждый блок ```` ```text ```` с двумя и более строками «Шаг N»,
находит рядом модель (команда `takt-sim …` в соседнем блоке либо
`#example(read("examples/x.takt"))`) и сценарий (`-s …`), прогоняет и сверяет:

- **состояния** в скобках — если строка документа их показывает;
- **каждую напечатанную пару** `имя=значение`.

⚠️ Сверяется только НАПЕЧАТАННОЕ. Документ вправе показать подмножество
переменных и пропустить шаги (обычный приём: «Шаг 1 … Шаг 97»), поэтому лишние
переменные факта и пропущенные шаги ошибкой не считаются. Ошибка — когда
показанное расходится: другое состояние, другое значение, имя, которого у модели
нет вовсе.

⚠️ Дробное сравнивается с точностью ДОКУМЕНТА: `temperature=16.90` и
`16.9000` — одно значение, а `21.62` сверяется с округлённым `21.6175`.

⚠️ Длина прогона со сценарием равна длине сценария (симулятор останавливается,
когда шаги кончились), поэтому трасса, где событие наступает через десятки тысяч
тактов, воспроизводима только без входов. Такие блоки — в
`scripts/book-traces-baseline.txt`, узаконенный долг с ратчетом; запись, которая
начала сходиться, роняет прогон.

⚠️ Пустое множество трасс — ошибка, а не успех (урок фикса 0202-01).

Корень дерева переопределяется `BTR_ROOT`, симулятор — `TAKT_SIM`: обе нужны
сторожу `scripts/test-book-traces.sh`.

Использование:

    python3 scripts/check-book-traces.py
"""

import glob
import os
import re
import subprocess
import sys

ROOT = os.environ.get(
    "BTR_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)
CHAPTERS = os.path.join(ROOT, "book", "src")
BASELINE = os.path.join(ROOT, "scripts", "book-traces-baseline.txt")

EXCLUDED = ("appendix-",)
# "Шаг 12 ( 3ms): [A, B] in:x=1 vars:y=2" - время и преисправления вида `vars:`
# принадлежат печати симулятора, документ их опускает.
STEP_RE = re.compile(r"^\s*Шаг\s+(\d+)\s*(?:\([^)]*\))?\s*:\s*(.*)$")
STATES_RE = re.compile(r"\s*\[([^\]]*)\]\s*(.*)$")
PAIR_RE = re.compile(r"(?:\w+:)?([A-Za-z_]\w*)=(\[[^\]]*\]|\S+)")
SIM_RE = re.compile(r"takt-sim\s+(\S+\.takt)((?:\s+-\w+\s+\S+)*)")
EXAMPLE_RE = re.compile(r'#example\(read\("([^"]+)"\)\)')
LOOKBACK = 40


def simulator_path():
    explicit = os.environ.get("TAKT_SIM")
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
    return os.path.join(target, "debug", "takt-sim")


def split_line(text):
    """Строка трассы → (состояния | None, {имя: значение})."""
    states = None
    rest = text
    match = STATES_RE.match(text)
    if match:
        states = [item.strip() for item in match.group(1).split(",")]
        rest = match.group(2)
    return states, dict(PAIR_RE.findall(rest))


def same_value(shown, actual):
    """Число сравнивается с точностью документа, прочее — как текст."""
    if shown == actual:
        return True
    try:
        wanted = float(shown)
        got = float(actual)
    except ValueError:
        return False
    digits = len(shown.split(".")[1]) if "." in shown else 0
    return round(got, digits) == round(wanted, digits)


def traces(text):
    """Блоки трасс: (строка начала, [(шаг, содержимое)], модель, сценарий)."""
    lines = text.split("\n")
    found = []
    index = 0
    while index < len(lines):
        if lines[index].strip() != "```text":
            index += 1
            continue
        stop = index + 1
        body = []
        while stop < len(lines) and lines[stop].strip() != "```":
            body.append(lines[stop])
            stop += 1
        steps = []
        for raw in body:
            match = STEP_RE.match(raw)
            if match:
                steps.append((int(match.group(1)), match.group(2)))
        if len(steps) >= 2:
            model = scenario = None
            for back in range(index - 1, max(-1, index - LOOKBACK), -1):
                if model is None:
                    call = SIM_RE.search(lines[back])
                    if call:
                        model = call.group(1)
                        flag = re.search(r"-s\s+(\S+)", call.group(2))
                        scenario = flag.group(1) if flag else None
                if model is None:
                    example = EXAMPLE_RE.search(lines[back])
                    if example:
                        model = example.group(1)
            found.append((index + 1, steps, model, scenario))
        index = stop + 1
    return found


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


def run(simulator, path, steps, scenario):
    command = [simulator, path, "-n", str(steps)]
    if scenario:
        command += ["-s", scenario]
    try:
        done = subprocess.run(command, capture_output=True, text=True, timeout=300)
    except subprocess.TimeoutExpired:
        return {}, "прогон не уложился в 300 с"
    fact = {}
    for line in (done.stdout + done.stderr).split("\n"):
        match = STEP_RE.match(line)
        if match:
            fact[int(match.group(1))] = split_line(match.group(2))
    if not fact:
        return {}, (done.stdout + done.stderr).strip().split("\n")[0][:120]
    return fact, None


def compare(steps, fact):
    problems = []
    for step, text in steps:
        shown_states, shown_pairs = split_line(text)
        if step not in fact:
            problems.append(f"шага {step} в прогоне нет")
            continue
        actual_states, actual_pairs = fact[step]
        if shown_states is not None and actual_states is not None:
            if shown_states != actual_states:
                problems.append(
                    f"шаг {step}: состояния {shown_states} против {actual_states}"
                )
        for name, value in shown_pairs.items():
            if name not in actual_pairs:
                problems.append(f"шаг {step}: переменной '{name}' у модели нет")
            elif not same_value(value, actual_pairs[name]):
                problems.append(
                    f"шаг {step}: {name}={value} против {actual_pairs[name]}"
                )
    return problems


def main():
    simulator = simulator_path()
    if not os.access(simulator, os.X_OK):
        print(f"ОШИБКА: не найден симулятор {simulator}", file=sys.stderr)
        return 1

    files = [
        path
        for path in sorted(glob.glob(os.path.join(CHAPTERS, "**", "*.typ"), recursive=True))
        if not any(part in path for part in EXCLUDED)
    ]
    print("Трассы прогонов в главах book/ (фича 0522)...")
    baseline = read_baseline()
    checked = 0
    total = 0
    broken, stale = [], []
    for path in files:
        rel = os.path.relpath(path, CHAPTERS)
        directory = os.path.dirname(path)
        with open(path, encoding="utf-8") as handle:
            found = traces(handle.read())
        for number, (line, steps, model, scenario) in enumerate(found, 1):
            total += 1
            key = f"{rel}#{number}"
            reason = None
            model_path = None
            if not model:
                reason = "рядом с трассой не названы ни модель, ни команда прогона"
            else:
                model_path = os.path.join(
                    directory, "examples", os.path.basename(model)
                )
                if not os.path.exists(model_path):
                    reason = f"файл модели не найден: {os.path.basename(model)}"
            problems = [reason] if reason else []
            if not problems:
                scenario_path = (
                    os.path.join(directory, "examples", os.path.basename(scenario))
                    if scenario
                    else None
                )
                if scenario_path and not os.path.exists(scenario_path):
                    problems = [f"сценарий не найден: {os.path.basename(scenario)}"]
                else:
                    fact, failure = run(
                        simulator,
                        model_path,
                        max(step for step, _ in steps),
                        scenario_path,
                    )
                    problems = [failure] if failure else compare(steps, fact)
            if not problems and key in baseline:
                stale.append((key, line))
            elif not problems:
                checked += 1
            elif key not in baseline:
                broken.append((key, line, problems))

    if not total:
        print(
            "ОШИБКА: в главах не найдено ни одной трассы: проверять нечего "
            "(сменилась разметка?)",
            file=sys.stderr,
        )
        return 1

    if broken:
        print(
            f"  ОШИБКА: трасса расходится с прогоном ({len(broken)}):", file=sys.stderr
        )
        for key, line, problems in broken:
            print(f"    {key} (строка {line}):", file=sys.stderr)
            for problem in problems[:6]:
                print(f"      {problem}", file=sys.stderr)
    if stale:
        print(
            f"  ОШИБКА: запись долга протухла — трасса сходится ({len(stale)}), "
            f"удалите её из {os.path.basename(BASELINE)}:",
            file=sys.stderr,
        )
        for key, line in stale:
            print(f"    {key} (строка {line})", file=sys.stderr)
    if broken or stale:
        return 1

    print(f"  Трасс в главах: {total}; сверено с прогоном: {checked}; долга: {len(baseline)}.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

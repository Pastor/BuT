#!/usr/bin/env python3
"""check-book-chapter-examples.py — примеры В ГЛАВАХ разбираются, а обещанная
ошибка приходит (фича 0521).

Примеры глав живут в двух видах. Файл `examples/*.takt`, вставленный
`#example(read(…))`, гоняют гейты 0133 (компиляция и симуляция), 0274 (снимки) и
0513 (инструменты целей). А **инлайн-блок** ```` ```takt ````, набранный прямо в
тексте раздела, не проверял никто: его никто не компилирует и не разбирает.

Замер 2026-09-03 (156 блоков в главах) нашёл два примера, которые не
разбираются вовсе: поле `next: Node` в разборе `SE-124` и функция `fn next(…)`
в разделе о стиле — `next` ключевое слово, и читатель, скопировавший пример,
получает `SY-002` вместо обещанного. Оба исправлены, класс закрыт этим гейтом.

Проверки:

- **Ч1 — блок разбирается.** Текст прогоняется через `taktc fmt --stdin` (только
  разбор, без семантики) в трёх формах: как есть, как тело состояния и как тело
  блока. Фрагмент законен — незаконен фрагмент, который не разбирается ни в
  одной из них.
- **Ч2 — названный код приходит.** Блок, чей комментарий называет код
  диагностики (`// ошибка SE-124: …`), обязан этот код дать: те же формы плюс
  добавленный автомат, цели `c` и `c-hal`.

Не берутся (счёт печатается, молчаливого пропуска нет):

- **скелеты с многоточием** (`model Cabin { … }`) — это не программа, а форма
  записи;
- **перечни литералов** (`42`, `"привет"`) — в блоке нет ни `;`, ни `{`;
- **блоки, обещающие ошибку без кода** — проверять нечего: что именно обещано,
  знает только человек.

⚠️ Приложение «Ошибки» и «Порождённый код примера» исключены: первое держит гейт
0520 (там у примера есть цитата ответа), второе набрано не на Takt.

⚠️ Невоспроизводимые по устройству блоки перечислены в
`scripts/book-chapter-examples-baseline.txt` — узаконенный долг с ратчетом:
запись оттуда можно только удалять. Запись, которая начала проходить, роняет
прогон: иначе долг растёт молча.

⚠️ Пустое множество блоков — ошибка, а не успех (урок фикса 0202-01).

Корень дерева переопределяется `BCE_ROOT`, компилятор — `TAKTC`: обе нужны
сторожу `scripts/test-book-chapter-examples.sh`.

Использование:

    python3 scripts/check-book-chapter-examples.py
"""

import glob
import os
import re
import shutil
import subprocess
import sys
import tempfile

ROOT = os.environ.get(
    "BCE_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)
CHAPTERS = os.path.join(ROOT, "book", "src")
BASELINE = os.path.join(ROOT, "scripts", "book-chapter-examples-baseline.txt")

# Приложение "Ошибки" держит проверка 0520, "Порождённый код" набран не на Takt.
EXCLUDED = ("appendix-errors", "appendix-generated")

CODE_RE = re.compile(r"\b([A-Z]{2,3}-[0-9]{3})\b")
COMMENT_RE = re.compile(r"//.*")
PROMISE_RE = re.compile(r"\b(ОШИБКА|ошибка|ошибочна|ошибочно)\b")
START_RE = re.compile(r"(^|\n)\s*start\s+\w")


def taktc_path():
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


def blocks(text):
    """Инлайн-блоки ```takt в порядке текста: (строка, исходник)."""
    lines = text.split("\n")
    out = []
    index = 0
    while index < len(lines):
        if lines[index].strip() != "```takt":
            index += 1
            continue
        stop = index + 1
        while stop < len(lines) and lines[stop].strip() != "```":
            stop += 1
        out.append((index + 1, "\n".join(lines[index + 1 : stop])))
        index = stop + 1
    return out


def forms(source):
    """Формы обвязки: фрагмент бывает телом состояния или телом блока."""
    yield source
    yield "start Probe {\n" + source + "\n}"
    yield "start Probe {\n    always {\n" + source + "\n    }\n    ref Probe;\n}"


def parses(taktc, source):
    done = subprocess.run(
        [taktc, "fmt", "--stdin"],
        input=source + "\n",
        capture_output=True,
        text=True,
        timeout=60,
    )
    return done.returncode == 0, (done.stdout + done.stderr)


def compiles(taktc, work, source, target):
    path = os.path.join(work, "model.takt")
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(source + "\n")
    out = os.path.join(work, "out")
    shutil.rmtree(out, ignore_errors=True)
    try:
        done = subprocess.run(
            [taktc, "compile", "-t", target, path, "-o", out],
            capture_output=True,
            text=True,
            timeout=60,
        )
    except subprocess.TimeoutExpired:
        return set()
    return set(CODE_RE.findall(done.stdout + done.stderr))


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


def chapter_files():
    found = []
    for path in sorted(glob.glob(os.path.join(CHAPTERS, "**", "*.typ"), recursive=True)):
        if any(part in path for part in EXCLUDED):
            continue
        found.append(path)
    return found


def main():
    taktc = taktc_path()
    if not os.access(taktc, os.X_OK):
        print(f"ОШИБКА: не найден компилятор {taktc}", file=sys.stderr)
        return 1
    files = chapter_files()
    if not files:
        print(f"ОШИБКА: разделы не найдены: {CHAPTERS}", file=sys.stderr)
        return 1

    print("Разбор примеров в главах book/ (фича 0521)...")
    baseline = read_baseline()
    work = tempfile.mkdtemp(prefix="takt_bce_")
    unparsed, unpromised, stale = [], [], []
    parsed = promised = 0
    skipped = {"скелет": 0, "перечень": 0, "обещание без кода": 0}
    total = 0
    try:
        for path in files:
            rel = os.path.relpath(path, CHAPTERS)
            with open(path, encoding="utf-8") as handle:
                found = blocks(handle.read())
            for number, (line, source) in enumerate(found, 1):
                total += 1
                key = f"{rel}#{number}"
                if "…" in source:
                    skipped["скелет"] += 1
                    continue
                if ";" not in source and "{" not in source:
                    skipped["перечень"] += 1
                    continue
                comments = " ".join(COMMENT_RE.findall(source))
                wanted = CODE_RE.findall(comments)
                if wanted:
                    seen = set()
                    hit = False
                    for form in (*forms(source), source + "\n\nstart Probe { ref Probe; }"):
                        for target in ("c", "c-hal"):
                            seen |= compiles(taktc, work, form, target)
                            if wanted[0] in seen:
                                hit = True
                                break
                        if hit:
                            break
                    if hit and key in baseline:
                        stale.append((key, line))
                    elif hit:
                        promised += 1
                    elif key not in baseline:
                        got = ", ".join(sorted(seen)) or "нет диагностик"
                        unpromised.append((key, line, wanted[0], got))
                    continue
                if PROMISE_RE.search(comments):
                    skipped["обещание без кода"] += 1
                    continue
                ok = False
                last = ""
                for form in forms(source):
                    good, text = parses(taktc, form)
                    if good:
                        ok = True
                        break
                    last = text.strip().split("\n")[0]
                if ok and key in baseline:
                    stale.append((key, line))
                elif ok:
                    parsed += 1
                elif key not in baseline:
                    unparsed.append((key, line, last[:120]))
    finally:
        shutil.rmtree(work, ignore_errors=True)

    if not total:
        print(
            "ОШИБКА: в главах не найдено ни одного блока `takt`: проверять нечего "
            "(сменилась разметка?)",
            file=sys.stderr,
        )
        return 1

    if unparsed:
        print(f"  ОШИБКА: пример не разбирается ({len(unparsed)}):", file=sys.stderr)
        for key, line, text in unparsed:
            print(f"    {key} (строка {line}): {text}", file=sys.stderr)
    if unpromised:
        print(
            f"  ОШИБКА: пример обещает код, которого инструмент не даёт "
            f"({len(unpromised)}):",
            file=sys.stderr,
        )
        for key, line, want, got in unpromised:
            print(
                f"    {key} (строка {line}): обещан {want}, получено {got}",
                file=sys.stderr,
            )
    if stale:
        print(
            f"  ОШИБКА: запись долга протухла — пример проходит ({len(stale)}), "
            f"удалите её из {os.path.basename(BASELINE)}:",
            file=sys.stderr,
        )
        for key, line in stale:
            print(f"    {key} (строка {line})", file=sys.stderr)
    if unparsed or unpromised or stale:
        return 1

    print(
        f"  Блоков в главах: {total}; разобрано: {parsed}; обещаний сверено: "
        f"{promised}; долга: {len(baseline)}."
    )
    print(
        "  Не берутся: скелетов с многоточием "
        f"{skipped['скелет']}, перечней литералов {skipped['перечень']}, "
        f"обещаний без кода {skipped['обещание без кода']}."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

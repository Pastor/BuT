#!/usr/bin/env python3
"""check-book-flags.py — ключ сборки НАЗВАН в документе (фича 0527).

Флаг командной строки меняет то, что получит железо: `--bounds-check` заводит
проверку индекса и порт `bounds_fault`, `--float-embedded` заменяет вещественную
арифметику целочисленной, `--guard-disable` убирает из вывода проверки свойств.
Документ описывал **поведение** («если сборка попросила проверку границ»), но не
говорил, **чем** попросить: замер 2026-09-03 нашёл шесть флагов, не названных
нигде, и два — названных только в приложении «Ошибки» как условие диагностики.

Гейт берёт флаги **из разбора аргументов** (`takt-lang/src/compile_cli/`,
`takt-lang/src/bin/taktc.rs`) — то есть из единственного места, где они на самом
деле объявлены, — и требует, чтобы каждый был назван хоть раз в `book/src/`.

⚠️ Список флагов НЕ повторяется в гейте: второй носитель разошёлся бы с первым
(класс 0084/0193/0195). Он вычитывается из исходников по образцу разбора.

⚠️ Служебные и намеренно не описанные ключи живут в
`scripts/book-flags-baseline.txt` — узаконенный долг с ратчетом: запись можно
только удалять. Запись, которая начала находиться в документе, роняет прогон:
иначе долг растёт молча.

⚠️ Пустое множество флагов — ошибка, а не успех: сменился разбор аргументов, и
проверка выродилась бы в тривиальный успех (урок фикса 0202-01).

Корень дерева переопределяется `BF_ROOT` — для сторожа
`scripts/test-book-flags.sh`.

Использование:

    python3 scripts/check-book-flags.py
"""

import glob
import os
import re
import sys

ROOT = os.environ.get(
    "BF_ROOT", os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
)
SOURCES = (
    os.path.join(ROOT, "takt-lang", "src", "compile_cli"),
    os.path.join(ROOT, "takt-lang", "src", "bin"),
)
CHAPTERS = os.path.join(ROOT, "book", "src")
BASELINE = os.path.join(ROOT, "scripts", "book-flags-baseline.txt")

# Флаг объявлен там, где разбирается: `"--flag"` в match и `"--flag="` в
# starts_with. Комментарии отброшены - в них флаги упоминаются пояснением.
FLAG_RE = re.compile(r'"(--[a-z][a-z0-9-]*)=?"')
COMMENT_RE = re.compile(r"^\s*//")


def declared_flags():
    found = set()
    for directory in SOURCES:
        for path in sorted(glob.glob(os.path.join(directory, "*.rs"))):
            if path.endswith("tests.rs"):
                continue
            with open(path, encoding="utf-8") as handle:
                for line in handle:
                    if COMMENT_RE.match(line):
                        continue
                    found.update(FLAG_RE.findall(line))
    return found


def documented_flags():
    text = []
    for path in sorted(glob.glob(os.path.join(CHAPTERS, "**", "*.typ"), recursive=True)):
        with open(path, encoding="utf-8") as handle:
            text.append(handle.read())
    return "\n".join(text)


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


def main():
    flags = declared_flags()
    if not flags:
        print(
            "ОШИБКА: в разборе аргументов не найдено ни одного флага: проверять "
            "нечего (сменилось устройство CLI?)",
            file=sys.stderr,
        )
        return 1
    document = documented_flags()
    if not document:
        print(f"ОШИБКА: разделы документа не найдены: {CHAPTERS}", file=sys.stderr)
        return 1

    print("Ключи сборки против документа (фича 0527)...")
    baseline = read_baseline()
    missing, stale = [], []
    named = 0
    for flag in sorted(flags):
        # Слово целиком: `--float-as-q` не должен засчитываться за `--float-as-q=m.n`
        # соседа, а `--guard-enable` - за `--guard-enabled`.
        found = re.search(re.escape(flag) + r"(?![a-z0-9-])", document) is not None
        if found and flag in baseline:
            stale.append(flag)
        elif found:
            named += 1
        elif flag not in baseline:
            missing.append(flag)

    if missing:
        print(
            f"  ОШИБКА: ключ сборки не назван в документе ({len(missing)}):",
            file=sys.stderr,
        )
        for flag in missing:
            print(f"    {flag}", file=sys.stderr)
    if stale:
        print(
            f"  ОШИБКА: запись долга протухла — ключ назван ({len(stale)}), "
            f"удалите её из {os.path.basename(BASELINE)}:",
            file=sys.stderr,
        )
        for flag in stale:
            print(f"    {flag}", file=sys.stderr)
    if missing or stale:
        return 1

    print(f"  Ключей в CLI: {len(flags)}; названо в документе: {named}; долга: {len(baseline)}.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

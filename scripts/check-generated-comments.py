#!/usr/bin/env python3
"""Гейт комментариев порождённого кода (фича 0535, задача 0535-01).

Читателю вывода компилятор не рассказывает о своём устройстве. Гейт собирает
корпус `examples/*.takt` всеми целями и требует, чтобы в выводе не было
**сочинённого генератором** текста:

* `G1` — адрес решения проекта: `фича NNNN`, `ADR NNNN`, путь `scripts/…`,
  слово `precheck`. У читателя порождённого файла нет ни карточек фич, ни
  `docs/`, ни предкоммита: ссылка обещает документ, которого он не найдёт.
* `G2` — служебный маркер `NOTICE:`. Не предупреждение, не `TODO`, не
  соглашение целевого языка — слово без значения.
* `G3` — пустая строка комментария (`//`, `///`, `*` без текста). У цели `rust`
  пустой `///` — ещё и пустая doc-строка, которую `cargo doc` покажет абзацем.
* `G4` — doc-строка (`///`) в выводе цели `rust`: справку публичного API
  генератор не сочиняет (решение заказчика 2026-09-06). ⚠️ Правило переживёт
  задачу `0535-04`: авторский комментарий переносится формой `//`, а не `///`.

⚠️ Гейт **положительно** проверяет и то, что осталось: шапка «Порождено
компилятором» обязана быть у целей, которые её печатают. Запрет без контроля
доказывал бы лишь, что вывод пуст.

⚠️ Границы правила названы: комментарии АВТОРА модели гейт не судит — их
перенос заводит задача `0535-04`, и тогда запрет `G1`…`G3` будет применяться
только к тексту, который печатает генератор.

Самопроверка (правило 0315) — `scripts/test-check-generated-comments.sh`.
Переменная `GC_ROOT` переопределяет корень дерева (сторож гоняет гейт на копии),
`TAKTC` — путь к компилятору.
"""

import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

TARGETS = ["c", "c-hal", "st", "st-at", "rust", "sv", "sv-mmio", "plantuml"]

# Строка вывода, начинающаяся с комментария, либо комментарий в хвосте строки.
ADDRESS = re.compile(r"(фича\s+\d{4}|ADR\s+\d{4}|scripts/[\w.-]+|precheck)")
NOTICE = re.compile(r"NOTICE")
EMPTY = re.compile(r"^\s*(///|//|\*|\(\*\s*\*\))\s*$")
RUST_DOC = re.compile(r"^\s*///")
# Шапка: единственный текст, который генератор пишет от себя (задача 0535-02
# переработает её форму, но не факт существования).
HEADER = "Порождено компилятором"


def build_corpus(root: Path, taktc: Path, out: Path) -> list[str]:
    """Собирает корпус всеми целями; возвращает список отказов запуска."""
    failures = []
    examples = sorted((root / "examples").glob("*.takt"))
    if not examples:
        failures.append("в дереве нет примеров examples/*.takt")
    for target in TARGETS:
        for path in examples:
            run = subprocess.run(
                [str(taktc), "compile", "-t", target, str(path), "-o", str(out / target)],
                capture_output=True,
                text=True,
            )
            # Отказ цели — её названная граница (например `SV-002`), а не дефект
            # гейта: такие примеры просто не дают файлов.
            if run.returncode not in (0, 1):
                failures.append(f"{taktc.name} {target} {path.name}: код {run.returncode}")
    return failures


def main() -> int:
    root = Path(os.environ.get("GC_ROOT", Path(__file__).resolve().parent.parent))
    taktc = Path(os.environ.get("TAKTC", root / "target" / "precheck" / "debug" / "taktc"))
    if not taktc.is_file():
        print(f"  пропуск: компилятор не собран ({taktc})")
        return 1 if os.environ.get("PRECHECK_STRICT") else 0

    errors: list[str] = []
    with tempfile.TemporaryDirectory(prefix="takt_gc_") as tmp:
        out = Path(tmp)
        errors.extend(build_corpus(root, taktc, out))

        files = [p for p in out.rglob("*") if p.is_file()]
        if not files:
            # Вырожденный вход: гейт, молчащий на пустом корпусе, ничего не
            # прочёл (урок задачи 0531-07a).
            print("ОШИБКА: корпус не собран — судить нечего", file=sys.stderr)
            return 1

        headers = 0
        for path in sorted(files):
            rel = path.relative_to(out)
            text = path.read_text(encoding="utf-8", errors="replace")
            if HEADER in text:
                headers += 1
            for no, line in enumerate(text.splitlines(), 1):
                if HEADER in line:
                    continue
                # Судится только текст комментария: `фича` в имени переменной
                # модели — дело автора, а не генератора.
                comment = comment_part(line, path.suffix)
                if comment is None:
                    continue
                if ADDRESS.search(comment):
                    errors.append(f"G1 {rel}:{no}: адрес решения проекта — {comment.strip()}")
                if NOTICE.search(comment):
                    errors.append(f"G2 {rel}:{no}: служебный маркер — {comment.strip()}")
                if EMPTY.match(line):
                    errors.append(f"G3 {rel}:{no}: пустой комментарий")
                if path.suffix == ".rs" and RUST_DOC.match(line):
                    errors.append(f"G4 {rel}:{no}: doc-строка сочинена генератором")

        if headers == 0:
            errors.append("контроль: шапки «Порождено компилятором» нет ни в одном файле")

    if errors:
        print("Гейт комментариев порождённого кода (0535): нарушения", file=sys.stderr)
        for line in errors:
            print(f"  {line}", file=sys.stderr)
        return 1

    print(f"  комментарии вывода: {len(files)} файлов, сочинённого нет; шапок {headers}")
    return 0


def comment_part(line: str, suffix: str) -> str | None:
    """Текст комментария в строке вывода или `None`, если его там нет.

    ⚠️ У цели `c` последовательность `(*` встречается в КОДЕ — вызов по
    указателю (`(*model->write_bit)(…)`), — поэтому форма `(* … *)` считается
    комментарием только в файлах ST.
    """
    if suffix == ".st":
        start = line.find("(*")
        return line[start:] if start >= 0 else None
    for mark in ("//", "/*"):
        start = line.find(mark)
        if start >= 0:
            return line[start:]
    return None


if __name__ == "__main__":
    sys.exit(main())

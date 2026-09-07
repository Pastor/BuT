#!/usr/bin/env python3
"""Гейт КАЧЕСТВА диагностик компилятора (фича 0467).

Реестр 0077 сверяет коды с исходниками: код, который эмитируется, обязан быть
в реестре, и наоборот. Чего он не проверяет — **что видит автор модели**:
достижим ли код вообще и отвечает ли сообщение правилам проекта.

Гейт прогоняет корпус `.takt` через все восемь целей и судит каждую
напечатанную диагностику:

  D1 — у диагностики есть КОД (`[SE-036]`): без кода её не найти ни в
       реестре, ни в приложении документа;
  D2 — у диагностики есть ПОЗИЦИЯ (`файл:строка:колонка`) либо её код стоит
       в реестре долга: автор обязан знать, где именно ошибка;
  D3 — текст на РУССКОМ: язык сообщений — свойство инструмента, а не автора
       строки (исключения — тот же реестр долга);
  D4 — в тексте нет ВНУТРЕННЕГО ПРЕДСТАВЛЕНИЯ (`Debug`-дамп узла): правило
       0231, здесь проверенное сплошь, а не на фикстурах;
  D5 — ПОКРЫТИЕ: код, которого не дал ни один вход корпуса, обязан стоять в
       реестре недостижимых — иначе неизвестно, работает ли он вообще.

⚠️ Область — `taktc compile`. Коды эталона (`SIM`), стиля (`CS`) и форматтера
(`FM`) сюда не входят: их печатают другие инструменты, и корпус входов у них
свой. Граница названа, а не забыта: реестр покрытия эти префиксы не считает.
"""

import os
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
REGISTRY = ROOT / "docs/diagnostics/README.md"
# Пути переопределяются переменными окружения - так проверка гоняет его Тест
# на копии дерева прогон стоил бы полного корпуса, а предмет
# проверки - сами правила, и им довольно трёх входов.
POSITION_DEBT = Path(
    os.environ.get("TAKT_DIAG_POSITION_DEBT", ROOT / "scripts/diagnostic-position-baseline.txt")
)
COVERAGE_DEBT = Path(
    os.environ.get("TAKT_DIAG_COVERAGE_DEBT", ROOT / "scripts/diagnostic-coverage-baseline.txt")
)

# Префикса, которые печатает `taktc compile`. Прочие - область других
# инструментов (см. шапку).
COMPILER_PREFIXES = ("LE", "SY", "SE", "CC", "RS", "SV", "ST", "AM", "DF", "PU")

TARGETS = ["c", "c-hal", "st", "st-at", "rust", "sv", "sv-mmio", "plantuml"]

# Маркеры внутреннего представления в тексте.
DEBUG_MARKERS = (
    "Node {",
    "Node(",
    "ExpressionNode::",
    "StatementNode::",
    "TypeNode::",
    "Some(",
    "None)",
    "RefCell",
)

DIAGNOSTIC = re.compile(r"\[([A-Z]{2,3}-\d{3})\]:\s*(.*)$")
POSITION = re.compile(r"^\S+:\d+:\d+:")
# Заметка печатается своей строкой, и координата в ней стоит после слова
# "примечание" (`  примечание: 62:13: причина [CC-021]: ...`). Судить её общей
# регуляркой нельзя: та требует позицию в начале строки и объявляла бы
# безадресной заметку, которая место как раз называет.
NOTE = re.compile(r"^\s*примечание:\s*")
NOTE_POSITION = re.compile(r"^\s*примечание:\s*(\S+:)?\d+:\d+:")


def fail(message):
    print(f"ОТКАЗ: {message}")
    sys.exit(1)


def read_debt(path):
    """Реестр долга: код → причина."""
    debt = {}
    if not path.is_file():
        return debt
    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        code, _, reason = line.partition("#")
        debt[code.strip()] = reason.strip()
    return debt


def registry_codes():
    text = REGISTRY.read_text(encoding="utf-8")
    codes = set(re.findall(r"`([A-Z]{2,3}-\d{3})`", text))
    return {c for c in codes if c.split("-")[0] in COMPILER_PREFIXES}


def corpus():
    """Входы корпуса; `TAKT_DIAG_CORPUS` подменяет их для сторожа гейта."""
    override = os.environ.get("TAKT_DIAG_CORPUS")
    if override:
        return sorted(Path(override).rglob("*.takt"))
    files = []
    for base in ("takt-lang/tests/data", "examples", "book/src"):
        root = ROOT / base
        if root.is_dir():
            files.extend(sorted(root.rglob("*.takt")))
    return files


def taktc():
    """Бинарник компилятора: собранный предкоммитом либо обычный `target`."""
    for candidate in ("target/precheck/debug/taktc", "target/debug/taktc"):
        path = ROOT / candidate
        if path.is_file():
            return path
    fail("бинарник taktc не собран — соберите `cargo build --bin taktc`")


def main():
    binary = taktc()
    position_debt = read_debt(POSITION_DEBT)
    coverage_debt = read_debt(COVERAGE_DEBT)
    out_dir = Path(os.environ.get("TMPDIR", "/tmp")) / "takt_diag_quality"

    seen = set()
    # Коды, встреченные без позиции: по ним судится, не протухла ли запись
    # долга. Без этой половины долг мог бы только расти - записи о
    # починенных кодах оставались бы навсегда.
    seen_without_position = set()
    problems = []
    checked = 0
    for source in corpus():
        for target in TARGETS:
            run = subprocess.run(
                [
                    str(binary),
                    "compile",
                    "-t",
                    target,
                    str(source),
                    "-o",
                    str(out_dir),
                ],
                capture_output=True,
                text=True,
            )
            for line in (run.stderr + run.stdout).splitlines():
                found = DIAGNOSTIC.search(line)
                if not found:
                    # D1: строка отказа без кода - "безликая диагностика".
                    if line.startswith("Ошибка компиляции") or line.startswith(
                        "Предупреждение"
                    ):
                        problems.append(f"D1 {source.name}/{target}: {line[:100]}")
                    continue
                code, text = found.group(1), found.group(2)
                if code.split("-")[0] not in COMPILER_PREFIXES:
                    continue
                seen.add(code)
                checked += 1
                where = f"{code} ({source.name}, цель {target})"
                has_position = (
                    NOTE_POSITION.match(line)
                    if NOTE.match(line)
                    else POSITION.match(line)
                )
                if not has_position:
                    seen_without_position.add(code)
                    if code not in position_debt:
                        problems.append(f"D2 {where}: позиции нет — {text[:70]}")
                if not re.search(r"[а-яА-ЯёЁ]", text) and code not in position_debt:
                    problems.append(f"D3 {where}: текст не на русском — {text[:70]}")
                for marker in DEBUG_MARKERS:
                    if marker in text:
                        problems.append(
                            f"D4 {where}: внутреннее представление '{marker}' — {text[:70]}"
                        )
                        break

    # Режим отчёта: печатает достигнутые коды и выходит. Нужен Тесту проверки
    # - иначе он строил бы реестры разбором отказа, то есть
    # проверял бы проверка его же выводом.
    if "--emit-reached" in sys.argv:
        for code in sorted(seen):
            print(code)
        return

    # D5: покрытие реестра корпусом.
    all_codes = registry_codes()
    unreached = sorted(all_codes - seen)
    for code in unreached:
        if code not in coverage_debt:
            problems.append(
                f"D5 {code}: код в реестре, но ни один вход корпуса его не даёт "
                f"и в реестре недостижимых его нет"
            )
    # D2: запись долга позиций, чей код корпус даёт всегда с координатой.
    for code in sorted(position_debt):
        if code in seen and code not in seen_without_position:
            problems.append(
                f"D2 {code}: записан как «без позиции», но корпус печатает его "
                f"с координатой — запись протухла"
            )

    stale = sorted(code for code in coverage_debt if code in seen)
    for code in stale:
        problems.append(
            f"D5 {code}: записан недостижимым, но корпус его даёт — запись протухла"
        )
    # Запись долга позиций, чей код корпус больше не даёт, проверить нечем -
    # она не протухла, а стала непроверяемой; молчать о таком нельзя.
    unchecked_positions = sorted(code for code in position_debt if code not in seen)

    if problems:
        print("ОТКАЗ: качество диагностик:")
        for problem in problems[:40]:
            print(f"  {problem}")
        if len(problems) > 40:
            print(f"  … и ещё {len(problems) - 40}")
        sys.exit(1)

    print(
        f"Качество диагностик: {checked} сообщений проверено, "
        f"кодов достигнуто {len(seen)} из {len(all_codes)}, "
        f"долг позиций {len(position_debt)}, долг покрытия {len(coverage_debt)}."
    )
    if unchecked_positions:
        print(
            "  ⚠️ записи долга позиций, которых корпус не даёт (проверить нечем): "
            + ", ".join(unchecked_positions)
        )


if __name__ == "__main__":
    main()

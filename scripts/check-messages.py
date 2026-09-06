#!/usr/bin/env python3
"""Гейт каталогов сообщений (фича 0532, задача 0532-01).

Каталоги живут в `takt-lang/messages/<код>.txt` и наполняются РУКАМИ: перевод —
работа человека, а машина сторожит то, что человек пропускает молча.

Проверки (падают СПИСКОМ, а не первой находкой):

* `M1` — формат: строка без `=`, пустой ключ, повтор ключа в одном каталоге.
* `M2` — паритет ключей: набор каждого каталога равен набору базового `ru`.
  ⚠️ Без ратчета — намеренно: ратчет позволил бы «перевести потом», а `render`
  в этом случае молча отдаёт базовый текст, и английский вывод оказался бы
  наполовину русским при зелёном гейте.
* `M3` — паритет подстановок: у ключа одинаковый набор `{имя}` во всех языках.
  Разойдись он — в переводе появится либо видимое `{name}`, либо потерянное
  значение.
* `M4` — в неосновном каталоге нет кириллицы (признак «ключ скопирован, но не
  переведён»; базовый каталог русский и под правило не подпадает).
* `M5` — мёртвый ключ: константа ключа не упоминается ни в одном исходнике.
  Ключ, который никто не печатает, — мусор, растущий молча.

Самопроверка (правило 0315: у гейта есть сторож, и он идёт первым) —
`scripts/test-check-messages.sh`.

Переменная `MSG_ROOT` переопределяет корень дерева: сторож гоняет гейт на КОПИИ,
не трогая рабочие каталоги.
"""

import os
import re
import sys
from pathlib import Path

BASE_LANG = "ru"
CYRILLIC = re.compile(r"[А-Яа-яЁё]")
PLACEHOLDER = re.compile(r"\{([A-Za-z_][A-Za-z0-9_]*)\}")


def const_name(key: str) -> str:
    """Имя константы из ключа — то же правило, что в `takt-lang/build.rs`."""
    return key.replace("-", "_").replace(".", "_").upper()


def parse_catalogue(path: Path, errors: list[str]) -> dict[str, str]:
    """Разбирает каталог; нарушения формата (`M1`) складывает в `errors`."""
    entries: dict[str, str] = {}
    for no, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        if "=" not in line:
            errors.append(f"M1 {path.name}:{no}: строка без '='")
            continue
        key, value = line.split("=", 1)
        key = key.strip()
        if not key:
            errors.append(f"M1 {path.name}:{no}: пустой ключ")
            continue
        if key in entries:
            errors.append(f"M1 {path.name}:{no}: ключ '{key}' повторён")
            continue
        entries[key] = value.strip()
    return entries


def sources(root: Path) -> str:
    """Текст всех исходников крейтов — в нём ищутся упоминания констант (`M5`)."""
    text = []
    for crate in ("takt-lang", "takt-sim", "takt-wasm"):
        src = root / crate / "src"
        if not src.is_dir():
            continue
        for path in src.rglob("*.rs"):
            text.append(path.read_text(encoding="utf-8", errors="replace"))
    return "\n".join(text)


def main() -> int:
    root = Path(os.environ.get("MSG_ROOT", Path(__file__).resolve().parent.parent))
    directory = root / "takt-lang" / "messages"
    if not directory.is_dir():
        print(f"ОШИБКА: нет каталога сообщений {directory}", file=sys.stderr)
        return 1

    errors: list[str] = []
    catalogues = {
        path.stem: parse_catalogue(path, errors)
        for path in sorted(directory.glob("*.txt"))
    }

    if BASE_LANG not in catalogues:
        print(f"ОШИБКА: нет базового каталога {BASE_LANG}.txt", file=sys.stderr)
        return 1

    base = catalogues[BASE_LANG]
    if not base:
        # Пустой базовый каталог - вырожденный вход: проверка, молчащий на нём,
        # ничего не прочёл.
        errors.append(f"M1 {BASE_LANG}.txt: базовый каталог пуст")

    for lang, entries in catalogues.items():
        if lang == BASE_LANG:
            continue
        for key in sorted(set(base) - set(entries)):
            errors.append(f"M2 {lang}.txt: нет перевода ключа '{key}'")
        for key in sorted(set(entries) - set(base)):
            errors.append(f"M2 {lang}.txt: ключ '{key}' отсутствует в {BASE_LANG}.txt")
        for key in sorted(set(base) & set(entries)):
            want = set(PLACEHOLDER.findall(base[key]))
            got = set(PLACEHOLDER.findall(entries[key]))
            if want != got:
                errors.append(
                    f"M3 {lang}.txt: ключ '{key}' — подстановки "
                    f"{sorted(want)} против {sorted(got)}"
                )
            if CYRILLIC.search(entries[key]):
                errors.append(f"M4 {lang}.txt: ключ '{key}' не переведён (кириллица)")

    text = sources(root)
    for key in sorted(base):
        if const_name(key) not in text:
            errors.append(f"M5 {BASE_LANG}.txt: ключ '{key}' не используется ни одним печатником")

    if errors:
        print("Гейт каталогов сообщений (0532): нарушения", file=sys.stderr)
        for line in errors:
            print(f"  {line}", file=sys.stderr)
        return 1

    langs = ", ".join(sorted(catalogues))
    print(f"  каталоги сообщений: {len(base)} ключей × [{langs}] — паритет соблюдён")
    return 0


if __name__ == "__main__":
    sys.exit(main())

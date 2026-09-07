#!/usr/bin/env python3
"""Проверка комментариев исходного кода.

Комментарий описывает назначение кода. Правило записано в `docs/CODE.md`,
раздел "Комментарии"; здесь оно проверяется машиной.

Проверки (падают списком, а не первой находкой):

* `C1` - пиктограммы и типографские знаки: только ASCII.
* `C2` - номера фич, задач, правил, инвариантов и архитектурных решений со
  словом-маркером ("фича 0448"). Ссылка живёт вне кода и протухает молча.
* `C3` - упоминание того, по чьей просьбе сделано.
* `C4` - разделы истории правки: "Что было", "Зачем", "Замер", "Цена".
* `C5` - жаргон и англицизмы там, где есть русский термин: "проверка", "контроль",
  "фикс", "дефолт", "бэкенд", "рантайм".
* `C6` - выделение прописными. Прописные - только у аббревиатур; список
  кириллических ведётся здесь же.
* `C7` - отсылка к решению, требованию, критерию или тесту карточки по коду
  со словом или в скобках (`решение A5`, `(R4)`). Код живёт в документе и
  протухает так же, как номер.
* `C8` - отсылка к образцу, у которого перенято устройство: код описывает
  себя, а не источник заимствования.
* `C9` - датированный замер, проба, прогон или проработка. Это история
  работы; ссылка на стандарт (IEEE, IEC, RFC, ГОСТ) под правило не подпадает.
* `C10` - номер работы без слова-маркера: голый (`0276`), через дробь
  (`0431/0432`), после косой черты (`/0134`, `/08`). Номер работы дополнен
  нулём до четырёх знаков, и по этой форме он узнаётся; десятичная дробь
  (`0.0125`) и путь к карточке (`0029-c-type-mapping.md`) под правило не
  подпадают: путь есть адрес ссылки.
* `C11` - код решения, требования или теста карточки без слова и скобок
  (`R7`, `T11`, `**T1.**`). Внутри `scripts/` не проверяется: проверки
  называют так свои собственные классы, и это их имена, а не отсылки.
* `C12` - история словами: "прежде", "первая редакция", "найдено замером",
  "починено", "дожило". Оборот "прежде чем" под правило не подпадает.
* `C13` - происхождение модуля: "вынесено из", "по правилу размера",
  "упирается в лимит". Место модуля в дереве говорит `mod`.
* `C14` - форма после снятия: хвостовой пробел, висячий дефис перед знаком
  препинания (`карта) -.`), пустая строка `//!` или `///` последней в блоке.

Проверка идёт ратчетом: долг заморожен в `scripts/comment-baseline.txt`
строками "путь код число". Число находок класса в файле не растёт, файл без
записи долга не имеет, а протухшая запись (находок меньше, чем разрешено) есть
отказ: `--update-baseline` переписывает реестр по текущему состоянию, а
`--list [ПРЕФИКС]` печатает все находки под префиксом пути без оглядки на
реестр - так правят долг файл за файлом.

Область - все отслеживаемые файлы проекта: исходники, скрипты, конфигурации,
примеры, фикстуры, плагины, книга. Единственный каталог-исключение - `docs/`.
Синтаксис комментария выбирается по расширению (`//` и `/* */`, `#`, `<!-- -->`,
`(* *)`); в документах `*.md` комментарием считается только разметка, а текст
судит `check-docs-style.py`. Файлы `*.svg` не разбираются: рисунок строит
graphviz, и комментарий в нём не авторский. Блоки примеров (```) внутри
комментария пропускаются: там знаки принадлежат примеру, а не описанию.

Самопроверка - `scripts/test-check-comments.sh`.
Переменная `CC_ROOT` переопределяет корень дерева.
"""

import os
import re
import sys
from pathlib import Path

# Область - все отслеживаемые файлы проекта, кроме каталога `docs/`: карточки
# и своды хранят историю работы по своему назначению. Вне git (копия дерева
# для самопроверки) файлы перечисляются обходом.
EXCLUDED_DIRS = ("docs", ".git", "target", "node_modules")
# Синтаксис комментария выбирается по расширению; файл незнакомого рода
# (двоичный, `json`, `lock`) не разбирается.
CLIKE = re.compile(r"^\s*(?://+!?|/\*+|\*(?!/))\s?(.*)$")
TYPST = re.compile(r"^\s*(?://+|/\*+)\s?(.*)$")
HASH = re.compile(r"^\s*#\s?(.*)$")
MARKUP = re.compile(r"^\s*<!--\s?(.*)$")
IEC = re.compile(r"^\s*(?://+|\(\*)\s?(.*)$")
GRAPHVIZ = re.compile(r"^\s*(?://+|/\*+|\*(?!/)|#)\s?(.*)$")
SYNTAX = {
    **dict.fromkeys(
        [".rs", ".js", ".mjs", ".ts", ".kt", ".kts", ".gradle", ".css", ".scss",
         ".c", ".h", ".sv", ".svh", ".ld", ".lalrpop", ".takt", ".java"],
        CLIKE,
    ),
    ".typ": TYPST,
    ".dot": GRAPHVIZ,
    ".st": IEC,
    ".ebnf": IEC,
    **dict.fromkeys(
        [".py", ".sh", ".bash", ".zsh", ".ps1", ".toml", ".yml", ".yaml",
         ".sublime-syntax", ".conf", ".cfg", ".ini", ".properties", ".txt",
         ".env", ".example", ".mk", ".editorconfig", ".gitignore",
         ".gitattributes", ".dockerignore", ".clang-format", ".clang-tidy"],
        HASH,
    ),
    **dict.fromkeys([".html", ".htm", ".xml", ".md", ".tmTheme", ".plist"], MARKUP),
}
HASH_NAMES = {"Makefile", "Dockerfile", "Justfile", "gradlew"}
BASELINE = "scripts/comment-baseline.txt"
CLASSES = ("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14")

SHEBANG = re.compile(r"^#!")
DOC = re.compile(r"^\s*(?://!|///)")

PICTO = re.compile(r"[⚠✓✗★⌘⚡•▪◆—–→←≡≤≥«»…×≠]")
NUMBER = re.compile(
    r"\b(?:фич\w*|задач\w*|подзадач\w*|правил\w*|инвариант\w*|ADR|фикс)\s+№?\s*\d{2,4}",
    re.I,
)
CUSTOMER = re.compile(r"\bзаказчик\w*", re.I)
# Формы жаргонного слова перечислены поимённо: "фиксированный", "фиксация" и
# "фикстура" - обычные слова, и под правило не подпадают.
JARGON = re.compile(
    r"\b(?:гейт\w*|сторож\w*|дефолт\w*|бэкенд\w*|рантайм\w*|оверлей\w*|"
    r"фикс|фикса|фиксу|фиксе|фиксом|фиксы|фиксов|фиксам|фиксами|фиксах)\b",
    re.I,
)
# Кириллические аббревиатуры: прописные у них законны.
ABBR = {"АСД", "ПЛК", "МК", "СУС", "ЦП", "ФС", "ПИД", "ОЗУ", "ПЗУ", "ЭВМ", "ГОСТ"}
CAPS = re.compile(r"\b[А-ЯЁ]{2,}\b")
CODE = re.compile(r"`[^`]*`")
# Код решения, требования, критерия или теста карточки.
DECISION = re.compile(
    r"\((?:см\.\s*)?(?:решени\w+|вариант\w*|требовани\w+|критери\w+|тест\w*)?"
    r"\s*[ARTODSNG]-?\d{1,2}[a-zа-я]?(?:\s*[/,]\s*(?:[ARTODSNG]?-?\d{1,2}[a-zа-я]?))*\)"
    r"|\b(?:решени\w+|вариант\w+|требовани\w+|критери\w+)\s+[ARTODSNG]-?\d{1,2}"
    r"|\b(?:решением|решение|вариант|вариантом)\s+\d{1,2}[AB]\b"
    r"|\b(?:задач\w+|фич\w+|подзадач\w+)\s*`\d[\w-]*`",
    re.I,
)
SAMPLE = re.compile(r"\bреференс\w*", re.I)
# Датированный замер и проработка; ссылка на стандарт остаётся.
MEASURE = re.compile(
    r"\((?!\s*(?:IEEE|IEC|RFC|ГОСТ|ISO|POSIX))[^()]{0,70}?"
    r"(?:проработк\w+|замер\w*|проба|проб[еы]|прогон\w*|\d{4}-\d{2}-\d{2})"
    r"[^()]{0,70}?\)",
    re.I,
)
HISTORY = re.compile(
    r"^#+\s*(что было|зачем|почему|цена|история|замер|что нашёл|что нашла|"
    r"как нашли|предыстория)\b",
    re.I,
)
# Номер работы узнаётся по форме: четыре знака с ведущим нулём. Десятичная
# дробь и путь к карточке (номер, за которым идёт слаг) исключены.
WORK_NUMBER = re.compile(
    r"(?<![\w.,])0\d{3}(?!\w|[.,]\d|-[a-z])|(?<=\s)/\d{2}[a-zа-я]?(?!\w)"
)
# Код карточки без слова и скобок. Буква `S` не входит: `S1`, `S2` - имена
# состояний в примерах.
CARD_CODE = re.compile(r"\b[ARTDONG]\d{1,2}\b")
HISTORY_WORDS = re.compile(
    r"\bпрежде\b(?!\s+(?:чем|всего))|\bперв\w+\s+редакци\w+|"
    r"\bнайден\w*\s+(?:замером|прогоном|перебором)|\bпочин\w+|\bдожил\w*|"
    r"\bуже\s+(?:исправлен|починен)\w*",
    re.I,
)
MODULE_ORIGIN = re.compile(
    r"\b(?:вынос\w*|вынесен\w*|перенес\w+|перенос\w*)\s+из\b|"
    r"\bправил\w+\s+размера\b|\bлимит\w*\s+размера\b|\bупира\w+\s+в\s+лимит",
    re.I,
)
TRAILING_SPACE = re.compile(r"[ \t]+$")
DANGLING_DASH = re.compile(r"\s-[.,;:]|\(\s*-\s*\)")


def check(path: Path, rel: str, comment: re.Pattern) -> dict[str, list[str]]:
    """Находки в одном файле по классам."""
    found: dict[str, list[str]] = {code: [] for code in CLASSES}
    hard = _Sink(found)
    soft = found
    try:
        text = path.read_text(encoding="utf-8")
    except (UnicodeDecodeError, OSError):
        return found
    in_example = False
    in_scripts = rel.startswith("scripts/")
    empty_doc: str | None = None
    for no, raw in enumerate(text.splitlines(), 1):
        if SHEBANG.match(raw.lstrip()):
            continue
        m = comment.match(raw)
        if not m:
            if empty_doc:
                hard.append(f"C14 {empty_doc}: пустая строка документации последней в блоке")
            empty_doc = None
            continue
        body = m.group(1)
        where = f"{rel}:{no}"
        if body.strip().startswith("```"):
            in_example = not in_example
            empty_doc = None
            continue
        if in_example:
            continue
        empty_doc = where if (not body.strip() and DOC.match(raw)) else None
        if PICTO.search(body):
            hard.append(f"C1 {where}: пиктограмма или типографский знак")
        if NUMBER.search(body):
            hard.append(f"C2 {where}: ссылка на номер")
        if CUSTOMER.search(body):
            hard.append(f"C3 {where}: упоминание просьбы")
        if HISTORY.match(body.strip()):
            hard.append(f"C4 {where}: раздел истории правки")
        if DECISION.search(body):
            hard.append(f"C7 {where}: отсылка к решению или требованию")
        if SAMPLE.search(body):
            hard.append(f"C8 {where}: отсылка к образцу заимствования")
        if MEASURE.search(body):
            hard.append(f"C9 {where}: датированный замер или проработка")
        if TRAILING_SPACE.search(raw):
            hard.append(f"C14 {where}: хвостовой пробел")
        if DANGLING_DASH.search(body):
            hard.append(f"C14 {where}: висячий дефис перед знаком препинания")
        plain = CODE.sub("", body)
        if JARGON.search(plain):
            hard.append(f"C5 {where}: жаргон вместо термина")
        caps = [w for w in CAPS.findall(plain) if w not in ABBR]
        if caps:
            hard.append(f"C6 {where}: выделение прописными - {caps[0]}")
        if WORK_NUMBER.search(plain):
            soft["C10"].append(f"C10 {where}: номер работы без слова")
        if not in_scripts and CARD_CODE.search(plain):
            soft["C11"].append(f"C11 {where}: код карточки без слова")
        if HISTORY_WORDS.search(plain):
            soft["C12"].append(f"C12 {where}: история словами")
        if MODULE_ORIGIN.search(plain):
            soft["C13"].append(f"C13 {where}: происхождение модуля")
    if empty_doc:
        hard.append(f"C14 {empty_doc}: пустая строка документации последней в блоке")
    return found


class _Sink:
    """Раскладывает строку находки в список её класса."""

    def __init__(self, found: dict[str, list[str]]):
        self.found = found

    def append(self, line: str) -> None:
        self.found[line.split(" ", 1)[0]].append(line)


def syntax_of(path: Path) -> re.Pattern | None:
    """Разбор комментария по роду файла; `None` - файл не разбирается."""
    if path.name in HASH_NAMES:
        return HASH
    return SYNTAX.get(path.suffix) if path.suffix else None


def project_files(root: Path) -> list[Path]:
    """Отслеживаемые файлы проекта вне `docs/`; без git - обход дерева."""
    import subprocess

    paths: list[Path] = []
    if (root / ".git").exists():
        out = subprocess.run(
            ["git", "-C", str(root), "ls-files", "-z"],
            check=True, capture_output=True,
        ).stdout
        names = [n for n in out.decode("utf-8").split("\0") if n]
    else:
        names = [
            p.relative_to(root).as_posix()
            for p in sorted(root.rglob("*"))
            if p.is_file()
        ]
    for name in names:
        head = name.split("/", 1)[0]
        if head in EXCLUDED_DIRS:
            continue
        path = root / name
        if path.is_file():
            paths.append(path)
    return sorted(paths)


def load_baseline(root: Path) -> dict[tuple[str, str], int]:
    """Реестр долга: `(путь, код) -> разрешённое число находок`."""
    path = root / BASELINE
    allowed: dict[tuple[str, str], int] = {}
    if not path.is_file():
        return allowed
    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.split("#", 1)[0].strip()
        if not line:
            continue
        parts = line.split()
        if len(parts) != 3 or not parts[2].isdigit():
            print(f"ОШИБКА: негодная строка реестра {BASELINE}: {line!r}", file=sys.stderr)
            sys.exit(1)
        allowed[(parts[0], parts[1])] = int(parts[2])
    return allowed


def write_baseline(root: Path, counts: dict[tuple[str, str], int]) -> None:
    lines = [
        "# Долг проверки scripts/check-comments.py: комментарии вне правила.",
        "# Строка - \"путь код число\": разрешённое число находок класса в файле.",
        "# Долг не растёт; запись снимается вместе с правкой комментария",
        "# (python3 scripts/check-comments.py --update-baseline).",
        "",
    ]
    for (rel, code), n in sorted(counts.items()):
        lines.append(f"{rel} {code} {n}")
    (root / BASELINE).write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> int:
    root = Path(os.environ.get("CC_ROOT", Path(__file__).resolve().parent.parent))
    args = sys.argv[1:]
    update = "--update-baseline" in args
    listing = "--list" in args
    prefix = ""
    if listing:
        rest = [a for a in args if a != "--list"]
        prefix = rest[0] if rest else ""
    allowed = load_baseline(root)
    errors: list[str] = []
    counts: dict[tuple[str, str], int] = {}
    files = 0
    for path in project_files(root):
        comment = syntax_of(path)
        if comment is None:
            continue
        files += 1
        rel = path.relative_to(root).as_posix()
        for code, hits in check(path, rel, comment).items():
            if not hits:
                continue
            if listing and rel.startswith(prefix):
                print("\n".join(hits))
            counts[(rel, code)] = len(hits)
            budget = allowed.get((rel, code), 0)
            if len(hits) > budget:
                errors.append(f"{code} {rel}: находок {len(hits)}, разрешено {budget}")
                errors.extend(f"  {h}" for h in hits[-(len(hits) - budget):])
    if not update:
        for (rel, code), budget in sorted(allowed.items()):
            actual = counts.get((rel, code), 0)
            if actual < budget:
                errors.append(
                    f"{code} {rel}: протухшая запись реестра - разрешено {budget}, "
                    f"находок {actual} (обнови --update-baseline)"
                )

    if not files:
        print("ОШИБКА: не найдено ни одного файла с кодом", file=sys.stderr)
        return 1
    if listing:
        return 0
    if update:
        write_baseline(root, counts)
        print(f"  комментарии: реестр долга переписан, записей {len(counts)}")
        return 0
    if errors:
        print("Проверка комментариев: нарушения", file=sys.stderr)
        for line in errors[:60]:
            print(f"  {line}", file=sys.stderr)
        if len(errors) > 60:
            print(f"  ... и ещё {len(errors) - 60}", file=sys.stderr)
        print(
            "\nКомментарий описывает назначение кода (docs/CODE.md, "
            '"Комментарии"): без номеров, истории правки и пиктограмм.',
            file=sys.stderr,
        )
        return 1

    print(f"  комментарии: проверено {files} файлов, нарушений нет")
    return 0


if __name__ == "__main__":
    sys.exit(main())

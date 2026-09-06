#!/usr/bin/env python3
"""Гейт согласованности конфига Zed с языком (фича 0465).

Расширение Zed описывает язык **декларативно**: суффикс файла, комментарии,
скобки, имя языкового сервера. Ни одно из этих утверждений машиной не
проверялось — правило 29 держалось на дисциплине, а расхождение замечает
только человек, открывший файл в редакторе.

Проверки (каждая — утверждение конфига против ФАКТА проекта):

  Z1 — суффикс файла: `path_suffixes` обязан содержать расширение исходников
       (`takt`), иначе редактор не опознаёт файлы языка вовсе;
  Z2 — комментарии: `line_comments` обязан перечислять формы, которые лексер
       действительно считает комментарием (`//` и doc-форму `///`);
  Z3 — скобки: `brackets` обязан покрывать ВСЕ парные скобки лексера
       (`{}`, `()`, `[]`) — пропущенная пара лишает редактор автозакрытия и
       подсветки пары;
  Z4 — языковой сервер: `language_servers` обязан называть бинарник, который
       проект собирает (`takt-lsp`), — иначе редактор молча остаётся без LSP;
  Z5 — блочный комментарий: если лексер знает форму `/* */`, конфиг обязан
       объявить `block_comment` — иначе редактор не умеет закомментировать
       выделение, а сама форма языка ему неизвестна.

⚠️ Гейт проверяет СОГЛАСОВАННОСТЬ, а не полноту: конфиг вправе описывать
больше (например автозакрытие кавычек), и это не ошибка.
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CONFIG = ROOT / "extensions/zed-takt/languages/takt/config.toml"
LEXER = ROOT / "takt-lang/src/parser/lexer.rs"
EXTENSION_TOML = ROOT / "extensions/zed-takt/extension.toml"

# Пары скобок языка: имя токена лексера -> пара символов.
BRACKET_TOKENS = {
    "OpenCurlyBrace": ("{", "}"),
    "OpenParenthesis": ("(", ")"),
    "OpenBracket": ("[", "]"),
}


def fail(message: str) -> None:
    print(f"ОТКАЗ: {message}")
    sys.exit(1)


def main() -> None:
    if not CONFIG.is_file():
        fail(f"конфига языка нет: {CONFIG.relative_to(ROOT)}")
    config = CONFIG.read_text(encoding="utf-8")
    lexer = LEXER.read_text(encoding="utf-8")

    # Z1 - суфисправление файла.
    suffixes = re.search(r"path_suffixes\s*=\s*\[([^\]]*)\]", config)
    if not suffixes or "takt" not in suffixes.group(1):
        fail("Z1: `path_suffixes` не содержит расширения 'takt' — редактор не "
             "опознает файлы языка")

    # Z2 - комментарии: обе формы обязаны быть у лексера и в конфиге.
    comments = re.search(r"line_comments\s*=\s*\[([^\]]*)\]", config)
    if not comments:
        fail("Z2: `line_comments` не объявлены")
    declared = {c.strip().strip('"').strip() for c in comments.group(1).split(",") if c.strip()}
    if "//" not in declared:
        fail("Z2: `line_comments` не содержит '//' — обычный комментарий языка")
    # Doc-комментарий лексер отличает от обычного (`///`), и редактор обязан
    # знать обе формы: иначе строка документации не сворачивается как комментарий.
    if "///" in lexer and "///" not in declared:
        fail("Z2: лексер знает doc-комментарий '///', а конфиг — нет")

    # Z3 - скобки.
    brackets = re.findall(r'start\s*=\s*"([^"]+)"\s*,\s*end\s*=\s*"([^"]+)"', config)
    declared_pairs = {(start, end) for start, end in brackets}
    for token, pair in BRACKET_TOKENS.items():
        if token not in lexer:
            fail(f"Z3: токен '{token}' исчез из лексера — обнови гейт вместе с языком")
        if pair not in declared_pairs:
            fail(f"Z3: пара скобок {pair[0]}{pair[1]} есть в лексере, но не в конфиге Zed")

    # Z5 - блочный комментарий: форма языка против объявления редактора.
    if "/* ... */" in lexer or "/* */" in lexer:
        block = re.search(r"block_comment\s*=\s*\[([^\]]*)\]", config)
        if not block:
            fail("Z5: лексер знает блочный комментарий '/* */', а конфиг Zed его "
                 "не объявляет (`block_comment`)")
        parts = [c.strip().strip('"').strip() for c in block.group(1).split(",") if c.strip()]
        if parts[:2] != ["/*", "*/"]:
            fail(f"Z5: `block_comment` объявлен как {parts}, а язык знает '/*' и '*/'")

    # Z4 - языковой сервер.
    servers = re.search(r"language_servers\s*=\s*\[([^\]]*)\]", config)
    if not servers or "takt-lsp" not in servers.group(1):
        fail("Z4: `language_servers` не называет 'takt-lsp' — редактор останется без LSP")
    if EXTENSION_TOML.is_file():
        manifest = EXTENSION_TOML.read_text(encoding="utf-8")
        if "takt-lsp" not in manifest:
            fail("Z4: манифест расширения не объявляет сервер 'takt-lsp'")

    print(
        f"Конфиг Zed: суффикс, {len(declared)} формы строчного комментария, "
        f"блочный комментарий, {len(declared_pairs)} пар скобок и языковой "
        f"сервер сверены с языком."
    )


if __name__ == "__main__":
    main()

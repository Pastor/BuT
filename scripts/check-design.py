#!/usr/bin/env python3
"""Гейт дизайн-системы веб-части (фича 0531, задача 07a).

Что доказывает — по одному правилу книги `web/design/BOOK.md` на проверку:

  D1  цвета числом нет вне ПАЛИТРЫ (палитра находится по форме правила;
      исключение — `mask-image`, где цвет есть канал прозрачности);
  D2  в `:hover`/`:active` нет готового цвета — только формула отклика;
  D3  пара «заливка / чернила» взята из реестра книги; реестр не содержит пар
      без применения, а витрина показывает ровно его (сверка трёх сторон:
      вёрстка, книга, витрина);
  D4  `font-size` берётся из шкалы кегля, а не пишется числом;
  D5  высота и скругление берутся из шкал;
  D6  книга и витрина не разошлись: у каждого контрола книги есть образец в
      витрине и наоборот;
  D7  у витрины нет СВОИХ цветов и кеглей — она стоит на живом `app.css`;
  D8  каждая упомянутая переменная где-то объявлена.

⚠️ D8 заведён по случившемуся: `--gap-lg` использовался раньше, чем был
объявлен, и модальное окно вышло ВОВСЕ без полей. Неизвестную переменную
браузер не считает ошибкой — он отбрасывает объявление молча, и увидеть это
можно только глазом на той самой странице.

⚠️ Правило дизайна, которое не сторожит машина, держится дисциплиной, а она у
одного разработчика заканчивается на второй неделе: у референса набралось 26
значений кегля на 92 объявления, прежде чем завели гейт.

⚠️ Палитра ищется ПО ФОРМЕ (тело правила из одних объявлений токенов), а не по
имени файла или селектора: правило, привязанное к имени, обходится переносом
строки в другой блок.
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CSS = ROOT / "web/static/app.css"
BOOK = ROOT / "web/design/BOOK.md"
SHOWCASE = ROOT / "web/design/controls.html"

# Цвет числом: шестнадцатеричный, `rgb()`, `hsl()` и обиходные имена.
# `transparent`, `currentColor` и `inherit` цветом числом не считаются: они
# не задают значения, а ссылаются на контекст.
COLOR = re.compile(
    r"(#[0-9a-fA-F]{3,8}\b|\brgba?\s*\(|\bhsla?\s*\(|"
    r"\b(?:red|green|blue|white|black|gray|grey|yellow|orange|purple|pink|brown)\b)"
)

# Кегль из шкалы: `var(--text-...)`; `inherit` наследует и значения не задаёт.
TEXT_SCALE = re.compile(r"var\(--text-[a-z]+\)|inherit")

# Высота и скругление из шкал.
SIZE_SCALE = re.compile(r"var\(--h-[a-z]+\)|var\(--radius\)|var\(--gap[a-z-]*\)")


class Rule:
    """Правило CSS: селектор, объявления и номер строки."""

    def __init__(self, selector, body, line):
        self.selector = " ".join(selector.split())
        self.line = line
        self.declarations = []
        for piece in body.split(";"):
            if ":" not in piece:
                continue
            name, _, value = piece.partition(":")
            self.declarations.append((name.strip(), value.strip()))

    @property
    def is_palette(self):
        """Правило палитры: тело из ОДНИХ объявлений токенов."""
        return bool(self.declarations) and all(
            name.startswith("--") for name, _ in self.declarations
        )

    def value(self, name):
        for declared, value in self.declarations:
            if declared == name:
                return value
        return None


def strip_comments(text):
    """Гасит комментарии, сохраняя переводы строк: номера не должны съехать."""
    return re.sub(
        r"/\*.*?\*/", lambda m: re.sub(r"[^\n]", " ", m.group(0)), text, flags=re.S
    )


def parse(css):
    """Разбирает CSS на правила. Вложенность `@media` учитывается глубиной."""
    text = strip_comments(css)
    rules = []
    depth = 0
    start = 0
    selector_start = 0
    for index, ch in enumerate(text):
        if ch == "{":
            if depth == 0:
                selector = text[selector_start:index]
                start = index + 1
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                body = text[start:index]
                if "{" not in body:
                    line = text[:selector_start].count("\n") + 1
                    rules.append(Rule(selector, body, line))
                else:
                    # `@media`: разбираем содержимое как самостоятельный CSS,
                    # сохраняя номера строк исходника.
                    inner = text[start:index]
                    offset = text[:start].count("\n")
                    for rule in parse(inner):
                        rule.line += offset
                        rules.append(rule)
                selector_start = index + 1
    return rules


def book_pairs(text):
    """Реестр пар из таблицы книги."""
    pairs = []
    for line in text.splitlines():
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if len(cells) != 3:
            continue
        fill, ink = cells[0], cells[1]
        if fill.startswith("`--surface") and ink.startswith("`--on-"):
            pairs.append((fill.strip("`"), ink.strip("`")))
    return pairs


def book_controls(text):
    """Контролы книги: заголовок `### Имя (\\`селектор\\`)`."""
    return [m.group(1) for m in re.finditer(r"^### .*\(`([^`]+)`\)", text, re.M)]


def token(value):
    """Имя токена из `var(--имя)`; `None` — значение не токен."""
    match = re.fullmatch(r"var\((--[\w-]+)\)", value.strip())
    return match.group(1) if match else None


def main():
    problems = []
    css = CSS.read_text(encoding="utf-8")
    book = BOOK.read_text(encoding="utf-8")
    showcase = SHOWCASE.read_text(encoding="utf-8")
    rules = parse(css)
    if not rules:
        print("  ОШИБКА: в app.css не разобрано ни одного правила")
        return 1

    # D8 - упомянутая переменная объявлена. Неизвестная переменная - не ошибка
    # для браузера: он молча отбрасывает объявление, и правило пропадает
    # целиком (`--gap-lg` оставил модальное окно без полей).
    declared = set(re.findall(r"^\s*(--[\w-]+)\s*:", css, re.M))
    # Запись `var(--x, умолчание)` в счёт не идёт: там отсутствие объявления
    # предусмотрено - так страница читает величины, которые ставит `shell.js`.
    for match in re.finditer(r"var\(\s*(--[\w-]+)\s*\)", css):
        if match.group(1) not in declared:
            line = css.count("\n", 0, match.start()) + 1
            problems.append(f"D8 app.css:{line}: переменная '{match.group(1)}' нигде не объявлена")

    registry = book_pairs(book)
    if not registry:
        print("  ОШИБКА: в книге нет реестра пар")
        return 1
    used_pairs = set()

    for rule in rules:
        where = f"app.css:{rule.line} ({rule.selector})"

        # D1 - цвет числом вне палитры.
        # Маска исключена намеренно: в `mask-image` цвет не цвет, а канал
        # прозрачности - чёрное значит "оставить", и заменить его токеном
        # нельзя. Исключение узкое: по имени свойства, а не по значению.
        if not rule.is_palette:
            for name, value in rule.declarations:
                if name.endswith("mask-image"):
                    continue
                if COLOR.search(value):
                    problems.append(f"D1 {where}: цвет числом в '{name}: {value}'")

        # D2 - готовый цвет в отклике.
        if re.search(r":hover|:active", rule.selector):
            for name, value in rule.declarations:
                if name in ("background", "background-color", "color") and "color-mix(" not in value:
                    problems.append(
                        f"D2 {where}: готовый цвет в отклике — '{name}: {value}'; "
                        f"отклик считается формулой одной плотности"
                    )

        # D3 - пара из реестра.
        fill = token(rule.value("background") or rule.value("background-color") or "")
        ink = token(rule.value("color") or "")
        if fill and ink and fill.startswith("--surface"):
            if (fill, ink) in registry:
                used_pairs.add((fill, ink))
            else:
                problems.append(
                    f"D3 {where}: пара '{fill}' / '{ink}' не названа в реестре книги"
                )

        # D4 - кегль из шкалы.
        if not rule.is_palette:
            for name, value in rule.declarations:
                if name == "font-size" and not TEXT_SCALE.fullmatch(value):
                    problems.append(f"D4 {where}: кегль мимо шкалы — '{value}'")

            # D5 - высота и скругление из шкал.
            for name, value in rule.declarations:
                if name in ("height", "min-height", "border-radius") and re.search(r"\d+px", value):
                    if not SIZE_SCALE.search(value):
                        problems.append(f"D5 {where}: '{name}: {value}' мимо шкалы")

    for pair in registry:
        if pair not in used_pairs:
            problems.append(
                f"D3 книга: пара '{pair[0]}' / '{pair[1]}' в реестре есть, "
                f"а в app.css не применяется"
            )

    # D3 (третья сторона) - образцы витрины против реестра книги.
    swatches = {
        tuple(m.group(1).split("/"))
        for m in re.finditer(r'data-pair="([^"]+)"', showcase)
    }
    for pair in set(registry) - swatches:
        problems.append(
            f"D3 витрина: пара '{pair[0]}' / '{pair[1]}' описана в книге и не показана"
        )
    for pair in swatches - set(registry):
        problems.append(
            f"D3 книга: пара '{pair[0]}' / '{pair[1]}' показана в витрине и не описана"
        )

    # D6 - книга и витрина не разошлись.
    shown = {
        m.group(1)
        for m in re.finditer(r'data-control="([^"]+)"', showcase)
        if not m.group(1).startswith("--")
    }
    described = set(book_controls(book))
    for control in sorted(described - shown):
        problems.append(f"D6 витрина: контрол '{control}' описан в книге и не показан")
    for control in sorted(shown - described):
        problems.append(f"D6 книга: контрол '{control}' показан в витрине и не описан")

    # D7 - у витрины нет своих цветов и кеглей.
    style = re.search(r"<style>(.*?)</style>", showcase, re.S)
    if style:
        for rule in parse(style.group(1)):
            for name, value in rule.declarations:
                if COLOR.search(value):
                    problems.append(f"D7 controls.html ({rule.selector}): свой цвет '{value}'")
                if name == "font-size":
                    problems.append(f"D7 controls.html ({rule.selector}): свой кегль '{value}'")

    if problems:
        print(f"  ОШИБКА: дизайн-система нарушена ({len(problems)}):")
        for problem in problems:
            print(f"    {problem}")
        return 1

    print(
        f"  Дизайн-система: правил {len(rules)}, пар в реестре {len(registry)}, "
        f"контролов {len(described)} — расхождений нет."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

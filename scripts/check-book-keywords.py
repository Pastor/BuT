#!/usr/bin/env python3
"""check-book-keywords.py — лексика приложения «Грамматика» против кода (фича 0160).

После фичи 0160 описание грамматики в проекте **одно**:
`book/src/appendix-grammar/index.typ` (корневой `Takt.ebnf` удалён — ADR 0160,
Option A). Единственность снимает расхождение двух описаний, но не защищает
оставшееся от отставания: замер ADR показал, что приложение отстало на 16
ключевых слов, восемь конструкций и позицию присваивания, — при этом ни один
гейт этого не видел.

Машиной сверяется **лексика**: она размножается механически и потому обязана
проверяться машиной, а не ревью. Правила EBNF машине недоступны — это проза о
форме; их держат правило 24 (стадия «Документирование» каждой языковой фичи) и
правило 4.

Четыре класса проверок:

- **K1** — ключевое слово лексера, которого нет в списке документа;
- **K2** — слово в списке документа, которого нет в лексере;
- **P1** — знак в сводке пунктуации документа, которого нет среди терминалов
  грамматики (так дожил `-->`, изъятый фичей 0201);
- **P2** — терминал-знак грамматики, которого нет в сводке документа (так
  отсутствовали `=>` ветви `match` и `#` анонимного порта);
- **H1…H4** — расхождение **списков подсветки** документа (фича 0240):
  `book/takt-keywords.txt` (инлайн-код) и `book/takt.sublime-syntax` (блоки кода)
  против лексера и друг против друга. Повод — замер: их предшественник
  `book/takt.kate.xml` отстал от лексера на пять слов (`parameter`, `clock`,
  `at`, `after`, `every`) и не проверялся ничем.

Источники истины — `takt-lang/src/parser/lexer.rs` (таблица `KEYWORDS`) и
extern-блок `takt-lang/src/grammar.lalrpop` (терминалы).

⚠️ Разбор документа идёт **по абзацу**, а не по строке: список ключевых слов
занимает четыре строки, и построчный поиск дал бы ноль совпадений, то есть
зелёный гейт, не проверяющий ничего. Ровно этим оказался слеп `check_crate_versions`
(фикс 0202-01) — здесь класс не повторяется.

⚠️ Пустое множество — **ошибка**, а не успех: при смене разметки проверки
«документ ⊆ код» выполнились бы тривиально.

Использование:

    python3 scripts/check-book-keywords.py              # проверка
    python3 scripts/check-book-keywords.py --self-test  # проверка самих ловушек
"""

import os
import re
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
APPENDIX = os.path.join(ROOT, "book", "src", "appendix-grammar", "index.typ")
LEXICAL = os.path.join(ROOT, "book", "src", "02-lexical", "index.typ")
KEYWORD_LIST = os.path.join(ROOT, "book", "takt-keywords.txt")
SYNTAX = os.path.join(ROOT, "book", "takt.sublime-syntax")
LEXER = os.path.join(ROOT, "takt-lang", "src", "parser", "lexer.rs")
GRAMMAR = os.path.join(ROOT, "takt-lang", "src", "grammar.lalrpop")

# Слова, которые лексер держит в KEYWORDS, но ключевыми они являются только В
# Позиции: правило `Identifier` грамматики принимает каждое как обычное имя.
# Приложение говорит об этом отдельной фразой, и вносить их в список "ключевых
# слов" значило бы утверждать неверное.
#
# X, F, G, U, R, LTL, Guard - операторы темпоральной логики внутри
# аннотации `: [LTL] ... ;`;
# from - терминал директивы импорта: вне
# `import ... from "...";` слово не значит ничего,
#                                и `struct Line { from: u8 }` законен.
#
# Исключение действует только на обязательность: подсвечивать контекстное
# слово не запрещено (класс H2 смотрит на `keywords_code`), и `from` в директиве
# импорта выделяется по-прежнему - там оно и читается ключевым.
CONTEXTUAL = {"X", "F", "G", "U", "R", "LTL", "Guard", "from"}

# Терминалы-знаки грамматики, которых в сводке пунктуации быть не должно.
# `::` языком не является: путь `A::B` разбирается двумя токенами `":"`, и
# отдельного терминала под него нет - перечислять его в сводке значило бы
# обещать лексему, которой нет.
PUNCT_EXCLUDED: set[str] = set()

MARK_KEYWORDS = "#strong[Ключевые слова:]"
MARK_PUNCT = "#strong[Операторы и пунктуация:]"

# Слово, выделяемое в документе намеренно, хотя ключевым словом языка оно не
# является: имена стандартных блоков состояния и модификатор формата `q`. Список
# живёт в секции `[extra]` файла подсветки - здесь только его роль в проверке.
HIGHLIGHT_EXTRA_SECTION = "extra"

# Ключевое слово лексера, которое подсветкой не выделяется намеренно: `_` в прозе
# значит что угодно, и выделение давало бы ложные срабатывания (то же решение
# было в прежнем фильтре `keywords.lua`).
HIGHLIGHT_EXCLUDED = {"_"}


def paragraphs(text):
    """Абзацы текста как одна строка каждый (переносы внутри абзаца сшиты)."""
    return [" ".join(block.split()) for block in re.split(r"\n\s*\n", text)]


def marked_paragraph(text, mark, source):
    """Абзац, начинающийся с метки; отсутствие — ошибка, а не пустое множество."""
    for block in paragraphs(text):
        if block.startswith(mark):
            return block[len(mark) :]
    sys.exit(
        f"ОШИБКА: в {source} нет абзаца «{mark}».\n"
        "Гейт лексики (фича 0160) держится на этой разметке: без неё он молча\n"
        "проверял бы пустое множество. Верните абзац либо обновите гейт вместе\n"
        "с разметкой — но не оставляйте проверку без входа."
    )


def backticked(fragment):
    """Слова во всех обратных кавычках фрагмента (группа `+ - *` даёт три записи)."""
    result = set()
    for item in re.findall(r"`([^`]+)`", fragment):
        result.update(item.split())
    return result


def doc_keywords(text):
    return backticked(marked_paragraph(text, MARK_KEYWORDS, "приложении «Грамматика»"))


def doc_punctuation(text):
    return backticked(marked_paragraph(text, MARK_PUNCT, "приложении «Грамматика»"))


def lexer_keywords(text):
    """Ключевые слова из таблицы `KEYWORDS` лексера."""
    _, _, tail = text.partition("static KEYWORDS")
    table, _, _ = tail.partition("};")
    if not table:
        sys.exit(
            f"ОШИБКА: в {os.path.relpath(LEXER, ROOT)} не найдена таблица KEYWORDS.\n"
            "Гейт лексики (фича 0160) читает её как источник истины о ключевых словах."
        )
    return set(re.findall(r'"([^"]+)"\s*=>', table))


def grammar_terminals(text):
    """Терминалы-строки extern-блока грамматики."""
    _, _, tail = text.partition("enum Token<'input>")
    if not tail:
        sys.exit(
            f"ОШИБКА: в {os.path.relpath(GRAMMAR, ROOT)} не найден extern-блок токенов.\n"
            "Гейт лексики (фича 0160) читает его как источник истины о пунктуации."
        )
    return set(re.findall(r'"([^"]+)"\s*=>', tail))


def punctuation_terminals(terminals):
    """Терминалы-знаки: всё, что не является словом языка.

    ⚠️ Фильтр обязателен: extern-блок держит и ключевые слова, и имена токенов
    (`identifier`, `number`, `duration`) — без него в «пунктуацию» попали бы
    полсотни слов, и сверка потеряла бы смысл.
    """
    return {t for t in terminals if not re.fullmatch(r"[A-Za-z_]+", t)}


def lexical_table(text, heading, source):
    """Тело таблицы раздела «Лексика», начинающейся после заголовка `heading`.

    ⚠️ Читается именно ТАБЛИЦА, а не секция целиком: в прозе раздела ключевые
    слова и знаки встречаются как примеры (`count = LIMIT`, врезка про `X`, `F`,
    `G`), и сверка по всему тексту потеряла бы смысл — класс L2 перестал бы
    ловить лишнее слово.
    """
    start = text.find(heading)
    if start < 0:
        sys.exit(
            f"ОШИБКА: в {source} нет раздела «{heading}».\n"
            "Гейт лексики (фича 0298) сверяет таблицы раздела «Лексика» с языком:\n"
            "без раздела он молча проверял бы пустое множество."
        )
    table = text.find("#table(", start)
    end = table_end(text, table) if table >= 0 else -1
    if table < 0 or end < 0:
        sys.exit(
            f"ОШИБКА: в {source} после «{heading}» не найдена таблица.\n"
            "Гейт лексики (фича 0298) читает её как список раздела."
        )
    return text[table:end]


def table_end(text, table):
    """Индекс скобки, закрывающей `#table(`; `-1` - скобка не закрыта.

    Конец ищется счётом скобок, а не строкой-разделителем: раскладка таблицы
    принадлежит документу, и `,)` в конце последней строки значит ровно то же,
    что `)` на своей. Содержимое обратных кавычек пропускается - там скобка есть
    знак языка (`( )` в таблице пунктуации), а не часть записи таблицы.
    """
    depth, in_code = 0, False
    for i in range(table, len(text)):
        ch = text[i]
        if ch == "`":
            in_code = not in_code
            continue
        if in_code:
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return i
    return -1


def lexical_keywords(text):
    """Слова таблицы «Ключевые слова» раздела «Лексика» (фича 0298)."""
    body = lexical_table(text, "== Ключевые слова", "разделе «Лексика»")
    return {w for w in backticked(body) if re.fullmatch(r"[A-Za-z_]+", w)}


def lexical_signs(text):
    """Знаки таблиц «Операторы» и «Пунктуация» раздела «Лексика» (фича 0298).

    ⚠️ Таблицы читаются ВМЕСТЕ: деление на операторы и пунктуацию — решение
    изложения, а не языка, и терминал `->` живёт во второй, а `:=` — в первой.
    ⚠️ Буквенные записи отбрасываются: во втором столбце стоят пояснения
    (пояснение «ветвь match»), и без фильтра `match` попал бы в знаки.
    """
    signs = set()
    for heading in ("== Операторы", "== Пунктуация"):
        body = lexical_table(text, heading, "разделе «Лексика»")
        for item in backticked(body):
            item = item.replace("\\", "")
            if not re.fullmatch(r"[A-Za-z_]+", item):
                signs.add(item)
    return signs


def highlight_sections(text):
    """Секции файла `takt-keywords.txt` как словарь `имя → множество слов`."""
    sections, name = {}, None
    for line in text.split("\n"):
        item = line.strip()
        if not item or item.startswith("#"):
            continue
        if item.startswith("[") and item.endswith("]"):
            name = item[1:-1]
            sections[name] = set()
            continue
        if name is not None:
            sections[name].add(item)
    return sections


def syntax_words(text):
    """Слова из регулярок подсветки `takt.sublime-syntax`.

    Берутся правила ключевых слов, константных литералов и модификатора формата —
    те, что выделяются в блоках кода как элементы языка. Имена типов идут своим
    правилом и с лексером не сверяются (ключевыми словами они не являются — фича
    0201).
    """
    words = set()
    for scope in ("keyword.control.takt", "constant.language.takt", "storage.modifier.takt"):
        for match in re.finditer(
            r"- match: \\b\(([^)]*)\)\\b\n\s*scope: " + re.escape(scope), text
        ):
            words.update(match.group(1).split("|"))
    return words


def check(doc_text, lexer_text, grammar_text, list_text=None, syntax_text=None,
          lexical_text=None):
    """Возвращает список находок `(класс, элемент, пояснение)`; пустой — гейт пройден."""
    problems = []

    keywords_doc = doc_keywords(doc_text)
    keywords_code = lexer_keywords(lexer_text)
    punct_doc = doc_punctuation(doc_text)
    punct_code = punctuation_terminals(grammar_terminals(grammar_text)) - PUNCT_EXCLUDED

    for name, group in (
        ("список ключевых слов документа", keywords_doc),
        ("таблица KEYWORDS лексера", keywords_code),
        ("сводка пунктуации документа", punct_doc),
        ("терминалы-знаки грамматики", punct_code),
    ):
        if not group:
            sys.exit(f"ОШИБКА: {name} пуст — гейт проверял бы пустое множество.")

    for word in sorted(keywords_code - keywords_doc - CONTEXTUAL):
        problems.append(("K1", word, "ключевое слово лексера отсутствует в списке документа"))
    for word in sorted(keywords_doc - keywords_code):
        problems.append(("K2", word, "слово документа не является ключевым в лексере"))
    for sign in sorted(punct_doc - punct_code):
        problems.append(("P1", sign, "знак документа отсутствует среди терминалов грамматики"))
    for sign in sorted(punct_code - punct_doc):
        problems.append(("P2", sign, "терминал грамматики отсутствует в сводке документа"))

    if list_text is not None and syntax_text is not None:
        problems += check_highlight(list_text, syntax_text, keywords_code)
    if lexical_text is not None:
        problems += check_lexical(lexical_text, keywords_code, punct_code)

    return problems


def check_lexical(lexical_text, keywords_code, punct_code):
    """Таблицы раздела «Лексика» против лексера и грамматики (фича 0298).

    Раздел — первое, что читает изучающий язык, и до 0298 он не сверялся ничем:
    замер 2026-08-20 нашёл в нём 43 ключевых слова против 47 в лексере (не было
    `after`, `at`, `clock`, `every` — фичи 0134 и 0187). Знаки при этом совпадали
    ровно, но держалось это на случайности, а не на проверке.
    """
    problems = []
    words = lexical_keywords(lexical_text)
    signs = lexical_signs(lexical_text)
    for name, group in (
        ("таблица ключевых слов раздела «Лексика»", words),
        ("таблицы знаков раздела «Лексика»", signs),
    ):
        if not group:
            sys.exit(f"ОШИБКА: {name} пуста — гейт проверял бы пустое множество.")

    for word in sorted(keywords_code - words - CONTEXTUAL):
        problems.append(("L1", word, "ключевое слово лексера отсутствует в разделе «Лексика»"))
    for word in sorted(words - keywords_code):
        problems.append(("L2", word, "раздел «Лексика» называет слово, которого в лексере нет"))
    # Контекстные слова LTL в лексере есть, поэтому предыдущая проверка их не
    # видит, - а называть их ключевыми раздел не вправе: прогон 2026-08-20
    # показал, что `var X: u8 := 1;` компилируется, и `SY-002` сам перечисляет
    # их среди допустимых идентификаторов. Прежде раздел утверждал обратное.
    for word in sorted(words & CONTEXTUAL):
        problems.append(
            ("L2", word, "контекстное слово названо ключевым: вне своей позиции это обычное имя")
        )
    for sign in sorted(punct_code - signs):
        problems.append(("L3", sign, "терминал грамматики отсутствует в таблицах раздела «Лексика»"))
    for sign in sorted(signs - punct_code):
        problems.append(("L4", sign, "раздел «Лексика» называет знак, которого в грамматике нет"))
    return problems


def check_highlight(list_text, syntax_text, keywords_code):
    """Списки подсветки документа против лексера и друг против друга (фича 0240)."""
    problems = []
    sections = highlight_sections(list_text)
    extra = sections.get(HIGHLIGHT_EXTRA_SECTION, set())
    listed = sections.get("keywords", set()) | sections.get("constants", set())
    if not listed:
        sys.exit(
            f"ОШИБКА: в {os.path.relpath(KEYWORD_LIST, ROOT)} нет секций "
            "[keywords]/[constants] — гейт проверял бы пустое множество."
        )
    from_syntax = syntax_words(syntax_text)
    if not from_syntax:
        sys.exit(
            f"ОШИБКА: в {os.path.relpath(SYNTAX, ROOT)} не найдены правила подсветки "
            "ключевых слов — гейт проверял бы пустое множество."
        )

    expected = keywords_code - CONTEXTUAL - HIGHLIGHT_EXCLUDED
    for word in sorted(expected - listed):
        problems.append(("H1", word, "ключевое слово лексера не выделяется в тексте документа"))
    for word in sorted(listed - keywords_code - extra):
        problems.append(("H2", word, "документ выделяет слово, которого в лексере нет"))
    for word in sorted(listed | extra) :
        if word not in from_syntax:
            problems.append(("H3", word, "слово выделяется в тексте, но не в блоках кода"))
    for word in sorted(from_syntax - listed - extra):
        problems.append(("H4", word, "блоки кода выделяют слово, которого нет в списке текста"))
    return problems


def report(problems):
    print("Лексика приложения «Грамматика» разошлась с языком (фича 0160):", file=sys.stderr)
    for kind, item, why in problems:
        print(f"  [{kind}] `{item}` — {why}", file=sys.stderr)
    print(
        "\nПосле фичи 0160 описание грамматики в проекте одно —\n"
        "book/src/appendix-grammar/index.typ. Приведите его абзацы «Ключевые слова»\n"
        "и «Операторы и пунктуация» к языку (правило 24: языковая фича обязана\n"
        "пройти стадию «Документирование»). Контекстные слова LTL —\n"
        f"{' '.join(sorted(CONTEXTUAL))} — в список не вносятся: вне аннотации\n"
        ": [LTL] … ; они обычные идентификаторы.\n"
        "Классы H1…H4 (фича 0240) говорят о ПОДСВЕТКЕ: списки\n"
        "book/takt-keywords.txt (инлайн-код) и book/takt.sublime-syntax (блоки кода)\n"
        "обязаны покрывать лексер и совпадать между собой; слово, выделяемое\n"
        "намеренно без своего ключевого слова, объявляется в секции [extra].\n"
        "Классы L1…L4 (фича 0298) говорят о РАЗДЕЛЕ «Лексика»\n"
        "(book/src/02-lexical/index.typ): его таблицы ключевых слов, операторов и\n"
        "пунктуации обязаны совпадать с лексером и терминалами грамматики —\n"
        "раздел читают первым, и слово, которого в нём нет, читатель считает\n"
        "свободным именем.",
        file=sys.stderr,
    )


def self_test():
    """Проверяет, что ловушки взведены: каждый класс обязан быть пойман.

    Без этого зелёный гейт неотличим от гейта, пропускающего всё: сам по себе он
    говорит «расхождений нет», и на сломанном разборе сказал бы то же.
    """
    doc = (
        "Проза документа.\n\n"
        f"{MARK_KEYWORDS} `as` `model`\n`state`.\n\n"
        f"{MARK_PUNCT} присваивание `:=`; прочие `.` `-->`.\n"
    )
    lexer = 'static KEYWORDS: phf::Map<&str, Token> = phf_map! {\n'
    lexer += '    "as" => Token::As,\n    "model" => Token::Model,\n'
    lexer += '    "while" => Token::While,\n    "X" => Token::LtlNext,\n};\n'
    grammar = 'extern {\n    enum Token<\'input> {\n'
    grammar += '        identifier => Token::Identifier(<&\'input str>),\n'
    grammar += '        ":=" => Token::ColonAssign,\n        "." => Token::Member,\n'
    grammar += '        "=>" => Token::FatArrow,\n        "as" => Token::As,\n    }\n}\n'

    # Списки подсветки: по одному расхождению каждого класса H.
    # `while` есть в лексере и нет в списке (H1), `ref` есть в списке и нет в
    # лексере пробы (H2), `always` выделяется в тексте и не выделяется в блоках
    # (H3), `enter` - наоборот (H4).
    keyword_list = "[keywords]\nas\nmodel\nref\nalways\n\n[types]\nbit\n\n[extra]\nalways\n"
    syntax = (
        "  main:\n"
        "    - match: \\b(as|model|ref|enter)\\b\n      scope: keyword.control.takt\n"
    )

    # Раздел "Лексика": `while` есть в лексере и нет в таблице (L1),
    # `pragma` названо разделом и не является ключевым (L2), терминал `.` мимо
    # таблиц знаков (L3), знак `-->` назван разделом и в грамматике отсутствует
    # (L4) - реальный случай 0201, здесь он ловится вторым списком.
    lexical = (
        "== Ключевые слова\n#table(\n    table.header([Назначение], [Слова],),\n"
        "    [Прочие], [`as`, `model`, `pragma`, `X`],\n  )\n\n"
        "== Операторы\n#table(\n    table.header([Категория], [Операторы],),\n"
        "    [Присваивание], [`:=` `-->`],\n  )\n\n"
        "== Пунктуация\n#table(\n    table.header([Знак], [Назначение],),\n"
        "    [`=>`], [ветвь `match`],\n  )\n\n== Литералы\n"
    )

    found = {
        (kind, item)
        for kind, item, _ in check(doc, lexer, grammar, keyword_list, syntax, lexical)
    }
    expected = {
        ("K1", "while"),  # слово лексера мимо списка документа
        ("K2", "state"),  # слово документа мимо лексера
        ("P1", "-->"),    # знак документа мимо грамматики (реальный случай 0201)
        ("P2", "=>"),     # терминал грамматики мимо документа (реальный случай 0189)
        ("H1", "while"),  # ключевое слово лексера не выделяется в тексте
        ("H2", "ref"),    # выделяется слово, которого в лексере пробы нет
        ("H3", "always"), # выделяется в тексте, но не в блоках кода
        ("H4", "enter"),  # выделяется в блоках кода, но не в тексте
        ("L1", "while"),  # слово лексера мимо таблицы раздела «Лексика»
        ("L2", "X"),      # контекстное слово LTL названо ключевым
        ("L2", "pragma"), # раздел называет слово, которого в лексере нет
        ("L3", "."),      # терминал грамматики мимо таблиц знаков раздела
        ("L4", "-->"),    # раздел называет знак, которого в грамматике нет
    }
    if found != expected:
        sys.exit(
            "САМОПРОВЕРКА ПРОВАЛЕНА: ловушки дали "
            f"{sorted(found)} вместо {sorted(expected)} — гейт зеленеет по случайности"
        )
    if ("K1", "X") in found:
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: контекстное слово LTL принято за пропуск")

    # Обратная сторона: согласованный вход ложных находок давать не должен -
    # иначе "ловушки ловят" означало бы лишь "ловят всё подряд".
    clean_doc = (
        f"{MARK_KEYWORDS} `as` `model`\n`while`.\n\n"
        f"{MARK_PUNCT} присваивание `:=`; прочие `.` `=>`.\n"
    )
    clean_list = "[keywords]\nas\nmodel\nwhile\n\n[types]\nbit\n\n[extra]\nenter\n"
    clean_syntax = (
        "  main:\n"
        "    - match: \\b(as|model|while|enter)\\b\n      scope: keyword.control.takt\n"
    )
    clean_lexical = (
        "== Ключевые слова\n#table(\n    table.header([Назначение], [Слова],),\n"
        "    [Прочие], [`as`, `model`, `while`],\n  )\n\n"
        "== Операторы\n#table(\n    table.header([Категория], [Операторы],),\n"
        "    [Присваивание], [`:=` `.`],\n  )\n\n"
        "== Пунктуация\n#table(\n    table.header([Знак], [Назначение],),\n"
        "    [`=>`], [ветвь `match`],\n  )\n\n== Литералы\n"
    )
    clean = check(
        clean_doc, lexer, grammar, clean_list, clean_syntax, clean_lexical
    )
    if clean:
        sys.exit(f"САМОПРОВЕРКА ПРОВАЛЕНА: согласованный вход дал находки: {clean}")

    # Раскладка таблицы принадлежит документу: закрывающая скобка стоит и своей
    # строкой, и в конце последней. Обе формы обязаны читаться одинаково, а
    # скобка внутри обратных кавычек - не считаться скобкой таблицы.
    inline = (
        "== Ключевые слова\n#table(columns: 2,\n    align: (auto,auto,),\n"
        "    [Прочие], [`as`, `model`, `while`],)\n\n== Литералы\n"
    )
    if lexical_keywords(inline) != {"as", "model", "while"}:
        sys.exit(
            "САМОПРОВЕРКА ПРОВАЛЕНА: таблица с закрывающей скобкой в строке "
            "прочитана не целиком"
        )
    parens = (
        "== Операторы\n#table(columns: 2,\n    [Присваивание], [`:=`],)\n\n"
        "== Пунктуация\n#table(columns: 2,\n"
        "    [`( )`], [группировка],\n    [`=>`], [ветвь `match`],)\n\n== Литералы\n"
    )
    if "=>" not in lexical_signs(parens):
        sys.exit(
            "САМОПРОВЕРКА ПРОВАЛЕНА: скобка в обратных кавычках оборвала чтение таблицы"
        )

    # Третья сторона: разметка без списка обязана быть ошибкой, а не пустотой.
    # Подпроба идёт отдельным процессом (проверка завершает процесс `sys.exit`),
    # её вывод глушится: ожидаемая ошибка в логе предкоммита читалась бы отказом.
    probe = subprocess.run(
        [sys.executable, os.path.abspath(__file__), "--self-test-empty"],
        capture_output=True,
        check=False,
    )
    if probe.returncode == 0:
        sys.exit("САМОПРОВЕРКА ПРОВАЛЕНА: документ без абзаца списка не дал ошибки")

    print("  самопроверка гейта: ловушка взведена (12 классов ловятся, согласованный вход — нет)")


def self_test_empty():
    """Подпроба: документ без абзацев-списков обязан валить гейт."""
    check("Проза без списков.\n", 'static KEYWORDS = phf_map! {\n"as" => Token::As,\n};\n', "")


def main():
    if "--self-test-empty" in sys.argv[1:]:
        self_test_empty()
        return 0
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    try:
        doc_text = open(APPENDIX, encoding="utf-8").read()
        lexer_text = open(LEXER, encoding="utf-8").read()
        grammar_text = open(GRAMMAR, encoding="utf-8").read()
        list_text = open(KEYWORD_LIST, encoding="utf-8").read()
        syntax_text = open(SYNTAX, encoding="utf-8").read()
        lexical_text = open(LEXICAL, encoding="utf-8").read()
    except OSError as error:
        sys.exit(f"ОШИБКА: не прочитать источник: {error}")

    problems = check(
        doc_text, lexer_text, grammar_text, list_text, syntax_text, lexical_text
    )
    if problems:
        report(problems)
        return 1

    keywords = doc_keywords(doc_text)
    punct = doc_punctuation(doc_text)
    highlighted = highlight_sections(list_text)
    print(
        f"Лексика приложения «Грамматика»: {len(keywords)} ключевых слов "
        f"и {len(punct)} знаков сверены с лексером и грамматикой, расхождений нет."
    )
    print(
        f"Раздел «Лексика»: {len(lexical_keywords(lexical_text))} ключевых слов и "
        f"{len(lexical_signs(lexical_text))} знаков сверены с языком, расхождений нет."
    )
    print(
        "Списки подсветки документа: "
        f"{len(highlighted.get('keywords', ()))} ключевых слов и "
        f"{len(highlighted.get('types', ()))} типов сверены с лексером, расхождений нет."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

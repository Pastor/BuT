// Шаблон документа "Язык Takt - описание".
//
// Заменяет собой прежнее устройство сборки: метаданные и опции вывода жили в
// `book/book.toml`, вёрстка - семнадцатью строками `header-includes` на LaTeX
// (fancyhdr, `\@mkboth`, `\pretocmd`), подсветка ключевых слов в инлайн-коде -
// lua-фильтром `book/keywords.lua`. Здесь всё это - код на Typst.
//
// Что даёт шаблон разделам документа:
//   #part("Имя")            - страница-разделитель части
//   #example(read("..."))     - пример на Takt из внешнего файла
//   #code(read("..."), "c")   - фрагмент на другом языке из внешнего файла
//   врезка                  - обычная цитата Markdown-происхождения (`#quote`)
//
// `read()` разрешает путь относительно Файла, Где Записан Вызов, поэтому
// чтение примера обязано стоять в файле раздела (`#example(read("examples/x.takt"))`),
// а не внутри функции шаблона: спрятав `read` в `example`, мы получили бы поиск
// примеров рядом с этим шаблоном.

// ── Метаданные документа ───────────────────────────────────────────────────
// Поля `title`, `authors` и `description` живут здесь, а не в `book/book.toml`; проверка
// символов (`scripts/check-book-glyphs.py`) читает их отсюда - они попадают на
// титульный лист и в свойства PDF.
#let doc-title = "Язык Takt — описание"
#let doc-authors = ("Pastor <viruszold@gmail.com>",)
#let doc-description = "Справочное описание языка Takt (Typed, Automata, Known Timing) — DSL для спецификации и синтеза конечных автоматов промышленных систем управления."

// ── Слова, выделяемые в инлайн-коде ────────────────────────────────────────
// Список читается из `book/takt-keywords.txt` - одного файла на весь документ
// (контроль против таблицы `KEYWORDS` лексера - `scripts/check-book-keywords.py`).
#let _keyword-file = read("/takt-keywords.txt")

#let _section-words(name) = {
  let lines = _keyword-file.split("\n")
  let inside = false
  let words = ()
  for line in lines {
    let s = line.trim()
    if s.starts-with("#") or s == "" { continue }
    if s.starts-with("[") {
      inside = s == "[" + name + "]"
      continue
    }
    if inside { words.push(s) }
  }
  words
}

// Ключевые слова печатаются цветом и Жирным, имена типов - тем же цветом без
// жирного: так же различал их lua-фильтр `keywords.lua` прежней сборки (роли
// `\KeywordTok` и `\DataTypeTok` палитры tango).
#let takt-keywords = (_section-words("keywords") + _section-words("constants") + _section-words("extra"))
#let takt-types = _section-words("types")

// Цвет ключевого слова - `KeywordTok` палитры tango (та же, что в подсветке
// блоков кода, `book/takt.tmTheme`): инлайн-слово и слово в примере выглядят
// одинаково.
#let keyword-color = rgb("#204A87")

// ── Вставка кода из внешнего файла ─────────────────────────────────────────
#let code(body, lang) = raw(body, lang: lang, block: true)
#let example(body) = code(body, "takt")

// ── Части документа ────────────────────────────────────────────────────────
#let _part-counter = counter("part")

#let part(name) = {
  pagebreak(weak: true)
  _part-counter.step()
  [#metadata(name) #label("part-marker")]
  // Страница-разделитель: "Часть I" и название по центру - как в прежней
  // вёрстке (`\part` класса report).
  v(1fr)
  align(center)[
    #text(size: 14pt, weight: "bold")[
      Часть #context _part-counter.display("I")
    ]
    #v(1.2em)
    #text(size: 17pt, weight: "bold")[#name]
  ]
  v(2fr)
  pagebreak(weak: true)
}

// ── Оглавление ─────────────────────────────────────────────────────────────
// Собирается вручную: части - не заголовки (иначе они сбивали бы сплошную
// нумерацию глав 1...26), поэтому штатный `#outline()` их не увидел бы. Глубина -
// только главы, как в прежнем документе.
#let _toc() = context if target() == "paged" {
  heading(level: 1, numbering: none, outlined: false)[Содержание]
  {
    let items = query(selector.or(heading.where(level: 1), label("part-marker")))
    for it in items {
      let page-no = counter(page).at(it.location()).first()
      if it.func() == metadata {
        // Запись части: римский номер + название, полужирным.
        let part-no = _part-counter.at(it.location()).first()
        v(0.6em)
        strong[
          #box(width: 2.2em)[#numbering("I", part-no)]
          #it.value
          #box(width: 1fr, repeat[])
          #page-no
        ]
        v(0.2em)
      } else if it.body != [Содержание] {
        let nums = counter(heading).at(it.location())
        let no = if it.numbering == none { none } else { numbering("1", ..nums) }
        block(above: 0.5em, below: 0.5em)[
          #box(width: 2.2em)[#if no != none [#no]]
          #it.body
          #box(width: 1fr, repeat[])
          #page-no
        ]
      }
    }
  }
}

// ── Колонтитулы ────────────────────────────────────────────────────────────
// Верхний правый колонтитул несёт имя текущей главы - кроме первой страницы
// главы (там имя и так стоит заголовком) и страницы-разделителя части. Прежняя
// вёрстка добивалась того же переопределением `\chaptermark` и стиля `plain`
// класса report.
//
// Колонтитулы объявлены Отдельно и подставляются в каждый `set page`: правила
// `set` накапливаются, поэтому титульный лист, выключающий колонтитул
// (`header: none`), гасил бы его до конца документа - восстановить его
// последующим `set page(numbering: ...)` нельзя, там речь о другом поле. Именно
// так колонтитул пропал во всём документе при первой сборке.
#let _running-head = context {
  let this-page = here().page()
  let chapters = query(heading.where(level: 1)).filter(h => (h.location().page() <= this-page))
  if chapters.len() == 0 { return }
  let last = chapters.last()
  if last.location().page() == this-page { return }
  let parts = query(label("part-marker")).filter(p => (p.location().page() == this-page))
  if parts.len() > 0 { return }
  set text(size: 8pt)
  align(right)[#last.body]
  v(-0.6em)
  line(length: 100%, stroke: 0.4pt)
}

#let _page-number = context align(center)[
  #text(size: 8pt)[#counter(page).display("1")]
]

// ── Шаблон ─────────────────────────────────────────────────────────────────
#let book(body) = {
  set document(title: doc-title, author: doc-authors, description: doc-description)

  // Страница: бумага a4, поля 1.5 см со всех сторон, номер страницы снизу по центру.
  set page(paper: "a4",
    margin: 1.5cm,
    numbering: "1",
    header: _running-head,
    footer: _page-number,)

  // Текст: Fira Code 10 pt, русские переносы, выключка по формату.
  set text(font: "Fira Code", size: 10pt, lang: "ru", hyphenate: true)
  set par(justify: true, leading: 0.65em, spacing: 1.1em)

  // У Fira Code нет курсивного начертания: наклон синтезируется сдвигом. В
  // прежней сборке ту же роль играл `AutoFakeSlant=0.2` шрифтовых опций xelatex.
  show emph: it => context if target() == "paged" { box(skew(ax: -12deg, it.body)) } else { it }

  // Заголовки: глава - с новой страницы, нумерация сплошная (1...26), приложения
  // продолжают счёт - как было в mdBook.
  set heading(numbering: "1.1.1")
  show heading.where(level: 1): it => context if target() != "paged" { it } else {
    pagebreak(weak: true)
    block(above: 1.2em, below: 1.4em)[
      #text(size: 17pt, weight: "bold")[
        #if it.numbering != none [#counter(heading).display("1") #h(0.5em)]
        #it.body
      ]
    ]
  }
  show heading.where(level: 2): it => context if target() != "paged" { it } else {
    block(above: 1.6em, below: 0.9em)[
      #text(size: 12.5pt, weight: "bold")[#it.body]
    ]
  }
  show heading.where(level: 3): it => context if target() != "paged" { it } else {
    block(above: 1.3em, below: 0.8em)[
      #text(size: 10.5pt, weight: "bold")[#it.body]
    ]
  }
  // Четвёртый уровень появляется там, где файл раздела несёт второй заголовок
  // первого уровня: при переводе он опускается в подзаголовок вместе со своими
  // подразделами (приложение "Порождённый код примера").
  show heading.where(level: 4): it => context if target() != "paged" { it } else {
    block(above: 1.1em, below: 0.7em)[
      #text(size: 10pt, weight: "bold", style: "oblique")[#it.body]
    ]
  }

  // Подсветка блоков кода: определение синтаксиса Takt и палитра tango - те же
  // роли, что играли `takt.kate.xml` и `highlight-style` в прежней сборке.
  // Определений три: язык проекта и два целевых/метаязыка, которых нет у
  // syntect - Structured Text и EBNF. Без них блоки ```st и
  // ```ebnf печатались чёрным, тогда как соседний ```c подсвечен.
  set raw(syntaxes: ("/takt.sublime-syntax", "/st.sublime-syntax", "/ebnf.sublime-syntax"),
    theme: "/takt.tmTheme",)
  show raw.where(block: true): it => block(width: 100%,
    above: 1.1em,
    below: 1.1em,
    breakable: true,
    text(size: 9pt, it),)
  // Инлайн-код, равный ключевому слову или имени типа, выделяется — роль
  // `keywords.lua`. Одиночные `X`/`F`/`G`/`U`/`R` и `_` не выделяются намеренно:
  // в прозе они значат не операторы LTL (то же решение было в прежнем фильтре).
  show raw.where(block: false): it => {
    if takt-keywords.contains(it.text) {
      text(fill: keyword-color, weight: "bold", it)
    } else if takt-types.contains(it.text) {
      text(fill: keyword-color, it)
    } else { it }
  }

  // Таблицы — «booktabs»: линии сверху, под заголовком и снизу, без вертикальных.
  set table(stroke: (_, y) => if y == 1 { (top: 0.5pt) } else { none },
    inset: (x: 5pt, y: 3.5pt),
    align: left + top,)
  show table: it => context if target() != "paged" { it } else { block(width: 100%, above: 1.1em, below: 1.1em)[
    #line(length: 100%, stroke: 0.6pt)
    #text(size: 9pt, it)
    #v(-0.4em)
    #line(length: 100%, stroke: 0.6pt)
  ] }

  // Врезка (в исходниках Markdown это была цитата `>`): отступ слева, без рамки —
  // как печатал прежний конвейер.
  show quote.where(block: true): it => block(inset: (left: 1.2em, rest: 0pt),
    above: 1.1em,
    below: 1.1em,
    it.body,)

  set list(indent: 0.8em, spacing: 0.7em)
  set enum(indent: 0.8em, spacing: 0.7em)
  show link: it => text(fill: rgb("#0000CC"), it)

  // ── Титульный лист ───────────────────────────────────────────────────────
  set page(numbering: none, header: none, footer: none)
  v(1fr)
  align(center)[
    #text(size: 15pt)[#doc-title]
    #v(1.6em)
    #text(size: 10pt)[#doc-authors.join(", ")]
  ]
  v(2fr)
  pagebreak()

  // Дальше — сплошная арабская нумерация, начиная с оглавления (как прежде).
  // Колонтитулы возвращаются явно: см. предупреждение при их объявлении.
  set page(numbering: "1", header: _running-head, footer: _page-number)
  counter(page).update(1)
  _toc()

  body
}

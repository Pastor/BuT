= Грамматика (EBNF) <sec-appendix-grammar>
Формальный синтаксис языка Takt в нотации #strong[ISO EBNF] (ISO/IEC
14977). Это #strong[нормативный] источник по структуре программы:
текстовые разделы описывают смысл конструкций, а грамматика — их
допустимую форму.

== Обозначения
<обозначения>
#table(columns: 2,
    align: (auto,auto,),
    table.header([Запись], [Значение],),
    [`rule = … ;`], [определение правила],
    [`a b`], [конкатенация (последовательность)],
    [`a \| b`], [альтернатива],
    [`[ a ]`], [опционально (0 или 1 раз)],
    [`{ a }`], [повторение (0 и более раз)],
    [`( a \| b )`], [группировка],
    [`"текст"`], [терминал (литерал)],
    [`lowercase`], [нетерминал],)

== Точка входа
<точка-входа>
Программа — последовательность элементов верхнего уровня.
```ebnf
source_unit = { model_element } ;
```

== Элементы верхнего уровня и состояния
<элементы-верхнего-уровня-и-состояния>
`model_element` допустим на верхнем уровне и внутри модели;
`state_element` — только в теле состояния (там лишь управление
автоматом, без объявлений).
```ebnf
model_element
    = import_define | type_define | variable_define | function_define
    | model | state_define | formula_define | inline_formula_define
    | condition_define | invariant_define | named_block
    | enum_define | struct_define
    | address_define | clock_define | ";" ;

state_element
    = named_block
    | "next" identifier ";"
    | "ref" identifier [ ":" condition ] ";"
    | inline_formula_define
    | invariant_define
    | every_define
    | ";" ;
```

== Импорт
<импорт>```ebnf
import_define
    = "import" import_path ";"
    | "import" import_path "as" identifier ";"
    | "import" "*" "as" identifier "from" import_path ";"
    | "import" "{" import_rename { "," import_rename } "}" "from" import_path ";" ;

import_path     = string_literal | identifier_path ;
import_rename   = identifier | identifier "as" identifier ;
identifier_path = identifier { "::" identifier } ;
```

== Типы
<типы>```ebnf
type_define = "type" identifier "=" type ";" ;

type = identifier                            (* bit, u8, duration, MyType *)
     | identifier "(" integer "," integer ")" [ identifier ]
                                             (* fixed-point: q(8, 8), q(8, 8) sat *)
     | "[" type ";" integer "]" ;            (* массив: [bit;8] *)
```

Конструктор `q(m, n)` записан через `identifier`: `q` — #strong[не]
ключевое слово, имя конструктора проверяет семантика. Иначе `q`
перестало бы годиться как имя переменной или типа.

Постфиксный модификатор формата записан `identifier` по той же причине:
`sat` ключевым словом не является, и допустимость слова проверяет
семантика (единственное допустимое — `sat`, прочее отвергается
`SE-104`).

== Переменные, константы, параметры, порты
<переменные-константы-параметры-порты>
Инициализатор — через `:=`. `const` и `parameter` требуют его (у
параметра это значение по умолчанию). Адрес порта задаётся отдельной
связкой `at`.
```ebnf
variable_define
    = "var"       identifier [ ":" type ] [ ":=" expression ] ";"
    | "const"     identifier [ ":" type ] ":=" expression ";"
    | "parameter" identifier [ ":" type ] ":=" expression ";"
    | "in"        identifier [ ":" type ] [ "at" address_expr ] [ ":=" expression ] ";"
    | "out"       identifier [ ":" type ] [ "at" address_expr ] [ ":=" expression ] ";"
    | "inout"     identifier [ ":" type ] [ "at" address_expr ] [ ":=" expression ] ";" ;

address_expr = address_literal | expression ;   (* без `:=` и без `?:` *)
```

`parameter` объявляется только на уровне модели: настройку задают в
месте инстанцирования, а у оператора такого места нет.

Связка `at` — позиция размещения: только здесь #strong[адресный
литерал] `0xADDR:бит` законен как запись адреса. В обычном выражении та
же запись отвергается (`SY-008`): адрес есть свойство размещения, а не
значение.

== Функции
<функции>```ebnf
function_define
    = [ attribute ] [ "extern" ] "fn" identifier parameter_list
      [ "->" type ] ( ";" | block_statement ) ;

attribute           = "[" identifier "]" ;
parameter_list      = "(" [ fn_parameter { "," fn_parameter } ] ")" ;
fn_parameter        = identifier ":" parameter_type_expr | parameter_type_expr ;
parameter_type_expr = "[" type ";" integer "]" | expression ;
```

`extern fn` — объявление без тела; `fn` — определение с телом.
Атрибут `attribute` — указание о размещении функции в порождённом коде:
`inline` (подставить тело в место вызова) и `noinline` (не подставлять).
Имя атрибута — #strong[обычный идентификатор], новых ключевых слов
конструкция не вводит; набор допустимых имён судит семантика
(`SE-126`).
Нетерминал `fn_parameter` — параметр функции; ключевое слово
`"parameter"` (в кавычках выше) — объявление параметра модели, это
разные вещи.

== Модель, состояние, перечисление, структура
<модель-состояние-перечисление-структура>```ebnf
model = "model" identifier [ "=" expression ] "{" { model_element } "}" ;

state_define = state_kind identifier [ "=" expression ]
               ( "{" { state_element } "}" | ";" ) ;
state_kind   = "start" | "state" ;

enum_define  = "enum" identifier "{" enum_variant { "," enum_variant } "}" ;
enum_variant = identifier [ "=" integer ] ;

struct_define = "struct" identifier "{" [ struct_field { "," struct_field } ] "}" ;
struct_field  = identifier ":" type ;
```

Выражение реализации (`= expression` у модели и состояния) —
композиция моделей `|`/`+` со скобками. Настройка модели при
инстанцировании записывается формой вызова —
`Model(имя := выражение, …)`; отдельного правила у неё нет, это то же
выражение реализации.

== Адреса и время
<адреса-и-время>
Оператор `address` — второй источник адреса порта (приоритет: `at`
ниже `address`, `address` ниже внешней карты `--address-map`). `clock`
задаёт частоту тактирования модели, `every` — периодическое действие
внутри состояния.
```ebnf
address_define = "address" identifier "=" address_expr ";" ;
clock_define   = "clock" frequency ";" ;
every_define   = "every" duration block_statement ;
```

== Именованные условия, инварианты и блоки
<именованные-условия-инварианты-и-блоки>```ebnf
condition_define = "cond" identifier "=" condition ";" ;
invariant_define = "invariant" identifier "=" condition ";" ;
named_block      = identifier block_statement ;   (* enter / exit / always *)
```

Связка у `cond` и `invariant` — `=`, а не `:=`: это определение имени,
не запись. Инвариант допустим и на уровне модели, и внутри состояния.

Условие перехода — отдельная грамматика (в ней `=` означает
#strong[проверку равенства]). Приоритет (от низшего к высшему): `|` ·
`&` · `= !=` · `< > <= >=` · `+ -` · унарный `!` · первичные.
```ebnf
condition
    = condition "|" condition  | condition "&" condition
    | condition "=" condition  | condition "!=" condition
    | condition "<" condition  | condition ">" condition
    | condition "<=" condition | condition ">=" condition
    | condition "+" condition  | condition "-" condition
    | "!" condition | "(" condition ")"
    | condition "." member
    | condition "[" condition "]"
    | identifier "(" [ condition { "," condition } ] ")"
    | "#" address_literal | "#" integer          (* обращение к ячейке без имени *)
    | "after" duration | "after" ticks           (* выдержка *)
    | "after" identifier | "after" "(" condition ")"
    | "true" | "false" | integer | rational | duration | identifier ;
```

Выдержка `after` записывается литералом (`after 3m`), тактами
(`after 3t`), именем константы (`after DWELL`) либо выражением в скобках
(`after (BASE + 30s)`). Скобки обязательны: `after` связывает крепче
арифметики, поэтому `after A + 1s` означает `(after A) + 1s`.

Обращение `#АДРЕС` — чтение ячейки, у которой нет объявленного имени.
Ширину доступа задаёт то, что стоит #strong[над] обращением: в условии
это битовый доступ (`#0x100.4`), в выражении — ещё и приведение
(`#0x100 as u8`). Собственной ширины у обращения нет.

== Встроенные формулы: Guard и LTL
<встроенные-формулы-guard-и-ltl>```ebnf
inline_formula_define
    = ":" condition { "," condition } ";"
    | ":" "[" "Guard" "]" condition { "," condition } ";"
    | ":" "[" "LTL"   "]" ltl_expr  { "," ltl_expr  } ";" ;
```

LTL-выражения (приоритет от низшего к высшему): `->`
(правоассоциативная) · `|` · `&` · `U R` · унарные `! X F G`.
```ebnf
ltl_expr    = ltl_implies ;
ltl_implies = ltl_or "->" ltl_implies | ltl_or ;
ltl_or      = ltl_or "|" ltl_and | ltl_and ;
ltl_and     = ltl_and "&" ltl_until_release | ltl_until_release ;
ltl_until_release
    = ltl_until_release "U" ltl_unary | ltl_until_release "R" ltl_unary | ltl_unary ;
ltl_unary   = "!" ltl_unary | "X" ltl_unary | "F" ltl_unary | "G" ltl_unary | ltl_primary ;
ltl_primary = "true" | "false" | identifier | "(" ltl_expr ")" ;
```

`X F G U R LTL Guard` — обычные идентификаторы вне аннотации
`: [LTL] … ;`.

== Выражения
<выражения>
#strong[Присваивание — `:=`; равенство — `=`] (не `==`, он изъят из
языка). Приоритет (от низшего к высшему): `?:` · `||` · `&&` · `= !=` ·
`< > <= >=` · `|` · `^` · `& as` · `<< >>` · `+ -` · `* / %` · `**` ·
унарные `! ~ + -` · первичные.

#quote(block: true)[
#strong[Осторожно.] Присваивания в этой цепочке нет. `:=` — не
операция выражения: он законен ровно в трёх позициях языка — оператор
тела (`statement_expr`), шаг цикла `for` (там же) и именованный аргумент
вызова (`call_arg`, форма `Model(kp := 0.25)`). Запись внутри значения
— `seen := (value := 3) + 1;` — отвергается грамматикой (`SY-006`).
]
```ebnf
statement_expr = chain13 ":=" expression | expression ;
call_arg       = chain13 ":=" expression | expression ;
```

`chain13` — левая часть: вся цепочка приоритетов #strong[ниже]
присваивания, то есть любое выражение, кроме самого присваивания и
тернарного. Правая часть — полное `expression`: цепочка `a := b := c`
есть присваивание внутри значения и потому незаконна.
```ebnf
expression
    = expression "?" expression ":" expression               (* тернарный *)
    | expression "||" expression | expression "&&" expression
    | expression "=" expression  | expression "!=" expression (* равенство *)
    | expression "<" expression  | expression ">" expression
    | expression "<=" expression | expression ">=" expression
    | expression "|" expression  | expression "^" expression | expression "&" expression
    | expression "<<" expression | expression ">>" expression
    | expression "as" type
    | expression "+" expression  | expression "-" expression
    | expression "*" expression  | expression "/" expression | expression "%" expression
    | expression "**" expression
    | "!" expression | "~" expression | "+" expression | "-" expression
    | "{" expression { "," expression } "}"                  (* инициализатор массива *)
    | identifier "(" [ call_arg { "," call_arg } ] ")"       (* вызов функции *)
    | expression "[" expression "]"                          (* индекс *)
    | expression "[" [ integer ] ":" [ integer ] "]"         (* срез *)
    | "#" address_literal | "#" integer                      (* ячейка без имени *)
    | expression "." member
    | "(" expression ")"
    | "true" | "false" | integer | rational | duration
    | string_literal { string_literal } | identifier ;

member = identifier | integer ;    (* x.flag или x.0 (бит) *)
```

`address_literal` — единый лексический токен `0xADDR` или `0xADDR:бит`
(не `integer ":" integer`), что снимает конфликт с тернарным `?:`. В
выражении он законен #strong[только] после `#` и в позиции размещения
(`at`, `address`); отдельно стоящий адресный литерал отвергается
(`SY-008`).

== Операторы
<операторы>```ebnf
statement
    = "if" expression block_statement
    | "if" expression block_statement "else" statement
    | "loop" block_statement | "loop" loop_cond block_statement
    | "while" loop_cond block_statement
    | "for" [ simple_statement ] ";" [ expression ] ";" [ statement_expr ]
      ( block_statement | ";" )
    | "match" loop_cond "{" { match_arm } "}"
    | block_statement
    | "assembly" [ string_literal ] block_statement
    | "formula"  [ string_literal ] formula_block
    | inline_formula_define
    | simple_statement ";"
    | "continue" ";" | "break" ";" | "return" [ expression ] ";" ;

block_statement  = "{" { statement } "}" ;
simple_statement = local_variable_define | statement_expr ;
local_variable_define
    = "var"   identifier [ ":" type ] [ ":=" expression ]
    | "const" identifier [ ":" type ] ":=" expression ;

match_arm     = match_pattern { "," match_pattern } "=>" block_statement ;
match_pattern = "_" | loop_cond ;
```

`if` без `else` — «открытый» оператор; `else` привязывается к
ближайшему `if`. `while условие { тело }` — синоним
`loop условие { тело }`: различает их только печать форматтера,
семантика одинакова.

`loop_cond` — как `expression`, но первый токен не `{` (снимает
конфликт `loop { тело }` vs `loop {init} { тело }`; для
инициализатора-массива в условии — скобки: `loop ({1, 2}) { }`). По
той же причине `loop_cond` стоит и в `match`, и в образце ветви.

#quote(block: true)[
#strong[Осторожно.] Оператором-выражением может быть только форма с
эффектом — присваивание либо вызов функции. `x + 1;` и `#0x100.4;`
грамматика принимает как `statement_expr`, но отвергает действием
правила (`SY-007`): в EBNF это ограничение невыразимо, потому что LR(1)
не различает «с эффектом» и «без» до конца разбора.
]

== Лексические элементы
<лексические-элементы>```ebnf
identifier      = ( xid_start | "_" | "$" ) { xid_continue | "$" } ;

integer         = decimal_integer | hex_integer | "-" decimal_integer ;
decimal_integer = digit { digit | "_" } ;
hex_integer     = "0x" hex_digit { hex_digit | "_" } ;
rational        = decimal_integer "." decimal_integer [ exponent ]
                | decimal_integer exponent ;
exponent        = ( "e" | "E" ) [ "-" ] decimal_integer ;

duration        = duration_part { duration_part } ;   (* 250ms, 1m30s *)
duration_part   = decimal_integer time_unit ;
time_unit       = "ns" | "us" | "ms" | "s" | "m" | "h" ;
frequency       = decimal_integer ( "Hz" | "kHz" | "MHz" ) ;
ticks           = decimal_integer "t" ;

address_literal = "0x" hex_digit { hex_digit | "_" } [ ":" decimal_integer ] ;

string_literal  = '"' { string_char } '"' | "'" { string_char } "'" ;
line_comment    = "//"  { any_char_except_newline } ( newline | eof ) ;
doc_comment     = "///" { any_char_except_newline } ( newline | eof ) ;
```

Идентификаторы — по Unicode (XID\_Start/XID\_Continue); ключевые слова
идентификаторами быть не могут. Блочные комментарии `/* … */` не
поддерживаются.

Составная длительность записывается #strong[строго по убыванию] единиц:
`1m30s` законно, `30s1m` — нет. Регистр единицы значим: у времени `m`
и `M` различались бы в 60 раз, у частоты `MHz` и `mHz` — в миллион,
поэтому молчаливого приравнивания нет.

#strong[Ключевые слова:] `_` `address` `after` `as` `assembly` `at`
`break` `clock` `cond` `const` `continue` `else` `enum` `every` `extern`
`false` `fn` `for` `formula` `if` `import` `in` `inout`
`invariant` `loop` `match` `model` `next` `out` `parameter` `ref`
`return` `start` `state` `struct` `true` `type` `var` `while`.

Слово `_` — образец «любое значение» в ветви `match`. Началом
идентификатора тот же знак остаётся: `_x` — имя, `_` — ключевое
слово.

Слово `from` в списке #strong[не значится]: ключевым оно является
только в директиве импорта (`import … from "файл";`). Вне её это
обычное имя, и `struct Line { from: u8, to: u8 }` — законное
объявление.

#strong[Операторы и пунктуация:] присваивание `:=`; арифметические
`+ - * / % **`; побитовые `& | ^ ~ << >>`; логические `&& || !`;
равенство/сравнение `= != < <= > >=`; прочие `?` `.` `:` `,` `;` `( )`
`{ }` `[ ]` `#` `->` `=>`.

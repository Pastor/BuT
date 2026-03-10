# BuT — Язык спецификации конечных автоматов

> **Версия документа:** 1.0
> **Автор анализа:** Системный аналитик
> **Дата:** 2026-03-10
> **Репозиторий:** https://github.com/Pastor/BuT

---

## Содержание

1. [Общее описание проекта](#1-общее-описание-проекта)
2. [Архитектура проекта](#2-архитектура-проекта)
3. [Лексика языка](#3-лексика-языка)
4. [Расширенная форма Бэкуса–Наура (РБНФ)](#4-расширенная-форма-бэкусанаура-рбнф)
5. [Система типов](#5-система-типов)
6. [Переменные и атрибуты](#6-переменные-и-атрибуты)
7. [Модели и конечные автоматы](#7-модели-и-конечные-автоматы)
8. [Состояния](#8-состояния)
9. [Переходы (ref)](#9-переходы-ref)
10. [Условия (Condition)](#10-условия-condition)
11. [Выражения (Expression)](#11-выражения-expression)
12. [Функции](#12-функции)
13. [Свойства (Property)](#13-свойства-property)
14. [Аннотации](#14-аннотации)
15. [Формулы LTL](#15-формулы-ltl)
16. [Импорт](#16-импорт)
17. [Структуры и перечисления](#17-структуры-и-перечисления)
18. [Композиция поведения](#18-композиция-поведения)
19. [Кодогенерация](#19-кодогенерация)
20. [Полные примеры программ](#20-полные-примеры-программ)

---

## 1. Общее описание проекта

**BuT** (от англ. *Behavior under Time* — «поведение во времени») — предметно-ориентированный язык программирования (DSL) для формальной спецификации, симуляции и кодогенерации **конечных автоматов** (FSM — Finite State Machines) и иерархических систем управления.

### 1.1 Назначение

BuT предназначен для разработчиков встроенного программного обеспечения, инженеров автоматизации и специалистов по формальным методам верификации, которым необходимо:

- Описывать поведение систем управления в виде конечных автоматов;
- Формально верифицировать свойства поведения с помощью логики LTL (Linear Temporal Logic);
- Генерировать рабочий код для различных целевых платформ (C, Verilog, IEC 61131-3 ST, ассемблер LC-3, ARM Thumb);
- Визуализировать автоматы в формате DOT/Graphviz.

### 1.2 Ключевые концепции

| Концепция | Описание |
|---|---|
| **model** | Конечный автомат — базовая единица программы |
| **state** | Состояние внутри автомата |
| **ref** | Переход между состояниями (с условием или без) |
| **port** | Переменная, отображённая на аппаратный регистр |
| **formula** | Встроенная LTL-спецификация |
| **behavior** | Выражение композиции автоматов |
| **property** | Именованное поведение или значение (enter, exit, start и т.д.) |

---

## 2. Архитектура проекта

Проект организован как монорепозиторий Cargo с несколькими крейтами:

```
BuT/
├── grammar/
│   ├── but/          # Парсер и AST языка BuT (but-grammar)
│   └── ltl/          # Парсер логики LTL (ltl-grammar)
├── middleware/       # Семантический анализ (but-middleware)
├── simulators/       # Симулятор FSM (but-simulators)
├── codegen/          # Кодогенераторы (but-codegen)
├── visual/           # Визуализация DOT (but-visual)
└── cli/              # Инструмент командной строки (but)
```

### 2.1 Зависимости между крейтами

```
but (CLI)
 ├── but-grammar      ← LALRPOP-парсер + AST
 ├── but-middleware   ← Семантический анализ
 ├── but-simulators   ← Симуляция FSM
 │    └── but-grammar
 ├── but-codegen      ← Кодогенерация
 │    └── but-grammar
 └── but-visual       ← Генерация DOT
      └── but-simulators

ltl-grammar           ← Независимый парсер LTL
```

### 2.2 Технологический стек

| Компонент | Технология |
|---|---|
| Парсер | LALRPOP 0.23 (LALR(1) с восстановлением ошибок) |
| Язык реализации | Rust 2021 edition, nightly toolchain |
| Генерация LTL | Отдельный LALRPOP-парсер (`ltl.lalrpop`) |
| Визуализация | DOT-формат (Graphviz) |
| Целевые платформы | C99, Verilog, IEC 61131-3 ST, LC-3 ASM, ARM Thumb |

---

## 3. Лексика языка

### 3.1 Идентификаторы

```
Identifier ::= [a-zA-Z_][a-zA-Z_0-9]*
```

Идентификаторы чувствительны к регистру. Unicode-идентификаторы поддерживаются через `unicode-xid`.

### 3.2 Зарезервированные слова

| Группа | Ключевые слова |
|---|---|
| Объявления | `model`, `state`, `fn`, `struct`, `enum`, `type` |
| Переменные | `let`, `mut`, `const`, `port`, `external`, `private` |
| Управление | `if`, `else`, `for`, `while`, `do`, `break`, `continue`, `return` |
| Специальные | `import`, `as`, `from`, `ref`, `is`, `abstract`, `template` |
| Формулы | `formula`, `assembly` |
| Литералы | `true`, `false` |

### 3.3 Литералы

| Тип | Примеры |
|---|---|
| Целые | `0`, `42`, `-7`, `1000` |
| Рациональные (с точкой) | `3.14`, `2.7182818284` |
| Шестнадцатеричные | `0xFF`, `0xFFB0`, `0x0000000` |
| Адресные | `0xFFB0:2` (адрес:смещение_бита) |
| Строковые | `"Hello"`, `u"Привет"` (unicode-строки) |
| Булевы | `true`, `false` |
| Hex-строки | `hex'01 FF A3'`, `hex"DEAD_BEEF"` |
| Инициализаторы | `{1, 2, 3}`, `{{1,2},{3,4}}` |

### 3.4 Комментарии

```
// Однострочный комментарий
/* Многострочный
   комментарий */
/** Документирующий комментарий */
```

### 3.5 Операторы

| Категория | Операторы |
|---|---|
| Арифметические | `+` `-` `*` `/` `%` `**` |
| Сравнения | `==` `!=` `<` `>` `<=` `>=` |
| Логические | `&&` `\|\|` `!` |
| Побитовые | `&` `\|` `^` `~` `<<` `>>` |
| Присваивания | `=` `+=` `-=` `*=` `/=` `%=` `\|=` `&=` `<<=` `>>=` |
| Инкремент | `++` `--` (префиксный и постфиксный) |
| Специальные | `->` (стрелка), `::` (путь), `as` (приведение) |

---

## 4. Расширенная форма Бэкуса–Наура (РБНФ)

### 4.1 Верхний уровень

```bnf
SourceUnit         ::= SourceUnitPart*

SourceUnitPart     ::= ImportDirective
                     | EnumDefinition
                     | StructDefinition
                     | TypeDefinition
                     | VariableDefinition
                     | GlobalAnnotationDefinition
                     | PropertyDefinition
                     | FunctionDefinition
                     | ModelDefinition
                     | FormulaDefinition
                     | ";"
```

### 4.2 Импорт

```bnf
ImportDirective    ::= "import" ImportPath ";"
                     | "import" ImportPath "as" Identifier ";"
                     | "import" "*" "as" Identifier "from" ImportPath ";"
                     | "import" "{" ImportRenameList "}" "from" ImportPath ";"

ImportPath         ::= StringLiteral | IdentifierPath

ImportRenameList   ::= ImportRename ("," ImportRename)*
ImportRename       ::= Identifier ("as" Identifier)?
```

### 4.3 Определение модели (автомата)

```bnf
ModelDefinition    ::= AnnotationDefinition*
                       ObjectType?
                       "model" Identifier ("=" Expression)?
                       "{" ModelPart* "}"

ObjectType         ::= "abstract" | "template"

ModelPart          ::= EnumDefinition
                     | StructDefinition
                     | TypeDefinition
                     | VariableDefinition
                     | GlobalAnnotationDefinition
                     | PropertyDefinition
                     | FunctionDefinition
                     | ModelDefinition
                     | StateDefinition
                     | FormulaDefinition
                     | ConditionDefinition
                     | ";"
```

### 4.4 Определение состояния

```bnf
StateDefinition    ::= AnnotationDefinition*
                       ObjectType?
                       "state" Identifier ("=" Expression)?
                       "{" StatePart* "}"

StatePart          ::= EnumDefinition
                     | StructDefinition
                     | TypeDefinition
                     | VariableDefinition
                     | GlobalAnnotationDefinition
                     | PropertyDefinition
                     | FunctionDefinition
                     | ModelDefinition
                     | FormulaDefinition
                     | ConditionDefinition
                     | ReferenceDeclaration
                     | ";"

ReferenceDeclaration ::= "ref" Identifier (":" Condition)? ";"
```

### 4.5 Типы данных

```bnf
Type               ::= Identifier
                     | "[" IntegerLiteral ":" Type "]"
```

**Примечание:** `[N : T]` — массив из N элементов типа T. Например:
- `[8 : bit]` — 8-битный массив (1 байт)
- `[32 : bit]` — 32-битный массив (4 байта)
- `[3 : [3 : u8]]` — матрица 3×3 байт

### 4.6 Объявление переменной

```bnf
VariableDefinition ::= AnnotationDefinition*
                       VariableAttribute+
                       Identifier ":" Type
                       ("=" Expression)?
                       ";"

VariableAttribute  ::= "const" | "let" | "mut" | "port" | "external" | "private"
```

### 4.7 Условие (Condition)

```bnf
Condition          ::= Condition ("==" | "!=") Condition
                     | Condition ("<" | ">" | "<=" | ">=") Condition
                     | Condition "|" Condition
                     | Condition "&" Condition
                     | Condition ("+" | "-") Condition
                     | "!" Condition
                     | Identifier
                     | Identifier "(" ConditionList ")"
                     | "(" Condition ")"
                     | Condition "." Member
                     | Identifier "[" IntegerLiteral "]"
                     | BoolLiteral | IntegerLiteral | RationalLiteral | HexLiteral
```

### 4.8 Выражение (Expression)

Выражения используют 14-уровневый приоритет (от наинизшего к наивысшему):

```bnf
Expression         ::= Expression AssignOp Expression        (* уровень 14: присваивания *)
                     | Expression "||" Expression             (* уровень 13 *)
                     | Expression "&&" Expression             (* уровень 12 *)
                     | Expression ("==" | "!=") Expression    (* уровень 11 *)
                     | Expression ("<"|">"|"<="|">=") Expression (* уровень 10 *)
                     | Expression "|" Expression              (* уровень 9 *)
                     | Expression "&" Expression              (* уровень 7 *)
                     | Expression "as" Type                   (* приведение типа *)
                     | Expression ("<<" | ">>") Expression    (* уровень 6 *)
                     | Expression ("+" | "-") Expression      (* уровень 5 *)
                     | Expression ("*" | "/" | "%") Expression (* уровень 4 *)
                     | Expression "**" Expression             (* уровень 3 *)
                     | "!" Expression | "++" Expression | "--" Expression  (* уровень 2 *)
                     | "-" Expression | "+" Expression
                     | Atom INC | Atom DEC                    (* постфикс *)
                     | Atom "." Member                        (* доступ к члену *)
                     | Atom "[" Slice "]"                     (* срез *)
                     | IntegerLiteral ":" IntegerLiteral      (* адрес:бит *)
                     | "{" ExpressionList "}"                 (* инициализатор *)
                     | Identifier | FunctionCall | Literal
```

### 4.9 Операторы

```bnf
Statement          ::= IfStatement
                     | ForStatement
                     | WhileStatement
                     | DoWhileStatement
                     | BlockStatement
                     | AssemblyBlock
                     | FormulaBlock
                     | SimpleStatement ";"
                     | "continue" ";"
                     | "break" ";"
                     | "return" Expression? ";"

IfStatement        ::= "if" Expression BlockStatement ("else" Statement)?
ForStatement       ::= "for" "(" SimpleStatement? ";" Expression? ";" Expression? ")" Statement
WhileStatement     ::= "while" "(" Expression ")" Statement
DoWhileStatement   ::= "do" Statement "while" "(" Expression ")" ";"
```

### 4.10 Функция

```bnf
FunctionDefinition ::= AnnotationDefinition*
                        "fn" Identifier ParameterList ("->" Type)?
                        (BlockStatement | ";")

ParameterList      ::= "(" Parameter ("," Parameter)* ")"
                     | "()"

Parameter          ::= Identifier ":" Type
                     | Type
```

### 4.11 Аннотации

```bnf
AnnotationDefinition     ::= "#[" AnnotationList "]"
GlobalAnnotationDefinition ::= "#![" AnnotationList "]"

AnnotationList     ::= Annotation ("," Annotation)*
Annotation         ::= IdentifierPath
                     | Identifier "(" AnnotationList ")"
                     | IdentifierPath "=" Expression
                     | IntegerLiteral | RationalLiteral | StringLiteral | BoolLiteral
```

### 4.12 Блоки формул

```bnf
FormulaDefinition  ::= "formula" StringLiteral? FormulaBlock

FormulaBlock       ::= "{" FormulaStatement* "}"

FormulaStatement   ::= FormulaBlock
                     | FormulaFunctionCall

FormulaFunctionCall ::= Identifier "(" FormulaExpressionList ")"

FormulaExpression  ::= IdentifierPath
                     | FormulaFunctionCall
                     | FormulaLiteral

FormulaLiteral     ::= (BoolLiteral | IntegerLiteral | HexLiteral | StringLiteral)
                       (":" Identifier)?
```

### 4.13 Свойство (Property)

```bnf
PropertyDefinition ::= AnnotationDefinition* Identifier "->" Property

Property           ::= Expression ";"
                     | BlockStatement
```

### 4.14 Определение условия (cond)

```bnf
ConditionDefinition ::= AnnotationDefinition* "cond" Identifier "=" Condition
```

---

## 5. Система типов

### 5.1 Базовые типы (встроенные псевдонимы)

| Тип BuT | Размер | Описание | C-аналог |
|---|---|---|---|
| `bit` | 1 бит | Логический бит (0/1) | `uint8_t` (1 бит) |
| `bool` | 1 бит | Булев тип | `int` |
| `u8` / `byte` | 8 бит | Беззнаковое целое | `uint8_t` |
| `u16` | 16 бит | Беззнаковое целое | `uint16_t` |
| `u32` | 32 бита | Беззнаковое целое | `uint32_t` |
| `u64` | 64 бита | Беззнаковое целое | `uint64_t` |
| `i32` / `int` | 32 бита | Знаковое целое | `int32_t` |
| `i64` | 64 бита | Знаковое целое | `int64_t` |
| `f32` | 32 бита | Число с плав. точкой | `float` |
| `f64` / `real` | 64 бита | Число с плав. точкой | `double` |
| `str` / `string` | динамич. | Строка | `char*` |
| `usize` | арх. | Размер (платформозависимый) | `size_t` |

### 5.2 Составные типы

#### Массив

```but
// Синтаксис: [размер : элемент]
type byte  = [8  : bit];   // 8-битный массив — аналог u8
type word  = [16 : bit];   // 16-битный массив — аналог u16
type block = [2  : [2: bit]]; // Матрица 2×2 бит

let mut matrix: [3: [3: u8]] = {{1,2,3},{4,5,6},{7,8,9}};
```

#### Псевдоним типа

```but
type bool  = bit;          // bool как псевдоним bit
type Timer = u32;          // Timer как псевдоним u32
```

#### Структура

```but
struct Point {
    x: i32;
    y: i32;
}
```

#### Перечисление

```but
#[repr(C)]
enum Color {
    Red, Green, Blue
}
```

### 5.3 Адресный тип

Особый тип для переменных, привязанных к адресу аппаратного регистра:

```but
// Синтаксис: HexNumber:BitOffset
port led: bit = 0xFFB0:2;    // Бит 2 по адресу 0xFFB0
port gpio: [8: bit] = 0xFFC0; // 8-битный регистр по адресу 0xFFC0
```

### 5.4 Совместимость типов

BuT использует **структурную типизацию** при приведении через `as`:

```but
fn process(input: u8, flag: bit) -> u16 {
    return (input + flag) as u16;
}
```

---

## 6. Переменные и атрибуты

### 6.1 Атрибуты переменных

| Атрибут | Значение | Семантика |
|---|---|---|
| `const` | Константа | Только чтение, инициализируется один раз |
| `let` | Локальная (только чтение) | Объявление без изменения по умолчанию |
| `mut` | Изменяемая | Может быть изменена во время выполнения |
| `port` | Порт ввода/вывода | Привязана к аппаратному регистру |
| `external` | Внешняя | Видима из внешних модулей |
| `private` | Приватная | Скрыта от внешних модулей |

### 6.2 Синтаксис объявления

```but
// Константа
const MAX_VALUE: u32 = 0xFFFF;
const PI: float = 3.14159;

// Только чтение
let counter: u64 = 0;

// Изменяемая переменная
let mut timer: int = 0;

// Порт ввода/вывода
port input:  bit = 0x01;        // Порт с адресом 0x01
port output: bit = 0x02;        // Порт с адресом 0x02
port gpio:  [8: bit] = 0xFFC0;  // 8-битный порт

// Инициализированный массив
let mut  var6: [3: [3: u8]] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
```

### 6.3 Область видимости

- **Глобальный уровень**: переменные видны во всём файле
- **Уровень модели**: переменные видны внутри автомата и его подавтоматов
- **Уровень состояния**: переменные видны только внутри состояния
- **Уровень функции**: локальные переменные функции

### 6.4 Аннотированные переменные

```but
#[extern(C), unused, inline(never)]
let  var1: bit = 1;
```

---

## 7. Модели и конечные автоматы

### 7.1 Базовый синтаксис

```but
model ИмяАвтомата {
    // Состояния
    state СтартовоеСостояние { ... }
    state ДругоеСостояние    { ... }

    // Начальное состояние
    start -> СтартовоеСостояние;
    // или: start: СтартовоеСостояние;

    // Глобальные переменные автомата
    let mut переменная: u32 = 0;

    // Глобальные обработчики
    enter -> тело_выполняется_каждый_такт;
    end   -> тело_при_завершении;
}
```

### 7.2 Зарезервированные имена свойств в модели

| Свойство | Когда выполняется |
|---|---|
| `start` | Указывает начальное состояние |
| `enter` | Выполняется на каждом такте |
| `end` | Выполняется при завершении автомата |
| `behavior` | Выражение композиции подавтоматов |

### 7.3 Абстрактные и шаблонные модели

```but
abstract model Базовый {
    state Ожидание {}
}

template model Шаблон {
    state Работа {}
}
```

### 7.4 Вложенные автоматы

Модели могут быть вложены в состояния:

```but
model Outer {
    state Complex {
        model Inner {
            state A { ref B; }
            state B { }
            start -> A;
        }
        behavior -> Inner;
    }
}
```

---

## 8. Состояния

### 8.1 Синтаксис

```but
state ИмяСостояния {
    // Переходы
    ref ЦелевоеСостояние: условие;
    ref ДругоеСостояние;          // Безусловный переход

    // Обработчики
    enter  -> { /*действия на каждом такте*/ }
    exit   -> { /*действия при выходе из состояния*/ }
    before -> { /*действия перед входом в это состояние*/ }

    // Локальные переменные состояния
    let mut local_var: u8 = 0;
}
```

### 8.2 Зарезервированные имена свойств состояния

| Свойство | Когда выполняется |
|---|---|
| `enter` | На каждом такте нахождения в состоянии |
| `exit` | При выходе из состояния (перед переходом) |
| `before` | Перед входом в это состояние (в целевом состоянии) |

### 8.3 Порядок выполнения в такте

1. Выполнение глобального `enter` модели
2. Выполнение `enter` текущего состояния
3. Проверка переходов (`ref`) по порядку объявления
4. При выполнении условия перехода:
   - Выполнение `exit` текущего состояния
   - Выполнение `before` целевого состояния
   - Смена текущего состояния

---

## 9. Переходы (ref)

### 9.1 Синтаксис

```but
ref ЦелевоеСостояние;                     // Безусловный переход (всегда срабатывает)
ref ЦелевоеСостояние: условие;            // Условный переход
ref ЦелевоеСостояние: переменная = 10;   // Проверка равенства
ref ЦелевоеСостояние: x > 5 && y < 3;   // Составное условие
ref ЦелевоеСостояние: input = 1;         // Порт равен значению
```

### 9.2 Особенности

- Переходы проверяются в **порядке объявления** (первый сработавший выполняется)
- Если несколько переходов выполняются одновременно — это **недетерминизм** (BuT предупреждает)
- Если целевое состояние не определено — это **конечное состояние** (завершение)

### 9.3 Примеры

```but
state Idle {
    ref Running: start_signal = 1;
    ref Idle;   // Остаться в Idle (безусловно — но проверяется после Running)
}

state Running {
    ref Done:  counter >= MAX;
    ref Error: error_flag = 1;
    ref Running;  // Остаться в Running
}
```

---

## 10. Условия (Condition)

Условия используются только в переходах `ref`. Это отдельная, упрощённая грамматика выражений (без присваиваний).

### 10.1 Операторы условий

| Оператор | Описание | Пример |
|---|---|---|
| `==` | Равенство | `x == 0` |
| `!=` | Неравенство | `flag != 1` |
| `<` | Меньше | `counter < 100` |
| `>` | Больше | `timer > 10` |
| `<=` | Не больше | `value <= MAX` |
| `>=` | Не меньше | `port4 >= 5` |
| `&&` | Логическое И (через `&`) | `a & b` |
| `\|\|` | Логическое ИЛИ (через `\|`) | `a \| b` |
| `!` | Логическое НЕ | `!flag` |
| `+`, `-` | Арифметика | `counter - 1 > 0` |

### 10.2 Пример комплексного условия

```but
state Wait {
    ref Listen: counter > 1000;
    ref Error:  port4 > 10 && error_flag = 1;
    ref Next:   (S(Sub) = End) & (S(Main) = Done);
}
```

---

## 11. Выражения (Expression)

Выражения используются в телах функций, обработчиках состояний и инициализаторах.

### 11.1 Таблица приоритетов (от наинизшего к наивысшему)

| Уровень | Операторы | Ассоциативность |
|---|---|---|
| 14 | `=` `+=` `-=` `*=` `/=` `%=` `\|=` `&=` `<<=` `>>=` | Право |
| 13 | `\|\|` | Лево |
| 12 | `&&` | Лево |
| 11 | `==` `!=` | Лево |
| 10 | `<` `>` `<=` `>=` | Лево |
| 9 | `\|` | Лево |
| 7 | `&`, `as` | Лево |
| 6 | `<<` `>>` | Лево |
| 5 | `+` `-` | Лево |
| 4 | `*` `/` `%` | Лево |
| 3 | `**` | Право |
| 2 | `!` `++` `--` `-` `+` (унарные) | Право |
| 1 | `++` `--` (постфикс), `.`, `[]` | Лево |

### 11.2 Приведение типа

```but
let result: u16 = (input1 + input2) as u16;
```

### 11.3 Инициализатор агрегата

```but
let arr: [3: u8] = {1, 2, 3};
let mat: [2: [2: u8]] = {{1, 2}, {3, 4}};
```

### 11.4 Адресный литерал

```but
port p: bit = 0xFFB0:2;  // Адрес 0xFFB0, бит 2
```

---

## 12. Функции

### 12.1 Определение функции

```but
// Функция с аргументами и возвращаемым типом
fn calculate(x: int, y: int) -> int {
    return x + y;
}

// Функция без возвращаемого значения
fn reset(counter: u32) {
    counter = 0;
}

// Внешняя функция (объявление без тела)
#[extern(C)]
fn printf(fmt: str, args: int);
```

### 12.2 Аннотации функций

```but
#[extern(C), unused, inline(never)]
fn write(n: int, c: int);

#[inline(always)]
fn pi() -> real {
    return 3.14159;
}

#[unused, always_inline, align(4)]
fn try_recover() {
    assembly { }
}
```

### 12.3 Встроенный ассемблер

```but
fn critical_section() {
    assembly {
        // Низкоуровневые инструкции
    }
}
```

---

## 13. Свойства (Property)

Свойство — именованное описание поведения или значения через оператор `->`.

### 13.1 Синтаксис

```but
// Свойство-выражение (заканчивается ";")
enter -> output = 1;
start -> IdleState;

// Свойство-блок (тело в фигурных скобках)
enter -> {
    output = 1;
    counter = counter + 1;
}

// Глобальное свойство поведения
behavior -> Consumer | Producer;
end      -> exit(0);
```

### 13.2 Специальные свойства модели

```but
model M {
    start  -> S1;        // Начальное состояние
    enter  -> actions;   // Каждый такт
    end    -> cleanup;   // При завершении
    behavior -> A | B;   // Композиция подавтоматов
}
```

---

## 14. Аннотации

Аннотации предоставляют метаданные для компилятора, оптимизатора и кодогенератора.

### 14.1 Аннотации элемента

```but
#[repr(C)]           // Структура/перечисление в C-совместимом формате
#[extern(C)]         // Внешняя C-функция
#[unused]            // Предотвратить удаление оптимизатором
#[inline(always)]    // Всегда встраивать
#[inline(never)]     // Никогда не встраивать
#[align(4)]          // Выровнять по 4 байта
#[display("Имя")]    // Имя для отображения в UI
#[skip]              // Пропустить в обработке
```

### 14.2 Глобальные аннотации файла

```but
#![fsm(Table)]           // Тип генерации: табличный автомат
#![fsm(Switch)]          // Тип генерации: switch-case
#![guard = "timer >= 0"] // Условие-стража
#![ltl = "G(timer)"]     // LTL-спецификация
```

---

## 15. Формулы LTL

### 15.1 Интеграция с BuT

LTL (Linear Temporal Logic — Линейная темпоральная логика) используется для верификации свойств поведения автоматов.

#### Способ 1: Аннотации

```but
model Producer {
    #![guard = "timer >= 0"]
    #![ltl = "G(timer)"]
    ...
}
```

#### Способ 2: Блок formula

```but
fn process(input1: u8, input2: bit) -> u16 {
    formula "LTL" {
        // Пустой диалект LTL
    }
    formula {
        None(true)
        None(false)
        Always(input1)
    }
    return (input1 + input2) as u16;
}
```

### 15.2 Грамматика LTL

```bnf
Formula      ::= Conjunction

Conjunction  ::= Conjunction "&" Disjunction
               | Disjunction

Disjunction  ::= Disjunction "|" Form
               | Form

Form         ::= UnaryTerm BinaryOp UnaryTerm
               | UnaryTerm

UnaryTerm    ::= UnaryOp Term | Term

Term         ::= BoolLiteral
               | Function
               | Function CompareOp Function
               | Function CompareOp Number
               | "(" Formula ")"

UnaryOp      ::= "[]"   (* Always/Globally *)
               | "<>"   (* Eventually *)
               | "!"    (* Not *)
               | "X"    (* Next *)

BinaryOp     ::= "U"    (* Until *)
               | "W"    (* Weak Until *)
               | "R"    (* Release *)
               | "->"   (* Implication *)
               | "<->"  (* Equivalence *)

CompareOp    ::= "==" | "!=" | ">=" | ">" | "<=" | "<"
```

### 15.3 Операторы LTL

| Оператор | Символ | Значение |
|---|---|---|
| Always (Globally) | `[]` / `G` | Формула истинна во всех будущих состояниях |
| Eventually | `<>` / `F` | Формула истинна хотя бы в одном будущем |
| Next | `X` | Формула истинна в следующем состоянии |
| Until | `U` | A истинно до тех пор, пока не стало B |
| Weak Until | `W` | Как Until, но B может не наступить |
| Release | `R` | B истинно до тех пор, пока не стало A |
| Implication | `->` | A → B |
| Equivalence | `<->` | A ↔ B |

### 15.4 Примеры LTL-формул

```
// Всегда истинно z2 после was
[]was(z2)

// Сложная формула
(is(x1) W was(z1)) & [](was(z2) -> (is(x1) U was(z1)) & was(z1) -> (!is(x1) R was(z2)))
```

---

## 16. Импорт и механизм include

### 16.1 Синтаксис директивы import

Директива `import` позволяет включить определения из другого `.but`-файла:

```but
// Прямой импорт — включает все определения из файла
import "module.but";

// Можно опустить расширение .but — резолвер добавляет его автоматически
import "module";

// Импорт с псевдонимом пространства имён
import "module.but" as MyModule;

// Импорт всех символов с псевдонимом
import * as Alias from "module.but";

// Именованный импорт конкретных символов
import { TypeA, FuncB as FB } from "module.but";
```

### 16.2 Семантика разрешения импортов (IncludeResolver)

Резолвер (`but_middleware::include::IncludeResolver`) выполняет:

| Функция | Описание |
|---------|----------|
| **Поиск файлов** | Относительно базового файла, затем по путям поиска `-I` |
| **Транзитивность** | Импортируемый файл в свою очередь разрешает свои импорты |
| **Дедупликация** | Один файл включается ровно один раз (по каноническому пути) |
| **Защита от циклов** | Повторный импорт уже посещённого файла тихо пропускается |
| **Накопление ошибок** | При нескольких ошибочных импортах все они собираются за один проход |

### 16.3 Порядок включения

Определения из импортируемых файлов размещаются **до** определений текущего файла.
При транзитивных импортах порядок соответствует порядку обхода в глубину (DFS).

```
main.but:             Результирующий SourceUnit:
  import "a";  →        [дефиниции из level2.but]
  import "b";           [дефиниции из a.but]
  const X = 1;          [дефиниции из b.but]
                        const X = 1;
```

### 16.4 Пути поиска

Резолвер просматривает файлы в следующем порядке:

1. Директория файла, содержащего директиву `import`
2. Пути из командной строки (флаг `-I` инструмента `but`)
3. Абсолютные пути (если в строке импорта указан абсолютный путь)

### 16.5 Примеры использования include

**Базовый пример: разделяемые типы**

```but
// types.but — разделяемые типы
type bool  = bit;
type byte  = [8: bit];
type word  = [16: bit];
type dword = [32: bit];
```

```but
// main.but — использует типы из types.but
import "types";

const VERSION: byte = 0x10;

model Blinker {
    output led: bool;
    var counter: byte;

    state Off { led = 0; }
    state On  { led = 1; }

    start -> Off;

    Off -> On:  if (counter < 10) { counter = counter + 1; }
    On  -> Off: if (counter >= 10) { counter = 0; }
}
```

**Вложенные импорты (транзитивная цепочка)**

```but
// level2.but
type BaseUnit = [4: bit];

// level1.but
import "level2";
type Register = [8: BaseUnit];  // используем тип из level2

// main.but
import "level1";              // транзитивно включает level2
var reg: Register;            // доступен, хотя level2 не импортируется явно
```

**Дедупликация (двойной импорт)**

```but
// common.but
type SharedType = [16: bit];

// a.but
import "common";
const A_CONST: SharedType = 0;

// main.but
import "a";       // включает common.but транзитивно
import "common";  // повторный импорт common.but — игнорируется
const X: SharedType = 1;  // SharedType определён ровно один раз
```

### 16.6 Обработка ошибок

| Ситуация | Ошибка | Действие |
|----------|--------|----------|
| Файл не найден | `FileNotFound` | Обработка продолжается для других импортов |
| Синтаксическая ошибка в импорте | `ParseError` | Собирается в список ошибок |
| Ошибка чтения файла | `IoError` | Собирается в список ошибок |
| Циклический импорт | Молчаливо пропускается | Дедупликация по `HashSet<PathBuf>` |

---

## 17. Структуры и перечисления

### 17.1 Структуры

```but
#[repr(C)]
struct Packet {
    header:  u8;
    payload: [8: u8];
    crc:     u16;
}
```

### 17.2 Перечисления

```but
#[repr(C)]
enum State {
    Idle, Running, Error, Done
}

// Безымянное перечисление (последовательность)
enum {
    A, B, C
}
```

### 17.3 Псевдонимы типов

```but
type bool = bit;
type byte = [8: bit];
type u8   = [8: bit];
type word = [16: bit];
```

---

## 18. Композиция поведения

### 18.1 Операторы композиции

| Оператор | Семантика |
|---|---|
| `A \| B` | **Параллельная** композиция (оба автомата работают одновременно) |
| `A + B` | **Последовательная** композиция (B запускается после A) |
| `(A + B) \| C` | Смешанная с группировкой |

### 18.2 Примеры

```but
// Параллельные автоматы
behavior -> Consumer | Producer;

// Последовательные автоматы
behavior -> Map + Reduce;

// Смешанная композиция
behavior -> (A + B) | C;
```

### 18.3 Обращение к состоянию подавтомата

```but
// Проверить, в каком состоянии находится подавтомат
ref Join: (S(Reduce) = End) & (S(Map) = End);
```

---

## 19. Кодогенерация

### 19.1 Поддерживаемые целевые форматы

| Формат | Команда | Описание |
|---|---|---|
| C99 | `--gen-c` | ANSI C с заголовком (.h) и исходником (.c) |
| Verilog | `--gen-verilog` | RTL-модуль Verilog для FPGA/ASIC |
| Structured Text | `--gen-st` | IEC 61131-3 для ПЛК |
| LC-3 ASM | `--gen-lc3` | Ассемблер учебной архитектуры LC-3 |
| ARM Thumb | `--gen-thumb` | Ассемблер ARMv7-M Thumb |

### 19.2 Пример генерации C-кода

Для модели `Delay`:

**Заголовок (delay.h):**
```c
#ifndef __DELAY_H__
#define __DELAY_H__

#include <stdint.h>

typedef enum {
    DELAY_ONE,
    DELAY_NONE,
    DELAY_END
} Delay_State_t;

extern uint8_t input;
extern uint8_t output;

void Delay_init(void);
void Delay_step(void);
Delay_State_t Delay_state(void);

#endif /* __DELAY_H__ */
```

**Исходник (delay.c):**
```c
#include "delay.h"

static Delay_State_t _state;
uint8_t input = 0x01;
uint8_t output = 0x02;

void Delay_init(void) {
    _state = DELAY_NONE;
}

Delay_State_t Delay_state(void) {
    return _state;
}

void Delay_step(void) {
    switch (_state) {
        case DELAY_ONE: {
            output = 1;
            if (input == 0) { _state = DELAY_NONE; }
            if (input == 1) { _state = DELAY_ONE; }
            break;
        }
        case DELAY_NONE: {
            output = 0;
            if (input == 1) { _state = DELAY_ONE; }
            if (input == 0) { _state = DELAY_NONE; }
            break;
        }
        case DELAY_END: {
            break;
        }
    }
}
```

### 19.3 Использование CLI

```bash
# Симуляция
but source.but --simulate --steps 20 --port input=1

# Визуализация в DOT
but source.but --visualize --output-dir ./gen

# Генерация всех форматов
but source.but --all --output-dir ./gen

# Только C и Verilog
but source.but --gen-c --gen-verilog --output-dir ./gen
```

---

## 20. Полные примеры программ

### 20.1 Простой двухсостоянчый автомат (задержка)

```but
model Delay {
    #[display("Единица")]
    state One {
        ref None: input = 0;
        ref One : input = 1;
        enter -> {
            output = 1;
        }
    }
    #[display("Ноль")]
    state None {
        ref One : input = 1;
        ref None: input = 0;
        enter -> {
            output = 0;
        }
    }
    #[skip]
    state End { }

    start -> None;
}

port input : bit = 0x01;
port output: bit = 0x02;
```

### 20.2 Многоуровневый автомат с LTL-верификацией

```but
#![fsm(Table)]

model Producer {
    #![guard = "timer >= 0"]
    #![ltl = "G(timer)"]

    state First_Step {
        ref Second_Step: timer = 10;
        ref Third_Step:  timer = 30;
        ref First_Step;              // Остаться по умолчанию
        before -> debug("before First_Step");
        exit   -> debug("exit First_Step");
        enter  -> debug("tick First_Step");
    }
    state Second_Step {
        ref First_Step: timer >= 20;
        before -> write(0, timer);
    }
    #[unused]
    state Third_Step { }

    start: First_Step;
    enter -> timer = timer + 1;
    end   -> exit(EXIT_PRODUCER);

    let mut timer: int = 0;
}

model Consumer {
    state Listen {
        ref Wait: port4 > 10;
    }
    state Wait {
        ref Listen: counter > 1000;
    }
    state Next {
        model Map {
            state Work { ref End: complete; }
            state End  { }
            start -> Work;
            port complete: bit = 0x10000:3;
        }
        behavior -> Map;
        ref Join: S(Map) = End;
        end -> assert(result == 1);
    }

    start -> Listen;
    enter -> counter = counter + 1;
    end   -> exit(EXIT_CONSUMER);

    let result: int = 0;
}

// Параллельная композиция
behavior -> Consumer | Producer;
end -> exit(EXIT);

#[extern(C)] fn exit(code: int);
#[extern(C)] fn debug(msg: str);
#[extern(C)] fn write(n: int, c: int);
#[extern(C)] fn assert(c: bool);

port port4: int = 7;
const EXIT_CONSUMER: int = 0;
const EXIT_PRODUCER: int = 1;
const EXIT: int = 2;
let mut counter: u64 = 0;
```

### 20.3 Определение типов и перечислений

```but
// Псевдонимы типов
type bool  = bit;
type byte  = [8: bit];
type u8    = [8: bit];

// Структуры
#[repr(C)]
struct Packet {
    id:   u8;
    data: [8: u8];
    crc:  u16;
}

// Перечисление
#[repr(C)]
enum Direction {
    North, South, East, West
}

// Использование в переменных
const PI: float = 3.14159;
let mut matrix: [3: [3: u8]] = {{1,2,3},{4,5,6},{7,8,9}};
```

---

## Приложение A: Семантические ограничения

| Ограничение | Описание |
|---|---|
| Уникальность имён | Перечисления, функции, переменные, структуры и типы должны иметь уникальные имена |
| Начальное состояние | Каждая модель должна иметь объявление `start` |
| Достижимость | Состояния без входящих переходов могут быть помечены предупреждением |
| Недетерминизм | Множество одновременно истинных переходов — предупреждение |

## Приложение B: Интеграция с инструментами

| Инструмент | Поддержка |
|---|---|
| Cargo (Rust) | Полная интеграция через рабочее пространство |
| CI/CD | GitHub Actions, Travis CI |
| Покрытие кода | codecov.yml конфигурация |
| Graphviz | Визуализация через DOT-формат |
| LALRPOP 0.23 | Генерация парсера из `.lalrpop`-файлов |

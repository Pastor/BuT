# BuT — Язык спецификации конечных автоматов

> **Версия документа:** 1.0
> **Автор анализа:** Системный аналитик
> **Дата:** 2026-03-11
> **Репозиторий:** https://github.com/Pastor/BuT

---

## Содержание

1. [Общее описание проекта](#1-общее-описание-проекта)
2. [Архитектура проекта](#2-архитектура-проекта)
3. [Лексика языка](#3-лексика-языка)
4. [Расширенная форма Бэкуса–Наура (РБНФ)](#4-расширенная-форма-бэкусанаура-рбнф)
5. [Система типов](#5-система-типов)
6. [Доступ к конкретному биту](#6-доступ-к-конкретному-биту)
7. [Переменные и атрибуты](#7-переменные-и-атрибуты)
8. [Модели и конечные автоматы](#8-модели-и-конечные-автоматы)
9. [Состояния](#9-состояния)
10. [Переходы (ref)](#10-переходы-ref)
11. [Условия (Condition)](#11-условия-condition)
12. [Выражения (Expression)](#12-выражения-expression)
13. [Функции](#13-функции)
14. [Свойства (Property)](#14-свойства-property)
15. [Аннотации](#15-аннотации)
16. [Формулы LTL](#16-формулы-ltl)
17. [Импорт](#17-импорт)
18. [Структуры и перечисления](#18-структуры-и-перечисления)
19. [Композиция поведения](#19-композиция-поведения)
20. [Кодогенерация](#20-кодогенерация)
21. [Полные примеры программ](#21-полные-примеры-программ)

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

### 5.1 Примитивные типы

В языке BuT существуют только **два примитивных типа**:

| Тип BuT | Описание | C | ST (IEC 61131-3) |
|---|---|---|---|
| `bit` | 1 логический бит | `uint8_t` | `BOOL` |
| `float` | Число с плавающей точкой | `float` | `REAL` |

Тип `bit` является основой всей целочисленной арифметики: все остальные целочисленные типы **выводятся из `bit`** через конструкцию массива `[N: bit]`.

Тип `float` является математическим типом и не выводится из `bit`. При кодогенерации он преобразуется в `float` (C) и `REAL` (ST).

### 5.2 Производные целочисленные типы

Целочисленные типы — это массивы битов. Конструкция `[N: bit]` задаёт N-битное беззнаковое целое.

**Правило кодогенерации для `[N: bit]`:**

| Число бит N | C-тип | ST-тип |
|---|---|---|
| 1 ≤ N ≤ 8   | `uint8_t`   | `BYTE`  |
| 9 ≤ N ≤ 16  | `uint16_t`  | `WORD`  |
| 17 ≤ N ≤ 32 | `uint32_t`  | `DWORD` |
| 33 ≤ N ≤ 64 | `uint64_t`  | `LWORD` |
| N > 64      | `uint64_t[⌈N/64⌉]` | `ARRAY [0..⌈N/64⌉-1] OF LWORD` |

Типы, превышающие размер нативных типов целевого языка, приводятся к **массивам с выравниванием по 64-битной границе**.

```but
// Примеры производных типов:
type u8   = [8:   bit];  // → uint8_t (C), BYTE (ST)
type u16  = [16:  bit];  // → uint16_t (C), WORD (ST)
type u32  = [32:  bit];  // → uint32_t (C), DWORD (ST)
type u64  = [64:  bit];  // → uint64_t (C), LWORD (ST)
type u128 = [128: bit];  // → uint64_t[2] (C), ARRAY [0..1] OF LWORD (ST)

// Логический тип — псевдоним bit:
type bool = bit;         // → uint8_t (C), BOOL (ST)

// Матрица 2×2 бит:
type block = [2: [2: bit]];
```

### 5.3 Стандартная библиотека типов (std.but)

Файл `include/std.but` содержит стандартные определения производных типов, готовых к использованию:

```but
import "include/std";

// После импорта доступны: u8, u16, u32, u64, u128, bool, byte,
// Counter, Flag, Timer, Size
```

Содержимое `include/std.but`:

| Тип | Определение | C | ST |
|---|---|---|---|
| `u8`, `byte` | `[8: bit]` | `uint8_t` | `BYTE` |
| `u16` | `[16: bit]` | `uint16_t` | `WORD` |
| `u32` | `[32: bit]` | `uint32_t` | `DWORD` |
| `u64` | `[64: bit]` | `uint64_t` | `LWORD` |
| `u128` | `[128: bit]` | `uint64_t[2]` | `ARRAY [0..1] OF LWORD` |
| `bool` | `bit` | `uint8_t` | `BOOL` |
| `Counter` | `u32` | `uint32_t` | `DWORD` |
| `Flag` | `bool` | `uint8_t` | `BOOL` |
| `Timer` | `u32` | `uint32_t` | `DWORD` |
| `Size` | `u64` | `uint64_t` | `LWORD` |

### 5.4 Псевдоним типа

Синтаксис: `type <Имя> = <Тип>;`

Псевдоним присваивает новое имя существующему типу. При кодогенерации псевдоним **рекурсивно раскрывается** до примитивного типа.

```but
import "include/std";

// Пользовательские псевдонимы на основе стандартных типов:
type Byte    = u8;         // Byte → u8 → [8: bit] → uint8_t (C)
type Counter = Byte;       // Counter → Byte → u8 → [8: bit] → uint8_t (C)
type Speed   = float;      // Speed → float → float (C), REAL (ST)
```

Таблица разворачивания для основных целевых платформ:

| Определение BuT | Разворачивается в | C | ST |
|---|---|---|---|
| `type A = [8: bit]`  | `[8: bit]`  | `uint8_t`  | `BYTE`  |
| `type A = [16: bit]` | `[16: bit]` | `uint16_t` | `WORD`  |
| `type A = [32: bit]` | `[32: bit]` | `uint32_t` | `DWORD` |
| `type A = [64: bit]` | `[64: bit]` | `uint64_t` | `LWORD` |
| `type A = [128: bit]`| `[128: bit]`| `uint64_t[2]` | `ARRAY [0..1] OF LWORD` |
| `type A = bit`       | `bit`       | `uint8_t`  | `BOOL`  |
| `type A = float`     | `float`     | `float`    | `REAL`  |
| `type A = B; type B = [32: bit]` | `[32: bit]` | `uint32_t` | `DWORD` |

Глубина раскрытия цепочки псевдонимов — до 8 уровней.

### 5.5 Составные типы

#### Массив произвольных элементов

```but
// [N: T] — массив из N элементов типа T
let mut matrix: [3: [3: u8]] = {{1,2,3},{4,5,6},{7,8,9}};
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

### 5.6 Адресный тип

Особый тип для переменных, привязанных к адресу аппаратного регистра:

```but
// Синтаксис: HexNumber:BitOffset
port led: bit = 0xFFB0:2;     // Бит 2 по адресу 0xFFB0
port gpio: [8: bit] = 0xFFC0; // 8-битный регистр по адресу 0xFFC0
```

### 5.7 Совместимость типов

BuT использует **структурную типизацию** при приведении через `as`:

```but
import "include/std";

fn process(input: u8, flag: bit) -> u16 {
    return (input + flag) as u16;
}
```

---

## 6. Доступ к конкретному биту

### 6.1 Синтаксис

Оператор точка с числом (`.N`) обеспечивает доступ к конкретному биту переменной нематематического типа:

```
переменная.N          — чтение N-го бита
переменная.N = expr   — запись в N-й бит
```

Нумерация битов начинается с **0** (младший бит).

Оператор применим к переменным **любого производного от `bit` типа**: `[N: bit]`, `u8`, `u16`, `u32`, `u64`, `u128` и их псевдонимам. Тип `float` и другие математические типы **не поддерживают** побитовый доступ.

### 6.2 Примеры

```but
let mut a: [8: bit] = 0;

a.5 = true;   // Запись: установить 6-й бит (нумерация с 0) → a |= (1 << 5)
a.0 = false;  // Запись: сбросить 1-й бит

let b: [8: bit] = a;
let flag = b.5;  // Чтение: получить значение 6-го бита (0 или 1)
```

```but
import "include/std";

let mut data: u32 = 0xDEADBEEF;

data.31 = false;  // Сбросить старший бит 32-битного значения
data.0  = true;   // Установить младший бит

// В условии перехода:
// ref Active : data.7;  // переход если бит 7 установлен
```

```but
// Доступ к массиву битов:
let mut b: [128: bit] = 0;

b.127 = true;  // Установить 128-й бит (последний допустимый)
b.250 = false; // ОШИБКА: выход за границы типа (максимальный бит — 127)
```

### 6.3 Семантическая проверка

Семантический анализатор BuT проверяет, что индекс бита не выходит за границы типа переменной:

| Тип | Допустимые биты | Пример ошибки |
|---|---|---|
| `[8: bit]` | 0 ≤ N ≤ 7 | `x.8 = true` — ошибка |
| `[16: bit]` | 0 ≤ N ≤ 15 | `x.16 = true` — ошибка |
| `[32: bit]` | 0 ≤ N ≤ 31 | `x.32 = true` — ошибка |
| `[64: bit]` | 0 ≤ N ≤ 63 | `x.64 = true` — ошибка |
| `[128: bit]` | 0 ≤ N ≤ 127 | `x.128 = true` — ошибка |

При обнаружении нарушения компилятор выдаёт диагностическое сообщение:
```
ошибка: обращение к биту 8 выходит за границы типа (ширина 8 бит)
```

### 6.4 Кодогенерация

Для примеров используется переменная `flags: [8: bit]` и следующий BuT-код:

```but
flags.5 = true;       // запись: установить бит 5
flags.1 = false;      // запись: сбросить бит 1
let x = flags.5;      // чтение: получить значение бита 5
ref Next : flags.5;   // условие перехода по биту 5
```

#### C

Генератор использует побитовые операции стандарта C99 с типом `uint*_t`.

**Чтение** (`flags.5` в выражении или условии перехода):
```c
((flags) >> 5) & 1
```

**Запись** (`flags.5 = true`):
```c
flags = ((flags) & ~(1UL << 5)) | ((1 & 1) << 5)
```

**Запись** (`flags.1 = false`):
```c
flags = ((flags) & ~(1UL << 1)) | ((0 & 1) << 1)
```

**Общая формула** для записи `var.N = expr`:
```c
var = ((var) & ~(1UL << N)) | ((expr & 1) << N)
```

#### ST (IEC 61131-3 / Structured Text)

Генератор ST (`st.rs`) использует те же функции `expr_to_c` и `condition_to_c`, что и C-бэкенд, вставляя C-выражения непосредственно в ST-операторы. Большинство промышленных ПЛК-компиляторов принимают побитовую арифметику в стиле C внутри ST.

**Чтение** (`ref Next : flags.5`):
```pascal
IF ((flags) >> 5) & 1 THEN
    state := 1;
END_IF;
```

**Запись** (`flags.5 = true` в блоке `enter`):
```pascal
flags = ((flags) & ~(1UL << 5)) | ((1 & 1) << 5) ;
```

#### Verilog

Verilog поддерживает встроенный синтаксис выбора бита `[N]`, который генератор использует напрямую.

**Чтение** (`flags.5` в условии):
```verilog
flags[5]
```

**Запись** (`flags.5 = true`):
```verilog
flags[5] = 1;
```

**Условие перехода** (`ref Next : flags.5`):
```verilog
if (flags[5]) begin
    state <= STATE_NEXT;
end
```

#### LC3 Assembly

LC3-генератор выводит условия переходов как ассемблерные комментарии (`; условие: ...`), используя C-представление выражения. Непосредственный расчёт битов в тексте инструкций не генерируется — реализуется вручную по шаблону ниже.

**Сгенерированный комментарий** для `ref Next : flags.5`:
```asm
    ; условие: ((flags) >> 5) & 1
    LD R3, CONST_1          ; Индекс следующего состояния
    ST R3, MODEL_STATE
    BRnzp MODEL_LOOP
```

**Шаблон ручной реализации** — чтение бита N регистра `R_VAR` в `R_BIT`:
```asm
    ; Чтение бита N из переменной (значение в R_VAR)
    AND R_BIT, R_VAR, #MASK  ; MASK = 1 << N (для N < 9 в LC3)
    BRz BIT_CLEAR
    AND R_BIT, R_BIT, #0
    ADD R_BIT, R_BIT, #1     ; R_BIT = 1
    BRnzp BIT_DONE
BIT_CLEAR
    AND R_BIT, R_BIT, #0     ; R_BIT = 0
BIT_DONE
```

> **Примечание:** LC3 имеет 16-битную шину данных. Для переменных шире 16 бит (`u32`, `u64`) требуется работа с несколькими словами в памяти.

#### ARM Thumb

ARM Thumb-генератор выводит выражения доступа к биту как комментарии (`/* если (expr) → State */`). Для условий сложнее `Condition::Variable` (в том числе `MemberAccess`) генерируется заглушка `/* сложное условие — вычислите вручную */`. Расчёт битов реализуется через инструкции `LSR`/`AND`/`BIC`/`ORR`.

**Сгенерированный фрагмент** для `ref Next : flags.5`:
```asm
    /* если (((flags) >> 5) & 1) → Next */
    /* сложное условие — вычислите вручную */
    movs r1, #1             @ индекс состояния Next
    ldr r0, =_model_state
    str r1, [r0]
    b model_done
```

**Шаблон ручной реализации** — чтение бита N из переменной `flags` (адрес в памяти):
```asm
    @ Чтение бита N из переменной flags → результат в r3
    ldr r0, =flags          @ загрузить адрес переменной
    ldr r1, [r0]            @ r1 = значение переменной
    lsrs r3, r1, #N         @ сдвинуть вправо на N позиций
    ands r3, r3, #1         @ оставить только бит 0 → r3 = 0 или 1
```

**Шаблон** — запись бита N (установить в 1):
```asm
    @ Установить бит N в переменной flags
    ldr r0, =flags
    ldr r1, [r0]
    movs r2, #1
    lsls r2, r2, #N         @ r2 = (1 << N)
    orrs r1, r1, r2         @ r1 |= (1 << N)
    str r1, [r0]
```

**Шаблон** — запись бита N (сбросить в 0):
```asm
    @ Сбросить бит N в переменной flags
    ldr r0, =flags
    ldr r1, [r0]
    movs r2, #1
    lsls r2, r2, #N         @ r2 = (1 << N)
    bics r1, r1, r2         @ r1 &= ~(1 << N)
    str r1, [r0]
```

#### Сводная таблица

| Платформа | Чтение `var.N` | Запись `var.N = val` | Статус |
|---|---|---|---|
| **C** | `((var) >> N) & 1` | `var = ((var) & ~(1UL << N)) \| ((val & 1) << N)` | Полная поддержка |
| **ST** | `((var) >> N) & 1` | `var = ((var) & ~(1UL << N)) \| ((val & 1) << N)` | Полная поддержка (C-синтаксис) |
| **Verilog** | `var[N]` | `var[N] = val` | Полная поддержка |
| **LC3 ASM** | комментарий | не генерируется | Ручная реализация |
| **ARM Thumb** | комментарий | не генерируется | Ручная реализация |

---

## 7. Переменные и атрибуты

### 7.1 Атрибуты переменных

| Атрибут | Значение | Семантика |
|---|---|---|
| `const` | Константа | Только чтение, инициализируется один раз |
| `let` | Локальная (только чтение) | Объявление без изменения по умолчанию |
| `mut` | Изменяемая | Может быть изменена во время выполнения |
| `port` | Порт ввода/вывода | Привязана к аппаратному регистру |
| `external` | Внешняя | Видима из внешних модулей |
| `private` | Приватная | Скрыта от внешних модулей |

### 7.2 Синтаксис объявления

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

### 7.3 Область видимости

- **Глобальный уровень**: переменные видны во всём файле
- **Уровень модели**: переменные видны внутри автомата и его подавтоматов
- **Уровень состояния**: переменные видны только внутри состояния
- **Уровень функции**: локальные переменные функции

### 7.4 Аннотированные переменные

```but
#[extern(C), unused, inline(never)]
let  var1: bit = 1;
```

---

## 8. Модели и конечные автоматы

### 8.1 Базовый синтаксис

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

### 8.2 Зарезервированные имена свойств в модели

| Свойство | Когда выполняется |
|---|---|
| `start` | Указывает начальное состояние |
| `enter` | Выполняется на каждом такте |
| `end` | Выполняется при завершении автомата |
| `behavior` | Выражение композиции подавтоматов |

### 8.3 Абстрактные и шаблонные модели

```but
abstract model Базовый {
    state Ожидание {}
}

template model Шаблон {
    state Работа {}
}
```

### 8.4 Вложенные автоматы

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

## 9. Состояния

### 9.1 Синтаксис

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

### 9.2 Зарезервированные имена свойств состояния

| Свойство | Когда выполняется |
|---|---|
| `enter` | На каждом такте нахождения в состоянии |
| `exit` | При выходе из состояния (перед переходом) |
| `before` | Перед входом в это состояние (в целевом состоянии) |

### 9.3 Порядок выполнения в такте

1. Выполнение глобального `enter` модели
2. Выполнение `enter` текущего состояния
3. Проверка переходов (`ref`) по порядку объявления
4. При выполнении условия перехода:
   - Выполнение `exit` текущего состояния
   - Выполнение `before` целевого состояния
   - Смена текущего состояния

---

## 10. Переходы (ref)

### 10.1 Синтаксис

```but
ref ЦелевоеСостояние;                     // Безусловный переход (всегда срабатывает)
ref ЦелевоеСостояние: условие;            // Условный переход
ref ЦелевоеСостояние: переменная = 10;   // Проверка равенства
ref ЦелевоеСостояние: x > 5 && y < 3;   // Составное условие
ref ЦелевоеСостояние: input = 1;         // Порт равен значению
```

### 10.2 Особенности

- Переходы проверяются в **порядке объявления** (первый сработавший выполняется)
- Если несколько переходов выполняются одновременно — это **недетерминизм** (BuT предупреждает)
- Если целевое состояние не определено — это **конечное состояние** (завершение)

### 10.3 Примеры

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

## 11. Условия (Condition)

Условия используются только в переходах `ref`. Это отдельная, упрощённая грамматика выражений (без присваиваний).

### 11.1 Операторы условий

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

### 11.2 Пример комплексного условия

```but
state Wait {
    ref Listen: counter > 1000;
    ref Error:  port4 > 10 && error_flag = 1;
    ref Next:   (S(Sub) = End) & (S(Main) = Done);
}
```

---

## 12. Выражения (Expression)

Выражения используются в телах функций, обработчиках состояний и инициализаторах.

### 12.1 Таблица приоритетов (от наинизшего к наивысшему)

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

### 12.2 Приведение типа

```but
let result: u16 = (input1 + input2) as u16;
```

### 12.3 Инициализатор агрегата

```but
let arr: [3: u8] = {1, 2, 3};
let mat: [2: [2: u8]] = {{1, 2}, {3, 4}};
```

### 12.4 Адресный литерал

```but
port p: bit = 0xFFB0:2;  // Адрес 0xFFB0, бит 2
```

---

## 13. Функции

### 13.1 Определение функции

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

### 13.2 Аннотации функций

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

### 13.3 Встроенный ассемблер

```but
fn critical_section() {
    assembly {
        // Низкоуровневые инструкции
    }
}
```

---

## 14. Свойства (Property)

Свойство — именованное описание поведения или значения через оператор `->`.

### 14.1 Синтаксис

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

### 14.2 Специальные свойства модели

```but
model M {
    start  -> S1;        // Начальное состояние
    enter  -> actions;   // Каждый такт
    end    -> cleanup;   // При завершении
    behavior -> A | B;   // Композиция подавтоматов
}
```

---

## 15. Аннотации

Аннотации предоставляют метаданные для компилятора, оптимизатора и кодогенератора.

### 15.1 Аннотации элемента

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

### 15.2 Глобальные аннотации файла

```but
#![fsm(Table)]           // Тип генерации: табличный автомат
#![fsm(Switch)]          // Тип генерации: switch-case
#![guard = "timer >= 0"] // Условие-стража
#![ltl = "G(timer)"]     // LTL-спецификация
```

---

## 16. Формулы LTL

### 16.1 Интеграция с BuT

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

### 16.2 Грамматика LTL

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

### 16.3 Операторы LTL

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

### 16.4 Примеры LTL-формул

```
// Всегда истинно z2 после was
[]was(z2)

// Сложная формула
(is(x1) W was(z1)) & [](was(z2) -> (is(x1) U was(z1)) & was(z1) -> (!is(x1) R was(z2)))
```

---

## 17. Импорт и механизм include

### 17.1 Синтаксис директивы import

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

### 17.2 Семантика разрешения импортов (IncludeResolver)

Резолвер (`but_middleware::include::IncludeResolver`) выполняет:

| Функция | Описание |
|---------|----------|
| **Поиск файлов** | Относительно базового файла, затем по путям поиска `-I` |
| **Транзитивность** | Импортируемый файл в свою очередь разрешает свои импорты |
| **Дедупликация** | Один файл включается ровно один раз (по каноническому пути) |
| **Защита от циклов** | Повторный импорт уже посещённого файла тихо пропускается |
| **Накопление ошибок** | При нескольких ошибочных импортах все они собираются за один проход |

### 17.3 Порядок включения

Определения из импортируемых файлов размещаются **до** определений текущего файла.
При транзитивных импортах порядок соответствует порядку обхода в глубину (DFS).

```
main.but:             Результирующий SourceUnit:
  import "a";  →        [дефиниции из level2.but]
  import "b";           [дефиниции из a.but]
  const X = 1;          [дефиниции из b.but]
                        const X = 1;
```

### 17.4 Пути поиска

Резолвер просматривает файлы в следующем порядке:

1. Директория файла, содержащего директиву `import`
2. Пути из командной строки (флаг `-I` инструмента `but`)
3. Абсолютные пути (если в строке импорта указан абсолютный путь)

### 17.5 Примеры использования include

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

### 17.6 Обработка ошибок

| Ситуация | Ошибка | Действие |
|----------|--------|----------|
| Файл не найден | `FileNotFound` | Обработка продолжается для других импортов |
| Синтаксическая ошибка в импорте | `ParseError` | Собирается в список ошибок |
| Ошибка чтения файла | `IoError` | Собирается в список ошибок |
| Циклический импорт | Молчаливо пропускается | Дедупликация по `HashSet<PathBuf>` |

### 17.7 Дополнительные директории поиска (`-I` / `--include-dir`)

При работе с проектами, где `.but`-файлы расположены в нескольких директориях
(общие библиотеки, платформенные HAL, vendor-зависимости), необходимо указать
дополнительные пути поиска для резолвера импортов.

#### Флаг командной строки

| Флаг | Сокращение | Описание |
|------|-----------|----------|
| `--include-dir DIR` | `-I DIR` | Добавить `DIR` в список путей поиска include-файлов |

Флаг можно указывать несколько раз — каждый вызов добавляет один путь:

```bash
but src/main.but --gen-c -I libs/platform -I libs/protocol --output-dir gen/
```

#### Порядок разрешения импортов

Резолвер ищет файл в следующем порядке (первое совпадение выигрывает):

1. **Директория самого файла**, содержащего `import "..."` (наивысший приоритет)
2. **Пути `-I`** в порядке их указания в командной строке (слева направо)
3. Если файл не найден ни в одном из путей — ошибка `FileNotFound`

```
Поиск import "platform" из src/main.but при -I libs/vendor -I libs/local:

  1. src/platform.but          ← директория файла (не существует)
  2. libs/vendor/platform.but  ← первый -I (проверяется первым)
  3. libs/local/platform.but   ← второй -I (достигается только если пункт 2 не найден)
```

#### Структура типичного проекта с библиотеками

```
project/
├── src/
│   └── main.but          # import "types"; import "frames";
├── libs/
│   ├── platform/
│   │   └── types.but     # ControlReg, StatusReg, MemAddress, IrqMask
│   └── protocol/
│       └── frames.but    # DataByte, FrameHeader, CRC32
└── gen/                  # сгенерированный код
```

```bash
# Запуск генерации с двумя путями поиска
but src/main.but --gen-c --gen-verilog --gen-st \
    -I libs/platform \
    -I libs/protocol \
    --output-dir gen/
```

#### Пример: main.but с многобиблиотечным импортом

```but
// src/main.but
// Типы из libs/platform/types.but и libs/protocol/frames.but
import "types";   // ControlReg, StatusReg, MemAddress, IrqMask
import "frames";  // DataByte, FrameHeader, CRC32

machine FrameReceiver(ctrl: ControlReg, status: StatusReg) {
    initial state Idle {
        on entry { ctrl[0] = 0b0; }
        when (status[1] == 0b1) goto Receiving;
    }
    state Receiving {
        var byte: DataByte;
        var crc:  CRC32;
        when (status[3] == 0b1) goto Idle;
    }
}
```

#### Использование в скриптах пакетной генерации

**Bash (gen_all.sh):**
```bash
./scripts/gen_all.sh src/ gen/ -I libs/platform -I libs/protocol
```

**PowerShell (gen_all.ps1):**
```powershell
.\scripts\gen_all.ps1 src\ gen\ -IncludeDir @("libs\platform", "libs\protocol")
```

**Windows CMD (gen_all.bat):**
```bat
scripts\gen_all.bat src gen -I libs\platform -I libs\protocol
```

#### API резолвера (Rust)

```rust
use but_middleware::include::IncludeResolver;

let mut resolver = IncludeResolver::from_file("src/main.but");
resolver.add_search_path("libs/platform");
resolver.add_search_path("libs/protocol");

let unit = resolver.resolve()?;
```

`add_search_path` возвращает `&mut Self`, поэтому вызовы можно объединять в цепочку:

```rust
let unit = IncludeResolver::from_file("src/main.but")
    .add_search_path("libs/platform")
    .add_search_path("libs/protocol")
    .resolve()?;
```

---

## 18. Структуры и перечисления

### 18.1 Структуры

```but
#[repr(C)]
struct Packet {
    header:  u8;
    payload: [8: u8];
    crc:     u16;
}
```

### 18.2 Перечисления

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

### 18.3 Псевдонимы типов

Псевдонимы объявляются на уровне файла, модели или состояния. Они **полностью прозрачны** — кодогенератор разворачивает их до базового типа перед генерацией кода.

#### Синтаксис

```but
type <Имя> = <Тип>;
```

#### Примеры объявлений

```but
// Простые псевдонимы для встроенных типов
type bool  = bit;
type byte  = [8: bit];
type u8    = [8: bit];
type word  = [16: bit];

// Доменные псевдонимы — улучшают читаемость
type Counter    = u32;   // Счётчик шагов
type SensorData = u8;    // Значение датчика 0–255
type Flag       = bool;  // Флаг состояния
type Speed      = f64;   // Скорость (числа с плав. точкой)

// Цепочки псевдонимов — разворачиваются рекурсивно
type Byte         = u8;
type SensorValue  = Byte;    // SensorValue → Byte → u8 → uint8_t
```

#### Использование псевдонимов в портах и переменных

```but
type Counter    = u32;
type SensorData = u8;
type Flag       = bool;

// Порты с псевдонимами типов
port step    : Counter    = 0;      // → uint32_t step
port reading : SensorData = 0x00;   // → uint8_t  reading
port alarm   : Flag       = false;  // → int      alarm

model Monitor {
    let limit: Counter = 100;       // локальная переменная

    state Watch {
        ref Alert: reading >= 0x80;
        ref Watch;
        enter -> { step += 1; }
    }
    state Alert {
        ref Watch: alarm = false;
        enter -> { alarm = true; step = 0; }
    }

    start -> Watch;
}
```

#### Генерируемый C-заголовок

Для приведённого выше кода `but-codegen` генерирует:

```c
#ifndef __MONITOR_H__
#define __MONITOR_H__

#include <stdint.h>

typedef enum {
    MONITOR_WATCH,
    MONITOR_ALERT
} Monitor_State_t;

/* Псевдонимы раскрыты: Counter→u32→uint32_t, SensorData→u8→uint8_t, Flag→bool→int */
extern uint32_t step;
extern uint8_t  reading;
extern int      alarm;

void Monitor_init(void);
void Monitor_step(void);
Monitor_State_t Monitor_state(void);

#endif /* __MONITOR_H__ */
```

#### Генерируемый ST-блок (IEC 61131-3)

```pascal
(* Сгенерировано but-codegen *)
FUNCTION_BLOCK MONITOR
VAR
    state : INT;
    (* Counter→u32→DWORD, SensorData→u8→BYTE, Flag→bool→BOOL *)
    step    AT %Q : DWORD := 0;
    reading AT %Q : BYTE  := 0;
    alarm   AT %Q : BOOL  := 0;
END_VAR
```

#### Ограничения

- Псевдонимы на структуры и перечисления в Codegen не поддерживаются (TODO #5).
- Псевдонимы в выражениях (приведение типа `as`) разворачиваются только при объявлении переменных; внутри тел функций и состояний тип подставляется как есть (TODO для будущих версий).
- Циклические псевдонимы (`type A = B; type B = A;`) не вызывают зависания — алгоритм ограничен 8 уровнями рекурсии; после исчерпания возвращается последний разрешённый тип.

---

## 19. Композиция поведения

Свойство `behavior` задаёт способ компоновки подмоделей внутри модели-контейнера. Вместо явных состояний и переходов модель описывает, **как** запускать другие модели и в каком порядке. Свойство `end` задаёт действие, выполняемое при завершении модели или композиции.

### 19.1 Синтаксис свойства `behavior`

```bnf
behavior -> SequentialExpr ";"
          | ParallelExpr ";"
          | ChoiceExpr ";"

SequentialExpr ::= "sequential" "(" Identifier ("," Identifier)* ")"
ParallelExpr   ::= "parallel"   "(" Identifier ("," Identifier)* ")"
ChoiceExpr     ::= "choice"     "(" Identifier ("," Identifier)* ")"
```

Свойство `behavior` объявляется на уровне модели и не совмещается с явными состояниями (`state`). Идентификаторы в скобках — имена других моделей (подмоделей).

### 19.2 Типы компоновки

| Тип | Синтаксис | Семантика |
|---|---|---|
| **Последовательная** | `sequential(M1, M2, …, Mn)` | M1 → M2 → … → Mn; следующая запускается после завершения предыдущей |
| **Параллельная** | `parallel(M1, M2, …, Mn)` | Все Mi выполняются одновременно; завершение — когда все завершились |
| **Выбор** | `choice(M1, M2, …, Mn)` | Выполняется первая модель в списке; когда она завершается — вся композиция завершена |

### 19.3 Свойство `end` — действие при завершении

Свойство `end` задаёт блок кода или выражение, которое выполняется автоматически при завершении модели:

- для обычной FSM — когда автомат переходит в **терминальное состояние** (состояние без исходящих переходов `ref`);
- для модели с `behavior` — когда вся композиция завершается.

```but
// Блок кода:
end -> { переменная = 1; вызов_функции(); }

// Выражение:
end -> pipeline_complete = 1;
```

### 19.4 Терминальные состояния

Состояние является **терминальным**, если оно не содержит ни одного перехода `ref`. При входе в такое состояние:

1. устанавливается внутренний флаг завершения модели (`is_done() == 1`);
2. если объявлено свойство `end` — выполняется его тело.

```but
model Calibrate {
    state Running {
        ref Done : samples_ready;  // есть переход → не терминальное
    }
    state Done { }  // нет переходов → терминальное

    start -> Running;
    end -> { calibration_done = 1; }  // выполнится при входе в Done
}
```

### 19.5 Примеры

#### Последовательная компоновка

```but
/// Пайплайн: Calibrate → Process → Store (каждый этап ждёт завершения предыдущего)
model Pipeline {
    behavior -> sequential(Calibrate, Process, Store);
    end -> { pipeline_complete = 1; }
}
```

#### Параллельная компоновка

```but
/// Запускает Calibrate и Process одновременно; завершается, когда оба завершились
model ParallelPrep {
    behavior -> parallel(Calibrate, Process);
    end -> { prep_done = 1; }
}
```

#### Выбор первой модели

```but
/// Выполняет первую модель из списка (Calibrate); при её завершении — завершается сам
model FastOrSlow {
    behavior -> choice(Calibrate, Store);
    end -> { choice_done = 1; }
}
```

### 19.6 Обращение к состоянию подавтомата

Для проверки состояния подавтомата внутри условий переходов используется функция `S()`:

```but
// Проверить, что подавтомат Map завершил работу
ref Join: (S(Map) = End);

// Обе ветви параллельной композиции завершились
ref Join: (S(Reduce) = End) & (S(Map) = End);
```

### 19.7 Полный пример — behavior_example.but

```but
model Calibrate {
    state Running { ref Done : samples_ready; }
    state Done { }
    start -> Running;
    end -> { calibration_done = 1; }
}

model Process {
    state Idle   { ref Active : start_signal; }
    state Active { ref Done : result_ready; ref Idle : !start_signal; }
    state Done   { enter -> { status = 2; } }
    start -> Idle;
    end -> { process_done = 1; }
}

model Store {
    state Writing  { ref Complete : write_done; }
    state Complete { }
    start -> Writing;
    end -> { stored = 1; }
}

// Последовательная компоновка
model Pipeline {
    behavior -> sequential(Calibrate, Process, Store);
    end -> { pipeline_complete = 1; }
}

// Параллельная компоновка
model ParallelPrep {
    behavior -> parallel(Calibrate, Process);
    end -> { prep_done = 1; }
}

// Выбор
model FastOrSlow {
    behavior -> choice(Calibrate, Store);
    end -> { choice_done = 1; }
}

port samples_ready    : bit = 0;
port start_signal     : bit = 0;
port result_ready     : bit = 0;
port write_done       : bit = 0;
port status           : bit = 0;
port calibration_done : bit = 0;
port process_done     : bit = 0;
port stored           : bit = 0;
port pipeline_complete: bit = 0;
port prep_done        : bit = 0;
port choice_done      : bit = 0;
```

---

## 20. Кодогенерация

### 20.1 Поддерживаемые целевые форматы

| Формат | Команда | Описание |
|---|---|---|
| C99 | `--gen-c` | ANSI C с заголовком (.h) и исходником (.c) |
| Verilog | `--gen-verilog` | RTL-модуль Verilog для FPGA/ASIC |
| Structured Text | `--gen-st` | IEC 61131-3 для ПЛК |
| LC-3 ASM | `--gen-lc3` | Ассемблер учебной архитектуры LC-3 |
| ARM Thumb | `--gen-thumb` | Ассемблер ARMv7-M Thumb |

### 20.2 Модель «один файл на источник»

Начиная с версии 2026-03-11 кодогенератор формирует **один выходной файл (пару файлов) на каждый входной `.but`-файл**, а не по одному файлу на каждую модель. Все модели из исходного файла объединяются в единый выходной артефакт.

Базовое имя выходных файлов совпадает с именем входного файла без расширения (`<basename>`).

| Целевой формат | Выходные файлы |
|---|---|
| C | `<basename>.h` + `<basename>.c` |
| Verilog | `<basename>.v` |
| Structured Text | `<basename>.FB.DECL.st` + `<basename>.FB.PRGS.st` |
| LC-3 ASM | `<basename>.asm` |
| ARM Thumb | `<basename>.S` |

Пример: при запуске `but pipeline.but --gen-c --gen-verilog --output-dir gen/` будут созданы:
```
gen/c/pipeline.h
gen/c/pipeline.c
gen/verilog/pipeline.v
```

### 20.3 Функция `is_done()` и флаг завершения

Для каждой модели, у которой есть хотя бы одно **терминальное состояние** (состояние без исходящих `ref`), кодогенератор формирует функцию завершённости:

#### C

```c
// Объявление в .h:
int ModelName_is_done(void);

// Реализация в .c:
int ModelName_is_done(void) {
    return _state == MODELNAME_TERMSTATE;
}
```

Для моделей с несколькими терминальными состояниями проверяются все варианты через `||`.

#### ST (IEC 61131-3)

В секции `VAR` функционального блока добавляется:
```pascal
is_done : BOOL;
```
Флаг устанавливается в `TRUE` при входе в терминальное состояние.

#### Verilog

```verilog
output reg done;
// В always-блоке при входе в терминальное состояние:
// done <= 1'b1;
```

#### LC-3 Assembly / ARM Thumb

Флаг завершения хранится в памяти (`MODEL_DONE` / `_model_done_flag`) и устанавливается при входе в терминальное состояние. Вызываться извне — через `LD` (LC-3) или `ldr` (Thumb).

### 20.4 Генерация кода для `behavior` — компоновочные модели

Модели с `behavior` не генерируют собственные состояния FSM — вместо этого кодогенератор создаёт **контроллер компоновки**, который управляет запуском и завершением подмоделей.

#### 20.4.1 Последовательная компоновка (C)

```but
model Pipeline {
    behavior -> sequential(Calibrate, Process, Store);
    end -> { pipeline_complete = 1; }
}
```

Генерируется фазовый автомат:

```c
// pipeline.h
typedef enum {
    PIPELINE_PHASE_CALIBRATE,
    PIPELINE_PHASE_PROCESS,
    PIPELINE_PHASE_STORE,
    PIPELINE_DONE
} Pipeline_Phase_t;

void pipeline_init(void);
void pipeline_step(void);
int  pipeline_is_done(void);

// pipeline.c
static Pipeline_Phase_t _pipeline_phase;

void pipeline_init(void) {
    _pipeline_phase = PIPELINE_PHASE_CALIBRATE;
    calibrate_init();
}

void pipeline_step(void) {
    switch (_pipeline_phase) {
        case PIPELINE_PHASE_CALIBRATE:
            calibrate_step();
            if (calibrate_is_done()) {
                _pipeline_phase = PIPELINE_PHASE_PROCESS;
                process_init();
            }
            break;
        case PIPELINE_PHASE_PROCESS:
            process_step();
            if (process_is_done()) {
                _pipeline_phase = PIPELINE_PHASE_STORE;
                store_init();
            }
            break;
        case PIPELINE_PHASE_STORE:
            store_step();
            if (store_is_done()) {
                _pipeline_phase = PIPELINE_DONE;
                pipeline_complete = 1;   /* end handler */
            }
            break;
        case PIPELINE_DONE:
            break;
    }
}

int pipeline_is_done(void) {
    return _pipeline_phase == PIPELINE_DONE;
}
```

#### 20.4.2 Параллельная компоновка (C)

```but
model ParallelPrep {
    behavior -> parallel(Calibrate, Process);
    end -> { prep_done = 1; }
}
```

```c
void parallelprep_init(void) {
    calibrate_init();
    process_init();
}

void parallelprep_step(void) {
    if (!calibrate_is_done()) calibrate_step();
    if (!process_is_done())   process_step();
    if (calibrate_is_done() && process_is_done()) {
        prep_done = 1;   /* end handler */
    }
}

int parallelprep_is_done(void) {
    return calibrate_is_done() && process_is_done();
}
```

#### 20.4.3 Выбор (C)

```but
model FastOrSlow {
    behavior -> choice(Calibrate, Store);
    end -> { choice_done = 1; }
}
```

```c
void fastorslow_init(void) { calibrate_init(); }

void fastorslow_step(void) {
    calibrate_step();
    if (calibrate_is_done()) {
        choice_done = 1;   /* end handler */
    }
}

int fastorslow_is_done(void) { return calibrate_is_done(); }
```

#### 20.4.4 Structured Text (IEC 61131-3)

Для `sequential` генерируется `FUNCTION_BLOCK` с переменной `phase : INT` и секцией `CASE`:

```pascal
FUNCTION_BLOCK PIPELINE
VAR
    phase : INT := 0;
    done  : BOOL := FALSE;
    fb_calibrate : CALIBRATE;
    fb_process   : PROCESS;
    fb_store     : STORE;
END_VAR

CASE phase OF
    0: fb_calibrate();
       IF fb_calibrate.is_done THEN phase := 1; END_IF;
    1: fb_process();
       IF fb_process.is_done THEN phase := 2; END_IF;
    2: fb_store();
       IF fb_store.is_done THEN
           pipeline_complete := 1;
           done := TRUE;
       END_IF;
END_CASE;
```

#### 20.4.5 Verilog

```verilog
module Pipeline(input clk, input rst, output reg done);
    reg [1:0] phase;
    // enable-сигналы и done-входы для каждой подмодели:
    reg  calibrate_en; wire calibrate_done;
    reg  process_en;   wire process_done;
    reg  store_en;     wire store_done;

    always @(posedge clk or posedge rst) begin
        if (rst) begin phase <= 0; done <= 0; end
        else case (phase)
            2'd0: if (calibrate_done) phase <= 2'd1;
            2'd1: if (process_done)   phase <= 2'd2;
            2'd2: if (store_done) begin
                      done <= 1'b1;
                  end
        endcase
    end
endmodule
```

#### 20.4.6 LC-3 Assembly

Контроллер компоновки реализован через суб-процедуры (`JSR`/`RET`):

```asm
PIPELINE_COMP_INIT
    JSR CALIBRATE_INIT
    JSR PROCESS_INIT
    JSR STORE_INIT
    AND R0, R0, #0
    ST R0, PIPELINE_PHASE

PIPELINE_COMP_STEP
    ; Фаза 0: Calibrate
    LD R0, PIPELINE_PHASE
    LD R1, PIPELINE_CONST_0
    NOT R2, R1
    ADD R2, R2, #1
    ADD R2, R0, R2
    BRz PIPELINE_DO_CALIBRATE
    ; Фаза 1: Process ...
    ...

PIPELINE_END_HANDLER
    ; end: pipeline_complete = 1
    RET
```

#### 20.4.7 ARM Thumb

```asm
pipeline_init:
    push {lr}
    bl calibrate_init
    bl process_init
    bl store_init
    ldr r0, =_pipeline_phase
    movs r1, #0
    str r1, [r0]
    pop {pc}

pipeline_step:
    push {lr}
    ldr r0, =_pipeline_phase
    ldr r1, [r0]
    cmp r1, #0
    bne .pipeline_phase1
    bl calibrate_step
    bl calibrate_is_done
    cmp r0, #0
    beq .pipeline_done
    ; переход к следующей фазе...
    ...
    pop {pc}

pipeline_end_handler:
    push {lr}
    ; end: pipeline_complete = 1
    pop {pc}
```

### 20.5 Пример генерации C-кода (FSM)

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

### 20.6 Разворачивание псевдонимов типов в кодогенерации

При генерации кода все пользовательские псевдонимы типов (`type T = U`) автоматически разворачиваются до базового типа. Это происходит в `CodegenContext`, который при инициализации строит **таблицу псевдонимов** из всех `TypeDefinition` в исходном файле.

#### Алгоритм разворачивания

1. Для каждого `type T = U` в исходном файле — добавить запись `T → U` в таблицу.
2. При генерации типа переменной/порта — попытаться найти имя в таблице.
3. Если найдено — подставить определение и повторить шаги 2–3 рекурсивно.
4. Ограничение глубины рекурсии: **8 уровней** (защита от циклов).
5. Если псевдоним не найден — использовать имя как есть (например, встроенные `u8`, `u32` — обрабатываются на следующем шаге по таблице встроенных типов).

#### Пример

**Исходный файл (sensor.but):**

```but
type Byte  = u8;
type Word  = u16;
type Count = Word;   // цепочка: Count → Word → u16

port raw_value : Byte  = 0x00;
port channel   : Count = 0;

model Sensor {
    state Active { }
    start -> Active;
}
```

**Сгенерированный заголовок (sensor.h):**

```c
#ifndef __SENSOR_H__
#define __SENSOR_H__

#include <stdint.h>

typedef enum {
    SENSOR_ACTIVE
} Sensor_State_t;

extern uint8_t  raw_value;  /* Byte  → u8   → uint8_t  */
extern uint16_t channel;    /* Count → Word → u16 → uint16_t */

void Sensor_init(void);
void Sensor_step(void);
Sensor_State_t Sensor_state(void);

#endif /* __SENSOR_H__ */
```

#### Таблица соответствия для ST (IEC 61131-3)

| Базовый тип BuT | Тип ST |
|---|---|
| `bit`, `u8`, `byte` | `BYTE` |
| `u16` | `WORD` |
| `u32` | `DWORD` |
| `u64` | `LWORD` |
| `i32`, `int` | `INT` |
| `i64` | `INT` |
| `f32`, `f64`, `real` | `REAL` |
| `bool` | `BOOL` |
| `str`, `string` | `STRING` |

### 20.7 Настраиваемый отступ при генерации кода

Все генераторы кода (C, ST, Verilog) поддерживают настройку стиля отступа через параметры CLI или программный интерфейс (`CodegenContext`).

#### Параметры CLI

| Флаг | Значение по умолчанию | Описание |
|---|---|---|
| `--indent-size N` | `4` | Количество пробелов на один уровень вложенности |
| `--indent-tab` | выключен | Использовать символ табуляции (`\t`) вместо пробелов |

Флаг `--indent-tab` имеет приоритет над `--indent-size`.

#### Примеры

```bash
# Стандартный отступ: 4 пробела (по умолчанию)
but source.but --gen-c

# Отступ в 2 пробела
but source.but --gen-c --indent-size 2

# Отступ символом табуляции
but source.but --gen-c --indent-tab

# Нулевой отступ (минимальный, без форматирования)
but source.but --gen-c --indent-size 0
```

#### Эффект на генерируемый код

Для одной и той же модели разные стили дают следующий результат (фрагмент C-кода):

**`--indent-size 4` (по умолчанию):**
```c
void Model_step(void) {
    switch (_state) {
        case MODEL_IDLE: {
            if (start) {
                _state = MODEL_ACTIVE;
            }
            break;
        }
    }
}
```

**`--indent-size 2`:**
```c
void Model_step(void) {
  switch (_state) {
    case MODEL_IDLE: {
      if (start) {
        _state = MODEL_ACTIVE;
      }
      break;
    }
  }
}
```

**`--indent-tab`:**
```c
void Model_step(void) {
	switch (_state) {
		case MODEL_IDLE: {
			if (start) {
				_state = MODEL_ACTIVE;
			}
			break;
		}
	}
}
```

#### Программный интерфейс

```rust
use but_codegen::{CodegenContext, IndentStyle, generate_c_all};

// Создать контекст с отступом в 2 пробела
let ctx = CodegenContext::from_source_with_indent(&source, IndentStyle::Spaces(2));

// Создать контекст с табуляцией
let ctx_tab = CodegenContext::from_source_with_indent(&source, IndentStyle::Tab);

// Сгенерировать C-код
let output = generate_c_all(&source, &ctx);
```

#### `IndentStyle`

```rust
pub enum IndentStyle {
    /// Отступ пробелами: количество пробелов на уровень вложенности
    Spaces(usize),
    /// Отступ символом табуляции: один `\t` на уровень
    Tab,
}
```

Метод `level(n: usize) -> String` возвращает строку отступа для уровня `n`:

| Стиль | `level(0)` | `level(1)` | `level(2)` |
|---|---|---|---|
| `Spaces(4)` | `""` | `"    "` | `"        "` |
| `Spaces(2)` | `""` | `"  "` | `"    "` |
| `Tab` | `""` | `"\t"` | `"\t\t"` |

### 20.8 Использование CLI

```bash
# Симуляция
but source.but --simulate --steps 20 --port input=1

# Визуализация в DOT
but source.but --visualize --output-dir ./gen

# Генерация всех форматов
but source.but --all --output-dir ./gen

# Только C и Verilog
but source.but --gen-c --gen-verilog --output-dir ./gen

# C-код с отступом в 2 пробела
but source.but --gen-c --indent-size 2 --output-dir ./gen

# Все форматы с табуляцией
but source.but --all --indent-tab --output-dir ./gen
```

---

## 21. Полные примеры программ

### 21.1 Простой двухсостоянчый автомат (задержка)

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

### 21.2 Многоуровневый автомат с LTL-верификацией

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

### 21.3 Определение типов и перечислений

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

## Приложение A: Статус реализации и TODO

> Анализ выполнен по состоянию на 2026-03-11.
> Источники: `grammar/but/src/`, `middleware/src/`, `codegen/src/`, `simulators/src/`, `visual/src/`.

### Условные обозначения

| Символ | Значение |
|--------|----------|
| ✅ | Полностью реализовано |
| 🟡 | Частично реализовано |
| ❌ | Не реализовано (конструкция есть в грамматике/AST, но отсутствует в обработке) |
| ⏭ | Явно пропускается (`println!("Skip ...")`) |

---

### A.1 Конструкции верхнего уровня (SourceUnitPart)

| Конструкция | Грамматика | AST | Middleware | Codegen | Simulator |
|-------------|:---:|:---:|:---:|:---:|:---:|
| `import "файл"` | ✅ | ✅ | ✅ | ❌ | ❌ |
| `model M { ... }` | ✅ | ✅ | ✅ | 🟡 | ✅ |
| `state S { ... }` | ✅ | ✅ | ✅ | 🟡 | ✅ |
| `enum E { ... }` | ✅ | ✅ | ✅ | ❌ | ❌ |
| `struct S { ... }` | ✅ | ✅ | ✅ | ❌ | ❌ |
| `type T = ...` | ✅ | ✅ | ✅ | ✅ | 🟡 |
| `fn f(...) { ... }` | ✅ | ✅ | ✅ | 🟡 | 🟡 |
| `const X: T = ...` | ✅ | ✅ | ✅ | 🟡 | ✅ |
| `let [mut] x: T = ...` | ✅ | ✅ | ✅ | 🟡 | ✅ |
| `port p: T = ...` | ✅ | ✅ | ✅ | ✅ | ✅ |
| `property P { ... }` | ✅ | ✅ | ✅ | ❌ | 🟡 |
| `formula F { ... }` | ✅ | ✅ | ⏭ | ❌ | 🟡 |
| `#[annotation]` | ✅ | ✅ | ⏭ | ❌ | ❌ |
| `#![global_annotation]` | ✅ | ✅ | ⏭ | ❌ | ❌ |
| `using ...` | ✅ | ✅ | ⏭ | ❌ | ❌ |
| `error E { ... }` | ✅ | ✅ | ⏭ | ❌ | ❌ |

---

### A.2 Тело модели / тела состояний

| Конструкция | Middleware | Codegen | Simulator |
|-------------|:---:|:---:|:---:|
| `ref NextState;` (переход без условия) | ✅ | 🟡 | ✅ |
| `ref NextState: condition;` | ✅ | 🟡 | ✅ |
| `start -> State;` | ✅ | 🟡 | ✅ |
| `enter -> action;` | ✅ | 🟡 | ✅ |
| `exit -> action;` | ✅ | 🟡 | ✅ |
| `before -> action;` | ✅ | 🟡 | ✅ |
| `end -> action;` | ✅ | 🟡 | ✅ |
| Вложенная модель (`model` внутри `state`) | 🟡 | ❌ | ❌ |
| Вложенное состояние (`state` внутри `state`) | 🟡 | ❌ | ❌ |
| `behavior -> M1 \| M2;` (параллельное) | 🟡 | ❌ | ✅ |
| `behavior -> M1 + M2;` (последовательное) | 🟡 | ❌ | ✅ |
| `abstract model/state` | ✅ (парсинг) | ❌ | ❌ |
| `template model/state` | ✅ (парсинг) | ❌ | ❌ |
| `model M = Base { ... }` (наследование) | ✅ (парсинг) | ❌ | ❌ |

---

### A.3 Система типов

| Тип | Middleware | Codegen | Simulator |
|-----|:---:|:---:|:---:|
| `bit` | ✅ | ✅ | ✅ |
| `bool` | ✅ | ✅ | ✅ |
| `int`, `u8`–`u128`, `i8`–`i128` | ✅ | ✅ | ✅ |
| `real`, `f32`, `f64` | ✅ | ✅ | 🟡 |
| `str` / `string` | ✅ | ✅ | 🟡 |
| `[N: T]` — массив фиксированной длины | ✅ | ✅ | ❌ |
| Псевдоним (`type T = U`) | ✅ | ✅ | 🟡 |
| Struct / Enum как типы | ✅ | ❌ | ❌ |
| Тип функции | ✅ (парсинг) | ❌ | ❌ |
| Адресный тип (с битовым диапазоном) | ✅ | 🟡 | ❌ |

---

### A.4 Выражения

#### Арифметические операторы

| Оператор | Codegen | Simulator |
|----------|:---:|:---:|
| `+`, `-`, `*`, `/`, `%` | ✅ | ✅ |
| `**` (возведение в степень) | 🟡 | ✅ |
| Унарный `-` | ✅ | ✅ |
| Унарный `+` | ✅ | ❌ |

#### Битовые операторы

| Оператор | Codegen | Simulator |
|----------|:---:|:---:|
| `&`, `\|`, `^` | ✅ | ✅ |
| `~` (битовое НЕ) | ✅ | ❌ |
| `<<`, `>>` | ✅ | ✅ |

#### Операторы сравнения и логические

| Оператор | Codegen | Simulator |
|----------|:---:|:---:|
| `==`, `!=`, `<`, `<=`, `>`, `>=` | ✅ | ✅ |
| `&&`, `\|\|`, `!` | ✅ | ✅ |
| `? :` (тернарный) | ✅ | ✅ |

#### Присваивание

| Оператор | Codegen | Simulator |
|----------|:---:|:---:|
| `=`, `+=`, `-=`, `*=`, `/=`, `%=` | ✅ | ✅ |
| `&=`, `\|=` | ✅ | ✅ |
| `^=`, `<<=`, `>>=` | ✅ | ❌ |
| `++x`, `x++`, `--x`, `x--` | ✅ | ✅ |

#### Доступ к данным

| Выражение | Codegen | Simulator |
|-----------|:---:|:---:|
| `expr.member` | ✅ | 🟡 |
| `expr[index]` | ✅ | 🟡 |
| `expr[a:b]` (срез) | ❌ | ❌ |
| `expr as T` (приведение типа) | ✅ | ❌ |
| `f(args)` (вызов функции) | ✅ | 🟡 |

---

### A.5 Операторы управления потоком (Statement)

| Оператор | Codegen | Simulator |
|----------|:---:|:---:|
| `{ ... }` блок | ✅ | ✅ |
| `if / else` | ✅ | ✅ |
| `while` | ✅ | ✅ |
| `for` | ✅ | ✅ |
| `do { } while` | ✅ | 🟡 |
| `return` | ✅ | ✅ |
| `break` | ✅ | ❌ |
| `continue` | ✅ | ❌ |
| `assembly { }` | ❌ | ❌ |
| `formula { }` внутри тела | ❌ | ❌ |

---

### A.6 LTL-формулы (Linear Temporal Logic)

| Конструкция | Middleware | Codegen | Simulator |
|-------------|:---:|:---:|:---:|
| `G(φ)` — глобально | 🟡 (извлечение) | ✅ | ✅ |
| `F(φ)` — в будущем | 🟡 | ✅ | ✅ |
| `X(φ)` — следующий такт | 🟡 | ✅ | ✅ |
| `U(φ, ψ)` — до тех пор пока | 🟡 | ✅ | ✅ |
| `R(φ, ψ)` — release | 🟡 | ❌ | ❌ |
| `W(φ, ψ)` — weak until | 🟡 | ❌ | ❌ |
| `#![ltl = "..."]` атрибут модели | 🟡 (парсинг) | ❌ | 🟡 |
| `#![guard = "..."]` | 🟡 (парсинг) | ❌ | ❌ |
| Проверка выполнения свойства | — | — | 🟡 |

---

### A.7 TODO — Приоритетный список доработок

#### 🔴 Критические (конструкции есть в грамматике, но нигде не обрабатываются)

1. **`error E { ... }`** — определения ошибок полностью игнорируются во всём стеке.
   Необходимо: семантический анализ, использование в переходах, кодогенерация.

2. **`assembly { ... }`** — AST-узел `Statement::Assembly` существует, но ни Codegen, ни Simulator не обрабатывают его.
   Необходимо: C-кодогенерация (`__asm__`), заглушка в Simulator.

3. **`using ...`** — директива полностью пропускается.
   Необходимо: разрешение пространств имён, применение псевдонимов.

4. ~~**Тип-псевдоним в Codegen/Simulator**~~ — ✅ **Реализовано (2026-03-10).**
   Добавлена таблица псевдонимов `type_aliases: HashMap<String, Type>` в `CodegenContext`.
   Функции `resolve_alias()`, `type_to_c_ctx()`, `type_to_st_ctx()` в `codegen/src/condition.rs`
   раскрывают цепочки псевдонимов (до 8 уровней). Генераторы `c.rs` и `st.rs` используют
   контекстные функции. В `simulators/src/builder.rs` добавлены сбор псевдонимов и
   `default_value_for_type()` для корректной инициализации переменных по типу-псевдониму.
   Пример: `grammar/tests_data/BuT/examples/type_alias.but`. Покрытие: 62 новых теста.

5. **`struct` и `enum` в Codegen** — объявляются в middleware, но для них нет генераторов C-структур / Verilog-параметров / ST-типов.
   Необходимо: `struct_to_c()`, `enum_to_c()`, соответствующие генераторы для остальных бэкендов.

#### 🟠 Важные (частичная реализация, видимые пробелы)

6. **Вложенные модели/состояния** — парсинг есть, middleware частично видит, но Codegen и Simulator не умеют опускаться в иерархию.
   Необходимо: рекурсивный обход AST в `builder.rs` и `condition.rs`.

7. **`template` / `abstract` модели и состояния** — ключевые слова распознаются, но нет семантики инстанцирования шаблонов и проверки абстрактности.
   Необходимо: разворачивание шаблонов в middleware, проверка полноты реализации.

8. **`model M = Base { ... }`** — наследование/расширение модели разбирается грамматикой, но не реализовано в middleware.
   Необходимо: механизм наследования состояний и переходов.

9. **Формульные блоки** — `formula { ... }` внутри тела функции/состояния (`Statement::Formula`) не обрабатываются ни в Codegen, ни в Simulator.
   Необходимо: трансляция в LTL-мониторинг.

10. **`break` / `continue` в Simulator** — операторы распознаются в executor, но не прерывают исполнение цикла.
    Необходимо: реализация через механизм исключений/сигналов в `executor.rs`.

11. **Срез массива `expr[a:b]`** — AST-узел есть, но ни Codegen, ни Simulator его не реализуют.
    Необходимо: генерация битовых извлечений в C и Verilog; интерпретация в Simulator.

12. **`as` (приведение типа) в Simulator** — `Expression::As` не вычисляется.
    Необходимо: реализация `eval_expr` для `As` с числовым и битовым приведением.

13. **`~` (битовое НЕ) в Simulator** — Codegen генерирует `~`, но `eval_expr` в simulators не обрабатывает этот оператор.

#### 🟡 Желательные улучшения

14. **`#![ltl = "..."]` и `#![guard = "..."]`** — атрибуты разбираются, но не используются при верификации.
    Необходимо: интеграция с LTL-монитором в Simulator.

15. **Аннотации `#[...]`** — полностью пропускаются. Частично используются в Codegen только для `#[extern(C)]`, `#[inline(...)]`, `#[unused]`.
    Необходимо: систематическая обработка атрибутов в middleware.

16. **LTL операторы `R` (release) и `W` (weak until)** — есть в LTL-грамматике, но нет в Codegen и Simulator.

17. **Унарный `+` в Simulator** — тривиальная доработка: `eval_expr(UnaryPlus(e)) = eval_expr(e)`.

18. **Операторы `^=`, `<<=`, `>>=` в Simulator** — не реализованы в ветке `Assign*`.

19. **Пользовательские функции в Simulator** — вызовы функций возвращают `Value::None` с предупреждением. Необходимо: хранение тела функции и его интерпретация.

20. **Псевдонимы типов в Simulator** — при разыменовании псевдонима возвращается `Value::None`. Необходима таблица типов в контексте выполнения.

22. ~~**Изменяемый отступ при генерации кода**~~ — ✅ **Реализовано.** `IndentStyle` (`Spaces(N)` / `Tab`) в `CodegenContext`; флаги CLI `--indent-size N` и `--indent-tab`. Применяется во всех генераторах (C, ST, Verilog). Подробнее: раздел 20.4.

---

## Приложение B: Семантические ограничения

| Ограничение | Описание |
|---|---|
| Уникальность имён | Перечисления, функции, переменные, структуры и типы должны иметь уникальные имена |
| Начальное состояние | Каждая модель должна иметь объявление `start` |
| Достижимость | Состояния без входящих переходов могут быть помечены предупреждением |
| Недетерминизм | Множество одновременно истинных переходов — предупреждение |

## Приложение C: Интеграция с инструментами

| Инструмент | Поддержка |
|---|---|
| Cargo (Rust) | Полная интеграция через рабочее пространство |
| CI/CD | GitHub Actions, Travis CI |
| Покрытие кода | codecov.yml конфигурация |
| Graphviz | Визуализация через DOT-формат |
| LALRPOP 0.23 | Генерация парсера из `.lalrpop`-файлов |

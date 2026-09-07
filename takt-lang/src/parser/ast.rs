//! Узлы абстрактного синтаксического дерева (АСД) языка Takt.
//!
//! Этот модуль определяет все типы, образующие АСД после синтаксического анализа.
//! Каждый узел несёт в себе информацию о местоположении ([`Location`]) в исходном
//! тексте - это позволяет генерировать точные диагностические сообщения.
//!
//! ## Иерархия узлов
//!
//! - [`Model`] - корневой узел программы Takt; содержит список [`ModelElement`].
//! - [`ModelElement`] - элемент модели: состояние, переменная, тип, условие,
//!   именованный блок, функция или импорт.
//! - [`StateDefine`] - определение состояния с элементами [`StateElement`].
//! - [`Expression`] - выражение: литерал, идентификатор, операция или вызов.
//! - [`Statement`] - оператор: присваивание, ветвление, цикл, возврат и т.п.
//! - [`Type`] - аннотация типа: `bit`, `bool`, массив, псевдоним.
//! - [`VariableDefine`] - объявление переменной, порта или константы.
//! - [`FunctionDefine`] - объявление функции (локальной или внешней).

use crate::diagnostics::Location;
#[cfg(feature = "ast-serde")]
use serde::{Deserialize, Serialize};

// Узел `Expression` вынесен в `parser::ast_expr`. Реэкспорт держит путь
// `parser::ast::Expression` для lalrpop/семантики/генераторов.
pub use crate::parser::ast_expr::Expression;
// Узел `Condition` вынесен в `parser::ast_cond` - тем же приёмом и по той же причине:
// предел размера модуля. Путь сохранён реэкспортом.
pub use crate::parser::ast_cond::Condition;

/// Идентификатор с местоположением в исходном тексте.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct Identifier {
    /// Местоположение идентификатора.
    pub loc: Location,
    /// Строковое имя идентификатора.
    pub name: String,
}

impl Identifier {
    /// Создаёт идентификатор с местоположением по умолчанию.
    pub fn new(s: impl Into<String>) -> Self {
        Self {
            loc: Location::default(),
            name: s.into(),
        }
    }
}

/// Путь из идентификаторов, разделённых `::` (например, `a::b::c`).
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct IdentifierPath {
    /// Местоположение всего пути.
    pub loc: Location,
    /// Последовательность идентификаторов.
    pub identifiers: Vec<Identifier>,
}

/// Комментарий в исходном тексте.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum Comment {
    /// Обычный строчный комментарий `// ...`.
    Line(Location, String),
    /// Документационный строчный комментарий `/// ...`.
    DocLine(Location, String),
    /// Блочный комментарий `/* ... */`.
    Block(Location, String),
}

impl Comment {
    /// Возвращает текстовое содержимое комментария.
    #[inline]
    pub fn value(&self) -> &str {
        match self {
            Self::Line(_, s) | Self::DocLine(_, s) | Self::Block(_, s) => s,
        }
    }

    /// Возвращает `true`, если комментарий является документационным (`///`).
    #[inline]
    pub const fn is_doc(&self) -> bool {
        matches!(self, Self::DocLine(..))
    }

    /// Возвращает `true`, если комментарий является строчным (оба варианта).
    #[inline]
    pub const fn is_line(&self) -> bool {
        matches!(self, Self::Line(..) | Self::DocLine(..))
    }

    /// Возвращает `true`, если комментарий является блочным (`/* */`).
    #[inline]
    pub const fn is_block(&self) -> bool {
        matches!(self, Self::Block(..))
    }
}

/// Описание импорта.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum ImportDefine {
    /// `import "путь";`
    Plain(ImportPath, Location),
    /// `import "путь" as Имя;` или `import * as Имя from "путь";`
    GlobalSymbol(ImportPath, Identifier, Location),
    /// `import { a, b as c } from "путь";`
    Rename(ImportPath, Vec<(Identifier, Option<Identifier>)>, Location),
}

/// Путь импорта.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum ImportPath {
    /// Путь в виде строкового литерала.
    Filename(StringLiteral),
    /// Путь в виде идентификаторного пути.
    Path(IdentifierPath),
}

impl ImportDefine {
    /// Возвращает строковый литерал пути, если он есть.
    #[inline]
    pub const fn literal(&self) -> Option<&StringLiteral> {
        match self {
            Self::Plain(ImportPath::Filename(literal), _)
            | Self::GlobalSymbol(ImportPath::Filename(literal), _, _)
            | Self::Rename(ImportPath::Filename(literal), _, _) => Some(literal),
            _ => None,
        }
    }
}

/// Список параметров функции: `Vec<(позиция, параметр?)>`.
///
/// `None` используется при ошибке восстановления парсера.
pub type ParameterList = Vec<(Location, Option<Parameter>)>;

/// Тип данных в языке Takt.
#[derive(Default, Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum Type {
    /// Адресный тип `порт[:бит]`.
    Address {
        /// Адрес порта.
        address: u64,
        /// Опциональный номер бита.
        bit: Option<u64>,
    },
    /// Примитивный тип `bit` (1 бит).
    Bit,
    /// Булевый тип `bool`.
    Bool,
    /// Тип с плавающей точкой `float`.
    Rational,
    /// Длительность `duration`: значение - целое число наносекунд.
    ///
    /// Отдельный тип, а не целое: единица обязана быть частью типа, иначе теряется в
    /// первом же присваивании (тот же довод, что у `q(m, n)`).
    Duration,
    /// Fixed-point `q(m, n)`: `(loc, ctor, m, n, модификатор)`; границы, имя `q` и
    /// смысл полей (`m`/`n` - сырые литералы) проверяет `construct_fixed`.
    ///
    /// Последнее поле - **необязательный постфиксный модификатор**:
    /// `q(8, 8) sat` даёт `Some("sat")` - насыщение вместо переноса. Слово здесь
    /// **сырое**: как и конструктор `q`, оно остаётся обычным идентификатором, а
    /// допустимость проверяет семантика (`SE-104`). Иначе `sat` перестало бы
    /// годиться как имя переменной.
    Fixed(Location, String, i128, i128, Option<String>),
    /// Псевдоним типа (ссылка по имени).
    Alias(Identifier),
    /// Массив битов: `[тип; N]`.
    Array {
        /// Местоположение в исходном тексте.
        loc: Location,
        /// Количество элементов.
        element_count: u16,
        /// Тип элемента.
        element_type: Box<Type>,
    },
    /// Перечисление типов.
    Enum(String),
    /// Структурный тип (ссылка по имени).
    Struct(String),
    /// Функциональный тип `(параметры) -> возврат`.
    Function {
        /// Список входных параметров.
        params: ParameterList,
        /// Список возвращаемых значений.
        returns: Option<ParameterList>,
    },
    /// Пустой тип (отсутствие значения).
    #[default]
    Unit,
}

/// Направление порта: входной (`in`) или выходной (`out`).
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum PortDirection {
    /// Входной порт - FSM только читает (значение поступает извне).
    In,
    /// Выходной порт - FSM только пишет (значение уходит наружу).
    Out,
    /// Двунаправленный порт - FSM может и читать, и писать.
    InOut,
}

/// Описание (`var` или `const` или `in`/`out`-порта).
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum VariableDefine {
    /// Переменная (изменяемое значение)
    Variable {
        /// Местоположение в исходном тексте.
        loc: Location,
        /// Тип переменной
        typ: Option<Type>,
        /// Имя переменной
        name: Option<Identifier>,
        /// Инициализатор переменной
        initializer: Option<Expression>,
    },
    /// Порт ввода-вывода, объявляется через `in` (входной) или `out` (выходной).
    Port {
        /// Местоположение в исходном тексте.
        loc: Location,
        /// Тип порта
        typ: Option<Type>,
        /// Имя порта
        name: Option<Identifier>,
        /// Размещение порта `at <адрес>` - **необязательное**: адрес может прийти по
        /// имени (оператор `address`, внешняя карта), и полноту проверяет слой адресов,
        /// а не разбор.
        address: Option<Expression>,
        /// Инициализатор порта: форма `:= 0x100` задаёт адрес, а не значение.
        initializer: Option<Expression>,
        /// Направление порта
        direction: PortDirection,
    },
    /// Константа (неизменяемое значение)
    Constant {
        /// Местоположение в исходном тексте.
        loc: Location,
        /// Тип константы
        typ: Option<Type>,
        /// Имя константы
        name: Option<Identifier>,
        /// Инициализатор константы
        initializer: Expression,
    },
    /// Параметр модели (`parameter Y: u8 := 100;`).
    ///
    /// Третья форма объявления наряду с `var` и `const`: значение задаётся в месте
    /// инстанцирования модели (`M(Y := 200)`), а инициализатор объявления - **значение
    /// по умолчанию**, поэтому он обязателен (в отличие от `var`). Форма разбирается
    /// только на уровне модели: параметр внутри блока оператора инстанцировать нечем.
    Parameter {
        /// Местоположение в исходном тексте.
        loc: Location,
        /// Тип параметра
        typ: Option<Type>,
        /// Имя параметра
        name: Option<Identifier>,
        /// Значение по умолчанию
        initializer: Expression,
    },
}

/// Корневой узел АСД - модель (автомат или компоновка автоматов).
///
/// При разборе всего файла без явного `model` блока корень является анонимной моделью
/// (`name == None`), содержащей все элементы верхнего уровня.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct Model {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Имя модели (`None` для анонимной корневой модели).
    pub name: Option<Identifier>,
    /// Элементы модели.
    pub elements: Vec<ModelElement>,
    /// Выражение реализации/компоновки (`= выражение`).
    pub implements: Option<Expression>,
}

/// Элемент модели верхнего уровня.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum ModelElement {
    /// Директива импорта.
    Import(ImportDefine),
    /// Определение функции.
    Function(Box<FunctionDefine>),
    /// Формула.
    Formula(Box<FormulaDefine>),
    /// Вставка в вывод цели на уровне модели (`assembly ["цель"] { ... }`).
    ///
    /// Места блоков `formula` и `assembly` выровнены решением 2026-09-03: обе
    /// конструкции стоят везде, где стоят прочие элементы. Вставка уровня модели
    /// исполняется каждый такт - как блок `always` уровня модели, в который её и
    /// разворачивает семантика.
    Assembly(Box<Statement>),
    /// Определение условия.
    Condition(Box<ConditionDefine>),
    /// Именованный инвариант (`invariant Имя = <Условие>;`).
    Invariant(Box<InvariantDefine>),
    /// Определение переменной.
    Variable(Box<VariableDefine>),
    /// Определение псевдонима типа.
    Type(Box<TypeDefine>),
    /// Определение состояния.
    State(Box<StateDefine>),
    /// Вложенная модель.
    Model(Box<Model>),
    /// Именованный блок кода (`enter`, `exit`, `always`, ...).
    NamedBlockCode(Box<NamedBlockCodeDefine>),
    /// Одиночная точка с запятой (допускается для совместимости).
    StraySemicolon(Location),
    /// Перечисление.
    Enum(Box<EnumDefine>),
    /// Структурный тип.
    Struct(Box<StructDefine>),
    /// Встроенная формула в теле модели.
    InlineFormula(Box<InlineFormulaDefine>),
    /// Задание адреса порта отдельным оператором (`address NAME = <expr>;`).
    ///
    /// Разрешение адреса (привязка к порту, приоритет источников, `AddressMap`)
    /// выполняется семантикой; здесь - только синтаксический узел.
    Address(Box<AddressDefine>),
    /// Частота тактирования модели (`clock 1kHz;`).
    Clock(Box<ClockDefine>),
}

/// Объявление частоты тактирования модели: `clock 1kHz;`.
///
/// Включает **профиль "такты"**: длительности пересчитываются в число тактов. Без
/// объявления и без флага `--tick-hz` действует профиль "часы" - частота не нужна
/// вовсе.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct ClockDefine {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Частота в герцах (каноническое представление литерала).
    pub hertz: u64,
    /// Литерал частоты как записан (`1kHz`) - для форматтера.
    pub text: String,
}

/// Вид состояния автомата.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum StateKind {
    /// Начальное состояние (`start`).
    Start,
    /// Конечное состояние.
    End,
    /// Переходное состояние.
    Next,
}

/// Определение состояния (`state` или `start`).
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct StateDefine {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Имя состояния.
    pub name: Option<Identifier>,
    /// Элементы состояния.
    pub elements: Vec<StateElement>,
    /// Выражение реализации.
    pub implements: Option<Expression>,
    /// Вид состояния.
    pub kind: Option<StateKind>,
}

/// Элемент состояния.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum StateElement {
    /// Переход к следующему состоянию (`next Имя`).
    Next(Identifier),
    /// Ссылка на состояние с условием перехода: `ref Имя [: Условие]`.
    Reference(Location, Identifier, Option<Condition>),
    /// Именованный блок кода.
    NamedBlockCode(Box<NamedBlockCodeDefine>),
    /// Одиночная точка с запятой.
    StraySemicolon(Location),
    /// Встроенная формула в теле состояния.
    InlineFormula(Box<InlineFormulaDefine>),
    /// Именованный инвариант состояния (`invariant Имя = <Условие>;`).
    Invariant(Box<InvariantDefine>),
    /// Периодическое действие (`every 100ms { ... }`).
    Every(Box<EveryDefine>),
    /// Формула-обязательство уровня состояния (`formula { ... }`).
    ///
    /// Компилятор её не переводит и не проверяет - как и формулу уровня модели; адресат -
    /// внешний анализатор.
    Formula(Box<FormulaDefine>),
    /// Вставка в вывод цели на уровне состояния (`assembly ["цель"] { ... }`).
    ///
    /// Исполняется в теле состояния - как блок `always` этого состояния, в который её и
    /// разворачивает семантика.
    Assembly(Box<Statement>),
}

/// Периодическое действие состояния: `every 100ms { ... }`.
///
/// Сахар над механизмом времени: своей семантики не вводит, скрытое состояние
/// разворачивает семантика.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct EveryDefine {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Период в наносекундах (каноническое представление литерала).
    pub nanos: i64,
    /// Литерал периода как записан (`100ms`) - для форматтера.
    pub text: String,
    /// Тело, исполняемое с этим периодом.
    pub body: Statement,
}

/// База наследования: `Имя[(аргументы,*)]`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct Base {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Путь идентификатора.
    pub name: IdentifierPath,
    /// Аргументы (опционально).
    pub args: Option<Vec<Expression>>,
}

/// Переменная с типом и инициализатором.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct Variable {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Тип переменной.
    pub ty: Box<TypeDefine>,
    /// Имя переменной.
    pub name: Option<Identifier>,
    /// Начальное значение.
    pub initializer: Option<Expression>,
}

/// Вариант перечисления.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct EnumVariant {
    /// Местоположение в исходном коде.
    pub loc: Location,
    /// Имя варианта.
    pub name: Identifier,
    /// Явное числовое значение, если задано.
    pub value: Option<i128>,
}

impl Eq for EnumVariant {}

/// Определение перечисления `enum Name { Variant1, Variant2 = 5, ... }`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct EnumDefine {
    /// Местоположение в исходном коде.
    pub loc: Location,
    /// Имя перечисления.
    pub name: Option<Identifier>,
    /// Варианты перечисления.
    pub variants: Vec<EnumVariant>,
}

impl Eq for EnumDefine {}

/// Определение структурного типа: `struct Имя { поле: Тип, ... }`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct StructDefine {
    /// Местоположение в исходном коде.
    pub loc: Location,
    /// Имя структуры.
    pub name: Option<Identifier>,
    /// Поля структуры.
    pub fields: Vec<StructField>,
}

impl Eq for StructDefine {}

/// Поле структуры: `имя: Тип`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct StructField {
    /// Местоположение поля в исходном коде.
    pub loc: Location,
    /// Имя поля.
    pub name: Identifier,
    /// Тип поля.
    pub ty: Type,
}

/// Определение псевдонима типа: `type Имя = Тип`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct TypeDefine {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Имя псевдонима.
    pub name: Identifier,
    /// Исходный тип.
    pub ty: Type,
}

/// Строковый литерал с признаком Unicode.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct StringLiteral {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// `true` - строка объявлена с префиксом `unicode`.
    pub unicode: bool,
    /// Содержимое строки (без кавычек).
    pub string: String,
}

/// Именованный аргумент вызова функции: `имя: выражение`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct NamedArgument {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Имя аргумента (может отсутствовать при ошибке).
    pub name: Option<Identifier>,
    /// Значение аргумента.
    pub expr: Expression,
}

/// Элемент доступа к члену (для оператора `.`).
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum Member {
    /// Доступ по имени: `.имя`.
    Identifier(Identifier),
    /// Доступ по индексу: `.0`, `.1`, ...
    Number(i128),
}

/// Параметр функции: опциональное имя и тип.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct Parameter {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Тип параметра (выражение).
    pub ty: Expression,
    /// Имя параметра (может отсутствовать).
    pub name: Option<Identifier>,
}

/// Встроенная формула: `: условие1[, условие2, ...];`
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum InlineFormulaDefine {
    /// По умолчанию: Guard формула.
    Guard {
        /// Местоположение в исходном тексте.
        loc: Location,
        /// Список условий.
        conditions: Vec<Condition>,
        /// Записана ли явная форма `: [Guard] условия;`.
        ///
        /// `: условия;` и `: [Guard] условия;` - синонимы с одной семантикой. Признак
        /// хранится, чтобы форматтер печатал форму автора; семантика его игнорирует.
        explicit: bool,
    },
    /// LTL формула.
    Ltl {
        /// Местоположение в исходном тексте.
        loc: Location,
        /// Список LTL формул.
        formulas: Vec<LtlExpr>,
    },
}

/// Выражение LTL формулы.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum LtlExpr {
    /// Логическая истина.
    True(Location),
    /// Логическая ложь.
    False(Location),
    /// Атомарное предложение (идентификатор).
    Atom(Identifier),
    /// Логическое не.
    Not(Location, Box<LtlExpr>),
    /// Оператор "в следующем состоянии" (Next, X).
    Next(Location, Box<LtlExpr>),
    /// Оператор "в будущем" (Finally, F).
    Finally(Location, Box<LtlExpr>),
    /// Оператор "всегда" (Globally, G).
    Globally(Location, Box<LtlExpr>),
    /// Логическое И.
    And(Location, Box<LtlExpr>, Box<LtlExpr>),
    /// Логическое или.
    Or(Location, Box<LtlExpr>, Box<LtlExpr>),
    /// Оператор "пока" (Until, U).
    Until(Location, Box<LtlExpr>, Box<LtlExpr>),
    /// Оператор "освобождение" (Release, R).
    Release(Location, Box<LtlExpr>, Box<LtlExpr>),
    /// Логическая импликация (->).
    Implies(Location, Box<LtlExpr>, Box<LtlExpr>),
    /// Выражение в скобках.
    Parenthesis(Location, Box<LtlExpr>),
}

/// Определение формулы (`formula`).
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct FormulaDefine {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Диалект внешнего анализатора: `formula "smt" { ... }`.
    ///
    /// Хранится ради печати: грамматика диалект разбирала и
    /// **выбрасывала**, поэтому форматтер потерял бы его молча - а тихая
    /// потеря исходника недопустима.
    pub dialect: Option<StringLiteral>,
    /// Блок тела формулы.
    pub formula: FormulaBlock,
}

/// Определение условия перехода (`cond Имя = Условие`).
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct ConditionDefine {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Имя условия.
    pub name: Option<Identifier>,
    /// Выражение условия.
    pub value: Condition,
}

/// Именованный инвариант (`invariant Имя = <Условие>;`).
///
/// Семантически - сахар над парой `cond Имя = C;` + `: [Guard] Имя;` (десахаризация в
/// семантическом проходе, Option C). АСД хранит форму автора: форматтер печатает
/// `invariant`, а не разворачивает пару. Правая часть - [`Condition`], **не**
/// `Expression`: `=` внутри есть равенство, связка имени - `=`, не `:=`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct InvariantDefine {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Имя инварианта.
    pub name: Option<Identifier>,
    /// Условие-обязательство.
    pub value: Condition,
}

/// Задание адреса порта отдельным оператором (`address Имя = <выражение>;`).
///
/// Аддитивно к инлайн-форме `in Имя: T := <адрес>;`. Само по себе только несёт
/// синтаксис; привязку к порту и разрешение приоритета источников выполняет семантика.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct AddressDefine {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Имя порта, которому назначается адрес.
    pub name: Option<Identifier>,
    /// Выражение-адрес (обычно литерал `0xADDR` или `0xADDR:bit`).
    pub value: Expression,
}

/// Именованный блок кода (`enter`, `exit`, `always`, ...).
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct NamedBlockCodeDefine {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Имя блока.
    pub name: Option<Identifier>,
    /// Тело блока (оператор).
    pub statement: Statement,
}

/// Определение функции (`fn Имя(параметры) [-> Тип] [{ тело }]`).
#[derive(Default, Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct FunctionDefine {
    /// Местоположение всего определения.
    pub loc: Location,
    /// Имя функции.
    pub name: Option<Identifier>,
    /// Местоположение имени.
    pub name_loc: Location,
    /// Список параметров.
    pub params: ParameterList,
    /// Возвращаемый тип (если есть).
    pub return_type: Option<Type>,
    /// Тело функции (если есть).
    pub body: Option<Statement>,
    /// Является ли функция внешней.
    pub external: bool,
    /// Атрибут объявления: `[inline]` либо `[noinline]`.
    ///
    /// Хранится **именем**, а не разобранным признаком: набор атрибутов знает
    /// семантика, а не грамматика, и неизвестное имя обязано получить диагностику с
    /// позицией, а не отказ разбора.
    pub attribute: Option<Identifier>,
}

impl FunctionDefine {
    /// Возвращает `true`, если функция не возвращает значения.
    #[inline]
    pub fn is_void(&self) -> bool {
        self.return_type.is_none()
    }

    /// Возвращает `true`, если тело функции отсутствует или пусто.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.body.as_ref().is_none_or(Statement::is_empty)
    }
}

/// Ключевое слово цикла, написанное автором.
///
/// `while условие { тело }` - **синоним** `loop условие { тело }` (одна семантика, один
/// АСД-узел). Какое слово написал автор, после разбора не восстановить, поэтому
/// хранится здесь - нужно форматтеру: без признака `while` молча переписывался бы в
/// `loop`. Семантика и генераторы признак игнорируют.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum LoopKeyword {
    /// Написано `loop`.
    #[default]
    Loop,
    /// Написано `while` (всегда с условием).
    While,
}

/// Оператор языка Takt.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
#[allow(clippy::large_enum_variant, clippy::type_complexity)]
#[non_exhaustive]
pub enum Statement {
    /// Блок операторов: `{ операторы* }`.
    Block {
        /// Местоположение блока.
        loc: Location,
        /// Признак проверяемого блока (зарезервировано).
        unchecked: bool,
        /// Список операторов.
        statements: Vec<Statement>,
    },
    /// Блок ассемблерного кода: `assembly [диалект] { ... }`.
    Assembly {
        /// Местоположение.
        loc: Location,
        /// Диалект ассемблера.
        dialect: Option<StringLiteral>,
        /// Флаги.
        flags: Option<Vec<StringLiteral>>,
        /// Блок тела.
        block: Box<Statement>,
    },
    /// Блок формулы: `formula [диалект] { ... }`.
    Formula {
        /// Местоположение.
        loc: Location,
        /// Диалект.
        dialect: Option<StringLiteral>,
        /// Флаги.
        flags: Option<Vec<StringLiteral>>,
        /// Блок тела.
        block: Box<FormulaBlock>,
    },
    /// Именованные аргументы: `{ ключ: значение, ... }`.
    Args(Location, Vec<NamedArgument>),
    /// Оператор `if`: `if условие блок [else ветка]`.
    If(Location, Expression, Box<Statement>, Option<Box<Statement>>),
    /// Цикл `loop [условие] { тело }` (или `while условие { тело }` - синоним).
    ///
    /// Без условия - бесконечный цикл; с условием - продолжается, пока условие истинно.
    /// Последнее поле - [`LoopKeyword`]: какое слово написал автор (нужно форматтеру,
    /// семантикой игнорируется).
    Loop(Location, Option<Expression>, Box<Statement>, LoopKeyword),
    /// Оператор-выражение.
    Expression(Location, Expression),
    /// Объявление переменной с опциональным инициализатором.
    Variable(Location, Box<VariableDefine>, Option<Expression>),
    /// Оператор `for`: `for (инит; условие; шаг) тело`.
    For(
        Location,
        Option<Box<Statement>>,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
        Option<Box<Statement>>,
    ),
    /// Оператор `continue`.
    Continue(Location),
    /// Оператор `break`.
    Break(Location),
    /// Оператор `return [выражение]`.
    Return(Location, Option<Expression>),
    /// Ошибка (вставляется при восстановлении после сбоя парсера).
    Error(Location),
    /// Одиночная точка с запятой.
    StraySemicolon(Location),
    /// Встроенная формула в блоке кода: `: [Type] условие1[, условие2, ...];`
    InlineFormula(Box<InlineFormulaDefine>),
    /// Оператор `match`: `match выражение { образец => тело, ... }`.
    Match(Location, Box<Expression>, Vec<MatchArm>),
}

/// Ветка оператора `match`: `образцы => тело`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct MatchArm {
    /// Местоположение ветки.
    pub loc: Location,
    /// Образцы (хотя бы один).
    pub patterns: Vec<MatchPattern>,
    /// Тело ветки.
    pub body: Box<Statement>,
}

/// Образец в ветке `match`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum MatchPattern {
    /// Конкретное значение: `42`, `Color::Red`, переменная.
    Value(Expression),
    /// Подстановочный образец `_` (совпадает с любым значением).
    Wildcard(Location),
}

impl Statement {
    /// Возвращает `true`, если блочный оператор не содержит элементов.
    #[inline]
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Block { statements, .. } => statements.is_empty(),
            Self::Assembly { block, .. } => block.is_empty(),
            Self::Args(_, args) => args.is_empty(),
            _ => false,
        }
    }

    /// Позиция оператора в исходном тексте.
    ///
    /// Живёт **у самого узла**, а не у потребителя: копий этой функции было **две** - в
    /// счётчике глубины разбора и в константном вычислителе, - и третий потребитель
    /// (отказ форматтера) сделал бы три. Расхождение здесь тихое: диагностика указала
    /// бы не на тот оператор.
    ///
    /// Разбор **исчерпывающий**: новый вариант оператора валит сборку здесь, а не молча
    /// теряет позицию. `#[non_exhaustive]` этому не мешает - он ограничивает внешние
    /// крейты, а не свой.
    pub fn loc(&self) -> Location {
        match self {
            Self::Block { loc, .. }
            | Self::Assembly { loc, .. }
            | Self::Formula { loc, .. }
            | Self::Args(loc, _)
            | Self::If(loc, _, _, _)
            | Self::Loop(loc, _, _, _)
            | Self::Expression(loc, _)
            | Self::Variable(loc, _, _)
            | Self::For(loc, _, _, _, _)
            | Self::Continue(loc)
            | Self::Break(loc)
            | Self::Return(loc, _)
            | Self::Error(loc)
            | Self::StraySemicolon(loc)
            | Self::Match(loc, _, _) => *loc,
            Self::InlineFormula(formula) => match formula.as_ref() {
                InlineFormulaDefine::Guard { loc, .. } | InlineFormulaDefine::Ltl { loc, .. } => {
                    *loc
                }
            },
        }
    }

    /// Название вида оператора - для диагностик, адресованных человеку.
    ///
    /// Не `Debug`-дамп: до отказ форматтера печатал `Statement::Assembly { loc:
    /// Source(0, 53, 71), dialect: ... }` - внутреннее представление вместо текста.
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::Block { .. } => "блок",
            Self::Assembly { .. } => "assembly",
            Self::Formula { .. } => "formula",
            Self::Args(_, _) => "именованные аргументы",
            Self::If(_, _, _, _) => "if",
            Self::Loop(_, _, _, _) => "цикл",
            Self::Expression(_, _) => "выражение-оператор",
            Self::Variable(_, _, _) => "объявление переменной",
            Self::For(_, _, _, _, _) => "for",
            Self::Continue(_) => "continue",
            Self::Break(_) => "break",
            Self::Return(_, _) => "return",
            Self::Error(_) => "ошибочный оператор",
            Self::StraySemicolon(_) => "лишняя ';'",
            Self::Match(_, _, _) => "match",
            Self::InlineFormula(_) => "формула-утверждение",
        }
    }
}

/// Оператор в блоке формулы.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum FormulaStatement {
    /// Выражение формулы.
    Expression(Location, FormulaExpression),
    /// Вложенный блок формулы.
    Block(FormulaBlock),
    /// Вызов функции формулы.
    Function(Box<FormulaFunction>),
    /// Ошибка при разборе.
    Error(Location),
}

/// Блок формулы: `{ операторы* }`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct FormulaBlock {
    /// Местоположение блока.
    pub loc: Location,
    /// Список операторов формулы.
    pub statements: Vec<FormulaStatement>,
}

impl FormulaBlock {
    /// Возвращает `true`, если блок не содержит операторов.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }
}

/// Выражение в формуле.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum FormulaExpression {
    /// Булевый литерал с опциональной аннотацией типа: `true[:тип]`.
    Bool(Location, bool, Option<Identifier>),
    /// Числовой литерал с опциональной аннотацией: `42[:тип]`.
    Number(Location, i128, Option<Identifier>),
    /// Строковый литерал с опциональной аннотацией: `"s"[:тип]`.
    String(StringLiteral, Option<Identifier>),
    /// Ссылка на переменную.
    Variable(Identifier),
    /// Вызов функции.
    Function(Box<FormulaFunction>),
    /// Доступ к члену: `выражение.имя`.
    SuffixAccess(Location, Box<FormulaExpression>, Identifier),
    /// Скобки.
    Parenthesis(Location, Box<FormulaExpression>),
}

/// Вызов функции в формуле: `id(аргументы,*)`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub struct FormulaFunction {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Имя функции.
    pub id: Identifier,
    /// Аргументы вызова.
    pub arguments: Vec<FormulaExpression>,
}

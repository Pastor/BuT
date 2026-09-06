//! Узел АСД "выражение" ([`Expression`]) языка Takt.
//!
//! Вынесено из `ast.rs`: чистое перемещение. Путь `parser::ast::Expression` сохранён
//! реэкспортом в `ast.rs` - от него зависят lalrpop-грамматика, семантика и генераторы.

use crate::diagnostics::Location;
use crate::parser::ast::{
    Identifier, Member, NamedArgument, ParameterList, Statement, StringLiteral, Type,
};
#[cfg(feature = "ast-serde")]
use serde::{Deserialize, Serialize};

/// Выражение языка Takt.
///
/// Поддерживает полный спектр операций: арифметику, побитовые операции, сравнения,
/// логику, обращение к массивам, вызовы функций и т.д.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum Expression {
    /// Доступ к элементу массива: `id[индекс]`.
    ArraySubscript(Location, Box<Expression>, Box<Expression>),
    /// Срез массива: `id[начало:конец]`.
    ArraySlice(Location, Box<Expression>, Option<i128>, Option<i128>),
    /// Скобки: `(выражение)`.
    Parenthesis(Location, Box<Expression>),
    /// Доступ к биту: `выражение.член`.
    BitAccess(Location, Box<Expression>, Member),
    /// Вызов функции: `id(аргументы,*)`.
    Function(Location, Identifier, Vec<Expression>),
    /// Блок кода как выражение: `выражение { ... }`.
    CodeBlock(Location, Box<Expression>, Box<Statement>),
    /// Вызов с именованными аргументами: `выражение({ ключ: значение, ... })`.
    NamedFunction(Location, Box<Expression>, Vec<NamedArgument>),
    /// Логическое не: `!выражение`.
    Not(Location, Box<Expression>),
    /// Побитовое не: `~выражение`.
    BitwiseNot(Location, Box<Expression>),
    /// Унарный плюс: `+выражение`.
    UnaryPlus(Location, Box<Expression>),
    /// Унарный минус: `-выражение`.
    Negate(Location, Box<Expression>),

    /// Возведение в степень: `левое ** правое`.
    Power(Location, Box<Expression>, Box<Expression>),
    /// Умножение: `левое * правое`.
    Multiply(Location, Box<Expression>, Box<Expression>),
    /// Деление: `левое / правое`.
    Divide(Location, Box<Expression>, Box<Expression>),
    /// Остаток от деления: `левое % правое`.
    Modulo(Location, Box<Expression>, Box<Expression>),
    /// Сложение: `левое + правое`.
    Add(Location, Box<Expression>, Box<Expression>),
    /// Вычитание: `левое - правое`.
    Subtract(Location, Box<Expression>, Box<Expression>),
    /// Сдвиг влево: `левое << правое`.
    ShiftLeft(Location, Box<Expression>, Box<Expression>),
    /// Сдвиг вправо: `левое >> правое`.
    ShiftRight(Location, Box<Expression>, Box<Expression>),
    /// Побитовое И: `левое & правое`.
    BitwiseAnd(Location, Box<Expression>, Box<Expression>),
    /// Побитовое исключающее или: `левое ^ правое`.
    BitwiseXor(Location, Box<Expression>, Box<Expression>),
    /// Побитовое или: `левое | правое`.
    BitwiseOr(Location, Box<Expression>, Box<Expression>),
    /// Меньше: `левое < правое`.
    Less(Location, Box<Expression>, Box<Expression>),
    /// Больше: `левое > правое`.
    More(Location, Box<Expression>, Box<Expression>),
    /// Меньше или равно: `левое <= правое`.
    LessEqual(Location, Box<Expression>, Box<Expression>),
    /// Больше или равно: `левое >= правое`.
    MoreEqual(Location, Box<Expression>, Box<Expression>),
    /// Равенство: `левое == правое`.
    Equal(Location, Box<Expression>, Box<Expression>),
    /// Неравенство: `левое != правое`.
    NotEqual(Location, Box<Expression>, Box<Expression>),
    /// Логическое И: `левое && правое`.
    And(Location, Box<Expression>, Box<Expression>),
    /// Логическое или: `левое || правое`.
    Or(Location, Box<Expression>, Box<Expression>),
    /// Тернарный оператор: `условие ? тогда : иначе`.
    ConditionalOperator(Location, Box<Expression>, Box<Expression>, Box<Expression>),
    /// Присваивание: `левое = правое`.
    Assign(Location, Box<Expression>, Box<Expression>),
    /// Целочисленный литерал.
    Number(Location, i128),
    /// Литерал длительности: `(позиция, наносекунды, как записано)` -.
    Duration(Location, i64, String),
    /// Вещественный литерал: `(строка, отрицательный)`.
    Rational(Location, String, bool),
    /// Конкатенация строковых литералов.
    String(Vec<StringLiteral>),
    /// Тип как выражение.
    Type(Location, Type),
    /// Адресный литерал: `адрес:бит`.
    Address(Location, i64, i64),
    /// Анонимное обращение к ячейке по адресу: `#адрес` или `#адрес:бит`.
    ///
    /// Хранит **выбор автора**, а не канонический вид: `None` в позиции бита означает,
    /// что бит не записан (`#0x100`), `Some(0)` - записан явно (`#0x100:0`). Формы
    /// означают одно и то же, но форматтер обязан печатать исходную (синонимы не
    /// канонизируются - то же правило, что у `while`/`loop`).
    ///
    /// Адрес хранится в `i128`, как и всякий числовой литерал: сужение до `i64` -
    /// задача семантики, и она делает его **явным отказом**, а не `as i64`.
    ///
    /// Ширина доступа в узле **не хранится**: её задаёт приведение (`as u32`) либо
    /// битовый доступ (`.4`), стоящие над обращением. Свёртку тройки `{адрес, бит,
    /// ширина}` делает семантика в единой воронке.
    AnonAddress(Location, i128, Option<i64>),
    /// Булевый литерал.
    Bool(Location, bool),
    /// Ссылка на переменную.
    Variable(Identifier),
    /// Список параметров: `(параметр,*)`.
    List(Location, ParameterList),
    /// Массивный литерал: `[элемент,*]`.
    Array(Location, Vec<Expression>),
    /// Инициализатор структуры: `{ элемент,* }`.
    Initializer(Location, Vec<Expression>),
    /// Приведение типа: `выражение as Тип`.
    Cast(Location, Box<Expression>, Type),
}

/// Вспомогательный макрос для получения компонент выражения.
///
/// Используется в методах [`Expression::components`] и [`Expression::components_mut`].
macro_rules! expr_components {
    ($s:ident) => {
        match $s {
            // Унарные: (None, Some)
            Not(_, expr)
            | BitwiseNot(_, expr)
            | UnaryPlus(_, expr)
            | Negate(_, expr)
            | Parenthesis(_, expr) => (None, Some(expr)),

            // Бинарные: (Some, Some)
            Power(_, left, right)
            | Multiply(_, left, right)
            | Divide(_, left, right)
            | Modulo(_, left, right)
            | Add(_, left, right)
            | Subtract(_, left, right)
            | ShiftLeft(_, left, right)
            | ShiftRight(_, left, right)
            | BitwiseAnd(_, left, right)
            | BitwiseXor(_, left, right)
            | BitwiseOr(_, left, right)
            | Less(_, left, right)
            | More(_, left, right)
            | LessEqual(_, left, right)
            | MoreEqual(_, left, right)
            | Equal(_, left, right)
            | NotEqual(_, left, right)
            | And(_, left, right)
            | Or(_, left, right)
            | Assign(_, left, right) => (Some(left), Some(right)),

            // Листовые: (None, None)
            BitAccess(..)
            | ConditionalOperator(..)
            | ArraySubscript(..)
            | ArraySlice(..)
            | Function(..)
            | CodeBlock(..)
            | NamedFunction(..)
            | Number(..)
            | Rational(..)
            | Duration(..)
            | String(..)
            | Type(..)
            | Bool(..)
            | Address(..)
            | AnonAddress(..)
            | Variable(..)
            | List(..)
            | Cast(..)
            | Initializer(..)
            | Array(..) => (None, None),
        }
    };
}

impl Expression {
    /// Убирает один уровень скобок.
    ///
    /// Если `self` является [`Parenthesis`](Expression::Parenthesis), возвращает
    /// внутреннее выражение; иначе возвращает `self`.
    #[inline]
    pub fn remove_parenthesis(&self) -> &Expression {
        if let Expression::Parenthesis(_, expr) = self {
            expr
        } else {
            self
        }
    }

    /// Рекурсивно убирает все уровни скобок.
    pub fn strip_parentheses(&self) -> &Expression {
        match self {
            Expression::Parenthesis(_, expr) => expr.strip_parentheses(),
            _ => self,
        }
    }

    /// Возвращает разделяемые ссылки на компоненты выражения.
    ///
    /// Возвращает пару `(левая_часть, правая_часть)`:
    /// - для унарных операторов - `(None, Some(операнд))`,
    /// - для бинарных - `(Some(левый), Some(правый))`,
    /// - для литералов и вызовов - `(None, None)`.
    ///
    /// # Примеры
    ///
    /// ```
    /// use takt_lang::parser::ast::{Expression, Identifier };
    /// use takt_lang::diagnostics::Location;
    ///
    /// // Унарный: ~a
    /// let var = Expression::Variable(Identifier::new("a"));
    /// let bitwise_not = Expression::BitwiseNot(Location::default(), Box::new(var.clone()));
    /// assert_eq!(bitwise_not.components(), (None, Some(&var)));
    ///
    /// // Бинарный: a + b
    /// let var_a = Expression::Variable(Identifier::new("a"));
    /// let var_b = Expression::Variable(Identifier::new("b"));
    /// let add = Expression::Add(
    ///     Location::default(),
    ///     Box::new(var_a.clone()),
    ///     Box::new(var_b.clone()),
    /// );
    /// assert_eq!(add.components(), (Some(&var_a), Some(&var_b)));
    ///
    /// // Литерал: 42
    /// let num = Expression::Number(Location::default(), 42);
    /// assert_eq!(num.components(), (None, None));
    /// ```
    #[inline]
    pub fn components(&self) -> (Option<&Self>, Option<&Self>) {
        use Expression::*;
        expr_components!(self)
    }

    /// Возвращает изменяемые ссылки на компоненты выражения.
    ///
    /// также [`Expression::components`].
    #[inline]
    pub fn components_mut(&mut self) -> (Option<&mut Self>, Option<&mut Self>) {
        use Expression::*;
        expr_components!(self)
    }

    /// Возвращает `true`, если выражение нельзя разбить на несколько строк.
    #[inline]
    pub const fn is_unsplittable(&self) -> bool {
        use Expression::*;
        matches!(
            self,
            Number(..) | Rational(..) | String(..) | Address(..) | Variable(..)
        )
    }

    /// Возвращает `true`, если вокруг оператора нужны пробелы.
    #[inline]
    pub const fn has_space_around(&self) -> bool {
        use Expression::*;
        !matches!(self, Not(..) | BitwiseNot(..) | UnaryPlus(..) | Negate(..))
    }

    /// Возвращает `true`, если выражение является литералом.
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            Expression::Address(..)
                | Expression::Number(..)
                | Expression::Array(..)
                | Expression::Rational(..)
                | Expression::String(..)
        )
    }

    /// Возвращает местоположение выражения в исходном тексте.
    pub fn loc(&self) -> Location {
        match self {
            Expression::ArraySubscript(loc, _, _) => *loc,
            Expression::ArraySlice(loc, _, _, _) => *loc,
            Expression::Parenthesis(loc, _) => *loc,
            Expression::BitAccess(loc, _, _) => *loc,
            Expression::Function(loc, _, _) => *loc,
            Expression::CodeBlock(loc, _, _) => *loc,
            Expression::NamedFunction(loc, _, _) => *loc,
            Expression::Not(loc, _) => *loc,
            Expression::BitwiseNot(loc, _) => *loc,
            Expression::UnaryPlus(loc, _) => *loc,
            Expression::Negate(loc, _) => *loc,
            Expression::Power(loc, _, _) => *loc,
            Expression::Multiply(loc, _, _) => *loc,
            Expression::Divide(loc, _, _) => *loc,
            Expression::Modulo(loc, _, _) => *loc,
            Expression::Add(loc, _, _) => *loc,
            Expression::Subtract(loc, _, _) => *loc,
            Expression::ShiftLeft(loc, _, _) => *loc,
            Expression::ShiftRight(loc, _, _) => *loc,
            Expression::BitwiseAnd(loc, _, _) => *loc,
            Expression::BitwiseXor(loc, _, _) => *loc,
            Expression::BitwiseOr(loc, _, _) => *loc,
            Expression::Less(loc, _, _) => *loc,
            Expression::More(loc, _, _) => *loc,
            Expression::LessEqual(loc, _, _) => *loc,
            Expression::MoreEqual(loc, _, _) => *loc,
            Expression::Equal(loc, _, _) => *loc,
            Expression::NotEqual(loc, _, _) => *loc,
            Expression::And(loc, _, _) => *loc,
            Expression::Or(loc, _, _) => *loc,
            Expression::ConditionalOperator(loc, _, _, _) => *loc,
            Expression::Assign(loc, _, _) => *loc,
            Expression::Number(loc, _) => *loc,
            Expression::Duration(loc, _, _) => *loc,
            Expression::Rational(loc, _, _) => *loc,
            Expression::String(_) => Location::Builtin,
            Expression::Type(loc, _) => *loc,
            Expression::Address(loc, _, _) => *loc,
            Expression::AnonAddress(loc, _, _) => *loc,
            Expression::Bool(loc, _) => *loc,
            Expression::Variable(var) => var.loc,
            Expression::List(loc, _) => *loc,
            Expression::Array(loc, _) => *loc,
            Expression::Initializer(loc, _) => *loc,
            Expression::Cast(loc, _, _) => *loc,
        }
    }
}

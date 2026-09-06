//! Узел АСД "условие перехода" ([`Condition`]) языка Takt.
//!
//! Вынесен из `ast.rs` **чистым перемещением** по той же причине, что и
//! [`Expression`](crate::parser::ast_expr::Expression): файл упёрся в предел размера
//! модуля, а условие - самостоятельное знание. Путь `parser::ast::Condition` сохранён
//! реэкспортом - от него зависят грамматика, семантика, форматтер и генераторы.

use crate::diagnostics::Location;
use crate::parser::ast::{Identifier, Member, StringLiteral};
#[cfg(feature = "ast-serde")]
use serde::{Deserialize, Serialize};

/// Условие перехода между состояниями.
///
/// Является упрощённым подмножеством [`Expression`], допускаемым в позиции условия
/// перехода `ref Имя: Условие`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum Condition {
    /// Доступ к элементу массива: `id[индекс]`.
    ArraySubscript(Location, Box<Condition>, Box<Condition>),
    /// Скобки: `(условие)`.
    Parenthesis(Location, Box<Condition>),
    /// Доступ к биту: `условие.член`.
    BitAccess(Location, Box<Condition>, Member),
    /// Вызов функции: `id(аргументы,*)`.
    Function(Location, Identifier, Vec<Condition>),
    /// Логическое не: `!условие`.
    Not(Location, Box<Condition>),
    /// Сложение: `левое + правое`.
    Add(Location, Box<Condition>, Box<Condition>),
    /// Вычитание: `левое - правое`.
    Subtract(Location, Box<Condition>, Box<Condition>),
    /// Побитовое И: `левое & правое`.
    And(Location, Box<Condition>, Box<Condition>),
    /// Побитовое или: `левое | правое`.
    Or(Location, Box<Condition>, Box<Condition>),
    /// Меньше: `левое < правое`.
    Less(Location, Box<Condition>, Box<Condition>),
    /// Больше: `левое > правое`.
    More(Location, Box<Condition>, Box<Condition>),
    /// Меньше или равно: `левое <= правое`.
    LessEqual(Location, Box<Condition>, Box<Condition>),
    /// Больше или равно: `левое >= правое`.
    MoreEqual(Location, Box<Condition>, Box<Condition>),
    /// Равенство: `левое = правое`.
    Equal(Location, Box<Condition>, Box<Condition>),
    /// Неравенство: `левое != правое`.
    NotEqual(Location, Box<Condition>, Box<Condition>),
    /// Целочисленный литерал.
    Number(Location, i128),
    /// Литерал длительности: `(позиция, наносекунды, как записано)` -.
    ///
    /// Исходный текст хранится ради форматтера: `1m30s` печатается как написано, а не
    /// канонизируется (приём узла `Rational`).
    Duration(Location, i64, String),
    /// Выдержка на ребре: `ref Имя: after 3s;`.
    ///
    /// Сахар над механизмом времени; скрытую метку времени заводит семантика, а не
    /// автор.
    After(Location, i64, String),
    /// Выдержка в **тактах** на ребре: `ref Имя: after 3t;`.
    ///
    /// Отдельный узел, а не длительность: такт - шаг логики, его физическая
    /// длительность неизвестна, и частота такой выдержке не нужна.
    AfterTicks(Location, i64, String),
    /// Выдержка **константным выражением**: `after DWELL`, `after (BASE + 30s)`.
    ///
    /// Внутреннее условие - арифметика над длительностями: литералы, имена констант
    /// типа `duration`, скобки, `+`/`-`. Скобочная форма сохраняет узел
    /// [`Condition::Parenthesis`] - форматтер обязан напечатать скобки обратно.
    ///
    /// Наносекунд здесь **нет**: значение вычисляет семантика, сводя узел к тому же
    /// `ConditionNode::After(нс)`, что и литерал - за границей семантики этой формы не
    /// существует, поэтому цели генерации о ней не знают. Отсюда и ограничение:
    /// операнды обязаны быть **константными**.
    AfterExpr(Location, Box<Condition>),
    /// Вещественный литерал: `(строка, отрицательный)`.
    Rational(Location, String, bool),
    /// Конкатенация строковых литералов.
    String(Vec<StringLiteral>),
    /// Булевый литерал.
    Bool(Location, bool),
    /// Обращение к ячейке по адресу в условии: `#адрес`/`#адрес:бит`.
    ///
    /// Без него ребро `ref Next: #0x100.0;` не разбиралось бы вовсе; ширину в условии
    /// задать нечем (`as` там нет), поэтому доходит битовая форма.
    AnonAddress(Location, i128, Option<i64>),
    /// Переменная.
    Variable(Identifier),
}

impl Condition {
    /// Возвращает местоположение условия в исходном тексте.
    pub fn loc(&self) -> Location {
        match self {
            Condition::ArraySubscript(loc, _, _)
            | Condition::Parenthesis(loc, _)
            | Condition::BitAccess(loc, _, _)
            | Condition::Function(loc, _, _)
            | Condition::Not(loc, _)
            | Condition::Add(loc, _, _)
            | Condition::Subtract(loc, _, _)
            | Condition::And(loc, _, _)
            | Condition::Or(loc, _, _)
            | Condition::Less(loc, _, _)
            | Condition::More(loc, _, _)
            | Condition::LessEqual(loc, _, _)
            | Condition::MoreEqual(loc, _, _)
            | Condition::Equal(loc, _, _)
            | Condition::NotEqual(loc, _, _)
            | Condition::Number(loc, _)
            | Condition::Rational(loc, _, _)
            | Condition::Duration(loc, _, _)
            | Condition::After(loc, _, _)
            | Condition::AfterTicks(loc, _, _)
            | Condition::AfterExpr(loc, _)
            | Condition::AnonAddress(loc, _, _)
            | Condition::Bool(loc, _) => *loc,
            Condition::Variable(id) => id.loc,
            Condition::String(parts) => parts.first().map(|s| s.loc).unwrap_or(Location::Implicit),
        }
    }
}

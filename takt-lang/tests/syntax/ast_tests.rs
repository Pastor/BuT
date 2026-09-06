//! Тесты для модуля [`takt_lang::parser::ast`].
//!
//! Проверяют все методы типов [`Location`], [`Comment`], [`ImportDefine`],
//! [`Expression`], [`FunctionDefine`], [`Statement`], [`FormulaBlock`] и
//! вспомогательных структур.

use takt_lang::diagnostics::Location;
use takt_lang::parser::ast::{
    Comment, Expression, FormulaBlock, FormulaStatement, FunctionDefine, Identifier, ImportDefine,
    ImportPath, Member, NamedArgument, Statement, StringLiteral, Type,
};

// ------------------------------------------------------------------- Вспомогательные
// функции -------------------------------------------------------------------

fn loc(start: usize, end: usize) -> Location {
    Location::source(0, start, end)
}

fn num(n: i128) -> Expression {
    Expression::Number(loc(0, 0), n)
}

fn var(name: &str) -> Expression {
    Expression::Variable(Identifier::new(name))
}

fn bool_expr(b: bool) -> Expression {
    Expression::Bool(loc(0, 0), b)
}

// ------------------------------------------------------------------- Location
// -------------------------------------------------------------------

/// Default Location - Source(0, 0, 0).
#[test]
fn location_default() {
    assert_eq!(Location::default(), Location::Source(0, 0, 0));
}

/// `begin_range()` возвращает нулевой диапазон в начале.
#[test]
fn location_begin_range() {
    let l = loc(5, 10);
    assert_eq!(l.begin_range(), Location::Source(0, 5, 5));
}

/// `begin_range()` на не-Source вариантах возвращает сам вариант.
#[test]
fn location_begin_range_builtin() {
    assert_eq!(Location::Builtin.begin_range(), Location::Builtin);
    assert_eq!(Location::CommandLine.begin_range(), Location::CommandLine);
    assert_eq!(Location::Implicit.begin_range(), Location::Implicit);
    assert_eq!(Location::Codegen.begin_range(), Location::Codegen);
}

/// `end_range()` возвращает нулевой диапазон в конце.
#[test]
fn location_end_range() {
    let l = loc(5, 10);
    assert_eq!(l.end_range(), Location::Source(0, 10, 10));
}

/// `end_range()` на не-Source вариантах.
#[test]
fn location_end_range_builtin() {
    assert_eq!(Location::Builtin.end_range(), Location::Builtin);
    assert_eq!(Location::Implicit.end_range(), Location::Implicit);
    assert_eq!(Location::Codegen.end_range(), Location::Codegen);
}

/// `try_file_no()` возвращает Some для Source.
#[test]
fn location_try_file_no_source() {
    let l = Location::Source(42, 0, 0);
    assert_eq!(l.try_file_no(), Some("42".to_string()));
}

/// `try_file_no()` возвращает None для не-Source вариантов.
#[test]
fn location_try_file_no_non_source() {
    assert_eq!(Location::Builtin.try_file_no(), None);
    assert_eq!(Location::CommandLine.try_file_no(), None);
    assert_eq!(Location::Implicit.try_file_no(), None);
    assert_eq!(Location::Codegen.try_file_no(), None);
}

/// `start()` возвращает начальное смещение.
#[test]
fn location_start() {
    assert_eq!(loc(3, 8).start(), 3);
}

/// `end()` возвращает конечное смещение.
#[test]
fn location_end() {
    assert_eq!(loc(3, 8).end(), 8);
}

/// `exclusive_end()` - `end + 1`.
#[test]
fn location_exclusive_end() {
    assert_eq!(loc(3, 8).exclusive_end(), 9);
}

/// `range()` возвращает стандартный диапазон.
#[test]
fn location_range() {
    assert_eq!(loc(2, 7).range(), 2..7);
}

/// `with_start()` заменяет начало.
#[test]
fn location_with_start() {
    let l = loc(0, 10).with_start(5);
    assert_eq!(l, loc(5, 10));
}

/// `with_end()` заменяет конец.
#[test]
fn location_with_end() {
    let l = loc(0, 10).with_end(20);
    assert_eq!(l, loc(0, 20));
}

/// `use_start_from()` копирует начало из другого Location.
#[test]
fn location_use_start_from() {
    let mut l = loc(0, 10);
    l.use_start_from(&loc(5, 15));
    assert_eq!(l, loc(5, 10));
}

/// `use_end_from()` копирует конец из другого Location.
#[test]
fn location_use_end_from() {
    let mut l = loc(0, 10);
    l.use_end_from(&loc(5, 20));
    assert_eq!(l, loc(0, 20));
}

/// `with_start_from()` - неизменяемая версия `use_start_from`.
#[test]
fn location_with_start_from() {
    let result = loc(0, 10).with_start_from(&loc(3, 15));
    assert_eq!(result, loc(3, 10));
}

/// `with_end_from()` - неизменяемая версия `use_end_from`.
#[test]
fn location_with_end_from() {
    let result = loc(0, 10).with_end_from(&loc(3, 25));
    assert_eq!(result, loc(0, 25));
}

/// Location Copy - можно скопировать без клонирования.
#[test]
fn location_is_copy() {
    let l = loc(1, 2);
    let _copy = l; // copy
    let _ = l.start(); // оригинал ещё доступен
}

// ------------------------------------------------------------------- Identifier
// -------------------------------------------------------------------

/// `Identifier::new()` создаёт идентификатор с default-позицией.
#[test]
fn identifier_new() {
    let id = Identifier::new("myVar");
    assert_eq!(id.name, "myVar");
    assert_eq!(id.loc, Location::default());
}

// ------------------------------------------------------------------- Comment
// -------------------------------------------------------------------

/// `Comment::Line::value()` возвращает текст.
#[test]
fn comment_line_value() {
    let c = Comment::Line(loc(0, 5), "текст".to_string());
    assert_eq!(c.value(), "текст");
}

/// `Comment::DocLine::value()` возвращает текст.
#[test]
fn comment_doc_line_value() {
    let c = Comment::DocLine(loc(0, 5), "документация".to_string());
    assert_eq!(c.value(), "документация");
}

/// `is_doc()` - только для DocLine.
#[test]
fn comment_is_doc() {
    let line = Comment::Line(loc(0, 5), "".to_string());
    let doc = Comment::DocLine(loc(0, 5), "".to_string());
    assert!(!line.is_doc());
    assert!(doc.is_doc());
}

/// `is_line()` - для обоих вариантов.
#[test]
fn comment_is_line() {
    let line = Comment::Line(loc(0, 5), "".to_string());
    let doc = Comment::DocLine(loc(0, 5), "".to_string());
    assert!(line.is_line());
    assert!(doc.is_line()); // DocLine тоже строчный
}

// -------------------------------------------------------------------
// ImportDefine::literal()
// -------------------------------------------------------------------

fn str_literal(s: &str) -> StringLiteral {
    StringLiteral {
        loc: loc(0, s.len()),
        unicode: false,
        string: s.to_string(),
    }
}

/// `ImportDefine::Plain` со строкой - literal() возвращает Some.
#[test]
fn import_plain_literal_some() {
    let import = ImportDefine::Plain(ImportPath::Filename(str_literal("foo.takt")), loc(0, 10));
    assert!(import.literal().is_some());
    assert_eq!(import.literal().unwrap().string, "foo.takt");
}

/// `ImportDefine::GlobalSymbol` со строкой - literal() возвращает Some.
#[test]
fn import_global_symbol_literal_some() {
    let import = ImportDefine::GlobalSymbol(
        ImportPath::Filename(str_literal("bar.takt")),
        Identifier::new("Bar"),
        loc(0, 20),
    );
    assert!(import.literal().is_some());
}

/// `ImportDefine::Rename` со строкой - literal() возвращает Some.
#[test]
fn import_rename_literal_some() {
    let import = ImportDefine::Rename(
        ImportPath::Filename(str_literal("baz.takt")),
        vec![],
        loc(0, 20),
    );
    assert!(import.literal().is_some());
}

/// `ImportDefine::Plain` с путём (не строкой) - literal() возвращает None.
#[test]
fn import_plain_path_literal_none() {
    let path = ImportPath::Path(takt_lang::parser::ast::IdentifierPath {
        loc: loc(0, 5),
        identifiers: vec![Identifier::new("std")],
    });
    let import = ImportDefine::Plain(path, loc(0, 10));
    assert!(import.literal().is_none());
}

// ------------------------------------------------------------------- Expression
// -------------------------------------------------------------------

/// `remove_parenthesis()` убирает одни скобки.
#[test]
fn expression_remove_parenthesis() {
    let inner = num(42);
    let parens = Expression::Parenthesis(loc(0, 5), Box::new(inner.clone()));
    assert_eq!(parens.remove_parenthesis(), &inner);
}

/// `remove_parenthesis()` на не-скобках возвращает self.
#[test]
fn expression_remove_parenthesis_no_op() {
    let e = num(1);
    assert_eq!(e.remove_parenthesis(), &e);
}

/// `strip_parentheses()` рекурсивно убирает скобки.
#[test]
fn expression_strip_parentheses() {
    let inner = num(7);
    let nested = Expression::Parenthesis(
        loc(0, 10),
        Box::new(Expression::Parenthesis(loc(1, 9), Box::new(inner.clone()))),
    );
    assert_eq!(nested.strip_parentheses(), &inner);
}

/// `is_unsplittable()` - числа, переменные, строки и т.д.
#[test]
fn expression_is_unsplittable() {
    assert!(num(1).is_unsplittable());
    assert!(var("x").is_unsplittable());
    assert!(Expression::Rational(loc(0, 0), "3.14".to_string(), false).is_unsplittable());
    assert!(Expression::Address(loc(0, 0), 0, 0).is_unsplittable());
    assert!(Expression::String(vec![]).is_unsplittable());
}

/// Бинарные и унарные операторы - не `is_unsplittable`.
#[test]
fn expression_not_unsplittable() {
    let add = Expression::Add(loc(0, 0), Box::new(num(1)), Box::new(num(2)));
    assert!(!add.is_unsplittable());

    let not = Expression::Not(loc(0, 0), Box::new(bool_expr(true)));
    assert!(!not.is_unsplittable());
}

/// `has_space_around()` - унарные не нуждаются в пробелах.
#[test]
fn expression_has_space_around_unary() {
    let not = Expression::Not(loc(0, 0), Box::new(bool_expr(false)));
    let bitwise_not = Expression::BitwiseNot(loc(0, 0), Box::new(num(0)));
    let plus = Expression::UnaryPlus(loc(0, 0), Box::new(num(1)));
    let neg = Expression::Negate(loc(0, 0), Box::new(num(1)));
    assert!(!not.has_space_around());
    assert!(!bitwise_not.has_space_around());
    assert!(!plus.has_space_around());
    assert!(!neg.has_space_around());
}

/// `has_space_around()` - бинарные нуждаются в пробелах.
#[test]
fn expression_has_space_around_binary() {
    let add = Expression::Add(loc(0, 0), Box::new(num(1)), Box::new(num(2)));
    assert!(add.has_space_around());
}

/// `is_literal()` - адрес, число, массив, рациональное, строка.
#[test]
fn expression_is_literal() {
    assert!(Expression::Number(loc(0, 0), 42).is_literal());
    assert!(Expression::Address(loc(0, 0), 0, 0).is_literal());
    assert!(Expression::Array(loc(0, 0), vec![]).is_literal());
    assert!(Expression::Rational(loc(0, 0), "1.5".to_string(), false).is_literal());
    assert!(Expression::String(vec![]).is_literal());
}

/// `is_literal()` - переменная и операторы не являются литералами.
#[test]
fn expression_is_not_literal() {
    assert!(!var("x").is_literal());
    assert!(!bool_expr(true).is_literal());
    assert!(!Expression::Add(loc(0, 0), Box::new(num(1)), Box::new(num(2))).is_literal());
}

/// `loc()` возвращает корректное местоположение для числа.
#[test]
fn expression_loc_number() {
    let e = Expression::Number(loc(3, 8), 42);
    assert_eq!(e.loc(), loc(3, 8));
}

/// `loc()` возвращает Builtin для String(vec![]).
#[test]
fn expression_loc_string_is_builtin() {
    let e = Expression::String(vec![]);
    assert_eq!(e.loc(), Location::Builtin);
}

/// `loc()` для Variable - берётся из Identifier.
#[test]
fn expression_loc_variable() {
    let mut id = Identifier::new("x");
    id.loc = loc(5, 6);
    let e = Expression::Variable(id);
    assert_eq!(e.loc(), loc(5, 6));
}

/// `components()` для унарного оператора: (None, Some).
#[test]
fn expression_components_unary() {
    let inner = num(1);
    let not = Expression::Not(loc(0, 0), Box::new(inner.clone()));
    assert_eq!(not.components(), (None, Some(&inner)));
}

/// `components()` для бинарного оператора: (Some, Some).
#[test]
fn expression_components_binary() {
    let a = num(1);
    let b = num(2);
    let add = Expression::Add(loc(0, 0), Box::new(a.clone()), Box::new(b.clone()));
    assert_eq!(add.components(), (Some(&a), Some(&b)));
}

/// `components()` для листового выражения: (None, None).
#[test]
fn expression_components_leaf() {
    assert_eq!(num(42).components(), (None, None));
    assert_eq!(var("x").components(), (None, None));
    assert_eq!(bool_expr(false).components(), (None, None));
}

/// `components_mut()` для унарного оператора возвращает изменяемую ссылку.
#[test]
fn expression_components_mut_unary() {
    let mut e = Expression::Not(loc(0, 0), Box::new(num(5)));
    let (left, right) = e.components_mut();
    assert!(left.is_none());
    assert!(right.is_some());
}

/// `components()` для всех бинарных вариантов.
#[test]
fn expression_components_all_binary() {
    let a = num(1);
    let b = num(2);
    let make_bin = |e: Expression| e.components() != (None, None);

    // Бинарные
    assert!(make_bin(Expression::Power(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::Multiply(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::Divide(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::Modulo(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::Subtract(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::ShiftLeft(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::ShiftRight(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::BitwiseAnd(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::BitwiseXor(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::BitwiseOr(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::Less(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::More(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::LessEqual(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::MoreEqual(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::Equal(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::NotEqual(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::And(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::Or(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
    assert!(make_bin(Expression::Assign(
        loc(0, 0),
        Box::new(a.clone()),
        Box::new(b.clone())
    )));
}

/// `loc()` для всех вариантов Expression (не паникует).
#[test]
fn expression_loc_all_variants() {
    let l = loc(0, 1);
    let a = num(1);
    let b = num(2);

    let variants: Vec<Expression> = vec![
        Expression::ArraySubscript(
            l,
            Box::new(Expression::Variable(Identifier::new("x"))),
            Box::new(Expression::Number(l, 0)),
        ),
        Expression::ArraySlice(
            l,
            Box::new(Expression::Variable(Identifier::new("x"))),
            None,
            None,
        ),
        Expression::Parenthesis(l, Box::new(a.clone())),
        Expression::BitAccess(l, Box::new(a.clone()), Member::Number(0)),
        Expression::Function(l, Identifier::new("f"), vec![]),
        Expression::Not(l, Box::new(a.clone())),
        Expression::BitwiseNot(l, Box::new(a.clone())),
        Expression::UnaryPlus(l, Box::new(a.clone())),
        Expression::Negate(l, Box::new(a.clone())),
        Expression::Power(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::Multiply(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::Divide(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::Modulo(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::Add(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::Subtract(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::ShiftLeft(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::ShiftRight(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::BitwiseAnd(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::BitwiseXor(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::BitwiseOr(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::Less(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::More(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::LessEqual(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::MoreEqual(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::Equal(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::NotEqual(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::And(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::Or(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::ConditionalOperator(
            l,
            Box::new(a.clone()),
            Box::new(b.clone()),
            Box::new(a.clone()),
        ),
        Expression::Assign(l, Box::new(a.clone()), Box::new(b.clone())),
        Expression::Number(l, 0),
        Expression::Rational(l, "1.0".to_string(), false),
        Expression::Type(l, Type::Bit),
        Expression::Address(l, 0, 0),
        Expression::Bool(l, true),
        Expression::List(l, vec![]),
        Expression::Array(l, vec![]),
        Expression::Initializer(l, vec![]),
        Expression::Cast(l, Box::new(a.clone()), Type::Rational),
    ];

    for e in &variants {
        let _ = e.loc(); // не должно паниковать
    }
}

// ------------------------------------------------------------------- FunctionDefine
// -------------------------------------------------------------------

fn empty_fn() -> FunctionDefine {
    FunctionDefine {
        loc: loc(0, 0),
        name: Some(Identifier::new("f")),
        name_loc: loc(0, 0),
        params: vec![],
        return_type: None,
        body: None,
        external: false,
        attribute: None,
    }
}

/// `is_void()` - нет возвращаемого типа.
#[test]
fn function_define_is_void_true() {
    let f = empty_fn();
    assert!(f.is_void());
}

/// `is_void()` - есть возвращаемый тип.
#[test]
fn function_define_is_void_false() {
    let mut f = empty_fn();
    f.return_type = Some(Type::Bit);
    assert!(!f.is_void());
}

/// `is_empty()` - тело отсутствует.
#[test]
fn function_define_is_empty_no_body() {
    assert!(empty_fn().is_empty());
}

/// `is_empty()` - тело пустой блок.
#[test]
fn function_define_is_empty_empty_block() {
    let mut f = empty_fn();
    f.body = Some(Statement::Block {
        loc: loc(0, 0),
        unchecked: false,
        statements: vec![],
    });
    assert!(f.is_empty());
}

/// `is_empty()` - тело с операторами: не пустое.
#[test]
fn function_define_is_empty_non_empty_body() {
    let mut f = empty_fn();
    f.body = Some(Statement::Block {
        loc: loc(0, 0),
        unchecked: false,
        statements: vec![Statement::Continue(loc(0, 0))],
    });
    assert!(!f.is_empty());
}

// -------------------------------------------------------------------
// Statement::is_empty()
// -------------------------------------------------------------------

/// Пустой блок - `is_empty()` -> true.
#[test]
fn statement_block_is_empty() {
    let s = Statement::Block {
        loc: loc(0, 0),
        unchecked: false,
        statements: vec![],
    };
    assert!(s.is_empty());
}

/// Непустой блок - `is_empty()` -> false.
#[test]
fn statement_block_is_not_empty() {
    let s = Statement::Block {
        loc: loc(0, 0),
        unchecked: false,
        statements: vec![Statement::Break(loc(0, 0))],
    };
    assert!(!s.is_empty());
}

/// Не-блоковые операторы - `is_empty()` -> false.
#[test]
fn statement_non_block_is_not_empty() {
    assert!(!Statement::Continue(loc(0, 0)).is_empty());
    assert!(!Statement::Break(loc(0, 0)).is_empty());
    assert!(!Statement::Return(loc(0, 0), None).is_empty());
    assert!(!Statement::Error(loc(0, 0)).is_empty());
    assert!(!Statement::StraySemicolon(loc(0, 0)).is_empty());
    assert!(!Statement::Expression(loc(0, 0), num(1)).is_empty());
}

/// `Statement::Args` пустой список - `is_empty()` -> true.
#[test]
fn statement_args_empty_is_empty() {
    assert!(Statement::Args(loc(0, 0), vec![]).is_empty());
}

/// `Statement::Args` с элементом - `is_empty()` -> false.
#[test]
fn statement_args_non_empty_is_not_empty() {
    let arg = NamedArgument {
        loc: loc(0, 0),
        name: Some(Identifier::new("k")),
        expr: num(1),
    };
    assert!(!Statement::Args(loc(0, 0), vec![arg]).is_empty());
}

// -------------------------------------------------------------------
// FormulaBlock::is_empty()
// -------------------------------------------------------------------

/// Пустой блок формулы.
#[test]
fn formula_block_is_empty() {
    let b = FormulaBlock {
        loc: loc(0, 0),
        statements: vec![],
    };
    assert!(b.is_empty());
}

/// Непустой блок формулы.
#[test]
fn formula_block_is_not_empty() {
    let b = FormulaBlock {
        loc: loc(0, 0),
        statements: vec![FormulaStatement::Error(loc(0, 0))],
    };
    assert!(!b.is_empty());
}

// ------------------------------------------------------------------- Контр-примеры
// -------------------------------------------------------------------

/// `strip_parentheses()` на нескольких уровнях - не-скобочное выражение.
#[test]
fn expression_strip_no_parens() {
    let e = num(99);
    assert_eq!(e.strip_parentheses(), &e);
}

/// `is_literal()` на Bool - не литерал.
#[test]
fn expression_bool_is_not_literal() {
    assert!(!bool_expr(true).is_literal());
}

/// `is_literal()` на Initializer - не литерал (инициализатор структуры).
#[test]
fn expression_initializer_is_not_literal() {
    let e = Expression::Initializer(loc(0, 0), vec![num(1)]);
    assert!(!e.is_literal());
}

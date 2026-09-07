//! Зонд-тесты: смена операторов (Option B).
//!
//! Проверяют новую операторную семантику на уровне разбора:
//! - `:=` - присваивание/инициализация значения (`Expression::Assign`, инициализатор);
//! - `=` - сравнение на равенство в выражениях (`Expression::Equal`);
//! - `<=` - по-прежнему "меньше-или-равно" (`Expression::LessEqual`), а не присваивание;
//! - `==` - **выведен** из языка (ошибка разбора);
//! - `=` в условиях (`cond`) - по-прежнему равенство (`Condition::Equal`).
//!
//! Легаси-фикстуры (старый синтаксис `=`/`==`) мигрируются отдельно.

use takt_lang::parse;
use takt_lang::parser::ast::{
    Condition, Expression, ModelElement, StateElement, Statement, VariableDefine,
};

/// Разбирает `src` и возвращает корневую модель (паникует при ошибках разбора).
fn must_parse(src: &str) -> takt_lang::parser::ast::Model {
    parse(src, 0)
        .unwrap_or_else(|d| {
            let msgs: Vec<_> = d.iter().map(|x| x.message.clone()).collect();
            panic!("Разбор завершился с ошибками: {msgs:?}");
        })
        .0
}

/// Инициализатор первого `var` в корневой модели.
fn first_var_initializer(src: &str) -> Option<Expression> {
    let root = must_parse(src);
    root.elements.into_iter().find_map(|e| match e {
        ModelElement::Variable(v) => match *v {
            VariableDefine::Variable { initializer, .. } => Some(initializer),
            _ => None,
        },
        _ => None,
    })?
}

/// `:=` инициализирует значение переменной (`var x := 5`).
#[test]
fn colon_assign_is_value_initializer() {
    let init = first_var_initializer("var x: u8 := 5; model M { start S; }");
    assert!(
        init.is_some(),
        "Инициализатор через := должен присутствовать"
    );
}

/// `=` в позиции выражения - это равенство (`Expression::Equal`), а не присваивание.
#[test]
fn equals_is_equality_in_expression() {
    let init = first_var_initializer("var b: bit := 1 = 1; model M { start S; }")
        .expect("ожидался инициализатор");
    assert!(
        matches!(init, Expression::Equal(..)),
        "`=` в выражении должно давать Expression::Equal, получено: {init:?}"
    );
}

/// `<=` по-прежнему реляционный оператор (не превращён в присваивание).
#[test]
fn less_equal_stays_relational() {
    let init = first_var_initializer("var b: bit := 3 <= 5; model M { start S; }")
        .expect("ожидался инициализатор");
    assert!(
        matches!(init, Expression::LessEqual(..)),
        "`<=` должно оставаться Expression::LessEqual, получено: {init:?}"
    );
}

/// `:=` даёт `Expression::Assign` - в позиции **оператора**.
///
/// Прежде тест брал форму `var y: u8 := (x := 7);`, то есть присваивание
/// **внутри выражения**: до исправления
/// [](../../docs/fixes/-assignment-is-statement-in-grammar.md)
/// грамматика её принимала. Теперь правило "присваивание - отдельная операция"
/// держится синтаксисом (`SY-006`), и узел проверяется там, где запись
/// законна, - в операторе тела.
#[test]
fn colon_assign_expression_is_assign_node() {
    let src = "var x: u8 := 0; start S { always { x := 7; } }";
    let root = must_parse(src);
    let assign = root
        .elements
        .into_iter()
        .find_map(|e| match e {
            ModelElement::State(state) => state.elements.into_iter().find_map(|el| match el {
                StateElement::NamedBlockCode(block) => match block.statement {
                    Statement::Block { statements, .. } => {
                        statements.into_iter().find_map(|st| match st {
                            Statement::Expression(_, expr) => Some(expr),
                            _ => None,
                        })
                    }
                    _ => None,
                },
                _ => None,
            }),
            _ => None,
        })
        .expect("ожидался оператор-выражение в теле блока");
    assert!(
        matches!(assign, Expression::Assign(..)),
        "`:=` в позиции оператора обязано давать Expression::Assign, получено: {assign:?}"
    );
}

/// Присваивание **внутри выражения** отвергается разбором.
///
/// Три формы, которые прежде принимались: инициализатор, арифметика, условие. Отказ
/// приходит от парсера с кодом `SY-006` и позицией самого токена `:=` - а не от
/// семантики с позицией объявления цели записи.
#[test]
fn assignment_inside_expression_is_syntax_error() {
    for src in [
        "var x: u8 := 0; var y: u8 := (x := 7); model M { start S; }",
        "var a: u8 := 0; var b: u8 := 0; start S { always { b := (a := 3) + 1; } }",
        "var a: u8 := 0; start S { always { if (a := 1) = 1 { a := 2; } } }",
        "var a: u8 := 0; var b: u8 := 0; var c: u8 := 0; start S { always { a := b := c; } }",
    ] {
        let codes: Vec<String> = takt_lang::parse(src, 0)
            .expect_err("форма обязана отвергаться разбором")
            .into_iter()
            .filter_map(|d| d.code)
            .collect();
        assert!(
            codes.contains(&"SY-006".to_string()),
            "ожидался SY-006 для {src:?}, получено {codes:?}"
        );
    }
}

/// `==` выведен из языка - ошибка разбора (подсказка "использовать `=`").
#[test]
fn double_equals_is_parse_error() {
    let result = parse("var b: bit := 1 == 1; model M { start S; }", 0);
    assert!(
        result.is_err(),
        "`==` должно давать ошибку разбора (оператор выведен)"
    );
}

/// `=` в `cond` по-прежнему равенство (`Condition::Equal`) - инвариант условий.
#[test]
fn condition_equality_unchanged() {
    let root = must_parse("var x: bit := false; cond IsZero = x = 0; model M { start S; }");
    let cond = root
        .elements
        .into_iter()
        .find_map(|e| match e {
            ModelElement::Condition(c) => Some(c.value),
            _ => None,
        })
        .expect("ожидалось условие IsZero");
    assert!(
        matches!(cond, Condition::Equal(..)),
        "`=` в cond должно оставаться Condition::Equal, получено: {cond:?}"
    );
}

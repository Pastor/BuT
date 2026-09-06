//! Координата судей тела - позиция употребления, не объявления.
//!
//! # Что здесь тестытся
//!
//! Понижённое выражение своей позиции не имеет: `ExpressionNode::loc()` выводит её из
//! объявлений операндов, а позиции вхождений живут отдельным слоем `semantic::usages`.
//! Поэтому судьи тела указывали не туда:
//!
//! | Вход | Координата до фичи | После |
//! |---|---|---|
//! | `f(n) := 1;` в строке 10 | **3:1** - строка, где объявлена `f` | 10:9 |
//! | `5 := 2;` в строке 6 | **нет вовсе** (сообщение без преисправления) | 6:9 |
//! | `n := g(n := 2);` в строке 9 | позиция объявления `n` (строка 1) | 9:9 |
//!
//! **Чужая верная координата хуже отсутствующей:** она выглядит достоверной, и автор
//! идёт читать не ту строку.
//!
//! Позицию несёт `StatementNode::Expression`: при понижении она берётся у АСД-оператора -
//! единственного места, где координата употребления ещё цела.

use takt_lang::diagnostics::Location;

/// Строит дерево и возвращает диагностику отказа.
fn error_of(src: &str) -> takt_lang::diagnostics::Diagnostic {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    takt_lang::semantic::tree::construct_model(&ast, None, &[])
        .expect_err("вход обязан отвергаться")
}

/// Строка и колонка диагностики - по её смещению в исходнике.
fn line_column(src: &str, diagnostic: &takt_lang::diagnostics::Diagnostic) -> (usize, usize) {
    let Location::Source(_, start, _) = diagnostic.loc else {
        panic!(
            "у диагностики нет позиции в исходнике: {:?}",
            diagnostic.loc
        );
    };
    let prefix = &src[..start as usize];
    let line = prefix.matches('\n').count() + 1;
    let column = prefix
        .rsplit('\n')
        .next()
        .map_or(1, |l| l.chars().count() + 1);
    (line, column)
}

/// **T1.** Вызов функции слева: координата - строка употребления, не объявления.
#[test]
fn call_on_the_left_points_at_the_statement() {
    let src = "var n: u8 := 0;\n\
               \n\
               fn f(a: u8) -> u8 {\n\
               \x20   return a;\n\
               }\n\
               \n\
               start Run {\n\
               \x20   always {\n\
               \x20       f(n) := 1;\n\
               \x20   }\n\
               }\n";
    let err = error_of(src);
    assert_eq!(err.code.as_deref(), Some("SE-111"));
    assert_eq!(
        line_column(src, &err).0,
        9,
        "координата обязана указывать на строку с `f(n) := 1;`, а не на объявление `f`"
    );
}

/// **T2.** Чистый литерал слева: координата есть (её не было вовсе).
#[test]
fn literal_on_the_left_has_a_position() {
    let src = "var n: u8 := 0;\n\
               \n\
               start Run {\n\
               \x20   always {\n\
               \x20       5 := 2;\n\
               \x20   }\n\
               }\n";
    let err = error_of(src);
    assert_eq!(err.code.as_deref(), Some("SE-111"));
    assert!(
        matches!(err.loc, Location::Source(..)),
        "у литерала слева координаты не было вовсе: {:?}",
        err.loc
    );
    assert_eq!(line_column(src, &err).0, 5);
}

/// **T3.** Соседний судья (`SE-095`) получил ту же координату.
///
/// Ограничение было общим у судей тела: `target_of` подставлял `Location::Codegen`,
/// когда цель не опознана, и позицию объявления, когда опознана. Обе - не о том месте,
/// где автор написал присваивание.
#[test]
fn assignment_in_value_position_points_at_the_statement() {
    let src = "var n: u8 := 0;\n\
               \n\
               fn g(a: u8) -> u8 {\n\
               \x20   return a;\n\
               }\n\
               \n\
               start Run {\n\
               \x20   always {\n\
               \x20       n := g(n := 2);\n\
               \x20   }\n\
               }\n";
    let err = error_of(src);
    assert_eq!(err.code.as_deref(), Some("SE-095"));
    assert_eq!(
        line_column(src, &err).0,
        9,
        "координата обязана указывать на строку употребления"
    );
}

/// **T4. Контроль: законная запись по-прежнему проходит.**
///
/// Позиция - свойство диагностики; правка не должна была тронуть решение о том, законен
/// ли вход.
#[test]
fn legal_assignment_is_still_accepted() {
    let src = "var n: u8 := 0;\n\
               \n\
               start Run {\n\
               \x20   always { n := n + 1; }\n\
               \x20   ref Run: n < 3;\n\
               }\n";
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    takt_lang::semantic::tree::construct_model(&ast, None, &[])
        .expect("законная запись обязана строиться");
}

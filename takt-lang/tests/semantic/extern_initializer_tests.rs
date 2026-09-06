//! Вызов внешней функции в инициализаторе - `SE-084`.
//!
//! # Границы правила
//!
//! Запрет **узкий** - только внешняя функция. Вызов локальной вычисляется
//! (`const_eval`), и `var x := seed();` работает; вызов `extern` **в теле** законен и
//! остаётся штатным путём.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;

fn build(src: &str) -> Result<(), takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = parse(src, 0).expect("разбор");
    construct_model(&ast, None, &[]).map(|_| ())
}

/// Предмет: внешняя функция в инициализаторе отвергается с `SE-084`.
#[test]
fn extern_call_in_initializer_is_rejected() {
    let err =
        build("extern fn sensor() -> u8;\nvar mirror: u8 := sensor();\nstart Run { ref Run; }\n")
            .expect_err("вход обязан отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-084"), "{err:?}");
    assert!(
        err.message.contains("sensor") && err.message.contains("always"),
        "сообщение обязано назвать функцию и штатный путь:\n{}",
        err.message
    );
}

/// Внешняя функция **внутри выражения** ловится так же: обход рекурсивен.
#[test]
fn extern_call_nested_in_expression_is_rejected() {
    let err = build(
        "extern fn sensor() -> u8;\nvar mirror: u8 := sensor() + 1;\nstart Run { ref Run; }\n",
    )
    .expect_err("вложенный вызов обязан отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-084"), "{err:?}");
}

/// **Контроль:** вызов локальной функции вычисляется и остаётся законным.
///
/// Без него "внешняя отвергается" означало бы "отвергается любой вызов".
#[test]
fn local_call_in_initializer_is_accepted() {
    build("fn seed() -> u8 { return 7; }\nvar mirror: u8 := seed();\nstart Run { ref Run; }\n")
        .expect("вызов локальной функции вычисляется при компиляции");
}

/// **Контроль:** та же внешняя функция в теле состояния законна - это штатный
/// путь, названный в тексте диагностики.
#[test]
fn extern_call_in_body_is_accepted() {
    build(
        "extern fn sensor() -> u8;\nvar mirror: u8 := 0;\n\
         start Run { always { mirror := sensor(); } ref Run; }\n",
    )
    .expect("вызов внешней функции в теле — штатный путь");
}

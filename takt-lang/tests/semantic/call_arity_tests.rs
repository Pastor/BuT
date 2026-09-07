//! Число аргументов вызова сверяется с объявлением - `SE-122`.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;

fn build(src: &str) -> Result<(), takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = parse(src, 0).expect("разбор");
    construct_model(&ast, None, &[]).map(|_| ())
}

const BODY: &str = "start Run { always { r := CALL; } ref Run; }\n";

fn with_call(prelude: &str, call: &str) -> String {
    format!("{prelude}var r: u8 := 0;\n{}", BODY.replace("CALL", call))
}

/// Предмет: локальная функция с недостачей аргументов.
#[test]
fn local_function_arity_is_checked() {
    let err = build(&with_call(
        "fn two(a: u8, b: u8) -> u8 { return a + b; }\n",
        "two(1)",
    ))
    .expect_err("вызов обязан отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-122"), "{err:?}");
    assert!(
        err.message.contains("two") && err.message.contains('2') && err.message.contains('1'),
        "текст обязан назвать функцию и оба числа:\n{}",
        err.message
    );
}

/// Встроенная функция проверяется **той же** проверкой.
///
/// Без этой проверки вход доезжает до эталона и получает `SIM-020` с текстом про `S(Модель)` -
/// объяснение **чужого** случая.
#[test]
fn builtin_function_arity_is_checked() {
    let err = build(&with_call("", "min(1)")).expect_err("вызов обязан отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-122"), "{err:?}");
    assert!(err.message.contains("min"), "{}", err.message);
}

/// Лишний аргумент ловится так же, как недостача.
#[test]
fn extra_argument_is_checked() {
    let err = build(&with_call("", "abs(1, 2)")).expect_err("вызов обязан отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-122"), "{err:?}");
}

/// **Контроль:** согласованный вызов законен - и локальный, и встроенный.
///
/// Без него "недостача отвергается" означало бы "отвергается любой вызов".
#[test]
fn matching_calls_are_accepted() {
    build(&with_call(
        "fn two(a: u8, b: u8) -> u8 { return a + b; }\n",
        "two(1, 2)",
    ))
    .expect("согласованный вызов локальной функции законен");
    build(&with_call("", "min(1, 2)")).expect("согласованный вызов встроенной законен");
}

/// **Граница:** неразрешённое имя судит своя диагностика, а не эта.
///
/// Второй ответ на тот же вход был бы шумом: `SE-004` уже говорит, что функции нет
/// вовсе.
#[test]
fn unknown_function_keeps_its_own_diagnostic() {
    let err = build(&with_call("", "nosuch(1)")).expect_err("имя не разрешается");
    assert_ne!(err.code.as_deref(), Some("SE-122"), "{err:?}");
}

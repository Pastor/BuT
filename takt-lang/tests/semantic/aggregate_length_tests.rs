//! Длина агрегата сверяется с объявлением - `SE-123`.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;

fn build(src: &str) -> Result<(), takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = parse(src, 0).expect("разбор");
    construct_model(&ast, None, &[]).map(|_| ())
}

fn array_decl(init: &str) -> Result<(), takt_lang::diagnostics::Diagnostic> {
    build(&format!(
        "var a: [u8; 2] := {init};\nstart Run {{ ref Run; }}\n"
    ))
}

/// Предмет: лишний элемент массива отвергается, текст называет оба числа.
#[test]
fn extra_element_is_refused() {
    let err = array_decl("{1, 2, 3}").expect_err("вход обязан отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-123"), "{err:?}");
    assert!(
        err.message.contains('2') && err.message.contains('3'),
        "текст обязан назвать объявленное и переданное:\n{}",
        err.message
    );
}

/// Недостача - тот же код: усечение и расширение одинаково не определены.
#[test]
fn missing_element_is_refused() {
    let err = array_decl("{1}").expect_err("вход обязан отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-123"), "{err:?}");
}

/// Структура судится тем же правилом: "длина" - число объявленных полей.
#[test]
fn struct_field_count_is_checked() {
    let err = build(
        "struct Gains { kp: u8, ki: u8 }\nvar g: Gains := {2, 3, 4};\nstart Run { ref Run; }\n",
    )
    .expect_err("вход обязан отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-123"), "{err:?}");
    assert!(err.message.contains("Gains"), "{}", err.message);
}

/// **Контроль:** верная длина законна - и у массива, и у структуры.
///
/// Без него "несовпадение отвергается" означало бы "отвергается любой агрегат".
#[test]
fn matching_lengths_are_accepted() {
    array_decl("{1, 2}").expect("верная длина массива законна");
    build("struct Gains { kp: u8, ki: u8 }\nvar g: Gains := {2, 3};\nstart Run { ref Run; }\n")
        .expect("верное число полей законно");
}

/// **Граница:** объявление без агрегата правилом не задето.
#[test]
fn scalar_initializer_is_untouched() {
    build("var v: u8 := 5;\nstart Run { ref Run; }\n").expect("скаляр правилом не задет");
}

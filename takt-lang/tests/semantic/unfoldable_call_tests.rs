//! Невычислимый вызов функции в инициализаторе - `SE-084` с **причиной**.
//!
//! # Границы правила
//!
//! Отказ **узкий**: спрашивается наличие вызова. Вычислимый вызов сворачивается в
//! литерал и остаётся законным, прочие невычислимые формы не затронуты.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;

/// Функция, читающая переменную модели: вычислителю она недоступна - значение
/// переменной известно только в такте.
const READS_VARIABLE: &str = "var base: u8 := 2;\n\
     fn wobble() -> u8 { var t: u8 := base; t := t + 1; return t; }\n";

fn build(src: &str) -> Result<(), takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = parse(src, 0).expect("разбор");
    construct_model(&ast, None, &[]).map(|_| ())
}

/// Предмет: невычислимый вызов отвергается, и сообщение называет **причину**.
///
/// Проверяется именно причина, а не факт отказа: до фичи слова вычислителя существовали
/// и терялись - отказ без них отправил бы автора искать дефект в объявлении, тогда как
/// он в теле функции.
#[test]
fn unfoldable_call_is_rejected_with_reason() {
    let src = format!("{READS_VARIABLE}var scale: u8 := wobble();\nstart Run {{ ref Run; }}\n");
    let err = build(&src).expect_err("вход обязан отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-084"), "{err:?}");
    assert!(
        err.message.contains("base") && err.message.contains("такт"),
        "сообщение обязано назвать причину невычислимости:\n{}",
        err.message
    );
    assert!(
        err.message.contains("always"),
        "сообщение обязано назвать штатный путь:\n{}",
        err.message
    );
}

/// Вызов **внутри выражения** ловится так же: признак рекурсивен.
#[test]
fn unfoldable_call_nested_in_expression_is_rejected() {
    let src = format!("{READS_VARIABLE}var scale: u8 := wobble() + 1;\nstart Run {{ ref Run; }}\n");
    let err = build(&src).expect_err("вложенный вызов обязан отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-084"), "{err:?}");
}

/// Дробное объявление отвечает **тем же** кодом, а не `SE-114`.
///
/// Проверка порядка: `SE-114` судит невычислимую **арифметику**, а голый вызов
/// арифметикой не является - разойдясь, две проверки дали бы на один вход два разных
/// объяснения.
#[test]
fn unfoldable_call_in_fractional_initializer_names_the_call() {
    let src = "var base: q(8, 8) := 2.0;\n\
         fn wobble() -> q(8, 8) { var t: q(8, 8) := base; return t; }\n\
         var scale: q(8, 8) := wobble();\nstart Run { ref Run; }\n";
    let err = build(src).expect_err("вход обязан отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-084"), "{err:?}");
    assert!(
        err.message.contains("зовёт функцию"),
        "дробный случай обязан говорить о вызове, а не об арифметике:\n{}",
        err.message
    );
}

/// **Контроль:** вычислимый вызов остаётся законным.
///
/// Без него "невычислимый отвергается" означало бы "отвергается любой вызов": замер
/// показал, что `var x := seed();` согласован у всех девяти потребителей (значение
/// `3`), и ломать его нельзя.
#[test]
fn foldable_call_is_accepted() {
    build("fn seed() -> u8 { return 3; }\nvar scale: u8 := seed();\nstart Run { ref Run; }\n")
        .expect("вычислимый вызов сворачивается в литерал");
}

/// **Контроль:** та же функция в теле состояния законна - это штатный путь,
/// названный в тексте диагностики.
#[test]
fn same_call_in_body_is_accepted() {
    let src = format!(
        "{READS_VARIABLE}var scale: u8 := 0;\n\
         start Run {{ always {{ scale := wobble(); }} ref Run; }}\n"
    );
    build(&src).expect("вызов в теле — штатный путь");
}

/// **Устройство:** место эмиссии `SE-084` для инициализатора **одно**.
///
/// Пока проверка стояла и в свёртке, и в обходе `validate`, у одного кода
/// было два текста на один вход, и побеждал тот, чья стадия раньше. Мутация
/// (снятие ветви из `validate`) не уронила ни одного теста - ветвь была
/// **мертва**, и это находка, а не догадка.
#[test]
fn initializer_refusal_lives_in_a_single_place() {
    let validate = std::fs::read_to_string("src/semantic/validate/init_undefined_read.rs")
        .expect("обход инициализаторов читается");
    // Ищется эмиссия, а не упоминание: в шапке модуля код назван словами - там сказано,
    // где правило живёт, и запрещать это значит запрещать объяснение.
    assert!(
        !validate.contains(r#".with_code("SE-084")"#),
        "отказ на вызов в инициализаторе обязан жить в свёртке (declaration.rs), \
         иначе один код получает два текста"
    );
}

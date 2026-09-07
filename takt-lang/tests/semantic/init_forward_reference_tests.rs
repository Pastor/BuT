//! Ссылка вперёд в инициализаторе переменной - `SE-109`.
//!
//! Правило: имя в инициализаторе значит начальное значение и ссылается только
//! назад по тексту.
//!
//! Здесь - отказ на запрещённой форме и **отсутствие ложных отказов** на соседних:
//! ссылка назад и ссылка вперёд на константу - законны, а чтение порта отвергается
//! **чужим** правилом (`SE-113`), и граница эта проверяется отдельно.

use takt_lang::diagnostics::Diagnostic;

/// Строит семантическое дерево, возвращая диагностику отказа.
fn construct(src: &str) -> Result<(), Diagnostic> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    takt_lang::semantic::tree::construct_model(&ast, None, &[]).map(|_| ())
}

/// **Ссылка вперёд на переменную - `SE-109` с позицией имени.**
#[test]
fn forward_reference_to_variable_is_rejected() {
    let error = construct(
        "var ahead: u8 := future + 1;\n\
         var future: u8 := 3;\n\
         start Run { }\n",
    )
    .expect_err("ссылка вперёд обязана быть отвергнута");
    assert_eq!(error.code.as_deref(), Some("SE-109"));
    assert!(
        error.message.contains("future"),
        "сообщение обязано назвать переменную: {}",
        error.message
    );
    // Позиция - у имени в инициализаторе, а не у объявления: сообщение в пачке без
    // координаты бесполезно.
    assert!(
        matches!(error.loc, takt_lang::diagnostics::Location::Source(_, start, _) if start == 17),
        "позиция обязана указывать на имя в инициализаторе, получено {:?}",
        error.loc
    );
}

/// **Простейшая форма без арифметики - тот же отказ.**
#[test]
fn bare_forward_reference_is_rejected() {
    let error = construct(
        "var ahead: u8 := future;\n\
         var future: u8 := 3;\n\
         start Run { }\n",
    )
    .expect_err("ссылка вперёд обязана быть отвергнута");
    assert_eq!(error.code.as_deref(), Some("SE-109"));
}

/// **Ссылка назад законна - не сломано.**
#[test]
fn backward_reference_is_accepted() {
    construct(
        "var later: u8 := 7;\n\
         var early: u8 := later + 1;\n\
         start Run { }\n",
    )
    .expect("ссылка назад — законная форма (фича 0192)");
}

/// **Ссылка вперёд на константу законна.**
///
/// У констант разрешение идёт проходами до неподвижной точки и даёт согласованный
/// результат у эталона и целей - проверено пробой. Запрет здесь сломал бы работающие
/// входы.
#[test]
fn forward_reference_to_constant_is_accepted() {
    construct(
        "var value: u8 := LATER + 1;\n\
         const LATER: u8 := 3;\n\
         start Run { }\n",
    )
    .expect("ссылка вперёд на константу — законная форма");
}

/// **Прочие невычислимые инициализаторы по-прежнему приняты.**
///
/// Фича исполняет принятое правило, а не ужесточает язык: чтение порта в инициализаторе
/// **этой** фичей не задето.
///
/// Форма перестала быть законной позже - её отвергает `SE-113`. Граница от этого не
/// исчезла, а стала проверяемой точнее: за ссылку вперёд отвечает своя проверка, и она обязана
/// молчать о порте, чем бы тот ни судился. Мутация "считать порт ссылкой вперёд" валит
/// именно этот тест.
#[test]
fn port_initializer_is_not_a_forward_reference() {
    let error = construct(
        "in sensor: bit;\n\
         var mirror: u8 := sensor;\n\
         start Run { }\n",
    )
    .expect_err("форму отвергает SE-113 (фича 0266)");
    assert_eq!(
        error.code.as_deref(),
        Some("SE-113"),
        "порт судится временем чтения, а не ссылкой вперёд: {error:?}"
    );
}

/// **Граница: имя, объявленное ниже, но внутри выражения под скобками и `as`.**
///
/// Обход выражения обязан спускаться в унарные формы и приведение, иначе проверка
/// ловила бы лишь плоские записи.
#[test]
fn forward_reference_is_found_under_parens_and_cast() {
    for init in [
        "(future) + 1",
        "~future",
        "-future",
        "future as u16",
        "1 + (2 * future)",
    ] {
        let src = format!(
            "var ahead: u16 := {init};\n\
             var future: u8 := 3;\n\
             start Run {{ }}\n"
        );
        let error = construct(&src).expect_err(&format!("форма '{init}' обязана отвергаться"));
        assert_eq!(
            error.code.as_deref(),
            Some("SE-109"),
            "форма '{init}': ожидался SE-109, получено {:?}",
            error.code
        );
    }
}

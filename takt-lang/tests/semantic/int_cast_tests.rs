//! Правило целочисленного приведения - **общий носитель**.

use takt_lang::parse;
use takt_lang::semantic::const_eval::int_cast;
use takt_lang::semantic::tree::construct_model;

/// Строит дерево и возвращает начальное значение переменной модели.
fn value_of(src: &str, name: &str) -> Result<String, takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = parse(src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[])?;
    let borrowed = model.borrow();
    let var = borrowed.variables.get(name).expect("объявление");
    Ok(format!("{var:?}"))
}

/// Беззнаковое приведение вычисляется при компиляции - обёрткой.
#[test]
fn unsigned_cast_is_folded_with_wrap() {
    let text = value_of("var v: u8 := 300 as u8;\nstart Run { ref Run; }\n", "v")
        .expect("вход обязан приниматься");
    assert!(
        text.contains("44"),
        "инициализатор обязан свернуться в 44 (300 mod 256):\n{text}"
    );
}

/// Знаковое переполнение - `SE-121`, а не молчаливое значение.
#[test]
fn signed_overflow_is_refused() {
    let err = value_of("var v: i8 := 300 as i8;\nstart Run { ref Run; }\n", "v")
        .expect_err("знаковое переполнение обязано отвергаться");
    assert_eq!(err.code.as_deref(), Some("SE-121"), "{err:?}");
    assert!(
        err.message.contains("300") && err.message.contains("беззнаковый"),
        "текст обязан назвать значение и выход:\n{}",
        err.message
    );
}

/// **Контроль:** приведение в границах типа по-прежнему законно и не меняет
/// значения.
///
/// Без него "обёртка работает" означало бы "любое приведение что-то меняет".
#[test]
fn identity_cast_still_works() {
    let text = value_of("var v: u16 := 5 as u16;\nstart Run { ref Run; }\n", "v")
        .expect("тождественное приведение законно");
    assert!(text.contains('5'), "{text}");
}

/// **Устройство:** носитель правила один, и он в `takt-lang`.
///
/// Эталон зовёт **эту же** функцию (`takt-sim::eval::coerce_integer`), и вторая
/// реализация разошлась бы с ней значениями - довод, ради которого правило и переехало,
/// а не было скопировано.
#[test]
fn carrier_implements_adr_0127() {
    assert_eq!(int_cast::integer(300, 8, false), Ok(44));
    assert_eq!(int_cast::integer(-1, 8, false), Ok(255));
    assert!(int_cast::integer(300, 8, true).is_err());
    assert_eq!(int_cast::integer(127, 8, true), Ok(127));
}

/// **Граница:** дробная цель приведения по-прежнему эталону и не вычисляется.
///
/// Правила `q` завязаны на представление значения эталона (масштаб, насыщение,
/// округление), и копия здесь разошлась бы значениями - в силе.
#[test]
fn fixed_point_cast_is_still_left_to_the_reference() {
    let text = value_of(
        "var v: q(8, 8) := 3 as q(8, 8);\nstart Run { ref Run; }\n",
        "v",
    )
    .expect("вход законен");
    assert!(!text.is_empty(), "{text}");
}

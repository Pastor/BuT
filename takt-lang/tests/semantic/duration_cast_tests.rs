//! Приведение к `duration` и обратно вычисляется при компиляции.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;

fn value_of(src: &str, name: &str) -> Result<String, takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = parse(src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[])?;
    let borrowed = model.borrow();
    Ok(format!(
        "{:?}",
        borrowed.variables.get(name).expect("объявление")
    ))
}

/// Предмет: число к длительности - мост через миллисекунды.
#[test]
fn integer_to_duration_is_folded() {
    let text = value_of(
        "var v: duration := 250 as duration;\nstart Run { ref Run; }\n",
        "v",
    )
    .expect("вход законен");
    assert!(
        text.contains("250000000"),
        "инициализатор обязан свернуться в наносекунды:\n{text}"
    );
}

/// Обратное направление - длительность к целому - тем же мостом.
#[test]
fn duration_to_integer_is_folded() {
    let text = value_of(
        "const D: duration := 250ms;\nvar v: u32 := D as u32;\nstart Run { ref Run; }\n",
        "v",
    )
    .expect("вход законен");
    assert!(
        text.contains("250"),
        "инициализатор обязан свернуться в миллисекунды:\n{text}"
    );
}

/// **Контроль:** литерал длительности работает как прежде.
///
/// Без него "приведение считается" означало бы "свёртка трогает всё подряд".
#[test]
fn plain_duration_literal_is_unchanged() {
    let text =
        value_of("var v: duration := 250ms;\nstart Run { ref Run; }\n", "v").expect("вход законен");
    assert!(text.contains("250000000"), "{text}");
}

/// **Граница:** миллисекунды, не помещающиеся в целевой тип, судит правило
/// целого (`SE-121`) - второго знания о переносе не заводится.
#[test]
fn overflowing_duration_uses_the_integer_rule() {
    let err = value_of(
        "const D: duration := 250ms;\nvar v: i8 := D as i8;\nstart Run { ref Run; }\n",
        "v",
    )
    .expect_err("250 не помещается в i8");
    assert_eq!(err.code.as_deref(), Some("SE-121"), "{err:?}");
}

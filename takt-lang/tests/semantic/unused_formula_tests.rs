//! Ce13 (`SE-036`) обходит формулы - переменная только в LTL/Guard- формуле не
//! считается мёртвой.
//!
//! До 0082 `semantic/unused.rs` не обходил `ModelNode::formulas`/
//! `StateNode::formulas`, поэтому переменная, используемая **только** в свойстве
//! верификации, давала **ложное** `SE-036`. Дефект обострён (Ce13 теперь печатается
//! пользователю).

use takt_lang::semantic::tree::construct_model;

fn se036_names(src: &str) -> Vec<String> {
    let (ast, _) = takt_lang::parse(src, 0).expect("ошибка разбора");
    let model = construct_model(&ast, None, &[]).expect("ошибка построения");
    takt_lang::unused_variable_warnings(model)
        .into_iter()
        .filter(|w| w.code.as_deref() == Some("SE-036"))
        .map(|w| w.message)
        .collect()
}

/// Переменная только в LTL-формуле состояния (`: [LTL] G flag;`) - не мёртвая.
#[test]
fn var_used_only_in_ltl_formula_no_unused_warning() {
    let names = se036_names(
        r#"
        var flag: bit := 0;
        start S {
            : [LTL] G flag;
            ref Done: true;
        }
        state Done;
    "#,
    );
    assert!(
        !names.iter().any(|m| m.contains("flag")),
        "flag используется в LTL-формуле — SE-036 быть не должно, получено: {names:?}"
    );
}

/// Переменная только в `invariant` (Guard-формуле уровня модели) - не мёртвая.
#[test]
fn var_used_only_in_invariant_no_unused_warning() {
    let names = se036_names(
        r#"
        var g: bit := 0;
        invariant Inv = g = 0;
        start S { ref Done: true; }
        state Done;
    "#,
    );
    assert!(
        !names.iter().any(|m| m.contains("'g'")),
        "g используется в invariant — SE-036 быть не должно, получено: {names:?}"
    );
}

/// Реально неиспользуемая переменная по-прежнему даёт
/// `SE-036` - обход формул не глушит настоящие находки.
#[test]
fn truly_unused_var_still_warns_after_formula_traversal() {
    let names = se036_names(
        r#"
        var dead: bit := 0;
        start S { ref Done: true; }
        state Done;
    "#,
    );
    assert!(
        names.iter().any(|m| m.contains("dead")),
        "реально мёртвая переменная обязана давать SE-036, получено: {names:?}"
    );
}

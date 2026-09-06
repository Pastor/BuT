use takt_lang::parse;
use takt_lang::parser::ast::{InlineFormulaDefine, ModelElement, Statement};

#[test]
fn test_parse_ltl_formula() {
    let src = r#"
model M {
    always {
        :[LTL] X a;
        :[LTL] G (a & b) -> F c;
        :[LTL] a U b;
        :[LTL] a R b;
    }
}
"#;
    let (model, _) = parse(src, 0).unwrap();
    let elements = &model.elements;
    let model_element = elements
        .iter()
        .find(|e| matches!(e, ModelElement::Model(_)))
        .unwrap();
    if let ModelElement::Model(m) = model_element {
        let always = m
            .elements
            .iter()
            .find(|e| matches!(e, ModelElement::NamedBlockCode(_)))
            .unwrap();
        if let ModelElement::NamedBlockCode(nb) = always
            && let Statement::Block { statements, .. } = &nb.statement
        {
            assert_eq!(statements.len(), 4);
            for stmt in statements {
                assert!(matches!(stmt, Statement::InlineFormula(_)));
                if let Statement::InlineFormula(f) = stmt {
                    assert!(matches!(**f, InlineFormulaDefine::Ltl { .. }));
                }
            }
        }
    }
}

#[test]
fn test_parse_guard_formula() {
    let src = r#"
model M {
    always {
        : a > 0;
        :[Guard] b < 10;
    }
}
"#;
    let (model, _) = parse(src, 0).unwrap();
    let elements = &model.elements;
    let model_element = elements
        .iter()
        .find(|e| matches!(e, ModelElement::Model(_)))
        .unwrap();
    if let ModelElement::Model(m) = model_element {
        let always = m
            .elements
            .iter()
            .find(|e| matches!(e, ModelElement::NamedBlockCode(_)))
            .unwrap();
        if let ModelElement::NamedBlockCode(nb) = always
            && let Statement::Block { statements, .. } = &nb.statement
        {
            assert_eq!(statements.len(), 2);
            for stmt in statements {
                assert!(matches!(stmt, Statement::InlineFormula(_)));
                if let Statement::InlineFormula(f) = stmt {
                    assert!(matches!(**f, InlineFormulaDefine::Guard { .. }));
                }
            }
        }
    }
}

#[test]
fn test_parse_mixed_formulas() {
    let src = r#"
model M {
    always {
        :[LTL] X End;
        : i > 0;
    }
}
"#;
    let (model, _) = parse(src, 0).unwrap();
    let elements = &model.elements;
    let model_element = elements
        .iter()
        .find(|e| matches!(e, ModelElement::Model(_)))
        .unwrap();
    if let ModelElement::Model(m) = model_element {
        let always = m
            .elements
            .iter()
            .find(|e| matches!(e, ModelElement::NamedBlockCode(_)))
            .unwrap();
        if let ModelElement::NamedBlockCode(nb) = always
            && let Statement::Block { statements, .. } = &nb.statement
        {
            assert_eq!(statements.len(), 2);

            if let Statement::InlineFormula(f) = &statements[0] {
                assert!(matches!(**f, InlineFormulaDefine::Ltl { .. }));
            } else {
                panic!("Expected LTL formula");
            }

            if let Statement::InlineFormula(f) = &statements[1] {
                assert!(matches!(**f, InlineFormulaDefine::Guard { .. }));
            } else {
                panic!("Expected Guard formula");
            }
        }
    }
}

// ---: семантика LTL (не АСД - риск Р6) ------------------------------

use takt_lang::semantic::tree::construct_model;

fn warnings(src: &str) -> Vec<takt_lang::diagnostics::Diagnostic> {
    let (ast, _) = parse(src, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    takt_lang::ltl_warnings(model)
}

fn codes(ws: &[takt_lang::diagnostics::Diagnostic]) -> Vec<String> {
    ws.iter().filter_map(|w| w.code.clone()).collect()
}

/// A1 (тест против тихой потери): LTL в блоке не выбрасывается - доходит до семантики
/// и даёт SE-055. При возврате `Vec::new()` (прежнее поведение) тест падал бы (0
/// предупреждений).
#[test]
fn ltl_in_block_is_not_silently_dropped() {
    let ws = warnings("var b: u8 := 0; start S { always { : [LTL] G b; } }");
    assert!(
        codes(&ws).contains(&"SE-055".to_string()),
        "LTL в блоке обязана дать SE-055 (не теряться молча): {ws:?}"
    );
}

/// A4 (R3): SE-055 выдаётся с каждого уровня - модель, состояние, блок.
#[test]
fn ltl_se055_from_all_three_levels() {
    let ws = warnings(
        "var b: u8 := 0; : [LTL] F b; \
         start S { : [LTL] G b; always { : [LTL] X b; } }",
    );
    let se055 = codes(&ws).iter().filter(|c| *c == "SE-055").count();
    assert_eq!(se055, 3, "по одному SE-055 на каждый уровень: {ws:?}");
}

/// A5 (R4): неизвестный атом -> SE-056; известный (`var b`) и `true`/`false` - нет.
#[test]
fn ltl_unknown_atom_is_se056() {
    let unknown = warnings("start S { : [LTL] F undefined_thing; }");
    assert!(
        codes(&unknown).contains(&"SE-056".to_string()),
        "неизвестный атом обязан дать SE-056: {unknown:?}"
    );
    let known = warnings("var b: u8 := 0; start S { : [LTL] F b; }");
    assert!(
        !codes(&known).contains(&"SE-056".to_string()),
        "известный атом SE-056 давать не должен: {known:?}"
    );
    let literals = warnings("start S { : [LTL] G true, F false; }");
    assert!(
        !codes(&literals).contains(&"SE-056".to_string()),
        "true/false — не атомы, SE-056 не дают: {literals:?}"
    );
}

/// A2 (паритет уровней): состояние - имя `S` - валидный атом (это состояние).
#[test]
fn ltl_state_name_is_valid_atom() {
    let ws = warnings("start S { : [LTL] F S; } state Done;");
    assert!(
        !codes(&ws).contains(&"SE-056".to_string()),
        "имя состояния — валидный атом: {ws:?}"
    );
}

/// R6/A7: модель без LTL - ни одного предупреждения LTL.
#[test]
fn no_ltl_no_warnings() {
    let ws = warnings("var a: u8 := 0; start S { always { a := a + 1; } }");
    assert!(
        ws.is_empty(),
        "без LTL предупреждений быть не должно: {ws:?}"
    );
}

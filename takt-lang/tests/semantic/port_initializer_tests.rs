//! инициализатор порта - это **адрес**, а не значение.
//!
//! `in BTN: bit at 0x00100000;` - адрес, а не начальное значение бита. вывела
//! `VariableNode::Port` из-под проверки - она остаётся только для `var`/`const` (там
//! инициализатор - действительно значение).
//!
//! Примеры/контрпримеры. Тесты живут отдельным файлом, а не в `semantic_tests.rs`: тот
//! заморожен реестром размера модулей и расти не имеет права.

use takt_lang::semantic::tree::construct_model;
use takt_lang::{diagnostics::Diagnostic, parse, semantic::ModelNode};

/// Разбирает исходник и строит корневой [`ModelNode`] (успех обязателен).
fn build(src: &str) -> ModelNode {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[])
        .expect("ошибка построения семантического дерева")
        .take()
}

/// Разбирает исходник и ожидает семантическую ошибку.
fn build_err(src: &str) -> Diagnostic {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[]).expect_err("ожидалась ошибка")
}

/// R1/A1: голый адрес на `bit`-порту не даёт `SE-035`.
#[test]
fn port_bit_bare_address_no_se035() {
    let node = build("in P: bit at 0x00100000; start Idle;");
    assert!(
        node.search_var("P").is_some(),
        "порт P должен построиться без SE-035"
    );
}

/// R3/A3: `bit := 0` / `bit := 1` на порту валидны (адрес 0/1).
#[test]
fn port_bit_zero_one_valid() {
    assert!(build("in P: bit; start Idle;").search_var("P").is_some());
    assert!(build("in Q: bit; start Idle;").search_var("Q").is_some());
}

/// R6/A1: `bit`-порт и `u8`-порт с голым адресом принимаются одинаково (устранена
/// асимметрия u8/bit).
#[test]
fn port_bit_and_u8_bare_address_symmetric() {
    assert!(
        build("in P: bit at 0x100; start Idle;")
            .search_var("P")
            .is_some(),
        "bit-порт с голым адресом должен приниматься (как u8)"
    );
    assert!(
        build("in P: u8 at 0x100; start Idle;")
            .search_var("P")
            .is_some(),
        "u8-порт с голым адресом принимался и раньше"
    );
}

/// R2/A2: контрпример - не-порт `var: bit := N` (N∉{0,1}) сохраняет `SE-035`.
#[test]
fn non_port_bit_bad_value_still_se035() {
    let err = build_err("var flag: bit := 5; start Idle;");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-035"),
        "не-порт bit := 5 должен давать SE-035, получено: {:?}",
        err.code
    );
}

/// R2/A2: контрпример - `const: bit := N` (N∉{0,1}) тоже сохраняет `SE-035`.
#[test]
fn const_bit_bad_value_still_se035() {
    let err = build_err("const C: bit := 5; start Idle;");
    assert_eq!(
        err.code.as_deref(),
        Some("SE-035"),
        "const bit := 5 должен давать SE-035, получено: {:?}",
        err.code
    );
}

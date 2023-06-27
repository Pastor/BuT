#![allow(non_snake_case)]
mod expression;
pub mod types;
mod utils;

lalrpop_mod!(#[allow(clippy::all)] #[allow(dead_code)] pub grammar);

#[test]
fn test_formula() {
    assert!(grammar::FormulaParser::new().parse("[]was(z2)").is_ok());
    assert!(!grammar::FormulaParser::new().parse("[]was").is_ok());
}

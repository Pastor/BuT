#![allow(non_snake_case)]

pub mod expression;
pub mod types;
mod utils;

lalrpop_mod!(#[allow(clippy::all)] #[allow(dead_code)] pub ltl);

#[test]
fn test_formula() {
    assert!(ltl::FormulaParser::new().parse("[]was(z2)").is_ok());
    assert!(ltl::FormulaParser::new().parse("[]was").is_err());
}

use std::fmt::{Display, Formatter};

use crate::ast::types::Value;

#[derive(PartialEq, Debug)]
pub enum Expr {
    Constant(Value),
    Identifier(String),
    BinaryOp(Box<Expr>, BinaryOpcode, Box<Expr>),
    UnaryOp(UnaryOpcode, Box<Expr>),
    Function(String, Vec<String>),
    Compare(Box<Expr>, CompareOpcode, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Constant(v) => write!(f, "{}", v),
            Expr::Identifier(id) => write!(f, "{}", id),
            Expr::BinaryOp(e1, op, e2) => write!(f, "({} {} {})", e1, op, e2),
            Expr::UnaryOp(op, e) => write!(f, "({}{})", op, e),
            Expr::Function(name, args) => write!(
                f,
                "{}({})",
                name,
                args.into_iter().fold("".to_string(), |acc, x| {
                    if acc.is_empty() {
                        format!("{x}")
                    } else {
                        format!("{acc}, {x}")
                    }
                })
            ),
            Expr::Compare(e1, op, e2) => write!(f, "({} {} {})", e1, op, e2),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum BinaryOpcode {
    Form(FormOpcode),
    Conjunction,
    Disjunction,
}

impl Display for BinaryOpcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOpcode::Form(op) => write!(f, "{}", op),
            BinaryOpcode::Conjunction => write!(f, "&"),
            BinaryOpcode::Disjunction => write!(f, "|"),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum UnaryOpcode {
    Not,
    Form(FormOpcode),
}

impl Display for UnaryOpcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOpcode::Not => write!(f, "!"),
            UnaryOpcode::Form(op) => write!(f, "{}", op),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum FormOpcode {
    Always,
    Eventually,
    Until,
    WeakUntil,
    Release,
    Next,
    Implication,
    Equivalence,
}

impl Display for FormOpcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FormOpcode::Always => write!(f, "[]"),
            FormOpcode::Eventually => write!(f, "<>"),
            FormOpcode::Until => write!(f, "U"),
            FormOpcode::WeakUntil => write!(f, "W"),
            FormOpcode::Release => write!(f, "R"),
            FormOpcode::Next => write!(f, "X"),
            FormOpcode::Implication => write!(f, "->"),
            FormOpcode::Equivalence => write!(f, "<->"),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum CompareOpcode {
    Equivalence,
    NotEquivalence,
    GreaterEquivalence,
    Greater,
    LowerEquivalence,
    Lower,
}

impl Display for CompareOpcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompareOpcode::Equivalence => write!(f, "=="),
            CompareOpcode::NotEquivalence => write!(f, "!="),
            CompareOpcode::GreaterEquivalence => write!(f, ">="),
            CompareOpcode::Greater => write!(f, ">"),
            CompareOpcode::LowerEquivalence => write!(f, "<="),
            CompareOpcode::Lower => write!(f, "<"),
        }
    }
}

#[cfg(test)]
mod test {
    use rstest::*;

    use crate::ast::grammar;

    #[rstest]
    #[case("[]was(z2)", "([]was(z2))")]
    #[case("(is(x1) W was(z1)) & [](was(z2) -> (is(x1) U was(z1)) & was(z1) -> (!is(x1) R was(z2)))",
    "((is(x1) W was(z1)) & ([]((was(z2) -> (is(x1) U was(z1))) & (was(z1) -> ((!is(x1)) R was(z2))))))")]
    fn test_formula_parser(#[case] expression: &str, #[case] expected: &str) {
        let parsed = grammar::FormulaParser::new()
            .parse(expression)
            .expect("Unable to parse expression");
        assert_eq!(expected, parsed.to_string())
    }
}

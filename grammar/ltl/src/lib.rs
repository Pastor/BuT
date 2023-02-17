mod ast;

extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate function_name;
extern crate pad;

use crate::ast::UnaryOp::Unknown;
use crate::ast::{BinaryOp, CompareOp, Node, UnaryOp};
use ::function_name::named;
pub use pest::Parser;
use pad::PadStr;

#[derive(pest_derive::Parser)]
#[grammar = "ltl.pest"]
pub struct LtlParser;

#[named]
pub fn parse(source: &str) -> Result<Vec<Node>, pest::error::Error<Rule>> {
    let mut ast = vec![];
    let pairs = LtlParser::parse(Rule::grammar, source)?;
    for pair in pairs {
        if let Rule::grammar = pair.as_rule() {
            ast.push(build_ast_from_formula(0, pair.into_inner().next().unwrap()));
        }
    }
    Ok(ast)
}

#[named]
fn build_ast_from_formula(pad: i8, pair: pest::iterators::Pair<Rule>) -> Node {
    let mut pairs = pair.into_inner();
    let conj = build_ast_from_conjunction(pad + 1, pairs.next().unwrap());
    let mut terms = Vec::new();
    terms.push(Box::new(conj));
    loop {
        let pair_buf = pairs.next();
        if pair_buf != None {
            let conj = build_ast_from_conjunction(pad + 1, pair_buf.unwrap());
            terms.push(Box::new(conj));
        } else {
            break;
        }
    }
    Node::TermFormula(terms)
}

#[named]
fn build_ast_from_conjunction(pad: i8, pair: pest::iterators::Pair<Rule>) -> Node {
    let mut pairs = pair.into_inner();
    let mut next_ltl = pairs.next().unwrap();
    let ltl = build_ast_from_ltl(pad + 1, next_ltl);
    let mut terms = Vec::new();
    terms.push(Box::new(ltl));
    loop {
        let pair_buf = pairs.next();
        if pair_buf != None {
            let pair_buf = pair_buf.unwrap();
            let ltl = build_ast_from_ltl(pad + 1, pair_buf);
            terms.push(Box::new(ltl));
        } else {
            break;
        }
    }
    Node::Conjunction(terms)
}

#[named]
fn build_ast_from_ltl(pad: i8, pair: pest::iterators::Pair<Rule>) -> Node {
    let mut pairs = pair.into_inner();
    let part = pairs.next().unwrap();
    let un_term = build_ast_from_un_term(pad + 1, part);
    let mut terms = Vec::new();
    loop {
        let pair_buf = pairs.next();
        if pair_buf != None {
            let pair = pair_buf.unwrap();
            let op = parse_binary_op(pair);
            if let BinaryOp::Unknown(text) = op {
                panic!("LTL. Unknown binary operation {:?}", text);
            }
            let un_term = build_ast_from_un_term(pad + 1, pairs.next().unwrap());
            terms.push((op, Box::new(un_term)));
        } else {
            break;
        }
    }
    Node::TermLtl {
        term: Box::new(un_term),
        terms,
    }
}

#[named]
fn build_ast_from_un_term(pad: i8, pair: pest::iterators::Pair<Rule>) -> Node {
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();
    match pair.as_rule() {
        Rule::unOp => {
            let op = parse_unary_op(pair);
            let term = build_ast_from_term(pad + 1, pairs.next().unwrap());
            Node::TermOp {
                op,
                term: Box::new(term),
            }
        }
        Rule::unTerm => {
            build_ast_from_un_term(pad + 1, pair)
        }
        _ => {
            build_ast_from_term(pad + 1, pair)
        }
    }
}

#[named]
fn build_ast_from_term(pad: i8, pair: pest::iterators::Pair<Rule>) -> Node {
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();
    match pair.as_rule() {
        Rule::ConstBool => Node::ConstBool(pair.as_str().parse().unwrap()),
        Rule::ConstNum => {
            let num: i32 = pairs.next().unwrap().as_str().parse().unwrap();
            let op = parse_compare_op(pairs.next().unwrap());
            Node::TermNum {
                num,
                op,
                func: Box::new(build_ast_from_func(pad + 1, pairs.next().unwrap())),
            }
        }
        Rule::formula => {
            build_ast_from_formula(pad + 1, pair)
        }
        Rule::func => {
            let func = build_ast_from_func(pad + 1, pair);
            let op = pairs.next();
            if op != None {
                let op = parse_compare_op(op.unwrap());
                let next = pairs.next().unwrap();
                match next.as_rule() {
                    Rule::func => Node::TermFuncOpFunc {
                        func: Box::new(func),
                        op,
                        opf: Box::new(build_ast_from_func(pad + 1, next)),
                    },
                    Rule::ConstNum => Node::TermFuncOpNum {
                        func: Box::new(func),
                        op,
                        num: next.as_str().parse().unwrap(),
                    },
                    unknown => panic!("Unknown: {:?}", unknown),
                }
            } else {
                Node::TermFunc(Box::new(func))
            }
        }
        unknown => panic!("UnImplement: {:?}", unknown),
    }
}

#[named]
fn build_ast_from_func(pad: i8, pair: pest::iterators::Pair<Rule>) -> Node {
    let mut pairs = pair.into_inner();
    let name = pairs.next().unwrap().as_str();
    let mut args = Vec::new();
    loop {
        let pair_buf = pairs.next();
        if pair_buf != None {
            args.push(pair_buf.unwrap().as_str().to_string());
        } else {
            // println!("{: >26}:{:0>3} - {}{}", function_name!(), line!(), padding(pad), "Function complete");
            break;
        }
    }
    Node::Function {
        name: name.to_string(),
        args,
    }
}

fn padding(pad: i8) -> String {
    "".pad_to_width(pad as usize)
}

fn parse_un_term_op(op: pest::iterators::Pair<Rule>, term: Node) -> Node {
    Node::TermOp {
        op: match op.as_str() {
            "[]" => UnaryOp::Always,
            "<>" => UnaryOp::Eventually,
            "X" => UnaryOp::Next,
            "!" => UnaryOp::Not,
            _ => unreachable!(),
        },
        term: Box::new(term),
    }
}

fn parse_unary_op(op: pest::iterators::Pair<Rule>) -> UnaryOp {
    match op.as_str() {
        "[]" => UnaryOp::Always,
        "<>" => UnaryOp::Eventually,
        "X" => UnaryOp::Next,
        "!" => UnaryOp::Not,
        _ => Unknown,
    }
}

fn parse_binary_op(op: pest::iterators::Pair<Rule>) -> BinaryOp {
    match op.as_str() {
        "U" => BinaryOp::Until,
        "W" => BinaryOp::WeakUntil,
        "R" => BinaryOp::Release,
        "->" => BinaryOp::Implication,
        "<->" => BinaryOp::Equivalence,
        unk => BinaryOp::Unknown(unk.to_string()),
    }
}

fn parse_compare_op(op: pest::iterators::Pair<Rule>) -> CompareOp {
    match op.as_str() {
        "==" => CompareOp::Equal,
        "!=" => CompareOp::NoEqual,
        ">=" => CompareOp::GTE,
        ">" => CompareOp::GT,
        "<=" => CompareOp::LTE,
        "<" => CompareOp::LT,
        unk => CompareOp::Unknown(unk.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    const LTL: &'static str =
        "(is(x1) W was(z1)) & [](was(z2) -> (is(x1) U was(z1)) & was(z1) -> (!is(x1) R was(z2)))";

    #[test]
    fn basics() {
        assert!(parse("0").is_err());
    }

    #[test]
    fn test_parse() {
        let result = parse(LTL);
        assert!(result.is_ok());
        let ast = result.unwrap();
        assert_eq!(ast.len(), 1);
        println!("{}", ast.iter().next().unwrap())
    }

    #[test]
    fn test_parse_ltl() {
        let pairs = LtlParser::parse(Rule::grammar, LTL).unwrap_or_else(|e| panic!("{}", e));
        for pair in pairs {
            println!("Rule   : {:?}", pair.as_rule());
            println!("Span   : {:?}", pair.as_span());
            println!("Text   : {}", pair.as_str());
            for inner_pair in pair.into_inner() {
                match inner_pair.as_rule() {
                    Rule::EOI => (),
                    Rule::Name => println!("Name   : {}", inner_pair.as_str()),
                    Rule::formula => println!("Formula: {}", inner_pair.as_str()),
                    _ => println!("_{:?}: {}", inner_pair.as_rule(), inner_pair.as_str()),
                };
            }
        }
    }
}

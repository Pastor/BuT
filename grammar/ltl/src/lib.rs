mod ast;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use crate::ast::UnaryOp::Unknown;
use crate::ast::{BinaryOp, CompareOp, Node, UnaryOp};
pub use pest::Parser;

#[derive(pest_derive::Parser)]
#[grammar = "ltl.pest"]
pub struct LtlParser;

pub fn parse(source: &str) -> std::result::Result<Vec<Node>, pest::error::Error<Rule>> {
    let mut ast = vec![];
    let pairs = LtlParser::parse(Rule::grammar, source)?;
    for pair in pairs {
        if let Rule::grammar = pair.as_rule() {
            let text = pair.to_string();
            println!("{}", text);
            ast.push(build_ast_from_formula(pair.into_inner().next().unwrap()));
        }
    }
    Ok(ast)
}

fn build_ast_from_formula(pair: pest::iterators::Pair<Rule>) -> Node {
    match pair.as_rule() {
        Rule::formula => {
            let mut pair = pair.into_inner();
            let text = pair.to_string();
            let conj = build_ast_from_conj(pair.next().unwrap());
            let mut terms = Vec::new();
            terms.push(Box::new(conj));
            loop {
                let pair_buf = pair.next();
                if pair_buf != None {
                    let op = pair_buf.unwrap();
                    if op.as_str() != "|" {
                        panic!("Only | operation was present")
                    }
                    let conj = build_ast_from_conj(pair.next().unwrap());
                    terms.push(Box::new(conj));
                } else {
                    break;
                }
            }
            Node::TermFormula(terms)
        }
        Rule::conj => {
            let mut pair = pair.into_inner();
            let ltl = build_ast_from_ltl(pair.next().unwrap());
            let mut ltls = Vec::new();
            ltls.push(Box::new(ltl));
            loop {
                let pair_buf = pair.next();
                if pair_buf != None {
                    let op = pair_buf.unwrap();
                    if op.as_str() != "&" {
                        panic!("Only & operation was present")
                    }
                    let ltl = build_ast_from_ltl(pair.next().unwrap());
                    ltls.push(Box::new(ltl));
                } else {
                    break;
                }
            }
            Node::Conjunction(ltls)
        }
        Rule::ltlForm => {
            let mut pair = pair.into_inner();
            let un = build_ast_from_un_term(pair.next().unwrap());
            Node::Unknown(un.to_string())
        }
        unknown => panic!("Unknown formula: {:?}", unknown),
    }
}

fn build_ast_from_conj(pair: pest::iterators::Pair<Rule>) -> Node {
    match pair.as_rule() {
        Rule::conj => {
            let text = pair.to_string();
            build_ast_from_conj(pair.into_inner().next().unwrap())
        }
        Rule::ltlForm => {
            let text1 = pair.to_string();
            let mut pair = pair.into_inner();
            let text2 = pair.to_string();
            build_ast_from_ltl(pair.next().unwrap())
        }
        unknown => panic!("Unknown conj: {:?}", unknown),
    }
}

fn build_ast_from_ltl(pair: pest::iterators::Pair<Rule>) -> Node {
    match pair.as_rule() {
        Rule::ltlForm => {
            let text = pair.to_string();
            build_ast_from_ltl(pair.into_inner().next().unwrap())
        }
        Rule::unTerm => {
            let text1 = pair.to_string();
            let mut pair = pair.into_inner();
            let text2 = pair.to_string();
            let un_term = build_ast_from_un_term(pair.next().unwrap());
            let text3 = pair.to_string();
            let mut terms = Vec::new();
            loop {
                let pair_buf = pair.next();
                if pair_buf != None {
                    let op = parse_binary_op(pair_buf.unwrap());
                    let un_term = build_ast_from_ltl(pair.next().unwrap());
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
        unknown => panic!("Unknown unTerm: {:?}", unknown),
    }
}

fn build_ast_from_un_term(pair: pest::iterators::Pair<Rule>) -> Node {
    match pair.as_rule() {
        Rule::unOp => {
            let text = pair.to_string();
            let mut pair = pair.into_inner();
            let text = pair.to_string();
            let op = pair.next().unwrap();
            let text = pair.to_string();
            let term = build_ast_from_term(pair.next().unwrap());
            parse_un_term_op(op, term)
        }
        _ => {
            let text = pair.to_string();
            build_ast_from_term(pair.into_inner().next().unwrap())
        }
    }
}

fn build_ast_from_term(pair: pest::iterators::Pair<Rule>) -> Node {
    match pair.as_rule() {
        Rule::term => {
            let text = pair.to_string();
            build_ast_from_term(pair.into_inner().next().unwrap())
        }
        Rule::ConstBool => Node::ConstBool(pair.as_str().parse().unwrap()),
        Rule::ConstNum => {
            let mut pair = pair.into_inner();
            let num: i32 = pair.as_str().parse().unwrap();
            let op = parse_compare_op(pair.next().unwrap());
            Node::TermNum {
                num,
                op,
                func: Box::new(build_ast_from_func(pair.next().unwrap())),
            }
        }
        Rule::formula => {
            let text = pair.to_string();
            build_ast_from_formula(pair.into_inner().next().unwrap())
        }
        Rule::func => {
            let mut pair = pair.into_inner();
            let name = pair.next().unwrap().as_str();
            let mut args = Vec::new();
            loop {
                let pair_buf = pair.next();
                if pair_buf != None {
                    args.push(pair_buf.unwrap().as_str().to_string());
                } else {
                    break;
                }
            }
            let func = Node::Function {
                name: name.to_string(),
                args,
            };
            let op = pair.next();
            if op != None {
                let op = parse_compare_op(op.unwrap());
                let next = pair.next().unwrap();
                match next.as_rule() {
                    Rule::func => Node::TermFuncOpFunc {
                        func: Box::new(func),
                        op,
                        opf: Box::new(build_ast_from_func(next)),
                    },
                    Rule::ConstNum => Node::TermFuncOpNum {
                        func: Box::new(func),
                        op,
                        num: pair.next().unwrap().as_str().parse().unwrap(),
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

fn build_ast_from_func(pair: pest::iterators::Pair<Rule>) -> Node {
    let text = pair.to_string();
    match pair.as_rule() {
        Rule::func => {
            let mut pair = pair.into_inner();
            let name = pair.next().unwrap().as_str();
            let mut args = Vec::new();
            loop {
                let pair_buf = pair.next();
                if pair_buf != None {
                    args.push(pair_buf.unwrap().as_str().to_string());
                } else {
                    break;
                }
            }
            Node::Function {
                name: name.to_string(),
                args,
            }
        }
        unknown => {
            let text = pair.to_string();
            panic!("CantParse: {:?}", text)
        }
    }
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
        _ => UnaryOp::Unknown,
    }
}

fn parse_binary_op(op: pest::iterators::Pair<Rule>) -> BinaryOp {
    match op.as_str() {
        "U" => BinaryOp::Until,
        "W" => BinaryOp::WeakUntil,
        "R" => BinaryOp::Release,
        "->" => BinaryOp::Implication,
        "<->" => BinaryOp::Equivalence,
        _ => BinaryOp::Unknown,
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

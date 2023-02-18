use std::fmt;
use std::fmt::Debug;
use std::ops::Add;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    // []
    Always,
    // <>
    Eventually,
    // X
    Next,
    // !
    Not,
    Unknown,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match &self {
            UnaryOp::Always => write!(f, "[]"),
            UnaryOp::Eventually => write!(f, "<>"),
            UnaryOp::Next => write!(f, "X"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    // U
    Until,
    // W
    WeakUntil,
    // R
    Release,
    // ->
    Implication,
    // <->
    Equivalence,
    Unknown(String),
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match &self {
            BinaryOp::Until => write!(f, "U"),
            BinaryOp::WeakUntil => write!(f, "W"),
            BinaryOp::Release => write!(f, "R"),
            BinaryOp::Implication => write!(f, "->"),
            BinaryOp::Equivalence => write!(f, "<->"),
            BinaryOp::Unknown(text) => write!(f, "Unknown_{}", text),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompareOp {
    // ==
    Equal,
    // !=
    NoEqual,
    // >=
    GTE,
    // >
    GT,
    // <=
    LTE,
    // <
    LT,
    Unknown(String),
}

impl fmt::Display for CompareOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match &self {
            CompareOp::Equal => write!(f, "=="),
            CompareOp::NoEqual => write!(f, "!="),
            CompareOp::GTE => write!(f, ">="),
            CompareOp::GT => write!(f, ">"),
            CompareOp::LTE => write!(f, "<="),
            CompareOp::LT => write!(f, "<"),
            CompareOp::Unknown(text) => write!(f, "Unknown_{}", text),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Node {
    ConstNum(i32),
    ConstBool(bool),
    Ident(String),
    TermFormula(Vec<Box<Node>>),
    TermConst(Box<Node>),
    TermFunc(Box<Node>),
    TermFuncOpFunc {
        func: Box<Node>,
        op: CompareOp,
        opf: Box<Node>,
    },
    TermFuncOpNum {
        func: Box<Node>,
        op: CompareOp,
        num: i32,
    },
    TermOp {
        op: UnaryOp,
        term: Box<Node>,
    },
    TermNum {
        num: i32,
        op: CompareOp,
        func: Box<Node>,
    },
    TermLtl {
        term: Box<Node>,
        terms: Vec<(BinaryOp, Box<Node>)>,
    },
    Function {
        name: String,
        args: Vec<String>,
    },
    Conjunction(Vec<Box<Node>>),
    Unknown(String),
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.to_str())
    }
}

impl Node {
    pub(crate) fn to_str(&self) -> String {
        match &self {
            Node::ConstNum(n) => format!("{}", n),
            Node::ConstBool(n) => format!("{}", n),
            Node::Ident(n) => format!("{}", n),
            Node::TermFormula(n) => format!("{}", self.join(n.clone(), " | ")),
            Node::TermConst(n) => format!("{}", n.to_str()),
            Node::TermFunc(func) => format!("{}", func.to_str()),
            Node::TermFuncOpFunc { func, op, opf } => format!("{} {} {}", func.to_str(), op, opf.to_str()),
            Node::TermFuncOpNum { func, op, num } => format!("{} {} {}", func.to_str(), op, num),
            Node::TermOp { op, term } => format!("{} {}", op, term.to_str()),
            Node::TermNum { num, op, func } => format!("{} {} {}", num, op, func.to_str()),
            Node::Function { name, args } => format!("{}({}) ", name, args.join(", ")),
            Node::Conjunction(ltl) => format!("{}", self.join(ltl.clone(), " & ")),
            Node::TermLtl { term, terms } => format!("{} {}", term.to_str(), self.join_op(terms.clone())),
            Node::Unknown(text) => format!("{}", text),
        }
    }
    fn join(&self, data: Vec<Box<Node>>, delim: &'_ str) -> String {
        let mut result = String::new();
        for (pos, node) in data.iter().enumerate() {
            if pos > 0 {
                result = result.add(delim);
            }
            result = result.add(&*node.to_str().as_str());
        }
        result
    }

    fn join_op(&self, data: Vec<(BinaryOp, Box<Node>)>) -> String {
        let mut result = String::new();
        for (pos, &ref node) in data.iter().enumerate() {
            if pos > 0 {
                result = result.add(" ");
            }
            result = result.add(node.0.to_string().as_str()).add(" ")
                .add(node.1.to_str().as_str());
        }
        result
    }
}

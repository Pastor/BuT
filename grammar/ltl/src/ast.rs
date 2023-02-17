use std::fmt;

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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    Unknown,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match &self {
            BinaryOp::Until => write!(f, "U"),
            BinaryOp::WeakUntil => write!(f, "W"),
            BinaryOp::Release => write!(f, "R"),
            BinaryOp::Implication => write!(f, "->"),
            BinaryOp::Equivalence => write!(f, "<->"),
            BinaryOp::Unknown => write!(f, "unknown"),
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
        match &self {
            Node::ConstNum(n) => write!(f, "{}", n),
            Node::ConstBool(n) => write!(f, "{}", n),
            Node::Ident(n) => write!(f, "{}", n),
            Node::TermFormula(n) => write!(f, "{:?}", n),
            Node::TermConst(n) => write!(f, "{}", *n),
            Node::TermFunc(func) => write!(f, "{}", func),
            Node::TermFuncOpFunc { func, op, opf } => write!(f, "{}{}{}", func, op, opf),
            Node::TermFuncOpNum { func, op, num } => write!(f, "{}{}{}", func, op, num),
            Node::TermOp { op, term } => write!(f, "{} {}", op, term),
            Node::TermNum { num, op, func } => write!(f, "{} {} {}", num, op, func),
            Node::Function { name, args } => write!(f, "{}( {:?} ) ", name, args),
            Node::Conjunction(ltl) => write!(f, "{:?}", ltl),
            Node::TermLtl { term, terms } => write!(f, "{:?} {:?}", term, terms),
            Node::Unknown(text) => write!(f, "{}", text),
        }
    }
}

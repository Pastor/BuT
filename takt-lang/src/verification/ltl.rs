use std::fmt;
use std::rc::Rc;

/// Формула линейной темпоральной логики (LTL).
///
/// Поддерживаются все стандартные операторы LTL:
/// - пропозициональные: [`True`](Ltl::True), [`False`](Ltl::False), [`Atom`](Ltl::Atom),
///   [`Not`](Ltl::Not), [`And`](Ltl::And), [`Or`](Ltl::Or), [`Implies`](Ltl::Implies);
/// - темпоральные: [`Next`](Ltl::Next), [`Finally`](Ltl::Finally),
///   [`Globally`](Ltl::Globally), [`Until`](Ltl::Until), [`Release`](Ltl::Release).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Ltl {
    /// Логическая истина (`true`).
    True,
    /// Логическая ложь (`false`).
    False,
    /// Пропозициональный атом - имя переменной или состояния.
    Atom(String),
    /// Логическое отрицание (`!φ`).
    Not(Rc<Ltl>),
    /// Логическая конъюнкция (`φ & ψ`).
    And(Rc<Ltl>, Rc<Ltl>),
    /// Логическая дизъюнкция (`φ | ψ`).
    Or(Rc<Ltl>, Rc<Ltl>),
    /// Импликация (`φ -> ψ`).
    Implies(Rc<Ltl>, Rc<Ltl>),
    /// Темпоральный оператор "следующий шаг" (`X φ`).
    Next(Rc<Ltl>),
    /// Темпоральный оператор "в некоторый будущий момент" (`F φ`).
    Finally(Rc<Ltl>),
    /// Темпоральный оператор "всегда" (`G φ`).
    Globally(Rc<Ltl>),
    /// Темпоральный оператор "пока" (`φ U ψ`): `φ` выполнено до тех пор, пока не
    /// выполнится `ψ`.
    Until(Rc<Ltl>, Rc<Ltl>),
    /// Темпоральный оператор "освобождение" (`φ R ψ`): двойственный к `Until`.
    Release(Rc<Ltl>, Rc<Ltl>),
}

impl fmt::Display for Ltl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ltl::True => write!(f, "true"),
            Ltl::False => write!(f, "false"),
            Ltl::Atom(a) => write!(f, "{}", a),
            Ltl::Not(a) => write!(f, "!{}", a),
            Ltl::And(a, b) => write!(f, "({} & {})", a, b),
            Ltl::Or(a, b) => write!(f, "({} | {})", a, b),
            Ltl::Implies(a, b) => write!(f, "({} -> {})", a, b),
            Ltl::Next(a) => write!(f, "X {}", a),
            Ltl::Finally(a) => write!(f, "F {}", a),
            Ltl::Globally(a) => write!(f, "G {}", a),
            Ltl::Until(a, b) => write!(f, "({} U {})", a, b),
            Ltl::Release(a, b) => write!(f, "({} R {})", a, b),
        }
    }
}

// ============================================================================
// 2. Парсер (рекурсивный спуск, приоритеты: ! > X,F,G > U,R > & > | > ->)
// ============================================================================

fn parse_atom(s: &str) -> (&str, Ltl) {
    let s = s.trim_start();
    if let Some(rest) = s.strip_prefix("true") {
        (rest, Ltl::True)
    } else if let Some(rest) = s.strip_prefix("false") {
        (rest, Ltl::False)
    } else if let Some(c) = s.chars().next() {
        if c.is_ascii_lowercase() {
            let name = c.to_string();
            (&s[1..], Ltl::Atom(name))
        } else {
            panic!("ожидался атом или true/false, найдено: {}", s);
        }
    } else {
        panic!("неожиданный конец строки");
    }
}

fn parse_paren(s: &str) -> (&str, Ltl) {
    let s = s.trim_start();
    if let Some(rest) = s.strip_prefix('(') {
        let (rest, inner) = parse_implies(rest);
        let rest = rest.trim_start();
        if let Some(rest) = rest.strip_prefix(')') {
            (rest, inner)
        } else {
            panic!("закрывающая скобка не найдена");
        }
    } else {
        parse_atom(s)
    }
}

fn parse_unary(s: &str) -> (&str, Ltl) {
    let s = s.trim_start();
    if let Some(rest) = s.strip_prefix('!') {
        let (rest, inner) = parse_unary(rest);
        (rest, Ltl::Not(Rc::new(inner)))
    } else if let Some(rest) = s.strip_prefix('X') {
        let (rest, inner) = parse_unary(rest);
        (rest, Ltl::Next(Rc::new(inner)))
    } else if let Some(rest) = s.strip_prefix('F') {
        let (rest, inner) = parse_unary(rest);
        (rest, Ltl::Finally(Rc::new(inner)))
    } else if let Some(rest) = s.strip_prefix('G') {
        let (rest, inner) = parse_unary(rest);
        (rest, Ltl::Globally(Rc::new(inner)))
    } else {
        parse_paren(s)
    }
}

fn parse_until_release(s: &str) -> (&str, Ltl) {
    let (mut rest, mut left) = parse_unary(s);
    loop {
        let trimmed = rest.trim_start();
        if let Some(r) = trimmed.strip_prefix('U') {
            let (r, right) = parse_unary(r);
            rest = r;
            left = Ltl::Until(Rc::new(left), Rc::new(right));
        } else if let Some(r) = trimmed.strip_prefix('R') {
            let (r, right) = parse_unary(r);
            rest = r;
            left = Ltl::Release(Rc::new(left), Rc::new(right));
        } else {
            break;
        }
    }
    (rest, left)
}

fn parse_and(s: &str) -> (&str, Ltl) {
    let (mut rest, mut left) = parse_until_release(s);
    loop {
        let trimmed = rest.trim_start();
        if let Some(r) = trimmed.strip_prefix('&') {
            let (r, right) = parse_until_release(r);
            rest = r;
            left = Ltl::And(Rc::new(left), Rc::new(right));
        } else {
            break;
        }
    }
    (rest, left)
}

fn parse_or(s: &str) -> (&str, Ltl) {
    let (mut rest, mut left) = parse_and(s);
    loop {
        let trimmed = rest.trim_start();
        if let Some(r) = trimmed.strip_prefix('|') {
            let (r, right) = parse_and(r);
            rest = r;
            left = Ltl::Or(Rc::new(left), Rc::new(right));
        } else {
            break;
        }
    }
    (rest, left)
}

fn parse_implies(s: &str) -> (&str, Ltl) {
    let (mut rest, mut left) = parse_or(s);
    loop {
        let trimmed = rest.trim_start();
        if let Some(r) = trimmed.strip_prefix("->") {
            let (r, right) = parse_or(r);
            rest = r;
            left = Ltl::Implies(Rc::new(left), Rc::new(right));
        } else {
            break;
        }
    }
    (rest, left)
}

/// Разбирает LTL-формулу из строки.
///
/// Парсер реализован методом рекурсивного спуска с приоритетами: `!`, `X`, `F`, `G` >
/// `U`, `R` > `&` > `|` > `->`.
///
/// # Паника
/// Паникует при синтаксических ошибках (непарные скобки, неизвестные символы,
/// остаток непарсенного ввода).
pub fn parse_ltl(input: &str) -> Ltl {
    let (rest, f) = parse_implies(input);
    if !rest.trim().is_empty() {
        panic!("остаток строки после парсинга: {}", rest);
    }
    f
}

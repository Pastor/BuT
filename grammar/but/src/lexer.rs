use std::str::FromStr;
use std::{fmt, str::CharIndices};

use itertools::{peek_nth, PeekNth};
use phf::phf_map;
use thiserror::Error;
use unicode_xid::UnicodeXID;

use crate::ast::{Comment, Loc};

/// A spanned [Token].
pub type Spanned<'a> = (usize, Token<'a>, usize);

/// [Lexer]'s Result type.
pub type Result<'a, T = Spanned<'a>, E = LexicalError> = std::result::Result<T, E>;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[allow(missing_docs)]
pub enum Token<'input> {
    Identifier(&'input str),
    /// `(unicode, literal)`
    StringLiteral(bool, &'input str),
    AddressLiteral(&'input str),
    HexLiteral(&'input str),
    Number(i64),
    RationalNumber(&'input str, bool),
    HexNumber(&'input str),
    Divide,
    Library,
    Interface,
    Function,
    Pragma,
    Import,

    Struct,
    Enum,
    Type,

    Do,
    Continue,
    Break,

    Emit,
    Return,

    String,

    Sharp,
    Semicolon,
    Comma,
    OpenParenthesis,
    CloseParenthesis,
    OpenCurlyBrace,
    CloseCurlyBrace,

    BitwiseOr,
    BitwiseOrAssign,
    Or,

    BitwiseAnd,
    BitwiseAndAssign,
    And,

    AddAssign,
    Increment,
    Add,

    SubtractAssign,
    Decrement,
    Subtract,

    MulAssign,
    Mul,
    Power,
    DivideAssign,
    ModuloAssign,
    Modulo,

    Equal,
    Assign,

    NotEqual,
    Not,

    True,
    False,
    Else,
    For,
    While,
    If,

    ShiftRight,
    ShiftRightAssign,
    Less,
    LessEqual,

    ShiftLeft,
    ShiftLeftAssign,
    More,
    MoreEqual,

    Member,
    Colon,
    OpenBracket,
    CloseBracket,

    As,
    Is,

    Assembly,

    Annotation(&'input str),

    Constant,
    Writable,
    Readable,
    Portable,
    External,
}

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Identifier(id) => write!(f, "{id}"),
            Token::StringLiteral(false, s) => write!(f, "\"{s}\""),
            Token::StringLiteral(true, s) => write!(f, "unicode\"{s}\""),
            Token::HexLiteral(hex) => write!(f, "{hex}"),
            Token::AddressLiteral(address) => write!(f, "{address}"),
            Token::Number(n) => write!(f, "{n}"),
            Token::RationalNumber(n, d) => {
                if *d {
                    write!(f, "-")?;
                }
                write!(f, "{n}")
            }
            Token::HexNumber(n) => write!(f, "{n}"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Sharp => write!(f, "#"),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
            Token::OpenCurlyBrace => write!(f, "{{"),
            Token::CloseCurlyBrace => write!(f, "}}"),
            Token::BitwiseOr => write!(f, "|"),
            Token::BitwiseOrAssign => write!(f, "|="),
            Token::Or => write!(f, "||"),
            Token::BitwiseAnd => write!(f, "&"),
            Token::BitwiseAndAssign => write!(f, "&="),
            Token::And => write!(f, "&&"),
            Token::AddAssign => write!(f, "+="),
            Token::Increment => write!(f, "++"),
            Token::Add => write!(f, "+"),
            Token::SubtractAssign => write!(f, "-="),
            Token::Decrement => write!(f, "--"),
            Token::Subtract => write!(f, "-"),
            Token::MulAssign => write!(f, "*="),
            Token::Mul => write!(f, "*"),
            Token::Power => write!(f, "**"),
            Token::Divide => write!(f, "/"),
            Token::DivideAssign => write!(f, "/="),
            Token::ModuloAssign => write!(f, "%="),
            Token::Modulo => write!(f, "%"),
            Token::Equal => write!(f, "=="),
            Token::Assign => write!(f, "="),
            Token::NotEqual => write!(f, "!="),
            Token::Not => write!(f, "!"),
            Token::ShiftLeft => write!(f, "<<"),
            Token::ShiftLeftAssign => write!(f, "<<="),
            Token::More => write!(f, ">"),
            Token::MoreEqual => write!(f, ">="),
            Token::Member => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::ShiftRightAssign => write!(f, "<<="),
            Token::ShiftRight => write!(f, "<<"),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::String => write!(f, "string"),
            Token::Library => write!(f, "library"),
            Token::Interface => write!(f, "interface"),
            Token::Function => write!(f, "function"),
            Token::Pragma => write!(f, "pragma"),
            Token::Import => write!(f, "import"),
            Token::Struct => write!(f, "struct"),
            Token::Enum => write!(f, "enum"),
            Token::Type => write!(f, "type"),
            Token::Constant => write!(f, "const"),
            Token::Do => write!(f, "do"),
            Token::Continue => write!(f, "continue"),
            Token::Break => write!(f, "break"),
            Token::Emit => write!(f, "emit"),
            Token::Return => write!(f, "return"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Else => write!(f, "else"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::If => write!(f, "if"),
            Token::As => write!(f, "as"),
            Token::Is => write!(f, "is"),
            Token::Assembly => write!(f, "assembly"),
            Token::Annotation(name) => write!(f, "@{name}"),
            Token::External => write!(f, "external"),
            Token::Readable => write!(f, "let"),
            Token::Writable => write!(f, "mut"),
            Token::Portable => write!(f, "pio"),
        }
    }
}

/// Custom Solidity lexer.
///
/// # Examples
///
/// ```
/// use but::lexer::{Lexer, Token};
///
/// let source = "int number = 0;";
/// let mut comments = Vec::new();
/// let mut errors = Vec::new();
/// let mut lexer = Lexer::new(source, 0, &mut comments, &mut errors);
///
/// let mut next_token = || lexer.next().map(|(_, token, _)| token);
/// assert_eq!(next_token(), Some(Token::Identifier("int")));
/// assert_eq!(next_token(), Some(Token::Identifier("number")));
/// assert_eq!(next_token(), Some(Token::Assign));
/// assert_eq!(next_token(), Some(Token::Number(0i64)));
/// assert_eq!(next_token(), Some(Token::Semicolon));
/// assert_eq!(next_token(), None);
/// assert!(errors.is_empty());
/// assert!(comments.is_empty());
/// ```
#[derive(Debug)]
pub struct Lexer<'input> {
    input: &'input str,
    chars: PeekNth<CharIndices<'input>>,
    comments: &'input mut Vec<Comment>,
    file_no: usize,
    last_tokens: [Option<Token<'input>>; 2],
    /// The mutable reference to the error vector.
    pub errors: &'input mut Vec<LexicalError>,
}

/// An error thrown by [Lexer].
#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[allow(missing_docs)]
pub enum LexicalError {
    #[error("end of file found in comment")]
    EndOfFileInComment(Loc),

    #[error("end of file found in string literal")]
    EndOfFileInString(Loc),

    #[error("end of file found in hex literal string")]
    EndofFileInHex(Loc),

    #[error("missing number")]
    MissingNumber(Loc),

    #[error("invalid character '{1}' in hex literal string")]
    InvalidCharacterInHexLiteral(Loc, char),

    #[error("unrecognised token '{1}'")]
    UnrecognisedToken(Loc, String),

    #[error("missing exponent")]
    MissingExponent(Loc),

    #[error("'{1}' found where 'from' expected")]
    ExpectedFrom(Loc, String),
}

/// Returns whether `word` is a keyword.
pub fn is_keyword(word: &str) -> bool {
    KEYWORDS.contains_key(word)
}

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "break" => Token::Break,
    "const" => Token::Constant,
    "continue" => Token::Continue,
    "do" => Token::Do,
    "else" => Token::Else,
    "emit" => Token::Emit,
    "enum" => Token::Enum,
    "false" => Token::False,
    "for" => Token::For,
    "fn" => Token::Function,
    "if" => Token::If,
    "import" => Token::Import,
    "interface" => Token::Interface,
    "library" => Token::Library,
    "pragma" => Token::Pragma,
    "return" => Token::Return,
    "string" => Token::String,
    "struct" => Token::Struct,
    "true" => Token::True,
    "type" => Token::Type,
    "while" => Token::While,
    "as" => Token::As,
    "is" => Token::Is,
    "assembly" => Token::Assembly,
    "let" => Token::Readable,
    "mut" => Token::Writable,
    "pio" => Token::Portable,
    "external" => Token::External,
};

impl<'input> Lexer<'input> {
    /// Instantiates a new Lexer.
    ///
    /// # Examples
    ///
    /// ```
    /// use but::lexer::Lexer;
    ///
    /// let source = "uint256 number = 0;";
    /// let mut comments = Vec::new();
    /// let mut errors = Vec::new();
    /// let mut lexer = Lexer::new(source, 0, &mut comments, &mut errors);
    /// ```
    pub fn new(
        input: &'input str,
        file_no: usize,
        comments: &'input mut Vec<Comment>,
        errors: &'input mut Vec<LexicalError>,
    ) -> Self {
        Lexer {
            input,
            chars: peek_nth(input.char_indices()),
            comments,
            file_no,
            last_tokens: [None, None],
            errors,
        }
    }

    fn parse_number(&mut self, mut start: usize, ch: char) -> Result<'input> {
        let mut is_rational = false;
        let mut is_minus = false;
        if ch == '0' {
            if let Some((_, 'x')) = self.chars.peek() {
                // hex number
                self.chars.next();

                let mut end = match self.chars.next() {
                    Some((end, ch)) if ch.is_ascii_hexdigit() => end,
                    Some((..)) => {
                        return Err(LexicalError::MissingNumber(Loc::Source(
                            self.file_no,
                            start,
                            start + 1,
                        )));
                    }
                    None => {
                        return Err(LexicalError::EndofFileInHex(Loc::Source(
                            self.file_no,
                            start,
                            self.input.len(),
                        )));
                    }
                };

                while let Some((i, ch)) = self.chars.peek() {
                    if !ch.is_ascii_hexdigit() && *ch != '_' {
                        break;
                    }
                    end = *i;
                    self.chars.next();
                }

                let hex = &self.input[start + 2..=end];
                return Ok((
                    start,
                    Token::Number(i64::from_str_radix(hex, 16).unwrap()),
                    end + 1,
                ));
            }
        }

        if ch == '.' {
            is_rational = true;
            start -= 1;
        }
        if ch == '-' {
            is_minus = true;
        }

        let mut end = start;
        while let Some((i, ch)) = self.chars.peek() {
            if !ch.is_ascii_digit() && *ch != '_' {
                break;
            }
            end = *i;
            self.chars.next();
        }
        let mut rational_end = end;
        let mut end_before_rational = end + 1;
        let mut rational_start = end;
        if is_rational {
            end_before_rational = start;
            rational_start = start + 1;
        }

        if let Some((_, '.')) = self.chars.peek() {
            if let Some((i, ch)) = self.chars.peek_nth(1) {
                if ch.is_ascii_digit() && !is_rational {
                    rational_start = *i;
                    rational_end = *i;
                    is_rational = true;
                    self.chars.next(); // advance over '.'
                    while let Some((i, ch)) = self.chars.peek() {
                        if !ch.is_ascii_digit() && *ch != '_' {
                            break;
                        }
                        rational_end = *i;
                        end = *i;
                        self.chars.next();
                    }
                }
            }
        }

        let old_end = end;
        let mut exp_start = end + 1;

        if let Some((i, 'e' | 'E')) = self.chars.peek() {
            exp_start = *i + 1;
            self.chars.next();
            // Negative exponent
            while matches!(self.chars.peek(), Some((_, '-'))) {
                self.chars.next();
            }
            while let Some((i, ch)) = self.chars.peek() {
                if !ch.is_ascii_digit() && *ch != '_' {
                    break;
                }
                end = *i;
                self.chars.next();
            }

            if exp_start > end {
                return Err(LexicalError::MissingExponent(Loc::Source(
                    self.file_no,
                    start,
                    self.input.len(),
                )));
            }
        }

        if is_rational {
            let integer = &self.input[start..end_before_rational];
            let fraction = &self.input[rational_start..=rational_end];
            let exp = &self.input[exp_start..=end];

            return Ok((
                start,
                Token::RationalNumber(&self.input[start..=rational_end], is_minus),
                end + 1,
            ));
        }

        let integer = &self.input[start..=old_end];
        let exp = &self.input[exp_start..=end];

        let mut n = i64::from_str(&integer).unwrap();
        if is_minus {
            n = -n;
        }
        Ok((start, Token::Number(n), end + 1))
    }

    fn string(
        &mut self,
        unicode: bool,
        token_start: usize,
        string_start: usize,
        quote_char: char,
    ) -> Result<'input> {
        let mut end;

        let mut last_was_escape = false;

        loop {
            if let Some((i, ch)) = self.chars.next() {
                end = i;
                if !last_was_escape {
                    if ch == quote_char {
                        break;
                    }
                    last_was_escape = ch == '\\';
                } else {
                    last_was_escape = false;
                }
            } else {
                return Err(LexicalError::EndOfFileInString(Loc::Source(
                    self.file_no,
                    token_start,
                    self.input.len(),
                )));
            }
        }

        Ok((
            token_start,
            Token::StringLiteral(unicode, &self.input[string_start..end]),
            end + 1,
        ))
    }

    fn next(&mut self) -> Option<Spanned<'input>> {
        'toplevel: loop {
            match self.chars.next() {
                Some((start, ch)) if ch == '_' || ch == '$' || UnicodeXID::is_xid_start(ch) => {
                    let (id, end) = self.match_identifier(start);

                    if id == "unicode" {
                        match self.chars.peek() {
                            Some((_, quote_char @ '"')) | Some((_, quote_char @ '\'')) => {
                                let quote_char = *quote_char;

                                self.chars.next();
                                let str_res = self.string(true, start, start + 8, quote_char);
                                match str_res {
                                    Err(lex_err) => self.errors.push(lex_err),
                                    Ok(val) => return Some(val),
                                }
                            }
                            _ => (),
                        }
                    }

                    if id == "hex" {
                        match self.chars.peek() {
                            Some((_, quote_char @ '"')) | Some((_, quote_char @ '\'')) => {
                                let quote_char = *quote_char;

                                self.chars.next();

                                for (i, ch) in &mut self.chars {
                                    if ch == quote_char {
                                        return Some((
                                            start,
                                            Token::HexLiteral(&self.input[start..=i]),
                                            i + 1,
                                        ));
                                    }

                                    if !ch.is_ascii_hexdigit() && ch != '_' {
                                        // Eat up the remainer of the string
                                        for (_, ch) in &mut self.chars {
                                            if ch == quote_char {
                                                break;
                                            }
                                        }

                                        self.errors.push(
                                            LexicalError::InvalidCharacterInHexLiteral(
                                                Loc::Source(self.file_no, i, i + 1),
                                                ch,
                                            ),
                                        );
                                        continue 'toplevel;
                                    }
                                }

                                self.errors
                                    .push(LexicalError::EndOfFileInString(Loc::Source(
                                        self.file_no,
                                        start,
                                        self.input.len(),
                                    )));
                                return None;
                            }
                            _ => (),
                        }
                    }

                    if id == "address" {
                        match self.chars.peek() {
                            Some((_, quote_char @ '"')) | Some((_, quote_char @ '\'')) => {
                                let quote_char = *quote_char;

                                self.chars.next();

                                for (i, ch) in &mut self.chars {
                                    if ch == quote_char {
                                        return Some((
                                            start,
                                            Token::AddressLiteral(&self.input[start..=i]),
                                            i + 1,
                                        ));
                                    }
                                }

                                self.errors
                                    .push(LexicalError::EndOfFileInString(Loc::Source(
                                        self.file_no,
                                        start,
                                        self.input.len(),
                                    )));
                                return None;
                            }
                            _ => (),
                        }
                    }

                    return if let Some(w) = KEYWORDS.get(id) {
                        Some((start, *w, end))
                    } else {
                        Some((start, Token::Identifier(id), end))
                    };
                }
                Some((start, quote_char @ '"')) | Some((start, quote_char @ '\'')) => {
                    let str_res = self.string(false, start, start + 1, quote_char);
                    match str_res {
                        Err(lex_err) => self.errors.push(lex_err),
                        Ok(val) => return Some(val),
                    }
                }
                Some((start, '/')) => {
                    match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            return Some((start, Token::DivideAssign, start + 2));
                        }
                        Some((_, '/')) => {
                            // line comment
                            self.chars.next();

                            let mut newline = false;

                            let doc_comment = match self.chars.next() {
                                Some((_, '/')) => {
                                    // ///(/)+ is still a line comment
                                    !matches!(self.chars.peek(), Some((_, '/')))
                                }
                                Some((_, ch)) if ch == '\n' || ch == '\r' => {
                                    newline = true;
                                    false
                                }
                                _ => false,
                            };

                            let mut last = start + 3;

                            if !newline {
                                loop {
                                    match self.chars.next() {
                                        None => {
                                            last = self.input.len();
                                            break;
                                        }
                                        Some((offset, '\n' | '\r')) => {
                                            last = offset;
                                            break;
                                        }
                                        Some(_) => (),
                                    }
                                }
                            }

                            if doc_comment {
                                self.comments.push(Comment::DocLine(
                                    Loc::Source(self.file_no, start, last),
                                    self.input[start..last].to_owned(),
                                ));
                            } else {
                                self.comments.push(Comment::Line(
                                    Loc::Source(self.file_no, start, last),
                                    self.input[start..last].to_owned(),
                                ));
                            }
                        }
                        Some((_, '*')) => {
                            // multiline comment
                            self.chars.next();

                            let doc_comment_start = matches!(self.chars.peek(), Some((_, '*')));

                            let mut last = start + 3;
                            let mut seen_star = false;

                            loop {
                                if let Some((i, ch)) = self.chars.next() {
                                    if seen_star && ch == '/' {
                                        break;
                                    }
                                    seen_star = ch == '*';
                                    last = i;
                                } else {
                                    self.errors.push(LexicalError::EndOfFileInComment(
                                        Loc::Source(self.file_no, start, self.input.len()),
                                    ));
                                    return None;
                                }
                            }

                            // `/**/` is not a doc comment
                            if doc_comment_start && last > start + 2 {
                                self.comments.push(Comment::DocBlock(
                                    Loc::Source(self.file_no, start, last + 2),
                                    self.input[start..last + 2].to_owned(),
                                ));
                            } else {
                                self.comments.push(Comment::Block(
                                    Loc::Source(self.file_no, start, last + 2),
                                    self.input[start..last + 2].to_owned(),
                                ));
                            }
                        }
                        _ => {
                            return Some((start, Token::Divide, start + 1));
                        }
                    }
                }
                Some((start, ch)) if ch.is_ascii_digit() => {
                    let parse_result = self.parse_number(start, ch);
                    match parse_result {
                        Err(lex_err) => {
                            self.errors.push(lex_err.clone());
                            if matches!(lex_err, LexicalError::EndofFileInHex(_)) {
                                return None;
                            }
                        }
                        Ok(parse_result) => return Some(parse_result),
                    }
                }
                Some((i, '#')) => return Some((i, Token::Sharp, i + 1)),
                Some((i, ';')) => return Some((i, Token::Semicolon, i + 1)),
                Some((i, ',')) => return Some((i, Token::Comma, i + 1)),
                Some((i, '(')) => return Some((i, Token::OpenParenthesis, i + 1)),
                Some((i, ')')) => return Some((i, Token::CloseParenthesis, i + 1)),
                Some((i, '{')) => return Some((i, Token::OpenCurlyBrace, i + 1)),
                Some((i, '}')) => return Some((i, Token::CloseCurlyBrace, i + 1)),
                Some((i, '=')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::Equal, i + 2))
                        }
                        _ => Some((i, Token::Assign, i + 1)),
                    };
                }
                Some((i, '!')) => {
                    return if let Some((_, '=')) = self.chars.peek() {
                        self.chars.next();
                        Some((i, Token::NotEqual, i + 2))
                    } else {
                        Some((i, Token::Not, i + 1))
                    };
                }
                Some((i, '|')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::BitwiseOrAssign, i + 2))
                        }
                        Some((_, '|')) => {
                            self.chars.next();
                            Some((i, Token::Or, i + 2))
                        }
                        _ => Some((i, Token::BitwiseOr, i + 1)),
                    };
                }
                Some((i, '&')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::BitwiseAndAssign, i + 2))
                        }
                        Some((_, '&')) => {
                            self.chars.next();
                            Some((i, Token::And, i + 2))
                        }
                        _ => Some((i, Token::BitwiseAnd, i + 1)),
                    };
                }
                Some((i, '+')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::AddAssign, i + 2))
                        }
                        Some((_, '+')) => {
                            self.chars.next();
                            Some((i, Token::Increment, i + 2))
                        }
                        _ => Some((i, Token::Add, i + 1)),
                    };
                }
                Some((i, '-')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::SubtractAssign, i + 2))
                        }
                        Some((_, '-')) => {
                            self.chars.next();
                            Some((i, Token::Decrement, i + 2))
                        }
                        Some((_, other)) if other.is_ascii_digit() => {
                            return match self.parse_number(i + 1, '-') {
                                Err(lex_error) => {
                                    self.errors.push(lex_error);
                                    None
                                }
                                Ok(parse_result) => Some(parse_result),
                            };
                        }
                        _ => Some((i, Token::Subtract, i + 1)),
                    };
                }
                Some((i, '*')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::MulAssign, i + 2))
                        }
                        Some((_, '*')) => {
                            self.chars.next();
                            Some((i, Token::Power, i + 2))
                        }
                        _ => Some((i, Token::Mul, i + 1)),
                    };
                }
                Some((i, '%')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::ModuloAssign, i + 2))
                        }
                        _ => Some((i, Token::Modulo, i + 1)),
                    };
                }
                Some((i, '<')) => {
                    return match self.chars.peek() {
                        Some((_, '<')) => {
                            self.chars.next();
                            if let Some((_, '=')) = self.chars.peek() {
                                self.chars.next();
                                Some((i, Token::ShiftLeftAssign, i + 3))
                            } else {
                                Some((i, Token::ShiftLeft, i + 2))
                            }
                        }
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::LessEqual, i + 2))
                        }
                        _ => Some((i, Token::Less, i + 1)),
                    };
                }
                Some((i, '>')) => {
                    return match self.chars.peek() {
                        Some((_, '>')) => {
                            self.chars.next();
                            if let Some((_, '=')) = self.chars.peek() {
                                self.chars.next();
                                Some((i, Token::ShiftRightAssign, i + 3))
                            } else {
                                Some((i, Token::ShiftRight, i + 2))
                            }
                        }
                        Some((_, '=')) => {
                            self.chars.next();
                            Some((i, Token::MoreEqual, i + 2))
                        }
                        _ => Some((i, Token::More, i + 1)),
                    };
                }
                Some((i, '.')) => {
                    if let Some((_, a)) = self.chars.peek() {
                        if a.is_ascii_digit() {
                            return match self.parse_number(i + 1, '.') {
                                Err(lex_error) => {
                                    self.errors.push(lex_error);
                                    None
                                }
                                Ok(parse_result) => Some(parse_result),
                            };
                        }
                    }
                    return Some((i, Token::Member, i + 1));
                }
                Some((i, '[')) => return Some((i, Token::OpenBracket, i + 1)),
                Some((i, ']')) => return Some((i, Token::CloseBracket, i + 1)),
                Some((i, ':')) => return Some((i, Token::Colon, i + 1)),
                Some((_, ch)) if ch.is_whitespace() => (),
                Some((start, _)) => {
                    let mut end;

                    loop {
                        if let Some((i, ch)) = self.chars.next() {
                            end = i;

                            if ch.is_whitespace() {
                                break;
                            }
                        } else {
                            end = self.input.len();
                            break;
                        }
                    }

                    self.errors.push(LexicalError::UnrecognisedToken(
                        Loc::Source(self.file_no, start, end),
                        self.input[start..end].to_owned(),
                    ));
                }
                None => return None, // End of file
            }
        }
    }

    /// Next token is pragma value. Return it
    fn pragma_value(&mut self) -> Option<Spanned<'input>> {
        // special parser for pragma solidity >=0.4.22 <0.7.0;
        let mut start = None;
        let mut end = 0;

        // solc will include anything upto the next semicolon, whitespace
        // trimmed on left and right
        loop {
            match self.chars.peek() {
                Some((_, ';')) | None => {
                    return if let Some(start) = start {
                        Some((
                            start,
                            Token::StringLiteral(false, &self.input[start..end]),
                            end,
                        ))
                    } else {
                        self.next()
                    };
                }
                Some((_, ch)) if ch.is_whitespace() => {
                    self.chars.next();
                }
                Some((i, _)) => {
                    if start.is_none() {
                        start = Some(*i);
                    }
                    self.chars.next();

                    // end should point to the byte _after_ the character
                    end = match self.chars.peek() {
                        Some((i, _)) => *i,
                        None => self.input.len(),
                    }
                }
            }
        }
    }

    fn match_identifier(&mut self, start: usize) -> (&'input str, usize) {
        let end;
        loop {
            if let Some((i, ch)) = self.chars.peek() {
                if !UnicodeXID::is_xid_continue(*ch) && *ch != '$' {
                    end = *i;
                    break;
                }
                self.chars.next();
            } else {
                end = self.input.len();
                break;
            }
        }

        (&self.input[start..end], end)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        // Lexer should be aware of whether the last two tokens were
        // pragma followed by identifier. If this is true, then special parsing should be
        // done for the pragma value
        let token = if let [Some(Token::Pragma), Some(Token::Identifier(_))] = self.last_tokens {
            self.pragma_value()
        } else {
            self.next()
        };

        self.last_tokens = [
            self.last_tokens[1],
            match token {
                Some((_, n, _)) => Some(n),
                _ => None,
            },
        ];

        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let mut comments = Vec::new();
        let mut errors = Vec::new();

        let multiple_errors = r#" a - 9e € bool hex hex"g"   /**  "#;
        let tokens = Lexer::new(multiple_errors, 0, &mut comments, &mut errors).collect::<Vec<_>>();
        assert_eq!(
            tokens,
            vec![
                (1, Token::Identifier("a"), 2),
                (3, Token::Subtract, 4),
                (12, Token::Identifier("bool"), 16),
                (17, Token::Identifier("hex"), 20),
            ]
        );

        assert_eq!(
            errors,
            vec![
                LexicalError::MissingExponent(Loc::Source(0, 5, 35)),
                LexicalError::UnrecognisedToken(Loc::Source(0, 8, 11), '€'.to_string()),
                LexicalError::InvalidCharacterInHexLiteral(Loc::Source(0, 25, 26), 'g'),
                LexicalError::EndOfFileInComment(Loc::Source(0, 30, 35)),
            ]
        );

        let mut errors = Vec::new();

        let tokens = Lexer::new("hex", 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(tokens, vec!((0, Token::Identifier("hex"), 3)));

        let tokens = Lexer::new(
            "hex\"cafe_dead\" /* adad*** */",
            0,
            &mut comments,
            &mut errors,
        )
        .collect::<Vec<_>>();

        assert_eq!(tokens, vec!((0, Token::HexLiteral("hex\"cafe_dead\""), 14)));

        let tokens = Lexer::new("\"foo\"", 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(tokens, vec!((0, Token::StringLiteral(false, "foo"), 5)));

        let tokens = Lexer::new(
            "pragma solidity >=0.5.0 <0.7.0;",
            0,
            &mut comments,
            &mut errors,
        )
        .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::Pragma, 6),
                (7, Token::Identifier("solidity"), 15),
                (16, Token::StringLiteral(false, ">=0.5.0 <0.7.0"), 30),
                (30, Token::Semicolon, 31),
            )
        );

        let tokens = Lexer::new(
            "pragma solidity \t>=0.5.0 <0.7.0 \n ;",
            0,
            &mut comments,
            &mut errors,
        )
        .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::Pragma, 6),
                (7, Token::Identifier("solidity"), 15),
                (17, Token::StringLiteral(false, ">=0.5.0 <0.7.0"), 31),
                (34, Token::Semicolon, 35),
            )
        );

        let tokens =
            Lexer::new("pragma solidity 赤;", 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::Pragma, 6),
                (7, Token::Identifier("solidity"), 15),
                (16, Token::StringLiteral(false, "赤"), 19),
                (19, Token::Semicolon, 20)
            )
        );

        let tokens = Lexer::new(">>= >> >= >", 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::ShiftRightAssign, 3),
                (4, Token::ShiftRight, 6),
                (7, Token::MoreEqual, 9),
                (10, Token::More, 11),
            )
        );

        let tokens = Lexer::new("<<= << <= <", 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::ShiftLeftAssign, 3),
                (4, Token::ShiftLeft, 6),
                (7, Token::LessEqual, 9),
                (10, Token::Less, 11),
            )
        );

        let tokens = Lexer::new("-16 -- - -=", 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec!(
                (1, Token::Number(-16i64), 3),
                (4, Token::Decrement, 6),
                (7, Token::Subtract, 8),
                (9, Token::SubtractAssign, 11),
            )
        );

        let tokens = Lexer::new("-4 ", 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(tokens, vec!((1, Token::Number(-4i64), 2),));

        let mut errors = Vec::new();
        let _ = Lexer::new(r#"hex"abcdefg""#, 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(
            errors,
            vec![LexicalError::InvalidCharacterInHexLiteral(
                Loc::Source(0, 10, 11),
                'g',
            )]
        );

        let mut errors = Vec::new();
        let _ = Lexer::new(r#" € "#, 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(
            errors,
            vec!(LexicalError::UnrecognisedToken(
                Loc::Source(0, 1, 4),
                "€".to_owned(),
            ))
        );

        let mut errors = Vec::new();
        let _ = Lexer::new(r#"€"#, 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(
            errors,
            vec!(LexicalError::UnrecognisedToken(
                Loc::Source(0, 0, 3),
                "€".to_owned(),
            ))
        );

        let tokens =
            Lexer::new(r#"pragma foo bar"#, 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::Pragma, 6),
                (7, Token::Identifier("foo"), 10),
                (11, Token::StringLiteral(false, "bar"), 14),
            )
        );

        comments.truncate(0);

        let tokens = Lexer::new(r#"/// foo"#, 0, &mut comments, &mut errors).count();

        assert_eq!(tokens, 0);
        assert_eq!(
            comments,
            vec![Comment::DocLine(Loc::Source(0, 0, 7), "/// foo".to_owned())],
        );

        comments.truncate(0);

        let tokens = Lexer::new("/// jadajadadjada\n// bar", 0, &mut comments, &mut errors).count();

        assert_eq!(tokens, 0);
        assert_eq!(
            comments,
            vec!(
                Comment::DocLine(Loc::Source(0, 0, 17), "/// jadajadadjada".to_owned()),
                Comment::Line(Loc::Source(0, 18, 24), "// bar".to_owned())
            )
        );

        comments.truncate(0);

        let tokens = Lexer::new("/**/", 0, &mut comments, &mut errors).count();

        assert_eq!(tokens, 0);
        assert_eq!(
            comments,
            vec!(Comment::Block(Loc::Source(0, 0, 4), "/**/".to_owned()))
        );

        comments.truncate(0);

        let tokens = Lexer::new(r#"/** foo */"#, 0, &mut comments, &mut errors).count();

        assert_eq!(tokens, 0);
        assert_eq!(
            comments,
            vec!(Comment::DocBlock(
                Loc::Source(0, 0, 10),
                "/** foo */".to_owned()
            ))
        );

        comments.truncate(0);

        let tokens = Lexer::new(
            "/** jadajadadjada */\n/* bar */",
            0,
            &mut comments,
            &mut errors,
        )
        .count();

        assert_eq!(tokens, 0);
        assert_eq!(
            comments,
            vec!(
                Comment::DocBlock(Loc::Source(0, 0, 20), "/** jadajadadjada */".to_owned()),
                Comment::Block(Loc::Source(0, 21, 30), "/* bar */".to_owned())
            )
        );

        let tokens = Lexer::new("/************/", 0, &mut comments, &mut errors).next();
        assert_eq!(tokens, None);

        let mut errors = Vec::new();
        let _ = Lexer::new("/**", 0, &mut comments, &mut errors).next();
        assert_eq!(
            errors,
            vec!(LexicalError::EndOfFileInComment(Loc::Source(0, 0, 3)))
        );

        let mut errors = Vec::new();
        let tokens = Lexer::new("//////////////", 0, &mut comments, &mut errors).next();
        assert_eq!(tokens, None);

        // some unicode tests
        let tokens = Lexer::new(
            ">=\u{a0} . très\u{2028}αβγδεζηθικλμνξοπρστυφχψω\u{85}カラス",
            0,
            &mut comments,
            &mut errors,
        )
        .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::MoreEqual, 2),
                (5, Token::Member, 6),
                (7, Token::Identifier("très"), 12),
                (15, Token::Identifier("αβγδεζηθικλμνξοπρστυφχψω"), 63),
                (65, Token::Identifier("カラス"), 74)
            )
        );

        let tokens = Lexer::new(r#"unicode"€""#, 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(tokens, vec!((0, Token::StringLiteral(true, "€"), 12)));

        let tokens =
            Lexer::new(r#"unicode "€""#, 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::Identifier("unicode"), 7),
                (8, Token::StringLiteral(false, "€"), 13),
            )
        );

        // scientific notation
        let tokens = Lexer::new(r#" 1e0 "#, 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(tokens, vec!((1, Token::Number(1i64), 4)));

        let mut errors = Vec::new();
        let tokens = Lexer::new(r#" - 9e"#, 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(tokens, vec!((1, Token::Subtract, 2)));
        assert_eq!(
            errors,
            vec!(LexicalError::MissingExponent(Loc::Source(0, 3, 5)))
        );

        let mut errors = Vec::new();
        let tokens = Lexer::new(r#"9ea"#, 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(tokens, vec!((2, Token::Identifier("a"), 3)));
        assert_eq!(
            errors,
            vec!(LexicalError::MissingExponent(Loc::Source(0, 0, 3)))
        );

        let mut errors = Vec::new();
        let tokens = Lexer::new(r#"42.a"#, 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::Number(42i64), 2),
                (2, Token::Member, 3),
                (3, Token::Identifier("a"), 4)
            )
        );

        comments.truncate(0);

        let tokens = Lexer::new("/// jadajadadjada\n// bar", 0, &mut comments, &mut errors).count();

        assert_eq!(tokens, 0);
        assert_eq!(
            comments,
            vec!(
                Comment::DocLine(Loc::Source(0, 0, 17), "/// jadajadadjada".to_owned()),
                Comment::Line(Loc::Source(0, 18, 24), "// bar".to_owned())
            )
        );

        comments.truncate(0);

        let tokens = Lexer::new("/**/", 0, &mut comments, &mut errors).count();

        assert_eq!(tokens, 0);
        assert_eq!(
            comments,
            vec!(Comment::Block(Loc::Source(0, 0, 4), "/**/".to_owned()))
        );

        comments.truncate(0);

        let tokens = Lexer::new(r#"/** foo */"#, 0, &mut comments, &mut errors).count();

        assert_eq!(tokens, 0);
        assert_eq!(
            comments,
            vec!(Comment::DocBlock(
                Loc::Source(0, 0, 10),
                "/** foo */".to_owned()
            ))
        );

        comments.truncate(0);

        let tokens = Lexer::new(
            "/** jadajadadjada */\n/* bar */",
            0,
            &mut comments,
            &mut errors,
        )
        .count();

        assert_eq!(tokens, 0);
        assert_eq!(
            comments,
            vec!(
                Comment::DocBlock(Loc::Source(0, 0, 20), "/** jadajadadjada */".to_owned()),
                Comment::Block(Loc::Source(0, 21, 30), "/* bar */".to_owned())
            )
        );

        let tokens = Lexer::new("/************/", 0, &mut comments, &mut errors).next();
        assert_eq!(tokens, None);

        let mut errors = Vec::new();
        let _ = Lexer::new("/**", 0, &mut comments, &mut errors).next();
        assert_eq!(
            errors,
            vec!(LexicalError::EndOfFileInComment(Loc::Source(0, 0, 3)))
        );

        let mut errors = Vec::new();
        let tokens = Lexer::new("//////////////", 0, &mut comments, &mut errors).next();
        assert_eq!(tokens, None);

        // some unicode tests
        let tokens = Lexer::new(
            ">=\u{a0} . très\u{2028}αβγδεζηθικλμνξοπρστυφχψω\u{85}カラス",
            0,
            &mut comments,
            &mut errors,
        )
        .collect::<Vec<(usize, Token, usize)>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::MoreEqual, 2),
                (5, Token::Member, 6),
                (7, Token::Identifier("très"), 12),
                (15, Token::Identifier("αβγδεζηθικλμνξοπρστυφχψω"), 63),
                (65, Token::Identifier("カラス"), 74)
            )
        );

        let tokens =
            Lexer::new(r#"unicode"€""#, 0, &mut comments, &mut errors)
                .collect::<Vec<(usize, Token, usize)>>();

        assert_eq!(tokens, vec!((0, Token::StringLiteral(true, "€"), 12)));

        let tokens =
            Lexer::new(r#"unicode "€""#, 0, &mut comments, &mut errors)
                .collect::<Vec<(usize, Token, usize)>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::Identifier("unicode"), 7),
                (8, Token::StringLiteral(false, "€"), 13),
            )
        );

        let mut errors = Vec::new();
        let tokens =
            Lexer::new(r#" - 9e"#, 0, &mut comments, &mut errors)
                .collect::<Vec<(usize, Token, usize)>>();

        assert_eq!(tokens, vec!((1, Token::Subtract, 2)));
        assert_eq!(
            errors,
            vec!(LexicalError::MissingExponent(Loc::Source(0, 3, 5)))
        );

        let mut errors = Vec::new();
        let tokens = Lexer::new(r#"9ea"#, 0, &mut comments, &mut errors)
            .collect::<Vec<(usize, Token, usize)>>();

        assert_eq!(tokens, vec!((2, Token::Identifier("a"), 3)));
        assert_eq!(
            errors,
            vec!(LexicalError::MissingExponent(Loc::Source(0, 0, 3)))
        );

        let mut errors = Vec::new();

        let tokens =
            Lexer::new(r#"42..a"#, 0, &mut comments, &mut errors)
                .collect::<Vec<(usize, Token, usize)>>();

        assert_eq!(
            tokens,
            vec!(
                (0, Token::Number(42i64), 2),
                (2, Token::Member, 3),
                (3, Token::Member, 4),
                (4, Token::Identifier("a"), 5)
            )
        );

        let mut errors = Vec::new();
        let _ = Lexer::new(r#"hex"g""#, 0, &mut comments, &mut errors)
            .collect::<Vec<(usize, Token, usize)>>();
        assert_eq!(
            errors,
            vec!(LexicalError::InvalidCharacterInHexLiteral(
                Loc::Source(0, 4, 5),
                'g',
            ),)
        );

        let mut errors = Vec::new();
        let tokens = Lexer::new(".9", 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(tokens, vec!((0, Token::RationalNumber(".9", false), 2)));

        let mut errors = Vec::new();
        let tokens = Lexer::new(".9e10", 0, &mut comments, &mut errors).collect::<Vec<_>>();

        assert_eq!(tokens, vec!((0, Token::RationalNumber(".9", false), 5)));

        errors.clear();
        comments.clear();
    }
}

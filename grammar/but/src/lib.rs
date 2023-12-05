#![doc = include_str!("../README.adoc")]
#![warn(missing_debug_implementations, missing_docs)]

use lalrpop_util::ParseError;

use diagnostics::Diagnostic;

use crate::ast::CodeLocation;
use crate::ast::Loc;
use crate::lexer::LexicalError;
use crate::lexer::Token;

pub mod ast;
pub mod diagnostics;
pub mod doccomment;
pub mod helpers;
pub mod lexer;

mod annotations;
mod functions;
mod properties;
#[cfg(test)]
mod tests;
mod typing;
mod variables;

#[allow(
    clippy::needless_lifetimes,
    clippy::type_complexity,
    clippy::ptr_arg,
    clippy::redundant_clone,
    clippy::just_underscores_and_digits
)]
mod but {
    include!(concat!(env!("OUT_DIR"), "/but.rs"));
    // include!("but.rs");
}

pub fn parse(
    src: &str,
    file_no: usize,
) -> Result<(ast::SourceUnit, Vec<ast::Comment>), Vec<Diagnostic>> {
    // parse phase
    let mut comments = Vec::new();
    let mut lexer_errors = Vec::new();
    let mut lex = lexer::Lexer::new(src, file_no, &mut comments, &mut lexer_errors);

    let mut parser_errors = Vec::new();
    let res = but::SourceUnitParser::new().parse(src, file_no, &mut parser_errors, &mut lex);

    let mut diagnostics = Vec::with_capacity(lex.errors.len() + parser_errors.len());
    for lexical_error in lex.errors {
        diagnostics.push(Diagnostic::parser_error(
            lexical_error.loc(),
            lexical_error.to_string(),
        ))
    }

    for e in parser_errors {
        diagnostics.push(parser_error_to_diagnostic(&e.error, file_no));
    }

    match res {
        Err(e) => {
            diagnostics.push(parser_error_to_diagnostic(&e, file_no));
            Err(diagnostics)
        }
        _ if !diagnostics.is_empty() => Err(diagnostics),
        Ok(res) => Ok((res, comments)),
    }
}

/// Convert lalrop parser error to a Diagnostic
fn parser_error_to_diagnostic(
    error: &ParseError<usize, Token, LexicalError>,
    file_no: usize,
) -> Diagnostic {
    match error {
        ParseError::InvalidToken { location } => Diagnostic::parser_error(
            Loc::Source(file_no, *location, *location),
            "invalid token".to_string(),
        ),
        ParseError::UnrecognizedToken {
            token: (l, token, r),
            expected,
        } => Diagnostic::parser_error(
            Loc::Source(file_no, *l, *r),
            format!(
                "unrecognised token '{}', expected {}",
                token,
                expected.join(", ")
            ),
        ),
        ParseError::User { error } => Diagnostic::parser_error(error.loc(), error.to_string()),
        ParseError::ExtraToken { token } => Diagnostic::parser_error(
            Loc::Source(file_no, token.0, token.2),
            format!("extra token '{}' encountered", token.0),
        ),
        ParseError::UnrecognizedEof { expected, location } => Diagnostic::parser_error(
            Loc::Source(file_no, *location, *location),
            format!("unexpected end of file, expecting {}", expected.join(", ")),
        ),
    }
}

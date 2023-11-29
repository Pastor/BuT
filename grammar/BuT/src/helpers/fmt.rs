use std::fmt::{Debug, Pointer};
use std::{
    borrow::Cow,
    fmt::{Display, Formatter, Result, Write},
};

use crate::ast;
use crate::ast::{Annotation, Type};

macro_rules! write_opt {
    // no sep
    ($f:expr, $opt:expr $(,)?) => {
        if let Some(t) = $opt {
            Display::fmt(t, $f)?;
        }
    };

    // sep before
    ($f:expr, $sep:literal, $opt:expr $(,)?) => {
        if let Some(t) = $opt {
            Display::fmt(&$sep, $f)?;
            Display::fmt(t, $f)?;
        }
    };

    // sep after
    ($f:expr, $opt:expr, $sep:literal $(,)?) => {
        if let Some(t) = $opt {
            Display::fmt(t, $f)?;
            Display::fmt(&$sep, $f)?;
        }
    };

    // both
    ($f:expr, $sep1:literal, $opt:expr, $sep2:literal $(,)?) => {
        if let Some(t) = $opt {
            Display::fmt(&$sep1, $f)?;
            Display::fmt(t, $f)?;
            Display::fmt(&$sep2, $f)?;
        }
    };
}

// structs
impl Display for ast::Annotation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Annotation::Identifier(_, id) => {
                Display::fmt(id, f)?;
            }
            Annotation::Function { name, args, loc: _ } => {
                Display::fmt(name, f)?;
                f.write_str("(")?;
                for i in args.iter() {
                    Display::fmt(i, f)?;
                    f.write_str(", ")?;
                }
                f.write_str(")")?;
            }
            Annotation::Assign {
                name,
                value,
                loc: _,
            } => {
                Display::fmt(name, f)?;
                f.write_str(" = ")?;
                Display::fmt(value, f)?;
            }
        }
        Ok(())
    }
}

impl Display for ast::Base {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        std::fmt::Display::fmt(&self.name, f)?;
        if let Some(args) = &self.args {
            f.write_char('(')?;
            write_separated(args, f, ", ")?;
            f.write_char(')')?;
        }
        Ok(())
    }
}

impl Display for ast::EnumDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str("enum ")?;
        write_opt!(f, &self.name, ' ');

        f.write_char('{')?;
        write_separated_iter(self.values.iter().flatten(), f, ", ")?;
        f.write_char('}')
    }
}

impl Display for ast::ErrorDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        std::fmt::Display::fmt(&self.keyword, f)?;
        write_opt!(f, ' ', &self.name);

        f.write_char('(')?;
        write_separated(&self.fields, f, ", ")?;
        f.write_str(");")
    }
}

impl Display for ast::ErrorParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        std::fmt::Display::fmt(&self.ty, f)?;
        write_opt!(f, ' ', &self.name);
        Ok(())
    }
}

impl Display for ast::FunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        std::fmt::Display::fmt(&self.ty, f)?;
        write_opt!(f, ' ', &self.name);

        f.write_char('(')?;
        fmt_parameter_list(&self.params, f)?;
        f.write_char(')')?;

        if !self.attributes.is_empty() {
            f.write_char(' ')?;
            write_separated(&self.attributes, f, " ")?;
        }

        if !self.returns.is_empty() {
            f.write_str(" returns (")?;
            fmt_parameter_list(&self.returns, f)?;
            f.write_char(')')?;
        }

        if let Some(body) = &self.body {
            f.write_char(' ')?;
            std::fmt::Display::fmt(&body, f)
        } else {
            f.write_char(';')
        }
    }
}

impl Display for ast::HexLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.hex)
    }
}

impl Display for ast::Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.name)
    }
}

impl Display for ast::IdentifierPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write_separated(&self.identifiers, f, "::")
    }
}

impl Display for ast::NamedArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        std::fmt::Display::fmt(&self.name, f)?;
        f.write_str(": ")?;
        std::fmt::Display::fmt(&self.expr, f)
    }
}

impl Display for ast::Parameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write_opt!(f, &self.annotation, ' ');
        Debug::fmt(&self.ty, f)?;
        write_opt!(f, ' ', &self.storage);
        write_opt!(f, ' ', &self.name);
        Ok(())
    }
}

impl Display for ast::SourceUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write_separated(&self.0, f, "\n")
    }
}

impl Display for ast::StringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.unicode {
            f.write_str("unicode")?;
        }
        f.write_char('"')?;
        f.write_str(&self.string)?;
        f.write_char('"')
    }
}

impl Display for ast::StructDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str("struct ")?;
        write_opt!(f, &self.name, ' ');

        f.write_char('{')?;
        write_separated(&self.fields, f, "; ")?;
        if !self.fields.is_empty() {
            f.write_char(';')?;
        }
        f.write_char('}')
    }
}

impl Display for ast::TypeDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str("type ")?;
        std::fmt::Display::fmt(&self.name, f)?;
        f.write_str(" is ")?;
        std::fmt::Display::fmt(&self.ty, f)?;
        f.write_char(';')
    }
}

impl Display for ast::Using {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str("using ")?;

        std::fmt::Display::fmt(&self.list, f)?;

        f.write_str(" for ")?;

        match &self.ty {
            Some(ty) => Display::fmt(ty, f),
            None => f.write_str("*"),
        }?;

        write_opt!(f, ' ', &self.global);
        f.write_char(';')
    }
}

impl Display for ast::UsingFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        std::fmt::Display::fmt(&self.path, f)?;
        write_opt!(f, " as ", &self.oper);
        Ok(())
    }
}

impl Display for ast::VariableDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        // self.ty.map(|e| e.fmt(f));
        write_opt!(f, ' ', &self.storage);
        write_opt!(f, ' ', &self.name);
        Ok(())
    }
}

impl Display for ast::VariableDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if !self.attrs.is_empty() {
            f.write_char(' ')?;
            write_separated(&self.attrs, f, " ")?;
        }
        if let Some(ty) = &self.ty {
            Display::fmt(ty, f)?;
        }
        write_opt!(f, ' ', &self.name);
        write_opt!(f, " = ", &self.initializer);
        f.write_char(';')
    }
}

impl Display for ast::YulBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_char('{')?;
        write_separated(&self.statements, f, " ")?;
        f.write_char('}')
    }
}

impl Display for ast::YulFor {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str("for ")?;
        std::fmt::Display::fmt(&self.init_block, f)?;
        f.write_char(' ')?;
        std::fmt::Display::fmt(&self.condition, f)?;
        f.write_char(' ')?;
        std::fmt::Display::fmt(&self.post_block, f)?;
        f.write_char(' ')?;
        std::fmt::Display::fmt(&self.execution_block, f)
    }
}

impl Display for ast::YulFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        std::fmt::Display::fmt(&self.id, f)?;
        f.write_char('(')?;
        write_separated(&self.arguments, f, ", ")?;
        f.write_char(')')
    }
}

impl Display for ast::YulFunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str("function ")?;
        std::fmt::Display::fmt(&self.id, f)?;
        f.write_char('(')?;
        write_separated(&self.params, f, ", ")?;
        f.write_str(") ")?;

        if !self.returns.is_empty() {
            f.write_str("-> (")?;
            write_separated(&self.returns, f, ", ")?;
            f.write_str(") ")?;
        }

        std::fmt::Display::fmt(&self.body, f)
    }
}

impl Display for ast::YulSwitch {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str("switch ")?;
        std::fmt::Display::fmt(&self.condition, f)?;
        if !self.cases.is_empty() {
            f.write_char(' ')?;
            write_separated(&self.cases, f, " ")?;
        }
        write_opt!(f, " ", &self.default);
        Ok(())
    }
}

impl Display for ast::YulTypedIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        std::fmt::Display::fmt(&self.id, f)?;
        write_opt!(f, ": ", &self.ty);
        Ok(())
    }
}

// enums
impl Display for ast::CatchClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Simple(_, param, block) => {
                f.write_str("catch ")?;
                write_opt!(f, '(', param, ") ");
                std::fmt::Display::fmt(&block, f)
            }
            Self::Named(_, ident, param, block) => {
                f.write_str("catch ")?;
                std::fmt::Display::fmt(&ident, f)?;
                f.write_char('(')?;
                std::fmt::Display::fmt(&param, f)?;
                f.write_str(") ")?;
                std::fmt::Display::fmt(&block, f)
            }
        }
    }
}

impl Display for ast::Comment {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(self.value())
    }
}

impl Display for ast::ContractPart {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::StructDefinition(inner) => Pointer::fmt(&inner, f),
            Self::EnumDefinition(inner) => Pointer::fmt(&inner, f),
            Self::ErrorDefinition(inner) => Pointer::fmt(&inner, f),
            Self::VariableDefinition(inner) => Pointer::fmt(&inner, f),
            Self::FunctionDefinition(inner) => Pointer::fmt(&inner, f),
            Self::TypeDefinition(inner) => Pointer::fmt(&inner, f),
            Self::AnnotationDefinition(inner) => Pointer::fmt(&inner, f),
            Self::Using(inner) => Pointer::fmt(&inner, f),
            Self::StraySemicolon(_) => f.write_char(';'),
        }
    }
}

impl Display for ast::ContractTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(self.as_str())
    }
}

impl ast::ContractTy {
    /// Returns the string representation of this type.
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Abstract(..) => "abstract contract",
            Self::Contract(..) => "contract",
            Self::Interface(..) => "interface",
            Self::Library(..) => "library",
        }
    }
}

impl Display for ast::Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::New(_, expr) => {
                f.write_str("new ")?;
                Pointer::fmt(&expr, f)
            }
            Self::Delete(_, expr) => {
                f.write_str("delete ")?;
                Pointer::fmt(&expr, f)
            }

            Self::Type(_, ty) => std::fmt::Display::fmt(&ty, f),

            Self::Variable(ident) => std::fmt::Display::fmt(&ident, f),

            Self::ArrayLiteral(_, exprs) => {
                f.write_char('[')?;
                write_separated(exprs, f, ", ")?;
                f.write_char(']')
            }
            Self::Initializer(_, exprs) => {
                f.write_char('{')?;
                write_separated(exprs, f, ", ")?;
                f.write_char('}')
            }
            Self::ArraySubscript(_, expr1, expr2) => {
                Pointer::fmt(&expr1, f)?;
                f.write_char('[')?;
                write_opt!(f, expr2);
                f.write_char(']')
            }
            Self::ArraySlice(_, arr, l, r) => {
                Pointer::fmt(&arr, f)?;
                f.write_char('[')?;
                write_opt!(f, l);
                f.write_char(':')?;
                write_opt!(f, r);
                f.write_char(']')
            }

            Self::MemberAccess(_, expr, ident) => {
                Pointer::fmt(&expr, f)?;
                f.write_char('.')?;
                std::fmt::Display::fmt(&ident, f)
            }

            Self::Parenthesis(_, expr) => {
                f.write_char('(')?;
                Pointer::fmt(&expr, f)?;
                f.write_char(')')
            }
            Self::List(_, list) => {
                f.write_char('(')?;
                fmt_parameter_list(list, f)?;
                f.write_char(')')
            }

            Self::AddressLiteral(_, address, bit) => {
                Display::fmt(address, f)?;
                f.write_char(':')?;
                Display::fmt(bit, f)?;
                Ok(())
            }
            Self::BoolLiteral(_, b) => Display::fmt(b, f),
            Self::StringLiteral(vals) => write_separated(vals, f, " "),
            Self::HexLiteral(vals) => write_separated(vals, f, " "),
            Self::HexNumberLiteral(_, val, unit) => {
                // TODO: Check with and write the checksummed address when len == 42
                // ref: https://docs.soliditylang.org/en/latest/types.html#address-literals
                f.write_str(val)?;
                write_opt!(f, ' ', unit);
                Ok(())
            }
            Self::NumberLiteral(_, val) => {
                Display::fmt(val, f)?;
                Ok(())
            }
            Self::RationalNumberLiteral(_, val) => {
                let val = rm_underscores(val);
                f.write_str(&val)?;
                Ok(())
            }

            Self::FunctionCall(_, expr, exprs) => {
                Pointer::fmt(&expr, f)?;
                f.write_char('(')?;
                write_separated(exprs, f, ", ")?;
                f.write_char(')')
            }
            Self::FunctionCallBlock(_, expr, block) => {
                Pointer::fmt(&expr, f)?;
                Pointer::fmt(&block, f)
            }
            Self::NamedFunctionCall(_, expr, args) => {
                Pointer::fmt(&expr, f)?;
                f.write_str("({")?;
                write_separated(args, f, ", ")?;
                f.write_str("})")
            }

            Self::ConditionalOperator(_, cond, l, r) => {
                Pointer::fmt(&cond, f)?;
                f.write_str(" ? ")?;
                Pointer::fmt(&l, f)?;
                f.write_str(" : ")?;
                Pointer::fmt(&r, f)
            }

            Self::PreIncrement(..)
            | Self::PostIncrement(..)
            | Self::PreDecrement(..)
            | Self::PostDecrement(..)
            | Self::Not(..)
            | Self::BitwiseNot(..)
            | Self::UnaryPlus(..)
            | Self::Add(..)
            | Self::Negate(..)
            | Self::Subtract(..)
            | Self::Power(..)
            | Self::Multiply(..)
            | Self::Divide(..)
            | Self::Modulo(..)
            | Self::ShiftLeft(..)
            | Self::ShiftRight(..)
            | Self::BitwiseAnd(..)
            | Self::BitwiseXor(..)
            | Self::BitwiseOr(..)
            | Self::Less(..)
            | Self::More(..)
            | Self::LessEqual(..)
            | Self::MoreEqual(..)
            | Self::And(..)
            | Self::Or(..)
            | Self::Equal(..)
            | Self::NotEqual(..)
            | Self::Assign(..)
            | Self::AssignOr(..)
            | Self::AssignAnd(..)
            | Self::AssignXor(..)
            | Self::AssignShiftLeft(..)
            | Self::AssignShiftRight(..)
            | Self::AssignAdd(..)
            | Self::AssignSubtract(..)
            | Self::AssignMultiply(..)
            | Self::AssignDivide(..)
            | Self::AssignModulo(..) => {
                let (left, right) = self.components();
                let has_spaces = self.has_space_around();

                if let Some(left) = left {
                    std::fmt::Display::fmt(&left, f)?;
                    if has_spaces {
                        f.write_char(' ')?;
                    }
                }

                let operator = self.operator().unwrap();
                f.write_str(operator)?;

                if let Some(right) = right {
                    if has_spaces {
                        f.write_char(' ')?;
                    }
                    std::fmt::Display::fmt(&right, f)?;
                }

                Ok(())
            }
        }
    }
}

impl ast::Expression {
    /// Returns the operator string of this expression, if any.
    #[inline]
    pub const fn operator(&self) -> Option<&'static str> {
        use ast::Expression::*;
        let operator = match self {
            New(..) => "new",
            Delete(..) => "delete",

            PreIncrement(..) | PostIncrement(..) => "++",
            PreDecrement(..) | PostDecrement(..) => "--",

            Not(..) => "!",
            BitwiseNot(..) => "~",
            UnaryPlus(..) | Add(..) => "+",
            Negate(..) | Subtract(..) => "-",
            Power(..) => "**",
            Multiply(..) => "*",
            Divide(..) => "/",
            Modulo(..) => "%",
            ShiftLeft(..) => "<<",
            ShiftRight(..) => ">>",
            BitwiseAnd(..) => "&",
            BitwiseXor(..) => "^",
            BitwiseOr(..) => "|",

            Less(..) => "<",
            More(..) => ">",
            LessEqual(..) => "<=",
            MoreEqual(..) => ">=",
            And(..) => "&&",
            Or(..) => "||",
            Equal(..) => "==",
            NotEqual(..) => "!=",

            Assign(..) => "=",
            AssignOr(..) => "|=",
            AssignAnd(..) => "&=",
            AssignXor(..) => "^=",
            AssignShiftLeft(..) => "<<=",
            AssignShiftRight(..) => ">>=",
            AssignAdd(..) => "+=",
            AssignSubtract(..) => "-=",
            AssignMultiply(..) => "*=",
            AssignDivide(..) => "/=",
            AssignModulo(..) => "%=",

            MemberAccess(..)
            | ArraySubscript(..)
            | ArraySlice(..)
            | FunctionCall(..)
            | FunctionCallBlock(..)
            | NamedFunctionCall(..)
            | ConditionalOperator(..)
            | NumberLiteral(..)
            | RationalNumberLiteral(..)
            | HexNumberLiteral(..)
            | StringLiteral(..)
            | Type(..)
            | HexLiteral(..)
            | BoolLiteral(..)
            | AddressLiteral(..)
            | Variable(..)
            | List(..)
            | ArrayLiteral(..)
            | Initializer(..)
            | Parenthesis(..) => return None,
        };
        Some(operator)
    }
}

impl Display for ast::FunctionAttribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Mutability(mutability) => std::fmt::Display::fmt(&mutability, f),
            Self::Visibility(visibility) => std::fmt::Display::fmt(&visibility, f),
            Self::Virtual(_) => f.write_str("virtual"),
            Self::Immutable(_) => f.write_str("immutable"),
            Self::Override(_, idents) => {
                f.write_str("override")?;
                if !idents.is_empty() {
                    f.write_char('(')?;
                    write_separated(idents, f, ", ")?;
                    f.write_char(')')?;
                }
                Ok(())
            }
            Self::BaseOrModifier(_, base) => std::fmt::Display::fmt(&base, f),
            Self::Error(_) => Ok(()),
        }
    }
}

impl Display for ast::FunctionTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(self.as_str())
    }
}

impl ast::FunctionTy {
    /// Returns the string representation of this type.
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Constructor => "constructor",
            Self::Function => "function",
            Self::Fallback => "fallback",
            Self::Receive => "receive",
            Self::Modifier => "modifier",
        }
    }
}

impl Display for ast::Import {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Plain(lit, _) => {
                f.write_str("import ")?;
                std::fmt::Display::fmt(&lit, f)?;
                f.write_char(';')
            }
            Self::GlobalSymbol(lit, ident, _) => {
                f.write_str("import ")?;
                std::fmt::Display::fmt(&lit, f)?;
                f.write_str(" as ")?;
                std::fmt::Display::fmt(&ident, f)?;
                f.write_char(';')
            }
            Self::Rename(lit, idents, _) => {
                f.write_str("import {")?;

                // same as `write_separated_iter`
                let mut idents = idents.iter();
                if let Some((ident, as_ident)) = idents.next() {
                    std::fmt::Display::fmt(&ident, f)?;
                    write_opt!(f, " as ", as_ident);
                    for (ident, as_ident) in idents {
                        f.write_str(", ")?;
                        std::fmt::Display::fmt(&ident, f)?;
                        write_opt!(f, " as ", as_ident);
                    }
                }
                f.write_str("} from ")?;
                std::fmt::Display::fmt(&lit, f)?;
                f.write_char(';')
            }
        }
    }
}

impl Display for ast::ImportPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Filename(lit) => std::fmt::Display::fmt(&lit, f),
            Self::Path(path) => std::fmt::Display::fmt(&path, f),
        }
    }
}

impl Display for ast::Mutability {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(self.as_str())
    }
}

impl ast::Mutability {
    /// Returns the string representation of this type.
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Pure(_) => "pure",
            Self::Constant(_) | Self::View(_) => "view",
            Self::Payable(_) => "payable",
        }
    }
}

impl Display for ast::SourceUnitPart {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::ImportDirective(inner) => Pointer::fmt(&inner, f),
            Self::EnumDefinition(inner) => Pointer::fmt(&inner, f),
            Self::StructDefinition(inner) => Pointer::fmt(&inner, f),
            Self::ErrorDefinition(inner) => Pointer::fmt(&inner, f),
            Self::FunctionDefinition(inner) => Pointer::fmt(&inner, f),
            Self::VariableDefinition(inner) => Pointer::fmt(&inner, f),
            Self::TypeDefinition(inner) => Pointer::fmt(&inner, f),
            Self::AnnotationDefinition(inner) => Pointer::fmt(&inner, f),
            Self::Using(inner) => Pointer::fmt(&inner, f),
            Self::PragmaDirective(_, ident, lit) => {
                f.write_str("pragma")?;
                write_opt!(f, ' ', ident);
                // this isn't really a string literal, it's just parsed as one by the lexer
                write_opt!(f, ' ', lit.as_ref().map(|lit| &lit.string));
                f.write_char(';')
            }
            Self::StraySemicolon(_) => f.write_char(';'),
        }
    }
}

impl Display for ast::Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Block {
                unchecked,
                statements,
                ..
            } => {
                if *unchecked {
                    f.write_str("unchecked ")?;
                }

                f.write_char('{')?;
                write_separated(statements, f, " ")?;
                f.write_char('}')
            }
            Self::Assembly {
                dialect,
                flags,
                block,
                ..
            } => {
                f.write_str("assembly ")?;
                write_opt!(f, dialect, ' ');
                if let Some(flags) = flags {
                    if !flags.is_empty() {
                        f.write_char('(')?;
                        write_separated(flags, f, ", ")?;
                        f.write_str(") ")?;
                    }
                }
                std::fmt::Display::fmt(&block, f)
            }
            Self::Args(_, args) => {
                f.write_char('{')?;
                write_separated(args, f, ", ")?;
                f.write_char('}')
            }
            Self::If(_, cond, block, end_block) => {
                f.write_str("if (")?;
                std::fmt::Display::fmt(&cond, f)?;
                f.write_str(") ")?;
                Pointer::fmt(&block, f)?;
                write_opt!(f, " else ", end_block);
                Ok(())
            }
            Self::While(_, cond, block) => {
                f.write_str("while (")?;
                std::fmt::Display::fmt(&cond, f)?;
                f.write_str(") ")?;
                Pointer::fmt(&block, f)
            }
            Self::Expression(_, expr) => std::fmt::Display::fmt(&expr, f),
            Self::VariableDefinition(_, var, expr) => {
                std::fmt::Display::fmt(&var, f)?;
                write_opt!(f, " = ", expr);
                f.write_char(';')
            }
            Self::For(_, init, cond, expr, block) => {
                f.write_str("for (")?;
                // edge case, don't write semicolon on a variable definition
                match init.as_deref() {
                    Some(var @ ast::Statement::VariableDefinition(..)) => {
                        std::fmt::Display::fmt(&var, f)
                    }
                    Some(stmt) => {
                        std::fmt::Display::fmt(&stmt, f)?;
                        f.write_char(';')
                    }
                    None => f.write_char(';'),
                }?;
                write_opt!(f, ' ', cond);
                f.write_char(';')?;
                write_opt!(f, ' ', expr);
                f.write_str(") ")?;
                if let Some(block) = block {
                    Pointer::fmt(&block, f)
                } else {
                    f.write_char(';')
                }
            }
            Self::DoWhile(_, block, cond) => {
                f.write_str("do ")?;
                Pointer::fmt(&block, f)?;
                f.write_str(" while (")?;
                std::fmt::Display::fmt(&cond, f)?;
                f.write_str(");")
            }
            Self::Continue(_) => f.write_str("continue;"),
            Self::Break(_) => f.write_str("break;"),
            Self::Return(_, expr) => {
                f.write_str("return")?;
                write_opt!(f, ' ', expr);
                f.write_char(';')
            }
            Self::Revert(_, ident, exprs) => {
                f.write_str("revert")?;
                write_opt!(f, ' ', ident);
                f.write_char('(')?;
                write_separated(exprs, f, ", ")?;
                f.write_str(");")
            }
            Self::RevertNamedArgs(_, ident, args) => {
                f.write_str("revert")?;
                write_opt!(f, ' ', ident);
                f.write_char('(')?;
                if !args.is_empty() {
                    f.write_char('{')?;
                    write_separated(args, f, ", ")?;
                    f.write_char('}')?;
                }
                f.write_str(");")
            }
            Self::Emit(_, expr) => {
                f.write_str("emit ")?;
                std::fmt::Display::fmt(&expr, f)?;
                f.write_char(';')
            }
            Self::Try(_, expr, returns, catch) => {
                f.write_str("try ")?;
                std::fmt::Display::fmt(&expr, f)?;

                if let Some((list, stmt)) = returns {
                    f.write_str(" returns (")?;
                    fmt_parameter_list(list, f)?;
                    f.write_str(") ")?;
                    Pointer::fmt(&stmt, f)?;
                }

                if !catch.is_empty() {
                    f.write_char(' ')?;
                    write_separated(catch, f, " ")?;
                }
                Ok(())
            }
            Self::Error(_) => Ok(()),
        }
    }
}

impl Display for ast::StorageLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(self.as_str())
    }
}

impl ast::StorageLocation {
    /// Returns the string representation of this type.
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Memory(_) => "memory",
            Self::Storage(_) => "storage",
            Self::Calldata(_) => "calldata",
        }
    }
}

impl Display for ast::Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Address => f.write_str("address"),
            Self::AddressPayable => f.write_str("address payable"),
            Self::Payable => f.write_str("payable"),
            Self::Bool => f.write_str("bool"),
            Self::String => f.write_str("string"),
            Self::Rational => f.write_str("fixed"),
            Self::DynamicBytes => f.write_str("bytes"),
            Self::Bytes(n) => {
                f.write_str("bytes")?;
                std::fmt::Display::fmt(&n, f)
            }
            Self::Int(n) => {
                f.write_str("int")?;
                std::fmt::Display::fmt(&n, f)
            }
            Self::Uint(n) => {
                f.write_str("uint")?;
                std::fmt::Display::fmt(&n, f)
            }
            Self::Mapping {
                key,
                key_name,
                value,
                value_name,
                ..
            } => {
                f.write_str("mapping(")?;

                Pointer::fmt(&key, f)?;
                write_opt!(f, ' ', key_name);

                f.write_str(" => ")?;

                Pointer::fmt(&value, f)?;
                write_opt!(f, ' ', value_name);

                f.write_char(')')
            }
            Self::Function {
                params,
                attributes,
                returns,
            } => {
                f.write_str("function (")?;
                fmt_parameter_list(params, f)?;
                f.write_char(')')?;

                if !attributes.is_empty() {
                    f.write_char(' ')?;
                    write_separated(attributes, f, " ")?;
                }

                if let Some((returns, attrs)) = returns {
                    if !attrs.is_empty() {
                        f.write_char(' ')?;
                        write_separated(attrs, f, " ")?;
                    }

                    if !returns.is_empty() {
                        f.write_str(" returns (")?;
                        fmt_parameter_list(returns, f)?;
                        f.write_char(')')?;
                    }
                }
                Ok(())
            }
            Type::Alias(name) => {
                f.write_str(" = ")?;
                std::fmt::Display::fmt(&name, f)
            }
            Type::Array {
                element_type,
                element_count,
                loc: _,
            } => {
                f.write_str("[ ")?;
                std::fmt::Display::fmt(&element_count, f)?;
                f.write_str(" : ")?;
                Pointer::fmt(element_type, f)?;
                f.write_str(" ]")
            }
        }
    }
}

impl Display for ast::UserDefinedOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(self.as_str())
    }
}

impl ast::UserDefinedOperator {
    /// Returns the string representation of this type.
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::BitwiseAnd => "&",
            Self::BitwiseNot => "~",
            Self::Negate => "-",
            Self::BitwiseOr => "|",
            Self::BitwiseXor => "^",
            Self::Add => "+",
            Self::Divide => "/",
            Self::Modulo => "%",
            Self::Multiply => "*",
            Self::Subtract => "-",
            Self::Equal => "==",
            Self::More => ">",
            Self::MoreEqual => ">=",
            Self::Less => "<",
            Self::LessEqual => "<=",
            Self::NotEqual => "!=",
        }
    }
}

impl Display for ast::UsingList {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Library(ident) => std::fmt::Display::fmt(&ident, f),
            Self::Functions(list) => {
                f.write_char('{')?;
                write_separated(list, f, ", ")?;
                f.write_char('}')
            }
            Self::Error => Ok(()),
        }
    }
}

impl Display for ast::VariableAttribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Visibility(vis) => std::fmt::Display::fmt(&vis, f),
            Self::Constant(_) => f.write_str("const"),
            Self::Readable(_) => f.write_str("ro"),
            Self::Writable(_) => f.write_str("wo"),
            Self::Portable(_) => f.write_str("port"),
        }
    }
}

impl Display for ast::Visibility {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(self.as_str())
    }
}

impl ast::Visibility {
    /// Returns the string representation of this type.
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Public(_) => "public",
            Self::Internal(_) => "internal",
            Self::Private(_) => "private",
            Self::External(_) => "external",
        }
    }
}

impl Display for ast::YulExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::BoolLiteral(_, value, ident) => {
                let value = if *value { "true" } else { "false" };
                f.write_str(value)?;
                write_opt!(f, ": ", ident);
                Ok(())
            }
            Self::NumberLiteral(_, value, exponent, ident) => {
                f.write_str(value)?;
                if !exponent.is_empty() {
                    f.write_char('e')?;
                    f.write_str(exponent)?;
                }
                write_opt!(f, ": ", ident);
                Ok(())
            }
            Self::HexNumberLiteral(_, value, ident) => {
                f.write_str(value)?;
                write_opt!(f, ": ", ident);
                Ok(())
            }
            Self::HexStringLiteral(value, ident) => {
                std::fmt::Display::fmt(&value, f)?;
                write_opt!(f, ": ", ident);
                Ok(())
            }
            Self::StringLiteral(value, ident) => {
                std::fmt::Display::fmt(&value, f)?;
                write_opt!(f, ": ", ident);
                Ok(())
            }
            Self::Variable(ident) => std::fmt::Display::fmt(&ident, f),
            Self::FunctionCall(call) => Pointer::fmt(&call, f),
            Self::SuffixAccess(_, l, r) => {
                Pointer::fmt(&l, f)?;
                f.write_char('.')?;
                Pointer::fmt(&r, f)
            }
        }
    }
}

impl Display for ast::YulStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Block(inner) => std::fmt::Display::fmt(&inner, f),
            Self::FunctionDefinition(inner) => Pointer::fmt(&inner, f),
            Self::FunctionCall(inner) => Pointer::fmt(&inner, f),
            Self::For(inner) => std::fmt::Display::fmt(&inner, f),
            Self::Switch(inner) => std::fmt::Display::fmt(&inner, f),

            Self::Assign(_, exprs, eq_expr) => {
                write_separated(exprs, f, ", ")?;
                f.write_str(" := ")?;
                std::fmt::Display::fmt(&eq_expr, f)
            }
            Self::VariableDeclaration(_, vars, eq_expr) => {
                f.write_str("let")?;
                if !vars.is_empty() {
                    f.write_char(' ')?;
                    write_separated(vars, f, ", ")?;
                }
                write_opt!(f, " := ", eq_expr);
                Ok(())
            }

            Self::If(_, expr, block) => {
                f.write_str("if ")?;
                std::fmt::Display::fmt(&expr, f)?;
                f.write_char(' ')?;
                std::fmt::Display::fmt(&block, f)
            }

            Self::Leave(_) => f.write_str("leave"),
            Self::Break(_) => f.write_str("break"),
            Self::Continue(_) => f.write_str("continue"),

            Self::Error(_) => Ok(()),
        }
    }
}

impl Display for ast::YulSwitchOptions {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Case(_, expr, block) => {
                f.write_str("case ")?;
                std::fmt::Display::fmt(&expr, f)?;
                f.write_str(" ")?;
                std::fmt::Display::fmt(&block, f)
            }
            Self::Default(_, block) => {
                f.write_str("default ")?;
                std::fmt::Display::fmt(&block, f)
            }
        }
    }
}

// These functions are private so they should be inlined by the compiler.
// We provided these `#[inline]` hints regardless because we don't expect compile time penalties
// or other negative impacts from them.
// See: <https://github.com/hyperledger/solang/pull/1237#discussion_r1151557453>
#[inline]
fn fmt_parameter_list(list: &ast::ParameterList, f: &mut Formatter<'_>) -> Result {
    let iter = list.iter().flat_map(|(_, param)| param);
    write_separated_iter(iter, f, ", ")
}

#[inline]
fn write_separated<T: Display>(slice: &[T], f: &mut Formatter<'_>, sep: &str) -> Result {
    write_separated_iter(slice.iter(), f, sep)
}

fn write_separated_iter<T, I>(mut iter: I, f: &mut Formatter<'_>, sep: &str) -> Result
where
    I: Iterator<Item = T>,
    T: Display,
{
    if let Some(first) = iter.next() {
        first.fmt(f)?;
        for item in iter {
            f.write_str(sep)?;
            item.fmt(f)?;
        }
    }
    Ok(())
}

fn rm_underscores(s: &str) -> Cow<'_, str> {
    if s.is_empty() {
        Cow::Borrowed("0")
    } else if s.contains('_') {
        let mut s = s.to_string();
        s.retain(|c| c != '_');
        Cow::Owned(s)
    } else {
        Cow::Borrowed(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! struct_tests {
        ($(ast::$t:ident { $( $f:ident: $e:expr ),* $(,)? } => $expected:expr),* $(,)?) => {
            $(
                assert_eq_display(
                    ast::$t {
                        loc: loc!(),
                        $( $f: $e, )*
                    },
                    $expected,
                );
            )*
        };
    }

    macro_rules! enum_tests {
        ($(
            $t:ty: {
                $($p:expr => $expected:expr,)+
            }
        )+) => {
            $(
                $(
                    assert_eq_display($p, $expected);
                )+
            )+
        };
    }

    /// Expression
    macro_rules! expr {
        (this) => {
            ast::Expression::This(loc!())
        };

        ($i:ident) => {
            ast::Expression::Variable(id(stringify!($i)))
        };

        ($l:literal) => {
            ast::Expression::Variable(id(stringify!($l)))
        };

        (++ $($t:tt)+) => {
            ast::Expression::PreIncrement(loc!(), Box::new(expr!($($t)+)))
        };

        ($($t:tt)+ ++) => {
            ast::Expression::PostIncrement(loc!(), Box::new(expr!($($t)+)))
        };
    }
    macro_rules! yexpr {
        ($i:ident) => {
            ast::YulExpression::Variable(id(stringify!($i)))
        };
        ($l:literal) => {
            ast::YulExpression::Variable(id(stringify!($l)))
        };
    }

    /// Type
    macro_rules! ty {
        (uint256) => {
            ast::Type::Uint(256)
        };
        (string) => {
            ast::Type::String
        };
        (bytes) => {
            ast::Type::DynamicBytes
        };
        (address) => {
            ast::Type::Address
        };
    }
    macro_rules! expr_ty {
        ($($t:tt)+) => {
            ast::Expression::Type(loc!(), ty!($($t)+))
        };
    }

    /// Literals
    macro_rules! lit {
        // prefixes are not allowed in rust strings
        (unicode $($l:literal)+) => {
            ast::StringLiteral {
                loc: loc!(),
                unicode: true,
                string: concat!( $($l),+ ).to_string(),
            }
        };

        (hex $($l:literal)+) => {
            ast::HexLiteral {
                loc: loc!(),
                hex: concat!( "hex\"", $($l),+ , "\"" ).to_string(),
            }
        };

        ($($l:literal)+) => {
            ast::StringLiteral {
                loc: loc!(),
                unicode: false,
                string: concat!( $($l),+ ).to_string(),
            }
        };
    }

    /// Statement
    macro_rules! stmt {
        ( {} ) => {
            ast::Statement::Block {
                loc: loc!(),
                unchecked: false,
                statements: vec![],
            }
        };

        ( unchecked { $($t:tt)* } ) => {
            ast::Statement::Block {
                loc: loc!(),
                unchecked: true,
                statements: vec![stmt!($(t)*)],
            }
        };
        ( { $($t:tt)* } ) => {
            ast::Statement::Block {
                loc: loc!(),
                unchecked: false,
                statements: vec![stmt!($(t)*)],
            }
        };
    }

    /// IdentifierPath
    macro_rules! idp {
        ($($e:expr),* $(,)?) => {
            ast::IdentifierPath {
                loc: loc!(),
                identifiers: vec![$(id($e)),*],
            }
        };
    }

    macro_rules! loc {
        () => {
            ast::Loc::Source(0, 0, 0)
        };
    }

    /// Param
    macro_rules! param {
        ($i:ident) => {
            ast::Parameter {
                loc: loc!(),
                ty: expr_ty!($i),
                storage: None,
                name: None,
                annotation: None,
            }
        };

        ($i:ident $n:ident) => {
            ast::Parameter {
                loc: loc!(),
                ty: expr_ty!($i),
                storage: None,
                name: Some(id(stringify!($n))),
                annotation: None,
            }
        };

        ($i:ident $s:ident $n:ident) => {
            ast::Parameter {
                loc: loc!(),
                ty: expr_ty!($i),
                storage: Some(storage!($s)),
                name: Some(id(stringify!($n))),
                annotation: None,
            }
        };
    }

    macro_rules! storage {
        (memory) => {
            ast::StorageLocation::Memory(loc!())
        };
        (storage) => {
            ast::StorageLocation::Storage(loc!())
        };
        (calldata) => {
            ast::StorageLocation::Calldata(loc!())
        };
    }

    /// Identifier
    fn id(s: &str) -> ast::Identifier {
        ast::Identifier {
            loc: loc!(),
            name: s.to_string(),
        }
    }

    macro_rules! yid {
        ($i:ident) => {
            ast::YulTypedIdentifier {
                loc: loc!(),
                id: id(stringify!($i)),
                ty: None,
            }
        };

        ($i:ident : $t:ident) => {
            ast::YulTypedIdentifier {
                loc: loc!(),
                id: id(stringify!($i)),
                ty: Some(id(stringify!($t))),
            }
        };
    }

    fn var(s: &str) -> Box<ast::Expression> {
        Box::new(ast::Expression::Variable(id(s)))
    }

    fn yul_block() -> ast::YulBlock {
        ast::YulBlock {
            loc: loc!(),
            statements: vec![],
        }
    }

    fn assert_eq_display<T: Display + std::fmt::Debug>(item: T, expected: &str) {
        let ty = std::any::type_name::<T>();
        let actual = item.to_string();
        assert_eq!(actual, expected, "\"{ty}\": {item:?}");
        // TODO: Test parsing back into an item
        // let parsed = ;
        // assert_eq!(parsed, item, "failed to parse display back into an item: {expected}");
    }

    #[test]
    fn display_structs_simple() {
        struct_tests![
            ast::Base {
                name: idp!("id", "path"),
                args: None,
            } => "id.path",
            ast::Base {
                name: idp!("id", "path"),
                args: Some(vec![expr!(value)]),
            } => "id.path(value)",
            ast::Base {
                name: idp!("id", "path"),
                args: Some(vec![expr!(value1), expr!(value2)]),
            } => "id.path(value1, value2)",

            ast::ErrorParameter {
                ty: expr_ty!(uint256),
                name: None,
            } => "uint256",
            ast::ErrorParameter {
                ty: expr_ty!(uint256),
                name: Some(id("name")),
            } => "uint256 name",

            ast::HexLiteral {
                hex: "hex\"1234\"".into(),
            } => "hex\"1234\"",
            ast::HexLiteral {
                hex: "hex\"455318975130845\"".into(),
            } => "hex\"455318975130845\"",

            ast::Identifier {
                name: "name".to_string(),
            } => "name",

            ast::IdentifierPath {
                identifiers: vec![id("id")],
            } => "id",
            ast::IdentifierPath {
                identifiers: vec![id("id"), id("path")],
            } => "id.path",
            ast::IdentifierPath {
                identifiers: vec![id("long"), id("id"), id("path")],
            } => "long.id.path",

            ast::NamedArgument {
                name: id("name"),
                expr: expr!(expr),
            } => "name: expr",

            ast::Parameter {
                ty: expr_ty!(uint256),
                storage: None,
                name: None,
                annotation: None,
            } => "uint256",
            ast::Parameter {
                ty: expr_ty!(uint256),
                storage: None,
                name: Some(id("name")),
                annotation: None,
            } => "uint256 name",
            ast::Parameter {
                ty: expr_ty!(uint256),
                storage: Some(ast::StorageLocation::Calldata(Default::default())),
                name: Some(id("name")),
                annotation: None,
            } => "uint256 calldata name",
            ast::Parameter {
                ty: expr_ty!(uint256),
                storage: Some(ast::StorageLocation::Calldata(Default::default())),
                name: None,
                annotation: None,
            } => "uint256 calldata",

            ast::StringLiteral {
                unicode: false,
                string: "string".into(),
            } => "\"string\"",
            ast::StringLiteral {
                unicode: true,
                string: "string".into(),
            } => "unicode\"string\"",

            ast::UsingFunction {
                path: idp!["id", "path"],
                oper: None,
            } => "id.path",
            ast::UsingFunction {
                path: idp!["id", "path"],
                oper: Some(ast::UserDefinedOperator::Add),
            } => "id.path as +",

            ast::VariableDeclaration {
                ty: None,
                storage: None,
                name: None,
            } => "none"
        ];
    }

    #[test]
    fn display_structs_complex() {
        struct_tests![
            ast::EnumDefinition {
                name: Some(id("name")),
                values: vec![],
                annotations: vec![]
            } => "enum name {}",
            ast::EnumDefinition {
                name: Some(id("name")),
                values: vec![Some(id("variant"))],
                annotations: vec![]
            } => "enum name {variant}",
            ast::EnumDefinition {
                name: Some(id("name")),
                values: vec![
                    Some(id("variant1")),
                    Some(id("variant2")),
                ],
                annotations: vec![]
            } => "enum name {variant1, variant2}",

            ast::ErrorDefinition {
                keyword: expr!(error),
                name: Some(id("name")),
                fields: vec![],
            } => "error name();",
            ast::ErrorDefinition {
                keyword: expr!(error),
                name: Some(id("name")),
                fields: vec![ast::ErrorParameter {
                    loc: loc!(),
                    ty: expr_ty!(uint256),
                    name: None,
                }],
            } => "error name(uint256);",

            ast::FunctionDefinition {
                ty: ast::FunctionTy::Function,
                name: Some(id("name")),
                name_loc: loc!(),
                params: vec![],
                attributes: vec![],
                return_not_returns: None,
                returns: vec![],
                body: None,
            } => "function name();",
            ast::FunctionDefinition {
                ty: ast::FunctionTy::Function,
                name: Some(id("name")),
                name_loc: loc!(),
                params: vec![],
                attributes: vec![],
                return_not_returns: None,
                returns: vec![],
                body: Some(stmt!({})),
            } => "function name() {}",
            ast::FunctionDefinition {
                ty: ast::FunctionTy::Function,
                name: Some(id("name")),
                name_loc: loc!(),
                params: vec![],
                attributes: vec![],
                return_not_returns: None,
                returns: vec![(loc!(), Some(param!(uint256)))],
                body: Some(stmt!({})),
            } => "function name() returns (uint256) {}",
            ast::FunctionDefinition {
                ty: ast::FunctionTy::Function,
                name: Some(id("name")),
                name_loc: loc!(),
                params: vec![],
                attributes: vec![ast::FunctionAttribute::Virtual(loc!())],
                return_not_returns: None,
                returns: vec![(loc!(), Some(param!(uint256)))],
                body: Some(stmt!({})),
            } => "function name() virtual returns (uint256) {}",

            ast::StructDefinition {
                name: Some(id("name")),
                fields: vec![],
                annotations: vec![]
            } => "struct name {}",
            ast::StructDefinition {
                name: Some(id("name")),
                fields: vec![ast::VariableDeclaration {
                    loc: loc!(),
                    ty: None,
                    storage: None,
                    name: Some(id("a")),
                }],
                annotations: vec![]
            } => "struct name {uint256 a;}",
            ast::StructDefinition {
                name: Some(id("name")),
                fields: vec![
                    ast::VariableDeclaration {
                        loc: loc!(),
                        ty: None,
                        storage: None,
                        name: Some(id("a")),
                    },
                    ast::VariableDeclaration {
                        loc: loc!(),
                        // ty: expr_ty!(uint256),
                        ty: None,
                        storage: None,
                        name: Some(id("b")),
                    }
                ],
                annotations: vec![]
            } => "struct name {uint256 a; uint256 b;}",

            // ast::TypeDefinition {
            //     name: id("MyType"),
            //     ty: expr_ty!(uint256),
            // } => "type MyType is uint256;",

            ast::YulBlock {
                statements: vec![]
            } => "{}",

            ast::YulFor {
                init_block: yul_block(),
                condition: yexpr!(cond),
                post_block: yul_block(),
                execution_block: yul_block(),
            } => "for {} cond {} {}",

            ast::YulFunctionCall {
                id: id("name"),
                arguments: vec![],
            } => "name()",
            ast::YulFunctionCall {
                id: id("name"),
                arguments: vec![yexpr!(arg)],
            } => "name(arg)",
            ast::YulFunctionCall {
                id: id("name"),
                arguments: vec![yexpr!(arg1), yexpr!(arg2)],
            } => "name(arg1, arg2)",

            ast::YulFunctionDefinition {
                id: id("name"),
                params: vec![],
                returns: vec![],
                body: yul_block(),
            } => "function name() {}",
            ast::YulFunctionDefinition {
                id: id("name"),
                params: vec![yid!(param1: a), yid!(param2: b)],
                returns: vec![],
                body: yul_block(),
            } => "function name(param1: a, param2: b) {}",
            ast::YulFunctionDefinition {
                id: id("name"),
                params: vec![yid!(param1: a), yid!(param2: b)],
                returns: vec![yid!(ret1: c), yid!(ret2: d)],
                body: yul_block(),
            } => "function name(param1: a, param2: b) -> (ret1: c, ret2: d) {}",

            ast::YulSwitch {
                condition: yexpr!(cond),
                cases: vec![ast::YulSwitchOptions::Case(loc!(), yexpr!(expr), yul_block())],
                default: None,
            } => "switch cond case expr {}",
            ast::YulSwitch {
                condition: yexpr!(cond),
                cases: vec![
                    ast::YulSwitchOptions::Case(loc!(), yexpr!(0), yul_block()),
                    ast::YulSwitchOptions::Case(loc!(), yexpr!(1), yul_block()),
                ],
                default: None,
            } => "switch cond case 0 {} case 1 {}",
            ast::YulSwitch {
                condition: yexpr!(cond),
                cases: vec![ast::YulSwitchOptions::Case(loc!(), yexpr!(0), yul_block())],
                default: Some(ast::YulSwitchOptions::Default(loc!(), yul_block())),
            } => "switch cond case 0 {} default {}",
        ];
    }

    #[test]
    fn display_enums() {
        enum_tests![
            ast::Comment: {
                ast::Comment::Line(loc!(), "// line".into()) => "// line",
                ast::Comment::Block(loc!(), "/* \nblock\n*/".into()) => "/* \nblock\n*/",
                ast::Comment::DocLine(loc!(), "/// doc line".into()) => "/// doc line",
                ast::Comment::DocBlock(loc!(), "/**\n * doc block\n */".into()) => "/**\n * doc block\n */",
            }

            ast::Expression: {
                ast::Expression::New(loc!(), Box::new(expr_ty!(uint256))) => "new uint256",
                ast::Expression::Delete(loc!(), Box::new(expr_ty!(uint256))) => "delete uint256",

                ast::Expression::Type(loc!(), ty!(uint256)) => "uint256",
                ast::Expression::Variable(id("myVar")) => "myVar",

                ast::Expression::ArrayLiteral(loc!(), vec![expr!(1), expr!(2)]) => "[1, 2]",

                ast::Expression::ArraySubscript(loc!(), Box::new(expr!(arr)), None) => "arr[]",
                ast::Expression::ArraySubscript(loc!(), Box::new(expr!(arr)), Some(Box::new(expr!(0)))) => "arr[0]",
                ast::Expression::ArraySlice(loc!(), Box::new(expr!(arr)), None, None) => "arr[:]",
                ast::Expression::ArraySlice(loc!(), Box::new(expr!(arr)), Some(Box::new(expr!(left))), None)
                    => "arr[left:]",
                ast::Expression::ArraySlice(loc!(), Box::new(expr!(arr)), None, Some(Box::new(expr!(right))))
                    => "arr[:right]",
                ast::Expression::ArraySlice(loc!(), Box::new(expr!(arr)), Some(Box::new(expr!(left))), Some(Box::new(expr!(right))))
                    => "arr[left:right]",

                ast::Expression::MemberAccess(loc!(), Box::new(expr!(struct)), id("access")) => "struct.access",

                ast::Expression::Parenthesis(loc!(), Box::new(expr!(var))) => "(var)",
                ast::Expression::List(loc!(), vec![]) => "()",
                ast::Expression::List(loc!(), vec![(loc!(), Some(param!(address)))])
                    => "(address)",
                ast::Expression::List(loc!(), vec![(loc!(), Some(param!(address))), (loc!(), Some(param!(uint256)))])
                    => "(address, uint256)",

                ast::Expression::AddressLiteral(loc!(), "0x1234".parse().unwrap(), 0) => "0x1234",
                ast::Expression::StringLiteral(vec![lit!(unicode "¹²³")]) => "unicode\"¹²³\"",
                ast::Expression::HexLiteral(vec![lit!(hex "00112233")]) => "hex\"00112233\"",

                ast::Expression::HexNumberLiteral(loc!(), "0x1234".into(), None) => "0x1234",
                ast::Expression::HexNumberLiteral(loc!(), "0x1234".into(), Some(id("gwei"))) => "0x1234 gwei",
                ast::Expression::NumberLiteral(loc!(), "1234".parse().unwrap())
                    => "1234",
                ast::Expression::RationalNumberLiteral(loc!(), ".9".into())
                    => "0.9",
                ast::Expression::FunctionCall(loc!(), Box::new(expr!(func)), vec![]) => "func()",
                ast::Expression::FunctionCall(loc!(), Box::new(expr!(func)), vec![expr!(arg)])
                    => "func(arg)",
                ast::Expression::FunctionCall(loc!(), Box::new(expr!(func)), vec![expr!(arg1), expr!(arg2)])
                    => "func(arg1, arg2)",
                ast::Expression::FunctionCallBlock(loc!(), Box::new(expr!(func)), Box::new(stmt!({})))
                    => "func{}",
                ast::Expression::NamedFunctionCall(loc!(), Box::new(expr!(func)), vec![])
                    => "func({})",
                ast::Expression::NamedFunctionCall(loc!(), Box::new(expr!(func)), vec![ast::NamedArgument {
                    loc: loc!(),
                    name: id("arg"),
                    expr: expr!(value),
                }]) => "func({arg: value})",
                ast::Expression::NamedFunctionCall(loc!(), Box::new(expr!(func)), vec![
                    ast::NamedArgument {
                        loc: loc!(),
                        name: id("arg1"),
                        expr: expr!(value1),
                    },
                    ast::NamedArgument {
                        loc: loc!(),
                        name: id("arg2"),
                        expr: expr!(value2),
                    }
                ]) => "func({arg1: value1, arg2: value2})",

                ast::Expression::PreIncrement(loc!(), var("a")) => "++a",
                ast::Expression::PostIncrement(loc!(), var("a")) => "a++",
                ast::Expression::PreDecrement(loc!(), var("a")) => "--a",
                ast::Expression::PostDecrement(loc!(), var("a")) => "a--",
                ast::Expression::Not(loc!(), var("a")) => "!a",
                ast::Expression::BitwiseNot(loc!(), var("a")) => "~a",
                ast::Expression::UnaryPlus(loc!(), var("a")) => "+a",
                ast::Expression::Negate(loc!(), var("a")) => "-a",

                ast::Expression::Add(loc!(), var("a"), var("b")) => "a + b",
                ast::Expression::Subtract(loc!(), var("a"), var("b")) => "a - b",
                ast::Expression::Power(loc!(), var("a"), var("b")) => "a ** b",
                ast::Expression::Multiply(loc!(), var("a"), var("b")) => "a * b",
                ast::Expression::Divide(loc!(), var("a"), var("b")) => "a / b",
                ast::Expression::Modulo(loc!(), var("a"), var("b")) => "a % b",
                ast::Expression::ShiftLeft(loc!(), var("a"), var("b")) => "a << b",
                ast::Expression::ShiftRight(loc!(), var("a"), var("b")) => "a >> b",
                ast::Expression::BitwiseAnd(loc!(), var("a"), var("b")) => "a & b",
                ast::Expression::BitwiseXor(loc!(), var("a"), var("b")) => "a ^ b",
                ast::Expression::BitwiseOr(loc!(), var("a"), var("b")) => "a | b",
                ast::Expression::Less(loc!(), var("a"), var("b")) => "a < b",
                ast::Expression::More(loc!(), var("a"), var("b")) => "a > b",
                ast::Expression::LessEqual(loc!(), var("a"), var("b")) => "a <= b",
                ast::Expression::MoreEqual(loc!(), var("a"), var("b")) => "a >= b",
                ast::Expression::And(loc!(), var("a"), var("b")) => "a && b",
                ast::Expression::Or(loc!(), var("a"), var("b")) => "a || b",
                ast::Expression::Equal(loc!(), var("a"), var("b")) => "a == b",
                ast::Expression::NotEqual(loc!(), var("a"), var("b")) => "a != b",

                ast::Expression::Assign(loc!(), var("a"), var("b")) => "a = b",
                ast::Expression::AssignOr(loc!(), var("a"), var("b")) => "a |= b",
                ast::Expression::AssignAnd(loc!(), var("a"), var("b")) => "a &= b",
                ast::Expression::AssignXor(loc!(), var("a"), var("b")) => "a ^= b",
                ast::Expression::AssignShiftLeft(loc!(), var("a"), var("b")) => "a <<= b",
                ast::Expression::AssignShiftRight(loc!(), var("a"), var("b")) => "a >>= b",
                ast::Expression::AssignAdd(loc!(), var("a"), var("b")) => "a += b",
                ast::Expression::AssignSubtract(loc!(), var("a"), var("b")) => "a -= b",
                ast::Expression::AssignMultiply(loc!(), var("a"), var("b")) => "a *= b",
                ast::Expression::AssignDivide(loc!(), var("a"), var("b")) => "a /= b",
                ast::Expression::AssignModulo(loc!(), var("a"), var("b")) => "a %= b",
            }

            ast::FunctionAttribute: {
                ast::FunctionAttribute::Virtual(loc!()) => "virtual",
                ast::FunctionAttribute::Immutable(loc!()) => "immutable",

                ast::FunctionAttribute::Override(loc!(), vec![]) => "override",
                ast::FunctionAttribute::Override(loc!(), vec![idp!["a", "b"]]) => "override(a.b)",
                ast::FunctionAttribute::Override(loc!(), vec![idp!["a", "b"], idp!["c", "d"]])
                    => "override(a.b, c.d)",
            }

            ast::FunctionTy: {
                ast::FunctionTy::Constructor => "constructor",
                ast::FunctionTy::Function => "function",
                ast::FunctionTy::Fallback => "fallback",
                ast::FunctionTy::Receive => "receive",
                ast::FunctionTy::Modifier => "modifier",
            }

            ast::Import: {
                ast::Import::Plain(ast::ImportPath::Filename(lit!("path/to/import")), loc!()) => "import \"path/to/import\";",

                ast::Import::GlobalSymbol(ast::ImportPath::Filename(lit!("path-to-import")), id("ImportedContract"), loc!())
                    => "import \"path-to-import\" as ImportedContract;",

                ast::Import::Rename(ast::ImportPath::Filename(lit!("import\\to\\path")), vec![], loc!())
                    => "import {} from \"import\\to\\path\";",
                ast::Import::Rename(ast::ImportPath::Filename(lit!("import\\to\\path")), vec![(id("A"), None), (id("B"), Some(id("C")))], loc!())
                    => "import {A, B as C} from \"import\\to\\path\";",

                ast::Import::Plain(ast::ImportPath::Path(idp!("std", "stub")), loc!()) => "import std.stub;",

                ast::Import::GlobalSymbol(ast::ImportPath::Path(idp!("a", "b", "c")), id("ImportedContract"), loc!())
                    => "import a.b.c as ImportedContract;",

                ast::Import::Rename(ast::ImportPath::Path(idp!("std", "stub")), vec![], loc!())
                    => "import {} from std.stub;",
                ast::Import::Rename(ast::ImportPath::Path(idp!("std", "stub")), vec![(id("A"), None), (id("B"), Some(id("C")))], loc!())
                    => "import {A, B as C} from std.stub;",
            }

            ast::Statement: {
                ast::Statement::Assembly {
                    loc: loc!(),
                    dialect: None,
                    flags: None,
                    block: yul_block(),
                } => "assembly {}",
                ast::Statement::Assembly {
                    loc: loc!(),
                    dialect: None,
                    flags: Some(vec![lit!("memory-safe")]),
                    block: yul_block(),
                } => "assembly (\"memory-safe\") {}",
                ast::Statement::Assembly {
                    loc: loc!(),
                    dialect: None,
                    flags: Some(vec![lit!("memory-safe"), lit!("second-flag")]),
                    block: yul_block(),
                } => "assembly (\"memory-safe\", \"second-flag\") {}",

                ast::Statement::Args(loc!(), vec![]) => "{}",
                ast::Statement::Args(loc!(), vec![
                    ast::NamedArgument {
                        loc: loc!(),
                        name: id("name"),
                        expr: expr!(value),
                    },
                ]) => "{name: value}",
                ast::Statement::Args(loc!(), vec![
                    ast::NamedArgument {
                        loc: loc!(),
                        name: id("name1"),
                        expr: expr!(value1),
                    },
                    ast::NamedArgument {
                        loc: loc!(),
                        name: id("name2"),
                        expr: expr!(value2),
                    },
                ]) => "{name1: value1, name2: value2}",

                ast::Statement::If(loc!(), expr!(true), Box::new(stmt!({})), None) => "if (true) {}",
                ast::Statement::If(loc!(), expr!(true), Box::new(stmt!({})), Some(Box::new(stmt!({}))))
                    => "if (true) {} else {}",

                ast::Statement::While(loc!(), expr!(true), Box::new(stmt!({}))) => "while (true) {}",

                ast::Statement::Expression(loc!(), expr!(true)) => "true",

                ast::Statement::VariableDefinition(loc!(), ast::VariableDeclaration {
                    loc: loc!(),
                    ty: None,
                    storage: None,
                    name: Some(id("a")),
                }, None) => "uint256 a;",
                ast::Statement::VariableDefinition(loc!(), ast::VariableDeclaration {
                    loc: loc!(),
                    ty: None,
                    storage: None,
                    name: Some(id("a")),
                }, Some(expr!(0))) => "uint256 a = 0;",

                ast::Statement::For(loc!(), None, None, None, Some(Box::new(stmt!({}))))
                    => "for (;;) {}",
                ast::Statement::For(loc!(), Some(Box::new(ast::Statement::VariableDefinition(
                    loc!(),
                    ast::VariableDeclaration {
                        loc: loc!(),
                        ty: None,
                        storage: None,
                        name: Some(id("a")),
                    },
                    None
                ))), None, None, Some(Box::new(stmt!({}))))
                    => "for (uint256 a;;) {}",
                ast::Statement::For(loc!(), None, Some(Box::new(expr!(true))), None, Some(Box::new(stmt!({}))))
                    => "for (; true;) {}",
                ast::Statement::For(
                    loc!(),
                    None,
                    Some(Box::new(expr!(true))),
                    Some(Box::new(expr!(++i))),
                    Some(Box::new(stmt!({})))
                ) => "for (; true; ++i) {}",

                ast::Statement::DoWhile(loc!(), Box::new(stmt!({})), expr!(true))
                    => "do {} while (true);",

                ast::Statement::Continue(loc!()) => "continue;",
                ast::Statement::Break(loc!()) => "break;",

                ast::Statement::Return(loc!(), None) => "return;",
                ast::Statement::Return(loc!(), Some(expr!(true))) => "return true;",

                ast::Statement::Revert(loc!(), None, vec![]) => "revert();",
                ast::Statement::Revert(loc!(), None, vec![expr!("error")])
                    => "revert(\"error\");",
                ast::Statement::Revert(loc!(), Some(idp!("my", "error")), vec![expr!("error")])
                    => "revert my.error(\"error\");",

                ast::Statement::RevertNamedArgs(loc!(), None, vec![]) => "revert();",
                ast::Statement::RevertNamedArgs(loc!(), None, vec![ast::NamedArgument {
                    loc: loc!(),
                    name: id("name"),
                    expr: expr!(value),
                }]) => "revert({name: value});",
                ast::Statement::RevertNamedArgs(loc!(), Some(idp!("my", "error")), vec![ast::NamedArgument {
                    loc: loc!(),
                    name: id("name"),
                    expr: expr!(value),
                }]) => "revert my.error({name: value});",

                ast::Statement::Emit(loc!(), expr!(true)) => "emit true;",

                ast::Statement::Try(loc!(), expr!(true), None, vec![]) => "try true",
                ast::Statement::Try(loc!(), expr!(true), None, vec![ast::CatchClause::Simple(loc!(), None, stmt!({}))])
                    => "try true catch {}",
                ast::Statement::Try(loc!(), expr!(true), Some((vec![], Box::new(stmt!({})))), vec![])
                    => "try true returns () {}",
                ast::Statement::Try(
                    loc!(),
                    expr!(true),
                    Some((vec![], Box::new(stmt!({})))),
                    vec![ast::CatchClause::Simple(loc!(), None, stmt!({}))]
                ) => "try true returns () {} catch {}",
                ast::Statement::Try(
                    loc!(),
                    expr!(true),
                    Some((vec![(loc!(), Some(param!(uint256 a)))], Box::new(stmt!({})))),
                    vec![ast::CatchClause::Simple(loc!(), None, stmt!({}))]
                ) => "try true returns (uint256 a) {} catch {}",
            }

            ast::Type: {
                ast::Type::Address => "address",
                ast::Type::AddressPayable => "address payable",
                ast::Type::Payable => "payable",
                ast::Type::Bool => "bool",
                ast::Type::String => "string",
                ast::Type::Int(256) => "int256",
                ast::Type::Uint(256) => "uint256",
                ast::Type::Bytes(32) => "bytes32",
                ast::Type::Rational => "fixed",
                ast::Type::DynamicBytes => "bytes",

                ast::Type::Mapping {
                    loc: loc!(),
                    key: Box::new(expr_ty!(uint256)),
                    key_name: None,
                    value: Box::new(expr_ty!(uint256)),
                    value_name: None,
                } => "mapping(uint256 => uint256)",
                ast::Type::Mapping {
                    loc: loc!(),
                    key: Box::new(expr_ty!(uint256)),
                    key_name: Some(id("key")),
                    value: Box::new(expr_ty!(uint256)),
                    value_name: None,
                } => "mapping(uint256 key => uint256)",
                ast::Type::Mapping {
                    loc: loc!(),
                    key: Box::new(expr_ty!(uint256)),
                    key_name: Some(id("key")),
                    value: Box::new(expr_ty!(uint256)),
                    value_name: Some(id("value")),
                } => "mapping(uint256 key => uint256 value)",

                ast::Type::Function {
                    params: vec![],
                    attributes: vec![],
                    returns: None
                } => "function ()",
                ast::Type::Function {
                    params: vec![(loc!(), Some(param!(uint256)))],
                    attributes: vec![],
                    returns: None
                } => "function (uint256)",
                ast::Type::Function {
                    params: vec![(loc!(), Some(param!(uint256))), (loc!(), Some(param!(address)))],
                    attributes: vec![],
                    returns: None
                } => "function (uint256, address)",
                ast::Type::Function {
                    params: vec![(loc!(), Some(param!(uint256)))],
                    attributes: vec![ast::FunctionAttribute::Virtual(loc!())],
                    returns: None
                } => "function (uint256) virtual",
                ast::Type::Function {
                    params: vec![(loc!(), Some(param!(uint256)))],
                    attributes: vec![ast::FunctionAttribute::Virtual(loc!()), ast::FunctionAttribute::Override(loc!(), vec![])],
                    returns: None
                } => "function (uint256) virtual override",
                ast::Type::Function {
                    params: vec![(loc!(), Some(param!(uint256)))],
                    attributes: vec![ast::FunctionAttribute::Virtual(loc!()), ast::FunctionAttribute::Override(loc!(), vec![idp!["a", "b"]])],
                    returns: None
                } => "function (uint256) virtual override(a.b)",
                ast::Type::Function {
                    params: vec![(loc!(), Some(param!(uint256)))],
                    attributes: vec![],
                    returns: Some((vec![], vec![])),
                } => "function (uint256)",
                ast::Type::Function {
                    params: vec![(loc!(), Some(param!(uint256)))],
                    attributes: vec![],
                    returns: Some((vec![(loc!(), Some(param!(uint256)))], vec![])),
                } => "function (uint256) returns (uint256)",
                ast::Type::Function {
                    params: vec![(loc!(), Some(param!(uint256)))],
                    attributes: vec![],
                    returns: Some((vec![(loc!(), Some(param!(uint256))), (loc!(), Some(param!(address)))], vec![])),
                } => "function (uint256) returns (uint256, address)",
            }

            ast::UserDefinedOperator: {
                ast::UserDefinedOperator::BitwiseAnd => "&",
                ast::UserDefinedOperator::BitwiseNot => "~",
                ast::UserDefinedOperator::Negate => "-",
                ast::UserDefinedOperator::BitwiseOr => "|",
                ast::UserDefinedOperator::BitwiseXor => "^",
                ast::UserDefinedOperator::Add => "+",
                ast::UserDefinedOperator::Divide => "/",
                ast::UserDefinedOperator::Modulo => "%",
                ast::UserDefinedOperator::Multiply => "*",
                ast::UserDefinedOperator::Subtract => "-",
                ast::UserDefinedOperator::Equal => "==",
                ast::UserDefinedOperator::More => ">",
                ast::UserDefinedOperator::MoreEqual => ">=",
                ast::UserDefinedOperator::Less => "<",
                ast::UserDefinedOperator::LessEqual => "<=",
                ast::UserDefinedOperator::NotEqual => "!=",
            }

            ast::VariableAttribute: {
                ast::VariableAttribute::Constant(loc!()) => "constant",
            }

            ast::YulExpression: {
                ast::YulExpression::BoolLiteral(loc!(), false, None) => "false",
                ast::YulExpression::BoolLiteral(loc!(), true, None) => "true",
                ast::YulExpression::BoolLiteral(loc!(), false, Some(id("name"))) => "false: name",
                ast::YulExpression::BoolLiteral(loc!(), true, Some(id("name"))) => "true: name",

                ast::YulExpression::NumberLiteral(loc!(), "1234".into(), "".into(), None) => "1234",
                ast::YulExpression::NumberLiteral(loc!(), "1234".into(), "9".into(), None) => "1234e9",
                ast::YulExpression::NumberLiteral(loc!(), "1234".into(), "".into(), Some(id("name"))) => "1234: name",
                ast::YulExpression::NumberLiteral(loc!(), "1234".into(), "9".into(), Some(id("name"))) => "1234e9: name",

                ast::YulExpression::HexNumberLiteral(loc!(), "0x1234".into(), None) => "0x1234",
                ast::YulExpression::HexNumberLiteral(loc!(), "0x1234".into(), Some(id("name"))) => "0x1234: name",

                ast::YulExpression::HexStringLiteral(lit!(hex "1234"), None) => "hex\"1234\"",
                ast::YulExpression::HexStringLiteral(lit!(hex "1234"), Some(id("name"))) => "hex\"1234\": name",

                ast::YulExpression::StringLiteral(lit!("1234"), None) => "\"1234\"",
                ast::YulExpression::StringLiteral(lit!("1234"), Some(id("name"))) => "\"1234\": name",

                ast::YulExpression::Variable(id("name")) => "name",

                ast::YulExpression::FunctionCall(Box::new(ast::YulFunctionCall {
                    loc: loc!(),
                    id: id("name"),
                    arguments: vec![],
                })) => "name()",

                ast::YulExpression::SuffixAccess(loc!(), Box::new(yexpr!(struct)), id("access"))
                    => "struct.access",
            }

            ast::YulStatement: {
                // rest tested individually

                ast::YulStatement::Assign(loc!(), vec![yexpr!(var)], yexpr!(eq))
                    => "var := eq",
                ast::YulStatement::Assign(loc!(), vec![yexpr!(a), yexpr!(b)], yexpr!(eq))
                    => "a, b := eq",

                ast::YulStatement::VariableDeclaration(loc!(), vec![yid!(var)], None)
                    => "let var",
                ast::YulStatement::VariableDeclaration(loc!(), vec![yid!(a), yid!(b)], None)
                    => "let a, b",
                ast::YulStatement::VariableDeclaration(loc!(), vec![yid!(var)], Some(yexpr!(eq)))
                    => "let var := eq",
                ast::YulStatement::VariableDeclaration(loc!(), vec![yid!(a), yid!(b)], Some(yexpr!(eq)))
                    => "let a, b := eq",

                ast::YulStatement::If(loc!(), yexpr!(expr), yul_block()) => "if expr {}",

                ast::YulStatement::Leave(loc!()) => "leave",
                ast::YulStatement::Break(loc!()) => "break",
                ast::YulStatement::Continue(loc!()) => "continue",
            }

            ast::YulSwitchOptions: {
                ast::YulSwitchOptions::Case(loc!(), yexpr!(expr), yul_block()) => "case expr {}",
                ast::YulSwitchOptions::Default(loc!(), yul_block()) => "default {}",
            }
        ];
    }
}

use std::str::FromStr;
use std::{
    borrow::Cow,
    fmt::{Display, Formatter, Result, Write},
};

use crate::ast;
use crate::ast::{
    Annotation, Condition, Expression, FormulaExpression, ModelPart, Property, SourceUnitPart,
    StatePart, Type,
};

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
impl Display for Annotation {
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
            Annotation::String(s) => {
                Display::fmt(s, f)?;
            }
            Annotation::Number(_, n) => {
                Display::fmt(n, f)?;
            }
            Annotation::Rational(_, n, _) => {
                Display::fmt(n, f)?;
            }
            Annotation::Boolean(_, v) => {
                Display::fmt(v, f)?;
            }
            Annotation::Visibility(_, v) => {
                Display::fmt(v, f)?;
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

impl Display for ast::FormulaDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str("formula ")?;
        Display::fmt(&self.formula, f)?;
        Ok(())
    }
}

impl Display for ast::ModelDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for x in self.annotations.iter() {
            Display::fmt(&x, f)?;
        }
        f.write_str("model ")?;
        write_opt!(f, "", &self.name);
        write_opt!(f, ": ", &self.implements);
        f.write_str(" {")?;
        for x in self.parts.iter() {
            Display::fmt(&x, f)?;
        }
        f.write_str("}")?;
        Ok(())
    }
}

impl Display for ModelPart {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ModelPart::ImportDirective(inner) => Display::fmt(&inner, f)?,
            ModelPart::EnumDefinition(inner) => Display::fmt(&inner, f)?,
            ModelPart::StructDefinition(inner) => Display::fmt(&inner, f)?,
            ModelPart::ErrorDefinition(inner) => Display::fmt(&inner, f)?,
            ModelPart::FunctionDefinition(inner) => Display::fmt(&inner, f)?,
            ModelPart::FormulaDefinition(inner) => Display::fmt(&inner, f)?,
            ModelPart::VariableDefinition(inner) => Display::fmt(&inner, f)?,
            ModelPart::AnnotationDefinition(inner) => Display::fmt(&inner, f)?,
            ModelPart::PropertyDefinition(inner) => Display::fmt(&inner, f)?,
            ModelPart::StateDefinition(inner) => Display::fmt(&inner, f)?,
            ModelPart::TypeDefinition(inner) => Display::fmt(&inner, f)?,
            ModelPart::ModelDefinition(inner) => Display::fmt(&inner, f)?,
            ModelPart::Using(inner) => Display::fmt(&inner, f)?,
            ModelPart::StraySemicolon(_) => {}
        }
        Ok(())
    }
}

impl Display for ast::StateDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for x in self.annotations.iter() {
            Display::fmt(&x, f)?;
        }
        f.write_str("state ")?;
        write_opt!(f, "", &self.name);
        f.write_str(" {")?;
        for x in self.parts.iter() {
            Display::fmt(&x, f)?;
        }
        f.write_str("}")?;
        Ok(())
    }
}

impl Display for StatePart {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            StatePart::ImportDirective(inner) => Display::fmt(&inner, f)?,
            StatePart::EnumDefinition(inner) => Display::fmt(&inner, f)?,
            StatePart::StructDefinition(inner) => Display::fmt(&inner, f)?,
            StatePart::ErrorDefinition(inner) => Display::fmt(&inner, f)?,
            StatePart::FunctionDefinition(inner) => Display::fmt(&inner, f)?,
            StatePart::FormulaDefinition(inner) => Display::fmt(&inner, f)?,
            StatePart::VariableDefinition(inner) => Display::fmt(&inner, f)?,
            StatePart::AnnotationDefinition(inner) => Display::fmt(&inner, f)?,
            StatePart::PropertyDefinition(inner) => Display::fmt(&inner, f)?,
            StatePart::TypeDefinition(inner) => Display::fmt(&inner, f)?,
            StatePart::ModelDefinition(inner) => Display::fmt(&inner, f)?,
            StatePart::Using(inner) => Display::fmt(&inner, f)?,
            StatePart::Reference(_, name, e) => {
                f.write_str("ref ")?;
                Display::fmt(&name, f)?;
                write_opt!(f, ": ", &e);
            }
            StatePart::StraySemicolon(_) => {}
        }
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

        if !self.annotations.is_empty() {
            f.write_char(' ')?;
            write_separated(&self.annotations, f, " ")?;
        }

        if self.return_type.is_some() {
            write_opt!(f, " : ", &self.return_type);
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
        Display::fmt(&self.ty, f)?;
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
        write_separated(&self.fields, f, ";")?;
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
        f.write_str(": ")?;
        Display::fmt(&self.ty, f)?;
        Ok(())
    }
}

impl Display for ast::VariableDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if !self.attrs.is_empty() {
            f.write_char(' ')?;
            write_separated(&self.attrs, f, " ")?;
        }
        f.write_str(": ")?;
        Display::fmt(&self.ty, f)?;
        write_opt!(f, ' ', &self.name);
        write_opt!(f, " = ", &self.initializer);
        f.write_char(';')
    }
}

impl Display for ast::FormulaBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_char('{')?;
        write_separated(&self.statements, f, " ")?;
        f.write_char('}')
    }
}

impl Display for ast::FormulaFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        std::fmt::Display::fmt(&self.id, f)?;
        f.write_char('(')?;
        write_separated(&self.arguments, f, ", ")?;
        f.write_char(')')
    }
}

impl Display for ast::Comment {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(self.value())
    }
}

impl Display for ast::AnnotationDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str("#")?;
        if self.glob {
            f.write_str("!")?;
        }
        f.write_str("[")?;
        let mut iter = self.args.iter();
        if let Some(first) = iter.next() {
            Display::fmt(first, f)?;
            for item in iter {
                f.write_str(", ")?;
                Display::fmt(item, f)?;
            }
        }
        f.write_str("]")
    }
}

impl Display for Condition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::ArraySubscript(_, id, n) => {
                Display::fmt(id, f)?;
                f.write_str("[")?;
                Display::fmt(n, f)?;
                f.write_str("]")
            }
            Self::Parenthesis(_, cnd) => {
                f.write_str("(")?;
                Display::fmt(cnd, f)?;
                f.write_str(")")
            }
            Self::MemberAccess(_, cnd, id) => {
                Display::fmt(id, f)?;
                f.write_str(".")?;
                Display::fmt(cnd, f)
            }
            Self::FunctionCall(_, id, args) => {
                Display::fmt(id, f)?;
                f.write_str("(")?;
                write_separated(args, f, ", ")?;
                f.write_str(")")
            }
            Self::Not(_, cnd) => {
                f.write_str("!")?;
                Display::fmt(cnd, f)
            }
            Self::Add(_, lcnd, rcnd) => {
                Display::fmt(lcnd, f)?;
                f.write_str(" + ")?;
                Display::fmt(rcnd, f)
            }
            Self::Subtract(_, lcnd, rcnd) => {
                Display::fmt(lcnd, f)?;
                f.write_str(" - ")?;
                Display::fmt(rcnd, f)
            }
            Self::And(_, lcnd, rcnd) => {
                Display::fmt(lcnd, f)?;
                f.write_str(" & ")?;
                Display::fmt(rcnd, f)
            }
            Self::Or(_, lcnd, rcnd) => {
                Display::fmt(lcnd, f)?;
                f.write_str(" | ")?;
                Display::fmt(rcnd, f)
            }
            Self::Less(_, lcnd, rcnd) => {
                Display::fmt(lcnd, f)?;
                f.write_str(" < ")?;
                Display::fmt(rcnd, f)
            }
            Self::More(_, lcnd, rcnd) => {
                Display::fmt(lcnd, f)?;
                f.write_str(" > ")?;
                Display::fmt(rcnd, f)
            }
            Self::LessEqual(_, lcnd, rcnd) => {
                Display::fmt(lcnd, f)?;
                f.write_str(" <= ")?;
                Display::fmt(rcnd, f)
            }
            Self::MoreEqual(_, lcnd, rcnd) => {
                Display::fmt(lcnd, f)?;
                f.write_str(" >= ")?;
                Display::fmt(rcnd, f)
            }
            Self::Equal(_, lcnd, rcnd) => {
                Display::fmt(lcnd, f)?;
                f.write_str(" = ")?;
                Display::fmt(rcnd, f)
            }
            Self::NotEqual(_, lcnd, rcnd) => {
                Display::fmt(lcnd, f)?;
                f.write_str(" != ")?;
                Display::fmt(rcnd, f)
            }
            Self::NumberLiteral(_, n) => Display::fmt(n, f),
            Self::RationalNumberLiteral(_, n, b) => Display::fmt(n, f),
            Self::HexNumberLiteral(_, b, id) => Display::fmt(b, f),
            Self::StringLiteral(vals) => write_separated(vals, f, " "),
            Self::HexLiteral(vals) => write_separated(vals, f, " "),
            Self::BoolLiteral(_, b) => Display::fmt(b, f),
            Self::Variable(v) => Display::fmt(v, f),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Type(_, ty) => Display::fmt(&ty, f),

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
            Self::ArraySubscript(_, id, n) => {
                Display::fmt(id, f)?;
                f.write_char('[')?;
                Display::fmt(n, f)?;
                f.write_char(']')
            }
            Self::ArraySlice(_, id, l, r) => {
                Display::fmt(id, f)?;
                f.write_char('[')?;
                write_opt!(f, l);
                f.write_char(':')?;
                write_opt!(f, r);
                f.write_char(']')
            }

            Self::MemberAccess(_, expr, ident) => {
                Self::format_expr(f, &expr);
                f.write_char('.')?;
                std::fmt::Display::fmt(&ident, f)
            }

            Self::Parenthesis(_, expr) => {
                f.write_char('(')?;
                Self::format_expr(f, &expr);
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
            Self::Cast(_, e, ty) => {
                Display::fmt(e, f)?;
                f.write_str(" as ")?;
                Display::fmt(ty, f)?;
                Ok(())
            }
            Self::RationalNumberLiteral(_, val, _) => {
                let dig = f64::from_str(val).unwrap();
                // let val = rm_underscores(val);
                write!(f, "{dig}")?;
                // f.write_str(&val)?;
                Ok(())
            }

            Self::FunctionCall(_, expr, exprs) => {
                Display::fmt(&expr, f)?;
                f.write_char('(')?;
                write_separated(exprs, f, ", ")?;
                f.write_char(')')
            }
            Self::FunctionCallBlock(_, expr, block) => {
                Display::fmt(&expr, f)?;
                Display::fmt(&block, f)
            }
            Self::NamedFunctionCall(_, expr, args) => {
                Display::fmt(&expr, f)?;
                f.write_str("({")?;
                write_separated(args, f, ", ")?;
                f.write_str("})")
            }

            Self::ConditionalOperator(_, cond, l, r) => {
                Display::fmt(&cond, f)?;
                f.write_str(" ? ")?;
                Display::fmt(&l, f)?;
                f.write_str(" : ")?;
                Display::fmt(&r, f)
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

impl Expression {
    /// Returns the operator string of this expression, if any.
    #[inline]
    pub const fn operator(&self) -> Option<&'static str> {
        use ast::Expression::*;
        let operator = match self {
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

            Cast(..) => "as",
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

    fn format_expr(f: &mut Formatter, expr: &Box<Expression>) {
        let v = expr.as_ref();
        let r = if let Expression::Variable(id) = v {
            Display::fmt(id, f)
        } else {
            Display::fmt(&expr, f)
        };
        r.unwrap()
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
            Self::Function => "fn",
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

impl Display for ast::PropertyDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for i in self.annotations.iter() {
            Display::fmt(i, f)?;
        }
        write_opt!(f, "", &self.name);
        Display::fmt(&self.value, f)?;
        f.write_char(';')
    }
}

impl Display for Property {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Property::Expression(e) => Display::fmt(&e, f),
            Property::Function(stmt) => Display::fmt(&stmt, f),
        }
    }
}

impl Display for SourceUnitPart {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::ImportDirective(inner) => Display::fmt(&inner, f),
            Self::EnumDefinition(inner) => Display::fmt(&inner, f),
            Self::StructDefinition(inner) => Display::fmt(&inner, f),
            Self::ErrorDefinition(inner) => Display::fmt(&inner, f),
            Self::FunctionDefinition(inner) => Display::fmt(&inner, f),
            Self::FormulaDefinition(inner) => Display::fmt(&inner, f),
            Self::VariableDefinition(inner) => Display::fmt(&inner, f),
            Self::TypeDefinition(inner) => Display::fmt(&inner, f),
            Self::AnnotationDefinition(inner) => Display::fmt(&inner, f),
            Self::ModelDefinition(inner) => Display::fmt(&inner, f),
            Self::Using(inner) => Display::fmt(&inner, f),
            Self::StraySemicolon(_) => f.write_char(';'),
            SourceUnitPart::PropertyDefinition(inner) => Display::fmt(&inner, f),
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
                f.write_str("if ")?;
                std::fmt::Display::fmt(&cond, f)?;
                f.write_str(" ")?;
                Display::fmt(&block, f)?;
                write_opt!(f, " else ", end_block);
                Ok(())
            }
            Self::While(_, cond, block) => {
                f.write_str("while (")?;
                std::fmt::Display::fmt(&cond, f)?;
                f.write_str(") ")?;
                Display::fmt(&block, f)
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
                    Display::fmt(&block, f)
                } else {
                    f.write_char(';')
                }
            }
            Self::DoWhile(_, block, cond) => {
                f.write_str("do ")?;
                Display::fmt(&block, f)?;
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
            Self::Formula { block, .. } => {
                f.write_str("formula")?;
                Display::fmt(block, f)
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
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Address => f.write_str("address"),
            Self::Bool => f.write_str("bool"),
            Self::String => f.write_str("string"),
            Self::Rational => f.write_str("rational"),
            Self::Function {
                params,
                annotations: attributes,
                returns,
            } => {
                f.write_str("fn (")?;
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
            Type::Alias(name) => std::fmt::Display::fmt(&name, f),
            Type::Array {
                element_type,
                element_count,
                loc: _,
            } => {
                f.write_str("[ ")?;
                std::fmt::Display::fmt(&element_count, f)?;
                f.write_str(" : ")?;
                Display::fmt(element_type, f)?;
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

impl Display for FormulaExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::BoolLiteral(_, value, ident) => {
                let value = if *value { "true" } else { "false" };
                f.write_str(value)?;
                write_opt!(f, ": ", ident);
                Ok(())
            }
            Self::NumberLiteral(_, value, ident) => {
                Display::fmt(value, f)?;
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
            Self::FunctionCall(call) => Display::fmt(&call, f),
            Self::SuffixAccess(_, l, r) => {
                Display::fmt(&l, f)?;
                f.write_char('.')?;
                Display::fmt(&r, f)
            }
            FormulaExpression::Parenthesis(_, p) => {
                f.write_str("{")?;
                Display::fmt(&p, f)?;
                f.write_str("}")
            }
        }
    }
}

impl Display for ast::FormulaStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Block(inner) => std::fmt::Display::fmt(&inner, f),
            Self::FunctionCall(inner) => Display::fmt(&inner, f),
            Self::Expression(_, expr) => Display::fmt(&expr, f),
            Self::Error(_) => Ok(()),
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

fn _rm_underscores(s: &str) -> Cow<'_, str> {
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
    use crate::ast::Type::Alias;

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
    macro_rules! formula_expr {
        ($i:ident) => {
            ast::FormulaExpression::Variable(id(stringify!($i)))
        };
        ($l:literal) => {
            ast::FormulaExpression::Variable(id(stringify!($l)))
        };
    }

    /// Type
    macro_rules! ty {
        ($i:ident) => {
            ast::Type::Alias(id(stringify!($i)))
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
    }

    /// Identifier
    fn id(s: &str) -> ast::Identifier {
        ast::Identifier {
            loc: loc!(),
            name: s.to_string(),
        }
    }

    macro_rules! formula_id {
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

    fn var(s: &str) -> Box<Expression> {
        Box::new(Expression::Variable(id(s)))
    }

    fn formula_block() -> ast::FormulaBlock {
        ast::FormulaBlock {
            loc: loc!(),
            statements: vec![],
        }
    }

    fn block() -> Box<ast::Statement> {
        Box::new(ast::Statement::Block {
            loc: loc!(),
            unchecked: false,
            statements: vec![],
        })
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
            } => "id::path",
            ast::Base {
                name: idp!("id", "path"),
                args: Some(vec![expr!(value)]),
            } => "id::path(value)",
            ast::Base {
                name: idp!("id", "path"),
                args: Some(vec![expr!(value1), expr!(value2)]),
            } => "id::path(value1, value2)",

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
            } => "id::path",
            ast::IdentifierPath {
                identifiers: vec![id("long"), id("id"), id("path")],
            } => "long::id::path",

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
                storage: Some(ast::StorageLocation::Memory(Default::default())),
                name: Some(id("name")),
                annotation: None,
            } => "uint256 memory name",
            ast::Parameter {
                ty: expr_ty!(uint256),
                storage: Some(ast::StorageLocation::Memory(Default::default())),
                name: None,
                annotation: None,
            } => "uint256 memory",

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
            } => "id::path",
            ast::UsingFunction {
                path: idp!["id", "path"],
                oper: Some(ast::UserDefinedOperator::Add),
            } => "id::path as +",

            ast::VariableDeclaration {
                ty: Alias(id("uint256")),
                storage: None,
                name: None,
                annotations: vec![]
            } => ": uint256"
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
                annotations: vec![],
                attributes: vec![],
                return_type: None,
                body: None,
            } => "fn name();",
            ast::FunctionDefinition {
                ty: ast::FunctionTy::Function,
                name: Some(id("name")),
                name_loc: loc!(),
                params: vec![],
                annotations: vec![],
                attributes: vec![],
                return_type: None,
                body: Some(stmt!({})),
            } => "fn name() {}",
            ast::FunctionDefinition {
                ty: ast::FunctionTy::Function,
                name: Some(id("name")),
                name_loc: loc!(),
                params: vec![],
                annotations: vec![],
                attributes: vec![],
                return_type: Some(Type::Alias(id("uint256"))),
                body: Some(stmt!({})),
            } => "fn name() : uint256 {}",
            ast::FunctionDefinition {
                ty: ast::FunctionTy::Function,
                name: Some(id("name")),
                name_loc: loc!(),
                params: vec![],
                annotations: vec![],
                attributes: vec![],
                return_type: Some(Type::Alias(id("uint256"))),
                body: Some(stmt!({})),
            } => "fn name() : uint256 {}",

            ast::StructDefinition {
                name: Some(id("name")),
                fields: vec![],
                annotations: vec![]
            } => "struct name {}",
            ast::StructDefinition {
                name: Some(id("name")),
                fields: vec![ast::VariableDeclaration {
                    loc: loc!(),
                    ty: Alias(id("uint256")),
                    storage: None,
                    name: Some(id("a")),
                    annotations: vec![]
                }],
                annotations: vec![]
            } => "struct name { a: uint256;}",
            ast::StructDefinition {
                name: Some(id("name")),
                fields: vec![
                    ast::VariableDeclaration {
                        loc: loc!(),
                        ty: Alias(id("uint256")),
                        storage: None,
                        name: Some(id("a")),
                        annotations: vec![]
                    },
                    ast::VariableDeclaration {
                        loc: loc!(),
                        ty: Alias(id("uint256")),
                        storage: None,
                        name: Some(id("b")),
                        annotations: vec![]
                    }
                ],
                annotations: vec![]
            } => "struct name { a: uint256; b: uint256;}",

            // ast::TypeDefinition {
            //     name: id("MyType"),
            //     ty: expr_ty!(uint256),
            // } => "type MyType is uint256;",

            ast::FormulaBlock {
                statements: vec![]
            } => "{}",

            ast::FormulaFunctionCall {
                id: id("name"),
                arguments: vec![],
            } => "name()",
            ast::FormulaFunctionCall {
                id: id("name"),
                arguments: vec![formula_expr!(arg)],
            } => "name(arg)",
            ast::FormulaFunctionCall {
                id: id("name"),
                arguments: vec![formula_expr!(arg1), formula_expr!(arg2)],
            } => "name(arg1, arg2)",
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
                ast::Expression::Type(loc!(), ty!(uint256)) => "uint256",
                ast::Expression::Variable(id("myVar")) => "myVar",

                ast::Expression::ArrayLiteral(loc!(), vec![expr!(1), expr!(2)]) => "[1, 2]",

                ast::Expression::ArraySubscript(loc!(), id("arr"), 0) => "arr[0]",
                ast::Expression::ArraySlice(loc!(), id("arr"), None, None) => "arr[:]",
                ast::Expression::ArraySlice(loc!(), id("arr"), Some(1), None)
                    => "arr[1:]",
                ast::Expression::ArraySlice(loc!(), id("arr"), None, Some(1))
                    => "arr[:1]",
                ast::Expression::ArraySlice(loc!(), id("arr"), Some(1), Some(2))
                    => "arr[1:2]",

                ast::Expression::MemberAccess(loc!(), Box::new(expr!(struct)), id("access")) => "struct.access",

                ast::Expression::Parenthesis(loc!(), Box::new(expr!(var))) => "(var)",
                ast::Expression::List(loc!(), vec![]) => "()",
                ast::Expression::List(loc!(), vec![(loc!(), Some(param!(address)))])
                    => "(address)",
                ast::Expression::List(loc!(), vec![(loc!(), Some(param!(address))), (loc!(), Some(param!(uint256)))])
                    => "(address, uint256)",

                ast::Expression::AddressLiteral(loc!(), "1234".parse().unwrap(), 0) => "1234:0",
                ast::Expression::StringLiteral(vec![lit!(unicode "¹²³")]) => "unicode\"¹²³\"",
                ast::Expression::HexLiteral(vec![lit!(hex "00112233")]) => "hex\"00112233\"",

                ast::Expression::HexNumberLiteral(loc!(), "0x1234".into(), None) => "0x1234",
                ast::Expression::HexNumberLiteral(loc!(), "0x1234".into(), Some(id("gwei"))) => "0x1234 gwei",
                ast::Expression::NumberLiteral(loc!(), "1234".parse().unwrap())
                    => "1234",
                ast::Expression::RationalNumberLiteral(loc!(), ".9".into(), false)
                    => "0.9",
                ast::Expression::FunctionCall(loc!(), id("func"), vec![]) => "func()",
                ast::Expression::FunctionCall(loc!(), id("func"), vec![expr!(arg)])
                    => "func(arg)",
                ast::Expression::FunctionCall(loc!(), id("func"), vec![expr!(arg1), expr!(arg2)])
                    => "func(arg1, arg2)",
                ast::Expression::FunctionCallBlock(loc!(), Box::new(expr!(func)), Box::new(stmt!({})))
                    => "func{}",

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

            ast::FunctionTy: {
                ast::FunctionTy::Constructor => "constructor",
                ast::FunctionTy::Function => "fn",
            }

            ast::Import: {
                ast::Import::Plain(ast::ImportPath::Filename(lit!("path/to/import")), loc!()) => "import \"path/to/import\";",

                ast::Import::GlobalSymbol(ast::ImportPath::Filename(lit!("path-to-import")), id("ImportedContract"), loc!())
                    => "import \"path-to-import\" as ImportedContract;",

                ast::Import::Rename(ast::ImportPath::Filename(lit!("import\\to\\path")), vec![], loc!())
                    => "import {} from \"import\\to\\path\";",
                ast::Import::Rename(ast::ImportPath::Filename(lit!("import\\to\\path")), vec![(id("A"), None), (id("B"), Some(id("C")))], loc!())
                    => "import {A, B as C} from \"import\\to\\path\";",

                ast::Import::Plain(ast::ImportPath::Path(idp!("std", "stub")), loc!()) => "import std::stub;",

                ast::Import::GlobalSymbol(ast::ImportPath::Path(idp!("a", "b", "c")), id("ImportedContract"), loc!())
                    => "import a::b::c as ImportedContract;",

                ast::Import::Rename(ast::ImportPath::Path(idp!("std", "stub")), vec![], loc!())
                    => "import {} from std::stub;",
                ast::Import::Rename(ast::ImportPath::Path(idp!("std", "stub")), vec![(id("A"), None), (id("B"), Some(id("C")))], loc!())
                    => "import {A, B as C} from std::stub;",
            }

            ast::Statement: {
                ast::Statement::Assembly {
                    loc: loc!(),
                    dialect: None,
                    flags: None,
                    block: block(),
                } => "assembly {}",
                ast::Statement::Assembly {
                    loc: loc!(),
                    dialect: None,
                    flags: Some(vec![lit!("memory-safe")]),
                    block: block(),
                } => "assembly (\"memory-safe\") {}",
                ast::Statement::Assembly {
                    loc: loc!(),
                    dialect: None,
                    flags: Some(vec![lit!("memory-safe"), lit!("second-flag")]),
                    block: block(),
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

                ast::Statement::If(loc!(), expr!(true), Box::new(stmt!({})), None) => "if true {}",
                ast::Statement::If(loc!(), expr!(true), Box::new(stmt!({})), Some(Box::new(stmt!({}))))
                    => "if true {} else {}",

                ast::Statement::While(loc!(), expr!(true), Box::new(stmt!({}))) => "while (true) {}",

                ast::Statement::Expression(loc!(), expr!(true)) => "true",

                ast::Statement::VariableDefinition(loc!(), ast::VariableDeclaration {
                    loc: loc!(),
                    ty: Alias(id("uint256")),
                    storage: None,
                    name: Some(id("a")),
                    annotations: vec![]
                }, None) => " a: uint256;",
                ast::Statement::VariableDefinition(loc!(), ast::VariableDeclaration {
                    loc: loc!(),
                    ty: Alias(id("uint256")),
                    storage: None,
                    name: Some(id("a")),
                    annotations: vec![]
                }, Some(expr!(0))) => " a: uint256 = 0;",

                ast::Statement::For(loc!(), None, None, None, Some(Box::new(stmt!({}))))
                    => "for (;;) {}",
                ast::Statement::For(loc!(), Some(Box::new(ast::Statement::VariableDefinition(
                    loc!(),
                    ast::VariableDeclaration {
                        loc: loc!(),
                        ty: Alias(id("uint256")),
                        storage: None,
                        name: Some(id("a")),
                        annotations: vec![]
                    },
                    None
                ))), None, None, Some(Box::new(stmt!({}))))
                    => "for ( a: uint256;;) {}",
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
            }

            ast::Type: {
                ast::Type::Address => "address",
                ast::Type::Bool => "bool",
                ast::Type::String => "string",
                ast::Type::Rational => "rational",
                ast::Type::Function {
                    params: vec![],
                    annotations: vec![],
                    returns: None
                } => "fn ()",
                ast::Type::Function {
                    params: vec![(loc!(), Some(param!(uint256)))],
                    annotations: vec![],
                    returns: None
                } => "fn (uint256)",
                ast::Type::Function {
                    params: vec![(loc!(), Some(param!(uint256))), (loc!(), Some(param!(address)))],
                    annotations: vec![],
                    returns: None
                } => "fn (uint256, address)",
                ast::Type::Function {
                    params: vec![(loc!(), Some(param!(uint256)))],
                    annotations: vec![],
                    returns: Some((vec![], vec![])),
                } => "fn (uint256)",
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
                ast::VariableAttribute::Constant(loc!()) => "const",
            }

            ast::FormulaExpression: {
                ast::FormulaExpression::BoolLiteral(loc!(), false, None) => "false",
                ast::FormulaExpression::BoolLiteral(loc!(), true, None) => "true",
                ast::FormulaExpression::BoolLiteral(loc!(), false, Some(id("name"))) => "false: name",
                ast::FormulaExpression::BoolLiteral(loc!(), true, Some(id("name"))) => "true: name",

                ast::FormulaExpression::NumberLiteral(loc!(), "1234".parse().unwrap(), None) => "1234",
                ast::FormulaExpression::NumberLiteral(loc!(), "1234".parse().unwrap(), Some(id("name"))) => "1234: name",

                ast::FormulaExpression::HexNumberLiteral(loc!(), "0x1234".into(), None) => "0x1234",
                ast::FormulaExpression::HexNumberLiteral(loc!(), "0x1234".into(), Some(id("name"))) => "0x1234: name",

                ast::FormulaExpression::HexStringLiteral(lit!(hex "1234"), None) => "hex\"1234\"",
                ast::FormulaExpression::HexStringLiteral(lit!(hex "1234"), Some(id("name"))) => "hex\"1234\": name",

                ast::FormulaExpression::StringLiteral(lit!("1234"), None) => "\"1234\"",
                ast::FormulaExpression::StringLiteral(lit!("1234"), Some(id("name"))) => "\"1234\": name",

                ast::FormulaExpression::Variable(id("name")) => "name",
            }

            ast::FormulaStatement: {
                ast::FormulaStatement::Expression(loc!(), formula_expr!(None))
                    => "None",
            }
        ];
    }
}

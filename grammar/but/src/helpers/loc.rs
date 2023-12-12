use std::sync::Arc;
use std::{borrow::Cow, rc::Rc};

use crate::ast::{self, Loc};
use crate::lexer::LexicalError;

/// Returns the optional code location.
pub trait OptionalCodeLocation {
    /// Returns the optional code location of `self`.
    fn loc_opt(&self) -> Option<Loc>;
}

impl<T: CodeLocation> OptionalCodeLocation for Option<T> {
    fn loc_opt(&self) -> Option<Loc> {
        self.as_ref().map(CodeLocation::loc)
    }
}

impl OptionalCodeLocation for ast::Visibility {
    fn loc_opt(&self) -> Option<Loc> {
        match self {
            Self::Internal(l, ..)
            | Self::External(l, ..)
            | Self::Private(l, ..)
            | Self::Public(l, ..) => *l,
        }
    }
}

impl OptionalCodeLocation for ast::SourceUnit {
    #[inline]
    fn loc_opt(&self) -> Option<Loc> {
        self.0.loc_opt()
    }
}

impl<T: CodeLocation> OptionalCodeLocation for [T] {
    // TODO: Merge first with last span?
    fn loc_opt(&self) -> Option<Loc> {
        self.first().map(CodeLocation::loc)
    }
}

impl<T: CodeLocation> OptionalCodeLocation for Vec<T> {
    fn loc_opt(&self) -> Option<Loc> {
        (**self).loc_opt()
    }
}

impl<'a, T: ?Sized + OptionalCodeLocation> OptionalCodeLocation for &'a T {
    fn loc_opt(&self) -> Option<Loc> {
        (**self).loc_opt()
    }
}

impl<'a, T: ?Sized + OptionalCodeLocation> OptionalCodeLocation for &'a mut T {
    fn loc_opt(&self) -> Option<Loc> {
        (**self).loc_opt()
    }
}

impl<'a, T: ?Sized + ToOwned + OptionalCodeLocation> OptionalCodeLocation for Cow<'a, T> {
    fn loc_opt(&self) -> Option<Loc> {
        (**self).loc_opt()
    }
}

impl<T: ?Sized + OptionalCodeLocation> OptionalCodeLocation for Box<T> {
    fn loc_opt(&self) -> Option<Loc> {
        (**self).loc_opt()
    }
}

impl<T: ?Sized + OptionalCodeLocation> OptionalCodeLocation for Rc<T> {
    fn loc_opt(&self) -> Option<Loc> {
        (**self).loc_opt()
    }
}

impl<T: ?Sized + OptionalCodeLocation> OptionalCodeLocation for Arc<T> {
    fn loc_opt(&self) -> Option<Loc> {
        (**self).loc_opt()
    }
}

// would be: `impl<T: CodeLocation> OptionalCodeLocation for T { ... }`
// but then we wouldn't have the correct implementation for `Box<T>` and the other smart pointers
macro_rules! impl_optional_for_ast {
    ($($t:ty),+ $(,)?) => {
        $(
            impl OptionalCodeLocation for $t {
                #[inline]
                fn loc_opt(&self) -> Option<Loc> {
                    Some(<$t as CodeLocation>::loc(self))
                }
            }
        )+
    };
}

impl_optional_for_ast!(
    // structs
    ast::AnnotationDefinition,
    ast::Base,
    ast::EnumDefinition,
    ast::ErrorDefinition,
    ast::ErrorParameter,
    ast::FunctionDefinition,
    ast::HexLiteral,
    ast::Identifier,
    ast::IdentifierPath,
    ast::NamedArgument,
    ast::Parameter,
    ast::StringLiteral,
    ast::StructDefinition,
    ast::TypeDefinition,
    ast::Using,
    ast::UsingFunction,
    ast::VariableDeclaration,
    ast::VariableDefinition,
    ast::FormulaBlock,
    ast::FormulaFunctionCall,
    // enums
    ast::Comment,
    ast::Expression,
    ast::Import,
    ast::Loc,
    ast::Mutability,
    ast::SourceUnitPart,
    ast::Statement,
    ast::StorageLocation,
    ast::UsingList,
    ast::VariableAttribute,
    ast::FormulaExpression,
    ast::FormulaStatement,
    // other
    LexicalError,
);

/// Returns the code location.
pub trait CodeLocation {
    /// Returns the code location of `self`.
    fn loc(&self) -> Loc;
}

impl CodeLocation for Loc {
    #[inline]
    fn loc(&self) -> Loc {
        *self
    }
}

impl<'a, T: ?Sized + CodeLocation> CodeLocation for &'a T {
    fn loc(&self) -> Loc {
        (**self).loc()
    }
}

impl<'a, T: ?Sized + CodeLocation> CodeLocation for &'a mut T {
    fn loc(&self) -> Loc {
        (**self).loc()
    }
}

impl<'a, T: ?Sized + ToOwned + CodeLocation> CodeLocation for Cow<'a, T> {
    fn loc(&self) -> Loc {
        (**self).loc()
    }
}

impl<T: ?Sized + CodeLocation> CodeLocation for Box<T> {
    fn loc(&self) -> Loc {
        (**self).loc()
    }
}

impl<T: ?Sized + CodeLocation> CodeLocation for Rc<T> {
    fn loc(&self) -> Loc {
        (**self).loc()
    }
}

impl<T: ?Sized + CodeLocation> CodeLocation for Arc<T> {
    fn loc(&self) -> Loc {
        (**self).loc()
    }
}

macro_rules! impl_for_structs {
    ($($t:ty),+ $(,)?) => {
        $(
            impl CodeLocation for $t {
                #[inline]
                fn loc(&self) -> Loc {
                    self.loc
                }
            }
        )+
    };
}

// all structs except for SourceUnit
impl_for_structs!(
    ast::AnnotationDefinition,
    ast::Base,
    ast::EnumDefinition,
    ast::ErrorDefinition,
    ast::ErrorParameter,
    ast::FunctionDefinition,
    ast::HexLiteral,
    ast::Identifier,
    ast::IdentifierPath,
    ast::NamedArgument,
    ast::Parameter,
    ast::StringLiteral,
    ast::StructDefinition,
    ast::TypeDefinition,
    ast::Using,
    ast::UsingFunction,
    ast::VariableDeclaration,
    ast::VariableDefinition,
    ast::FormulaBlock,
    ast::FormulaFunctionCall,
);

macro_rules! impl_for_enums {
    ($(
        $t:ty: match $s:ident {
            $($m:tt)*
        }
    )+) => {
        $(
            impl CodeLocation for $t {
                fn loc(&$s) -> Loc {
                    match *$s {
                        $($m)*
                    }
                }
            }
        )+
    };
}

// all enums except for Type, UserDefinedOperator and FunctionTy
impl_for_enums! {

    ast::Comment: match self {
        Self::Line(l, ..)
        | Self::Block(l, ..)
        | Self::DocLine(l, ..)
        | Self::DocBlock(l, ..) => l,
    }

    ast::Expression: match self {
        // literals have at least one item
        Self::StringLiteral(ref l, ..) => l.loc_opt().unwrap(),
        Self::HexLiteral(ref l, ..) => l.loc_opt().unwrap(),
        Self::Variable(ref l, ..) => l.loc(),
        Self::BoolLiteral(ref l, ..) => l.loc(),
        Self::PostIncrement(l, ..)
        | Self::PostDecrement(l, ..)
        | Self::Parenthesis(l, ..)
        | Self::ArraySubscript(l, ..)
        | Self::ArraySlice(l, ..)
        | Self::MemberAccess(l, ..)
        | Self::FunctionCall(l, ..)
        | Self::FunctionCallBlock(l, ..)
        | Self::NamedFunctionCall(l, ..)
        | Self::Not(l, ..)
        | Self::BitwiseNot(l, ..)
        | Self::PreIncrement(l, ..)
        | Self::PreDecrement(l, ..)
        | Self::UnaryPlus(l, ..)
        | Self::Negate(l, ..)
        | Self::Power(l, ..)
        | Self::Multiply(l, ..)
        | Self::Divide(l, ..)
        | Self::Modulo(l, ..)
        | Self::Add(l, ..)
        | Self::Subtract(l, ..)
        | Self::ShiftLeft(l, ..)
        | Self::ShiftRight(l, ..)
        | Self::BitwiseAnd(l, ..)
        | Self::BitwiseXor(l, ..)
        | Self::BitwiseOr(l, ..)
        | Self::Less(l, ..)
        | Self::More(l, ..)
        | Self::LessEqual(l, ..)
        | Self::MoreEqual(l, ..)
        | Self::Equal(l, ..)
        | Self::NotEqual(l, ..)
        | Self::And(l, ..)
        | Self::Or(l, ..)
        | Self::ConditionalOperator(l, ..)
        | Self::Assign(l, ..)
        | Self::AssignOr(l, ..)
        | Self::AssignAnd(l, ..)
        | Self::AssignXor(l, ..)
        | Self::AssignShiftLeft(l, ..)
        | Self::AssignShiftRight(l, ..)
        | Self::AssignAdd(l, ..)
        | Self::AssignSubtract(l, ..)
        | Self::AssignMultiply(l, ..)
        | Self::AssignDivide(l, ..)
        | Self::AssignModulo(l, ..)
        | Self::NumberLiteral(l, ..)
        | Self::RationalNumberLiteral(l, ..)
        | Self::HexNumberLiteral(l, ..)
        | Self::ArrayLiteral(l, ..)
        | Self::Initializer(l, ..)
        | Self::List(l, ..)
        | Self::Type(l, ..)
        | Self::Cast(l, ..)
        | Self::AddressLiteral(l, ..) => l,
    }

    ast::Import: match self {
        Self::GlobalSymbol(.., l)
        | Self::Plain(.., l)
        | Self::Rename(.., l) => l,
    }

    ast::Mutability: match self {
        Self::Constant(l, ..)
        | Self::Payable(l, ..)
        | Self::Pure(l, ..)
        | Self::View(l, ..) => l,
    }

    ast::SourceUnitPart: match self {
        Self::ImportDirective(ref l, ..) => l.loc(),
        Self::EnumDefinition(ref l, ..) => l.loc(),
        Self::StructDefinition(ref l, ..) => l.loc(),
        Self::ErrorDefinition(ref l, ..) => l.loc(),
        Self::FunctionDefinition(ref l, ..) => l.loc(),
        Self::VariableDefinition(ref l, ..) => l.loc(),
        Self::TypeDefinition(ref l, ..) => l.loc(),
        Self::AnnotationDefinition(ref l, ..) => l.loc(),
        Self::PropertyDefinition(ref l, ..) => l.loc.loc(),
        Self::Using(ref l, ..) => l.loc(),
        | Self::StraySemicolon(l, ..) => l,
    }

    ast::Statement: match self {
        Self::Block { loc: l, .. }
        | Self::Assembly { loc: l, .. }
        | Self::Args(l, ..)
        | Self::If(l, ..)
        | Self::While(l, ..)
        | Self::Expression(l, ..)
        | Self::VariableDefinition(l, ..)
        | Self::For(l, ..)
        | Self::DoWhile(l, ..)
        | Self::Continue(l, ..)
        | Self::Break(l, ..)
        | Self::Return(l, ..)
        | Self::Formula{loc: l, ..}
        | Self::Error(l, ..) => l,
    }

    ast::StorageLocation: match self {
        | Self::Memory(l, ..) => l,
    }

    ast::UsingList: match self {
        Self::Library(ref l, ..) => l.loc(),
        Self::Functions(ref l, ..) => l.loc_opt().unwrap_or_default(),
        Self::Error => panic!("an error occurred"),
    }

    ast::VariableAttribute: match self {
        Self::Visibility(ref l, ..) => l.loc_opt().unwrap_or_default(),
        Self::Constant(l, ..)
        | Self::Readable(l, ..)
        | Self::Writable(l, ..)
        | Self::Portable(l, ..) => l,
    }

    ast::FormulaExpression: match self {
        Self::HexStringLiteral(ref l, ..) => l.loc(),
        Self::StringLiteral(ref l, ..) => l.loc(),
        Self::Variable(ref l, ..) => l.loc(),
        Self::FunctionCall(ref l, ..) => l.loc(),
        Self::Parenthesis(ref l, ..) => l.loc(),
        Self::BoolLiteral(l, ..)
        | Self::NumberLiteral(l, ..)
        | Self::HexNumberLiteral(l, ..)
        | Self::SuffixAccess(l, ..) => l,
    }

    ast::FormulaStatement: match self {
        Self::Block(ref l, ..) => l.loc(),
        Self::Expression(ref l, ..) => l.loc(),
        Self::FunctionCall(ref l, ..) => l.loc(),
        | Self::Error(l, ..) => l,
    }

    // other
    LexicalError: match self {
        Self::EndOfFileInComment(l)
        | Self::EndOfFileInString(l)
        | Self::EndofFileInHex(l)
        | Self::MissingNumber(l)
        | Self::InvalidCharacterInHexLiteral(l, _)
        | Self::UnrecognisedToken(l, _)
        | Self::ExpectedFrom(l, _)
        | Self::MissingExponent(l) => l,
    }
}

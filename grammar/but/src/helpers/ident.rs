use std::fmt::{Debug, Display, Formatter, Pointer, Write};

use crate::ast::{
    Annotation, AnnotationDefinition, FunctionDefinition, IdentifierPath, ModelDefinition,
    PropertyDefinition, SourceUnit, SourceUnitPart, TypeDefinition, VariableDefinition,
};

const IDENT: usize = 2;

pub trait DisplayIdent {
    fn ident(&self, ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result;
}

pub struct FormatterIdent {
    ident: usize,
    target: Box<dyn DisplayIdent>,
}

impl FormatterIdent {
    pub fn new(ident: usize, target: Box<dyn DisplayIdent>) -> Self {
        FormatterIdent { ident, target }
    }
}

impl Display for FormatterIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.target.ident(2usize, f)
    }
}

impl DisplayIdent for SourceUnit {
    fn ident(&self, ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("SourceUnit (")?;
        let parts = &self.0;
        if parts.len() > 0 {
            f.write_str("\n")?;
        }
        for part in parts.iter() {
            part.ident(ident, f)?;
            f.write_str(",\n")?;
        }
        f.write_str(")")?;
        Ok(())
    }
}

impl DisplayIdent for SourceUnitPart {
    fn ident(&self, ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceUnitPart::ImportDirective(_) => {}
            SourceUnitPart::EnumDefinition(e) => {}
            SourceUnitPart::StructDefinition(_) => {}
            SourceUnitPart::ErrorDefinition(_) => {}
            SourceUnitPart::FunctionDefinition(function) => function.ident(ident, f)?,
            SourceUnitPart::FormulaDefinition(_) => {}
            SourceUnitPart::VariableDefinition(variable) => variable.ident(ident, f)?,
            SourceUnitPart::AnnotationDefinition(annotation) => annotation.ident(ident, f)?,
            SourceUnitPart::PropertyDefinition(property) => property.ident(ident, f)?,
            SourceUnitPart::ModelDefinition(model) => model.ident(ident, f)?,
            SourceUnitPart::TypeDefinition(typedef) => typedef.ident(ident, f)?,
            SourceUnitPart::Using(_) => {}
            SourceUnitPart::StraySemicolon(_) => {}
        }
        Ok(())
    }
}

impl DisplayIdent for FunctionDefinition {
    fn ident(&self, ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        lpad(ident, f)?;
        f.write_str("FunctionDefinition (")?;
        f.write_str(")")?;
        Ok(())
    }
}

impl DisplayIdent for VariableDefinition {
    fn ident(&self, ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        lpad(ident, f)?;
        f.write_str("VariableDefinition (")?;
        f.write_str(")")?;
        Ok(())
    }
}

impl DisplayIdent for PropertyDefinition {
    fn ident(&self, ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        lpad(ident, f)?;
        f.write_str("PropertyDefinition (")?;
        f.write_str(")")?;
        Ok(())
    }
}

impl DisplayIdent for ModelDefinition {
    fn ident(&self, ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        lpad(ident, f)?;
        f.write_str("ModelDefinition (")?;
        f.write_str(")")?;
        Ok(())
    }
}

impl DisplayIdent for TypeDefinition {
    fn ident(&self, ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        lpad(ident, f)?;
        f.write_str("TypeDefinition (")?;
        f.write_str(")")?;
        Ok(())
    }
}

impl DisplayIdent for AnnotationDefinition {
    fn ident(&self, ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        lpad(ident, f)?;
        let new_ident = ident * 2;
        f.write_str("AnnotationDefinition (\n")?;
        lpad(new_ident, f)?;
        write!(f, "global: {},\n", self.glob.to_string())?;
        lpad(new_ident, f)?;
        write!(f, "arguments: [\n")?;
        for annotation in self.args.iter() {
            annotation.ident(new_ident + ident, f)?;
            f.write_str(", \n")?;
        }
        lpad(new_ident, f)?;
        write!(f, "]\n")?;
        lpad(ident, f)?;
        f.write_str(")")?;
        Ok(())
    }
}

impl DisplayIdent for Annotation {
    fn ident(&self, ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        lpad(ident, f)?;
        let new_ident = ident + ident;
        f.write_str("Annotation (\n")?;
        match self {
            Annotation::Identifier(_, id) => {
                id.ident(new_ident, f)?;
            }
            Annotation::Function { loc: _, name, args } => {
                lpad(new_ident, f)?;
                f.write_str("Function (\n")?;
                lpad(new_ident + ident, f)?;
                f.write_str("name: ")?;
                Display::fmt(&name.name, f)?;
                f.write_str(", \n")?;

                lpad(new_ident + ident, f)?;
                f.write_str("arguments: [\n")?;
                for annotation in args.iter() {
                    annotation.ident(new_ident + ident + ident, f)?;
                    f.write_str(", \n")?;
                }
                lpad(new_ident, f)?;
                f.write_str("])\n")?;
            }
            Annotation::Assign { .. } => {}
            Annotation::String(_) => {}
            Annotation::Number(_, _) => {}
            Annotation::Rational(_, _, _) => {}
            Annotation::Boolean(_, _) => {}
            Annotation::Visibility(_, _) => {}
        }
        lpad(ident, f)?;
        f.write_str(")")?;
        Ok(())
    }
}

impl DisplayIdent for IdentifierPath {
    fn ident(&self, ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        lpad(ident, f)?;
        f.write_str("IdentifierPath (")?;
        let mut path = String::new();
        for id in self.identifiers.iter() {
            if path.len() > 0 {
                path += "::";
            }
            path += &*format!("{}", id.name);
        }
        Display::fmt(&path, f)?;
        f.write_str(")")?;
        Ok(())
    }
}

fn lpad(ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
    for _ in 0..ident {
        f.write_char(' ')?;
    }
    Ok(())
}

struct Ident<'a> {
    f: &'a mut Formatter<'a>,
    ident_size: usize,
    ident: usize,
}

impl<'a> Ident<'a> {
    fn new(ident_size: usize, f: &'a mut Formatter<'a>) -> Self {
        Self {
            f,
            ident_size,
            ident: 0,
        }
    }

    fn lpad(ident: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        for _ in 0..ident {
            f.write_char(' ')?;
        }
        Ok(())
    }

    fn next(self) -> Self {
        Self {
            ident: self.ident + self.ident_size,
            ..self
        }
    }

    fn prev(self) -> Self {
        assert!(self.ident - self.ident_size > 0);
        Self {
            ident: self.ident - self.ident_size,
            ..self
        }
    }
}

mod expression;

use crate::bitaccess::Checker;
use crate::model::expression::{transform, Expression};
use but_grammar::ast::{
    Annotation, AnnotationDefinition, EnumDefinition, FunctionDefinition, Loc,
    ModelDefinition, ModelPart, PropertyDefinition, SourceUnit, SourceUnitPart, StructDefinition,
    Type, TypeDefinition, VariableDefinition,
};
use but_grammar::diagnostics::Diagnostic;
use log::warn;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct EntryPoint {
    pub name: String,
    pub display_name: String,
    pub implement: Expression,
}

fn extract(
    parent: Option<Box<Model>>,
    model_definition: ModelDefinition,
) -> Result<Model, Vec<Diagnostic>> {
    let mut models = Vec::new();
    let mut enums = Vec::new();
    let mut structs = Vec::new();
    let mut functions = Vec::new();
    let mut formulas = Vec::new();
    let mut variables = Vec::new();
    let mut annotations = Vec::new();
    let mut properties = Vec::new();
    let mut types = Vec::new();
    model_definition.clone().annotations.iter().for_each(|x| {
        annotations.push(Box::new(x.clone()));
    });
    for x in model_definition.parts.iter() {
        if let ModelPart::ModelDefinition(x) = x {
            let x = extract(None, *x.clone())?;
            models.push(x);
        } else if let ModelPart::EnumDefinition(x) = x {
            enums.push(x);
        } else if let ModelPart::StructDefinition(x) = x {
            structs.push(x);
        } else if let ModelPart::FunctionDefinition(x) = x {
            functions.push(x);
        } else if let ModelPart::FormulaDefinition(x) = x {
            formulas.push(x);
        } else if let ModelPart::VariableDefinition(x) = x {
            variables.push(x);
        } else if let ModelPart::AnnotationDefinition(x) = x {
            annotations.push(x.clone());
        } else if let ModelPart::PropertyDefinition(x) = x {
            properties.push(x);
        } else if let ModelPart::TypeDefinition(x) = x {
            types.push(x);
        } else {
            warn!("Unhandled part: {:?}", x);
        }
    }
    let name = model_definition.clone().name.map(|id| id.name.clone());
    let model = Model {
        parent: parent.clone(),
        name: name.clone(),
        entrypoint: search_entry_point(models.clone()),
        implement: model_definition
            .implements
            .map(|x| transform(x).unwrap())
            .unwrap_or(Expression::Identifier(
                name.unwrap_or("Undefined".to_string()),
            )),
        models: HashMap::new(),
        enum_table: enums
            .into_iter()
            .map(|e| (e.clone().name.unwrap().name, e.clone()))
            .collect(),
        type_table: types
            .into_iter()
            .map(|t| (t.clone().name.name, t.clone()))
            .collect(),
        property_table: properties
            .into_iter()
            .map(|p| (p.clone().name.unwrap().name, p.clone()))
            .collect(),
        variable_table: variables
            .into_iter()
            .map(|v| (v.clone().name.unwrap().name, v.clone()))
            .collect(),
        struct_table: structs
            .into_iter()
            .map(|s| (s.clone().name.unwrap().name, s.clone()))
            .collect(),
        function_table: functions
            .into_iter()
            .map(|f| (f.clone().name.unwrap().name, f.clone()))
            .collect(),
        annotations,
    };
    Ok(Model {
        models: models
            .into_iter()
            .map(|m| {
                (
                    m.clone().name.unwrap(),
                    Box::new(Model {
                        parent: Some(Box::new(model.clone())),
                        ..m
                    }),
                )
            })
            .collect(),
        ..model
    })
}

fn search_entry_point(models: Vec<Model>) -> Option<EntryPoint> {
    let mut main_model = None;

    for model in models.clone() {
        for x in model.clone().annotations.clone() {
            for x in x.args.iter() {
                if let Annotation::Identifier(_, path) = x
                    && path.identifiers.iter().any(|a| a.name == "main")
                {
                    if main_model.is_none() {
                        main_model = Some(model.clone());
                    } else {
                        warn!("More than one main model found. {:?}", model);
                    }
                }
            }
        }
    }
    if let Some(main_model) = main_model {
        let mut display_name = String::new();
        for x in main_model.annotations.iter() {
            for x in x.args.iter() {
                if let Annotation::Function {
                    loc: _,
                    name: id,
                    args,
                } = x
                    && id.name == "name"
                {
                    if args.len() == 1 {
                        display_name = args
                            .iter()
                            .map(|a| {
                                if let Annotation::Identifier(_, id) = a {
                                    id.identifiers.iter().map(|i| i.name.clone()).collect()
                                } else if let Annotation::String(text) = a {
                                    text.string.clone()
                                } else {
                                    "".to_string()
                                }
                            })
                            .filter(|s| !s.is_empty())
                            .collect();
                    }
                }
            }
        }
        if display_name.is_empty() {
            display_name = main_model
                .clone()
                .clone()
                .name
                .map(|id| id.clone())
                .unwrap_or("Unknown".to_string());
        }
        let name = main_model.clone().name.unwrap().clone();
        Some(EntryPoint {
            name: name.clone(),
            display_name: display_name.clone(),
            implement: main_model
                .clone()
                .entrypoint
                .map(|e| e.implement)
                .unwrap_or_else(|| main_model.clone().implement.clone()),
        })
    } else {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Model {
    parent: Option<Box<Model>>,
    name: Option<String>,
    entrypoint: Option<EntryPoint>,
    implement: Expression,
    models: HashMap<String, Box<Model>>,
    annotations: Vec<Box<AnnotationDefinition>>,
    enum_table: HashMap<String, Box<EnumDefinition>>,
    type_table: HashMap<String, Box<TypeDefinition>>,
    property_table: HashMap<String, Box<PropertyDefinition>>,
    variable_table: HashMap<String, Box<VariableDefinition>>,
    struct_table: HashMap<String, Box<StructDefinition>>,
    function_table: HashMap<String, Box<FunctionDefinition>>,
}

pub trait Visitor {
    fn visit_model(&mut self, model: &Model) -> Result<bool, Vec<Diagnostic>>;
    fn visit_annotation(
        &mut self,
        annotation: &AnnotationDefinition,
    ) -> Result<bool, Vec<Diagnostic>>;
    fn visit_enum(&mut self, enm: &EnumDefinition) -> Result<bool, Vec<Diagnostic>>;
    fn visit_struct(&mut self, st: &StructDefinition) -> Result<bool, Vec<Diagnostic>>;
    fn visit_variable(&mut self, var: &VariableDefinition) -> Result<bool, Vec<Diagnostic>>;
    fn visit_function(&mut self, fun: &FunctionDefinition) -> Result<bool, Vec<Diagnostic>>;
    fn visit_type(&mut self, ty: &TypeDefinition) -> Result<bool, Vec<Diagnostic>>;
    fn visit_property(&mut self, p: &PropertyDefinition) -> Result<bool, Vec<Diagnostic>>;
}

impl Model {
    pub fn visit(&self, visitor: &mut dyn Visitor) -> Result<(), Vec<Diagnostic>> {
        for (_, model) in self.models.iter() {
            let next = visitor.visit_model(model)?;
            if !next {
                break;
            }
            for (_, model) in model.models.iter() {
                model.visit(visitor)?;
            }
        }
        for a in self.annotations.iter() {
            let next = visitor.visit_annotation(a)?;
            if !next {
                break;
            }
        }
        Ok(())
    }

    pub(crate) fn analyze(&self) -> Result<EntryPoint, Vec<Diagnostic>> {
        if let Some(entrypoint) = self.entrypoint.clone() {
            entrypoint.implement.visit(&|s| {
                let search = self.get_model(&s, false);
                if search.is_none() {
                    return Err(vec![Diagnostic::decl_error(
                        Loc::Builtin,
                        format!("Can't find model: {}", s),
                    )]);
                }
                Ok(())
            })?;
            let mut checker = Checker::new(self);
            self.visit(&mut checker)?;

            if !checker.diags.is_empty() {
                return Err(checker.diags);
            }
            Ok(entrypoint)
        } else {
            Err(vec![Diagnostic::decl_error(
                Loc::Builtin,
                String::from("Can't find main model"),
            )])
        }
    }

    pub fn get_model(&self, name: &str, upper: bool) -> Option<&Model> {
        if let Some(model) = self.models.get(name) {
            return Some(model);
        } else if let Some(parent) = &self.parent
            && upper
        {
            return parent.get_model(name, upper);
        }
        None
    }

    pub fn get_models(&self, upper: bool) -> Vec<Model> {
        let mut v = Vec::new();
        self.models.iter().for_each(|(_, model)| {
            v.push(*model.clone());
        });
        if upper {
            v.clone().iter().for_each(|model| {
                model.get_models(upper).iter().for_each(|model| {
                    v.push(model.clone());
                })
            })
        }
        v
    }

    pub fn get_variable_types(&self) -> HashMap<String, Type> {
        let mut map = HashMap::new();
        self.variable_table.iter().for_each(|(k, v)| {
            map.insert(k.clone(), v.ty.clone());
        });
        map
    }

    pub fn get_types(&self) -> HashMap<String, Box<TypeDefinition>> {
        let mut map = HashMap::new();
        self.type_table.iter().for_each(|(k, v)| {
            map.insert(k.clone(), v.clone());
        });
        map
    }

    /// All variables of the model including the parent chain (parent has lower priority).
    pub fn get_all_variable_types(&self) -> HashMap<String, Type> {
        let mut map = self
            .parent
            .as_ref()
            .map(|p| p.get_all_variable_types())
            .unwrap_or_default();
        map.extend(self.get_variable_types());
        map
    }

    /// All type aliases including the parent chain.
    pub fn get_all_types(&self) -> HashMap<String, Box<TypeDefinition>> {
        let mut map = self
            .parent
            .as_ref()
            .map(|p| p.get_all_types())
            .unwrap_or_default();
        map.extend(self.get_types());
        map
    }

    /// Iterator over the model's variables.
    pub fn variables(&self) -> impl Iterator<Item = &VariableDefinition> {
        self.variable_table.values().map(|v| v.as_ref())
    }

    /// Iterator over the model's properties.
    pub fn properties(&self) -> impl Iterator<Item = &PropertyDefinition> {
        self.property_table.values().map(|p| p.as_ref())
    }

    /// Iterator over the model's functions.
    pub fn functions(&self) -> impl Iterator<Item = &FunctionDefinition> {
        self.function_table.values().map(|f| f.as_ref())
    }
}

impl Model {
    pub fn new(unit: SourceUnit) -> Self {
        let mut models = Vec::new();
        let mut enums = Vec::new();
        let mut structs = Vec::new();
        let mut functions = Vec::new();
        let mut formulas = Vec::new();
        let mut variables = Vec::new();
        let mut annotations = Vec::new();
        let mut properties = Vec::new();
        let mut types = Vec::new();
        for x in unit.clone().0 {
            if let SourceUnitPart::ModelDefinition(x) = x {
                let x = extract(None, *x.clone()).unwrap();
                models.push(x.clone());
            } else if let SourceUnitPart::EnumDefinition(x) = x {
                enums.push(x);
            } else if let SourceUnitPart::StructDefinition(x) = x {
                structs.push(x);
            } else if let SourceUnitPart::FunctionDefinition(x) = x {
                functions.push(x);
            } else if let SourceUnitPart::FormulaDefinition(x) = x {
                formulas.push(x);
            } else if let SourceUnitPart::VariableDefinition(x) = x {
                variables.push(x);
            } else if let SourceUnitPart::AnnotationDefinition(x) = x {
                annotations.push(x);
            } else if let SourceUnitPart::PropertyDefinition(x) = x {
                properties.push(x);
            } else if let SourceUnitPart::TypeDefinition(x) = x {
                types.push(x);
            } else {
                warn!("Unhandled part: {:?}", x);
            }
        }
        let entry_point = search_entry_point(models.clone());
        let model = Self {
            parent: None,
            name: entry_point.clone().map(|e| e.name.clone()),
            entrypoint: entry_point.clone(),
            implement: entry_point
                .clone()
                .map(|e| e.implement)
                .unwrap_or(Expression::Identifier("Undefined".to_string())),
            models: HashMap::new(),
            annotations,
            enum_table: enums
                .into_iter()
                .map(|e| (e.clone().name.unwrap().name, e))
                .collect(),
            type_table: types
                .into_iter()
                .map(|t| (t.clone().name.name, t))
                .collect(),
            property_table: properties
                .into_iter()
                .map(|p| (p.clone().name.unwrap().name, p))
                .collect(),
            variable_table: variables
                .into_iter()
                .map(|v| (v.clone().name.unwrap().name, v))
                .collect(),
            struct_table: structs
                .into_iter()
                .map(|s| (s.clone().name.unwrap().name, s))
                .collect(),
            function_table: functions
                .into_iter()
                .map(|f| (f.clone().name.unwrap().name, f))
                .collect(),
        };
        Self {
            models: models
                .into_iter()
                .map(|m| {
                    (
                        m.clone().name.unwrap(),
                        Box::new(Model {
                            parent: Some(Box::new(model.clone())),
                            ..m
                        }),
                    )
                })
                .collect(),
            ..model
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_model() {
        let src = r#"
            #[main, name(Main)]
            model One {
                model Inner {
                }
                start Start {
                    ref End: true;
                }
                state End {}
            }
        "#;
        let unit = but_grammar::parse(src, 0).unwrap();
        let model = Model::new(unit.0);
        let result = model.analyze();
        assert_eq!(result.is_ok(), true);
    }

    #[test]
    fn analyze() {
        let src = r#"
        model Processor {
            start Start {
                ref End: is_complete;
            }
            state End {}
        }
        model Memory {
            start Start {
                ref End: is_complete;
            }
            state End {
                enter -> is_complete = false;
            }
        }
        model Floppy {
            start Start {
                ref End: is_complete;
            }
            state End {}
        }

        #[main, name(Main)]
        model Refresh = (Processor | Memory) + Floppy {}

        port is_complete: bool = 0:0x058849;
        "#;
        let unit = but_grammar::parse(src, 0).unwrap();
        let model = Model::new(unit.0);
        let result = model.analyze();
        assert_eq!(result.is_ok(), true);
    }
}

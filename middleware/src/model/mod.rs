mod expression;

use crate::bitaccess;
use crate::model::expression::{Expression, transform};
use but_grammar::ast::{
    Annotation, AnnotationDefinition, EnumDefinition, FunctionDefinition, Identifier, Loc,
    ModelDefinition, ModelPart, PropertyDefinition, SourceUnit, SourceUnitPart, StructDefinition,
    TypeDefinition, VariableDefinition,
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
        models: models
            .into_iter()
            .map(|m| (m.clone().name.unwrap(), Box::new(m)))
            .collect(),
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
    Ok(model)
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

impl Model {
    pub(crate) fn analyze(&self) -> Result<EntryPoint, Vec<Diagnostic>> {
        // let mut diagnostics = Vec::new();
        // let globe = self
        //     .variable_table
        //     .iter()
        //     .map(|v| (v.0.clone(), v.1.ty.clone()))
        //     .collect();
        // let models = <HashMap<String, Box<Model>> as Clone>::clone(&self.models)
        //     .into_values()
        //     .collect::<Vec<Box<Model>>>();
        // bitaccess::check_models(&models, &globe, &self.type_table, &mut diagnostics);
        //
        // if !diagnostics.is_empty() {
        //     return Err(diagnostics);
        // }
        if let Some(entrypoint) = self.entrypoint.clone() {
            let models = self.models.clone();
            entrypoint.implement.visit(&|s| {
                let search = models.get(&s);
                if search.is_none() {
                    return Err(vec![Diagnostic::decl_error(
                        Loc::Builtin,
                        format!("Can't find model: {}", s),
                    )]);
                }
                Ok(())
            })?;
            return Ok(entrypoint);
        } else {
            Err(vec![Diagnostic::decl_error(
                Loc::Builtin,
                String::from("Can't find main model"),
            )])
        }
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
    fn analyze() {
        let src = r#"
        model Processor {
            //Начальное состояние
            start Start {
                ref End: is_complete;
            }
            //Терминальное состояние - нет переходов
            //Модель считается завершенной/отработанной если она находится в одном из терминальных состояний
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

        //Атрибутами задается основная модель которая будет попадать при генерации кода
        //main - основная модель. Если модель не имеет этого атрибута или не используется явно или не явно - код для нее не генерируется
        //name(Main) - задает имя модели при генерации
        #[main, name(Main)]
        //Описывает сложную компоновку модели
        model Refresh = (Processor | Memory) + Floppy {}

        port is_complete: bool = 0:0x058849;
        "#;
        let unit = but_grammar::parse(src, 0).unwrap();
        let model = Model::new(unit.0);
        let result = model.analyze();
        assert_eq!(result.is_ok(), true);
    }
}

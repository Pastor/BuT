mod expression;

use crate::model::expression::{transform, Expression};
use but_grammar::ast::{
    Annotation, EnumDefinition, FunctionDefinition, Identifier, Loc, ModelDefinition,
    PropertyDefinition, SourceUnit, SourceUnitPart, StructDefinition, TypeDefinition,
    VariableDefinition,
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

pub struct Model {
    models: HashMap<String, Box<ModelDefinition>>,
    enum_table: HashMap<String, Box<EnumDefinition>>,
    type_table: HashMap<String, Box<TypeDefinition>>,
    property_table: HashMap<String, Box<PropertyDefinition>>,
    variable_table: HashMap<String, Box<VariableDefinition>>,
    struct_table: HashMap<String, Box<StructDefinition>>,
    function_table: HashMap<String, Box<FunctionDefinition>>,
}

impl Model {
    pub(crate) fn analyze(&self) -> Result<EntryPoint, Vec<Diagnostic>> {
        if let Some(main) = self.search_entry_point() {
            Ok(main)
        } else {
            Err(vec![Diagnostic::decl_error(
                Loc::Builtin,
                String::from("Can't find main model"),
            )])
        }
    }

    fn search_entry_point(&self) -> Option<EntryPoint> {
        let mut main_model = None;

        let binding = self.models.values().into_iter().collect::<Vec<_>>();
        for model in binding {
            for x in model.annotations.clone() {
                for x in x.args.iter() {
                    if let Annotation::Identifier(_, path) = x
                        && path.identifiers.iter().any(|a| a.name == "main")
                    {
                        if main_model.is_none() {
                            main_model = Some(model);
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
                    .map(|id| id.name.clone())
                    .unwrap_or("Unknown".to_string());
            }
            Some(EntryPoint {
                name: main_model.clone().name.unwrap().name.clone(),
                display_name: display_name.clone(),
                implement: transform(main_model.clone().clone().implements.unwrap_or(
                    but_grammar::ast::Expression::Variable(Identifier::new(display_name.clone())),
                ))
                .unwrap(),
            })
        } else {
            None
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
                models.push(x);
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
        Self {
            models: models
                .into_iter()
                .map(|m| (m.clone().name.unwrap().name, m))
                .collect(),
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

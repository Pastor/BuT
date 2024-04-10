use std::collections::HashMap;

use but_grammar::ast::{EnumDefinition, FunctionDefinition, Identifier, Loc, ModelDefinition, PropertyDefinition, StructDefinition, TypeDefinition, VariableDefinition};
use but_grammar::diagnostics::Diagnostic;
use crate::unit::Unit;

pub(crate) struct TreeDefinition {
    models: Vec<Box<ModelDefinition>>,
    enum_table: HashMap<String, Box<EnumDefinition>>,
    type_table: HashMap<String, Box<TypeDefinition>>,
    property_table: HashMap<String, Box<PropertyDefinition>>,
    variable_table: HashMap<String, Box<VariableDefinition>>,
    struct_table: HashMap<String, Box<StructDefinition>>,
    function_table: HashMap<String, Box<FunctionDefinition>>,
}

impl TreeDefinition {
    pub(crate) fn new(models: Vec<Box<ModelDefinition>>,
                      global_enums: Vec<Box<EnumDefinition>>,
                      global_types: Vec<Box<TypeDefinition>>,
                      global_properties: Vec<Box<PropertyDefinition>>,
                      global_variables: Vec<Box<VariableDefinition>>,
                      global_structs: Vec<Box<StructDefinition>>,
                      global_functions: Vec<Box<FunctionDefinition>>,
                      diagnostics: &mut Vec<Diagnostic>) -> Self {
        let mut type_table = HashMap::new();
        for mut td in global_types {
            td = td.clone();
            let name = td.clone().name;
            if type_table.contains_key(&name.name) {
                let v: &Box<TypeDefinition> = type_table.get(&name.name).unwrap();
                diagnostics.push(Diagnostic::error(v.loc, format!("type '{}' already defined", name)));
            } else {
                type_table.insert(name.name, td);
            }
        }
        let enum_table = build_map("enum",
                                   global_enums, diagnostics, |d| { (d.name, d.loc) });
        let property_table = build_map("property",
                                       global_properties, diagnostics, |d| { (d.name, d.loc) });
        let variable_table = build_map("variable",
                                       global_variables, diagnostics, |d| { (d.name, d.loc) });
        let struct_table = build_map("struct",
                                     global_structs, diagnostics, |d| { (d.name, d.loc) });
        let function_table = build_map("function",
                                       global_functions, diagnostics, |d| { (d.name, d.loc) });
        return TreeDefinition {
            models,
            enum_table,
            type_table,
            property_table,
            variable_table,
            struct_table,
            function_table,
        };
    }

    pub(crate) fn build_tree(&self) -> Result<Unit, Vec<Diagnostic>> {
        return Err(vec![Diagnostic::error(Loc::Implicit, "Empty".to_string())]);
    }
}

#[inline]
fn build_map<Definition: Clone, F>(build_name: &str,
                                   values: Vec<Box<Definition>>,
                                   diagnostics: &mut Vec<Diagnostic>, d: F) -> HashMap<String, Box<Definition>>
    where F: Fn(Definition) -> (Option<Identifier>, Loc) {
    let mut table = HashMap::new();
    for mut fd in values {
        fd = fd.clone();
        let (name, loc) = d(*fd.clone());
        match name {
            None => {
                diagnostics.push(Diagnostic::warning(loc, format!("Unnamed {}", build_name)));
            }
            Some(id) => {
                if table.contains_key(&id.name) {
                    let v: &Box<Definition> = table.get(&id.name).unwrap();
                    diagnostics.push(Diagnostic::error(loc, format!("{} '{}' already defined", build_name, id.name)));
                } else {
                    table.insert(id.name, fd);
                }
            }
        }
    }
    return table;
}

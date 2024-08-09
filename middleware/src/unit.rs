use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

use but_grammar::ast::ModelDefinition;

pub struct Function {}

pub struct Enum {}

pub struct Type {}

pub struct Property {}

#[derive(PartialEq, Debug)]
pub struct Variable {}

pub struct Struct {}

pub struct Unit {
    parent: Option<Arc<RefCell<Unit>>>,
    models: HashMap<String, Model>,
    functions: HashMap<String, Function>,
    enums: HashMap<String, Enum>,
    types: HashMap<String, Type>,
    properties: HashMap<String, Property>,
    variables: HashMap<String, Variable>,
    structs: HashMap<String, Struct>,
}

impl Unit {
    fn global() -> Unit {
        Self::with_unit(None)
    }

    fn with_unit(unit: Option<Arc<RefCell<Unit>>>) -> Unit {
        Unit {
            parent: unit.clone(),
            models: HashMap::new(),
            functions: HashMap::new(),
            enums: HashMap::new(),
            types: HashMap::new(),
            properties: HashMap::new(),
            variables: HashMap::new(),
            structs: HashMap::new(),
        }
    }
    
    pub(crate) fn new() -> Self {
        Self {
            parent: None,
            models: Default::default(),
            functions: Default::default(),
            enums: Default::default(),
            types: Default::default(),
            properties: Default::default(),
            variables: Default::default(),
            structs: Default::default(),
        }
    }

    fn add_model(&mut self, md: ModelDefinition) {
        let rc = RefCell::new(&*self);
        let unit = Arc::new(rc);
        //let model = Model::new(unit);
        //self.models.insert(md.name.unwrap().name, model);
    }
}

pub struct Model {
    unit: Arc<RefCell<Unit>>,
}

impl Model {
    fn new(unit: Arc<RefCell<Unit>>) -> Model {
        Model {
            unit: unit.clone()
        }
    }
}


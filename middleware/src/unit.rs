use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

use but_grammar::ast::ModelDefinition;

pub struct Function {}

pub struct Enum {}

pub struct Type {}

pub struct Property {}

pub struct Variable {}

pub struct Struct {}

trait Context<'a> {
    fn variable(&'a self, name: &'a str) -> Option<&'a Variable>;
}

pub struct Unit {
    unit: Option<Arc<RefCell<Unit>>>,
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
            unit: unit.clone(),
            models: HashMap::new(),
            functions: HashMap::new(),
            enums: HashMap::new(),
            types: HashMap::new(),
            properties: HashMap::new(),
            variables: HashMap::new(),
            structs: HashMap::new(),
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

impl<'a> Context<'a> for Unit {
    fn variable(&'a self, name: &'a str) -> Option<&'a Variable> {
        let v = self.variables.get(name);
        if v.is_some() {
            return v;
        }
        None
        //self.unit.as_ref().and_then(|ctx| ctx.as_ref().borrow().variable(name))
    }
}

impl<'a> Context<'a> for Model {
    fn variable(&'a self, name: &'a str) -> Option<&'a Variable> {
        //self.unit.as_ref().borrow().variable(name)
        None
    }
}


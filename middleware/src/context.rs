use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::sync::{Mutex, Once};

use but_grammar::ast::{ModelDefinition, VariableDefinition};

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum Variable {
    None,
    Defined(VariableDefinition),
}

pub(crate) trait VariableContext {
    fn variable_set(&mut self, key: &str, value: Variable);
    fn variable_del(&mut self, key: &str);
    fn variable_get(&self, key: &str) -> Option<Variable>;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum Model {
    None,
    Defined(ModelDefinition),
}

pub(crate) trait ModelContext {
    fn model_set(&mut self, key: &str, model: Model);
    fn model_del(&mut self, key: &str);
    fn model_get(&self, key: &str) -> Option<Model>;
}

pub(crate) trait Context: VariableContext + ModelContext {
    #[allow(clippy::new_ret_no_self)]
    fn new(&mut self) -> Box<dyn Context>;
}

#[derive(Debug, Default)]
struct Context_ {
    parent: Option<*mut dyn Context>,
    variables: HashMap<String, Variable>,
    models: HashMap<String, Model>,
}

impl VariableContext for Context_ {
    fn variable_set(&mut self, key: &str, value: Variable) {
        self.variables.insert(key.to_string(), value);
    }

    fn variable_del(&mut self, key: &str) {
        self.variables.remove(key);
    }

    fn variable_get(&self, key: &str) -> Option<Variable> {
        if let Some(v) = self.variables.get(key) {
            return Some(v.clone());
        }
        return self.parent.as_ref().and_then(|ctx| {
            unsafe { ctx.as_ref().unwrap().variable_get(key) }
        });
    }
}

impl ModelContext for Context_ {
    fn model_set(&mut self, key: &str, model: Model) {
        self.models.insert(key.to_string(), model);
    }

    fn model_del(&mut self, key: &str) {
        self.models.remove(key);
    }

    fn model_get(&self, key: &str) -> Option<Model> {
        if let Some(v) = self.models.get(key) {
            return Some(v.clone());
        }
        return self.parent.as_ref().and_then(|ctx| {
            unsafe { ctx.as_ref().unwrap().model_get(key) }
        });
    }
}

impl Context for Context_ {
    fn new(&mut self) -> Box<dyn Context> {
        Box::new(Context_ {
            parent: Some(&mut *self),
            variables: Default::default(),
            models: Default::default(),
        })
    }
}

impl Context_ {}

fn root() -> &'static Mutex<Box<dyn Context>> {
    static mut CONF: MaybeUninit<Mutex<Box<dyn Context>>> = MaybeUninit::uninit();
    static ONCE: Once = Once::new();
    ONCE.call_once(|| unsafe {
        CONF.as_mut_ptr().write(Mutex::new(Box::new(Context_ {
            parent: None,
            variables: Default::default(),
            models: Default::default(),
        })));
    });

    unsafe { &*CONF.as_ptr() }
}

#[derive(Debug)]
pub(crate) struct Root;

impl Root {
    fn set(key: &str, value: Variable) {
        root().lock().unwrap().variable_set(key, value)
    }

    fn del(key: &str) {
        root().lock().unwrap().variable_del(key)
    }

    fn get(key: &str) -> Option<Variable> {
        root().lock().unwrap().variable_get(key)
    }

    fn new_context() -> Box<dyn Context> {
        root().lock().unwrap().new()
    }
}

#[cfg(test)]
mod tests0 {
    use but_grammar::ast::{Identifier, Type, VariableDefinition};

    use crate::context::{Root, Variable};

    #[test]
    fn test_context() {
        let def_v0 = VariableDefinition {
            loc: Default::default(),
            ty: Type::Address,
            attrs: vec![],
            name: Some(Identifier { loc: Default::default(), name: "V0".to_string() }),
            initializer: None,
            annotations: vec![],
        };
        let def_v1 = VariableDefinition {
            loc: Default::default(),
            ty: Type::Address,
            attrs: vec![],
            name: Some(Identifier { loc: Default::default(), name: "V1".to_string() }),
            initializer: None,
            annotations: vec![],
        };
        Root::set("V0", Variable::Defined(def_v0.clone()));
        let mut root = Root::new_context();
        root.variable_set("V1", Variable::Defined(def_v1.clone()));
        let ctx = root.new();
        assert_eq!(ctx.variable_get("V1").unwrap(), Variable::Defined(def_v1));
        assert_eq!(ctx.variable_get("V0").unwrap(), Variable::Defined(def_v0));
        Root::del("V0");
        assert_eq!(ctx.variable_get("V0"), None);
    }
}

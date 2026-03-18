use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Bit(bool),
    BitVec(Vec<bool>),
    Bytes(Vec<u8>),
}

pub trait Context {
    fn set(&mut self, key: String, value: Value);
    fn get(&self, key: &str) -> Option<&Value>;
    fn remove(&mut self, key: &str) -> Option<Value>;
    fn contains_key(&self, key: &str) -> bool;
}

#[derive(Default, Clone)]
struct PrivateContext<'a> {
    parent: Option<&'a dyn Context>,
    variables: HashMap<String, Value>,
}

impl<'a> Context for PrivateContext<'a> {
    fn set(&mut self, key: String, value: Value) {
        self.variables.insert(key, value);
    }

    fn get(&self, key: &str) -> Option<&Value> {
        self.variables
            .get(key)
            .or_else(|| self.parent.and_then(|p| p.get(key)))
    }

    fn remove(&mut self, key: &str) -> Option<Value> {
        self.variables.remove(key)
    }

    fn contains_key(&self, key: &str) -> bool {
        self.variables.contains_key(key) || self.parent.is_some_and(|p| p.contains_key(key))
    }
}

pub fn new_root_context() -> Box<dyn Context> {
    Box::new(PrivateContext {
        parent: None,
        variables: HashMap::new(),
    })
}

pub fn new_context<'a>(parent: &'a dyn Context) -> Box<dyn Context + 'a> {
    Box::new(PrivateContext {
        parent: Some(parent),
        variables: HashMap::new(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{new_root_context, Value};
    use std::cell::RefCell;

    #[test]
    fn root_context_set_get() {
        let mut ctx = new_root_context();
        ctx.set("name".to_string(), Value::String("Alice".to_string()));
        ctx.set("age".to_string(), Value::Number(30.0));
        ctx.set("active".to_string(), Value::Boolean(true));

        assert_eq!(ctx.get("name"), Some(&Value::String("Alice".to_string())));
        assert_eq!(ctx.get("age"), Some(&Value::Number(30.0)));
        assert_eq!(ctx.get("active"), Some(&Value::Boolean(true)));
        assert_eq!(ctx.get("missing"), None);
    }

    #[test]
    fn root_context_remove() {
        let mut ctx = new_root_context();
        ctx.set("key".to_string(), Value::String("value".to_string()));
        assert!(ctx.contains_key("key"));

        let removed = ctx.remove("key");
        assert_eq!(removed, Some(Value::String("value".to_string())));
        assert!(!ctx.contains_key("key"));
        assert_eq!(ctx.get("key"), None);
    }

    #[test]
    fn child_inherits_from_parent() {
        let mut parent = new_root_context();
        parent.set(
            "parent_var".to_string(),
            Value::String("parent_value".to_string()),
        );
        parent.set("common".to_string(), Value::Number(42.0));

        let mut child = new_context(parent.as_ref());
        assert_eq!(
            child.get("parent_var"),
            Some(&Value::String("parent_value".to_string()))
        );
        assert_eq!(child.get("common"), Some(&Value::Number(42.0)));

        child.set("child_var".to_string(), Value::Boolean(false));
        assert_eq!(child.get("child_var"), Some(&Value::Boolean(false)));

        child.set(
            "common".to_string(),
            Value::String("overridden".to_string()),
        );
        assert_eq!(
            child.get("common"),
            Some(&Value::String("overridden".to_string()))
        );
        assert_eq!(parent.get("common"), Some(&Value::Number(42.0)));
    }

    #[test]
    fn child_remove_does_not_affect_parent() {
        let mut parent = new_root_context();
        parent.set("shared".to_string(), Value::String("parent".to_string()));

        let mut child = new_context(parent.as_ref());
        assert_eq!(
            child.get("shared"),
            Some(&Value::String("parent".to_string()))
        );

        child.remove("shared");
        assert_eq!(
            child.get("shared"),
            Some(&Value::String("parent".to_string()))
        ); // всё ещё видна от родителя
        assert_eq!(
            parent.get("shared"),
            Some(&Value::String("parent".to_string()))
        );

        child.set("shared".to_string(), Value::Number(100.0));
        assert_eq!(child.get("shared"), Some(&Value::Number(100.0)));
        child.remove("shared");
        assert_eq!(
            child.get("shared"),
            Some(&Value::String("parent".to_string()))
        );
    }

    #[test]
    fn chain_of_contexts() {
        let mut root = new_root_context();
        root.set("level".to_string(), Value::Number(0.0));
        root.set("root_only".to_string(), Value::Boolean(true));

        let mid = RefCell::new(new_context(root.as_ref()));
        let binding = mid.borrow();
        let leaf = new_context(binding.as_ref());

        assert_eq!(leaf.get("level"), Some(&Value::Number(0.0)));
        assert_eq!(leaf.get("root_only"), Some(&Value::Boolean(true)));

        let mid_mut = {
            let mut temp = new_context(root.as_ref());
            temp.set("level".to_string(), Value::Number(1.0));
            temp
        };

        let leaf = new_context(mid_mut.as_ref());
        assert_eq!(leaf.get("level"), Some(&Value::Number(1.0)));
    }
}

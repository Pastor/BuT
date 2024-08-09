use std::cell::{Cell, UnsafeCell};
use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::sync::{Mutex, Once};

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub(crate) enum Value {
    #[default]
    None,
    String(String),
    Number(i64),
    Boolean(bool),
}

impl Value {
    pub fn name(&self) -> &'static str {
        match self {
            Value::None => "none",
            Value::String(_) => "str",
            Value::Number(_) => "num",
            Value::Boolean(_) => "bool",
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Number(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::None
    }
}

pub(crate) trait Context {
    fn set(&mut self, key: &str, value: Value);
    fn del(&mut self, key: &str);
    fn get(&self, key: &str) -> Option<*mut Value>;
    #[allow(clippy::new_ret_no_self)]
    fn new(&mut self) -> Box<dyn Context>;
}

#[derive(Debug, Default)]
struct PrivateContext {
    parent: Option<*mut dyn Context>,
    values: HashMap<String, UnsafeCell<Value>>,
}

impl Context for PrivateContext {
    fn set(&mut self, key: &str, value: Value) {
        self.values.insert(key.to_string(), UnsafeCell::new(value));
    }

    fn del(&mut self, key: &str) {
        self.values.remove(key);
    }

    fn get(&self, key: &str) -> Option<*mut Value> {
        if let Some(v) = self.values.get(key) {
            return Some(v.get());
        }
        self.parent.as_ref().and_then(|ctx| {
            unsafe { ctx.as_ref().unwrap().get(key) }
        })
    }

    fn new(&mut self) -> Box<dyn Context> {
        Box::new(PrivateContext {
            parent: Some(&mut *self),
            values: Default::default(),
        })
    }
}

impl PrivateContext {}

fn root() -> &'static Mutex<Box<dyn Context>> {
    static mut CONF: MaybeUninit<Mutex<Box<dyn Context>>> = MaybeUninit::uninit();
    static ONCE: Once = Once::new();
    ONCE.call_once(|| unsafe {
        CONF.as_mut_ptr().write(Mutex::new(Box::new(PrivateContext::default())));
    });

    unsafe { &*CONF.as_ptr() }
}

#[derive(Debug)]
pub(crate) struct Root;

impl Root {
    fn set(key: &str, value: Value) {
        root().lock().unwrap().set(key, value)
    }

    fn del(key: &str) {
        root().lock().unwrap().del(key)
    }

    fn get(key: &str) -> Option<*mut Value> {
        root().lock().unwrap().get(key)
    }

    fn new_context() -> Box<dyn Context> {
        root().lock().unwrap().new()
    }
}

#[cfg(test)]
mod tests0 {
    use crate::context::{Root, Value};

    #[test]
    fn test_context() {
        Root::set("V0", Value::Boolean(true));
        let mut root = Root::new_context();
        root.set("V1", Value::Boolean(false));
        let ctx = root.new();
        unsafe { assert_eq!(*ctx.get("V1").unwrap(), Value::Boolean(false)); }
        unsafe { assert_eq!(*ctx.get("V0").unwrap(), Value::Boolean(true)); }
        Root::del("V0");
        assert_eq!(ctx.get("V0"), None);
        unsafe { *root.get("V1").unwrap() = Value::Number(30) }
        unsafe { assert_eq!(*ctx.get("V1").unwrap(), Value::Number(30)); }
    }
}

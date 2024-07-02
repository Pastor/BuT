use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::sync::{Mutex, Once};

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) enum Value {
    None,
    String(String),
    Number(i64),
    Boolean(bool),
}

#[derive(Debug, Clone)]
pub(crate) struct Context {
    parent: Option<*mut Context>,
    values: HashMap<String, Value>,
}

impl Context {
    pub(crate) fn new(&mut self) -> Context {
        Context {
            parent: Some(&mut *self),
            values: Default::default(),
        }
    }

    pub(crate) fn set(&mut self, key: &str, value: Value) {
        self.values.insert(key.to_string(), value);
    }

    pub(crate) fn get(&self, key: &str) -> Option<Value> {
        if let Some(v) = self.values.get(key) {
            return Some(v.clone());
        }
        return self.parent.as_ref().and_then(|ctx| get(*ctx, key));
    }
}

fn get(context: *mut Context, key: &str) -> Option<Value> {
    unsafe { context.as_ref().unwrap().get(key) }
}

fn root() -> &'static Mutex<Context> {
    static mut CONF: MaybeUninit<Mutex<Context>> = MaybeUninit::uninit();
    static ONCE: Once = Once::new();
    ONCE.call_once(|| unsafe {
        CONF.as_mut_ptr().write(Mutex::new(Context {
            parent: None,
            values: Default::default(),
        }));
    });

    unsafe { &*CONF.as_ptr() }
}

pub fn set(key: &str, value: Value) {
    root().lock().unwrap().set(key, value)
}
pub fn new() -> Context {
    root().lock().unwrap().new()
}

#[cfg(test)]
mod tests {
    use crate::context::{new, set, Value};

    #[test]
    fn test_context() {
        set("V1", Value::String("V1".to_string()));
        let mut new1 = new();
        new1.set("V2", Value::None);
        println!("new 1  : {:#?}", new1);
        let new2 = new();
        println!("new 2  : {:#?}", new2);
        let v = new2.get("V1");
        assert!(v.is_some());
        assert_eq!(v.unwrap(), Value::String("V1".to_string()));
    }
}
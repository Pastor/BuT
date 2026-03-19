use indexmap::IndexMap;
use crate::value::Value;

/// Simulation execution context: stores variables and I/O ports.
#[derive(Debug, Clone, Default)]
pub struct SimContext {
    /// Regular variables (let, const, global).
    vars: IndexMap<String, Value>,
    /// I/O port values.
    ports: IndexMap<String, Value>,
}

impl SimContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Look up a value — ports first, then variables.
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.ports.get(name).or_else(|| self.vars.get(name))
    }

    /// Set a variable value.
    pub fn set(&mut self, name: &str, val: Value) {
        // If the name is a port, update the port; otherwise update the variable.
        if self.ports.contains_key(name) {
            self.ports.insert(name.to_string(), val);
        } else {
            self.vars.insert(name.to_string(), val);
        }
    }

    /// Set a port value directly (for external input).
    pub fn set_port(&mut self, name: &str, val: Value) {
        self.ports.insert(name.to_string(), val);
    }

    /// Get a port value.
    pub fn get_port(&self, name: &str) -> Option<&Value> {
        self.ports.get(name)
    }

    /// Declare a port (with an initial value).
    pub fn declare_port(&mut self, name: &str, val: Value) {
        self.ports.insert(name.to_string(), val);
    }

    /// Declare a variable (with an initial value).
    pub fn declare_var(&mut self, name: &str, val: Value) {
        self.vars.insert(name.to_string(), val);
    }

    /// Iterator over all ports.
    pub fn ports(&self) -> impl Iterator<Item = (&str, &Value)> {
        self.ports.iter().map(|(k, v)| (k.as_str(), v))
    }

    /// Iterator over all variables.
    pub fn vars(&self) -> impl Iterator<Item = (&str, &Value)> {
        self.vars.iter().map(|(k, v)| (k.as_str(), v))
    }

    /// Merge variables from another context (for nested machines).
    pub fn merge_from(&mut self, other: &SimContext) {
        for (k, v) in &other.vars {
            self.vars.insert(k.clone(), v.clone());
        }
        for (k, v) in &other.ports {
            self.ports.insert(k.clone(), v.clone());
        }
    }
}

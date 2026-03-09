use indexmap::IndexMap;
use crate::value::Value;

/// Контекст выполнения симуляции: хранит переменные и порты ввода/вывода.
#[derive(Debug, Clone, Default)]
pub struct SimContext {
    /// Обычные переменные (let, const, глобальные).
    vars: IndexMap<String, Value>,
    /// Значения портов ввода/вывода.
    ports: IndexMap<String, Value>,
}

impl SimContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Поиск значения — сначала в портах, затем в переменных.
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.ports.get(name).or_else(|| self.vars.get(name))
    }

    /// Установить значение переменной.
    pub fn set(&mut self, name: &str, val: Value) {
        // Если имя — порт, обновляем порт; иначе переменную.
        if self.ports.contains_key(name) {
            self.ports.insert(name.to_string(), val);
        } else {
            self.vars.insert(name.to_string(), val);
        }
    }

    /// Установить значение порта напрямую (для внешнего ввода).
    pub fn set_port(&mut self, name: &str, val: Value) {
        self.ports.insert(name.to_string(), val);
    }

    /// Получить значение порта.
    pub fn get_port(&self, name: &str) -> Option<&Value> {
        self.ports.get(name)
    }

    /// Объявить порт (с начальным значением).
    pub fn declare_port(&mut self, name: &str, val: Value) {
        self.ports.insert(name.to_string(), val);
    }

    /// Объявить переменную (с начальным значением).
    pub fn declare_var(&mut self, name: &str, val: Value) {
        self.vars.insert(name.to_string(), val);
    }

    /// Итератор по всем портам.
    pub fn ports(&self) -> impl Iterator<Item = (&str, &Value)> {
        self.ports.iter().map(|(k, v)| (k.as_str(), v))
    }

    /// Итератор по всем переменным.
    pub fn vars(&self) -> impl Iterator<Item = (&str, &Value)> {
        self.vars.iter().map(|(k, v)| (k.as_str(), v))
    }

    /// Слияние переменных из другого контекста (для вложенных автоматов).
    pub fn merge_from(&mut self, other: &SimContext) {
        for (k, v) in &other.vars {
            self.vars.insert(k.clone(), v.clone());
        }
        for (k, v) in &other.ports {
            self.ports.insert(k.clone(), v.clone());
        }
    }
}

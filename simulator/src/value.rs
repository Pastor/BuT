/// Значение времени выполнения при симуляции автомата.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Пустое значение (единичный тип).
    Unit,
    /// Булево значение.
    Bool(bool),
    /// Целочисленное значение.
    Int(i64),
    /// Число с плавающей точкой.
    Real(f64),
    /// Строковое значение.
    Str(String),
    /// Однобитовое значение.
    Bit(bool),
}

impl Value {
    /// Возвращает `true`, если значение истинно (ненулевое / не false).
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Unit => false,
            Value::Bool(b) => *b,
            Value::Int(n) => *n != 0,
            Value::Real(r) => *r != 0.0,
            Value::Str(s) => !s.is_empty(),
            Value::Bit(b) => *b,
        }
    }

    /// Попытка преобразования в f64 для числовых сравнений.
    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Value::Int(n) => Some(*n as f64),
            Value::Real(r) => Some(*r),
            Value::Bit(b) => Some(if *b { 1.0 } else { 0.0 }),
            Value::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
            _ => None,
        }
    }

    /// Попытка преобразования в i64.
    pub fn to_i64(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            Value::Real(r) => Some(*r as i64),
            Value::Bit(b) => Some(if *b { 1 } else { 0 }),
            Value::Bool(b) => Some(if *b { 1 } else { 0 }),
            _ => None,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Real(r) => write!(f, "{}", r),
            Value::Str(s) => write!(f, "{}", s),
            Value::Bit(b) => write!(f, "{}", if *b { 1 } else { 0 }),
        }
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;

    // --- is_truthy ---

    #[test]
    fn unit_не_истинно() {
        assert!(!Value::Unit.is_truthy());
    }

    #[test]
    fn bool_true_истинно() {
        assert!(Value::Bool(true).is_truthy());
    }

    #[test]
    fn bool_false_не_истинно() {
        assert!(!Value::Bool(false).is_truthy());
    }

    #[test]
    fn int_ненулевой_истинно() {
        assert!(Value::Int(42).is_truthy());
        assert!(Value::Int(-1).is_truthy());
    }

    #[test]
    fn int_нулевой_не_истинно() {
        assert!(!Value::Int(0).is_truthy());
    }

    #[test]
    fn real_ненулевой_истинно() {
        assert!(Value::Real(3.14).is_truthy());
        assert!(Value::Real(-0.1).is_truthy());
    }

    #[test]
    fn real_нулевой_не_истинно() {
        assert!(!Value::Real(0.0).is_truthy());
    }

    #[test]
    fn str_непустой_истинно() {
        assert!(Value::Str("hello".to_string()).is_truthy());
    }

    #[test]
    fn str_пустой_не_истинно() {
        assert!(!Value::Str(String::new()).is_truthy());
    }

    #[test]
    fn bit_true_истинно() {
        assert!(Value::Bit(true).is_truthy());
    }

    #[test]
    fn bit_false_не_истинно() {
        assert!(!Value::Bit(false).is_truthy());
    }

    // --- to_f64 ---

    #[test]
    fn int_в_f64() {
        assert_eq!(Value::Int(7).to_f64(), Some(7.0_f64));
    }

    #[test]
    fn real_в_f64() {
        assert_eq!(Value::Real(2.5).to_f64(), Some(2.5_f64));
    }

    #[test]
    fn bit_true_в_f64() {
        assert_eq!(Value::Bit(true).to_f64(), Some(1.0_f64));
    }

    #[test]
    fn bit_false_в_f64() {
        assert_eq!(Value::Bit(false).to_f64(), Some(0.0_f64));
    }

    #[test]
    fn bool_в_f64() {
        assert_eq!(Value::Bool(true).to_f64(), Some(1.0_f64));
        assert_eq!(Value::Bool(false).to_f64(), Some(0.0_f64));
    }

    #[test]
    fn unit_в_f64_none() {
        assert_eq!(Value::Unit.to_f64(), None);
    }

    #[test]
    fn str_в_f64_none() {
        assert_eq!(Value::Str("abc".to_string()).to_f64(), None);
    }

    // --- to_i64 ---

    #[test]
    fn int_в_i64() {
        assert_eq!(Value::Int(-100).to_i64(), Some(-100_i64));
    }

    #[test]
    fn real_в_i64_усечение() {
        assert_eq!(Value::Real(3.9).to_i64(), Some(3_i64));
    }

    #[test]
    fn bit_true_в_i64() {
        assert_eq!(Value::Bit(true).to_i64(), Some(1_i64));
    }

    #[test]
    fn bit_false_в_i64() {
        assert_eq!(Value::Bit(false).to_i64(), Some(0_i64));
    }

    #[test]
    fn bool_в_i64() {
        assert_eq!(Value::Bool(true).to_i64(), Some(1_i64));
        assert_eq!(Value::Bool(false).to_i64(), Some(0_i64));
    }

    #[test]
    fn unit_в_i64_none() {
        assert_eq!(Value::Unit.to_i64(), None);
    }

    #[test]
    fn str_в_i64_none() {
        assert_eq!(Value::Str("xyz".to_string()).to_i64(), None);
    }

    // --- Display ---

    #[test]
    fn display_unit() {
        assert_eq!(Value::Unit.to_string(), "()");
    }

    #[test]
    fn display_bool_true() {
        assert_eq!(Value::Bool(true).to_string(), "true");
    }

    #[test]
    fn display_bool_false() {
        assert_eq!(Value::Bool(false).to_string(), "false");
    }

    #[test]
    fn display_int() {
        assert_eq!(Value::Int(42).to_string(), "42");
        assert_eq!(Value::Int(-5).to_string(), "-5");
    }

    #[test]
    fn display_real() {
        assert_eq!(Value::Real(1.5).to_string(), "1.5");
    }

    #[test]
    fn display_str() {
        assert_eq!(Value::Str("hello".to_string()).to_string(), "hello");
    }

    #[test]
    fn display_bit_true() {
        assert_eq!(Value::Bit(true).to_string(), "1");
    }

    #[test]
    fn display_bit_false() {
        assert_eq!(Value::Bit(false).to_string(), "0");
    }
}

//! Ошибки вычисления - структурированные и **без позиции** в исходном тексте.
//!
//! Ядро [`crate::eval`] позиций не знает: оно оперирует значениями, а не узлами АСД.
//! Позицию добавляет адаптер, у которого она есть, - через
//! [`EvalError::to_diagnostic`]. Такое разделение даёт диагностику с позицией в
//! исходнике вместо `Location::Builtin`.

use takt_lang::diagnostics::{Diagnostic, Location};

use crate::eval::value::Value;

/// Причина, по которой выражение не удалось вычислить.
///
/// Каждый вариант соответствует строке таблицы семантики `S` из анализа либо ошибке
/// типов. Позиции нет - её добавляет адаптер.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum EvalError {
    /// S3: деление или взятие остатка на ноль (в C - UB, не воспроизводим).
    DivisionByZero,
    /// S4a: сдвиг на отрицательное число или на >= 64 бит (в C - UB).
    ShiftOutOfRange { by: i128 },
    /// S2: значение не помещается в знаковый тип назначения (в C - UB).
    SignedOverflow { value: i128, bits: u8 },
    /// Переполнение внутреннего 64-битного представления.
    ArithmeticOverflow { op: &'static str },
    /// Операция не определена для операндов таких типов (в т.ч.
    TypeMismatch {
        op: &'static str,
        lhs: &'static str,
        rhs: Option<&'static str>,
    },
    /// Значение нельзя привести к типу назначения.
    NotCoercible { value: &'static str, ty: String },
    /// Тип не поддерживается симулятором.
    UnsupportedType { ty: String },
    /// Доступ к полю (`.имя`) у значения, не являющегося структурой.
    FieldOfNonStruct { value: &'static str },
    /// Число полей инициализатора `{...}` не совпадает с объявлением структуры.
    StructArity {
        name: String,
        expected: usize,
        got: usize,
    },
    /// У структуры нет поля с таким именем (чтение/запись/приведение).
    UnknownField { name: String, field: String },
    /// Присваивание структуры другого типа (`q := p` при разных `struct`).
    StructTypeMismatch { expected: String, got: String },
    /// Обращение к структуре по номеру бита (`p.0`) - бита у структуры нет.
    BitIndexOfStruct { name: String },
    /// Доступ к биту (`.N`) у значения, не являющегося целым/логическим.
    BitOfNonInteger { value: &'static str },
    /// Номер бита вне разрядов значения.
    ///
    /// `width` - сколько разрядов у значения **на самом деле**: 64 у целого (ширина
    /// носителя), `1` у логического, `слов x 64` у бит-вектора `[bit;N > 64]`.
    BitIndexOutOfRange { bit: i128, width: usize },
    /// Индексная запись (`x[i] := ...`) в значение, не являющееся массивом.
    IndexOfNonArray { value: &'static str },
    /// Индекс записи вне границ массива (`data[i] := ...`, `i >= длины`).
    ArrayIndexOutOfBounds { index: usize, len: usize },
}

impl EvalError {
    /// Текст диагностики на русском языке ().
    pub(crate) fn message(&self) -> String {
        match self {
            EvalError::DivisionByZero => "деление на ноль".to_string(),
            EvalError::ShiftOutOfRange { by } => {
                format!("сдвиг на {by} бит: величина сдвига должна быть в диапазоне 0..64")
            }
            EvalError::SignedOverflow { value, bits } => {
                format!("значение {value} не помещается в знаковый {bits}-битный тип")
            }
            EvalError::ArithmeticOverflow { op } => {
                format!("переполнение при вычислении операции '{op}'")
            }
            EvalError::TypeMismatch { op, lhs, rhs } => match rhs {
                Some(rhs) => format!("операция '{op}' не определена для операндов {lhs} и {rhs}"),
                None => format!("операция '{op}' не определена для операнда {lhs}"),
            },
            EvalError::NotCoercible { value, ty } => {
                format!("значение {value} нельзя привести к типу {ty}")
            }
            EvalError::UnsupportedType { ty } => {
                format!("тип {ty} не поддерживается симулятором")
            }
            EvalError::FieldOfNonStruct { value } => {
                format!("доступ к полю возможен только у структуры, а не у значения {value}")
            }
            EvalError::StructArity {
                name,
                expected,
                got,
            } => format!(
                "инициализатор структуры '{name}' содержит {got} полей, а объявлено {expected}"
            ),
            EvalError::UnknownField { name, field } => {
                format!("структура '{name}' не имеет поля '{field}'")
            }
            EvalError::StructTypeMismatch { expected, got } => {
                format!("ожидалась структура '{expected}', получена '{got}'")
            }
            EvalError::BitIndexOfStruct { name } => {
                format!("к структуре '{name}' нельзя обратиться по номеру бита")
            }
            EvalError::BitOfNonInteger { value } => {
                format!("доступ к биту возможен только у целого значения, а не у {value}")
            }
            EvalError::BitIndexOutOfRange { bit, width } => {
                // Граница печатается включительно: "0..128" на 128-разрядном значении
                // читается как "128 допустим", а он - первый недопустимый. У
                // логического разряд один, и множественное число там звучало бы
                // ошибкой.
                if *width == 1 {
                    format!("номер бита {bit} недопустим: у логического значения один разряд — 0")
                } else {
                    format!(
                        "номер бита {bit} вне разрядов значения: доступны 0..{}",
                        width - 1
                    )
                }
            }
            EvalError::IndexOfNonArray { value } => {
                format!("индексная запись возможна только в массив, а не в значение {value}")
            }
            EvalError::ArrayIndexOutOfBounds { index, len } => {
                format!("индекс {index} вне границ массива (длина {len})")
            }
        }
    }

    /// Код диагностики (пространство `SIM-...` - вычисление в симуляторе).
    pub(crate) fn code(&self) -> &'static str {
        match self {
            EvalError::DivisionByZero => "SIM-001",
            EvalError::ShiftOutOfRange { .. } => "SIM-002",
            EvalError::SignedOverflow { .. } => "SIM-003",
            EvalError::ArithmeticOverflow { .. } => "SIM-004",
            EvalError::TypeMismatch { .. } => "SIM-005",
            EvalError::NotCoercible { .. } => "SIM-006",
            EvalError::UnsupportedType { .. } => "SIM-007",
            EvalError::FieldOfNonStruct { .. } => "SIM-012",
            EvalError::StructArity { .. } => "SIM-026",
            EvalError::UnknownField { .. } => "SIM-027",
            EvalError::StructTypeMismatch { .. } => "SIM-028",
            EvalError::BitIndexOfStruct { .. } => "SIM-029",
            // SIM-011 - прежний код доступа к биту (адаптер), теперь в ядре.
            EvalError::BitOfNonInteger { .. } | EvalError::BitIndexOutOfRange { .. } => "SIM-011",
            // SIM-010 - тот же код, что у ошибок чтения массива (`expression.rs`): "не
            // массив" и "вне границ" едины для чтения и записи.
            EvalError::IndexOfNonArray { .. } | EvalError::ArrayIndexOutOfBounds { .. } => {
                "SIM-010"
            }
        }
    }

    /// Превращает ошибку в диагностику, привязанную к позиции в исходном тексте.
    ///
    /// Позицию передаёт адаптер - единственный, кто её знает.
    pub(crate) fn to_diagnostic(&self, loc: Location) -> Diagnostic {
        Diagnostic::error(loc, self.message()).with_code(self.code())
    }
}

/// Имя типа значения для текстов диагностик.
pub(crate) fn value_kind(value: &Value) -> &'static str {
    match value {
        Value::Number(_) => "целое",
        Value::Real(_) => "вещественное",
        Value::Boolean(_) => "логическое",
        Value::Array(_) => "массив",
        Value::Fixed { .. } => "fixed-point",
        Value::Struct { .. } => "структура",
        Value::Duration(_) => "длительность",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn division_by_zero_has_message_and_code() {
        let err = EvalError::DivisionByZero;
        assert_eq!(err.message(), "деление на ноль");
        assert_eq!(err.code(), "SIM-001");
    }

    #[test]
    fn shift_out_of_range_mentions_amount() {
        let err = EvalError::ShiftOutOfRange { by: 64 };
        assert!(err.message().contains("64"));
        assert_eq!(err.code(), "SIM-002");
    }

    #[test]
    fn signed_overflow_mentions_value_and_bits() {
        let err = EvalError::SignedOverflow {
            value: 128,
            bits: 8,
        };
        assert!(err.message().contains("128"));
        assert!(err.message().contains('8'));
        assert_eq!(err.code(), "SIM-003");
    }

    #[test]
    fn type_mismatch_binary_mentions_both_operands() {
        let err = EvalError::TypeMismatch {
            op: "+",
            lhs: "логическое",
            rhs: Some("целое"),
        };
        assert!(err.message().contains("логическое"));
        assert!(err.message().contains("целое"));
    }

    #[test]
    fn type_mismatch_unary_mentions_single_operand() {
        let err = EvalError::TypeMismatch {
            op: "~",
            lhs: "вещественное",
            rhs: None,
        };
        assert!(err.message().contains("вещественное"));
        assert!(!err.message().contains(" и "));
    }

    #[test]
    fn to_diagnostic_carries_location_and_code() {
        // Позицию даёт адаптер - ядро её не знает.
        let loc = Location::Source(0, 10, 20);
        let diag = EvalError::DivisionByZero.to_diagnostic(loc);
        assert_eq!(diag.loc, loc);
        assert_eq!(diag.code.as_deref(), Some("SIM-001"));
        assert!(diag.message.contains("деление на ноль"));
    }

    #[test]
    fn value_kind_covers_all_variants() {
        assert_eq!(value_kind(&Value::Number(1)), "целое");
        assert_eq!(value_kind(&Value::Real(1.0)), "вещественное");
        assert_eq!(value_kind(&Value::Boolean(true)), "логическое");
        assert_eq!(value_kind(&Value::Array(vec![])), "массив");
    }
}

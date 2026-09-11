//! Ошибки вычисления - структурированные и **без позиции** в исходном тексте.
//!
//! Ядро [`crate::eval`] позиций не знает: оно оперирует значениями, а не узлами АСД.
//! Позицию добавляет адаптер, у которого она есть, - через
//! [`EvalError::to_diagnostic`]. Такое разделение даёт диагностику с позицией в
//! исходнике вместо `Location::Builtin`.
//!
//! Вид значения ошибка хранит ключом каталога, а не текстом: текст строится при
//! печати, на языке прогона.

use takt_lang::diagnostics::lang::{Key, keys};
use takt_lang::diagnostics::{Diagnostic, Location};
use takt_lang::msg;

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
    /// Операция не определена для операндов таких типов.
    TypeMismatch {
        op: &'static str,
        lhs: Key,
        rhs: Option<Key>,
    },
    /// Значение в позиции логического условия не является логическим.
    NotACondition { value: Key },
    /// Значение нельзя привести к типу назначения.
    NotCoercible { value: Key, ty: String },
    /// Тип не поддерживается симулятором.
    UnsupportedType { ty: String },
    /// Доступ к полю (`.имя`) у значения, не являющегося структурой.
    FieldOfNonStruct { value: Key },
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
    BitOfNonInteger { value: Key },
    /// Номер бита вне разрядов значения.
    ///
    /// `width` - сколько разрядов у значения **на самом деле**: 64 у целого (ширина
    /// носителя), `1` у логического, `слов x 64` у бит-вектора `[bit;N > 64]`.
    BitIndexOutOfRange { bit: i128, width: usize },
    /// Индексная запись (`x[i] := ...`) в значение, не являющееся массивом.
    IndexOfNonArray { value: Key },
    /// Индекс записи вне границ массива (`data[i] := ...`, `i >= длины`).
    ArrayIndexOutOfBounds { index: usize, len: usize },
}

impl EvalError {
    /// Текст диагностики на языке прогона.
    pub(crate) fn message(&self) -> String {
        match self {
            EvalError::DivisionByZero => msg!(keys::SIM_001_DIVISION_BY_ZERO),
            EvalError::ShiftOutOfRange { by } => msg!(keys::SIM_002_SHIFT_OUT_OF_RANGE, by = by),
            EvalError::SignedOverflow { value, bits } => {
                msg!(keys::SIM_003_SIGNED_OVERFLOW, value = value, bits = bits)
            }
            EvalError::ArithmeticOverflow { op } => {
                msg!(keys::SIM_004_ARITHMETIC_OVERFLOW, op = op)
            }
            EvalError::TypeMismatch { op, lhs, rhs } => match rhs {
                Some(rhs) => msg!(
                    keys::SIM_005_TYPE_MISMATCH_BINARY,
                    op = op,
                    lhs = msg!(*lhs),
                    rhs = msg!(*rhs)
                ),
                None => msg!(keys::SIM_005_TYPE_MISMATCH_UNARY, op = op, lhs = msg!(*lhs)),
            },
            EvalError::NotACondition { value } => msg!(
                keys::SIM_005_TYPE_MISMATCH_UNARY,
                op = msg!(keys::SIM_OP_LOGICAL_CONDITION),
                lhs = msg!(*value)
            ),
            EvalError::NotCoercible { value, ty } => {
                msg!(keys::SIM_006_NOT_COERCIBLE, value = msg!(*value), ty = ty)
            }
            EvalError::UnsupportedType { ty } => msg!(keys::SIM_007_UNSUPPORTED_TYPE, ty = ty),
            EvalError::FieldOfNonStruct { value } => {
                msg!(keys::SIM_012_FIELD_OF_NON_STRUCT, value = msg!(*value))
            }
            EvalError::StructArity {
                name,
                expected,
                got,
            } => msg!(
                keys::SIM_026_STRUCT_ARITY,
                name = name,
                got = got,
                expected = expected
            ),
            EvalError::UnknownField { name, field } => {
                msg!(keys::SIM_027_UNKNOWN_FIELD, name = name, field = field)
            }
            EvalError::StructTypeMismatch { expected, got } => {
                msg!(
                    keys::SIM_028_STRUCT_TYPE_MISMATCH,
                    expected = expected,
                    got = got
                )
            }
            EvalError::BitIndexOfStruct { name } => {
                msg!(keys::SIM_029_BIT_INDEX_OF_STRUCT, name = name)
            }
            EvalError::BitOfNonInteger { value } => {
                msg!(keys::SIM_011_BIT_OF_NON_INTEGER, value = msg!(*value))
            }
            EvalError::BitIndexOutOfRange { bit, width } => {
                // Граница печатается включительно: "0..128" на 128-разрядном значении
                // читается как "128 допустим", а он - первый недопустимый. У
                // логического разряд один, и множественное число там звучало бы
                // ошибкой.
                if *width == 1 {
                    msg!(keys::SIM_011_BIT_OF_BOOL, bit = bit)
                } else {
                    msg!(keys::SIM_011_BIT_OUT_OF_RANGE, bit = bit, last = width - 1)
                }
            }
            EvalError::IndexOfNonArray { value } => {
                msg!(keys::SIM_010_INDEX_OF_NON_ARRAY, value = msg!(*value))
            }
            EvalError::ArrayIndexOutOfBounds { index, len } => {
                msg!(keys::SIM_010_INDEX_OUT_OF_BOUNDS, index = index, len = len)
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
            EvalError::TypeMismatch { .. } | EvalError::NotACondition { .. } => "SIM-005",
            EvalError::NotCoercible { .. } => "SIM-006",
            EvalError::UnsupportedType { .. } => "SIM-007",
            EvalError::FieldOfNonStruct { .. } => "SIM-012",
            EvalError::StructArity { .. } => "SIM-026",
            EvalError::UnknownField { .. } => "SIM-027",
            EvalError::StructTypeMismatch { .. } => "SIM-028",
            EvalError::BitIndexOfStruct { .. } => "SIM-029",
            // SIM-011 - код доступа к биту и у адаптера, и у ядра.
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

/// Вид значения для текстов диагностик - ключом каталога.
pub(crate) fn value_kind(value: &Value) -> Key {
    match value {
        Value::Number(_) => keys::SIM_KIND_INTEGER,
        Value::Real(_) => keys::SIM_KIND_REAL,
        Value::Boolean(_) => keys::SIM_KIND_BOOLEAN,
        Value::Array(_) => keys::SIM_KIND_ARRAY,
        Value::Fixed { .. } => keys::SIM_KIND_FIXED,
        Value::Struct { .. } => keys::SIM_KIND_STRUCT,
        Value::Duration(_) => keys::SIM_KIND_DURATION,
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
            lhs: keys::SIM_KIND_BOOLEAN,
            rhs: Some(keys::SIM_KIND_INTEGER),
        };
        assert!(err.message().contains("логическое"));
        assert!(err.message().contains("целое"));
    }

    #[test]
    fn type_mismatch_unary_mentions_single_operand() {
        let err = EvalError::TypeMismatch {
            op: "~",
            lhs: keys::SIM_KIND_REAL,
            rhs: None,
        };
        assert!(err.message().contains("вещественное"));
        assert!(!err.message().contains(" и "));
    }

    /// Значение вне логического условия печатается прежней формой `SIM-005`.
    #[test]
    fn not_a_condition_reads_as_type_mismatch() {
        let err = EvalError::NotACondition {
            value: keys::SIM_KIND_ARRAY,
        };
        assert_eq!(
            err.message(),
            "операция 'логическое условие' не определена для операнда массив"
        );
        assert_eq!(err.code(), "SIM-005");
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
        assert_eq!(msg!(value_kind(&Value::Number(1))), "целое");
        assert_eq!(msg!(value_kind(&Value::Real(1.0))), "вещественное");
        assert_eq!(msg!(value_kind(&Value::Boolean(true))), "логическое");
        assert_eq!(msg!(value_kind(&Value::Array(vec![]))), "массив");
    }
}

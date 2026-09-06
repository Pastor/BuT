//! Тесты модуля `validate` (перенесены из `validate.rs`).

use super::types::{MAX_ARRAY_SIZE, check_type_array_size};
use crate::diagnostics::Location;
use crate::semantic::type_node::TypeNode;

/// Ce15: допустимый размер массива 8 - ошибок нет.
#[test]
fn array_size_8_is_ok() {
    let ty = TypeNode::Array(8, Box::new(TypeNode::Bit));
    assert!(
        check_type_array_size(&ty, Location::Implicit).is_ok(),
        "массив размером 8 должен быть допустим"
    );
}

/// Ce15: размер равный MAX_ARRAY_SIZE - допустим (граничное значение).
#[test]
fn array_size_max_is_ok() {
    let ty = TypeNode::Array(MAX_ARRAY_SIZE, Box::new(TypeNode::Bit));
    assert!(
        check_type_array_size(&ty, Location::Implicit).is_ok(),
        "массив размером MAX_ARRAY_SIZE должен быть допустим"
    );
}

/// Ce15: размер MAX_ARRAY_SIZE + 1 - ошибка.
#[test]
fn array_size_exceeding_max_is_error() {
    let ty = TypeNode::Array(MAX_ARRAY_SIZE + 1, Box::new(TypeNode::Bit));
    let result = check_type_array_size(&ty, Location::Implicit);
    assert!(
        result.is_err(),
        "массив размером MAX_ARRAY_SIZE+1 должен давать ошибку Ce15"
    );
    let err = result.unwrap_err();
    assert_eq!(
        err.code.as_deref(),
        Some("SE-038"),
        "код ошибки Ce15 должен быть SE-038: {:?}",
        err.code
    );
}

/// Ce15: максимальный u16 (65535) - ошибка, превышает MAX_ARRAY_SIZE.
#[test]
fn array_size_u16_max_is_error() {
    let ty = TypeNode::Array(u16::MAX, Box::new(TypeNode::Bit));
    let result = check_type_array_size(&ty, Location::Implicit);
    assert!(
        result.is_err(),
        "массив размером u16::MAX должен давать ошибку Ce15"
    );
}

/// Ce15: вложенный массив с превышением размера - также ошибка.
#[test]
fn nested_array_size_exceeding_max_is_error() {
    // Внешний массив допустим, но вложенный нет
    let inner = TypeNode::Array(MAX_ARRAY_SIZE + 1, Box::new(TypeNode::Bit));
    let outer = TypeNode::Array(2, Box::new(inner));
    let result = check_type_array_size(&outer, Location::Implicit);
    assert!(
        result.is_err(),
        "вложенный массив с превышением размера должен давать ошибку Ce15"
    );
}

/// Ce15: переменная с допустимым размером массива не даёт ошибку через validate_model.
#[test]
fn validate_model_accepts_small_array() {
    let (ast, _) = crate::parse("var x: [bit;8] := 0; start S;", 0).unwrap();
    let result = crate::semantic::tree::construct_model(&ast, None, &[]);
    assert!(
        result.is_ok(),
        "массив [bit;8] должен проходить валидацию: {:?}",
        result
    );
}

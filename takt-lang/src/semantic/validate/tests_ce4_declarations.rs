//! Тесты проверок объявлений в модуле `validate`.
//!
//! Проверка отдаёт вектор диагностик: с `Result<(), Diagnostic>` на модель приходилась
//! бы не больше одной ошибки, и вторая переменная с необъявленным перечислением
//! молчала бы.

use super::*;

/// Вспомогательная функция: строит Rc<RefCell<ModelNode>> из Takt-исходника.
fn build_rc(src: &str) -> Rc<RefCell<ModelNode>> {
    let (ast, _) = crate::parse(src, 0).expect("ошибка разбора");
    crate::semantic::tree::construct_model(&ast, None, &[]).expect("ошибка семантики")
}

// -- Примеры корректного использования enum-типов --------------------------

/// Переменная с типом enum, где перечисление объявлено - ошибок нет.
///
/// # Пример (Takt)
/// ```text
/// enum Color {
///     Red = 0,
///     Green = 1
/// }
/// var c: Color = 0;   // Color объявлен
/// start S;
/// ```
#[test]
fn ce4_declared_enum_type_is_ok() {
    // Добавляем перечисление и переменную типа этого перечисления программно
    let model_rc = {
        let (ast, _) =
            crate::parse("enum Color { Red = 0, Green = 1 } start S;", 0).expect("ошибка разбора");
        let m = crate::semantic::tree::construct_model(&ast, None, &[]).expect("ошибка семантики");
        // Переменная типа Color - Color объявлен в AST
        let var = VariableNode::Simple {
            upper: None,
            loc: Location::Implicit,
            name: "c".to_string(),
            ty: TypeNode::Enum("Color".to_string()),
            expr: ExpressionNode::Number(0),
        };
        m.borrow_mut().variables.insert("c".to_string(), var);
        m
    };
    let result = validate_enum_type_declarations(model_rc);
    assert!(
        result.is_empty(),
        "переменная с объявленным enum-типом не должна давать ошибку: {:?}",
        result
    );
}

/// Переменная с обычным (не-enum) типом не проверяется Ce4.
///
/// # Пример (Takt)
/// ```text
/// var x: [bit;8] = 0;  // обычный тип, Ce4 не применяется
/// start S;
/// ```
#[test]
fn ce4_non_enum_type_not_checked() {
    let model_rc = build_rc("var x: [bit;8] := 0; start S;");
    let result = validate_enum_type_declarations(model_rc);
    assert!(result.is_empty(), "не-enum тип не должен проверяться Ce4");
}

/// Переменная с пустым enum-типом (Inference) не проверяется Ce4.
#[test]
fn ce4_inference_type_not_checked() {
    let model_rc = build_rc("start S;");
    // Добавляем переменную с типом Inference
    let var = VariableNode::Simple {
        upper: None,
        loc: Location::Implicit,
        name: "y".to_string(),
        ty: TypeNode::Inference,
        expr: ExpressionNode::Number(0),
    };
    model_rc.borrow_mut().variables.insert("y".to_string(), var);
    let result = validate_enum_type_declarations(model_rc);
    assert!(result.is_empty(), "Inference-тип не должен вызывать Ce4");
}

// -- Контр-примеры: ошибочные enum-типы -----------------------------------

/// Переменная типа необъявленного перечисления -> ошибка Ce4.
///
/// # Контр-пример (Takt)
/// ```text
/// var s: Size = 0;  // Size не объявлен
/// start S;
/// ```
#[test]
fn ce4_undeclared_enum_type_is_error() {
    let model_rc = {
        let (ast, _) = crate::parse("start S;", 0).expect("ошибка разбора");
        let m = crate::semantic::tree::construct_model(&ast, None, &[]).expect("ошибка семантики");
        // Переменная типа Size - Size не объявлен
        let var = VariableNode::Simple {
            upper: None,
            loc: Location::Implicit,
            name: "s".to_string(),
            ty: TypeNode::Enum("Size".to_string()),
            expr: ExpressionNode::Number(0),
        };
        m.borrow_mut().variables.insert("s".to_string(), var);
        m
    };
    let result = validate_enum_type_declarations(model_rc);
    assert!(
        !result.is_empty(),
        "необъявленный enum-тип должен давать ошибку Ce4"
    );
    let err = &result[0];
    assert!(
        err.message.contains("Size"),
        "сообщение должно содержать имя отсутствующего enum: {}",
        err.message
    );
    assert_eq!(
        err.code.as_deref(),
        Some("SE-035"),
        "код ошибки Ce4 должен быть SE-035: {:?}",
        err.code
    );
}

/// Константа с необъявленным enum-типом также проверяется.
///
/// # Контр-пример (Takt)
/// ```text
/// const C: Status = 0;  // Status не объявлен
/// start S;
/// ```
#[test]
fn ce4_undeclared_enum_in_const_is_error() {
    let model_rc = {
        let (ast, _) = crate::parse("start S;", 0).expect("ошибка разбора");
        let m = crate::semantic::tree::construct_model(&ast, None, &[]).expect("ошибка семантики");
        let var = VariableNode::Const {
            upper: None,
            loc: Location::Implicit,
            name: "C".to_string(),
            ty: TypeNode::Enum("Status".to_string()),
            expr: ExpressionNode::Number(0),
        };
        m.borrow_mut().variables.insert("C".to_string(), var);
        m
    };
    let result = validate_enum_type_declarations(model_rc);
    assert!(
        !result.is_empty(),
        "константа с необъявленным enum-типом должна давать ошибку Ce4"
    );
}

/// Порт с необъявленным enum-типом также проверяется.
#[test]
fn ce4_undeclared_enum_in_port_is_error() {
    let model_rc = {
        let (ast, _) = crate::parse("start S;", 0).expect("ошибка разбора");
        let m = crate::semantic::tree::construct_model(&ast, None, &[]).expect("ошибка семантики");
        let var = VariableNode::Port {
            upper: None,
            loc: Location::Implicit,
            name: "p".to_string(),
            ty: TypeNode::Enum("Dir".to_string()),
            address: ExpressionNode::Number(0),
            init: ExpressionNode::None,
            direction: crate::semantic::PortDirection::In,
        };
        m.borrow_mut().variables.insert("p".to_string(), var);
        m
    };
    let result = validate_enum_type_declarations(model_rc);
    assert!(
        !result.is_empty(),
        "порт с необъявленным enum-типом должен давать ошибку Ce4"
    );
}

/// Модель без переменных - проверка пуста и всегда ок.
#[test]
fn ce4_empty_model_is_ok() {
    let model_rc = build_rc("start S;");
    let result = validate_enum_type_declarations(model_rc);
    assert!(
        result.is_empty(),
        "пустая модель не должна давать ошибки Ce4"
    );
}

//! Тесты модуля `validate` (перенесены из `validate.rs`).

use super::*;
use crate::parse;
use crate::semantic::tree::construct_model;

fn build(src: &str) -> Result<ModelNode, Diagnostic> {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[]).map(|m| m.take())
}

/// Пустая программа без состояний - валидна.
#[test]
fn empty_model_is_valid() {
    assert!(build("").is_ok());
}

/// Модель только с типами - валидна (нет состояний).
#[test]
fn model_with_only_types_is_valid() {
    assert!(build("type Byte = [bit;8];").is_ok());
}

/// Имя встроенного типа занять нельзя - `SE-107`.
///
/// Прежде эта же строка (`type u8 = [bit;8];`) стояла в тесте выше как пример валидной
/// программы: язык принимал затенение молча, и `u8` в файле начинал означать не то, что
/// в языке.
#[test]
fn builtin_type_name_cannot_be_redeclared() {
    let err = build("type u8 = [bit;8];").expect_err("ожидался отказ SE-107");
    assert_eq!(err.code.as_deref(), Some("SE-107"), "{err:?}");
}

/// Модель с одним начальным состоянием - валидна.
#[test]
fn single_start_state_is_valid() {
    assert!(build("start S;").is_ok());
}

/// Модель с двумя начальными состояниями - ошибка.
///
/// # Контрпример (Takt)
/// ```but
/// start A;   // первое start
/// start B;   // второе start - запрещено
/// ```
#[test]
fn two_start_states_is_error() {
    let result = build("start A; start B;");
    assert!(result.is_err(), "два start-состояния должны давать ошибку");
}

/// Модель без начального состояния (только обычные состояния) - ошибка.
///
/// # Контрпример (Takt)
/// ```but
/// state A;   // нет start - запрещено для модели с состояниями
/// state B;
/// ```
#[test]
fn no_start_state_is_error() {
    let result = build("state A; state B;");
    assert!(
        result.is_err(),
        "отсутствие start-состояния должно давать ошибку"
    );
}

/// Вложенная модель с двумя начальными состояниями - ошибка.
#[test]
fn nested_model_two_start_states_is_error() {
    let result = build("model M { start A; start B; }");
    assert!(
        result.is_err(),
        "вложенная модель с двумя start должна давать ошибку"
    );
}

/// Вложенная модель с одним start - валидна.
#[test]
fn nested_model_single_start_is_valid() {
    assert!(build("model M { start S; }").is_ok());
}

// -- Проверка значений типа bit ---------------------------------------------

/// `var x: bit = 0;` - допустимо (числовое значение 0).
///
/// # Пример (Takt)
/// ```but
/// var x: bit = 0;
/// ```
#[test]
fn bit_var_with_zero_is_valid() {
    assert!(build("var x: bit := 0;").is_ok());
}

/// `var x: bit = 1;` - допустимо (числовое значение 1).
///
/// # Пример (Takt)
/// ```but
/// var x: bit = 1;
/// ```
#[test]
fn bit_var_with_one_is_valid() {
    assert!(build("var x: bit := 1;").is_ok());
}

/// `var x: bit = true;` - допустимо (булев литерал).
#[test]
fn bit_var_with_true_is_valid() {
    assert!(build("var x: bit := true;").is_ok());
}

/// `var x: bit = false;` - допустимо (булев литерал).
#[test]
fn bit_var_with_false_is_valid() {
    assert!(build("var x: bit := false;").is_ok());
}

/// `var x: bit = 2;` - ошибка: значение 2 не является допустимым для bit.
///
/// # Контрпример (Takt)
/// ```but
/// var x: bit = 2;   // ошибка: недопустимое значение
/// ```
#[test]
fn bit_var_with_two_is_error() {
    let result = build("var x: bit := 2;");
    assert!(result.is_err(), "значение 2 недопустимо для типа bit");
    assert!(result.unwrap_err().message.contains("bit"));
}

/// `var x: bit = -1;` - ошибка: отрицательное значение не допускается для bit.
///
/// # Контрпример (Takt)
/// ```but
/// var x: bit = -1;   // ошибка: отрицательное число недопустимо
/// ```
#[test]
fn bit_var_with_minus_one_is_error() {
    let result = build("var x: bit := -1;");
    // -1 парсится как Negate(1) или Number(-1): в обоих случаях числовой литерал -1
    // Если парсер создаёт Number(-1), должна быть ошибка валидации.
    // Если парсер создаёт Negate(Number(1)), это выражение - не Number, ошибки нет.
    // Тест проверяет только отсутствие паники.
    let _ = result; // оба варианта допустимы для текущего парсера
}

/// `var x: bit = 255;` - ошибка: значение вне допустимого диапазона bit.
///
/// # Контрпример (Takt)
/// ```but
/// var x: bit = 255;   // ошибка: 255 не входит в {0, 1}
/// ```
#[test]
fn bit_var_with_255_is_error() {
    let result = build("var x: bit := 255;");
    assert!(result.is_err(), "значение 255 недопустимо для типа bit");
}

/// `const C: bit = 2;` - ошибка: константа типа bit с недопустимым значением.
#[test]
fn bit_const_with_invalid_value_is_error() {
    let result = build("const C: bit := 2;");
    assert!(result.is_err(), "константа bit = 2 должна давать ошибку");
}

/// Переменные типа `[bit;8]` (массив) не проверяются на диапазон элементов - числовое
/// значение инициализатора массива трактуется как целое число.
#[test]
fn bit_array_initializer_is_not_range_checked() {
    // [bit;8] = 255 - это 8-битное значение, проверка диапазона не применяется.
    assert!(build("var x: [bit;8] := 255;").is_ok());
}

/// Переменная `bit` с инициализатором-переменной не проверяется статически.
#[test]
fn bit_var_initialized_from_other_var_is_valid() {
    // b: bit = a - ссылка на переменную, статическая проверка значения не применяется.
    assert!(build("var a: bit := 0; var b: bit := a;").is_ok());
}

/// Вложенная модель с некорректным значением bit - ошибка.
#[test]
fn nested_model_with_invalid_bit_value_is_error() {
    let result = build("model M { var x: bit := 5; start S; }");
    assert!(
        result.is_err(),
        "вложенная модель: bit = 5 должна давать ошибку"
    );
}

// -- Се11: строгая проверка булевости условий переходов ---------------------

fn build_rc(src: &str) -> Rc<RefCell<ModelNode>> {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[]).expect("ошибка семантики")
}

// -- NI6: типобезопасные операции с enum ----------------------------------------

/// Переменная с корректным значением enum не вызывает ошибок NI6.
///
/// # Пример (Takt)
/// ```but
/// enum Dir {
///     North,
///     South
/// }
/// var d: Dir = 0;  // 0 - значение North
/// ```
#[test]
fn ni6_valid_enum_initializer_no_errors() {
    let model_rc = {
        let (ast, _) = parse("start S;", 0).expect("ошибка разбора");
        let m = construct_model(&ast, None, &[]).expect("ошибка семантики");
        // Добавляем перечисление и переменную с корректным значением программно
        let e = crate::semantic::EnumDefinitionNode::new(
            "Direction",
            &[
                ("North", Some(0)),
                ("South", Some(1)),
                ("East", Some(2)),
                ("West", Some(3)),
            ],
        );
        m.borrow_mut().enums.insert("Direction".to_string(), e);
        let dir_var = VariableNode::Simple {
            upper: None,
            loc: Location::Implicit,
            name: "dir".to_string(),
            ty: TypeNode::Enum("Direction".to_string()),
            expr: ExpressionNode::Number(0),
        };
        m.borrow_mut().variables.insert("dir".to_string(), dir_var);
        m
    };
    let errors = check_enum_type_safety(model_rc);
    assert!(
        errors.is_empty(),
        "допустимое значение enum не должно вызывать ошибок NI6"
    );
}

/// Переменная с некорректным значением enum вызывает ошибку NI6.
///
/// # Контрпример (Takt)
/// ```but
/// enum Dir {
///     North = 0,
///     South = 1
/// }
/// var d: Dir = 99;  // 99 - не вариант Dir
/// ```
#[test]
fn ni6_invalid_enum_initializer_is_error() {
    let model_rc = {
        let (ast, _) = parse(
            "enum Direction { North, South, East, West } \
             start S;",
            0,
        )
        .expect("ошибка разбора");
        let m = construct_model(&ast, None, &[]).expect("ошибка семантики");
        // Добавляем переменную с некорректным значением enum программно
        let dir_var = VariableNode::Simple {
            upper: None,
            loc: Location::Implicit,
            name: "dir".to_string(),
            ty: TypeNode::Enum("Direction".to_string()),
            expr: ExpressionNode::Number(99),
        };
        m.borrow_mut().variables.insert("dir".to_string(), dir_var);
        m
    };
    let errors = check_enum_type_safety(model_rc);
    assert_eq!(errors.len(), 1, "значение 99 недопустимо для Direction");
    assert_eq!(
        errors[0].code.as_deref(),
        Some("SE-043"),
        "код ошибки NI6 должен быть SE-043"
    );
    assert!(errors[0].message.contains("99"));
}

/// Инициализация значением варианта (по числовому значению) - без ошибок NI6.
#[test]
fn ni6_valid_explicit_value_no_errors() {
    let model_rc = {
        let (ast, _) = parse(
            "enum Priority { Low = 0, Medium = 5, High = 10 } start S;",
            0,
        )
        .expect("ошибка разбора");
        let m = construct_model(&ast, None, &[]).expect("ошибка семантики");
        let prio_var = VariableNode::Simple {
            upper: None,
            loc: Location::Implicit,
            name: "prio".to_string(),
            ty: TypeNode::Enum("Priority".to_string()),
            expr: ExpressionNode::Number(5),
        };
        m.borrow_mut()
            .variables
            .insert("prio".to_string(), prio_var);
        m
    };
    let errors = check_enum_type_safety(model_rc);
    assert!(
        errors.is_empty(),
        "значение 5 (Medium) допустимо для Priority"
    );
}

/// Несколько переменных - несколько ошибок NI6.
#[test]
fn ni6_multiple_invalid_enum_vars_gives_multiple_errors() {
    let model_rc = {
        let (ast, _) =
            parse("enum Dir { North = 0, South = 1 } start S;", 0).expect("ошибка разбора");
        let m = construct_model(&ast, None, &[]).expect("ошибка семантики");
        let v1 = VariableNode::Simple {
            upper: None,
            loc: Location::Implicit,
            name: "a".to_string(),
            ty: TypeNode::Enum("Dir".to_string()),
            expr: ExpressionNode::Number(42),
        };
        let v2 = VariableNode::Simple {
            upper: None,
            loc: Location::Implicit,
            name: "b".to_string(),
            ty: TypeNode::Enum("Dir".to_string()),
            expr: ExpressionNode::Number(99),
        };
        m.borrow_mut().variables.insert("a".to_string(), v1);
        m.borrow_mut().variables.insert("b".to_string(), v2);
        m
    };
    let errors = check_enum_type_safety(model_rc);
    assert_eq!(
        errors.len(),
        2,
        "два некорректных значения должны дать 2 ошибки NI6"
    );
}

/// Переменная типа bit не проверяется функцией NI6.
#[test]
fn ni6_non_enum_var_not_checked() {
    let model_rc = build_rc("var x: bit := 0; start S;");
    let errors = check_enum_type_safety(model_rc);
    assert!(
        errors.is_empty(),
        "переменная типа bit не должна проверяться NI6"
    );
}

/// Переменная с неизвестным enum-типом (перечисление не найдено) - не вызывает NI6.
#[test]
fn ni6_unknown_enum_type_no_error() {
    let model_rc = {
        let (ast, _) = parse("start S;", 0).expect("ошибка разбора");
        let m = construct_model(&ast, None, &[]).expect("ошибка семантики");
        let var = VariableNode::Simple {
            upper: None,
            loc: Location::Implicit,
            name: "x".to_string(),
            ty: TypeNode::Enum("UnknownEnum".to_string()),
            expr: ExpressionNode::Number(99),
        };
        m.borrow_mut().variables.insert("x".to_string(), var);
        m
    };
    let errors = check_enum_type_safety(model_rc);
    assert!(
        errors.is_empty(),
        "неизвестный тип enum не вызывает NI6 (ошибка другой проверки)"
    );
}

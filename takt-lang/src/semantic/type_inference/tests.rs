//! Тесты вывода типов.
//!
//! Вынесены из `type_inference.rs`: файл пришпилен реестром размеров
//! (`scripts/module-size-baseline.txt`) и расти не имеет права, а новая ветвь разбора
//! узла в него всё же обязана попасть. Приём тот же, что у `semantic/validate/tests.rs` -
//! подмодуль-файл рядом с предметом.

use super::*;
use crate::parse;
use crate::semantic::tree::construct_model;

/// Строит модель из Takt-кода и возвращает корневой ModelNode.
fn build(src: &str) -> Result<ModelNode, Diagnostic> {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[]).map(|m| m.take())
}

// -- Тесты extract_type ----------------------------------------------------

/// `extract_type(Bool(true))` -> `Bool`.
#[test]
fn bool_literal_type_is_bool() {
    let ty = extract_type(
        &ExpressionNode::Bool(true),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Bool);
}

/// `extract_type(Number(42))` -> `Array(8, Bit)`.
#[test]
fn number_literal_type_is_array8() {
    let ty = extract_type(
        &ExpressionNode::Number(42),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Array(8, Box::new(TypeNode::Bit)));
}

/// `extract_type(Rational("3.14", false))` -> `Rational`.
#[test]
fn rational_literal_type_is_rational() {
    let ty = extract_type(
        &ExpressionNode::Rational("3.14".to_string(), false),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Rational);
}

/// `extract_type(Parenthesis(Bool(_)))` -> `Bool` (тип из внутреннего).
#[test]
fn parenthesis_propagates_inner_type() {
    let inner = Box::new(ExpressionNode::Bool(false));
    let ty = extract_type(
        &ExpressionNode::Parenthesis(inner),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Bool);
}

/// `Not(Bool(_))` -> `Bit`.
#[test]
fn not_expression_type_is_bit() {
    let ty = extract_type(
        &ExpressionNode::Not(Box::new(ExpressionNode::Bool(true))),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Bit);
}

/// `Equal(Number, Number)` -> `Bit`.
#[test]
fn comparison_type_is_bit() {
    let ty = extract_type(
        &ExpressionNode::Equal(
            Box::new(ExpressionNode::Number(1)),
            Box::new(ExpressionNode::Number(2)),
        ),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Bit);
}

/// `Add(Number(1), Number(2))` -> `Array(8, Bit)` (оба маленьких числа, результат
/// [bit;8]).
#[test]
fn add_two_small_numbers_is_array8() {
    let ty = extract_type(
        &ExpressionNode::Add(
            Box::new(ExpressionNode::Number(1)),
            Box::new(ExpressionNode::Number(2)),
        ),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Array(8, Box::new(TypeNode::Bit)));
}

/// `Add(Rational, Number)` -> `Rational` (расширение типа).
#[test]
fn add_rational_bit_is_rational() {
    let ty = extract_type(
        &ExpressionNode::Add(
            Box::new(ExpressionNode::Rational("1.0".into(), false)),
            Box::new(ExpressionNode::Number(2)),
        ),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Rational);
}

/// `Negate(Rational)` -> `Rational`.
#[test]
fn negate_rational_is_rational() {
    let ty = extract_type(
        &ExpressionNode::Negate(Box::new(ExpressionNode::Rational("1.0".into(), false))),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Rational);
}

/// `wider_type(Bit, Bit)` -> `Bit`.
#[test]
fn wider_type_bit_bit() {
    assert_eq!(wider_type(TypeNode::Bit, TypeNode::Bit), TypeNode::Bit);
}

/// `wider_type(Rational, Bit)` -> `Rational`.
#[test]
fn wider_type_rational_bit() {
    assert_eq!(
        wider_type(TypeNode::Rational, TypeNode::Bit),
        TypeNode::Rational
    );
}

/// `wider_type(Bit, Rational)` -> `Rational`.
#[test]
fn wider_type_bit_rational() {
    assert_eq!(
        wider_type(TypeNode::Bit, TypeNode::Rational),
        TypeNode::Rational
    );
}

/// `ast_type_to_node(Type::Bit)` -> `Bit`.
#[test]
fn ast_type_bit_to_node() {
    assert_eq!(ast_type_to_node(&Type::Bit), TypeNode::Bit);
}

/// `ast_type_to_node(Type::Bool)` -> `Bool`.
#[test]
fn ast_type_bool_to_node() {
    assert_eq!(ast_type_to_node(&Type::Bool), TypeNode::Bool);
}

/// `ast_type_to_node(Type::Rational)` -> `Rational`.
#[test]
fn ast_type_rational_to_node() {
    assert_eq!(ast_type_to_node(&Type::Rational), TypeNode::Rational);
}

// -- Интеграционные тесты через type_inference -----------------------------

/// `var x = false;` -> тип `Bool`.
#[test]
fn infer_bool_initializer() {
    let node = build("var x := false;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(ty, TypeNode::Bool);
    } else {
        panic!("переменная x не найдена");
    }
}

/// `var x = 3.14;` -> тип `Rational`.
#[test]
fn infer_rational_initializer() {
    let node = build("var x := 3.14;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(ty, TypeNode::Rational);
    } else {
        panic!("переменная x не найдена");
    }
}

/// `const C = false;` -> тип `Bool`.
#[test]
fn infer_const_bool() {
    let node = build("const C := false;").unwrap();
    if let Some(VariableNode::Const { ty, .. }) = node.search_var("C") {
        assert_eq!(ty, TypeNode::Bool);
    } else {
        panic!("константа C не найдена");
    }
}

/// Переменная с явным типом не перезаписывается выводом типа.
#[test]
fn explicit_type_not_overwritten() {
    let node = build("var x: [bit;8] := false;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(ty, TypeNode::Array(8, Box::new(TypeNode::Bit)));
    } else {
        panic!("переменная x не найдена");
    }
}

/// Вывод типа из другой переменной: `var b: bit; var a = b;` -> `a: Bit`.
#[test]
fn infer_type_from_variable() {
    let node = build("var b: bit := false; var a := b;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("a") {
        assert_eq!(ty, TypeNode::Bit);
    } else {
        panic!("переменная a не найдена");
    }
}

/// Вывод типа: `var x = 1 + 2;` -> `Array(8, Bit)` (оба операнда числовые литералы
/// <=255).
#[test]
fn infer_type_from_add_numbers() {
    let node = build("var x := 1 + 2;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(ty, TypeNode::Array(8, Box::new(TypeNode::Bit)));
    } else {
        panic!("переменная x не найдена");
    }
}

// -- Тесты extract_type: унарные и бинарные операции ----------------------

/// `UnaryPlus(Number(5))` -> `Array(8, Bit)`.
#[test]
fn unary_plus_number_type_is_bit() {
    let ty = extract_type(
        &ExpressionNode::UnaryPlus(Box::new(ExpressionNode::Number(5))),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Array(8, Box::new(TypeNode::Bit)));
}

/// `BitwiseNot(Rational)` -> `Rational`.
#[test]
fn bitwise_not_rational_is_rational() {
    let ty = extract_type(
        &ExpressionNode::BitwiseNot(Box::new(ExpressionNode::Rational("1.0".into(), false))),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Rational);
}

/// `Multiply(Rational, Number)` -> `Rational`.
#[test]
fn multiply_rational_bit_is_rational() {
    let ty = extract_type(
        &ExpressionNode::Multiply(
            Box::new(ExpressionNode::Rational("2.0".into(), false)),
            Box::new(ExpressionNode::Number(3)),
        ),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Rational);
}

/// `Subtract(Number(5), Number(3))` -> `Array(8, Bit)`.
#[test]
fn subtract_two_numbers_is_array8() {
    let ty = extract_type(
        &ExpressionNode::Subtract(
            Box::new(ExpressionNode::Number(5)),
            Box::new(ExpressionNode::Number(3)),
        ),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Array(8, Box::new(TypeNode::Bit)));
}

/// `ShiftLeft(Number(1), Number(2))` -> `Array(8, Bit)`.
#[test]
fn shift_left_numbers_is_array8() {
    let ty = extract_type(
        &ExpressionNode::ShiftLeft(
            Box::new(ExpressionNode::Number(1)),
            Box::new(ExpressionNode::Number(2)),
        ),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Array(8, Box::new(TypeNode::Bit)));
}

// -- Тесты extract_type: условие и тернарный оператор ---------------------

/// `ConditionalOperator(_, Bit, Rational)` -> `Rational`.
#[test]
fn conditional_operator_widens_type() {
    let ty = extract_type(
        &ExpressionNode::ConditionalOperator(
            Box::new(ExpressionNode::Bool(true)),
            Box::new(ExpressionNode::Number(0)),
            Box::new(ExpressionNode::Rational("1.0".into(), false)),
        ),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Rational);
}

/// `ConditionalOperator(_, Number(1), Number(0))` -> `Array(8, Bit)`.
#[test]
fn conditional_operator_both_bit_is_bit() {
    let ty = extract_type(
        &ExpressionNode::ConditionalOperator(
            Box::new(ExpressionNode::Bool(true)),
            Box::new(ExpressionNode::Number(1)),
            Box::new(ExpressionNode::Number(0)),
        ),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Array(8, Box::new(TypeNode::Bit)));
}

// -- Тесты extract_type: массивы -------------------------------------------

/// `Array([Number(1), Number(2)])` -> `Array(2, Array(8, Bit))`.
#[test]
fn array_literal_infers_element_type() {
    let ty = extract_type(
        &ExpressionNode::Array(vec![ExpressionNode::Number(1), ExpressionNode::Number(2)]),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(
        ty,
        TypeNode::Array(2, Box::new(TypeNode::Array(8, Box::new(TypeNode::Bit))))
    );
}

/// `Array([])` -> `Array(0, Bit)`.
#[test]
fn empty_array_literal_type() {
    let ty = extract_type(
        &ExpressionNode::Array(vec![]),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Array(0, Box::new(TypeNode::Bit)));
}

/// `Assign(_, Rational)` -> `Rational`.
#[test]
fn assign_infers_rhs_type() {
    let ty = extract_type(
        &ExpressionNode::Assign(
            Box::new(ExpressionNode::None),
            Box::new(ExpressionNode::Rational("3.14".into(), false)),
        ),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Rational);
}

// -- Тесты extract_type: приведение типа ----------------------------------

/// `Cast(_, Type::Rational)` -> `Rational`.
#[test]
fn cast_to_rational_type() {
    let ty = extract_type(
        &ExpressionNode::Cast(Box::new(ExpressionNode::Number(0)), TypeNode::Rational),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Rational);
}

/// `Cast(_, Type::Array{...})` -> `Array(N, Bit)`.
#[test]
fn cast_to_array_type() {
    let ty = extract_type(
        &ExpressionNode::Cast(
            Box::new(ExpressionNode::Number(0)),
            TypeNode::Array(4, Box::new(TypeNode::Bit)),
        ),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Array(4, Box::new(TypeNode::Bit)));
}

// -- Тесты extract_type: спец. выражения ---------------------------------

/// `String([...])` -> `Unsupported`.
#[test]
fn string_literal_type_is_unsupported() {
    let ty = extract_type(
        &ExpressionNode::String(vec!["hello".into()]),
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Unsupported);
}

/// `Expression::None` -> `Unsupported`.
#[test]
fn none_expression_type_is_unsupported() {
    let ty = extract_type(
        &ExpressionNode::None,
        Rc::new(RefCell::new(ModelNode::default())),
    )
    .unwrap();
    assert_eq!(ty, TypeNode::Unsupported);
}

/// `wider_type(Unsupported, Bit)` -> `Unsupported`.
#[test]
fn wider_type_unsupported_bit() {
    assert_eq!(
        wider_type(TypeNode::Unsupported, TypeNode::Bit),
        TypeNode::Unsupported
    );
}

/// `wider_type(Array, Bit)` -> `Array` (пока сохраняет первый тип массива).
#[test]
fn wider_type_array_bit_returns_array() {
    let arr = TypeNode::Array(4, Box::new(TypeNode::Bit));
    let result = wider_type(arr.clone(), TypeNode::Bit);
    assert!(matches!(result, TypeNode::Array(4, _)));
}

/// `ast_type_to_node(Type::Unit)` -> `Unit`.
#[test]
fn ast_type_unit_to_node() {
    use crate::parser::ast::Type;
    assert_eq!(ast_type_to_node(&Type::Unit), TypeNode::Unit);
}

/// `ast_type_to_node(Type::Array{...})` -> `Array`.
#[test]
fn ast_type_array_to_node() {
    use crate::parser::ast::Type;
    let arr_type = Type::Array {
        loc: crate::diagnostics::Location::default(),
        element_type: Box::new(Type::Bit),
        element_count: 8,
    };
    assert_eq!(
        ast_type_to_node(&arr_type),
        TypeNode::Array(8, Box::new(TypeNode::Bit))
    );
}

// -- Интеграционные тесты через type_inference -----------------------------

/// `var x = 1 + 3.14;` -> тип `Rational` (расширение через сложение).
#[test]
fn infer_type_from_add_rational() {
    let node = build("var x := 1 + 3.14;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(ty, TypeNode::Rational);
    } else {
        panic!("переменная x не найдена");
    }
}

/// `const C = 1;` -> тип `Array(8, Bit)` (числовой литерал <=255 -> [bit;8]).
#[test]
fn infer_const_number() {
    let node = build("const C := 1;").unwrap();
    if let Some(VariableNode::Const { ty, .. }) = node.search_var("C") {
        assert_eq!(ty, TypeNode::Array(8, Box::new(TypeNode::Bit)));
    } else {
        panic!("константа C не найдена");
    }
}

/// `const C = 42;` -> тип `Array(8, Bit)` (42 <= 255 -> [bit;8]).
#[test]
fn infer_const_number_42_is_array8() {
    let node = build("const C := 42;").unwrap();
    if let Some(VariableNode::Const { ty, .. }) = node.search_var("C") {
        assert_eq!(ty, TypeNode::Array(8, Box::new(TypeNode::Bit)));
    } else {
        panic!("константа C не найдена");
    }
}

// -- Тесты infer_int_type --------------------------------------------------

/// `infer_int_type(0)` -> `Array(8, Bit)`.
#[test]
fn infer_int_type_zero() {
    assert_eq!(
        infer_int_type(0),
        TypeNode::Array(8, Box::new(TypeNode::Bit))
    );
}

/// `infer_int_type(255)` -> `Array(8, Bit)`.
#[test]
fn infer_int_type_255() {
    assert_eq!(
        infer_int_type(255),
        TypeNode::Array(8, Box::new(TypeNode::Bit))
    );
}

/// `infer_int_type(256)` -> `Array(16, Bit)`.
#[test]
fn infer_int_type_256() {
    assert_eq!(
        infer_int_type(256),
        TypeNode::Array(16, Box::new(TypeNode::Bit))
    );
}

/// `infer_int_type(65535)` -> `Array(16, Bit)`.
#[test]
fn infer_int_type_65535() {
    assert_eq!(
        infer_int_type(65535),
        TypeNode::Array(16, Box::new(TypeNode::Bit))
    );
}

/// `infer_int_type(65536)` -> `Array(32, Bit)`.
#[test]
fn infer_int_type_65536() {
    assert_eq!(
        infer_int_type(65536),
        TypeNode::Array(32, Box::new(TypeNode::Bit))
    );
}

/// `infer_int_type(4294967296)` -> `Array(64, Bit)`.
#[test]
fn infer_int_type_large() {
    assert_eq!(
        infer_int_type(4294967296),
        TypeNode::Array(64, Box::new(TypeNode::Bit))
    );
}

/// `infer_int_type(-1)` -> `Array(64, Bit)` (отрицательное -> 64-бит).
#[test]
fn infer_int_type_negative() {
    assert_eq!(
        infer_int_type(-1),
        TypeNode::Array(64, Box::new(TypeNode::Bit))
    );
}

// -- Ce4: Тесты wider_type для перечислений --------------------------------

/// Ce4: два одинаковых перечисления -> сохраняют тип.
///
/// # Пример
/// Выражение `color1 + color2`, оба типа `Color` -> тип результата `Color`.
#[test]
fn wider_type_same_enum_returns_enum() {
    let a = TypeNode::Enum("Color".to_string());
    let b = TypeNode::Enum("Color".to_string());
    assert_eq!(wider_type(a, b), TypeNode::Enum("Color".to_string()));
}

/// Ce4: два разных перечисления несовместимы -> `Unsupported`.
///
/// # Контр-пример
/// `Color` и `Size` - разные типы, смешение недопустимо.
#[test]
fn wider_type_different_enums_is_unsupported() {
    let a = TypeNode::Enum("Color".to_string());
    let b = TypeNode::Enum("Size".to_string());
    assert_eq!(wider_type(a, b), TypeNode::Unsupported);
}

/// Ce4: перечисление с числовым типом несовместимо -> `Unsupported`.
///
/// # Контр-пример
/// `Color` + `Bit` - нельзя расширить enum до Bit.
#[test]
fn wider_type_enum_and_bit_is_unsupported() {
    let a = TypeNode::Enum("Color".to_string());
    assert_eq!(wider_type(a.clone(), TypeNode::Bit), TypeNode::Unsupported);
    assert_eq!(wider_type(TypeNode::Bit, a), TypeNode::Unsupported);
}

/// Ce4: перечисление с Rational несовместимо -> `Unsupported`.
///
/// # Контр-пример
/// Enum не расширяется до Rational - это нарушение типовой безопасности.
#[test]
fn wider_type_enum_and_rational_is_unsupported() {
    let a = TypeNode::Enum("Dir".to_string());
    assert_eq!(
        wider_type(a.clone(), TypeNode::Rational),
        TypeNode::Unsupported
    );
    assert_eq!(wider_type(TypeNode::Rational, a), TypeNode::Unsupported);
}

// -- Ce4: Тесты ast_type_to_node для перечислений --------------------------

/// Ce4: `ast_type_to_node(Type::Enum("Color"))` -> `TypeNode::Enum("Color")`.
#[test]
fn ast_type_enum_to_node() {
    use crate::parser::ast::Type;
    assert_eq!(
        ast_type_to_node(&Type::Enum("Color".to_string())),
        TypeNode::Enum("Color".to_string())
    );
}

/// Ce4: `ast_type_to_node_ctx` при наличии enum в модели -> `TypeNode::Enum`.
///
/// # Пример
/// Объявлен `enum Dir { ... }`, тип `Dir` разрешается в `TypeNode::Enum("Dir")`.
#[test]
fn ast_type_to_node_ctx_with_enum_in_model() {
    use crate::parser::ast::Type;
    use crate::semantic::EnumDefinitionNode;

    let model = Rc::new(RefCell::new(ModelNode::default()));
    let e = EnumDefinitionNode::new("Dir", &[("North", None), ("South", None)]);
    model.borrow_mut().enums.insert("Dir".to_string(), e);

    assert_eq!(
        ast_type_to_node_ctx(&Type::Enum("Dir".to_string()), model),
        TypeNode::Enum("Dir".to_string())
    );
}

/// Ce4: `ast_type_to_node_ctx` при отсутствии enum в модели -> `TypeNode::Unsupported`.
///
/// # Контр-пример
/// Тип `UnknownEnum` не объявлен -> `Unsupported` (ошибка диагностируется в validate_model).
#[test]
fn ast_type_to_node_ctx_unknown_enum_is_unsupported() {
    use crate::parser::ast::Type;

    let model = Rc::new(RefCell::new(ModelNode::default()));
    assert_eq!(
        ast_type_to_node_ctx(&Type::Enum("UnknownEnum".to_string()), model),
        TypeNode::Unsupported
    );
}

// -- Тесты wider_type: Bool-варианты и массивы -----------------------------

/// `wider_type(Bool, Bool)` -> `Bool`.
#[test]
fn wider_type_bool_bool() {
    assert_eq!(wider_type(TypeNode::Bool, TypeNode::Bool), TypeNode::Bool);
}

/// `wider_type(Bool, Bit)` -> `Bit`.
#[test]
fn wider_type_bool_bit() {
    assert_eq!(wider_type(TypeNode::Bool, TypeNode::Bit), TypeNode::Bit);
}

/// `wider_type(Bit, Bool)` -> `Bit`.
#[test]
fn wider_type_bit_bool() {
    assert_eq!(wider_type(TypeNode::Bit, TypeNode::Bool), TypeNode::Bit);
}

/// `wider_type(Array(8, Bit), Array(16, Bit))` -> `Array(16, Bit)`.
#[test]
fn wider_type_array8_array16_returns_array16() {
    let a = TypeNode::Array(8, Box::new(TypeNode::Bit));
    let b = TypeNode::Array(16, Box::new(TypeNode::Bit));
    assert_eq!(
        wider_type(a, b),
        TypeNode::Array(16, Box::new(TypeNode::Bit))
    );
}

/// `wider_type(Array(16, Bit), Array(8, Bit))` -> `Array(16, Bit)`.
#[test]
fn wider_type_array16_array8_returns_array16() {
    let a = TypeNode::Array(16, Box::new(TypeNode::Bit));
    let b = TypeNode::Array(8, Box::new(TypeNode::Bit));
    assert_eq!(
        wider_type(a, b),
        TypeNode::Array(16, Box::new(TypeNode::Bit))
    );
}

// -- Интеграционные тесты: вывод типа для больших числовых литералов -------

/// `var x = 256;` -> тип `Array(16, Bit)`.
#[test]
fn infer_number_256_is_array16() {
    let node = build("var x := 256;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(ty, TypeNode::Array(16, Box::new(TypeNode::Bit)));
    } else {
        panic!("переменная x не найдена");
    }
}

/// `var x = 65536;` -> тип `Array(32, Bit)`.
#[test]
fn infer_number_65536_is_array32() {
    let node = build("var x := 65536;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(ty, TypeNode::Array(32, Box::new(TypeNode::Bit)));
    } else {
        panic!("переменная x не найдена");
    }
}

// -- Именованные целые в таблице расширения --------------------

/// Пара именованных целых: ширина - наибольшая, знак - от знакового.
#[test]
fn wider_type_integer_pair_takes_max_width() {
    let u16t = TypeNode::Integer {
        bits: 16,
        signed: false,
    };
    let u8t = TypeNode::Integer {
        bits: 8,
        signed: false,
    };
    assert_eq!(wider_type(u16t.clone(), u8t.clone()), u16t);
    // Симметрия: порядок операндов на результат не влияет.
    assert_eq!(wider_type(u8t, u16t.clone()), u16t);
}

/// Знак заразителен: `i8 + u8` даёт знаковый тип.
#[test]
fn wider_type_integer_pair_keeps_sign() {
    let i8t = TypeNode::Integer {
        bits: 8,
        signed: true,
    };
    let u8t = TypeNode::Integer {
        bits: 8,
        signed: false,
    };
    assert_eq!(
        wider_type(i8t.clone(), u8t.clone()),
        TypeNode::Integer {
            bits: 8,
            signed: true
        }
    );
    assert_eq!(
        wider_type(u8t, i8t),
        TypeNode::Integer {
            bits: 8,
            signed: true
        }
    );
}

/// Тип источника побеждает тип литерала: `i16 + [bit;8]` остаётся знаковым.
#[test]
fn wider_type_integer_beats_literal_vector() {
    let i16t = TypeNode::Integer {
        bits: 16,
        signed: true,
    };
    let lit = TypeNode::Array(8, Box::new(TypeNode::Bit));
    assert_eq!(wider_type(i16t.clone(), lit.clone()), i16t);
    assert_eq!(wider_type(lit, i16t.clone()), i16t);
}

/// Граница: вектор шире именованного целого - решают прежние ветви `Array`.
#[test]
fn wider_type_wider_vector_stays_array() {
    let u8t = TypeNode::Integer {
        bits: 8,
        signed: false,
    };
    let lit = TypeNode::Array(16, Box::new(TypeNode::Bit));
    assert_eq!(wider_type(u8t, lit.clone()), lit);
}

/// Граница: массив не битов с целым не смешивается - прежнее правило.
#[test]
fn wider_type_data_array_is_untouched() {
    let u8t = TypeNode::Integer {
        bits: 8,
        signed: false,
    };
    let arr = TypeNode::Array(4, Box::new(TypeNode::Rational));
    assert!(matches!(wider_type(u8t, arr), TypeNode::Array(4, _)));
}

/// Бит и логическое значение уточняют тип целого, не подменяя его.
#[test]
fn wider_type_integer_with_bit_and_bool() {
    let u8t = TypeNode::Integer {
        bits: 8,
        signed: false,
    };
    assert_eq!(wider_type(u8t.clone(), TypeNode::Bit), u8t);
    assert_eq!(wider_type(TypeNode::Bit, u8t.clone()), u8t);
    assert_eq!(wider_type(u8t.clone(), TypeNode::Bool), u8t);
    assert_eq!(wider_type(TypeNode::Bool, u8t.clone()), u8t);
}

/// Граница: соседние правила сильнее - `Rational` и перечисление не задеты.
#[test]
fn wider_type_neighbours_win_over_integer() {
    let u8t = TypeNode::Integer {
        bits: 8,
        signed: false,
    };
    assert_eq!(
        wider_type(u8t.clone(), TypeNode::Rational),
        TypeNode::Rational
    );
    assert_eq!(
        wider_type(u8t, TypeNode::Enum("Color".to_string())),
        TypeNode::Unsupported
    );
}

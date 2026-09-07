//! Генерация исходного C-файла (`.c`) из семантического дерева Takt.
//!
//! Точка входа: [`generate_source`] - собирает все секции `.c`-файла, делегируя
//! генерацию деклараций, функций и моделей соответствующим модулям.

use super::c_decl::{generate_constants_and_ports_and_enums, generate_functions};
use super::c_model::{generate_function_prototypes, generate_model_functions};
use crate::diagnostics::Diagnostic;
use crate::generator::c::c_map::CMap;
use crate::generator::header::{CommentStyle, file_header};
use crate::generator::indent::Printer;

/// Генерирует содержимое `.c`-файла для модели.
pub(super) fn generate_source(filename: &str, map: &CMap) -> Result<String, Diagnostic> {
    let mut source = String::new();
    let mut printer = Printer::new(4, &mut source);
    for line in file_header(
        crate::generator::c::c_header::c_target_name(map.hal()),
        CommentStyle::Slashes,
    ) {
        printer.print(&line).nl();
    }
    printer
        .print(format!("#include \"{}.h\"", filename).as_str())
        .nl();
    printer.print("#include <assert.h>").nl();
    printer.print("#include <math.h>").nl();
    generate_constants_and_ports_and_enums(&mut printer, map)?;
    generate_function_prototypes(&mut printer, map)?;
    generate_functions(&mut printer, map)?;
    for model in map.using_models() {
        generate_model_functions(&mut printer, &model, map)?;
    }
    generate_model_functions(&mut printer, &map.model(), map)?;
    Ok(super::c_expr::insert_fixed_helpers(source)) // Q-хелперы - по вызову
}

#[cfg(test)]
mod tests {
    use crate::generator::c::c_map::CMap;
    use crate::generator::c::c_source::generate_source;
    use crate::semantic::ExpressionNode;
    use crate::{parse, semantic};

    use crate::generator::c::c_expr::generate_stmt_expression;
    use crate::generator::indent::Printer;
    use crate::semantic::minimap::Element;

    // -- Вспомогательная функция ------------------------------------------------

    /// Создаёт минимальный CMap для тестов генерации выражений.
    fn make_map_and_owner(src: &str) -> (CMap, Element) {
        let (ast, _) = parse(src, 0).expect("ошибка разбора");
        let model_rc = semantic::tree::construct_model(&ast, None, &[]).unwrap();
        model_rc.borrow_mut().name = Some("Main".to_string());
        let model = model_rc.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let owner = Element::Model {
            name: map.root_name().clone(),
            states: map.states().clone(),
            start: map.start().clone(),
        };
        (map, owner)
    }

    /// Генерирует C-строку из выражения.
    fn expr_to_str(map: &CMap, owner: &Element, expr: &ExpressionNode) -> String {
        let mut s = String::new();
        let mut printer = Printer::new(4, &mut s);
        generate_stmt_expression(&mut printer, map, owner, vec![], expr, true).unwrap();
        s
    }

    // -- Тесты литералов --------------------------------------------------------

    #[test]
    fn test_expr_number() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::Number(42);
        assert_eq!(expr_to_str(&map, &owner, &expr), "42");
    }

    #[test]
    fn test_expr_bool_true() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        assert_eq!(
            expr_to_str(&map, &owner, &ExpressionNode::Bool(true)),
            "true"
        );
    }

    #[test]
    fn test_expr_bool_false() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        assert_eq!(
            expr_to_str(&map, &owner, &ExpressionNode::Bool(false)),
            "false"
        );
    }

    #[test]
    fn test_expr_rational_positive() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::Rational("3.14".to_string(), false);
        assert_eq!(expr_to_str(&map, &owner, &expr), "3.14");
    }

    #[test]
    fn test_expr_rational_negative() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::Rational("3.14".to_string(), true);
        assert_eq!(expr_to_str(&map, &owner, &expr), "-3.14");
    }

    #[test]
    fn test_expr_string() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::String(vec!["hello".to_string()]);
        assert_eq!(expr_to_str(&map, &owner, &expr), "\"hello\"");
    }

    // -- Тесты унарных операторов -----------------------------------------------

    #[test]
    fn test_expr_negate() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // Атом (число) - скобки не нужны
        let expr = ExpressionNode::Negate(Box::new(ExpressionNode::Number(42)));
        assert_eq!(expr_to_str(&map, &owner, &expr), "-42");
    }

    #[test]
    fn test_expr_negate_complex() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // Бинарное выражение внутри унарного - скобки нужны
        let inner = ExpressionNode::Add(
            Box::new(ExpressionNode::Number(1)),
            Box::new(ExpressionNode::Number(2)),
        );
        let expr = ExpressionNode::Negate(Box::new(inner));
        assert_eq!(expr_to_str(&map, &owner, &expr), "-(1 + 2)");
    }

    #[test]
    fn test_expr_negate_negate() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // Двойное отрицание - скобки нужны чтобы избежать `--x` (декремент в C)
        let inner = ExpressionNode::Negate(Box::new(ExpressionNode::Number(5)));
        let expr = ExpressionNode::Negate(Box::new(inner));
        assert_eq!(expr_to_str(&map, &owner, &expr), "-(-5)");
    }

    #[test]
    fn test_expr_not() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // Атом - без скобок
        let expr = ExpressionNode::Not(Box::new(ExpressionNode::Bool(true)));
        assert_eq!(expr_to_str(&map, &owner, &expr), "!true");
    }

    #[test]
    fn test_expr_not_complex() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // Логическое И внутри NOT - нужны скобки
        let inner = ExpressionNode::And(
            Box::new(ExpressionNode::Bool(true)),
            Box::new(ExpressionNode::Bool(false)),
        );
        let expr = ExpressionNode::Not(Box::new(inner));
        assert_eq!(expr_to_str(&map, &owner, &expr), "!(true && false)");
    }

    #[test]
    fn test_expr_bitwise_not() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // Атом - без скобок
        let expr = ExpressionNode::BitwiseNot(Box::new(ExpressionNode::Number(0xFF)));
        assert_eq!(expr_to_str(&map, &owner, &expr), "~255");
    }

    #[test]
    fn test_expr_parenthesis() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // Явные скобки из исходника - всегда генерируются
        let inner = Box::new(ExpressionNode::Number(42));
        let expr = ExpressionNode::Parenthesis(inner);
        assert_eq!(expr_to_str(&map, &owner, &expr), "(42)");
    }

    // -- Тесты бинарных операторов ----------------------------------------------

    #[test]
    fn test_expr_add() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // Атомы - без скобок
        let expr = ExpressionNode::Add(
            Box::new(ExpressionNode::Number(1)),
            Box::new(ExpressionNode::Number(2)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "1 + 2");
    }

    #[test]
    fn test_expr_subtract() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::Subtract(
            Box::new(ExpressionNode::Number(5)),
            Box::new(ExpressionNode::Number(3)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "5 - 3");
    }

    #[test]
    fn test_expr_multiply() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::Multiply(
            Box::new(ExpressionNode::Number(4)),
            Box::new(ExpressionNode::Number(5)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "4 * 5");
    }

    #[test]
    fn test_expr_divide() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::Divide(
            Box::new(ExpressionNode::Number(10)),
            Box::new(ExpressionNode::Number(2)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "10 / 2");
    }

    #[test]
    fn test_expr_modulo() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::Modulo(
            Box::new(ExpressionNode::Number(7)),
            Box::new(ExpressionNode::Number(3)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "7 % 3");
    }

    #[test]
    fn test_expr_shift_left() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::ShiftLeft(
            Box::new(ExpressionNode::Number(1)),
            Box::new(ExpressionNode::Number(4)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "1 << 4");
    }

    #[test]
    fn test_expr_shift_right() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::ShiftRight(
            Box::new(ExpressionNode::Number(16)),
            Box::new(ExpressionNode::Number(2)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "16 >> 2");
    }

    #[test]
    fn test_expr_bitwise_and() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::BitwiseAnd(
            Box::new(ExpressionNode::Number(0xF0)),
            Box::new(ExpressionNode::Number(0xFF)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "240 & 255");
    }

    #[test]
    fn test_expr_bitwise_or() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::BitwiseOr(
            Box::new(ExpressionNode::Number(0xF0)),
            Box::new(ExpressionNode::Number(0x0F)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "240 | 15");
    }

    #[test]
    fn test_expr_bitwise_xor() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::BitwiseXor(
            Box::new(ExpressionNode::Number(0xAA)),
            Box::new(ExpressionNode::Number(0x55)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "170 ^ 85");
    }

    #[test]
    fn test_expr_less() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::Less(
            Box::new(ExpressionNode::Number(1)),
            Box::new(ExpressionNode::Number(2)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "1 < 2");
    }

    #[test]
    fn test_expr_more() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::More(
            Box::new(ExpressionNode::Number(3)),
            Box::new(ExpressionNode::Number(2)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "3 > 2");
    }

    #[test]
    fn test_expr_equal() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::Equal(
            Box::new(ExpressionNode::Number(0)),
            Box::new(ExpressionNode::Number(0)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "0 == 0");
    }

    #[test]
    fn test_expr_not_equal() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::NotEqual(
            Box::new(ExpressionNode::Number(1)),
            Box::new(ExpressionNode::Number(0)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "1 != 0");
    }

    #[test]
    fn test_expr_logical_and() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::And(
            Box::new(ExpressionNode::Bool(true)),
            Box::new(ExpressionNode::Bool(false)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "true && false");
    }

    #[test]
    fn test_expr_logical_or() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::Or(
            Box::new(ExpressionNode::Bool(false)),
            Box::new(ExpressionNode::Bool(true)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "false || true");
    }

    #[test]
    fn test_expr_conditional_operator() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // Атомы - без скобок
        let expr = ExpressionNode::ConditionalOperator(
            Box::new(ExpressionNode::Bool(true)),
            Box::new(ExpressionNode::Number(1)),
            Box::new(ExpressionNode::Number(0)),
        );
        assert_eq!(expr_to_str(&map, &owner, &expr), "true ? 1 : 0");
    }

    #[test]
    fn test_expr_cast() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        use crate::semantic::type_node::TypeNode;
        // Атом - без скобок после типа
        let expr = ExpressionNode::Cast(Box::new(ExpressionNode::Number(42)), TypeNode::Bit);
        // (Д2): `bit` -> `uint8_t`, а не `int` (32-битный знаковый).
        assert_eq!(expr_to_str(&map, &owner, &expr), "(uint8_t)42");
    }

    #[test]
    fn test_expr_cast_complex() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        use crate::semantic::type_node::TypeNode;
        // Бинарное выражение - нужны скобки после типа
        let inner = ExpressionNode::Add(
            Box::new(ExpressionNode::Number(1)),
            Box::new(ExpressionNode::Number(2)),
        );
        let expr = ExpressionNode::Cast(Box::new(inner), TypeNode::Bit);
        // (Д2): `bit` -> `uint8_t`.
        assert_eq!(expr_to_str(&map, &owner, &expr), "(uint8_t)(1 + 2)");
    }

    // -- Тесты приоритета операторов --------------------------------------------

    #[test]
    fn test_expr_precedence_mul_wins_over_add_left() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // (a*b) + c -> a * b + c (умножение на левой стороне сложения - без скобок)
        let mul = ExpressionNode::Multiply(
            Box::new(ExpressionNode::Number(2)),
            Box::new(ExpressionNode::Number(3)),
        );
        let expr = ExpressionNode::Add(Box::new(mul), Box::new(ExpressionNode::Number(4)));
        assert_eq!(expr_to_str(&map, &owner, &expr), "2 * 3 + 4");
    }

    #[test]
    fn test_expr_precedence_add_needs_parens_in_mul() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // (a+b) * c -> (a + b) * c (сложение в левом операнде умножения - скобки)
        let add = ExpressionNode::Add(
            Box::new(ExpressionNode::Number(2)),
            Box::new(ExpressionNode::Number(3)),
        );
        let expr = ExpressionNode::Multiply(Box::new(add), Box::new(ExpressionNode::Number(4)));
        assert_eq!(expr_to_str(&map, &owner, &expr), "(2 + 3) * 4");
    }

    #[test]
    fn test_expr_precedence_sub_right_needs_parens() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // a - (b - c) -> a - (b - c) (тот же приоритет на правой стороне вычитания)
        let sub_right = ExpressionNode::Subtract(
            Box::new(ExpressionNode::Number(3)),
            Box::new(ExpressionNode::Number(1)),
        );
        let expr =
            ExpressionNode::Subtract(Box::new(ExpressionNode::Number(5)), Box::new(sub_right));
        assert_eq!(expr_to_str(&map, &owner, &expr), "5 - (3 - 1)");
    }

    #[test]
    fn test_expr_precedence_or_inside_and() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // (a || b) && c -> (a || b) && c (OR имеет меньший приоритет чем AND)
        let or_expr = ExpressionNode::Or(
            Box::new(ExpressionNode::Bool(true)),
            Box::new(ExpressionNode::Bool(false)),
        );
        let expr = ExpressionNode::And(Box::new(or_expr), Box::new(ExpressionNode::Bool(true)));
        assert_eq!(expr_to_str(&map, &owner, &expr), "(true || false) && true");
    }

    #[test]
    fn test_expr_precedence_and_no_parens_inside_or() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // (a && b) || c -> a && b || c (AND имеет больший приоритет чем OR - без
        // скобок)
        let and_expr = ExpressionNode::And(
            Box::new(ExpressionNode::Bool(true)),
            Box::new(ExpressionNode::Bool(false)),
        );
        let expr = ExpressionNode::Or(Box::new(and_expr), Box::new(ExpressionNode::Bool(true)));
        assert_eq!(expr_to_str(&map, &owner, &expr), "true && false || true");
    }

    #[test]
    fn test_expr_precedence_compare_in_not() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        // !(a > b) -> !(a > b)
        let cmp = ExpressionNode::More(
            Box::new(ExpressionNode::Number(5)),
            Box::new(ExpressionNode::Number(3)),
        );
        let expr = ExpressionNode::Not(Box::new(cmp));
        assert_eq!(expr_to_str(&map, &owner, &expr), "!(5 > 3)");
    }

    #[test]
    fn test_expr_initializer() {
        let (map, owner) = make_map_and_owner("start Main { always { } }");
        let expr = ExpressionNode::Initializer(vec![
            ExpressionNode::Number(1),
            ExpressionNode::Number(2),
            ExpressionNode::Number(3),
        ]);
        assert_eq!(expr_to_str(&map, &owner, &expr), "{1, 2, 3}");
    }

    // -- Интеграционные тесты generate_source ----------------------------------

    /// Порождает `.c` для исходника (корень - `Main`).
    fn source_of(src: &str) -> Result<String, crate::diagnostics::Diagnostic> {
        let (model_ast, _) = parse(src, 0).unwrap();
        let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model_rc.borrow_mut().name = Some("Main".to_string());
        let model = model_rc.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        generate_source(map.get_filename(), &map)
    }

    /// Агрегатный инициализатор массива печатается поэлементно.
    ///
    /// Массив в C не является изменяемым lvalue: `model->arr = {0,0,0,0};` отвергается
    /// (`error: expected expression`), и составной литерал не спасает - присваивание
    /// массиву запрещено в принципе. Выход один: поэлементная запись. Строки сняты с
    /// настоящего вывода `taktc -t c`, а сам вывод проверен `cc -std=c11 -Wall -Werror`.
    #[test]
    fn test_array_aggregate_initializer_is_element_wise() {
        let src = r#"
type Byte = [bit;8];
var arr: [Byte;4] := {0,0,0,0};
var counter: u8 := 0;
start Idle { always { arr[0] := 7; counter := 1; } }
"#;
        let source = source_of(src).expect("порождение .c");
        for i in 0..4 {
            assert!(
                source.contains(&format!("model->arr[{}] = 0;", i)),
                "элемент {} обязан инициализироваться отдельно:\n{source}",
                i
            );
        }
        assert!(
            !source.contains("model->arr = {"),
            "присваивание агрегата массиву — невалидный C:\n{source}"
        );
    }

    /// **.** Скалярный инициализатор массива -> `CC-017`, а не догадка.
    ///
    /// `var data: [u8;4] := 0;` язык не определяет: обнулить весь массив? записать в
    /// первый элемент? Цель `st` инициализатор отбрасывает, симулятор кладёт скаляр
    /// (после чего `data[0]` даёт `SIM-010`) - три ответа расходятся. Выбор одного -
    /// вопрос семантики языка, вне полномочий.
    #[test]
    fn test_array_scalar_initializer_is_rejected_with_cc_017() {
        let src = r#"
var data: [u8;4] := 0;
var counter: u8 := 0;
start Idle { always { data[0] := 7; counter := 1; } }
"#;
        let diag =
            source_of(src).expect_err("скалярный инициализатор массива обязан быть отвергнут");
        assert_eq!(diag.code.as_deref(), Some("CC-017"));
        assert!(
            diag.message.contains("data"),
            "сообщение обязано называть переменную: {}",
            diag.message
        );
    }

    /// Бит-вектор - скаляр, и присваивание ему печатается целиком, а не поэлементно.
    #[test]
    fn test_bit_vector_initializer_stays_scalar_assignment() {
        let src = r#"
type Byte = [bit;8];
var b: Byte := 0xFF;
var counter: u8 := 0;
start Idle { always { counter := b; } }
"#;
        let source = source_of(src).expect("порождение .c");
        assert!(
            source.contains("model->b = 255;"),
            "[bit;8] — скаляр uint8_t, присваивание законно:\n{source}"
        );
    }

    #[test]
    fn test_generate_source_has_include_and_math() {
        let src = r#"start Main { always { } }"#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model_rc.borrow_mut().name = Some("Main".to_string());
        let model = model_rc.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let source = generate_source(map.get_filename(), &map).unwrap();
        assert!(
            source.contains("#include \""),
            "отсутствует #include header:\n{source}"
        );
        assert!(
            source.contains(".h\""),
            "отсутствует .h в include:\n{source}"
        );
        assert!(
            source.contains("#include <math.h>"),
            "отсутствует #include <math.h>:\n{source}"
        );
    }

    #[test]
    fn test_generate_source_with_const_and_port() {
        // LIMIT используется в блоке always (присваивание переменной), чтобы константа
        // попала в UsageSet и не была отфильтрована.
        let src = r#"
const LIMIT: u8 := 100;
in SENSOR: u8 at 0x100000;
var v: u8 := 0;
start Main { always { v := LIMIT; } }
        "#;
        let (model_ast, _) = parse(src, 0).unwrap();
        let model_rc = semantic::tree::construct_model(&model_ast, None, &[]).unwrap();
        model_rc.borrow_mut().name = Some("Main".to_string());
        let model = model_rc.borrow();
        let map = CMap::new(model.name(), &*model, true).unwrap();
        let source = generate_source(map.get_filename(), &map).unwrap();
        assert!(
            source.contains("CONST_MAIN_LIMIT"),
            "CONST_MAIN_LIMIT отсутствует:\n{source}"
        );
        // Порт теперь генерируется как вариант enum в заголовочном файле, а не как
        // #define в.c-файле - в source больше нет PORT_MAIN_SENSOR.
        assert!(
            !source.contains("PORT_MAIN_SENSOR"),
            "PORT_MAIN_SENSOR не должен присутствовать в .c-файле (теперь это enum в .h):\n{source}"
        );
    }

    mod part2;
}

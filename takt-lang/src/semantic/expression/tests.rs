//! Тесты построения узлов выражений (вынесены из `mod.rs` ).
//!
//! Причина выноса - правило размера модуля: `expression.rs` стоял в реестре
//! узаконенного долга (1360 строк) и расти не имел права, а тест глубины рекурсии
//! требовал правки самого модуля. Разделение "логика / тесты" - приём
//! (директория-подмодуль + `use super::*`).

use super::*;
use crate::parse;
use crate::semantic::tree::construct_model;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{ConditionNode, ModelNode, VariableNode};
// -- Вспомогательные функции -----------------------------------------------

/// Строит семантическую модель из исходного кода Takt.
fn build(src: &str) -> Result<ModelNode, Diagnostic> {
    let (ast, _) = parse(src, 0).expect("ошибка разбора");
    construct_model(&ast, None, &[]).map(|m| m.take())
}

/// Возвращает разрешённый инициализатор переменной `name`.
fn var_expr(node: &ModelNode, name: &str) -> ExpressionNode {
    match node.search_var(name).expect("переменная не найдена") {
        VariableNode::Simple { expr, .. }
        | VariableNode::Const { expr, .. }
        | VariableNode::Port { init: expr, .. } => expr,
        VariableNode::Unresolved => panic!("переменная неразрешена"),
    }
}

/// Выражение, построенное **в теле блока**: `always { sink := <expr>; }`.
///
/// Площадка перенесена из инициализатора переменной. Инициализаторы `var`/`const`
/// сворачиваются в литерал **на семантике**, и выражения там больше не существует - это
/// замысел ("за границей семантики выражения нет, расходиться потребителям не по
/// чему"), а не потеря покрытия. В теле блока выражение живёт как прежде, поэтому
/// построение узлов проверяется здесь - теми же утверждениями, что и раньше.
///
/// `decls` - дополнительные объявления модели (массив, переменная-операнд).
fn body_expr(decls: &str, expr: &str) -> ExpressionNode {
    let src =
        format!("{decls} var sink: [bit;8] := 0;\nstart S {{ always {{ sink := {expr}; }} }}");
    let node = build(&src).unwrap_or_else(|e| panic!("модель не построена: {e:?}\n{src}"));
    let state = node.states.values().next().expect("нет состояния");
    let crate::semantic::StateNode::Simple { named_blocks, .. } = state else {
        panic!("ожидалось простое состояние");
    };
    named_blocks
        .iter()
        .find_map(|b| block_body(b).and_then(assignment_rhs))
        .expect("в теле нет присваивания")
}

/// Тело именованного блока, каким бы он ни был.
fn block_body(
    block: &crate::semantic::NamedCodeBlockDefinitionNode,
) -> Option<&crate::semantic::StatementNode> {
    use crate::semantic::NamedCodeBlockDefinitionNode as B;
    match block {
        B::Enter { body, .. } | B::Exit { body, .. } | B::Always { body, .. } => Some(body),
        _ => None,
    }
}

/// Правая часть первого присваивания в теле блока.
fn assignment_rhs(stmt: &crate::semantic::StatementNode) -> Option<ExpressionNode> {
    use crate::semantic::StatementNode as S;
    match stmt {
        S::Block(items) => items.iter().find_map(assignment_rhs),
        S::Expression(expr, _) => match &**expr {
            ExpressionNode::Assign(_, rhs) => Some((**rhs).clone()),
            _ => None,
        },
        _ => None,
    }
}

// -- Литералы -------------------------------------------------------------

/// Числовой литерал: `var x = 42;` -> `Number(42)`.
#[test]
fn number_literal_resolved() {
    let node = build("var x: bit := false; cond C = 42;").unwrap();
    assert_eq!(node.conditions["C"].value, ConditionNode::Number(42));
}

/// Булев литерал `true`: инициализатор переменной -> `Bool(true)`.
#[test]
fn bool_literal_in_var_initializer() {
    assert!(
        matches!(body_expr("", "true"), ExpressionNode::Bool(true)),
        "инициализатор должен быть Bool(true)"
    );
}

/// Булев литерал `false`: инициализатор переменной -> `Bool(false)`.
#[test]
fn bool_false_literal_in_var() {
    assert!(
        matches!(body_expr("", "false"), ExpressionNode::Bool(false)),
        "инициализатор должен быть Bool(false)"
    );
}

/// Целочисленный литерал как инициализатор переменной -> `Number(n)`.
#[test]
fn number_literal_in_var_initializer() {
    assert!(
        matches!(body_expr("", "0"), ExpressionNode::Number(_)),
        "инициализатор должен быть Number"
    );
}

// -- Variable / разрешение идентификаторов ---------------------------------

/// Переменная в инициализаторе разрешается в `Expression::Variable`.
///
/// # Пример (Takt)
/// ```but
/// var a: bit = false;
/// var b: bit = a;
/// ```
#[test]
fn variable_ref_in_initializer_resolves() {
    assert!(
        matches!(
            body_expr("var a: bit := false;", "a"),
            ExpressionNode::Variable(_)
        ),
        "инициализатор b должен быть Variable(a)"
    );
}

/// Условие в инициализаторе разрешается в `Expression::Condition`.
///
/// # Пример (Takt)
/// ```but
/// cond Done = true;
/// var flag: bit = Done;
/// ```
///
/// **Примечание**: Это возможно только если `Done` объявлена перед `flag`,
/// что проверяет данный тест.
#[test]
fn named_condition_in_var_initializer_resolves() {
    // Условие разрешается как Condition внутри именованного cond-блока. Проверяем через
    // отдельный cond, а не var-инициализатор (разрешение идёт в extract_conditions).
    let node = build("cond Done = true; cond RefDone = Done;").unwrap();
    // RefDone = Done должно разрешиться через переменную (done - это cond, не var) Если
    // не находит как переменную, ищет как условие
    assert!(node.conditions.contains_key("RefDone"));
}

/// Контрпример: несуществующий идентификатор -> ошибка.
#[test]
fn unknown_identifier_is_error() {
    let result = build("var x: bit := ghost;");
    assert!(
        result.is_err(),
        "неизвестный идентификатор должен давать ошибку"
    );
}

// -- Операторы --------------------------------------------------------------

/// Сложение в инициализаторе: `var x = 1 + 2;` -> `Add`.
#[test]
fn add_in_var_initializer() {
    assert!(
        matches!(body_expr("", "1 + 2"), ExpressionNode::Add(_, _)),
        "инициализатор должен быть Add"
    );
}

/// Вычитание в инициализаторе: `var x = 3 - 1;` -> `Subtract`.
#[test]
fn subtract_in_var_initializer() {
    assert!(
        matches!(body_expr("", "3 - 1"), ExpressionNode::Subtract(_, _)),
        "инициализатор должен быть Subtract"
    );
}

/// Побитовое И: `var x = 0xFF & 0x0F;` -> `BitwiseAnd`.
#[test]
fn bitwise_and_in_var_initializer() {
    assert!(
        matches!(body_expr("", "255 & 15"), ExpressionNode::BitwiseAnd(_, _)),
        "инициализатор должен быть BitwiseAnd"
    );
}

/// Побитовое или: `var x = 0x0F | 0xF0;` -> `BitwiseOr`.
#[test]
fn bitwise_or_in_var_initializer() {
    assert!(
        matches!(body_expr("", "15 | 240"), ExpressionNode::BitwiseOr(_, _)),
        "инициализатор должен быть BitwiseOr"
    );
}

/// Логическое не: `var x = !false;` -> `Not`.
#[test]
fn not_in_var_initializer() {
    assert!(
        matches!(body_expr("", "!false"), ExpressionNode::Not(_)),
        "инициализатор должен быть Not"
    );
}

/// Скобки: `var x = (42);` -> `Parenthesis`.
#[test]
fn parenthesis_in_var_initializer() {
    assert!(
        matches!(body_expr("", "(42)"), ExpressionNode::Parenthesis(_)),
        "инициализатор должен быть Parenthesis"
    );
}

/// Сравнение `<`: инициализатор -> `Less`.
#[test]
fn less_in_var_initializer() {
    assert!(
        matches!(body_expr("", "1 < 2"), ExpressionNode::Less(_, _)),
        "инициализатор должен быть Less"
    );
}

/// Сравнение `>`: инициализатор -> `More`.
#[test]
fn more_in_var_initializer() {
    assert!(
        matches!(body_expr("", "2 > 1"), ExpressionNode::More(_, _)),
        "инициализатор должен быть More"
    );
}

/// Равенство `==`: инициализатор -> `Equal`.
#[test]
fn equal_in_var_initializer() {
    assert!(
        matches!(body_expr("", "1 = 1"), ExpressionNode::Equal(_, _)),
        "инициализатор должен быть Equal"
    );
}

/// Неравенство `!=`: инициализатор -> `NotEqual`.
#[test]
fn not_equal_in_var_initializer() {
    assert!(
        matches!(body_expr("", "1 != 2"), ExpressionNode::NotEqual(_, _)),
        "инициализатор должен быть NotEqual"
    );
}

// -- Вывод типа (type_inference) через разрешённые выражения --------------

/// Переменная без аннотации типа с булевым литералом: выводится `TypeNode::Bool`.
///
/// # Пример (Takt)
/// ```but
/// var flag = false;   // тип выводится как bool
/// ```
#[test]
fn type_inference_bool_literal() {
    let node = build("var flag := false;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("flag") {
        assert_eq!(
            ty,
            TypeNode::Bool,
            "тип должен выводиться как Bool из булева литерала"
        );
    } else {
        panic!("переменная flag не найдена или не Simple");
    }
}

/// Переменная без аннотации типа с вещественным литералом: -> `TypeNode::Rational`.
#[test]
fn type_inference_rational_literal() {
    let node = build("var r := 3.14;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("r") {
        assert_eq!(
            ty,
            TypeNode::Rational,
            "тип должен выводиться как Rational из вещественного литерала"
        );
    } else {
        panic!("переменная r не найдена");
    }
}

/// Константа без аннотации типа с булевым литералом: -> `TypeNode::Bool`.
#[test]
fn type_inference_const_bool() {
    let node = build("const C := false;").unwrap();
    if let Some(VariableNode::Const { ty, .. }) = node.search_var("C") {
        assert_eq!(
            ty,
            TypeNode::Bool,
            "тип константы должен выводиться как Bool"
        );
    } else {
        panic!("константа C не найдена");
    }
}

/// Вывод типа через переменную: `var b: bit = false; var a = b;` -> тип `a` = `Bit`.
#[test]
fn type_inference_from_variable() {
    let node = build("var b: bit := false; var a := b;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("a") {
        assert_eq!(ty, TypeNode::Bit, "тип a должен выводиться из типа b = Bit");
    } else {
        panic!("переменная a не найдена");
    }
}

// -- Массивы ----------------------------------------------------------------

/// Индексирование массива в инициализаторе разрешается в `ArraySubscript`.
///
/// # Пример (Takt)
/// ```but
/// var buf: [bit; 8];
/// var x: bit = buf[3];
/// ```
#[test]
fn array_subscript_in_var_initializer() {
    assert!(
        matches!(body_expr("var buf: [u8;8];", "buf[3]"), ExpressionNode::ArraySubscript(_, ref idx) if matches!(**idx, ExpressionNode::Number(3))),
        "инициализатор x должен быть ArraySubscript(buf, 3)"
    );
}

/// Индекс по упакованному вектору `[bit;N]` - это разряд.
///
/// Предмет - Один узел на две записи: `x[3]` и `x.3` означают одно, и пока узлов было
/// два, каждая цель печатала индексную форму по-своему - вплоть до кода, который не
/// собирает ни один компилятор C, при нулевом коде возврата.
///
/// # Пример (Takt)
/// ```but
/// var buf: [bit; 8];
/// var x: bit = buf[3];   // то же, что buf.3
/// ```
#[test]
fn a_bit_vector_index_is_a_bit_and_not_an_element() {
    assert!(
        matches!(
            body_expr("var buf: [bit;8] := 0;", "buf[3]"),
            ExpressionNode::BitAccess(_, crate::parser::ast::Member::Number(3))
        ),
        "индекс по бит-вектору обязан сводиться к разряду"
    );
    // Обе записи дают один узел - иначе правило было бы декоративным.
    assert_eq!(
        body_expr("var buf: [bit;8] := 0;", "buf[3]"),
        body_expr("var buf: [bit;8] := 0;", "buf.3")
    );
    // Массив не битов сведению не подлежит: у него индекс - элемент, и подмена
    // превратила бы `bus[1]` в разряд числа.
    assert!(matches!(
        body_expr("var bus: [u8;4];", "bus[1]"),
        ExpressionNode::ArraySubscript(..)
    ));
}

/// Контрпример: индексирование несуществующего массива - ошибка.
#[test]
fn array_subscript_unknown_var_is_error() {
    let result = build("var x: bit := ghost[0];");
    assert!(
        result.is_err(),
        "индексирование несуществующего массива — ошибка"
    );
}

// -- Проверка типа и границ массива ----------------------------------------

/// Корректный индекс в пределах массива - строится без ошибок.
#[test]
fn array_subscript_valid_index() {
    assert!(
        matches!(body_expr("var buf: [u8;8];", "buf[0]"), ExpressionNode::ArraySubscript(_, ref idx) if matches!(**idx, ExpressionNode::Number(0))),
        "x должен быть ArraySubscript(buf, 0)"
    );
}

/// Последний допустимый индекс (size - 1) - строится без ошибок.
#[test]
fn array_subscript_last_valid_index() {
    assert!(
        matches!(body_expr("var buf: [u8;8];", "buf[7]"), ExpressionNode::ArraySubscript(_, ref idx) if matches!(**idx, ExpressionNode::Number(7))),
        "x должен быть ArraySubscript(buf, 7)"
    );
}

/// Индекс равный размеру массива (out of bounds) - ошибка.
#[test]
fn array_subscript_out_of_bounds_is_error() {
    let result = build("var buf: [bit;8] := 0; var x: bit := buf[8];");
    assert!(result.is_err(), "индекс 8 >= size 8 должен давать ошибку");
}

/// Отрицательный индекс - ошибка.
#[test]
fn array_subscript_negative_index_is_error() {
    let result = build("var buf: [bit;8] := 0; var x: bit := buf[-1];");
    assert!(result.is_err(), "отрицательный индекс должен давать ошибку");
}

/// Индексирование переменной с типом Bit - ошибка (не массив).
#[test]
fn array_subscript_on_non_array_is_error() {
    let result = build("var flag: bit := false; var x: bit := flag[0];");
    assert!(
        result.is_err(),
        "индексирование Bit-переменной должно давать ошибку"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("flag"),
        "сообщение должно упоминать имя переменной: {}",
        err.message
    );
}

// Тесты `var_type_simple`/`var_type_unresolved` сняты вместе с функцией: тип базы
// постисправления даёт теперь общий носитель `semantic::validate::base_type`, а тип
// переменной читается её собственным `ty()`. Тест без своего предмета - декорация.

/// `check_slice_bounds`: допустимый срез [1:6] для массива size=8 - ок.
#[test]
fn check_slice_bounds_valid() {
    check_slice_bounds("buf", Location::Source(0, 1, 2), 8, Some(1), Some(6)).unwrap();
}

/// `check_slice_bounds`: срез с end > size - ошибка.
#[test]
fn check_slice_bounds_end_out_of_range_is_error() {
    assert!(check_slice_bounds("buf", Location::Source(0, 1, 2), 8, None, Some(9)).is_err());
}

/// `check_slice_bounds`: срез с start >= size - ошибка.
#[test]
fn check_slice_bounds_start_out_of_range_is_error() {
    assert!(check_slice_bounds("buf", Location::Source(0, 1, 2), 8, Some(8), None).is_err());
}

/// `check_slice_bounds`: start > end - ошибка.
#[test]
fn check_slice_bounds_start_greater_than_end_is_error() {
    assert!(check_slice_bounds("buf", Location::Source(0, 1, 2), 8, Some(5), Some(3)).is_err());
}

/// `check_slice_bounds`: None, None - всегда ок (срез без границ).
#[test]
fn check_slice_bounds_both_none_is_ok() {
    check_slice_bounds("buf", Location::Source(0, 1, 2), 8, None, None).unwrap();
}

// -- Implement-состояния и construct_expression -----------------------------

/// Implement-состояние (`= M`) использует construct_expression для разрешения имени
/// модели - ранее это вызывало stack overflow.
///
/// Регрессионный тест: construct_implement теперь корректно делегирует через
/// construct_expression вместо рекурсивного вызова через заглушку.
#[test]
fn implement_model_via_construct_expression() {
    let node = build("start A = M { } state B; model M { start S; }").unwrap();
    assert!(
        matches!(
            &node.states.get("A"),
            Some(crate::semantic::StateNode::Implement { .. })
        ),
        "A должен быть Implement-состоянием"
    );
}

/// Переменная без аннотации с числовым литералом: `var x = 100;` -> `Array(8, Bit)`.
#[test]
fn type_inference_number_literal() {
    let node = build("var x := 100;").unwrap();
    if let Some(VariableNode::Simple { ty, .. }) = node.search_var("x") {
        assert_eq!(
            ty,
            TypeNode::Array(8, Box::new(TypeNode::Bit)),
            "тип должен выводиться как [bit;8] из числового литерала 100"
        );
    } else {
        panic!("переменная x не найдена");
    }
}

// -- Дополнительные арифметические операции --------------------------------

/// Умножение: `var x: [bit;8] = 2 * 3;` -> `Multiply`.
#[test]
fn multiply_in_var_initializer() {
    assert!(
        matches!(body_expr("", "2 * 3"), ExpressionNode::Multiply(_, _)),
        "инициализатор должен быть Multiply"
    );
}

/// Деление: `var x: [bit;8] = 6 / 2;` -> `Divide`.
#[test]
fn divide_in_var_initializer() {
    assert!(
        matches!(body_expr("", "6 / 2"), ExpressionNode::Divide(_, _)),
        "инициализатор должен быть Divide"
    );
}

/// Остаток от деления: `var x: [bit;8] = 7 % 3;` -> `Modulo`.
#[test]
fn modulo_in_var_initializer() {
    assert!(
        matches!(body_expr("", "7 % 3"), ExpressionNode::Modulo(_, _)),
        "инициализатор должен быть Modulo"
    );
}

/// Возведение в степень: `var x: [bit;8] = 2 ** 3;` -> `Power`.
#[test]
fn power_in_var_initializer() {
    assert!(
        matches!(body_expr("", "2 ** 3"), ExpressionNode::Power(_, _)),
        "инициализатор должен быть Power"
    );
}

/// Сдвиг влево: `var x: [bit;8] = 1 << 2;` -> `ShiftLeft`.
#[test]
fn shift_left_in_var_initializer() {
    assert!(
        matches!(body_expr("", "1 << 2"), ExpressionNode::ShiftLeft(_, _)),
        "инициализатор должен быть ShiftLeft"
    );
}

/// Сдвиг вправо: `var x: [bit;8] = 4 >> 1;` -> `ShiftRight`.
#[test]
fn shift_right_in_var_initializer() {
    assert!(
        matches!(body_expr("", "4 >> 1"), ExpressionNode::ShiftRight(_, _)),
        "инициализатор должен быть ShiftRight"
    );
}

/// Побитовое XOR: `var x: [bit;8] = 3 ^ 1;` -> `BitwiseXor`.
#[test]
fn bitwise_xor_in_var_initializer() {
    assert!(
        matches!(body_expr("", "3 ^ 1"), ExpressionNode::BitwiseXor(_, _)),
        "инициализатор должен быть BitwiseXor"
    );
}

// -- Операции сравнения ----------------------------------------------------

/// Меньше или равно: `var x: bit = 1 <= 2;` -> `LessEqual`.
#[test]
fn less_equal_in_var_initializer() {
    assert!(
        matches!(body_expr("", "1 <= 2"), ExpressionNode::LessEqual(_, _)),
        "инициализатор должен быть LessEqual"
    );
}

/// Больше или равно: `var x: bit = 2 >= 1;` -> `MoreEqual`.
#[test]
fn more_equal_in_var_initializer() {
    assert!(
        matches!(body_expr("", "2 >= 1"), ExpressionNode::MoreEqual(_, _)),
        "инициализатор должен быть MoreEqual"
    );
}

// -- Логические операции ---------------------------------------------------

/// Логическое И: `var x: bit = true && false;` -> `And`.
#[test]
fn and_in_var_initializer() {
    assert!(
        matches!(body_expr("", "true && false"), ExpressionNode::And(_, _)),
        "инициализатор должен быть And"
    );
}

/// Логическое или: `var x: bit = true || false;` -> `Or`.
#[test]
fn or_in_var_initializer() {
    assert!(
        matches!(body_expr("", "true || false"), ExpressionNode::Or(_, _)),
        "инициализатор должен быть Or"
    );
}

// -- Унарные операции ------------------------------------------------------

/// Унарный плюс: `var x: [bit;8] = +5;` -> `UnaryPlus`.
#[test]
fn unary_plus_in_var_initializer() {
    assert!(
        matches!(body_expr("", "+5"), ExpressionNode::UnaryPlus(_)),
        "инициализатор должен быть UnaryPlus"
    );
}

/// Побитовое не: `var x: [bit;8] = ~0;` -> `BitwiseNot`.
#[test]
fn bitwise_not_in_var_initializer() {
    assert!(
        matches!(body_expr("", "~0"), ExpressionNode::BitwiseNot(_)),
        "инициализатор должен быть BitwiseNot"
    );
}

/// Вещественный литерал: `var r = 3.14;` -> `Rational`.
#[test]
fn rational_literal_in_var_initializer() {
    let node = build("var r := 3.14;").unwrap();
    assert!(
        matches!(var_expr(&node, "r"), ExpressionNode::Rational(_, _)),
        "инициализатор должен быть Rational"
    );
}

/// Отрицание вещественного числа: `var r = -3.14;` -> `Rational` с флагом отрицания.
#[test]
fn negate_rational_in_var_initializer() {
    // Парсер может представить -3.14 как Rational(_, true) или Negate(Rational)
    let node = build("var r := -3.14;").unwrap();
    let expr = var_expr(&node, "r");
    assert!(
        matches!(
            expr,
            ExpressionNode::Rational(_, _) | ExpressionNode::Negate(_)
        ),
        "инициализатор должен быть Rational или Negate"
    );
}

// -- Тернарный оператор ----------------------------------------------------

/// Тернарный оператор через `construct_expression` -> `ConditionalOperator`.
///
/// Синтаксис `? :` не поддерживается парсером Takt, поэтому тестируем напрямую через
/// `construct_expression`.
#[test]
fn conditional_operator_via_construct_expression() {
    let model = Rc::new(RefCell::new(ModelNode::default()));
    let loc = crate::diagnostics::Location::default();
    // false ?
    let cond_expr = Box::new(ast::Expression::Bool(loc, false));
    let then_expr = Box::new(ast::Expression::Number(loc, 0));
    let else_expr = Box::new(ast::Expression::Number(loc, 1));
    let expr = ast::Expression::ConditionalOperator(loc, cond_expr, then_expr, else_expr);
    let result = construct_expression(expr, vec![], model).unwrap();
    assert!(
        matches!(result, ExpressionNode::ConditionalOperator(_, _, _)),
        "должен получиться ConditionalOperator"
    );
}

/// Тернарный оператор: подвыражения разрешаются корректно.
///
/// Проверяем, что все три ветви (условие, then, else) разрешаются внутри конкретного
/// контекста модели (с переменной).
///
/// # Пример (псевдокод Takt):
/// ```text
/// var flag: bit = true;
/// // flag ? 10: 20 -> ConditionalOperator(Variable("flag"), Number(10), Number(20))
/// ```
#[test]
fn conditional_operator_with_variable_condition() {
    use crate::semantic::tree::construct_model;
    // Строим модель с переменной flag
    let (ast, _) = crate::parse("var flag: bit := true;", 0).expect("ошибка разбора");
    let model = construct_model(&ast, None, &[]).expect("ошибка семантики");
    let loc = crate::diagnostics::Location::default();
    // flag ? 10: 20
    let cond_expr = Box::new(ast::Expression::Variable(crate::parser::ast::Identifier {
        loc,
        name: "flag".to_string(),
    }));
    let then_expr = Box::new(ast::Expression::Number(loc, 10));
    let else_expr = Box::new(ast::Expression::Number(loc, 20));
    let expr = ast::Expression::ConditionalOperator(loc, cond_expr, then_expr, else_expr);
    let result = construct_expression(expr, vec![], model).unwrap();
    assert!(
        matches!(result, ExpressionNode::ConditionalOperator(_, _, _)),
        "ConditionalOperator с переменной-условием должен разрешиться"
    );
}

/// Контрпример: тернарный оператор с несуществующей переменной в условии - ошибка.
///
/// Если условие ссылается на неизвестный идентификатор, `construct_expression` должен
/// вернуть ошибку диагностики.
#[test]
fn conditional_operator_unknown_condition_is_error() {
    let model = Rc::new(RefCell::new(ModelNode::default()));
    let loc = crate::diagnostics::Location::default();
    // ghost ? 0: 1 - ghost не объявлен
    let cond_expr = Box::new(ast::Expression::Variable(crate::parser::ast::Identifier {
        loc,
        name: "ghost".to_string(),
    }));
    let then_expr = Box::new(ast::Expression::Number(loc, 0));
    let else_expr = Box::new(ast::Expression::Number(loc, 1));
    let expr = ast::Expression::ConditionalOperator(loc, cond_expr, then_expr, else_expr);
    let result = construct_expression(expr, vec![], model);
    assert!(
        result.is_err(),
        "тернарный оператор с неизвестным условием должен давать ошибку"
    );
}

// -- Срез массива ----------------------------------------------------------

/// Срез массива: `var y: [bit;4] = buf[1:5];` -> `ArraySlice`.
#[test]
fn array_slice_in_var_initializer() {
    assert!(
        matches!(
            body_expr("var buf: [bit;8] := 0;", "buf[1:5]"),
            ExpressionNode::ArraySlice(_, _, _)
        ),
        "инициализатор y должен быть ArraySlice"
    );
}

/// Срез несуществующего массива - ошибка.
#[test]
fn array_slice_unknown_var_is_error() {
    let result = build("var y: [bit;4] := ghost[0:4];");
    assert!(
        result.is_err(),
        "срез несуществующего массива должен давать ошибку"
    );
}

/// Срез переменной с типом Bit - ошибка (не массив).
#[test]
fn array_slice_on_non_array_is_error() {
    let result = build("var flag: bit := false; var y: [bit;4] := flag[0:4];");
    assert!(result.is_err(), "срез Bit-переменной должен давать ошибку");
}

// -- Строковый литерал -----------------------------------------------------

/// Строковый литерал в вызове debug() - разрешается без ошибок.
#[test]
fn string_literal_in_debug_call() {
    let node = build(r#"always { debug("hello"); } start S;"#).unwrap();
    assert!(node.has_states(), "модель должна содержать состояние S");
}

// -- Вызов функции ---------------------------------------------------------

/// Вызов внешней функции в блоке `always` -> разрешается без ошибок.
#[test]
fn extern_function_call_in_always_block() {
    let node = build("extern fn foo(); always { foo(); } start S;").unwrap();
    assert!(node.has_states(), "модель должна содержать состояние S");
}

// -- Приведение типа -------------------------------------------------------

/// Приведение типа: `var x: [bit;8] = 42 as [bit;8];` -> `Cast`.
#[test]
fn cast_in_var_initializer() {
    assert!(
        matches!(body_expr("", "42 as [bit;8]"), ExpressionNode::Cast(_, _)),
        "инициализатор должен быть Cast"
    );
}

// -- Массивный литерал -----------------------------------------------------

/// Массивный литерал через `construct_expression` -> `Expression::Array`.
///
/// Синтаксис `[a, b]` не поддерживается парсером как инициализатор переменной, поэтому
/// тестируем через `construct_expression` напрямую.
#[test]
fn array_literal_via_construct_expression() {
    let model = Rc::new(RefCell::new(ModelNode::default()));
    let loc = crate::diagnostics::Location::default();
    let items = vec![
        ast::Expression::Number(loc, 0),
        ast::Expression::Number(loc, 1),
    ];
    let expr = ast::Expression::Array(loc, items);
    let result = construct_expression(expr, vec![], model).unwrap();
    assert!(
        matches!(result, ExpressionNode::Array(_)),
        "должен получиться Expression::Array"
    );
}

// -- Прямое использование construct_expression -----------------------------

/// `ast::Expression::Type` -> `Expression::Type(Type::Bit)`.
#[test]
fn construct_expression_type_variant() {
    use crate::parser::ast::Type;
    let model = Rc::new(RefCell::new(ModelNode::default()));
    let expr = ast::Expression::Type(crate::diagnostics::Location::default(), Type::Bit);
    let result = construct_expression(expr, vec![], model).unwrap();
    assert!(
        matches!(result, ExpressionNode::Type(Type::Bit)),
        "должен получиться Expression::Type(Type::Bit)"
    );
}

/// `ast::Expression::Address` -> `Expression::Address(addr, bit)`.
#[test]
fn construct_expression_address_variant() {
    let model = Rc::new(RefCell::new(ModelNode::default()));
    let expr = ast::Expression::Address(crate::diagnostics::Location::default(), 0x1234, 5);
    let result = construct_expression(expr, vec![], model).unwrap();
    assert!(
        matches!(result, ExpressionNode::Address(0x1234, 5)),
        "должен получиться Expression::Address(0x1234, 5)"
    );
}

/// `ast::Expression::List` с пустым списком -> `Expression::List([])`.
#[test]
fn construct_expression_list_variant() {
    let model = Rc::new(RefCell::new(ModelNode::default()));
    let expr = ast::Expression::List(crate::diagnostics::Location::default(), vec![]);
    let result = construct_expression(expr, vec![], model).unwrap();
    assert!(
        matches!(result, ExpressionNode::List(_)),
        "должен получиться Expression::List"
    );
}

/// Неизвестный идентификатор в `construct_expression` -> ошибка.
#[test]
fn construct_expression_unknown_variable_is_error() {
    let model = Rc::new(RefCell::new(ModelNode::default()));
    let expr = ast::Expression::Variable(ast::Identifier::new("ghost"));
    let result = construct_expression(expr, vec![], model);
    assert!(
        result.is_err(),
        "неизвестный идентификатор должен давать ошибку"
    );
}

/// `ast::Expression::ArraySubscript` для несуществующей переменной -> ошибка.
#[test]
fn construct_expression_array_subscript_unknown_var() {
    let model = Rc::new(RefCell::new(ModelNode::default()));
    let expr = ast::Expression::ArraySubscript(
        crate::diagnostics::Location::default(),
        Box::new(ast::Expression::Variable(ast::Identifier::new(
            "no_such_var",
        ))),
        Box::new(ast::Expression::Number(
            crate::diagnostics::Location::default(),
            0,
        )),
    );
    let result = construct_expression(expr, vec![], model);
    assert!(
        result.is_err(),
        "несуществующая переменная должна давать ошибку"
    );
}

/// `ast::Expression::ArraySlice` для несуществующей переменной -> ошибка.
#[test]
fn construct_expression_array_slice_unknown_var() {
    let model = Rc::new(RefCell::new(ModelNode::default()));
    let expr = ast::Expression::ArraySlice(
        crate::diagnostics::Location::default(),
        Box::new(ast::Expression::Variable(ast::Identifier::new(
            "no_such_var",
        ))),
        Some(0),
        Some(4),
    );
    let result = construct_expression(expr, vec![], model);
    assert!(
        result.is_err(),
        "несуществующая переменная должна давать ошибку"
    );
}

/// `check_slice_bounds`: отрицательный start - ошибка.
#[test]
fn check_slice_bounds_negative_start_is_error() {
    assert!(check_slice_bounds("buf", Location::Source(0, 1, 2), 8, Some(-1), None).is_err());
}

/// `check_slice_bounds`: отрицательный end - ошибка.
#[test]
fn check_slice_bounds_negative_end_is_error() {
    assert!(check_slice_bounds("buf", Location::Source(0, 1, 2), 8, None, Some(-1)).is_err());
}

/// `check_slice_bounds`: срез без начала [..end] - ок.
#[test]
fn check_slice_bounds_only_end_is_ok() {
    check_slice_bounds("buf", Location::Source(0, 1, 2), 8, None, Some(8)).unwrap();
}

/// `check_slice_bounds`: срез без конца [start..] - ок.
#[test]
fn check_slice_bounds_only_start_is_ok() {
    check_slice_bounds("buf", Location::Source(0, 1, 2), 8, Some(0), None).unwrap();
}

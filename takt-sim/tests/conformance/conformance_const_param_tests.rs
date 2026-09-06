//! Сверка константного вычислителя компилятора с эталоном -,.
//!
//! # Как устроено
//!
//! Модель объявляет функцию и вызывает её в `always`, кладя результат в
//! переменную: её значение - эталон. То же выражение вычисляется компилятором
//! **до** всякого исполнения. Сравниваются числа.

use std::cell::RefCell;
use std::rc::Rc;
use takt_lang::parser::ast;
use takt_lang::semantic::ModelNode;
use takt_lang::semantic::const_eval::{Budget, eval};
use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Value, build_unit};

/// Строит дерево семантики.
fn model_of(src: &str) -> Rc<RefCell<ModelNode>> {
    let (tree, _) = takt_lang::parse(src, 0).expect("разбор");
    construct_model(&tree, None, &[]).expect("семантика")
}

/// Значение переменной после одного такта - эталон симулятора.
fn simulated(src: &str, name: &str) -> i128 {
    let mut unit = build_unit(model_of(src)).expect("построение юнита");
    match unit.tick() {
        TickResult::Processing | TickResult::Terminated => {}
        other => panic!("такт завершился неожиданно: {other:?}"),
    }
    match unit.variable(name) {
        Some(Value::Number(n)) => n,
        Some(Value::Boolean(b)) => i128::from(b),
        other => panic!("переменная '{name}': неожиданное значение {other:?}"),
    }
}

/// То же выражение, вычисленное **компилятором**.
fn compiled(src: &str, expr_src: &str) -> i128 {
    let model = model_of(src);
    let probe = format!("const PROBE := {expr_src};\nstart S;\n");
    let (probe_tree, _) = takt_lang::parse(&probe, 0).expect("разбор пробы");
    let expr = probe_tree
        .elements
        .iter()
        .find_map(|element| match element {
            ast::ModelElement::Variable(def) => match def.as_ref() {
                ast::VariableDefine::Constant { initializer, .. } => Some(initializer.clone()),
                _ => None,
            },
            _ => None,
        })
        .expect("проба обязана содержать константу");
    let mut budget = Budget::new();
    eval(&expr, &model, &mut budget)
        .unwrap_or_else(|d| panic!("компилятор не вычислил '{expr_src}': {}", d.message))
        .as_int()
        .unwrap_or_else(|| panic!("значение '{expr_src}' не целое"))
}

/// Проверяет, что компилятор и симулятор согласны по значению.
fn assert_agree(src: &str, expr_src: &str) {
    let by_compiler = compiled(src, expr_src);
    let by_simulator = simulated(src, "result");
    assert_eq!(
        by_compiler, by_simulator,
        "компилятор и симулятор разошлись на '{expr_src}': {by_compiler} против {by_simulator}"
    );
}

/// Модель с константной функцией и её вызовом в такте.
///
/// `body` - тело функции, `call` - вызов, попадающий и в такт, и в пробу.
fn source(body: &str, call: &str) -> String {
    format!(
        "const U: u8 := 3;\n\
         fn compute(x: u8) -> u8 {{ {body} }}\n\
         var result: u8 := 0;\n\
         start S {{\n\
         \x20   always {{ result := {call}; }}\n\
         }}\n"
    )
}

/// Арифметика: приоритеты и целочисленное деление совпадают у обеих реализаций.
#[test]
fn arithmetic_agrees_with_the_reference() {
    let src = source("return x * 2 + 1;", "compute(U + 67)");
    assert_agree(&src, "compute(U + 67)");
}

/// Деление и остаток: усечение к нулю - там, где реализации легче всего разойтись (в C
/// деление знакового усекается к нулю, в других языках - нет).
#[test]
fn division_agrees_with_the_reference() {
    let src = source("return x / 3 + x % 3;", "compute(10)");
    assert_agree(&src, "compute(10)");
}

/// Ветвление: обе реализации выбирают одну ветвь.
#[test]
fn branching_agrees_with_the_reference() {
    let src = source("if x > 10 { return 1; } else { return 2; }", "compute(11)");
    assert_agree(&src, "compute(11)");
}

/// Цикл с накоплением: у компилятора он под бюджетом шагов, у симулятора - нет, и
/// значение обязано совпасть при обоих устройствах.
#[test]
fn loop_agrees_with_the_reference() {
    let body = "var acc: u8 := 0; var i: u8 := 0; \
                while i < 3 { acc := acc + x; i := i + 1; } return acc;";
    let src = source(body, "compute(7)");
    assert_agree(&src, "compute(7)");
}

/// Побитовые операции и сдвиг.
#[test]
fn bitwise_agrees_with_the_reference() {
    let src = source("return (x << 2) & 15;", "compute(5)");
    assert_agree(&src, "compute(5)");
}

/// Вложенный вызов: функция зовёт функцию.
#[test]
fn nested_call_agrees_with_the_reference() {
    let src = "const U: u8 := 3;\n\
               fn twice(x: u8) -> u8 { return x * 2; }\n\
               fn compute(x: u8) -> u8 { return twice(x) + 1; }\n\
               var result: u8 := 0;\n\
               start S {\n\
               \x20   always { result := compute(U + 4); }\n\
               }\n";
    assert_agree(src, "compute(U + 4)");
}

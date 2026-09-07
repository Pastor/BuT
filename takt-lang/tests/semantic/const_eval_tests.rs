//! Константный вычислитель,.
//!
//! Проверяются **значения** (а не факт отсутствия ошибки) и **причины отказа**:
//! выражение, которое компилятор не может вычислить, обязано объяснить, почему -
//! молчаливый ноль в настройке автомата дороже отказа (образец - `SE-072` ).
//!
//! Сверку с эталоном (`takt-sim/src/eval/`) держит
//! `takt-sim/tests/conformance_const_param_tests.rs`: это **вторая** реализация одного
//! смысла в проекте, и разойтись она может только молча.

use std::cell::RefCell;
use std::rc::Rc;
use takt_lang::parse;
use takt_lang::semantic::ModelNode;
use takt_lang::semantic::const_eval::{Budget, ConstValue, eval};
use takt_lang::semantic::tree::construct_model;

/// Строит дерево модели.
///
/// Модели здесь **без аргументов инстанцирования**: вычислитель проверяется на
/// выражениях, а не на их применении, поэтому временный тест `SE-082` в этих тестах
/// не срабатывает.
fn model_of(src: &str) -> Rc<RefCell<ModelNode>> {
    let (tree, _) = parse(src, 0).expect("исходник должен разбираться");
    construct_model(&tree, None, &[]).expect("дерево модели")
}

/// Вычисляет выражение в области видимости модели.
fn value_of(src: &str, expr_src: &str) -> Result<ConstValue, String> {
    let model = model_of(src);
    // Выражение разбирается как инициализатор константы отдельного файла - дешёвый
    // способ получить `ast::Expression` без публичного разбора выражений.
    let probe = format!("const PROBE := {expr_src};\nstart S;\n");
    let (probe_tree, _) = parse(&probe, 0).expect("проба должна разбираться");
    let expr = probe_tree
        .elements
        .iter()
        .find_map(|element| match element {
            takt_lang::parser::ast::ModelElement::Variable(def) => match def.as_ref() {
                takt_lang::parser::ast::VariableDefine::Constant { initializer, .. } => {
                    Some(initializer.clone())
                }
                _ => None,
            },
            _ => None,
        })
        .expect("проба обязана содержать константу");
    let mut budget = Budget::new();
    eval(&expr, &model, &mut budget).map_err(|d| {
        format!(
            "{}|{}",
            d.code.clone().unwrap_or_default(),
            d.message.clone()
        )
    })
}

/// Модель с константами и константной функцией.
const SRC: &str = "const BASE: u8 := 60;\n\
                   const U: u8 := 3;\n\
                   const DWELL: duration := 1s;\n\
                   fn twice_plus_one(x: u8) -> u8 { return x * 2 + 1; }\n\
                   fn sum_three(x: u8) -> u8 {\n\
                   \x20   var acc: u8 := 0;\n\
                   \x20   var i: u8 := 0;\n\
                   \x20   while i < 3 { acc := acc + x; i := i + 1; }\n\
                   \x20   return acc;\n\
                   }\n\
                   fn conditional(x: u8) -> u8 {\n\
                   \x20   if x > 10 { return 1; } else { return 2; }\n\
                   }\n\
                   fn reads_variable() -> u8 { return counter; }\n\
                   extern fn read_adc() -> u8;\n\
                   fn calls_extern() -> u8 { return read_adc(); }\n\
                   fn forever(x: u8) -> u8 { loop { x := x + 1; } }\n\
                   var counter: u8 := 0;\n\
                   start S;\n";

// --- Значения ----------------------------------------------------------------

/// Литералы и арифметика над целыми.
#[test]
fn integer_arithmetic() {
    for (expr, expected) in [
        ("1", 1),
        ("2 + 3 * 4", 14),
        ("(2 + 3) * 4", 20),
        ("10 / 3", 3),
        ("10 % 3", 1),
        ("1 << 4", 16),
        ("255 & 15", 15),
        ("-5 + 2", -3),
    ] {
        assert_eq!(
            value_of(SRC, expr).map(|v| v.as_int()),
            Ok(Some(expected)),
            "выражение {expr}"
        );
    }
}

/// Имя константы и цепочка констант (`BASE + 7`).
#[test]
fn constant_names_are_resolved() {
    assert_eq!(value_of(SRC, "BASE + 7").map(|v| v.as_int()), Ok(Some(67)));
}

/// Длительности складываются между собой (наносекунды).
#[test]
fn durations_add_up() {
    assert_eq!(
        value_of(SRC, "2s + 500ms").map(|v| v.as_nanos()),
        Ok(Some(2_500_000_000))
    );
    assert_eq!(
        value_of(SRC, "DWELL + 1s").map(|v| v.as_nanos()),
        Ok(Some(2_000_000_000))
    );
}

/// Длительность и число не смешиваются: в языке это ошибка (`SE-065`), и вычислитель не
/// вправе быть "умнее" языка.
#[test]
fn duration_and_number_do_not_mix() {
    let err = value_of(SRC, "1s + 1").expect_err("смешение обязано отвергаться");
    assert!(
        err.starts_with("SE-083"),
        "ожидался SE-083, получено: {err}"
    );
    assert!(
        err.contains("разных видов"),
        "причина обязана быть названа: {err}"
    );
}

// --- Константные функции (уточнение 4 ) -----------------------------

/// Тело из одного `return` - вычисляется.
#[test]
fn function_with_return_is_evaluated() {
    assert_eq!(
        value_of(SRC, "twice_plus_one(U + 67)").map(|v| v.as_int()),
        Ok(Some(141))
    );
}

/// Локальные значения, присваивание и цикл - вычисляются под бюджетом шагов.
#[test]
fn function_with_loop_is_evaluated() {
    assert_eq!(
        value_of(SRC, "sum_three(7)").map(|v| v.as_int()),
        Ok(Some(21))
    );
}

/// Ветвление - вычисляется в обе стороны.
#[test]
fn function_with_branch_is_evaluated() {
    assert_eq!(
        value_of(SRC, "conditional(11)").map(|v| v.as_int()),
        Ok(Some(1))
    );
    assert_eq!(
        value_of(SRC, "conditional(1)").map(|v| v.as_int()),
        Ok(Some(2))
    );
}

// --- Отказы с названной причиной ---------------------------------------------

/// Функция читает переменную модели - значение известно лишь в такте.
#[test]
fn function_reading_a_variable_is_rejected() {
    let err = value_of(SRC, "reads_variable()").expect_err("обязана отвергаться");
    assert!(
        err.contains("переменная"),
        "причина обязана назвать переменную: {err}"
    );
}

/// Функция зовёт `extern fn` - значение даёт внешний код во время работы.
#[test]
fn function_calling_extern_is_rejected() {
    let err = value_of(SRC, "calls_extern()").expect_err("обязана отвергаться");
    assert!(
        err.starts_with("SE-084") && err.contains("extern"),
        "ожидался SE-084 с упоминанием extern, получено: {err}"
    );
}

/// Незавершаемое тело упирается в предел шагов, а не вешает компилятор (и LSP, который
/// зовёт ту же семантику при каждом нажатии).
#[test]
fn nonterminating_function_hits_the_step_limit() {
    let err = value_of(SRC, "forever(0)").expect_err("обязана отвергаться");
    assert!(
        err.starts_with("SE-085"),
        "ожидался SE-085 (предел вычисления), получено: {err}"
    );
}

/// Переменная в выражении - отказ с указанием, что значение известно лишь в такте.
#[test]
fn variable_in_expression_is_rejected() {
    let err = value_of(SRC, "counter + 1").expect_err("обязана отвергаться");
    assert!(
        err.starts_with("SE-083") && err.contains("такте"),
        "ожидался SE-083 про такт, получено: {err}"
    );
}

/// **Точная** арифметика над дробными вычисляется: сложение,
/// вычитание и умножение десятичных литералов округления не требуют.
///
/// Прежде отвергалась **любая** дробная арифметика, и довод был верен лишь наполовину:
/// "представление выбирают флаги, округление q задано эталоном" - это про округление.
#[test]
fn exact_rational_arithmetic_is_folded() {
    let value = value_of(SRC, "0.8 * 2.0").expect("точное произведение обязано вычисляться");
    assert_eq!(value, ConstValue::Rational("1.60".to_string(), false));
}

/// Смешение с целым точно так же точно - отвергать за форму записи не за что.
#[test]
fn mixed_integer_and_rational_is_folded() {
    let value = value_of(SRC, "1 + 3.14").expect("смешанная запись обязана вычисляться");
    assert_eq!(value, ConstValue::Rational("4.14".to_string(), false));
}

/// А **деление** отвергается с причиной: в десятичной записи оно не представимо, а
/// выбор округления - та самая часть, что задана эталоном.
#[test]
fn rational_division_is_rejected_with_the_reason() {
    let err = value_of(SRC, "1.0 / 3.0").expect_err("деление обязано отвергаться");
    assert!(
        err.contains("представление") && err.contains("флаги сборки"),
        "причина обязана объяснять, почему это не пробел: {err}"
    );
}

/// А сам дробный литерал проходит насквозь - значение доносится как записано.
#[test]
fn rational_literal_passes_through() {
    let value = value_of(SRC, "0.8").expect("литерал обязан вычисляться");
    assert_eq!(value, ConstValue::Rational("0.8".to_string(), false));
}

/// Цикл определений упирается в предел глубины, а не зацикливается.
#[test]
fn constant_cycle_hits_the_depth_limit() {
    let src = "const A: u8 := B;\nconst B: u8 := A;\nstart S;\n";
    let err = value_of(src, "A + 1").expect_err("цикл обязан отвергаться");
    assert!(
        err.starts_with("SE-085"),
        "ожидался SE-085 (предел глубины), получено: {err}"
    );
}

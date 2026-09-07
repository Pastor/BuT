//! Аргументы инстанцирования модели `M(ИМЯ := ЗНАЧЕНИЕ)`,.
//!
//! Проверяется **разбор и структурные диагностики**: форма аргумента, существование
//! параметра, отсутствие повторов, все позиции инстанцирования. Вычисление значения -
//! (`const_eval_tests`), применение - (`model_parameter_apply_tests` и сверки в
//! `takt-sim`).
//!
//! Тест снят вместе с применением значений.

use takt_lang::parse;
use takt_lang::semantic::tree::construct_model;

/// Код диагностики для исходника, который обязан быть отвергнут.
fn error_code(src: &str) -> String {
    let (tree, _) = parse(src, 0).expect("исходник должен разбираться");
    let diagnostic = construct_model(&tree, None, &[]).expect_err("ожидалась семантическая ошибка");
    diagnostic
        .code
        .clone()
        .unwrap_or_else(|| panic!("диагностика без кода: {}", diagnostic.message))
}

/// Модель с одним параметром и одной переменной.
const TUNER: &str = "model Tuner {\n\
                     \x20   parameter limit: u8 := 100;\n\
                     \x20   var acc: u8 := 0;\n\
                     \x20   start Count;\n\
                     }\n";

// --- Структурные диагностики  --------------------------------------------

/// Аргумент вне формы `имя := значение` - `SE-076`. Позиционных аргументов нет: у
/// параметров есть значения по умолчанию, поэтому позиция ничего не означает.
#[test]
fn positional_argument_is_se076() {
    assert_eq!(
        error_code(&format!("{TUNER}start Main = Tuner(5);\n")),
        "SE-076"
    );
}

/// Слева от `:=` - не имя: `SE-076` же (та же ошибка формы).
#[test]
fn non_name_on_the_left_is_se076() {
    assert_eq!(
        error_code(&format!("{TUNER}start Main = Tuner(1 := 5);\n")),
        "SE-076"
    );
}

/// Модель не объявляет параметров вовсе - `SE-077`. Отдельный код от "нет такого
/// параметра": автор скорее перепутал модель, чем имя.
#[test]
fn model_without_parameters_is_se077() {
    let src = "model Plain { var acc: u8 := 0; start Count; }\n\
               start Main = Plain(limit := 5);\n";
    assert_eq!(error_code(src), "SE-077");
}

/// У модели есть параметры, но не этот - `SE-078`; сообщение перечисляет объявленные
/// (опечатку видно сразу).
#[test]
fn unknown_parameter_is_se078() {
    assert_eq!(
        error_code(&format!("{TUNER}start Main = Tuner(step := 5);\n")),
        "SE-078"
    );
}

/// Имя объявлено, но это переменная - `SE-079`: при инстанцировании задаются только
/// параметры (граница "настройка сборки vs величина такта").
#[test]
fn assigning_a_plain_variable_is_se079() {
    assert_eq!(
        error_code(&format!("{TUNER}start Main = Tuner(acc := 5);\n")),
        "SE-079"
    );
}

/// Повтор имени в одном вызове - `SE-080`.
#[test]
fn duplicate_argument_is_se080() {
    assert_eq!(
        error_code(&format!(
            "{TUNER}start Main = Tuner(limit := 5, limit := 6);\n"
        )),
        "SE-080"
    );
}

/// Неизвестная модель в позиции инстанцирования - прежний `SE-001`.
#[test]
fn unknown_model_is_se001() {
    assert_eq!(
        error_code(&format!("{TUNER}start Main = Missing(limit := 5);\n")),
        "SE-001"
    );
}

/// Форма, реализацией быть не могущая, получила **код и позицию** (`SE-081`).
#[test]
fn unsupported_implementation_form_is_se081() {
    assert_eq!(
        error_code(&format!("{TUNER}start Main = Tuner * Tuner;\n")),
        "SE-081"
    );
}

// --- Все позиции инстанцирования  ----------------------------------------

/// Разбор доходит до дерева во **всех** позициях: корень, композиции `+`/`|`,
/// реализация состояния, скобки - модель строится без ошибок в каждой.
#[test]
fn arguments_are_parsed_in_every_instantiation_position() {
    let cases = [
        (
            "корень",
            format!("{TUNER}start Main = Tuner(limit := 1);\n"),
        ),
        (
            "последовательная композиция",
            format!("{TUNER}start Main = Tuner(limit := 1) + Tuner;\n"),
        ),
        (
            "параллельная композиция",
            format!("{TUNER}start Main = Tuner | Tuner(limit := 2);\n"),
        ),
        (
            "реализация состояния",
            format!("{TUNER}model Host {{ start B = Tuner(limit := 3); }}\nstart Main = Host;\n"),
        ),
        (
            "скобки",
            format!("{TUNER}start Main = (Tuner(limit := 4));\n"),
        ),
    ];
    for (what, src) in cases {
        let (tree, _) = parse(&src, 0).expect("разбор");
        construct_model(&tree, None, &[]).unwrap_or_else(|e| {
            panic!("позиция «{what}»: модель обязана строиться: {}", e.message)
        });
    }
}

/// Вызов без аргументов ведёт себя как прежде - обратная совместимость.
#[test]
fn call_without_arguments_still_builds() {
    let src = format!("{TUNER}start Main = Tuner;\n");
    let (tree, _) = parse(&src, 0).expect("разбор");
    construct_model(&tree, None, &[]).expect("модель без аргументов обязана строиться");
}

// --- Значение вычислено и лежит в дереве -------------------------------------

/// Аргумент в дереве - уже **вычисленный литерал**, а не сырое выражение: за границей
/// семантики выражения аргумента не существует.
#[test]
fn argument_value_is_folded_to_a_literal() {
    let src = format!("{TUNER}const BASE: u8 := 60;\nstart Main = Tuner(limit := BASE + 7);\n");
    let (tree, _) = parse(&src, 0).expect("разбор");
    let model = construct_model(&tree, None, &[]).expect("модель строится");
    let takt_lang::semantic::StateNode::Implement { implements, .. } =
        model.borrow().states["Main"].clone()
    else {
        panic!("корень обязан быть состоянием-реализацией");
    };
    let takt_lang::semantic::extend::Extend::Model(_, _, args) = implements else {
        panic!("реализация обязана быть инстанцированием модели");
    };
    assert_eq!(args.len(), 1);
    assert_eq!(args[0].name, "limit");
    assert_eq!(
        args[0].value,
        takt_lang::semantic::ExpressionNode::Number(67),
        "значение обязано быть вычислено: BASE + 7 = 67"
    );
}

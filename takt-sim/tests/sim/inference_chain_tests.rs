//! Эталон исполняет объявление, тип которого выведен по ссылке.

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

/// Цепочка `x -> y`: `y` наследует тип `x`, тело накапливает.
const CHAIN: &str = "var x := 42;\n\
                     var y := x;\n\
                     start S { always { y := y + 1; } ref S; }\n";

/// Выдержка **ссылкой** на другую константу.
///
/// Модель обёрнута в `model`, потому что объявление `clock` - её элемент.
const DWELL_REF: &str = "model Chain {\n\
                         \x20   clock 1kHz;\n\
                         \x20   const BASE := 3ms;\n\
                         \x20   const DWELL := BASE;\n\
                         \x20   var n: u8 := 0;\n\
                         \x20   start Run { always { n := n + 1; } ref Done: after DWELL; }\n\
                         \x20   state Done;\n\
                         }\n\
                         start Main = Chain;\n";

/// Та же модель **литералом** - эталон сравнения (приём 0143).
const DWELL_LITERAL: &str = "model Chain {\n\
                             \x20   clock 1kHz;\n\
                             \x20   var n: u8 := 0;\n\
                             \x20   start Run { always { n := n + 1; } ref Done: after 3ms; }\n\
                             \x20   state Done;\n\
                             }\n\
                             start Main = Chain;\n";

fn unit_of(source: &str) -> Unit {
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    build_unit(model).expect("построение юнита")
}

fn number(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(n)) => n,
        other => panic!("переменная '{name}': неожиданное значение {other:?}"),
    }
}

/// **T11.** Переменная с выведенным по ссылке типом считается, а не падает.
///
/// Начальное значение приходит от `x` (42), затем тело прибавляет по единице за такт:
/// 43, 44, 45.
#[test]
fn variable_typed_through_reference_runs_and_counts() {
    let mut unit = unit_of(CHAIN);
    let mut trace = Vec::new();
    for _ in 0..3 {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "прогон не должен падать: {result:?}"
        );
        trace.push(number(&unit, "y"));
    }
    assert_eq!(trace, vec![43, 44, 45], "значения по тактам разошлись");
    assert_eq!(number(&unit, "x"), 42, "источник изменился");
}

/// **T12.** Выдержка ссылкой ведёт себя тождественно литеральной форме.
///
/// Приём взят у 0143 - там тождественность литеральной форме и есть критерий.
///
/// До фичи форма работала **в обход типа**: вычислитель выдержки трактовал
/// `Inference`/`Unsupported` как "вывод сюда не дошёл" и решал по значению. Тест
/// проверяет, что появившийся тип ничего не сдвинул.
#[test]
fn dwell_through_reference_matches_literal_form() {
    let by_ref = dwell_tick(DWELL_REF);
    let by_literal = dwell_tick(DWELL_LITERAL);
    assert_eq!(
        by_ref, by_literal,
        "выдержка ссылкой разошлась с литеральной формой"
    );
    assert!(by_ref.is_some(), "выдержка не сработала ни в одной форме");
}

/// Номер такта, на котором модель дошла до терминального состояния.
fn dwell_tick(source: &str) -> Option<i64> {
    let mut unit = unit_of(source);
    for tick in 1..=8i64 {
        // 1 мс на такт - та же частота, что объявлена моделью.
        unit.set_time_ns((tick - 1) * 1_000_000);
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "прогон не должен падать: {result:?}"
        );
        if result == TickResult::Terminated {
            return Some(tick);
        }
    }
    None
}

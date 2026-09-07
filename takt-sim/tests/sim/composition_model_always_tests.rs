//! Model-level `always` у модели-композиции.
//!
//! # Что покрыто
//!
//! Обе формы объявления (анонимный корень и именованная модель) x обе композиции (`|` и
//! `+`), плюс контрпример "модель со своими состояниями" - он проверяет направление
//! правки: чинили ветвь композиции, а не всё подряд.

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

/// Тактов в трассе.
const TICKS: usize = 4;

/// Анонимный корень: `start Main = A | B;` на верхнем уровне файла.
const ANON_PARALLEL: &str = "var n: u8 := 0;\n\
                             model A { start Count { ref Count; } }\n\
                             model B { start Count { ref Count; } }\n\
                             always { n := n + 1; }\n\
                             start Main = A | B;\n";

/// Именованная модель-композиция: тело объявлено в ней, а не в корне.
const NAMED_PARALLEL: &str = "var n: u8 := 0;\n\
                              model A { start Count { ref Count; } }\n\
                              model B { start Count { ref Count; } }\n\
                              model Root2 {\n\
                              \x20   always { n := n + 1; }\n\
                              \x20   start Inner = A | B;\n\
                              }\n\
                              start Main = Root2;\n";

/// Последовательная композиция: другая ветвь такта (`tick_sequential`).
const ANON_SEQUENTIAL: &str = "var n: u8 := 0;\n\
                              model A { start S1 { ref D1; } state D1; }\n\
                              model B { start S2 { ref D2; } state D2; }\n\
                              always { n := n + 1; }\n\
                              start Main = A + B;\n";

/// Контрпример: модель со **своими** состояниями - работала и до фичи.
const OWN_STATES: &str = "var n: u8 := 0;\n\
                          model Own {\n\
                          \x20   always { n := n + 1; }\n\
                          \x20   start Count { ref Count; }\n\
                          }\n\
                          start Main = Own;\n";

/// Потактовая трасса переменной `n`.
fn trace(source: &str) -> Vec<i128> {
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model).expect("построение юнита");
    let mut out = Vec::new();
    for _ in 0..TICKS {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция не должна падать: {result:?}"
        );
        out.push(value(&unit));
        if result == TickResult::Terminated {
            break;
        }
    }
    out
}

fn value(unit: &Unit) -> i128 {
    match unit.variable("n") {
        Some(Value::Number(n)) => n,
        other => panic!("переменная 'n': неожиданное значение {other:?}"),
    }
}

/// Ожидаемая трасса: по одному исполнению на такт.
fn one_per_tick(len: usize) -> Vec<i128> {
    (1..=len as i128).collect()
}

/// Анонимный корень-композиция `|`.
#[test]
fn anonymous_root_parallel_runs_body_once_per_tick() {
    let got = trace(ANON_PARALLEL);
    assert_eq!(
        got,
        one_per_tick(got.len()),
        "тело владельца обязано исполняться ровно раз за такт: пропуск дал бы \
         нули (дефект 0194), двойное исполнение — 2,4,6 (дефект 0181-01)"
    );
    assert!(got.len() >= 3, "трасса слишком коротка: {got:?}");
}

/// Именованная модель-композиция - другая ветвь `build_impl`.
#[test]
fn named_composition_model_runs_body_once_per_tick() {
    let got = trace(NAMED_PARALLEL);
    assert_eq!(
        got,
        one_per_tick(got.len()),
        "форма с именованной моделью идёт той же делегирующей ветвью и обязана \
         вести себя так же"
    );
}

/// Последовательная композиция `+` - ветвь `tick_sequential`.
///
/// Отдельный тест, а не "то же самое": у `|` и `+` разные ветви такта, а вызов
/// `execution("always")` общий - совпадение надо доказать, а не принять.
#[test]
fn sequential_composition_runs_body_once_per_tick() {
    let got = trace(ANON_SEQUENTIAL);
    assert_eq!(
        got,
        one_per_tick(got.len()),
        "последовательная композиция обязана исполнять тело владельца так же, \
         как параллельная"
    );
}

/// Модель со своими состояниями не задета.
///
/// Она работала верно и до фичи; тест проверяет направление правки - чинили ветвь
/// композиции, а не поведение `always` вообще.
#[test]
fn model_with_own_states_is_unchanged() {
    let got = trace(OWN_STATES);
    assert_eq!(
        got,
        one_per_tick(got.len()),
        "поведение модели со своими состояниями обязано остаться прежним"
    );
}

//! Голое имя состояния или модели условием не является - `SE-110`.
//!
//! # Что здесь тестытся
//!
//! 1. отказ приходит на **обе** формы: голое состояние и голая модель;
//! 2. законные записи не задеты - паттерн `S(Модель) = Состояние` и краткая
//!    `Модель = Состояние` проходят: их левый операнд - тот же
//!    `ConditionNode::Model`, и разбирает его единственная функция
//!    `state_of_model`;
//! 3. отказ действует **во всех местах объявления условия** (ребро, `cond`,
//!    инвариант), потому что обход `validate` доставляет туда все места.

use takt_lang::collect_compile_diagnostics;

/// Коды диагностик компиляции - тем же входом, которым их получают CLI и LSP.
fn codes(source: &str) -> Vec<String> {
    collect_compile_diagnostics("проба.takt", source, &[], false)
        .iter()
        .map(|d| d.code.clone().unwrap_or_default())
        .collect()
}

/// **A1: голое имя состояния на ребре - `SE-110`.**
#[test]
fn bare_state_on_edge_is_se110() {
    const SRC: &str = "var n: u8 := 0;\n\
                       start Idle { always { n := n + 1; } ref Done: Idle; }\n\
                       state Done;\n";
    assert!(
        codes(SRC).contains(&"SE-110".to_string()),
        "голое имя состояния обязано отвергаться: {:?}",
        codes(SRC)
    );
}

/// **A1: голое имя модели на ребре - `SE-110`.**
#[test]
fn bare_model_on_edge_is_se110() {
    const SRC: &str = "var n: u8 := 0;\n\
                       model Sub { start Run { always { } ref Run; } }\n\
                       start Idle { always { n := n + 1; } ref Done: Sub; }\n\
                       state Done;\n";
    assert!(
        codes(SRC).contains(&"SE-110".to_string()),
        "голое имя модели обязано отвергаться: {:?}",
        codes(SRC)
    );
}

/// **A1: то же в именованном условии и в инварианте.**
///
/// Места объявления условия перечисляет обход `validate`: проверка обязана действовать
/// всюду, а не только на ребре.
#[test]
fn bare_state_is_rejected_in_cond_and_invariant() {
    const IN_COND: &str = "var n: u8 := 0;\n\
                           cond Weird = Idle;\n\
                           start Idle { always { n := n + 1; } ref Done: Weird; }\n\
                           state Done;\n";
    assert!(
        codes(IN_COND).contains(&"SE-110".to_string()),
        "голое имя в `cond` обязано отвергаться: {:?}",
        codes(IN_COND)
    );

    const IN_INVARIANT: &str = "var n: u8 := 0;\n\
                                invariant Sane = Idle;\n\
                                start Idle { always { n := n + 1; } }\n";
    assert!(
        codes(IN_INVARIANT).contains(&"SE-110".to_string()),
        "голое имя в инварианте обязано отвергаться: {:?}",
        codes(IN_INVARIANT)
    );
}

/// **A2: законные формы проверки состояния не задеты (регресс 0245).**
///
/// Контр-проверка обязательна: левый операнд паттерна - тот же `ConditionNode::Model`,
/// что отвергается голым. Ошибка в различении сделала бы `SE-110` запретом на саму
/// проверку состояния.
#[test]
fn state_of_model_pattern_still_compiles() {
    const FULL: &str = "var n: u8 := 0;\n\
                        model Feeder { start Run { always { } ref Idle; } state Idle; }\n\
                        start Work { always { n := n + 1; } ref Done: S(Feeder) = Idle; }\n\
                        state Done;\n\
                        start Main = Work | Feeder;\n";
    assert!(
        !codes(FULL).contains(&"SE-110".to_string()),
        "полная форма 'S(Модель) = Состояние' обязана проходить: {:?}",
        codes(FULL)
    );

    const SHORT: &str = "var n: u8 := 0;\n\
                         model Feeder { start Run { always { } ref Idle; } state Idle; }\n\
                         start Work { always { n := n + 1; } ref Done: Feeder = Idle; }\n\
                         state Done;\n\
                         start Main = Work | Feeder;\n";
    assert!(
        !codes(SHORT).contains(&"SE-110".to_string()),
        "краткая форма 'Модель = Состояние' обязана проходить: {:?}",
        codes(SHORT)
    );
}

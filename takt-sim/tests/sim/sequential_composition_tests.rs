//! Значенческие тесты последовательной композиции `+`.
//!
//! # Эталон
//!
//! Нормативным поведением `+` объявлена цель `c`. Её трасса пришпилена в
//! `conformance_sv_tests` литералом `1, 1, 2, 2, 2` и там же сверяется с симулятором и
//! RTL. Здесь проверяются формы, до которых сверка с C не доходит: вложенность,
//! отсутствие `next`, контрпримеры.

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

/// Тактов в трассе - с запасом над её длиной.
const BUDGET: usize = 8;

fn unit_of(source: &str) -> Unit {
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    build_unit(model).expect("построение юнита")
}

fn number(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(n)) => n,
        Some(Value::Boolean(b)) => i128::from(b),
        other => panic!("переменная '{name}': неожиданное значение {other:?}"),
    }
}

/// Потактовая трасса значения `name`: по одному значению за такт, прогон обрывается на
/// терминальном такте (протокол `conformance_sv_tests`).
fn trace(source: &str, name: &str) -> Vec<i128> {
    let mut unit = unit_of(source);
    let mut out = Vec::new();
    for _ in 0..BUDGET {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "симуляция не должна падать: {result:?}"
        );
        out.push(number(&unit, name));
        if result == TickResult::Terminated {
            break;
        }
    }
    out
}

/// Два шага, пишущие общую `stage`: 1 и 2 соответственно.
const STEPS: &str = "model A { start S1 { always { stage := 1; } ref S2: 1 = 1; } state S2; } \
                     model B { start S1 { always { stage := 2; } ref S2: 1 = 1; } state S2; } ";

/// Эталон цели `c` для двухшаговой цепочки (пришпилен в `conformance_sv_tests`).
const REFERENCE: [i128; 5] = [1, 1, 2, 2, 2];

// -- Примеры: законные формы, обязанные исполняться ---------------------------

/// Композиция с переходом `next` исполняется и уходит по `next` по её завершении.
#[test]
fn concatenation_with_next_runs_every_step() {
    let source =
        format!("var stage: u8 := 0; {STEPS} start P = A + B {{ next Done; }} state Done;");
    assert_eq!(
        trace(&source, "stage"),
        REFERENCE,
        "каждый шаг обязан исполниться: до фичи 0181 состояние-реализация с \
         `next` уходило по нему на такте 1, не тикнув шаги (stage оставался 0)"
    );
}

/// То же во вложенной модели - глубина роли не играет.
///
/// Форма из пробы: дефект срабатывает и через вложение, поэтому правка только
/// корня" была бы мнимой.
#[test]
fn nested_concatenation_with_next_runs_every_step() {
    let source = format!(
        "var stage: u8 := 0; {STEPS} \
         model P {{ start Q = A + B {{ next Done; }} state Done; }} \
         start R = P;"
    );
    assert_eq!(
        trace(&source, "stage"),
        REFERENCE,
        "вложенная композиция обязана исполняться так же, как корневая"
    );
}

/// Композиция **без** `next` не проваливает значение и наблюдается после
/// завершения.
///
/// Два дефекта одной пробы: шаги не делили родительский контекст (`stage`, записанный
/// шагом A, шагу B был не виден - трасса 1, **0**, 2), а по завершении цепочки
/// наблюдение обрывалось вовсе (`units[index]` при `index == units.len()`).
#[test]
fn concatenation_without_next_keeps_shared_value() {
    let source = format!("var stage: u8 := 0; {STEPS} start P = A + B;");
    let observed = trace(&source, "stage");
    assert!(
        !observed.contains(&0),
        "значение общей переменной не должно проваливаться в 0 на такте \
         переключения шага: шаги `+` делят родительский контекст.\nтрасса={observed:?}"
    );
    assert_eq!(
        observed,
        vec![1, 1, 2, 2],
        "трасса без `next` — тот же эталон цели `c` без такта завершающего \
         перехода.\nтрасса={observed:?}"
    );
}

/// Повторный вход в состояние-реализацию **не** перезапускает композицию.
///
/// Контракт взят у цели `c`: вход в состояние её не переинициализирует -
/// `generate_concat_item_init` зовётся только при продвижении шага, а первый шаг
/// инициализируется однажды в `_init`. Поэтому вернувшееся состояние застаёт цепочку
/// отработавшей и уходит по `next` немедленно. Тест пришпиливает именно это, чтобы
/// "улучшение" с перезапуском не разошлось с эталоном молча.
#[test]
fn re_entering_implemented_state_does_not_restart_chain() {
    let source = format!(
        "var stage: u8 := 0; var back: u8 := 0; {STEPS} \
         start P = A + B {{ next Mid; }} \
         state Mid {{ always {{ back := back + 1; }} ref P: back < 2; }}"
    );
    let observed = trace(&source, "stage");
    assert!(
        observed.iter().all(|&v| v != 0),
        "после возврата в состояние-реализацию значение не обнуляется.\nтрасса={observed:?}"
    );
    assert_eq!(
        observed.last().copied(),
        Some(2),
        "повторный вход застаёт цепочку отработавшей: последний шаг остаётся \
         последним, композиция с начала НЕ стартует (эталон — цель `c`).\n\
         трасса={observed:?}"
    );
}

// -- Накапливающее тело: регресс исправления --------------------------------
//
// Тесты ниже пишут **накопление** (`n := n + 1`) - единственную форму, которая его
// ловит.

/// Шаг композиции, накапливающий `n`.
const COUNTER: &str = "model A { start S { always { n := n + 1; } ref T: n >= 3; } state T; } ";

/// в **последовательной** композиции `always` исполняется ровно один раз за такт.
#[test]
fn concatenation_executes_always_once_per_tick() {
    let source = format!("var n: u8 := 0; {COUNTER} model B {{ start S; }} start P = A + B;");
    let observed = trace(&source, "n");
    assert_eq!(
        observed.first().copied(),
        Some(1),
        "за первый такт накопитель обязан вырасти на ЕДИНИЦУ: композит не \
         должен исполнять `always` ребёнка за него — ребёнок делает это сам в \
         своём такте.\nтрасса={observed:?}"
    );
}

/// то же в **параллельной** композиции - исходная форма, на которой дефект и был
/// замечен (симулятор давал 2, цель `c` - 1).
#[test]
fn parallel_executes_always_once_per_tick() {
    let source = format!("var n: u8 := 0; {COUNTER} model B {{ start S; }} start P = A | B;");
    let observed = trace(&source, "n");
    assert_eq!(
        observed.first().copied(),
        Some(1),
        "за первый такт накопитель обязан вырасти на ЕДИНИЦУ.\nтрасса={observed:?}"
    );
}

// -- Контрпримеры: без них "исправление", берущее переход всегда, тоже зелено --

/// Реализация, которая не завершается, перехода **не** даёт.
///
/// Ровно тот случай, который дефект и порождал: `next` безусловен, и до он срабатывал
/// немедленно. Правка, лишь переставившая проверку и не дождавшаяся завершения,
/// тест выше прошёл бы, а этот - нет.
#[test]
fn unfinished_implementation_never_fires_next() {
    // Шаг `L` вечен: единственное ребро ведёт в него же по истинному условию.
    let source = "var stage: u8 := 0; \
                  model L { start S { always { stage := 1; } ref S: 1 = 1; } } \
                  start P = L { next Done; } state Done;";
    let mut unit = unit_of(source);
    for tick in 1..=BUDGET {
        let result = unit.tick();
        assert_eq!(
            result,
            TickResult::Processing,
            "такт {tick}: незавершённая реализация обязана держать состояние, \
             а не отпускать переход"
        );
        assert_eq!(
            unit.current_state(),
            Some("P"),
            "такт {tick}: состояние обязано остаться `P` — `next` берётся \
             только по завершении реализации"
        );
    }
}

/// Ошибка вычисления **внутри** реализации доходит наружу как `Failed`.
///
/// Различие "ошибка != ложное условие". Проглоти узел ошибку ребёнка,
/// прогон продолжился бы на недостоверных данных.
#[test]
fn failure_inside_implementation_propagates() {
    // Деление на ноль в теле шага: ошибка вычисления, а не ложное условие.
    let source = "var stage: u8 := 0; var zero: u8 := 0; \
                  model A { start S1 { always { stage := stage / zero; } ref S2: 1 = 1; } state S2; } \
                  start P = A { next Done; } state Done;";
    let mut unit = unit_of(source);
    assert!(
        matches!(unit.tick(), TickResult::Failed(_)),
        "ошибка внутри реализации обязана подняться до узла, а не быть \
         проглоченной (R5 ADR 0057)"
    );
}

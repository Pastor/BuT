//! Проверка состояния под-модели в эталоне: места употребления и отказы - 0245.
//!
//! Сверка с целью `c` живёт в `tests/conformance/conformance_state_of_model_tests.rs`
//! (потактовая трасса). Здесь - **значения**: каждое место, где условие вычисляется, и
//! каждый вид отказа. Тесты пишутся на значения, а не на факт перехода, - правило,
//! заведённое: именно отсутствие этого слоя дало восемь дефектов при зелёных тестах.

use takt_sim::{TickResult, Unit, Value};

/// Строит юнит из исходника.
fn unit(src: &str) -> Unit {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    let model = takt_lang::semantic::tree::construct_model(&ast, None, &[]).expect("семантика");
    takt_sim::build_unit(model).expect("построение Unit")
}

/// Прогоняет `ticks` тактов и возвращает значение переменной.
fn run_value(src: &str, ticks: usize, name: &str) -> i128 {
    let mut unit = unit(src);
    for step in 1..=ticks {
        if let TickResult::Failed(why) = unit.tick() {
            panic!("прогон остановлен на шаге {step}: {why}");
        }
    }
    match unit.variable(name) {
        Some(Value::Number(v)) => v,
        other => panic!("переменная '{name}' обязана быть числом, получено {other:?}"),
    }
}

/// Прогоняет такты и возвращает причину остановки (для проверки диагностик).
fn run_failure(src: &str, ticks: usize) -> String {
    let mut unit = unit(src);
    for _ in 0..ticks {
        if let TickResult::Failed(why) = unit.tick() {
            return why;
        }
    }
    panic!("прогон обязан был остановиться, но дошёл до конца");
}

/// Наблюдатель за моделью `Feeder`: `WATCH` подставляется условием.
fn watcher(watch: &str) -> String {
    format!(
        "model Feeder {{\n\
         \x20   var n: u8 := 0;\n\
         \x20   start Idle {{ always {{ n := n + 1; }} ref Done: n >= 2; }}\n\
         \x20   state Done {{ }}\n\
         }}\n\
         model Watcher {{\n\
         \x20   var seen: u8 := 0;\n\
         {watch}\n\
         }}\n\
         start Main = Feeder | Watcher;\n"
    )
}

/// **A2: условие ребра `ref`.**
#[test]
fn state_of_model_works_on_edge() {
    let src = watcher(
        "    start Wait { ref Report: S(Feeder) = Done; }\n\
         \x20   state Report { enter { seen := 1; } }",
    );
    assert_eq!(run_value(&src, 4, "seen"), 1);
}

/// **A2: именованное условие `cond`.**
///
/// Разрешение `cond` идёт другим путём, чем условие ребра, и до 0245 оба одинаково
/// упирались в `SIM-013`.
#[test]
fn state_of_model_works_in_named_condition() {
    let src = watcher(
        "    cond ready = S(Feeder) = Done;\n\
         \x20   start Wait { ref Report: ready; }\n\
         \x20   state Report { enter { seen := 1; } }",
    );
    assert_eq!(run_value(&src, 4, "seen"), 1);
}

/// **Граница конструкции: в теле блока проверка состояния не разрешается.**
///
/// `if S(Feeder) = Done { ... }` внутри `always` отвергает **семантика** (`SE-003`: имя
/// состояния ищется среди переменных области видимости, а правая часть паттерна не
/// разрешается по построению - инвариант рёбер `ref`). Отказ приходит до симулятора и
/// одинаков для всех целей, поэтому проверка состояния - это **условие**: место ей там,
/// где язык принимает условие (ребро `ref`, `cond`, формула/`invariant`).
///
/// Контрпример держит границу: молчаливое исполнение здесь означало бы, что эталон
/// понимает запись, которую компилятор не принимает.
#[test]
fn state_of_model_in_block_body_is_rejected_by_semantics() {
    let src = watcher("    start Wait { always { if S(Feeder) = Done { seen := 7; } } }");
    let (ast, _) = takt_lang::parse(&src, 0).expect("разбор");
    let error = takt_lang::semantic::tree::construct_model(&ast, None, &[])
        .expect_err("тело блока не принимает проверку состояния");
    assert_eq!(error.code.as_deref(), Some("SE-003"));
}

/// **A1: отрицание `!=` - зеркало равенства.**
#[test]
fn state_of_model_supports_not_equal() {
    let src = watcher(
        "    start Wait { ref Report: Feeder != Done; }\n\
         \x20   state Report { enter { seen := 5; } }",
    );
    // Наблюдатель уходит уже на первом такте: `Feeder` ещё в `Idle`.
    assert_eq!(run_value(&src, 3, "seen"), 5);
}

/// **A2: инвариант модели (и составное условие через `|`).**
///
/// Инвариант проверяется каждый такт: нарушение останавливает прогон. Здесь он истинен
/// всегда - `Feeder` бывает только в `Idle` или `Done`, - поэтому прогон обязан дойти
/// до конца, а `seen` дорасти до числа тактов. Заодно проверяется, что паттерн
/// вычисляется **внутри** составного условия, а не только в его вершине.
///
/// Самопереход (`ref Wait;`) обязателен: без него состояние терминально, и автомат
/// завершается на первом такте - так делает и прошивка цели `c`.
#[test]
fn state_of_model_works_in_invariant() {
    let src = watcher(
        "    invariant sane = S(Feeder) = Idle | S(Feeder) = Done;\n\
         \x20   start Wait { always { seen := seen + 1; } ref Wait; }",
    );
    assert_eq!(run_value(&src, 3, "seen"), 3);
}

/// **A4: модель, не запущенная в прогоне, даёт `SIM-036`, а не "ложно".**
///
/// Молчаливое `false` развело бы эталон с целью `c`: та на такой модели отказывает
/// `CC-012`.
#[test]
fn unknown_model_is_a_failure_not_false() {
    let src = "model Idler { start Sleep { } }\n\
               model Watcher {\n\
               \x20   var seen: u8 := 0;\n\
               \x20   start Wait { ref Report: S(Ghost) = Done; }\n\
               \x20   state Report { enter { seen := 1; } }\n\
               }\n\
               model Ghost { start Done { } }\n\
               start Main = Idler | Watcher;\n";
    let why = run_failure(src, 3);
    assert!(
        why.contains("SIM-036") && why.contains("Ghost"),
        "отказ обязан назвать код и модель, получено: {why}"
    );
}

/// **A5: заглушки `SIM-013` для этого класса больше нет.**
///
/// Тест против возврата к прежнему поведению: сообщение "пока не поддерживается" не
/// должно появляться ни на одной из двух форм записи.
#[test]
fn sim_013_is_not_emitted_for_state_of_model() {
    for watch in [
        "    start Wait { ref Report: S(Feeder) = Done; }\n\
         \x20   state Report { enter { seen := 1; } }",
        "    start Wait { ref Report: Feeder = Done; }\n\
         \x20   state Report { enter { seen := 1; } }",
    ] {
        let src = watcher(watch);
        let mut unit = unit(&src);
        for _ in 0..4 {
            if let TickResult::Failed(why) = unit.tick() {
                panic!("прогон остановлен: {why}");
            }
        }
    }
}

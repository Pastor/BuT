//! Длину прогона задаёт `-n`, а сценарий задаёт входы.
//!
//!
//! Проверяется и обратное направление: без `-n` длину по-прежнему задаёт сценарий, а
//! `-n` короче сценария по-прежнему усекает прогон. Иначе правка молча сменила бы два
//! поведения вместо одного.

use takt_lang::semantic::tree::construct_model;
use takt_sim::json_input::SimStep;
use takt_sim::runner::{PortNames, RunResult, SimulationRunner};
use takt_sim::{Value, build_unit};

/// Счётчик, который держится входом: пока `hold = 1`, растёт; иначе стоит. Так видно,
/// что значение входа удерживается и после конца сценария.
const HOLDER: &str = r#"
model Counter {
    in hold: bit;
    var ticks: u8 := 0;
    start Run {
        always { if hold { ticks := ticks + 1; } }
        ref Run;
    }
}
start Root = Counter;
"#;

fn run(scenario: &str, steps: Option<usize>) -> (RunResult, i128) {
    let (ast, _) = takt_lang::parse(HOLDER, 0).expect("разбор модели");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let unit = build_unit(model.clone()).expect("построение Unit");
    let names = PortNames::from_model(&model.borrow());
    let steps_json: Vec<SimStep> = serde_json::from_str(scenario).expect("разбор сценария");

    let mut runner = SimulationRunner::new(unit, steps_json, steps, names);
    let outcome = runner.run().expect("прогон");
    let ticks = match runner.unit().variable("ticks") {
        Some(Value::Number(n)) => n,
        other => panic!("ticks — число, получено {other:?}"),
    };
    (outcome, ticks)
}

/// L1: `-n` больше сценария - прогон идёт дальше, вход удерживается.
#[test]
fn run_continues_past_the_scenario_holding_inputs() {
    let (outcome, ticks) = run(r#"[{"in_ports": {"hold": 1}}]"#, Some(5));
    match outcome {
        RunResult::StepsReached { steps } => assert_eq!(steps, 5, "прогон обязан дойти до -n"),
        other => panic!("ожидался StepsReached, получено {other:?}"),
    }
    assert_eq!(
        ticks, 5,
        "вход удерживается после конца сценария: счётчик растёт все пять тактов"
    );
}

/// L2: удерживается последнее значение, а не первое.
#[test]
fn held_value_is_the_last_one_of_the_scenario() {
    let (_, ticks) = run(
        r#"[{"in_ports": {"hold": 1}}, {"in_ports": {"hold": 0}}]"#,
        Some(6),
    );
    assert_eq!(
        ticks, 1,
        "после нуля счётчик стоит: удерживается последний шаг сценария, а не первый"
    );
}

/// L3: без `-n` длину задаёт сценарий - прежнее поведение сохранено.
#[test]
fn without_limit_the_scenario_sets_the_length() {
    let (outcome, _) = run(r#"[{"in_ports": {"hold": 1}}, {}]"#, None);
    match outcome {
        RunResult::StepsReached { steps } => assert_eq!(steps, 2, "длина равна длине сценария"),
        other => panic!("ожидался StepsReached, получено {other:?}"),
    }
}

/// L4: `-n` короче сценария по-прежнему усекает прогон.
#[test]
fn limit_shorter_than_the_scenario_still_truncates() {
    let (outcome, _) = run(r#"[{"in_ports": {"hold": 1}}, {}, {}, {}]"#, Some(2));
    match outcome {
        RunResult::StepsReached { steps } => assert_eq!(steps, 2, "-n короче сценария усекает"),
        other => panic!("ожидался StepsReached, получено {other:?}"),
    }
}

//! Именованные порты в сценариях симуляции -.
//!
//! Позиционная форма хрупка не тем, что нечитаема, а тем, что **молчит**: индекс - это
//! место имени в алфавитном списке портов, поэтому добавление или переименование порта
//! сдвигает весь массив, и шаг начинает описывать другое событие. Поэтому проверяется
//! не только "именованная форма работает", но и "опечатка и двусмысленность становятся
//! ошибкой".

use std::path::PathBuf;
use takt_lang::semantic::tree::construct_model;
use takt_sim::graphics_config::{GraphicsConfig, OutputMode};
use takt_sim::json_input::SimStep;
use takt_sim::runner::{PortNames, RunResult, SimulationRunner};
use takt_sim::{Value, build_unit};

/// Две под-модели с **одноимённым** входным портом `sensor`: в корпусе таких моделей
/// нет (замечено ), поэтому двусмысленность моделируется специально - иначе тест
/// проверял бы собственную удачу.
const AMBIGUOUS: &str = r#"
model Left {
    in sensor: bit;
    out lamp: bit;
    start S { always { lamp := sensor; } }
}
model Right {
    in sensor: bit;
    out beep: bit;
    start S { always { beep := sensor; } }
}
start Root = Left | Right;
"#;

/// Модель с несколькими различимыми портами - для именованной формы.
const SIMPLE: &str = r#"
model Panel {
    in start_btn: bit;
    in stop_btn: bit;
    out running: bit;
    start Idle {
        always { if start_btn { running := 1; } }
        ref Done: stop_btn;
    }
    state Done { always { running := 0; } }
}
start Root = Panel;
"#;

/// Прогоняет сценарий (JSON-текст) на модели и возвращает исход.
fn run(src: &str, scenario: &str, steps: usize) -> Result<RunResult, String> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор модели");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let unit = build_unit(model.clone()).expect("построение Unit");
    let names = PortNames::from_model(&model.borrow());
    let steps_json: Vec<SimStep> = serde_json::from_str(scenario).expect("разбор сценария");

    let mut runner = SimulationRunner::new(
        unit,
        steps_json,
        Some(steps),
        None::<&PathBuf>,
        "test",
        // Графика не пишется: `output_dir = None`, поэтому режим не важен.
        OutputMode::Gif,
        names,
        None,
        GraphicsConfig::default(),
    )
    .expect("создание бегуна");
    runner.run()
}

/// A1: именованный вход задаёт названный порт и не трогает соседний.
#[test]
fn named_input_sets_only_the_named_port() {
    let outcome = run(
        SIMPLE,
        r#"[{"in_ports": {"start_btn": 1}, "guard": {"out": {"running": 1}}}]"#,
        1,
    );
    assert!(outcome.is_ok(), "прогон должен пройти: {outcome:?}");
}

/// A2: `guard.out` в объектной форме ловит расхождение.
#[test]
fn named_guard_detects_mismatch() {
    let outcome = run(
        SIMPLE,
        r#"[{"in_ports": {"start_btn": 1}, "guard": {"out": {"running": 0}}}]"#,
        1,
    );
    let Err(message) = outcome else {
        panic!("ожидалось расхождение guard: {outcome:?}");
    };
    assert!(
        message.contains("running"),
        "сообщение обязано назвать порт: {message}"
    );
}

/// A3: позиционная форма продолжает работать дословно.
///
/// Порядок портов - алфавитный: `start_btn`, `stop_btn`.
#[test]
fn positional_form_still_works() {
    let outcome = run(
        SIMPLE,
        r#"[{"in_ports": [1, 0], "guard": {"out": [1]}}]"#,
        1,
    );
    assert!(
        outcome.is_ok(),
        "позиционная форма не должна сломаться: {outcome:?}"
    );
}

/// A4: опечатка в имени - ошибка `SIM-030`, а не тихий пропуск.
#[test]
fn unknown_port_name_is_an_error() {
    let outcome = run(SIMPLE, r#"[{"in_ports": {"start_bttn": 1}}]"#, 1);
    let Err(message) = outcome else {
        panic!("несуществующее имя обязано быть ошибкой: {outcome:?}");
    };
    assert!(message.contains("SIM-030"), "{message}");
    assert!(message.contains("start_bttn"), "{message}");
}

/// A4: имя порта **другого направления** - тоже ошибка.
///
/// Задать выход из сценария нельзя; прежде такая запись молча не делала ничего.
#[test]
fn wrong_direction_port_is_an_error() {
    let outcome = run(SIMPLE, r#"[{"in_ports": {"running": 1}}]"#, 1);
    let Err(message) = outcome else {
        panic!("выходной порт во входах обязан быть ошибкой: {outcome:?}");
    };
    assert!(message.contains("SIM-030"), "{message}");
}

/// A5: голое имя, объявленное двумя моделями, - ошибка `SIM-031` с перечислением
/// вариантов.
#[test]
fn ambiguous_bare_name_is_an_error() {
    let outcome = run(AMBIGUOUS, r#"[{"in_ports": {"sensor": 1}}]"#, 1);
    let Err(message) = outcome else {
        panic!("двусмысленное имя обязано быть ошибкой: {outcome:?}");
    };
    assert!(message.contains("SIM-031"), "{message}");
    assert!(
        message.contains("Left::sensor") && message.contains("Right::sensor"),
        "сообщение обязано перечислить варианты: {message}"
    );
}

/// A5: квалифицированное имя адресует **одну** ветвь композиции.
#[test]
fn qualified_name_addresses_single_branch() {
    let outcome = run(
        AMBIGUOUS,
        r#"[{"in_ports": {"Left::sensor": 1},
             "guard": {"out": {"Left::lamp": 1, "Right::beep": 0}}}]"#,
        1,
    );
    assert!(
        outcome.is_ok(),
        "квалифицированное имя обязано работать: {outcome:?}"
    );
}

/// A5: квалифицированное имя несуществующей модели - ошибка.
#[test]
fn qualified_name_of_unknown_model_is_an_error() {
    let outcome = run(AMBIGUOUS, r#"[{"in_ports": {"Middle::sensor": 1}}]"#, 1);
    let Err(message) = outcome else {
        panic!("неизвестная модель обязана быть ошибкой: {outcome:?}");
    };
    assert!(message.contains("SIM-030"), "{message}");
}

/// Реестр квалифицированных имён строится тем же обходом, что и список двусмысленных:
/// два источника разошлись бы.
#[test]
fn qualified_registry_covers_every_port() {
    let (ast, _) = takt_lang::parse(AMBIGUOUS, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let names = PortNames::from_model(&model.borrow());
    for expected in ["Left::sensor", "Right::sensor", "Left::lamp", "Right::beep"] {
        assert!(
            names.qualified.contains(expected),
            "в реестре нет `{expected}`: {:?}",
            names.qualified
        );
    }
}

/// Значение действительно доезжает до порта.
#[test]
fn named_input_reaches_the_port() {
    let (ast, _) = takt_lang::parse(SIMPLE, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let mut unit = build_unit(model.clone()).expect("построение Unit");
    unit.set_port("start_btn", Value::Number(1));
    assert_eq!(unit.variable("start_btn"), Some(Value::Number(1)));
}

/// A6: позиционный массив неверной длины даёт предупреждение `SIM-032`, но прогон
/// продолжается.
///
/// Проверяется сквозным прогоном: предупреждение печатается пользователю, и именно это
/// надо увидеть - приёмника внутри у него нет.
#[test]
fn positional_length_mismatch_warns_but_continues() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .args([
            "tests/data/named0132/panel.takt",
            "-s",
            "tests/data/named0132/short_positional.json",
            "--steps",
            "1",
        ])
        .output()
        .expect("запуск симулятора");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("SIM-032"),
        "ожидалось предупреждение о длине: {stderr}"
    );
    assert!(
        out.status.success(),
        "предупреждение не должно прерывать прогон: {stderr}"
    );
}

/// Сквозная проверка именованной формы через бинарник - то, что увидит пользователь.
#[test]
fn named_scenario_runs_through_the_binary() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .args([
            "tests/data/named0132/panel.takt",
            "-s",
            "tests/data/named0132/named.json",
            "--steps",
            "1",
        ])
        .output()
        .expect("запуск симулятора");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "прогон обязан пройти: {stderr}");
    assert!(
        !stderr.contains("SIM-03"),
        "именованная форма нужной длины не должна давать диагностик: {stderr}"
    );
}

/// A1: позиционная форма даёт `SIM-037` - и **ровно один раз**, сколько бы шагов её ни
/// использовало.
///
/// Счёт вхождений здесь существеннее самого факта: предупреждение на каждый шаг
/// превратило бы длинный сценарий в стену одинаковых строк, и следующее - настоящее -
/// предупреждение потерялось бы среди повторов.
#[test]
fn positional_form_warns_once_per_run() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .args([
            "tests/data/named0132/panel.takt",
            "-s",
            "tests/data/named0132/positional_multi_step.json",
            "--steps",
            "4",
        ])
        .output()
        .expect("запуск симулятора");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        stderr.matches("SIM-037").count(),
        1,
        "предупреждение о форме обязано печататься один раз за прогон: {stderr}"
    );
    assert!(
        out.status.success(),
        "форма устарела, но принимается: прогон обязан пройти: {stderr}"
    );
}

/// A2: именованная форма молчит - новое предупреждение её не задевает.
#[test]
fn named_form_does_not_warn_about_deprecation() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .args([
            "tests/data/named0132/panel.takt",
            "-s",
            "tests/data/named0132/named.json",
            "--steps",
            "1",
        ])
        .output()
        .expect("запуск симулятора");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        !stderr.contains("SIM-037"),
        "именованная форма не должна давать предупреждения об устаревании: {stderr}"
    );
}

/// A3: `SIM-032` не поглощён новым кодом - они о разном.
///
/// `SIM-037` - о **форме** (массив вместо имён), `SIM-032` - о **длине**
/// (значений меньше, чем портов). Вход, где верно и то и другое, обязан дать
/// **оба**: слив их, мы потеряли бы различие "форма устарела" и "массив не той
/// длины".
#[test]
fn length_and_form_warnings_are_independent() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_takt-sim"))
        .args([
            "tests/data/named0132/panel.takt",
            "-s",
            "tests/data/named0132/short_positional.json",
            "--steps",
            "1",
        ])
        .output()
        .expect("запуск симулятора");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("SIM-032"), "о длине: {stderr}");
    assert!(stderr.contains("SIM-037"), "о форме: {stderr}");
}

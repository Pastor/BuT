//! Именованные порты в сценариях симуляции.
//!
//! Позиционная форма хрупка не тем, что нечитаема, а тем, что **молчит**: индекс - это
//! место имени в алфавитном списке портов, поэтому добавление или переименование порта
//! сдвигает весь массив, и шаг начинает описывать другое событие. Поэтому проверяется
//! не только "именованная форма работает", но и "опечатка и двусмысленность становятся
//! ошибкой".

use takt_lang::semantic::tree::construct_model;
use takt_sim::json_input::SimStep;
use takt_sim::runner::{PortNames, RunResult, RunWarning, SimulationRunner};
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

    let mut runner = SimulationRunner::new(unit, steps_json, Some(steps), names);
    runner.run()
}

/// Прогоняет сценарий по тактам и собирает предупреждения, которые вернул бегун.
///
/// Проверка **возврата**, а не печати: печать проверяют соседние тесты через `stderr`
/// бинарника, и одной её мало - потребитель без консоли получает предупреждения
/// только полем шага.
pub(super) fn collect_warnings_of(src: &str, scenario: &str, steps: usize) -> Vec<RunWarning> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор модели");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let unit = build_unit(model.clone()).expect("построение Unit");
    let names = PortNames::from_model(&model.borrow());
    let steps_json: Vec<SimStep> = serde_json::from_str(scenario).expect("разбор сценария");
    let mut runner = SimulationRunner::new(unit, steps_json, Some(steps), names);

    let mut collected = Vec::new();
    loop {
        let step = runner.step().expect("такт прогона");
        collected.extend(step.warnings);
        if step.result.is_some() {
            return collected;
        }
    }
}

/// Предупреждение о длине позиционного массива возвращается шагом, а не только
/// печатается.
#[test]
fn positional_length_warning_is_returned_by_step() {
    let warnings = collect_warnings_of(SIMPLE, r#"[{"in_ports": [1]}, {"in_ports": [1]}]"#, 2);
    let length: Vec<_> = warnings.iter().filter(|w| w.code == "SIM-032").collect();
    assert_eq!(
        length.len(),
        2,
        "о длине говорится на каждом позиционном шаге: {warnings:?}"
    );
    assert_eq!(
        length[0].step,
        Some(1),
        "номер шага в предупреждении: {warnings:?}"
    );
    assert_eq!(
        length[1].step,
        Some(2),
        "номер шага в предупреждении: {warnings:?}"
    );
}

/// Предупреждение о форме возвращается **один раз за прогон** - признак пережил
/// перенос на возврат.
///
/// Соблазн переложить дедупликацию на потребителя прямо отвергнут: потребителей два, и
/// они разошлись бы.
#[test]
fn deprecation_warning_is_returned_once() {
    let warnings = collect_warnings_of(
        SIMPLE,
        r#"[{"in_ports": [1, 0]}, {"in_ports": [1, 0]}, {"in_ports": [1, 0]}]"#,
        3,
    );
    assert_eq!(
        warnings.iter().filter(|w| w.code == "SIM-037").count(),
        1,
        "о форме говорится один раз за прогон: {warnings:?}"
    );
    assert_eq!(
        warnings
            .iter()
            .find(|w| w.code == "SIM-037")
            .expect("предупреждение о форме")
            .step,
        None,
        "предупреждение о форме относится к прогону, а не к шагу"
    );
}

/// Именованная форма не порождает предупреждений вовсе.
#[test]
fn named_form_returns_no_warnings() {
    let warnings = collect_warnings_of(
        SIMPLE,
        r#"[{"in_ports": {"start_btn": 1}}, {"in_ports": {"stop_btn": 1}}]"#,
        2,
    );
    assert!(
        warnings.is_empty(),
        "именованная форма молчит: {warnings:?}"
    );
}

/// Именованный вход задаёт названный порт и не трогает соседний.
#[test]
fn named_input_sets_only_the_named_port() {
    let outcome = run(
        SIMPLE,
        r#"[{"in_ports": {"start_btn": 1}, "guard": {"out": {"running": 1}}}]"#,
        1,
    );
    assert!(outcome.is_ok(), "прогон должен пройти: {outcome:?}");
}

/// `guard.out` в объектной форме ловит расхождение.
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

/// Позиционная форма продолжает работать дословно.
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

/// Опечатка в имени - ошибка `SIM-030`, а не тихий пропуск.
#[test]
fn unknown_port_name_is_an_error() {
    let outcome = run(SIMPLE, r#"[{"in_ports": {"start_bttn": 1}}]"#, 1);
    let Err(message) = outcome else {
        panic!("несуществующее имя обязано быть ошибкой: {outcome:?}");
    };
    assert!(message.contains("SIM-030"), "{message}");
    assert!(message.contains("start_bttn"), "{message}");
}

/// Имя порта **другого направления** - тоже ошибка.
///
/// Задать выход из сценария нельзя; без отказа такая запись молча не делает ничего.
#[test]
fn wrong_direction_port_is_an_error() {
    let outcome = run(SIMPLE, r#"[{"in_ports": {"running": 1}}]"#, 1);
    let Err(message) = outcome else {
        panic!("выходной порт во входах обязан быть ошибкой: {outcome:?}");
    };
    assert!(message.contains("SIM-030"), "{message}");
}

/// Голое имя, объявленное двумя моделями, - ошибка `SIM-031` с перечислением
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

/// Квалифицированное имя адресует **одну** ветвь композиции.
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

/// Квалифицированное имя несуществующей модели - ошибка.
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

/// Позиционный массив неверной длины даёт предупреждение `SIM-032`, но прогон
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

/// Позиционная форма даёт `SIM-037` - и **ровно один раз**, сколько бы шагов её ни
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

/// Именованная форма молчит - новое предупреждение её не задевает.
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

/// `SIM-032` не поглощён новым кодом - они о разном.
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

/// Вывод встроенной функции `debug` возвращается шагом, а не печатается из
/// библиотеки.
///
/// Канал у него свой: это вывод модели, а не сообщение инструмента о ней, и в
/// `warnings` он попадать не должен - иначе отладочная печать автора показывалась бы
/// как замечание к его же модели.
#[test]
fn debug_output_is_returned_by_step() {
    const WITH_DEBUG: &str = r#"
model Probe {
    var n: u8 := 0;

    start Run {
        always {
            n := n + 1;
            debug("такт исполнен");
        }
        ref Done: n >= 2;
    }

    state Done { }
}
start Root = Probe;
"#;

    let (ast, _) = takt_lang::parse(WITH_DEBUG, 0).expect("разбор модели");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    let unit = build_unit(model.clone()).expect("построение Unit");
    let names = PortNames::from_model(&model.borrow());
    let mut runner = SimulationRunner::new(unit, Vec::new(), Some(2), names);

    let mut output = Vec::new();
    let mut warnings = Vec::new();
    loop {
        let step = runner.step().expect("такт прогона");
        output.extend(step.output);
        warnings.extend(step.warnings);
        if step.result.is_some() {
            break;
        }
    }
    // Строк две: тело `always` исполняется и на такте, которым автомат уходит в
    // терминальное состояние.
    assert_eq!(
        output,
        vec![
            "debug: такт исполнен".to_string(),
            "debug: такт исполнен".to_string()
        ],
        "вывод программы приходит полем шага: {output:?}"
    );
    assert!(
        warnings.is_empty(),
        "вывод программы не предупреждение: {warnings:?}"
    );
}

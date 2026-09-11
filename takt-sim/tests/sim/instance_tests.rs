//! Адрес экземпляра активного состояния.
//!
//! Одна модель бывает шагом композиции несколько раз, и имя состояния не говорит,
//! какой экземпляр его несёт. Проверяется адрес: путь по владельцам реализаций с
//! номером листа выражения и моделью листа. Строка трассы и список имён от адреса не
//! зависят - это держат сверки, а здесь проверено, что имена адресов и есть `states`.

use std::path::PathBuf;
use takt_lang::semantic::tree::construct_model;
use takt_sim::graphics_config::{GraphicsConfig, OutputMode};
use takt_sim::runner::{PortNames, SimulationRunner};
use takt_sim::{ActiveState, Segment, TickResult, Unit, build_unit};

/// Тактов прогона - с запасом над длиной любой модели набора.
const BUDGET: usize = 40;

/// Модель, проходящая два состояния и завершающаяся.
const LEAF: &str = "model E { start A { ref B; } state B; }";

fn unit_of(source: &str) -> Unit {
    let (ast, _) = takt_lang::parse(source, 0).expect("разбор");
    let model = construct_model(&ast, None, &[]).expect("семантика");
    build_unit(model).expect("построение юнита")
}

fn seg(owner: &str, step: usize, model: &str) -> Segment {
    Segment {
        owner: owner.to_string(),
        step,
        model: model.to_string(),
    }
}

/// Адреса активных состояний по тактам до завершения.
fn trace(source: &str) -> Vec<Vec<ActiveState>> {
    let mut unit = unit_of(source);
    let mut out = Vec::new();
    for _ in 0..BUDGET {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "прогон упал: {result:?}"
        );
        out.push(unit.active_instances());
        if result == TickResult::Terminated {
            break;
        }
    }
    out
}

/// Пути экземпляров такта, кроме корня.
fn paths(active: &[ActiveState]) -> Vec<Vec<Segment>> {
    let mut out: Vec<Vec<Segment>> = active
        .iter()
        .filter(|a| !a.path.is_empty())
        .map(|a| a.path.clone())
        .collect();
    out.dedup();
    out
}

/// Цепочка с параллелью: шаг, две ветви разом, шаг - каждый своим номером.
#[test]
fn chain_with_parallel_numbers_leaves_left_to_right() {
    let source = format!("{LEAF} start Main = E + (E | E) + E {{ next Done; }} state Done;");
    let ticks = trace(&source);
    let mut seen: Vec<Vec<Vec<Segment>>> = Vec::new();
    for tick in &ticks {
        let now = paths(tick);
        if seen.last() != Some(&now) {
            seen.push(now);
        }
    }
    assert_eq!(
        seen,
        vec![
            vec![vec![seg("Main", 1, "E")]],
            vec![vec![seg("Main", 2, "E")], vec![seg("Main", 3, "E")]],
            vec![vec![seg("Main", 4, "E")]],
            vec![],
        ],
        "шаги идут по номерам листов, ветви параллели - вместе"
    );
    // Корень адреса не имеет, и имена адресов - это строка трассы.
    let root = &ticks[0][0];
    assert_eq!((root.path.is_empty(), root.state.as_str()), (true, "Main"));
}

/// Имена адресов - поэлементно `active_states`: второго обхода нет.
#[test]
fn instance_names_are_active_states() {
    let source = format!("{LEAF} start Main = E + (E | E) + E {{ next Done; }} state Done;");
    let mut unit = unit_of(&source);
    for _ in 0..BUDGET {
        let result = unit.tick();
        let names: Vec<String> = unit
            .active_instances()
            .into_iter()
            .map(|a| a.state)
            .collect();
        assert_eq!(names, unit.active_states());
        if result == TickResult::Terminated {
            break;
        }
    }
}

/// Вложенная реализация: путь из двух сегментов - владелец корня и владелец модели.
#[test]
fn nested_implementation_gives_two_segments() {
    let source = format!(
        "{LEAF} model Mid {{ start Run = E + E {{ next Fin; }} state Fin; }} \
         start Main = Mid + Mid {{ next Done; }} state Done;"
    );
    let leaf_paths: Vec<Vec<Segment>> = trace(&source)
        .iter()
        .flat_map(|tick| {
            tick.iter()
                .filter(|a| a.path.len() == 2)
                .map(|a| a.path.clone())
        })
        .fold(Vec::new(), |mut acc, path| {
            if acc.last() != Some(&path) {
                acc.push(path);
            }
            acc
        });
    assert_eq!(
        leaf_paths,
        vec![
            vec![seg("Main", 1, "Mid"), seg("Run", 1, "E")],
            vec![seg("Main", 1, "Mid"), seg("Run", 2, "E")],
            vec![seg("Main", 2, "Mid"), seg("Run", 1, "E")],
            vec![seg("Main", 2, "Mid"), seg("Run", 2, "E")],
        ],
        "листы нумеруются внутри своего владельца"
    );
}

/// Свёрнутая модель узла не имеет: листы её выражения принадлежат её единственному
/// состоянию. Реализацию модели (`model Duo = E + E { }`) семантика разворачивает в
/// состояние `Implement`, и владелец её листов - оно.
#[test]
fn folded_and_stateless_models_own_their_leaves() {
    let folded = format!(
        "{LEAF} model Pair {{ start Both = E | E; }} start Main = Pair {{ next Done; }} state Done;"
    );
    assert_eq!(
        paths(&trace(&folded)[0]),
        vec![
            vec![seg("Main", 1, "Pair"), seg("Both", 1, "E")],
            vec![seg("Main", 1, "Pair"), seg("Both", 2, "E")],
        ]
    );
    let stateless =
        format!("{LEAF} model Duo = E + E {{ }} start Main = Duo {{ next Done; }} state Done;");
    assert_eq!(
        paths(&trace(&stateless)[0]),
        vec![vec![seg("Main", 1, "Duo"), seg("Implement", 1, "E")]]
    );
}

/// `done` - узел завершён: шаг цепочки стоит в конце и ждёт передачи хода.
#[test]
fn done_marks_a_finished_step_until_the_handoff() {
    let source = format!("{LEAF} start Main = E + E {{ next Done; }} state Done;");
    let ticks = trace(&source);
    let first = |tick: &[ActiveState]| {
        tick.iter()
            .find(|a| a.path == vec![seg("Main", 1, "E")])
            .map(|a| (a.state.clone(), a.done))
    };
    let seen: Vec<Option<(String, bool)>> = ticks.iter().map(|t| first(t)).collect();
    let finished = seen
        .iter()
        .position(|s| s.as_ref().is_some_and(|(_, done)| *done))
        .expect("первый шаг обязан завершиться");
    assert_eq!(
        seen[finished].as_ref().unwrap().0,
        "B",
        "завершён в конечном состоянии"
    );
    assert!(
        seen[..finished]
            .iter()
            .all(|s| s.as_ref().is_some_and(|(_, done)| !done)),
        "до завершения шаг не помечен"
    );
    assert!(
        seen[finished + 1].is_none(),
        "после передачи хода первого шага в адресах нет"
    );
    assert!(
        ticks[finished + 1]
            .iter()
            .any(|a| a.path == vec![seg("Main", 2, "E")]),
        "ход у второго шага"
    );
}

/// Прогон бегуном: два экземпляра `Heater` проходят одни и те же состояния, и только
/// адрес говорит, какой шаг цепочки идёт.
#[test]
fn runner_step_carries_instances_of_pid_heater() {
    let examples = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../examples");
    let source = std::fs::read_to_string(examples.join("pid_heater.takt")).expect("чтение примера");
    let (ast, _) = takt_lang::parse(&source, 0).expect("разбор");
    // Пример подключает библиотеку из того же каталога.
    let search = vec![examples.to_string_lossy().into_owned()];
    let model = construct_model(&ast, None, &search).expect("семантика");
    let unit = build_unit(model.clone()).expect("построение юнита");
    let names = PortNames::from_model(&model.borrow());
    let mut runner = SimulationRunner::new(
        unit,
        Vec::new(),
        Some(BUDGET * 2),
        None::<&PathBuf>,
        "test",
        OutputMode::Gif,
        names,
        None,
        GraphicsConfig::default(),
    )
    .expect("создание бегуна");
    let mut steps = Vec::new();
    loop {
        let step = runner.step().expect("такт");
        if step.line.is_none() {
            break;
        }
        let states: Vec<String> = step.active.iter().map(|a| a.state.clone()).collect();
        assert_eq!(states, step.states, "имена адресов - это `states` шага");
        if let Some(heater) = step.active.iter().find(|a| a.path.len() == 1) {
            let segment = &heater.path[0];
            assert_eq!(
                (segment.owner.as_str(), segment.model.as_str()),
                ("PidHeater", "Heater")
            );
            if steps.last() != Some(&segment.step) {
                steps.push(segment.step);
            }
        }
        if step.result.is_some() {
            break;
        }
    }
    assert_eq!(steps, vec![1, 2], "сначала первый экземпляр, затем второй");
}

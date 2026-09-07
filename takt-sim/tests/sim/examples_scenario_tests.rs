//! Корпусной проверка достижимости: примеры `examples/*.takt` **запускаются**, а не только
//! компилируются.
//!
//! # Границы (сознательно)
//!
//! - **Статический анализ достижимости не вводится** - в общем виде задача
//!   неразрешима при свободных входных портах. Достижимость проверяется
//!   **прогоном**: эмпирически, но честно и дёшево.
//! - **Сверки с порождённым C нет**: наблюдается цепочка состояний **внутри**
//!   симулятора.
//! - **`guard`-JSON не используется**: он проверяет порты и переменные, а
//!   состояния проверять не умеет (`takt-sim/src/json_input.rs`).

use std::cell::RefCell;
use std::collections::BTreeSet;
use std::rc::Rc;
use takt_lang::semantic::tree::construct_model;
use takt_lang::semantic::{ModelNode, StateNode};
use takt_sim::{TickResult, Unit, Value, build_unit};

// -- Контракты и исключения ---------------------------------------------------

/// Контракт сценария примера: что он обязан пройти при прогоне без входных данных.
struct Contract {
    /// Имя файла в `examples/`.
    file: &'static str,
    /// Состояния, которые обязаны встретиться в трассе **в этом порядке**
    /// (подпоследовательность, а не точная цепочка: число циклов задают константы
    /// модели и не должно быть вписано в тест).
    chain: &'static [&'static str],
    /// Бюджет шагов - с запасом (целевой сценарий ≈172 шага).
    budget: usize,
    /// Прогон обязан завершиться терминальным состоянием, а не исчерпанием лимита.
    must_terminate: bool,
}

/// Пример без контракта: почему его сценарий не проверяется прогоном.
struct Exception {
    /// Имя файла в `examples/`.
    file: &'static str,
    /// Причина - обязательна: исключение без причины неотличимо от забывчивости.
    reason: &'static str,
}

/// Единственный пример, задающий поведение **без внешних входов**, - то есть
/// единственный, чей сценарий вообще можно проверить безусловным прогоном.
const CONTRACTS: &[Contract] = &[
    Contract {
        file: "comprehensive.takt",
        chain: &["Idle", "Heating", "Cooling", "Done"],
        budget: 400,
        must_terminate: true,
    },
    // Регулятор на q(8, 8): сходится сам и завершается.
    Contract {
        file: "regulator.takt",
        chain: &["Adjust", "Settled", "Done"],
        budget: 50,
        // `state Done { always { ready := 1; } }` - состояние С телом, и с оно автомат
        // Не завершает: "держи выход поднятым" значит каждый такт, а не однажды.
        // Цепочка состояний прежняя, завершения нет.
        must_terminate: false,
    },
    // ПИД-регулятор на прозрачном float: симулятор считает нативным f64, сходится с
    // anti-windup и завершается. q(8, 8) формируется под sv/встраиваемые флагами сборки
    // (--float-as-q / --float-embedded).
    Contract {
        file: "pid_regulator.takt",
        chain: &["Control", "Settled", "Done"],
        budget: 300,
        // Конечное состояние С телом (`always { ready := 1; }`) автомат не завершает:
        // "держи выход поднятым" значит каждый такт.
        must_terminate: false,
    },
    // Регулятор на прозрачном float: симулятор считает нативным f64 (native-режим),
    // сходится так же, как q-версия, и завершается.
    Contract {
        file: "float_regulator.takt",
        chain: &["Adjust", "Settled", "Done"],
        budget: 50,
        // Конечное состояние С телом (`always { ready := 1; }`) автомат не завершает:
        // "держи выход поднятым" значит каждый такт.
        must_terminate: false,
    },
    // Технологический цикл на последовательной композиции `+`.
    //
    // Цепочка перечисляет состояния всех трёх фаз по порядку - и вход в фазу, и её
    // терминальное состояние. Это и есть проверка того, что `+` не выродилась в `|`:
    // при одновременном исполнении фаз такая подпоследовательность не сложилась бы, а
    // без завершающих `Full`/`Blended`/`Dry` прошла бы и модель, проскочившая фазу
    // насквозь.
    //
    // Элемент трассы - `active_states().join(", ")`, то есть у составного состояния это
    // Пара "состояние корня, состояние активной фазы". Прочие контракты корпуса
    // односоставны просто потому, что их модели плоские.
    //
    // Контракт стал возможен только после: до неё симулятор не исполнял композицию,
    // объявленную реализацией состояния с переходом `next`.
    Contract {
        file: "batch_cycle.takt",
        chain: &[
            "Cycle, Fill",
            "Cycle, Full",
            "Cycle, Stir",
            "Cycle, Blended",
            "Cycle, Empty",
            "Cycle, Dry",
            "Done",
        ],
        budget: 40,
        // Конечное состояние С телом автомат не завершает.
        must_terminate: false,
    },
    // Применение библиотечного регулятора: контур замыкается в такте объекта - по
    // измерению считается воздействие, по воздействию пересчитывается измерение, -
    // сходится сам и завершается: партию греют до уставки, выдерживают до температуры
    // выдачи, цикл окончен.
    //
    // Ступеней две: вторая идёт со своей уставкой, заданной параметром при
    // инстанцировании. Цепочка проверяет обе - "Heating" встречается дважды, между ними
    // полный цикл первой ступени, а завершает прогон состояние `Finished` после
    // последовательной композиции.
    //
    // Регулятор перестал быть моделью: состояний `Control` и `Settled` в цепочке больше
    // нет - контур стал типом данных, и его состояние живёт в полях переменной, а не в
    // вершинах автомата.
    Contract {
        file: "pid_heater.takt",
        chain: &[
            "PidHeater, Heating",
            "PidHeater, Holding",
            "PidHeater, Done",
            "PidHeater, Heating",
            "PidHeater, Holding",
            "PidHeater, Done",
            "Finished",
        ],
        budget: 100,
        must_terminate: true,
    },
];

const EXCEPTIONS: &[Exception] = &[
    Exception {
        file: "pid_law.takt",
        reason: "библиотека регулирования: тип состояния контура (`struct Pid`), три \
                 операции над ним и протокол связи (`target`/`meas`/`ctrl`); модели \
                 объекта не содержит. Единственное состояние `Run` существует ради \
                 вызова операций (без вызова они не попадают в порождённый код), а \
                 без объекта измерение никто не меняет — осмысленного сценария у \
                 файла нет, он проверяется применением `pid_heater.takt`",
    },
    Exception {
        file: "elevator.takt",
        reason: "стенд парсера: шапка объявляет файл позитивным тестом разбора; \
                 корень — цепочка `next` без условий, поведения не заявлено",
    },
    Exception {
        file: "extend_complex.takt",
        reason: "стенд синтаксиса (структуры, порты с адресами, композиция \
                 `A + B + (C | D) + E`); сценария не заявляет",
    },
    Exception {
        file: "stacker.takt",
        reason: "сценарии подаются извне — `examples/simulations/stacker_*.json`, \
                 харнесс `scripts/run_simulations.sh`",
    },
    Exception {
        file: "elevator_mini.takt",
        reason: "реактивный автомат (`Cabin | Motor`): без входов стоит в Idle и \
                 не завершается — сценарии подаются извне, \
                 `examples/simulations/elevator_mini_floor2.json`. Дефект 0079 \
                 (порт под-модели композиции не перечислялся драйвером → SIM-009) \
                 ИСПРАВЛЕН: `PortNames::from_model` рекурсивна, порты драйвятся; \
                 регрессия — `composition_ports_tests.rs`",
    },
];

// -- Вспомогательное ----------------------------------------------------------

fn examples_dir() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../examples")
}

/// Минимальные фикстуры контрпримеров и примера - не копии примера: они
/// изолируют **класс** ошибки проектирования и переживут любую его переделку.
fn fixture_path(file: &str) -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/data/scenario")
        .join(file)
}

fn read_example(file: &str) -> String {
    read_source(&examples_dir().join(file))
}

fn read_source(path: &std::path::Path) -> String {
    std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("не прочитать модель {}: {e}", path.display()))
}

fn model_of(file: &str) -> Rc<RefCell<ModelNode>> {
    model_at(&examples_dir().join(file))
}

fn model_at(path: &std::path::Path) -> Rc<RefCell<ModelNode>> {
    let source = read_source(path);
    let name = path.display();
    let (ast, _) = takt_lang::parse(&source, 0).unwrap_or_else(|e| panic!("{name}: разбор: {e:?}"));
    // Каталог самой модели - путь поиска импортов: пример может подключать библиотеку
    // из корпуса (`pid_heater.takt` -> `pid_law.takt`). Компилятор ищет рядом с
    // Импортирующим файлом, но здесь исходник передаётся строкой, и знать этот каталог
    // ему неоткуда.
    let search = path
        .parent()
        .map(|d| vec![d.to_string_lossy().into_owned()])
        .unwrap_or_default();
    construct_model(&ast, None, &search).unwrap_or_else(|e| panic!("{name}: семантика: {e:?}"))
}

/// Итог прогона примера - ровно то, что наблюдает пользователь через симулятор.
struct Run {
    /// Цепочка активных состояний по шагам (соседние повторы сжаты).
    chain: Vec<String>,
    /// Активные состояния на каждом шаге - без сжатия (сколько такт длилось).
    per_step: Vec<String>,
    /// Наблюдённые переходы `(из, в)` - вход проверки мёртвых рёбер.
    transitions: BTreeSet<(String, String)>,
    /// Завершился ли прогон терминальным состоянием (а не лимитом шагов).
    terminated: bool,
    /// Ошибка вычисления, если случилась.
    failure: Option<String>,
    /// Сколько шагов исполнено.
    steps: usize,
    /// Юнит после прогона - для наблюдения значений.
    unit: Unit,
}

impl Run {
    /// Сколько подряд идущих шагов трасса провела в состоянии `name`.
    fn steps_in(&self, name: &str) -> usize {
        self.per_step.iter().filter(|s| s == &name).count()
    }
}

fn run_example(file: &str, budget: usize) -> Run {
    run_model(&examples_dir().join(file), budget)
}

fn run_fixture(file: &str, budget: usize) -> Run {
    run_model(&fixture_path(file), budget)
}

/// Прогоняет модель так же, как `SimulationRunner::run`: тик до терминального
/// состояния, ошибки или исчерпания бюджета.
fn run_model(path: &std::path::Path, budget: usize) -> Run {
    let name = path.display().to_string();
    let mut unit: Unit =
        build_unit(model_at(path)).unwrap_or_else(|e| panic!("{name}: построение юнита: {e:?}"));

    let mut chain: Vec<String> = Vec::new();
    let mut per_step: Vec<String> = Vec::new();
    let mut transitions = BTreeSet::new();
    let mut terminated = false;
    let mut failure = None;
    let mut steps = 0usize;

    let push = |chain: &mut Vec<String>, unit: &Unit| {
        let active = unit.active_states().join(", ");
        if chain.last().map(String::as_str) != Some(active.as_str()) {
            chain.push(active);
        }
    };

    for _ in 0..budget {
        let result = unit.tick();
        steps += 1;
        if let TickResult::Failed(details) = &result {
            failure = Some(format!("{details:?}"));
            break;
        }
        push(&mut chain, &unit);
        per_step.push(unit.active_states().join(", "));
        for (from, to, _pred) in unit.take_last_transitions() {
            transitions.insert((from, to));
        }
        if result == TickResult::Terminated {
            terminated = true;
            break;
        }
    }

    Run {
        chain,
        per_step,
        transitions,
        terminated,
        failure,
        steps,
        unit,
    }
}

/// Собирает объявленные рёбра `ref` по всему дереву моделей: `(из, в)`.
fn declared_edges(model: &Rc<RefCell<ModelNode>>) -> BTreeSet<(String, String)> {
    let mut out = BTreeSet::new();
    collect_edges(model, &mut out);
    out
}

fn collect_edges(model: &Rc<RefCell<ModelNode>>, out: &mut BTreeSet<(String, String)>) {
    let node = model.borrow();
    for state in node.states.values() {
        let (name, references) = match state {
            StateNode::Simple {
                name, references, ..
            }
            | StateNode::Implement {
                name, references, ..
            } => (name, references),
            StateNode::Unresolved => continue,
        };
        for reference in references {
            out.insert((name.clone(), reference.name.clone()));
        }
    }
    for nested in node.models.values() {
        collect_edges(nested, out);
    }
}

/// Есть ли `needle` в `haystack` как **подпоследовательность** (в порядке).
fn is_subsequence(needle: &[&str], haystack: &[String]) -> bool {
    let mut it = haystack.iter();
    needle.iter().all(|want| it.any(|got| got == want))
}

/// Встречается ли слово `word` в тексте как отдельная лексема.
fn has_word(text: &str, word: &str) -> bool {
    text.split(|c: char| !c.is_alphanumeric() && c != '_')
        .any(|token| token == word)
}

/// Тело примера - без шапочного комментария (иначе `grep` находит обещания шапки, а не
/// конструкции модели: ровно та ошибка, которую фича и чинит).
fn body_of(file: &str) -> String {
    read_example(file)
        .lines()
        .skip_while(|line| {
            let l = line.trim_start();
            l.starts_with("///") || l.is_empty()
        })
        .collect::<Vec<_>>()
        .join("\n")
}

// -- Группа 4: корпусной проверка -----------------------

/// У каждого примера - контракт **либо** исключение. Третьего не дано.
///
/// Это и есть работающая часть проверки: пример, про который забыли, валит тест.
#[test]
fn every_example_is_accounted_for() {
    let mut files: Vec<String> = std::fs::read_dir(examples_dir())
        .expect("не прочитать каталог examples/")
        .filter_map(Result::ok)
        .map(|entry| entry.file_name().to_string_lossy().into_owned())
        .filter(|name| name.ends_with(".takt"))
        .collect();
    files.sort();

    let mut unaccounted = Vec::new();
    for file in &files {
        let has_contract = CONTRACTS.iter().any(|c| c.file == file);
        let has_exception = EXCEPTIONS.iter().any(|e| e.file == file);
        assert!(
            !(has_contract && has_exception),
            "пример '{file}' одновременно имеет контракт и исключение — \
             определитесь: он проверяется прогоном или нет"
        );
        if !has_contract && !has_exception {
            unaccounted.push(file.clone());
        }
    }

    assert!(
        unaccounted.is_empty(),
        "примеры не имеют ни контракта сценария, ни исключения с причиной: {unaccounted:?}\n\
         Добавьте в CONTRACTS (ожидаемая цепочка состояний + бюджет шагов) — либо \
         в EXCEPTIONS с причиной, почему сценарий не проверяется прогоном.\n\
         Молча пропустить пример нельзя: именно так `comprehensive.takt` годами \
         объявлял недостижимый сценарий при зелёных гейтах."
    );

    // Обратная сторона: контракт/исключение на несуществующий файл - тоже дефект
    // (переименовали пример, а проверка продолжает "проверять" пустоту).
    for contract in CONTRACTS {
        assert!(
            files.iter().any(|f| f == contract.file),
            "контракт ссылается на несуществующий пример '{}'",
            contract.file
        );
    }
    for exception in EXCEPTIONS {
        assert!(
            files.iter().any(|f| f == exception.file),
            "исключение ссылается на несуществующий пример '{}'",
            exception.file
        );
    }
}

/// У каждого исключения - непустая причина.
#[test]
fn every_exception_states_a_reason() {
    for exception in EXCEPTIONS {
        assert!(
            exception.reason.len() > 20,
            "исключение '{}' не объясняет причину: исключение без причины \
             неотличимо от забывчивости",
            exception.file
        );
    }
}

/// Контракты **исполняются**, а не просто существуют.
#[test]
fn contracts_are_executed() {
    for contract in CONTRACTS {
        let run = run_example(contract.file, contract.budget);

        assert!(
            run.failure.is_none(),
            "{}: прогон упал: {}",
            contract.file,
            run.failure.unwrap_or_default()
        );

        // Заявленная цепочка проходится в объявленном порядке.
        assert!(
            is_subsequence(contract.chain, &run.chain),
            "{}: заявленная цепочка {:?} не пройдена.\nФактическая трасса: {:?}",
            contract.file,
            contract.chain,
            run.chain
        );

        // Сообщения различаются - "не завершился" != "упёрся в лимит".
        if contract.must_terminate {
            assert!(
                run.terminated,
                "{}: прогон НЕ завершился терминальным состоянием — исчерпан бюджет \
                 в {} шагов (исполнено {}).\nТрасса: {:?}\n\
                 Если сценарий стал длиннее — поднимите бюджет; если модель не \
                 приходит в терминальное состояние — это дефект модели.",
                contract.file, contract.budget, run.steps, run.chain
            );
        }
    }
}

// -- Группа 1: сценарий comprehensive.takt -------------------------------------

/// Достижимы **все** объявленные состояния.
#[test]
fn comprehensive_reaches_every_state() {
    let run = run_example("comprehensive.takt", 400);
    let visited: BTreeSet<&str> = run.chain.iter().map(String::as_str).collect();
    let expected: BTreeSet<&str> = ["Idle", "Heating", "Cooling", "Done"].into_iter().collect();

    assert_eq!(
        visited,
        expected,
        "недостижимые состояния: {:?}\nТрасса: {:?}",
        expected.difference(&visited).collect::<Vec<_>>(),
        run.chain
    );
}

/// Мёртвых рёбер нет - каждое объявленное `ref` срабатывает хотя бы раз.
///
/// Именно эта проверка ловит класс дефекта целиком: ребро, которое нельзя пройти ни при
/// каком входе, - мёртвый код в витрине языка.
#[test]
fn comprehensive_has_no_dead_edges() {
    let run = run_example("comprehensive.takt", 400);
    let declared = declared_edges(&model_of("comprehensive.takt"));

    let dead: Vec<_> = declared.difference(&run.transitions).collect();
    assert!(
        dead.is_empty(),
        "мёртвые рёбра (объявлены, но не срабатывают ни разу): {dead:?}\n\
         Наблюдённые переходы: {:?}",
        run.transitions
    );

    let undeclared: Vec<_> = run.transitions.difference(&declared).collect();
    assert!(
        undeclared.is_empty(),
        "наблюдены переходы, которых нет среди объявленных `ref`: {undeclared:?}"
    );
}

// -- Группа 2: контрпримеры и пример ----------------------

/// Контрпример "тупик": выход из состояния недостижим по логике модели.
///
/// Сжатая копия дефекта, из-за которого `comprehensive.takt` годами стоял в `Heating`.
/// Исправлениеирует границу ответственности: **симулятор прав, дефектна модель** - и что
/// проверка прогоном на такое реагирует.
#[test]
fn t6_counterexample_deadlock_never_leaves_state() {
    let run = run_fixture("deadlock.takt", 50);

    assert_eq!(
        run.chain,
        vec!["Heating"],
        "тупик обязан остаться тупиком: модель не может покинуть Heating"
    );
    assert!(
        !run.terminated,
        "прогон обязан упереться в лимит шагов, а не завершиться: `Done` недостижим"
    );
    assert_eq!(run.steps, 50, "модель стоит в тупике весь бюджет");
}

/// Контрпример "срыв удержания": `Cooling` уходит на первом же такте.
///
/// Ровно тот второй дефект, который "минимальная правка" из бэклога **не чинила**:
/// `Cooling` достигался, а `Done` - нет. Проверка, останавливающаяся на достижении
/// `Cooling`, признала бы такую модель исправной.
#[test]
fn t7_counterexample_hold_break_leaves_cooling_immediately() {
    let run = run_fixture("hold_break.takt", 50);

    // Состояние не удерживается **ни одного** наблюдаемого такта: сняв 3 градуса из
    // 101, модель бросает охлаждение.
    assert_eq!(
        run.steps_in("Cooling"),
        0,
        "срыв удержания: охлаждение обязано прерваться на первом же такте.\nТрасса: {:?}",
        run.chain
    );
    assert!(
        !run.chain.iter().any(|s| s == "Done"),
        "`Done` обязан остаться недостижимым: он и есть предмет контрпримера.\nТрасса: {:?}",
        run.chain
    );
}

/// Положительный пример: удержание до конца охлаждения.
///
/// Тот же контроллер, спроектированный правильно: выход из `Cooling` - только при
/// `temperature = 0`. этот пример годится для раздела документации о проектировании
/// состояний.
#[test]
fn t8_example_hold_keep_leaves_cooling_only_when_cooled() {
    let run = run_fixture("hold_keep.takt", 100);

    assert!(
        run.chain.iter().any(|s| s == "Done"),
        "`Done` обязан достигаться.\nТрасса: {:?}",
        run.chain
    );
    assert!(
        !run.chain.iter().any(|s| s == "Idle"),
        "при выполненной программе испытаний ветка `Cooled & !CyclesDone` не берётся"
    );
    // Прямая формулировка удержания: уйти можно только охладившись.
    assert_eq!(
        run.unit.variable("temperature"),
        Some(Value::Number(0)),
        "выход из Cooling обязан произойти ровно тогда, когда temperature = 0"
    );
    // Удержание в тактах: 101 единица по COOL_STEP=3 за такт - 34 исполнения тела.
    // Наблюдаемых тактов на один меньше: `Cooling` - стартовое состояние (его тело идёт
    // уже на такте 1, контракт ), а на 34-м такте активным виден уже `Done`. Уход
    // раньше означал бы срыв удержания.
    assert_eq!(
        run.steps_in("Cooling"),
        33,
        "охлаждение обязано удерживать управление до конца.\nТрасса: {:?}",
        run.chain
    );
    assert!(run.terminated, "модель обязана завершиться сама");
}

// -- Группа 3: соответствие шапки телу  ------------------------------

/// Каждая конструкция, заявленная шапкой примера, присутствует в теле.
#[test]
fn comprehensive_header_matches_body() {
    let body = body_of("comprehensive.takt");
    let promised = [
        "if", "else", "loop", "while", "for", "match", "cond", "enum", "extern", "fn", "start",
        "state",
    ];

    let missing: Vec<&str> = promised
        .into_iter()
        .filter(|word| !has_word(&body, word))
        .collect();

    assert!(
        missing.is_empty(),
        "шапка примера обещает конструкции, которых в теле нет: {missing:?}\n\
         Шапка — часть документации по языку (правило 15): обещанное обязано \
         присутствовать."
    );
}

/// Значение `float`-переменной модели (native-режим симулятора - `f64`) на текущем
/// такте. `pid_regulator.takt` переведён на `float`: симулятор без флага понижения
/// считает нативным `f64`, то есть переменные приходят [`Value::Real`], а не
/// [`Value::Fixed`].
fn real_val(unit: &Unit, name: &str) -> f64 {
    match unit.variable(name) {
        Some(Value::Real(x)) => x,
        other => panic!("переменная '{name}': ожидался Real, получено {other:?}"),
    }
}

/// **Anti-windup работает** - интеграл ПИД не переполняется, а
/// PV сходится к уставке. Пример переведён на `float`:
/// симулятор в native-режиме считает `f64`, поэтому проверка идёт в **значениях**
/// (не в q-repr). Anti-windup оставлен ради единой логики во всех представлениях
/// (в q-версии - sv/встраиваемые - без clamp интеграл `q(8, 8)` "перевернулся" бы
/// wraparound'ом; для native float clamp безвреден, но активен).
#[test]
fn pid_integral_stays_bounded_and_converges() {
    let path = examples_dir().join("pid_regulator.takt");
    let mut unit = build_unit(model_at(&path)).expect("построение юнита");

    // float (native f64): imax = 32.0; setpoint = 8.0; eps = 0.125.
    const IMAX: f64 = 32.0;
    const SP: f64 = 8.0;
    let mut converged = false;

    for _ in 0..300 {
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "ПИД не должен падать: {result:?}"
        );
        // Anti-windup: интеграл в пределах [−Imax, Imax] на каждом такте.
        let i = real_val(&unit, "i_acc");
        assert!(
            (-IMAX..=IMAX).contains(&i),
            "интеграл вышел за anti-windup [{}, {}]: {i} — clamp не работает",
            -IMAX,
            IMAX
        );
        // PV не должен "взорваться" (следствие переполнения интеграла).
        let pv = real_val(&unit, "meas");
        assert!(
            pv.abs() <= 2.0 * SP,
            "PV разошёлся: {pv} (регулятор неустойчив?)"
        );
        // Признак сходимости - Попадание в конечное состояние, а не завершение
        // автомата: с состояние `Done` имеет тело (`always { ready := 1; }`) и потому
        // работает вечно - "держи выход поднятым" значит каждый такт.
        if unit.active_states().iter().any(|state| state == "Done") {
            converged = true;
            // На завершении PV доведён до уставки (состояние Settled: meas:= target).
            assert!(
                (real_val(&unit, "meas") - SP).abs() < 1e-9,
                "PV не доведён до уставки: {}",
                real_val(&unit, "meas")
            );
            break;
        }
    }
    assert!(
        converged,
        "ПИД обязан сойтись и завершиться за бюджет (anti-windup + сходимость)"
    );
}

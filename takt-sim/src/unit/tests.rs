//! Юнит-тесты исполнителя [`Unit`].
//!
//! Вынесены из `unit/mod.rs` (лимит размера модуля): инкапсуляция `Unit` в непрозрачный
//! newtype увеличила `mod.rs`, а правило размера файла запрещает рост записей реестра -
//! тесты переехали сюда без изменения утверждений. Как потомок модуля `unit`, файл
//! видит приватную форму [`UnitKind`] и поле-обёртку, поэтому конструирует узлы
//! напрямую.

use super::*;
use std::cell::Cell;
use std::collections::HashMap;

struct MockCtx(HashMap<String, Value>);

impl Context for MockCtx {
    fn get_value(&self, name: &str) -> Option<Value> {
        self.0.get(name).cloned()
    }

    fn set_value(&mut self, name: &str, value: Value) {
        self.0.insert(name.to_string(), value);
    }
}

fn ctx_with(key: &str, val: Value) -> Rc<RefCell<dyn Context>> {
    let mut m = HashMap::new();
    m.insert(key.to_string(), val);
    Rc::new(RefCell::new(MockCtx(m)))
}

fn node() -> Unit {
    Unit(UnitKind::Node {
        time_ns: 0,
        ticks_in_state: 0,
        state_entered_ns: 0,
        model_name: None,
        entered_initial: false,
        exited_terminal: false,
        context: None,
        executions: HashMap::new(),
        state: None,
        state_transitions: HashMap::new(),
        state_executions: HashMap::new(),
        state_every: HashMap::new(),
        state_impls: HashMap::new(),
        every_consumed: Vec::new(),
        guards: Default::default(),
        invariant_violations: Vec::new(),
        last_transition: None,
    })
}

fn node_with(key: &str, val: Value) -> Unit {
    Unit(UnitKind::Node {
        time_ns: 0,
        ticks_in_state: 0,
        state_entered_ns: 0,
        model_name: None,
        entered_initial: false,
        exited_terminal: false,
        context: Some(ctx_with(key, val)),
        executions: HashMap::new(),
        state: None,
        state_transitions: HashMap::new(),
        state_executions: HashMap::new(),
        state_every: HashMap::new(),
        state_impls: HashMap::new(),
        every_consumed: Vec::new(),
        guards: Default::default(),
        invariant_violations: Vec::new(),
        last_transition: None,
    })
}

fn parallel(units: Vec<Unit>) -> Unit {
    Unit(UnitKind::Parallel {
        units: units
            .into_iter()
            .map(|u| Rc::new(RefCell::new(u)))
            .collect(),
        executions: HashMap::new(),
    })
}

fn sequential(units: Vec<Unit>) -> Unit {
    Unit(UnitKind::Sequential {
        units: units
            .into_iter()
            .map(|u| Rc::new(RefCell::new(u)))
            .collect(),
        index: 0,
        executions: HashMap::new(),
    })
}

fn len(u: &Unit) -> usize {
    match &u.0 {
        UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => units.len(),
        _ => 0,
    }
}

// -- union ----------------------------------------------------------------

// unit::union: нейтральный элемент - None | X == X. Создаём None.union(node) и
// проверяем, что результат - сам Node, без обёртки.
#[test]
fn test_union_none_with_node() {
    let result = Unit::default().union(&node());
    assert!(matches!(result.0, UnitKind::Node { .. }));
}

// unit::union: два Node объединяются в Parallel с двумя дочерними. Проверяем вариант
// результата и количество дочерних.
#[test]
fn test_union_node_with_node() {
    let result = node().union(&node());
    assert!(matches!(result.0, UnitKind::Parallel { .. }));
    assert_eq!(len(&result), 2);
}

// unit::union: Node | Parallel([a, b]) - self вставляется в начало существующего
// Parallel. Ожидаем Parallel из трёх элементов: [Node, a, b].
#[test]
fn test_union_node_with_parallel() {
    let result = node().union(&parallel(vec![node(), node()]));
    assert!(matches!(result.0, UnitKind::Parallel { .. }));
    assert_eq!(len(&result), 3);
}

// unit::union: Parallel([a]) | Parallel([b, c]) - списки units объединяются в один
// Parallel. Ожидаем три элемента: [a, b, c].
#[test]
fn test_union_parallel_with_parallel() {
    let result = parallel(vec![node()]).union(&parallel(vec![node(), node()]));
    assert!(matches!(result.0, UnitKind::Parallel { .. }));
    assert_eq!(len(&result), 3);
}

// unit::union: Parallel([a]) | Node - Node добавляется в конец существующего Parallel.
// Ожидаем два элемента.
#[test]
fn test_union_parallel_with_node() {
    let result = parallel(vec![node()]).union(&node());
    assert!(matches!(result.0, UnitKind::Parallel { .. }));
    assert_eq!(len(&result), 2);
}

// unit::union: Sequential | Node - Sequential оборачивается в новый Parallel. Ожидаем
// Parallel из двух элементов: [Sequential, Node].
#[test]
fn test_union_sequential_with_node() {
    let result = sequential(vec![node()]).union(&node());
    assert!(matches!(result.0, UnitKind::Parallel { .. }));
    assert_eq!(len(&result), 2);
}

// unit::union: Sequential | Parallel([a, b]) - Sequential вставляется в начало
// Parallel. Ожидаем Parallel из трёх элементов: [Sequential, a, b].
#[test]
fn test_union_sequential_with_parallel() {
    let result = sequential(vec![node()]).union(&parallel(vec![node(), node()]));
    assert!(matches!(result.0, UnitKind::Parallel { .. }));
    assert_eq!(len(&result), 3);
}

// -- add ------------------------------------------------------------------

// unit::add: нейтральный элемент - None + X == X. Создаём None.add(node) и проверяем,
// что результат - сам Node.
#[test]
fn test_add_none_with_node() {
    let result = Unit::default().add(&node());
    assert!(matches!(result.0, UnitKind::Node { .. }));
}

// unit::add: два Node формируют Sequential из двух элементов. Проверяем вариант
// результата и количество дочерних.
#[test]
fn test_add_node_with_node() {
    let result = node().add(&node());
    assert!(matches!(result.0, UnitKind::Sequential { .. }));
    assert_eq!(len(&result), 2);
}

// unit::add: Node + Sequential([a, b]) - выравнивание: Node вставляется в начало,
// результат Sequential([Node, a, b]) из трёх элементов, без вложенности.
#[test]
fn test_add_node_with_sequential() {
    let result = node().add(&sequential(vec![node(), node()]));
    assert!(matches!(result.0, UnitKind::Sequential { .. }));
    assert_eq!(len(&result), 3);
}

// unit::add: Sequential([a]) + Sequential([b, c]) - конкатенация списков units.
// Результат Sequential([a, b, c]) из трёх элементов.
#[test]
fn test_add_sequential_with_sequential() {
    let result = sequential(vec![node()]).add(&sequential(vec![node(), node()]));
    assert!(matches!(result.0, UnitKind::Sequential { .. }));
    assert_eq!(len(&result), 3);
}

// unit::add: Parallel + Node - Parallel не выравнивается, оборачивается целиком.
// Результат Sequential([Parallel, Node]) из двух элементов.
#[test]
fn test_add_parallel_with_node() {
    let result = parallel(vec![node(), node()]).add(&node());
    assert!(matches!(result.0, UnitKind::Sequential { .. }));
    assert_eq!(len(&result), 2);
}

// -- Context --------------------------------------------------------------

// get_value: Unit::None не содержит переменных - всегда возвращает None.
#[test]
fn test_context_none_returns_none() {
    assert!(Unit::default().get_value("x").is_none());
}

// get_value: Node без внешнего контекста и без variables - возвращает None.
#[test]
fn test_context_node_without_ctx_returns_none() {
    assert!(node().get_value("x").is_none());
}

// get_value: Node с внешним контекстом, содержащим переменную "x". Проверяем, что
// значение корректно делегируется из context.
#[test]
fn test_context_node_with_ctx_returns_value() {
    let u = node_with("x", Value::Number(42));
    assert!(matches!(u.get_value("x"), Some(Value::Number(42))));
}

// get_value: контрпример - запрашиваем ключ "y", в контексте есть только "x".
// Проверяем, что чужой ключ не возвращается.
#[test]
fn test_context_node_wrong_key_returns_none() {
    let u = node_with("x", Value::Number(1));
    assert!(u.get_value("y").is_none());
}

// get_value: Parallel ищет переменную во всех дочерних узлах. Переменная "a" есть
// только у первого ребёнка - проверяем, что она находится.
#[test]
fn test_context_parallel_finds_in_children() {
    let u = parallel(vec![node_with("a", Value::Boolean(true)), node()]);
    assert!(matches!(u.get_value("a"), Some(Value::Boolean(true))));
}

// get_value: контрпример для Parallel - запрашиваем ключ "b", которого нет ни у кого.
#[test]
fn test_context_parallel_missing_returns_none() {
    let u = parallel(vec![node_with("a", Value::Number(1))]);
    assert!(u.get_value("b").is_none());
}

// get_value: Sequential предоставляет контекст только активного (index=0) дочернего
// узла. Переменная "b" из узла index=1 недоступна, пока index не продвинут.
#[test]
fn test_context_sequential_reads_active_index() {
    let u = sequential(vec![
        node_with("a", Value::Number(10)),
        node_with("b", Value::Number(20)),
    ]);
    assert!(matches!(u.get_value("a"), Some(Value::Number(10))));
    assert!(u.get_value("b").is_none());
}

// -- is_terminal ----------------------------------------------------------

// is_terminal: Unit::None считается терминальным - это нейтральный пустой элемент.
#[test]
fn test_is_terminal_none() {
    assert!(Unit::default().is_terminal());
}

// is_terminal: Node с state: None и без переходов считается терминальным (нет активного
// состояния + нет возможных переходов -> выполнение завершено).
#[test]
fn test_is_terminal_node() {
    assert!(node().is_terminal());
}

// is_terminal: Parallel терминален только если все дочерние терминальны. Оба ребёнка
// (node() и None) терминальны - ожидаем true.
#[test]
fn test_is_terminal_parallel_all_terminal() {
    assert!(parallel(vec![node(), Unit::default()]).is_terminal());
}

// is_terminal: Sequential терминален только если все дочерние терминальны. Оба node()
// терминальны - ожидаем true.
#[test]
fn test_is_terminal_sequential_all_terminal() {
    assert!(sequential(vec![node(), node()]).is_terminal());
}

// -- tick: вспомогательные конструкторы -----------------------------------

/// Узел в именованном терминальном состоянии (нет исходящих переходов).
// -- R5: ошибка вычисления отличима от ложного условия ---

#[test]
fn r5_eval_error_is_distinguishable_from_false_condition() {
    // Ядро требования R5.
    let mut st = HashMap::new();
    let failing = Predicate::new("сломанное", |_| {
        Err(takt_lang::diagnostics::Diagnostic::error(
            takt_lang::diagnostics::Location::Builtin,
            "деление на ноль".to_string(),
        )
        .with_code("SIM-001"))
    });
    st.insert("A".to_string(), vec![("B".to_string(), failing)]);
    st.insert("B".to_string(), vec![]);
    let mut u = Unit(UnitKind::Node {
        time_ns: 0,
        ticks_in_state: 0,
        state_entered_ns: 0,
        model_name: None,
        entered_initial: true,
        exited_terminal: false,
        context: None,
        state_transitions: st,
        state_executions: HashMap::new(),
        state_every: HashMap::new(),
        state_impls: HashMap::new(),
        every_consumed: Vec::new(),
        state: Some("A".to_string()),
        executions: HashMap::new(),
        guards: Default::default(),
        invariant_violations: Vec::new(),
        last_transition: None,
    });
    match u.tick() {
        TickResult::Failed(details) => {
            assert!(details.contains("деление на ноль"), "детали: {details}");
            assert!(
                details.contains("SIM-001"),
                "код обязан быть в деталях: {details}"
            );
        }
        other => panic!("ошибка вычисления обязана давать Failed, получено {other:?}"),
    }
}

#[test]
fn r5_false_condition_is_not_an_error() {
    // Контрпример: честно ложное условие - это Processing, а не Failed.
    let mut st = HashMap::new();
    st.insert(
        "A".to_string(),
        vec![("B".to_string(), Predicate::new("ложное", |_| Ok(false)))],
    );
    st.insert("B".to_string(), vec![]);
    let mut u = Unit(UnitKind::Node {
        time_ns: 0,
        ticks_in_state: 0,
        state_entered_ns: 0,
        model_name: None,
        entered_initial: true,
        exited_terminal: false,
        context: None,
        state_transitions: st,
        state_executions: HashMap::new(),
        state_every: HashMap::new(),
        state_impls: HashMap::new(),
        every_consumed: Vec::new(),
        state: Some("A".to_string()),
        executions: HashMap::new(),
        guards: Default::default(),
        invariant_violations: Vec::new(),
        last_transition: None,
    });
    assert_eq!(u.tick(), TickResult::Processing);
}

// -- Д5: enter стартового состояния ----------------------

/// Строит узел с `enter`-исполнителем, считающим вызовы.
fn node_with_enter(counter: Rc<Cell<u32>>) -> Unit {
    let mut st = HashMap::new();
    st.insert("A".to_string(), vec![]);
    let mut execs: HashMap<String, Executions> = HashMap::new();
    let f: Execution = Rc::new(move |_ctx: &mut dyn Context| {
        counter.set(counter.get() + 1);
        Ok(Flow::Normal)
    });
    let mut m: Executions = HashMap::new();
    m.insert("enter".to_string(), vec![f]);
    execs.insert("A".to_string(), m);
    Unit(UnitKind::Node {
        time_ns: 0,
        ticks_in_state: 0,
        state_entered_ns: 0,
        model_name: None,
        entered_initial: false,
        exited_terminal: false,
        context: None,
        state_transitions: st,
        state_executions: execs,
        state_every: HashMap::new(),
        state_impls: HashMap::new(),
        every_consumed: Vec::new(),
        state: Some("A".to_string()),
        executions: HashMap::new(),
        guards: Default::default(),
        invariant_violations: Vec::new(),
        last_transition: None,
    })
}

#[test]
fn d5_enter_of_start_state_runs_on_first_tick() {
    // Д5: раньше `enter` вызывался только в ветке перехода, поэтому начальная
    // инициализация модели терялась.
    let counter = Rc::new(Cell::new(0));
    let mut u = node_with_enter(counter.clone());
    u.tick();
    assert_eq!(
        counter.get(),
        1,
        "enter стартового состояния обязан исполниться"
    );
}

#[test]
fn d5_enter_of_start_state_runs_exactly_once() {
    let counter = Rc::new(Cell::new(0));
    let mut u = node_with_enter(counter.clone());
    u.tick();
    u.tick();
    u.tick();
    assert_eq!(counter.get(), 1, "enter обязан исполниться ровно один раз");
}

#[test]
fn d5_restore_does_not_rerun_enter() {
    // Возобновление: модель уже в состоянии - входить повторно нельзя, иначе enter
    // затрёт загруженные значения.
    let counter = Rc::new(Cell::new(0));
    let mut u = node_with_enter(counter.clone());
    let snap = crate::state_io::snapshot(&u);
    crate::state_io::restore(&mut u, &snap);
    u.tick();
    assert_eq!(
        counter.get(),
        0,
        "после restore enter не должен исполняться"
    );
}

fn node_terminal(name: &str) -> Unit {
    let mut st = HashMap::new();
    st.insert(name.to_string(), vec![]);
    Unit(UnitKind::Node {
        time_ns: 0,
        ticks_in_state: 0,
        state_entered_ns: 0,
        model_name: None,
        entered_initial: false,
        exited_terminal: false,
        context: None,
        executions: HashMap::new(),
        state: Some(name.to_string()),
        state_transitions: st,
        state_executions: HashMap::new(),
        state_every: HashMap::new(),
        state_impls: HashMap::new(),
        every_consumed: Vec::new(),
        guards: Default::default(),
        invariant_violations: Vec::new(),
        last_transition: None,
    })
}

/// Узел в состоянии `from` с одним переходом в `to`; предикат всегда `cond`.
fn node_with_transition(from: &str, to: &str, cond: bool) -> Unit {
    let pred = Predicate::new("cond", move |_| Ok(cond));
    let mut st = HashMap::new();
    st.insert(from.to_string(), vec![(to.to_string(), pred)]);
    st.insert(to.to_string(), vec![]);
    Unit(UnitKind::Node {
        time_ns: 0,
        ticks_in_state: 0,
        state_entered_ns: 0,
        model_name: None,
        entered_initial: false,
        exited_terminal: false,
        context: None,
        executions: HashMap::new(),
        state: Some(from.to_string()),
        state_transitions: st,
        state_executions: HashMap::new(),
        state_every: HashMap::new(),
        state_impls: HashMap::new(),
        every_consumed: Vec::new(),
        guards: Default::default(),
        invariant_violations: Vec::new(),
        last_transition: None,
    })
}

fn current_state(u: &Unit) -> Option<&str> {
    match &u.0 {
        UnitKind::Node { state, .. } => state.as_deref(),
        _ => None,
    }
}

// -- tick: Unit::None -----------------------------------------------------

// tick: Unit::None всегда возвращает Terminated - пустой узел не имеет работы.
#[test]
fn test_tick_none_is_terminated() {
    assert_eq!(Unit::default().tick(), TickResult::Terminated);
}

// -- tick: Unit::Node -----------------------------------------------------

// tick/Node: узел без активного состояния (state: None) считается завершённым. Вызываем
// tick() и ожидаем Terminated - нет состояния, нечего выполнять.
#[test]
fn test_tick_node_no_state_is_terminated() {
    assert_eq!(node().tick(), TickResult::Terminated);
}

// tick/Node: узел в терминальном состоянии (переходов нет) возвращает Terminated.
// Создаём узел в состоянии "S" без исходящих переходов и вызываем tick().
#[test]
fn test_tick_node_terminal_state_is_terminated() {
    let mut u = node_terminal("S");
    assert_eq!(u.tick(), TickResult::Terminated);
}

// tick/Node: при срабатывании перехода возвращается Processing (работа продолжается).
// Узел A->B с предикатом true: после tick() ещё не завершён - в B ещё не были.
#[test]
fn test_tick_node_active_transition_returns_processing() {
    let mut u = node_with_transition("A", "B", true);
    assert_eq!(u.tick(), TickResult::Processing);
}

// tick/Node: срабатывание перехода обновляет поле state. После tick() состояние должно
// смениться с "A" на "B".
#[test]
fn test_tick_node_active_transition_updates_state() {
    let mut u = node_with_transition("A", "B", true);
    u.tick();
    assert_eq!(current_state(&u), Some("B"));
}

// tick/Node: если предикат не выполнен, состояние не меняется. Контрпример: узел A->B с
// предикатом false - после tick() остаётся в "A".
#[test]
fn test_tick_node_inactive_predicate_stays_in_same_state() {
    let mut u = node_with_transition("A", "B", false);
    u.tick();
    assert_eq!(current_state(&u), Some("A"));
}

// tick/Node: если предикат не выполнен, tick() возвращает Processing. Узел остаётся в
// активном состоянии с незавершёнными переходами.
#[test]
fn test_tick_node_inactive_predicate_returns_processing() {
    let mut u = node_with_transition("A", "B", false);
    assert_eq!(u.tick(), TickResult::Processing);
}

// tick/Node: детерминизм - при нескольких срабатывающих переходах берётся только
// первый. Из состояния "A" есть переходы A->B и A->C, оба с предикатом true. После
// tick() состояние должно быть "B" (первый по порядку), а не "C".
#[test]
fn test_tick_node_only_first_matching_transition_taken() {
    let pred = Predicate::new("always", |_| Ok(true));
    let mut st = HashMap::new();
    st.insert(
        "A".to_string(),
        vec![
            ("B".to_string(), pred.clone()),
            ("C".to_string(), pred.clone()),
        ],
    );
    st.insert("B".to_string(), vec![]);
    st.insert("C".to_string(), vec![]);
    let mut u = Unit(UnitKind::Node {
        time_ns: 0,
        ticks_in_state: 0,
        state_entered_ns: 0,
        model_name: None,
        entered_initial: false,
        exited_terminal: false,
        context: None,
        executions: HashMap::new(),
        state: Some("A".to_string()),
        state_transitions: st,
        state_executions: HashMap::new(),
        state_every: HashMap::new(),
        state_impls: HashMap::new(),
        every_consumed: Vec::new(),
        guards: Default::default(),
        invariant_violations: Vec::new(),
        last_transition: None,
    });
    u.tick();
    assert_eq!(current_state(&u), Some("B"));
}

// tick/Node: полный цикл - переход в терминальное состояние и обнаружение завершения.
// tick 1: A->B (Processing); tick 2: B терминальный (Terminated).
#[test]
fn test_tick_node_reaches_terminal_after_transition() {
    let mut u = node_with_transition("A", "B", true);
    u.tick();
    assert_eq!(u.tick(), TickResult::Terminated);
}

// -- tick: Unit::Parallel -------------------------------------------------

// tick/Parallel: все дочерние терминальны - Parallel возвращает Terminated. node()
// (state: None) и Unit::None оба терминальны.
#[test]
fn test_tick_parallel_all_terminated() {
    let mut u = parallel(vec![node(), Unit::default()]);
    assert_eq!(u.tick(), TickResult::Terminated);
}

// tick/Parallel: хотя бы один дочерний не завершён - Parallel возвращает Processing.
// Unit::None терминален, но второй узел ещё в процессе перехода.
#[test]
fn test_tick_parallel_one_active_returns_processing() {
    let mut u = parallel(vec![Unit::default(), node_with_transition("A", "B", true)]);
    assert_eq!(u.tick(), TickResult::Processing);
}

// tick/Parallel: все дочерние тикаются за один вызов, без раннего прерывания. Оба узла
// должны совершить переход за один tick(), даже если первый уже "готов". Проверяем
// состояния обоих дочерних после одного tick().
#[test]
fn test_tick_parallel_all_units_are_ticked() {
    let mut u = parallel(vec![
        node_with_transition("A", "B", true),
        node_with_transition("X", "Y", true),
    ]);
    u.tick();
    let UnitKind::Parallel { units, .. } = &u.0 else {
        panic!("ожидался Parallel")
    };
    assert_eq!(current_state(&units[0].borrow()), Some("B"));
    assert_eq!(current_state(&units[1].borrow()), Some("Y"));
}

// -- tick: Unit::Sequential -----------------------------------------------

// tick/Sequential: пустой список дочерних - сразу Terminated.
#[test]
fn test_tick_sequential_empty_is_terminated() {
    let mut u = sequential(vec![]);
    assert_eq!(u.tick(), TickResult::Terminated);
}

// tick/Sequential: последовательное продвижение через дочерние. node() с state: None
// завершается немедленно, поэтому каждый tick продвигает index.
//
// завершение последнего шага завершает цепочку в тот же такт - эталон цели `c`
// (`X_tick(&step); if (X_is_done(&step)) { ... }`: завершение проверяется на том же
// такте, что и тик). Прежняя редакция теста пришпиливала лишний такт ("index >= len"
// отдельным тиком) - то есть закрепляла расхождение симулятора с эталоном, а не
// проверяла его.
#[test]
fn test_tick_sequential_advances_through_children() {
    let mut u = sequential(vec![node(), node()]);
    assert_eq!(u.tick(), TickResult::Processing); // child[0] завершился -> index 0->1
    assert_eq!(u.tick(), TickResult::Terminated); // child[1] завершился -> index >= len
}

// tick/Sequential: ожидание активного дочернего перед продвижением к следующему. Первый
// ребёнок проходит A->B (два тика), затем завершается; второй завершается сразу и тем
// же тактом завершает цепочку (см. тест выше).
#[test]
fn test_tick_sequential_waits_for_child() {
    let mut u = sequential(vec![node_with_transition("A", "B", true), node()]);
    assert_eq!(u.tick(), TickResult::Processing); // child[0]: A->B, Processing
    assert_eq!(u.tick(), TickResult::Processing); // child[0]: B терминальный -> index 0->1
    assert_eq!(u.tick(), TickResult::Terminated); // child[1]: Terminated -> index >= len
}

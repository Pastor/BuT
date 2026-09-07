//! Модельное время и выдержка `after` в симуляторе.

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

/// Модель с выдержкой: дверь открыта, через 3 с закрывается.
const DOORS: &str = r#"
model Doors {
    out open: bit;
    start Opening {
        enter { open := 1; }
        ref Closing: after 3s;
    }
    state Closing {
        enter { open := 0; }
    }
}

start Main = Doors;
"#;

/// Строит `Unit` из исходника.
fn unit_of(source: &str) -> Unit {
    let (ast, _) = takt_lang::parse(source, 0).expect("исходник обязан разбираться");
    let model = construct_model(&ast, None, &[]).expect("семантика обязана строиться");
    build_unit(model).expect("Unit обязан строиться")
}

/// Прогоняет `ticks` тактов с шагом `step_ms`, возвращая `(такт, состояние)` переходов.
fn run(unit: &mut Unit, ticks: usize, step_ms: i64) -> Vec<(usize, String)> {
    let mut trace = Vec::new();
    for step in 0..ticks {
        // Часы двигаются перед тактом, кроме первого - как в `runner`.
        unit.set_time_ns(i64::try_from(step).unwrap_or(0) * step_ms * 1_000_000);
        let result = unit.tick();
        assert!(
            !matches!(result, TickResult::Failed(_)),
            "такт {} провалился: {result:?}",
            step + 1
        );
        if let Some(Value::Number(open)) = unit.variable("open") {
            trace.push((step + 1, format!("open={open}")));
        }
    }
    trace
}

#[test]
fn after_fires_exactly_three_seconds_from_state_entry() {
    let mut unit = unit_of(DOORS);
    // 1 с на такт: вход в стартовое состояние на такте 1 при t = 0, выдержка истекает
    // при t = 3 с - то есть на такте 4.
    let trace = run(&mut unit, 6, 1_000);
    let opened: Vec<usize> = trace
        .iter()
        .filter(|(_, v)| v == "open=1")
        .map(|(t, _)| *t)
        .collect();
    let closed: Vec<usize> = trace
        .iter()
        .filter(|(_, v)| v == "open=0")
        .map(|(t, _)| *t)
        .collect();
    assert_eq!(
        opened,
        vec![1, 2, 3],
        "дверь обязана быть открыта 3 такта: {trace:?}"
    );
    assert_eq!(
        closed,
        vec![4, 5, 6],
        "закрытие обязано быть на такте 4: {trace:?}"
    );
}

#[test]
fn faster_clock_needs_more_ticks_for_the_same_delay() {
    // Та же модель, вдвое более частые такты: та же физическая выдержка, вдвое больше
    // тактов. Ровно ради этого фича и заводится: длительность переносится между
    // частотами, а число тактов - нет.
    let mut unit = unit_of(DOORS);
    let trace = run(&mut unit, 10, 500);
    let closed_at = trace
        .iter()
        .find(|(_, v)| v == "open=0")
        .map(|(t, _)| *t)
        .expect("дверь обязана закрыться");
    assert_eq!(
        closed_at, 7,
        "при 500 мс на такт 3 с истекают на такте 7: {trace:?}"
    );
}

#[test]
fn without_time_the_delay_never_expires() {
    // Часы стоят (все такты при t = 0) - выдержка не истекает никогда. Это честнее, чем
    // "истекла сразу": модель, которой не дали времени, не имеет права делать вид,
    // будто время прошло.
    let mut unit = unit_of(DOORS);
    let trace = run(&mut unit, 5, 0);
    assert!(
        trace.iter().all(|(_, v)| v == "open=1"),
        "без хода времени выдержка истекать не должна: {trace:?}"
    );
}

#[test]
fn duration_variables_hold_nanoseconds() {
    // Длительность - значение, а не число: хранится в наносекундах (канон языка).
    let source = r#"
model Probe {
    out ready: bit;
    var left: duration := 1m30s;
    start Idle {
        enter { ready := 1; }
    }
}

start Main = Probe;
"#;
    let mut unit = unit_of(source);
    let _ = unit.tick();
    assert_eq!(
        unit.variable("left"),
        Some(Value::Duration(90_000_000_000)),
        "1m30s обязаны храниться как 90 с в наносекундах"
    );
}

#[test]
fn composition_branches_share_one_clock() {
    // Ветви композиции живут в одном времени: иначе выдержка в одной ветви шла бы по
    // своим часам, и трасса перестала бы быть воспроизводимой.
    let source = r#"
model Left {
    out a: bit;
    start L1 { ref L2: after 2s; }
    state L2 { enter { a := 1; } }
}

model Right {
    out b: bit;
    start R1 { ref R2: after 2s; }
    state R2 { enter { b := 1; } }
}

start Main = Left | Right;
"#;
    let mut unit = unit_of(source);
    let mut first_a = None;
    let mut first_b = None;
    for step in 0..6i32 {
        unit.set_time_ns(i64::from(step) * 1_000_000_000);
        let _ = unit.tick();
        if first_a.is_none() && unit.variable("a") == Some(Value::Number(1)) {
            first_a = Some(step + 1);
        }
        if first_b.is_none() && unit.variable("b") == Some(Value::Number(1)) {
            first_b = Some(step + 1);
        }
    }
    assert_eq!(first_a, first_b, "ветви обязаны сработать на одном такте");
    assert_eq!(first_a, Some(3), "2 с при 1 с на такт истекают на такте 3");
}

#[test]
fn snapshot_round_trip_keeps_duration_exact() {
    // Наносекунды - канон языка, поэтому круговой рейс через снимок точен (тот же
    // приём, что у representation `q(m, n)`).
    let mut unit = unit_of(
        r#"
model Probe {
    out ready: bit;
    var left: duration := 250ms;
    start Idle { enter { ready := 1; } }
}

start Main = Probe;
"#,
    );
    let _ = unit.tick();
    let before = unit.variable("left");
    let snapshot = takt_sim::state_io::snapshot(&unit);
    let json = serde_json::to_string(&snapshot).expect("снимок обязан сериализоваться");
    assert!(
        json.contains("250000000"),
        "снимок обязан нести наносекунды: {json}"
    );
    assert_eq!(before, Some(Value::Duration(250_000_000)));
}

// -- Требования от 2026-07-27 ---------------------------------------

/// Цепочка выдержек: каждая отсчитывается от входа в своё состояние-источник.
const CHAIN: &str = r#"
model Chain {
    out phase: u8;
    start A {
        enter { phase := 1; }
        ref B: after 2s;
    }
    state B {
        enter { phase := 2; }
        ref C: after 3s;
    }
    state C {
        enter { phase := 3; }
    }
}

start Main = Chain;
"#;

#[test]
fn each_delay_counts_from_entry_into_its_own_source_state() {
    // `ref C: after 3s` в состоянии B обязан отсчитывать 3 с от входа в **B**, а не от
    // начала прогона: иначе вторая выдержка "съела" бы первую.
    let mut unit = unit_of(CHAIN);
    let mut phases = Vec::new();
    for step in 0..9i32 {
        unit.set_time_ns(i64::from(step) * 1_000_000_000);
        let _ = unit.tick();
        if let Some(Value::Number(p)) = unit.variable("phase") {
            phases.push((step + 1, p));
        }
    }
    // Вход в A на такте 1 (t = 0) -> 2 с истекают при t = 2s (такт 3): переход в B.
    // Вход в B при t = 2s -> 3 с истекают при t = 5s (такт 6): переход в C.
    let first = |want: i128| {
        phases
            .iter()
            .find(|(_, p)| *p == want)
            .map(|(tick, _)| *tick)
    };
    assert_eq!(
        first(1),
        Some(1),
        "A занимается на первом такте: {phases:?}"
    );
    assert_eq!(
        first(2),
        Some(3),
        "B — при t = 2s, то есть на такте 3: {phases:?}"
    );
    assert_eq!(
        first(3),
        Some(6),
        "C — через 3 с ПОСЛЕ входа в B (t = 5s, такт 6), а не от начала прогона: {phases:?}"
    );
}

#[test]
fn re_entering_a_state_restarts_its_delay() {
    // Повторный вход в состояние начинает отсчёт заново: выдержка - свойство входа, а
    // не одноразовый будильник.
    let source = r#"
model Blink {
    out on: bit;
    var cycles: u8 := 0;
    start Off {
        enter { on := 0; }
        ref On: after 2s;
    }
    state On {
        enter { on := 1; cycles := cycles + 1; }
        ref Off: after 2s;
    }
}

start Main = Blink;
"#;
    let mut unit = unit_of(source);
    let mut switches = Vec::new();
    let mut previous = None;
    for step in 0..13i32 {
        unit.set_time_ns(i64::from(step) * 1_000_000_000);
        let _ = unit.tick();
        let now = unit.variable("on");
        if previous.is_some() && now != previous {
            switches.push(step + 1);
        }
        previous = now;
    }
    // Каждая половина периода - 2 с (2 такта по 1 с): переключения на 3, 5, 7...
    assert_eq!(
        switches,
        vec![3, 5, 7, 9, 11, 13],
        "выдержка обязана перезапускаться при каждом входе: {switches:?}"
    );
}

#[test]
fn duration_casts_to_milliseconds_and_back() {
    // `as` над длительностью даёт **миллисекунды**, обратное приведение числа к
    // `duration` трактует число как миллисекунды.
    let source = r#"
model Casts {
    out ready: bit;
    var d: duration := 1s500ms;
    var ms: u32 := 0;
    var back: duration := 0s;
    start Idle {
        always {
            ms := d as u32;
            back := 250 as duration;
            ready := 1;
        }
    }
}

start Main = Casts;
"#;
    let mut unit = unit_of(source);
    let _ = unit.tick();
    assert_eq!(
        unit.variable("ms"),
        Some(Value::Number(1_500)),
        "1s500ms обязаны дать 1500 мс"
    );
    assert_eq!(
        unit.variable("back"),
        Some(Value::Duration(250_000_000)),
        "250 обязаны дать 250 мс длительности"
    );
}

#[test]
fn duration_addition_and_subtraction_stay_duration() {
    let source = r#"
model Arith {
    out ready: bit;
    var a: duration := 1s;
    var b: duration := 250ms;
    var sum: duration := 0s;
    var diff: duration := 0s;
    start Idle {
        always {
            sum := a + b;
            diff := a - b;
            ready := 1;
        }
    }
}

start Main = Arith;
"#;
    let mut unit = unit_of(source);
    let _ = unit.tick();
    assert_eq!(unit.variable("sum"), Some(Value::Duration(1_250_000_000)));
    assert_eq!(unit.variable("diff"), Some(Value::Duration(750_000_000)));
}

// -- Запись длительности в трассе (исправление по требованию) ---------------

#[test]
fn trace_carries_duration_into_larger_units() {
    // Требование: пока значение укладывается в младшую единицу - она и печатается; при
    // переполнении появляется старшая, а остаток дописывается справа. Так запись
    // читается тем же способом, каким её пишут в исходнике.
    let cases = [
        (100_000_000i64, "100ms"),
        (999_000_000, "999ms"),
        (1_000_000_000, "1s"),
        (1_001_000_000, "1s1ms"),
        (90_000_000_000, "1m30s"),
        (3_600_000_000_000, "1h"),
        (3_661_001_000_000, "1h1m1s1ms"),
        (250_000, "250us"),
        (40, "40ns"),
        (0, "0ms"),
        (-1_001_000_000, "-1s1ms"),
    ];
    for (nanos, want) in cases {
        assert_eq!(
            takt_sim::format_duration(nanos),
            want,
            "{nanos} нс обязаны печататься как {want}"
        );
    }
}

// -- Выдержка в тактах (`after 3t`) -------------------------------------------

#[test]
fn tick_delay_needs_no_frequency_and_counts_steps() {
    // `after 3t` меряется шагами логики: частота не нужна, и от хода модельных часов
    // выдержка не зависит - ровно этим она и отличается от `after 3ms`.
    let source = r#"
model Steps {
    out ready: bit;
    start Waiting {
        ref Ready: after 3t;
    }
    state Ready { enter { ready := 1; } }
}

start Main = Steps;
"#;
    for step_ms in [0i64, 1, 1000] {
        let mut unit = unit_of(source);
        let mut fired = None;
        for step in 0..6i32 {
            unit.set_time_ns(i64::from(step) * step_ms * 1_000_000);
            let _ = unit.tick();
            if fired.is_none() && unit.variable("ready") == Some(Value::Number(1)) {
                fired = Some(step + 1);
            }
        }
        assert_eq!(
            fired,
            Some(4),
            "три такта от входа истекают на такте 4 при любом ходе часов ({step_ms} мс на такт)"
        );
    }
}

// -- Литерал длительности в теле блока -------------------------

#[test]
fn duration_literal_inside_body_is_a_value() {
    // Документ обещает `left := pause + 750ms;` - литерал длительности **в теле**
    // блока.
    //
    // Тест `duration_addition_and_subtraction_stay_duration` дефект **не ловил**: он
    // складывает две переменные, а падал именно литерал. Отсюда правило этого теста -
    // литерал обязан стоять в каждой позиции: справа от операции, слева, в сравнении.
    let source = r#"
model Literals {
    out late: bit;
    var pause: duration := 1s;
    var sum: duration := 0s;
    var diff: duration := 0s;
    var only: duration := 0s;
    start Idle {
        always {
            sum := pause + 750ms;
            diff := 3s - 1s;
            only := 250ms;
            late := (pause > 500ms) ? 1 : 0;
        }
    }
}

start Main = Literals;
"#;
    let mut unit = unit_of(source);
    let _ = unit.tick();
    assert_eq!(
        unit.variable("sum"),
        Some(Value::Duration(1_750_000_000)),
        "1s + 750ms обязаны дать 1750 мс"
    );
    assert_eq!(
        unit.variable("diff"),
        Some(Value::Duration(2_000_000_000)),
        "3s - 1s обязаны дать 2 с (литералы в обоих операндах)"
    );
    assert_eq!(
        unit.variable("only"),
        Some(Value::Duration(250_000_000)),
        "литерал в правой части присваивания обязан быть значением"
    );
    assert_eq!(
        unit.variable("late"),
        Some(Value::Number(1)),
        "сравнение переменной с литералом длительности обязано работать"
    );
}

#[test]
fn duration_literal_mixed_with_number_still_refused() {
    // Тест направления: правка литерала **не** открыла смешение с числом. Отказ
    // приходит раньше симулятора - из семантики (`SE-065`), и это верное место:
    // симулятор не должен оказаться "умнее" языка, иначе расхождение вылезет на целях.
    let source = r#"
model Mixed {
    out ready: bit;
    var pause: duration := 1s;
    var bad: duration := 0s;
    start Idle {
        always {
            bad := pause + 1;
            ready := 1;
        }
    }
}

start Main = Mixed;
"#;
    let (ast, _) = takt_lang::parse(source, 0).expect("исходник обязан разбираться");
    let error = construct_model(&ast, None, &[]).expect_err("смешение обязано отвергаться");
    assert_eq!(
        error.code.as_deref(),
        Some("SE-065"),
        "ожидался SE-065 о смешении длительности с числом, получено {error:?}"
    );
}

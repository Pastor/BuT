//! Интеграционные тесты вычислителя: модель `.takt` -> прогон -> **значения**.

use takt_lang::semantic::tree::construct_model;
use takt_sim::{TickResult, Unit, Value, build_unit};

// -- Вспомогательное -----------------------------------------------------------

fn unit_from(fixture: &str) -> Unit {
    let path = format!("tests/data/eval/{fixture}");
    let source = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("не прочитать фикстуру {path}: {e}"));
    let (ast, _) = takt_lang::parse(&source, 0).unwrap_or_else(|e| panic!("{path}: разбор: {e:?}"));
    let model =
        construct_model(&ast, None, &[]).unwrap_or_else(|e| panic!("{path}: семантика: {e:?}"));
    build_unit(model).unwrap_or_else(|e| panic!("{path}: построение юнита: {e:?}"))
}

/// Прогоняет до `steps` шагов; останавливается на терминальном состоянии или ошибке.
fn run(fixture: &str, steps: usize) -> (Unit, TickResult) {
    let mut unit = unit_from(fixture);
    let mut last = TickResult::Processing;
    for _ in 0..steps {
        last = unit.tick();
        if last != TickResult::Processing {
            break;
        }
    }
    (unit, last)
}

/// Целочисленное значение переменной - иначе внятный провал.
fn num(unit: &Unit, name: &str) -> i128 {
    match unit.variable(name) {
        Some(Value::Number(n)) => n,
        other => panic!("переменная '{name}': ожидалось целое, получено {other:?}"),
    }
}

/// Представление `q(m, n)`-переменной - иначе внятный провал.
fn fixed_repr(unit: &Unit, name: &str) -> i64 {
    match unit.variable(name) {
        Some(Value::Fixed { repr, .. }) => repr,
        other => panic!("переменная '{name}': ожидалось fixed-point, получено {other:?}"),
    }
}

// --: Q-арифметика симулятора (эталон сверки) ------------------------

/// Полный путь: инициализация литерала -> арифметика в теле -> наблюдение. T9 (floor к
/// −∞ на отрицательном) и T19 (wraparound) - через `.takt`, а не только юниты
/// `eval::fixed`.
#[test]
fn fixed_point_arithmetic_matches_normative_rules() {
    let (unit, _) = run("fixed_point.takt", 1);
    // sum = 1.5 + 0.5 = 2.0 -> 512 (сложение представлений).
    assert_eq!(fixed_repr(&unit, "sum"), 512, "q: сложение представлений");
    // prod = −1.5 · 2.0 = −3.0 -> −768 (floor к −∞; на положительном был бы невидим).
    assert_eq!(
        fixed_repr(&unit, "prod"),
        -768,
        "T9: `*` округляет floor к −∞"
    );
    // scaled = 1.5 + (3 as q) = 4.5 -> 1152 (каст масштабирует 3 -> 768).
    assert_eq!(fixed_repr(&unit, "scaled"), 1152, "T7: каст масштабирует");
    // wrap = 100.0 + 100.0 = 200.0 (вне q(8,8)) -> 51200 mod 2¹⁶ = −14336 (−56.0).
    assert_eq!(
        fixed_repr(&unit, "wrap"),
        -14336,
        "T19: переполнение `+` — wraparound"
    );
}

// -- Д1/Д2: арифметика в теле блока -------------------------------------------

#[test]
fn t1_arithmetic_in_always_is_evaluated() {
    // Ядро фичи: `c := a + 1` молча пропускалось (c оставалось 0).
    let (unit, _) = run("assign_arith.takt", 1);
    assert_eq!(num(&unit, "a"), 5);
    assert_eq!(num(&unit, "b"), 5, "присваивание переменной из переменной");
    assert_eq!(num(&unit, "c"), 6, "Д1/Д2: арифметика обязана исполняться");
}

// -- S1/S9: усечение по объявленному типу (сверено с C) -----------------------

#[test]
fn t9_t17_assignment_truncates_to_declared_type() {
    // Сверено с cc -std=c11: uint8_t a=255; a+1 -> 0; uint8_t b = 300 -> 44.
    let (unit, _) = run("overflow_u8.takt", 1);
    assert_eq!(num(&unit, "wrapped"), 0, "S1: 255 + 1 в u8 обязано дать 0");
    assert_eq!(num(&unit, "truncated"), 44, "S9: 300 в u8 обязано дать 44");
}

#[test]
fn t12_shift_promotes_then_truncates_like_c() {
    // S4: в C `uint8_t x = 1; x = x << 8;` даёт 0 без UB (продвижение до int).
    // Первоначальная формулировка S4 (UB -> диагностика) была ошибочной.
    let (unit, result) = run("shift_promo.takt", 1);
    assert_eq!(num(&unit, "x"), 0);
    assert_ne!(
        result,
        TickResult::Failed(String::new()),
        "сдвиг на 8 у u8 — определённое поведение, а не ошибка"
    );
}

// -- Д3: вызовы ---------------------------------------------------------------

#[test]
fn t3_bare_extern_procedure_call_does_not_block_block() {
    // `log_temp(x);` молча отбрасывался; проверяем, что блок исполняется целиком.
    let (unit, _) = run("call_stmt.takt", 3);
    assert_eq!(unit.current_state(), Some("Hot"), "переход при x > 7");
    assert_eq!(num(&unit, "x"), 8);
}

#[test]
fn t20_local_function_call_returns_correct_value() {
    // Критерий A7: метрика Чебышёва max(5, 3, 7) = 7 - как travel_time в stacker.
    let (unit, _) = run("local_fn_call.takt", 1);
    assert_eq!(
        num(&unit, "eta"),
        7,
        "вызов локальной fn обязан вычисляться"
    );
}

// -- Д4/Д6/Д7/Д8: условия переходов -------------------------------------------

#[test]
fn t4_function_call_in_condition_fires_transition() {
    let (unit, _) = run("fn_cond.takt", 1);
    assert_eq!(unit.current_state(), Some("Hot"));
}

#[test]
fn t6_mixed_int_real_condition_fires_transition() {
    // 1 + 2.5 = 3.5 > 3.
    let (unit, _) = run("mixed_num_cond.takt", 1);
    assert_eq!(unit.current_state(), Some("Hot"));
}

#[test]
fn t7_parenthesised_condition_fires_transition() {
    let (unit, _) = run("paren_cond.takt", 1);
    assert_eq!(unit.current_state(), Some("Hot"));
}

#[test]
fn t8_enum_variant_condition_fires_transition() {
    let (unit, _) = run("enum_cond.takt", 1);
    assert_eq!(unit.current_state(), Some("Hot"));
}

// -- Д5: enter стартового состояния -------------------------------------------

#[test]
fn t5_enter_of_start_state_runs_exactly_once() {
    let (unit, _) = run("start_enter.takt", 4);
    assert_eq!(
        num(&unit, "e"),
        7,
        "Д5: enter стартового состояния обязан идти"
    );
    assert_eq!(num(&unit, "n"), 1, "и ровно один раз — за 4 тика");
    assert_eq!(num(&unit, "t"), 4, "always при этом идёт каждый тик");
}

// -- Контрпримеры: отказ вместо тишины ----------------------------

#[test]
fn t11_division_by_zero_fails_loudly() {
    // R5: ошибка вычисления обязана быть отличима от "ничего не произошло".
    let (_, result) = run("div_zero.takt", 1);
    match result {
        TickResult::Failed(details) => {
            assert!(details.contains("деление на ноль"), "детали: {details}");
            assert!(
                details.contains("SIM-001"),
                "код обязан быть в деталях: {details}"
            );
        }
        other => panic!("деление на ноль обязано давать Failed, получено {other:?}"),
    }
}

#[test]
fn t23_extern_function_with_return_fails_loudly() {
    // Решение: тела нет -> отказ, а не тихий ноль.
    let (_, result) = run("extern_ret.takt", 1);
    match result {
        TickResult::Failed(details) => {
            assert!(details.contains("SIM-019"), "детали: {details}");
        }
        other => panic!("внешняя функция со значением обязана давать Failed, получено {other:?}"),
    }
}

#[test]
fn healthy_model_is_not_reported_as_failed() {
    // Контрпример к контрпримерам: исправная модель не должна давать Failed. Без этого
    // теста "объявлять ошибкой всё подряд" прошло бы проверки выше.
    let (_, result) = run("assign_arith.takt", 1);
    assert!(
        !matches!(result, TickResult::Failed(_)),
        "исправная модель не должна отмечаться как ошибочная: {result:?}"
    );
}

// --: композиция функций (f -> g в одной модели) ----------------------

/// Композиция функций внутри модели исполняется симулятором: r = f(5) = g(5) + 10 = (5
/// + 1) + 10 = 16. Сверено с порождённым C (r=16).
#[test]
fn fn_composition_is_evaluated() {
    let (unit, _) = run("fn_composition.takt", 1);
    assert_eq!(num(&unit, "r"), 16, "f→g: (5+1)+10 = 16");
}

// --: инварианты и assert в симуляторе ------------------------------

/// T14/T15 (A9): нарушение инварианта модели останавливает прогон с SIM-025 и именем
/// 'P'. Значение `c == 1` - проверка сработала до `always` второго такта (эталон C:
/// assert до switch), а не после.
#[test]
fn invariant_model_violation_stops_with_sim025() {
    let (unit, last) = run("invariant_violated.takt", 5);
    let TickResult::Failed(msg) = last else {
        panic!("ожидался Failed на нарушенном инварианте, получено {last:?}");
    };
    assert!(msg.contains("SIM-025"), "код SIM-025 в сообщении: {msg}");
    assert!(msg.contains("'P'"), "имя инварианта P в сообщении: {msg}");
    assert_eq!(num(&unit, "c"), 1, "остановка ДО always второго такта");
}

/// T19: истинный инвариант прогону не мешает.
#[test]
fn invariant_holds_does_not_interfere() {
    let (unit, last) = run("invariant_holds.takt", 3);
    assert!(
        !matches!(last, TickResult::Failed(_)),
        "истинный инвариант не должен ронять прогон: {last:?}"
    );
    assert_eq!(num(&unit, "c"), 2, "c растёт нормально");
}

/// T16 (A10): инвариант состояния Q нарушается (проверяется, пока автомат в A).
#[test]
fn invariant_state_violation_stops_with_name() {
    let (_unit, last) = run("invariant_state_violated.takt", 5);
    let TickResult::Failed(msg) = last else {
        panic!("ожидался Failed на инварианте состояния, получено {last:?}");
    };
    assert!(
        msg.contains("SIM-025") && msg.contains("'Q'"),
        "SIM-025 + имя Q: {msg}"
    );
}

// --: мягкий режим инвариантов (записать и продолжить) --------------

/// Прогоняет исправлениетуру в **мягком** режиме (`tick_soft`): нарушения инвариантов
/// записываются и прогон продолжается. Возвращает (unit, нарушения с шагом, последний
/// результат). Ошибка вычисления (`Failed`) обрывает, как и в бегуне.
fn run_soft(fixture: &str, steps: usize) -> (Unit, Vec<(usize, String)>, TickResult) {
    let mut unit = unit_from(fixture);
    let mut violations: Vec<(usize, String)> = Vec::new();
    let mut last = TickResult::Processing;
    for step in 1..=steps {
        last = unit.tick_soft();
        for d in unit.take_invariant_violations() {
            violations.push((step, d));
        }
        if last == TickResult::Terminated || matches!(last, TickResult::Failed(_)) {
            break;
        }
    }
    (unit, violations, last)
}

/// A2: мягкий режим не останавливает прогон на нарушении инварианта - записывает
/// нарушение и идёт дальше. `invariant_violated.takt`: P = c = 0 ложно со 2-го такта,
/// автомат осциллирует A↔B (не терминирует).
#[test]
fn invariant_soft_records_and_continues() {
    let (unit, violations, last) = run_soft("invariant_violated.takt", 5);
    // Прогон не упал (в отличие от жёсткого режима, где стоп на шаге 2).
    assert!(
        !matches!(last, TickResult::Failed(_)),
        "мягкий режим не должен ронять прогон: {last:?}"
    );
    // Нарушения на шагах 2..=5 (на шаге 1 c == 0, P держится).
    let steps: Vec<usize> = violations.iter().map(|(s, _)| *s).collect();
    assert_eq!(steps, vec![2, 3, 4, 5], "нарушения на каждом шаге со 2-го");
    assert!(
        violations
            .iter()
            .all(|(_, d)| d.contains("SIM-025") && d.contains("'P'")),
        "каждое нарушение — SIM-025 с именем P: {violations:?}"
    );
    // Прогон реально продолжился: c рос дальше 1 (жёсткий режим стоял на c == 1).
    assert!(num(&unit, "c") > 1, "c продолжил расти в мягком режиме");
}

/// A3: ошибка вычисления условия инварианта (индекс за границей массива, SIM-010) -
/// `Failed` даже в мягком режиме. Мягкий режим глушит только "инвариант ложен"
/// (SIM-025), не "условие не вычислилось" (R4).
#[test]
fn invariant_soft_does_not_swallow_eval_error() {
    let (_unit, violations, last) = run_soft("invariant_eval_error.takt", 5);
    let TickResult::Failed(msg) = last else {
        panic!("ошибка вычисления обязана дать Failed даже в мягком режиме: {last:?}");
    };
    assert!(
        msg.contains("SIM-010"),
        "индекс за границей → SIM-010: {msg}"
    );
    assert!(
        violations.is_empty(),
        "ошибка вычисления не записывается как нарушение инварианта: {violations:?}"
    );
}

/// A4: нарушение инварианта в под-модели композиции всплывает в мягком режиме
/// (рекурсивный слив по дереву Unit).
#[test]
fn invariant_soft_collects_from_composition() {
    let (_unit, violations, last) = run_soft("invariant_composite.takt", 4);
    assert!(
        !matches!(last, TickResult::Failed(_)),
        "мягкий режим не роняет композитный прогон: {last:?}"
    );
    assert!(
        !violations.is_empty() && violations.iter().all(|(_, d)| d.contains("'PA'")),
        "нарушения инварианта PA под-модели A всплыли: {violations:?}"
    );
}

/// T17 (A10): `: c;` (assert языка Takt) в блоке нарушается - так же, как invariant.
#[test]
fn assert_in_block_violation_stops() {
    let (_unit, last) = run("assert_in_block.takt", 3);
    let TickResult::Failed(msg) = last else {
        panic!("ожидался Failed на assert в блоке, получено {last:?}");
    };
    assert!(msg.contains("SIM-025"), "код SIM-025: {msg}");
}

/// переменная без инициализатора существует со значением по умолчанию (нулевым, как
/// default-init в C), а не даёт SIM-009 "переменная не найдена".
///
/// До исправления `var q: u8;` (и любой скаляр без init) в симуляторе не регистрировался -
/// чтение давало SIM-009 (гэп, регистрировалась лишь структура). Зонд-значения
/// захвачены прогоном, не угаданы.
#[test]
fn var_without_initializer_defaults_to_zero() {
    let (unit, last) = run("var_no_init.takt", 1);
    assert!(
        !matches!(last, TickResult::Failed(_)),
        "прогон не должен падать (в т.ч. SIM-009), получено {last:?}"
    );

    // Прямое чтение переменных без инициализатора -> нулевое значение по типу.
    assert_eq!(num(&unit, "q"), 0, "u8 без init → 0");
    assert_eq!(num(&unit, "flag"), 0, "bit без init → 0");
    assert_eq!(fixed_repr(&unit, "ratio"), 0, "q(8,8) без init → repr 0");

    // Чтение в теле (`seen := var`) прошло без SIM-009 и увидело нули - значит
    // переменная существует, а не "не найдена".
    assert_eq!(num(&unit, "seen_q"), 0, "seen_q := q → 0");
    assert_eq!(num(&unit, "seen_flag"), 0, "seen_flag := flag → 0");
    assert_eq!(
        fixed_repr(&unit, "seen_ratio"),
        0,
        "seen_ratio := ratio → repr 0"
    );
}

// --: исполнение массивов симулятором --------------------------------

/// Элемент массива по индексу - иначе внятный провал.
fn arr_elem(unit: &Unit, name: &str, i: usize) -> i128 {
    match unit.variable(name) {
        Some(Value::Array(items)) => match items.get(i) {
            Some(Value::Number(n)) => *n,
            other => panic!("{name}[{i}]: ожидалось целое, получено {other:?}"),
        },
        other => panic!("переменная '{name}': ожидался массив, получено {other:?}"),
    }
}

/// Полный путь: список-инициализатор -> запись элемента -> чтение элемента.
///
/// Теперь массив исполняется.
#[test]
fn array_element_write_and_read() {
    let (unit, last) = run("arrays.takt", 1);
    assert!(
        !matches!(last, TickResult::Failed(_)),
        "прогон массивов не должен падать (в т.ч. SIM-017/010), получено {last:?}"
    );

    // Запись элемента исполнена, соседи не тронуты (точечность).
    assert_eq!(arr_elem(&unit, "data", 0), 7, "data[0] := 7");
    assert_eq!(arr_elem(&unit, "data", 1), 2, "data[1] не тронут (init)");
    assert_eq!(arr_elem(&unit, "data", 2), 3, "data[2] не тронут (init)");
    assert_eq!(arr_elem(&unit, "data", 3), 99, "data[3] := 99");

    // Чтение элемента в теле: `first := data[0]` увидел записанное в этом же такте.
    assert_eq!(num(&unit, "first"), 7, "first := data[0] после записи → 7");
    assert_eq!(num(&unit, "third"), 3, "third := data[2] → 3");

    // Список-инициализатор приводит элементы к типу (u8): 300 -> 44 (усечение).
    assert_eq!(
        arr_elem(&unit, "big", 0),
        44,
        "big[0]: 300 усечено к u8 → 44"
    );
    assert_eq!(arr_elem(&unit, "big", 1), 5, "big[1] := 5");
}

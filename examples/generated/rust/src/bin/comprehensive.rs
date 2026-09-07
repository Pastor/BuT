//! Проверка порождённого Rust: модель `comprehensive` (`examples/comprehensive.takt`).
//!
//! Модель владеет аппаратным слоем (`Comprehensive::new(hal)`), поле `hal`
//! приватно и геттера нет - поэтому трасса собирается в общий `Rc<RefCell<_>>`,
//! а модели отдаётся дескриптор.
//!
//! # Что здесь закреплено
//!
//! Заявленный сценарий примера - термоциклирование `Idle -> Heating -> Cooling ->
//! ... MAX_COUNT раз -> Done`. Проверка ловит его **в наблюдаемом
//! поведении**: у цели `rust` поля модели приватны, поэтому единственное окно
//! наружу - вызовы `extern fn` через HAL плюс `is_done()`.
//!
//! **Прежняя редакция закрепляла дефект примера** ("в Heating температура не
//! меняется: 16 навсегда") - намеренно, и падала при исправлении.
//! Так и вышло: падение этого `main` и было сигналом, что пример исправлен.
//!
//! # Сверка целей: `rust` сходится с симулятором такт-в-такт
//!
//! Симулятор завершает модель за **172** шага
//! (`takt-sim/tests/sim/examples_scenario_tests.rs`), и порождённый Rust завершает
//! её за **те же 172** такта. Это независимая сверка двух реализаций одной
//! семантики: проверка `rustc` и `clippy` доказывает, что вывод компилируется, но не
//! что он считает то же самое (`CLAUDE.md`).

use std::cell::RefCell;
use std::rc::Rc;

use takt_generated::comprehensive::{Comprehensive, Hal};

/// Записанные вызовы внешних функций модели.
#[derive(Default)]
struct Trace {
    temp: Vec<u8>,
    count: Vec<u8>,
}

/// Дескриптор трассы, отдаваемый модели.
struct Probe(Rc<RefCell<Trace>>);

impl Hal for Probe {
    fn log_count(&mut self, n: u8) {
        self.0.borrow_mut().count.push(n);
    }

    fn log_temp(&mut self, value: u8) {
        self.0.borrow_mut().temp.push(value);
    }
}

/// Бюджет с запасом: сценарий укладывается в 172 такта.
const BUDGET: usize = 400;
/// Столько же шагов тратит симулятор - значение сверено, а не угадано.
const EXPECTED_TICKS: usize = 172;
/// `MAX_COUNT` модели: столько циклов "нагрев -> охлаждение" она выполняет.
const CYCLES: usize = 3;

fn main() {
    let trace = Rc::new(RefCell::new(Trace::default()));
    let mut m = Comprehensive::new(Probe(Rc::clone(&trace)));

    m.init();
    let mut ticks = 0usize;
    for tick in 1..=BUDGET {
        m.tick();
        if m.is_done() {
            ticks = tick;
            break;
        }
    }

    // Сценарий завершается сам - и ровно там же, где у симулятора.
    assert!(
        m.is_done(),
        "автомат обязан завершиться: `Done` достигнут за {EXPECTED_TICKS} тактов у \
         симулятора, здесь не завершился и за {BUDGET}"
    );
    assert_eq!(
        ticks, EXPECTED_TICKS,
        "порождённый Rust обязан сходиться с симулятором такт-в-такт"
    );

    let t = trace.borrow();

    // `Idle` греет на 1 за такт и логирует температуру - 11 тактов до порога
    // `WARMUP_TEMP` (10). Прогревов ровно столько, сколько циклов испытаний.
    assert_eq!(
        t.temp,
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11].repeat(CYCLES),
        "каждый цикл начинается прогревом камеры 1 → 11"
    );

    // Телеметрия остатка: в `Heating` - ступени до порога (`while` в
    // `steps_to_limit`), в `Cooling` - ступени до нуля (`loop` в `steps_to_zero`).
    // Обратный отсчёт до 0 в обоих - прямое свидетельство, что состояние
    // доводит работу до конца, а не бросает её (контрпример `hold_break.takt`).
    let heating: Vec<u8> = (0..=11).rev().collect();
    let cooling: Vec<u8> = (0..=33).rev().collect();
    let cycle: Vec<u8> = heating.iter().chain(cooling.iter()).copied().collect();
    assert_eq!(
        t.count,
        cycle.repeat(CYCLES),
        "нагрев обязан досчитать до порога (11 → 0), охлаждение — до нуля (33 → 0)"
    );

    println!(
        "comprehensive: OK ({EXPECTED_TICKS} тактов, {CYCLES} цикла термоциклирования → Done; \
         сходится с симулятором такт-в-такт)"
    );
}

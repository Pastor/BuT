//! Проверка порождённого Rust: модель `elevator` (`examples/elevator.takt`).
//!
//! Модель узнаёт о вызовах через переменную `has_call`, которую поднимает
//! внешняя функция (`extern fn scan_floor_buttons`). В цели `rust` внешние
//! функции живут за трейтом [`Hal`] и до переменных модели не дотягиваются -
//! поэтому вызова здесь не случается никогда, и проверяется ровно это: без
//! вызова автомат стоит в `Idle` и мотор не трогает.

use std::cell::RefCell;
use std::rc::Rc;

use takt_generated::elevator::{Elevator, Hal, InU8Port};

/// Записанные вызовы внешних функций модели.
#[derive(Default)]
struct Trace {
    calls: Vec<&'static str>,
}

/// Подставное железо: порт `SensorsCab` читается заданным значением, прочие - нулём.
struct Probe {
    trace: Rc<RefCell<Trace>>,
    cabin: u8,
}

impl Probe {
    fn note(&mut self, what: &'static str) {
        self.trace.borrow_mut().calls.push(what);
    }
}

impl Hal for Probe {
    fn read_u8(&mut self, port: InU8Port) -> u8 {
        match port {
            InU8Port::SensorsCab => self.cabin,
            _ => 0,
        }
    }

    fn door_close(&mut self) {
        self.note("door_close");
    }
    fn door_open(&mut self) {
        self.note("door_open");
    }
    fn motor_down(&mut self) {
        self.note("motor_down");
    }
    fn motor_stop(&mut self) {
        self.note("motor_stop");
    }
    fn motor_up(&mut self) {
        self.note("motor_up");
    }
    fn read_floor_sensors(&mut self) {
        self.note("read_floor_sensors");
    }
    fn scan_cabin_buttons(&mut self) {
        self.note("scan_cabin_buttons");
    }
    fn scan_floor_buttons(&mut self) {
        self.note("scan_floor_buttons");
    }
}

/// Прогоняет `ticks` тактов при `SensorsCab = cabin`.
fn run(cabin: u8, ticks: usize) -> (Vec<&'static str>, bool) {
    let trace = Rc::new(RefCell::new(Trace::default()));
    let mut m = Elevator::new(Probe {
        trace: Rc::clone(&trace),
        cabin,
    });
    m.init();
    for _ in 0..ticks {
        m.tick();
    }
    let calls = trace.borrow().calls.clone();
    (calls, m.is_done())
}

const TICKS: usize = 5;

fn main() {
    // Бит 0 порта SensorsCab снят: кабину не опрашиваем.
    let (calls, done) = run(0, TICKS);
    // Такт 1: вход в стартовое состояние `Idle` такта не расходует (контракт
    // ADR 0033) - `enter`-блок (door_open) и тело `Idle` идут одним тактом.
    assert_eq!(calls[0], "door_open", "enter стартового состояния — door_open");
    assert_eq!(
        calls.len(),
        1 + TICKS,
        "door_open один раз + scan_floor_buttons каждый такт, получено {calls:?}"
    );
    assert!(
        calls[1..].iter().all(|&c| c == "scan_floor_buttons"),
        "без вызова Idle только опрашивает этажные кнопки, получено {calls:?}"
    );
    assert!(!done, "автомат лифта не завершается");

    // Бит 0 порта SensorsCab поднят: добавляется опрос кабины.
    let (calls, done) = run(1, TICKS);
    assert_eq!(
        calls.len(),
        1 + TICKS * 2,
        "при SensorsCab.0 = 1 к опросу этажей добавляется опрос кабины, получено {calls:?}"
    );
    assert!(
        calls[1..]
            .chunks(2)
            .all(|c| c == ["scan_floor_buttons", "scan_cabin_buttons"]),
        "порядок опроса за такт: этажи, затем кабина; получено {calls:?}"
    );
    // Мотор и двери не трогаются: has_call поднять некому (см. шапку модуля).
    assert!(
        !calls.iter().any(|c| c.starts_with("motor_")),
        "без вызова мотор не трогается, получено {calls:?}"
    );
    assert!(!done, "автомат лифта не завершается");

    println!("elevator: OK (Idle опрашивает кнопки, мотор стоит — вызова нет)");
}

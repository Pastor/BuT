//! Проверка порождённого Rust: модель `elevator_mini` (`examples/elevator_mini.takt`).
//!
//! Аппаратный слой - эмулятор шахты: датчики этажа отражают положение кабины, а
//! команда мотору это положение двигает. Без такой обратной связи автомат не
//! доедет никуда: `current_floor` он узнаёт только из датчиков.
//!
//! Кнопки - **моментальные** (нажал-отпустил), как в жизни. Это не украшение
//! сценария: с "залипшей" кнопкой этажа автомат уезжает и приезжает вечно -
//! отпустив её, мы проверяем, что он останавливается.

use std::cell::RefCell;
use std::rc::Rc;

use takt_generated::elevator_mini::{ElevatorMini, Hal, InBitPort, OutBitPort};

/// Эмулятор шахты.
#[derive(Default)]
struct Shaft {
    /// Этаж, на котором стоит кабина (1..=9).
    floor: u8,
    /// Нажатая кнопка вызова кабины (0 = ни одной).
    cabin_button: u8,
    /// Нажата кнопка "закрыть двери".
    close_button: bool,
    /// Двери открыты (последняя запись в порт `door_open`).
    door_open: bool,
    /// Команда мотору за такт: `Some(true)` = вверх, `Some(false)` = вниз,
    /// `None` = стоп.
    motor: Option<bool>,
    /// Этажи, через которые прошла кабина (без повторов подряд).
    visited: Vec<u8>,
}

struct Probe(Rc<RefCell<Shaft>>);

/// Тактов на сценарий: дорога с 1-го на 4-й занимает ~7, остальное - проверка
/// того, что кабина после этого стоит.
const TICKS: usize = 30;

impl Hal for Probe {
    fn read_bit(&mut self, port: InBitPort) -> bool {
        let s = self.0.borrow();
        match port {
            InBitPort::CabinButtonDc => s.close_button,
            InBitPort::CabinButtonF1 => s.cabin_button == 1,
            InBitPort::CabinButtonF2 => s.cabin_button == 2,
            InBitPort::CabinButtonF3 => s.cabin_button == 3,
            InBitPort::CabinButtonF4 => s.cabin_button == 4,
            InBitPort::CabinButtonF5 => s.cabin_button == 5,
            InBitPort::CabinButtonF6 => s.cabin_button == 6,
            InBitPort::CabinButtonF7 => s.cabin_button == 7,
            InBitPort::CabinButtonF8 => s.cabin_button == 8,
            InBitPort::CabinButtonF9 => s.cabin_button == 9,
            InBitPort::FloorSensorF1Bottom => s.floor == 1,
            InBitPort::FloorSensorF2Bottom => s.floor == 2,
            InBitPort::FloorSensorF3Bottom => s.floor == 3,
            InBitPort::FloorSensorF4Bottom => s.floor == 4,
            InBitPort::FloorSensorF5Bottom => s.floor == 5,
            InBitPort::FloorSensorF6Bottom => s.floor == 6,
            InBitPort::FloorSensorF7Bottom => s.floor == 7,
            InBitPort::FloorSensorF8Bottom => s.floor == 8,
            InBitPort::FloorSensorF9Bottom => s.floor == 9,
            // Этажные кнопки в сценарии не нажимаются; концевиков мотора нет -
            // мотор останавливает команда Stop от кабины.
            _ => false,
        }
    }

    fn write_bit(&mut self, port: OutBitPort, value: bool) {
        let mut s = self.0.borrow_mut();
        match port {
            OutBitPort::DoorOpen => s.door_open = value,
            OutBitPort::ElevatorMotorUp => {
                if value {
                    s.motor = Some(true);
                }
            }
            OutBitPort::ElevatorMotorDown => {
                if value {
                    s.motor = Some(false);
                }
            }
            OutBitPort::ElevatorMotorStop => {
                if value {
                    s.motor = None;
                }
            }
        }
    }
}

fn main() {
    let shaft = Rc::new(RefCell::new(Shaft {
        floor: 1,
        visited: vec![1],
        // Сценарий: пассажир на 1-м этаже жмёт "на 4-й".
        cabin_button: 4,
        ..Shaft::default()
    }));
    let mut m = ElevatorMini::new(Probe(Rc::clone(&shaft)));
    m.init();

    let mut moved = false;
    for _ in 0..TICKS {
        m.tick();

        let mut s = shaft.borrow_mut();
        // Физика шахты: команда мотору сдвигает кабину на этаж за такт.
        match s.motor {
            Some(true) if s.floor < 9 => s.floor += 1,
            Some(false) if s.floor > 1 => s.floor -= 1,
            _ => {}
        }
        let floor = s.floor;
        if s.visited.last() != Some(&floor) {
            s.visited.push(floor);
        }

        // Кабина тронулась - кнопку отпускаем (она моментальная).
        if s.motor.is_some() {
            moved = true;
            s.cabin_button = 0;
        }
        // Доехали и мотор встал - жмём "закрыть двери".
        if moved && s.motor.is_none() {
            s.close_button = true;
        }
    }

    let s = shaft.borrow();

    // Кабина поехала вверх и прошла все этажи по порядку, без "телепортов".
    assert_eq!(
        s.visited,
        [1, 2, 3, 4, 5],
        "кабина обязана пройти этажи по порядку снизу вверх"
    );
    // Автомат отреагировал на датчик 4-го этажа и снял команду мотору. Кабина
    // при этом стоит на 5-м: состояние `Up` мотора пишет `ElevatorMotorUp`
    // до проверки команды `Stop`, поэтому на такте прибытия мотор ещё крутится
    // - перелёт на этаж заложен в самой модели (то же и в цели `c`).
    assert_eq!(s.floor, 5, "модель тормозит с перелётом ровно на один этаж");
    assert!(s.motor.is_none(), "доехав, мотор обязан быть остановлен");
    assert!(s.door_open, "на этаже двери обязаны быть открыты");
    // Кабина стоит: после `AtFloor` + кнопка DC -> `Idle`, а `Idle` без нажатой
    // кнопки цели не назначает (`target_floor = 0`) и мотор не трогает.
    assert!(
        !m.is_done(),
        "автомат лифта не завершается — он обслуживает вызовы вечно"
    );

    println!(
        "elevator_mini: OK (1 → 4 по датчикам, перелёт на 5-й, двери открыты, кабина стоит)"
    );
}

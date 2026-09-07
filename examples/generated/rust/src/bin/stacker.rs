//! Проверка порождённого Rust: модель `stacker` (`examples/stacker.takt`).
//!
//! Аппаратный слой - эмулятор штабелятора: позиция едет к выданной цели по
//! одной оси за такт, вилы (`CmdFork`) переключают наличие груза. Без этой
//! обратной связи автомат стоит: цель он выдаёт в порты, а "доехал" узнаёт
//! только из портов позиции.
//!
//! Проверяются два сценария: обычная приёмка (задание доведено до конца) и
//! разряд батареи посреди дороги (задание брошено, штабелятор уехал на зарядку
//! и после неё принял задание снова - то есть аварийный путь не заклинивает
//! автомат).

use std::cell::RefCell;
use std::rc::Rc;

use takt_generated::stacker::{Hal, InBitPort, InU8Port, OutBitPort, OutU8Port, Stacker};

/// Эмулятор штабелятора.
#[derive(Default)]
struct Plant {
    // Положение (стек, ряд, секция).
    pos_stack: u8,
    pos_row: u8,
    pos_section: u8,
    // Цель, выданная автоматом в порты.
    tgt_stack: u8,
    tgt_row: u8,
    tgt_section: u8,
    /// Груз на вилах.
    loaded: bool,
    /// Задание доступно (`TaskValid`).
    task_valid: bool,
    /// Тип задания: `false` = приёмка (pickup -> ячейка), `true` = отгрузка.
    task_type: bool,
    // Адрес ячейки задания.
    task_stack: u8,
    task_row: u8,
    task_section: u8,
    /// Разряд батареи.
    battery_low: bool,
    // Наблюдаемые выходы.
    ack: bool,
    done: bool,
    fork: bool,
    /// Такты, на которых автомат поднял `CmdDone` (завершение задания).
    done_ticks: Vec<usize>,
    /// Такты, на которых автомат поднял `CmdAck` (задание принято).
    ack_ticks: Vec<usize>,
    /// Текущий такт - проставляет ведущий цикл.
    tick: usize,
}

impl Plant {
    /// Физика: едем к цели по одной оси за такт.
    fn step(&mut self) {
        fn approach(cur: &mut u8, tgt: u8) -> bool {
            match (*cur).cmp(&tgt) {
                std::cmp::Ordering::Less => {
                    *cur += 1;
                    true
                }
                std::cmp::Ordering::Greater => {
                    *cur -= 1;
                    true
                }
                std::cmp::Ordering::Equal => false,
            }
        }
        let _ = approach(&mut self.pos_stack, self.tgt_stack)
            || approach(&mut self.pos_row, self.tgt_row)
            || approach(&mut self.pos_section, self.tgt_section);
    }

    /// Место зарядки - начало координат (константы `CHARGE_*` модели).
    fn at_charge(&self) -> bool {
        self.pos_stack == 0 && self.pos_row == 0 && self.pos_section == 0
    }

    fn target_is_charge(&self) -> bool {
        self.tgt_stack == 0 && self.tgt_row == 0 && self.tgt_section == 0
    }
}

struct Probe(Rc<RefCell<Plant>>);

impl Hal for Probe {
    fn read_bit(&mut self, port: InBitPort) -> bool {
        let p = self.0.borrow();
        match port {
            InBitPort::SenseAtCharge => p.at_charge(),
            InBitPort::SenseBatteryLow => p.battery_low,
            InBitPort::SenseLoaded => p.loaded,
            InBitPort::TaskType => p.task_type,
            InBitPort::TaskValid => p.task_valid,
        }
    }

    fn read_u8(&mut self, port: InU8Port) -> u8 {
        let p = self.0.borrow();
        match port {
            InU8Port::PosRow => p.pos_row,
            InU8Port::PosSection => p.pos_section,
            InU8Port::PosStack => p.pos_stack,
            InU8Port::TaskRowNo => p.task_row,
            InU8Port::TaskSectionNo => p.task_section,
            InU8Port::TaskStackNo => p.task_stack,
        }
    }

    fn write_bit(&mut self, port: OutBitPort, value: bool) {
        let mut p = self.0.borrow_mut();
        match port {
            OutBitPort::CmdAck => {
                if value && !p.ack {
                    let t = p.tick;
                    p.ack_ticks.push(t);
                }
                p.ack = value;
            }
            OutBitPort::CmdDone => {
                if value && !p.done {
                    let t = p.tick;
                    p.done_ticks.push(t);
                }
                p.done = value;
            }
            OutBitPort::CmdFork => {
                // Вилы: по фронту подъём груза, если пусто, и опускание, если гружено.
                if value && !p.fork {
                    p.loaded = !p.loaded;
                }
                p.fork = value;
            }
        }
    }

    fn write_u8(&mut self, port: OutU8Port, value: u8) {
        let mut p = self.0.borrow_mut();
        match port {
            OutU8Port::CmdTargetRow => p.tgt_row = value,
            OutU8Port::CmdTargetSection => p.tgt_section = value,
            OutU8Port::CmdTargetStack => p.tgt_stack = value,
        }
    }
}

/// Задание: приёмка (тип 0) в ячейку (стек 5, ряд 1, секция 1).
fn task() -> Plant {
    Plant {
        task_valid: true,
        task_type: false,
        task_stack: 5,
        task_row: 1,
        task_section: 1,
        ..Plant::default()
    }
}

/// Обычная приёмка: задание принято, груз взят на пункте приёмки, отвезён в
/// ячейку и выгружен, задание закрыто, штабелятор вернулся на зарядку.
fn scenario_normal() {
    let plant = Rc::new(RefCell::new(task()));
    let mut m = Stacker::new(Probe(Rc::clone(&plant)));
    m.init();

    let mut loaded_at_pickup = false;
    for tick in 0..80 {
        plant.borrow_mut().tick = tick;
        m.tick();
        let mut p = plant.borrow_mut();
        p.step();
        // Груз взят там, где положено - на пункте приёмки (PICKUP_* = 0, 1, 1).
        if p.loaded && !loaded_at_pickup {
            assert_eq!(
                (p.pos_stack, p.pos_row, p.pos_section),
                (0, 1, 1),
                "груз обязан подниматься на пункте приёмки, а не на такте {tick}"
            );
            loaded_at_pickup = true;
        }
        // Задание одно: после подтверждения снимаем TaskValid.
        if p.ack {
            p.task_valid = false;
        }
    }

    let p = plant.borrow();
    assert_eq!(p.ack_ticks, [0], "задание принимается на первом же такте");
    assert_eq!(
        p.done_ticks,
        [12],
        "задание обязано закрыться ровно один раз, за 12 тактов"
    );
    assert!(loaded_at_pickup, "груз так и не был поднят");
    assert!(!p.loaded, "груз обязан быть выгружен в ячейку");
    assert!(
        p.at_charge(),
        "закрыв задание, штабелятор обязан вернуться на зарядку, он в ({}, {}, {})",
        p.pos_stack,
        p.pos_row,
        p.pos_section
    );

    println!("stacker: OK сценарий «приёмка» (задание закрыто за 12 тактов, возврат на зарядку)");
}

/// Разряд батареи на дороге к ячейке: задание бросается, штабелятор уезжает на
/// зарядку. После зарядки он обязан принять задание снова.
fn scenario_battery_low() {
    let plant = Rc::new(RefCell::new(task()));
    let mut m = Stacker::new(Probe(Rc::clone(&plant)));
    m.init();

    for tick in 0..80 {
        {
            let mut p = plant.borrow_mut();
            p.tick = tick;
            // Такт 6: штабелятор в дороге к ячейке - сажаем батарею.
            if tick == 6 {
                assert!(
                    !p.at_charge(),
                    "к такту 6 штабелятор обязан быть в дороге — иначе сценарий не про то"
                );
                p.battery_low = true;
            }
            // Такт 30: батарея заряжена, задание всё ещё висит.
            if tick == 30 {
                assert!(
                    p.at_charge(),
                    "разрядившись, штабелятор обязан доехать до зарядки к такту 30"
                );
                p.battery_low = false;
                // Груз брошенного задания снимает оператор: на зарядку
                // штабелятор уехал с паллетой на вилах, а новое задание он
                // начнёт с пункта приёмки - то есть попытается взять вторую.
                // Это ограничение самой модели, а не трансляции: аварийный
                // выход груз не выгружает.
                p.loaded = false;
            }
        }
        m.tick();
        let mut p = plant.borrow_mut();
        p.step();
        // Задание снимаем только после повторного приёма (после зарядки): до
        // него оно обязано висеть, иначе штабелятору не к чему возвращаться.
        // Считаем по фронтам `CmdAck`, а не по его уровню: подтверждение живёт
        // ровно один такт (`AcceptingTask` снимает его следующим же).
        if p.ack_ticks.len() >= 2 {
            p.task_valid = false;
        }
        // Пока батарея разряжена, задание не должно закрываться.
        if p.battery_low {
            assert!(
                !p.done,
                "на разряженной батарее задание не может быть закрыто (такт {tick})"
            );
            assert!(
                p.target_is_charge(),
                "на разряженной батарее цель — только зарядка, получено ({}, {}, {}) на такте {tick}",
                p.tgt_stack,
                p.tgt_row,
                p.tgt_section
            );
        }
    }

    let p = plant.borrow();
    assert_eq!(
        p.ack_ticks.len(),
        2,
        "задание принято дважды: до разряда и после зарядки, получено {:?}",
        p.ack_ticks
    );
    assert_eq!(
        p.done_ticks.len(),
        1,
        "закрыто задание ровно один раз — брошенное не считается, получено {:?}",
        p.done_ticks
    );
    assert!(
        p.done_ticks[0] > 30,
        "закрыто оно уже после зарядки (такт {})",
        p.done_ticks[0]
    );
    assert!(!p.loaded, "груз обязан быть выгружен в ячейку");

    println!("stacker: OK сценарий «разряд батареи» (задание брошено, зарядка, повторный приём)");
}

fn main() {
    scenario_normal();
    scenario_battery_low();
}

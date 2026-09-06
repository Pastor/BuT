// Порождено компилятором Takt (taktc) — цель: Rust (профиль no_std).
// Не редактировать вручную: файл перезаписывается при каждой генерации.

#![forbid(unsafe_code)]

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutBitPort {
    Ready,
}

pub trait Hal {
    fn write_bit(&mut self, port: OutBitPort, value: bool);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FloatRegulatorFloatRegulatorState {
    Init,
    Adjust,
    Done,
    Settled,
    End,
}

pub struct FloatRegulatorFloatRegulator {
    half: f64,
    near: f64,
    setpoint: f64,
    value: f64,
    state: FloatRegulatorFloatRegulatorState,
}

impl FloatRegulatorFloatRegulator {
    fn new() -> Self {
        Self {
            half: 0.5,
            near: 9.5,
            setpoint: 10.0,
            value: 0.0,
            state: FloatRegulatorFloatRegulatorState::Init,
        }
    }

    fn init(&mut self) {
        self.half = 0.5;
        self.near = 9.5;
        self.setpoint = 10.0;
        self.value = 0.0;
        self.state = FloatRegulatorFloatRegulatorState::Init;
    }

    fn tick<H: Hal>(&mut self, hal: &mut H) {
        if self.state == FloatRegulatorFloatRegulatorState::Init {
            self.state = FloatRegulatorFloatRegulatorState::Adjust;
        }
        match self.state {
            FloatRegulatorFloatRegulatorState::Adjust => {
                self.value += (self.setpoint - self.value) * self.half;
                if self.value >= self.near {
                    self.state = FloatRegulatorFloatRegulatorState::Settled;
                }
            }
            FloatRegulatorFloatRegulatorState::Done => {
                hal.write_bit(OutBitPort::Ready, true);
            }
            FloatRegulatorFloatRegulatorState::Settled => {
                self.value = self.setpoint;
                self.state = FloatRegulatorFloatRegulatorState::Done;
            }
            FloatRegulatorFloatRegulatorState::End => {}
            FloatRegulatorFloatRegulatorState::Init => {}
        }
    }

    fn is_done(&self) -> bool {
        self.state == FloatRegulatorFloatRegulatorState::End
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FloatRegulatorState {
    Init,
    Main,
    End,
}

pub struct FloatRegulator<H: Hal> {
    state: FloatRegulatorState,
    main: FloatRegulatorFloatRegulator,
    hal: H,
}

impl<H: Hal> FloatRegulator<H> {
    pub fn new(hal: H) -> Self {
        Self {
            state: FloatRegulatorState::Init,
            main: FloatRegulatorFloatRegulator::new(),
            hal,
        }
    }

    pub fn init(&mut self) {
        self.state = FloatRegulatorState::Init;
        self.main.init();
    }

    pub fn tick(&mut self) {
        if self.state == FloatRegulatorState::Init {
            self.state = FloatRegulatorState::Main;
        }
        match self.state {
            FloatRegulatorState::Main => {
                self.main.tick(&mut self.hal);
                if self.main.is_done() {
                    self.state = FloatRegulatorState::End;
                }
            }
            FloatRegulatorState::End => {}
            FloatRegulatorState::Init => {}
        }
    }

    pub fn reset(&mut self) {
        self.init();
    }

    pub fn is_done(&self) -> bool {
        self.state == FloatRegulatorState::End
    }

}


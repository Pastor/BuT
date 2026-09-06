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
enum BatchCycleDoseState {
    Init,
    Fill,
    Full,
    End,
}

pub struct BatchCycleDose {
    dosed: u8,
    state: BatchCycleDoseState,
}

impl BatchCycleDose {
    fn new() -> Self {
        Self {
            dosed: 0,
            state: BatchCycleDoseState::Init,
        }
    }

    fn init(&mut self) {
        self.dosed = 0;
        self.state = BatchCycleDoseState::Init;
    }

    fn tick(&mut self, shared: &mut BatchCycleShared) {
        if self.state == BatchCycleDoseState::Init {
            self.state = BatchCycleDoseState::Fill;
        }
        match self.state {
            BatchCycleDoseState::Fill => {
                shared.stage = 1;
                self.dosed = self.dosed.wrapping_add(1);
                if self.dosed >= 3 {
                    self.state = BatchCycleDoseState::Full;
                }
            }
            BatchCycleDoseState::Full => {
                self.state = BatchCycleDoseState::End;
            }
            BatchCycleDoseState::End => {}
            BatchCycleDoseState::Init => {}
        }
    }

    fn is_done(&self) -> bool {
        self.state == BatchCycleDoseState::End
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BatchCycleDrainState {
    Init,
    Dry,
    Empty,
    End,
}

pub struct BatchCycleDrain {
    drained: u8,
    state: BatchCycleDrainState,
}

impl BatchCycleDrain {
    fn new() -> Self {
        Self {
            drained: 0,
            state: BatchCycleDrainState::Init,
        }
    }

    fn init(&mut self) {
        self.drained = 0;
        self.state = BatchCycleDrainState::Init;
    }

    fn tick(&mut self, shared: &mut BatchCycleShared) {
        if self.state == BatchCycleDrainState::Init {
            self.state = BatchCycleDrainState::Empty;
        }
        match self.state {
            BatchCycleDrainState::Dry => {
                self.state = BatchCycleDrainState::End;
            }
            BatchCycleDrainState::Empty => {
                shared.stage = 3;
                self.drained = self.drained.wrapping_add(1);
                if self.drained >= 2 {
                    self.state = BatchCycleDrainState::Dry;
                }
            }
            BatchCycleDrainState::End => {}
            BatchCycleDrainState::Init => {}
        }
    }

    fn is_done(&self) -> bool {
        self.state == BatchCycleDrainState::End
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BatchCycleMixState {
    Init,
    Blended,
    Stir,
    End,
}

pub struct BatchCycleMix {
    stirred: u8,
    state: BatchCycleMixState,
}

impl BatchCycleMix {
    fn new() -> Self {
        Self {
            stirred: 0,
            state: BatchCycleMixState::Init,
        }
    }

    fn init(&mut self) {
        self.stirred = 0;
        self.state = BatchCycleMixState::Init;
    }

    fn tick(&mut self, shared: &mut BatchCycleShared) {
        if self.state == BatchCycleMixState::Init {
            self.state = BatchCycleMixState::Stir;
        }
        match self.state {
            BatchCycleMixState::Blended => {
                self.state = BatchCycleMixState::End;
            }
            BatchCycleMixState::Stir => {
                shared.stage = 2;
                self.stirred = self.stirred.wrapping_add(1);
                if self.stirred >= 2 {
                    self.state = BatchCycleMixState::Blended;
                }
            }
            BatchCycleMixState::End => {}
            BatchCycleMixState::Init => {}
        }
    }

    fn is_done(&self) -> bool {
        self.state == BatchCycleMixState::End
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BatchCycleState {
    Init,
    Cycle,
    Done,
    End,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BatchCycleCycleSeq {
    Dose0,
    Mix1,
    Drain2,
}

struct BatchCycleShared {
    stage: u8,
}

pub struct BatchCycle<H: Hal> {
    shared: BatchCycleShared,
    state: BatchCycleState,
    cycle_seq: BatchCycleCycleSeq,
    cycle_dose0: BatchCycleDose,
    cycle_mix1: BatchCycleMix,
    cycle_drain2: BatchCycleDrain,
    hal: H,
}

impl<H: Hal> BatchCycle<H> {
    pub fn new(hal: H) -> Self {
        Self {
            shared: BatchCycleShared {
                stage: 0,
            },
            state: BatchCycleState::Init,
            cycle_seq: BatchCycleCycleSeq::Dose0,
            cycle_dose0: BatchCycleDose::new(),
            cycle_mix1: BatchCycleMix::new(),
            cycle_drain2: BatchCycleDrain::new(),
            hal,
        }
    }

    pub fn init(&mut self) {
        self.shared.stage = 0;
        self.state = BatchCycleState::Init;
        self.cycle_seq = BatchCycleCycleSeq::Dose0;
        self.cycle_dose0.init();
        self.cycle_mix1.init();
        self.cycle_drain2.init();
    }

    pub fn tick(&mut self) {
        if self.state == BatchCycleState::Init {
            self.state = BatchCycleState::Cycle;
        }
        match self.state {
            BatchCycleState::Cycle => {
                if self.cycle_seq == BatchCycleCycleSeq::Dose0 {
                    self.cycle_dose0.tick(&mut self.shared);
                    if self.cycle_dose0.is_done() {
                        self.cycle_mix1.init();
                        self.cycle_seq = BatchCycleCycleSeq::Mix1;
                    }
                } else if self.cycle_seq == BatchCycleCycleSeq::Mix1 {
                    self.cycle_mix1.tick(&mut self.shared);
                    if self.cycle_mix1.is_done() {
                        self.cycle_drain2.init();
                        self.cycle_seq = BatchCycleCycleSeq::Drain2;
                    }
                } else if self.cycle_seq == BatchCycleCycleSeq::Drain2 {
                    self.cycle_drain2.tick(&mut self.shared);
                    if self.cycle_drain2.is_done() {
                        self.state = BatchCycleState::Done;
                    }
                }
            }
            BatchCycleState::Done => {
                self.hal.write_bit(OutBitPort::Ready, true);
            }
            BatchCycleState::End => {}
            BatchCycleState::Init => {}
        }
    }

    pub fn reset(&mut self) {
        self.init();
    }

    pub fn is_done(&self) -> bool {
        self.state == BatchCycleState::End
    }

}


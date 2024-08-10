use crate::{Engine, PathDecorator};
use std::cell::RefCell;

#[derive(PartialEq, Debug, Copy, Clone)]
enum State {
    One,
    Two,
    Three,
    Four,
}

#[derive(PartialEq, Debug, Copy, Clone)]
struct Input(i64);

#[derive(Default, PartialEq, Eq, Debug, Copy, Clone)]
struct Output(i64);

struct TableEngine {
    state: RefCell<State>,
}

impl Default for TableEngine {
    fn default() -> Self {
        Self {
            state: RefCell::new(State::One)
        }
    }
}

impl TableEngine {
    fn new() -> Self {
        Default::default()
    }
}

impl Engine for TableEngine {
    type State = State;
    type Input = Input;
    type Output = Output;

    fn current_state(&self) -> Self::State {
        *self.state.borrow()
    }

    fn tick(&mut self, input: Self::Input) -> Self::Output {
        let (state, output) = match *self.state.borrow() {
            State::One if input.0 > 0 => (State::Four, Output(1)),
            State::Two => (State::Two, Output(0)),
            State::Three => (State::Three, Output(0)),
            State::Four => (State::Four, Output(0)),
            state => (state, Output(0))
        };
        self.state.replace(state);
        output
    }
}

#[cfg(test)]
mod test {
    use super::{Engine, Input, Output, PathDecorator, State, TableEngine};

    #[test]
    fn test_tick() {
        let engine: TableEngine = Default::default();
        let decorator = &mut PathDecorator::new(engine);
        let output = decorator.tick(Input(10));
        assert_eq!(output, Output(1));
        assert_eq!(decorator.current_state(), State::Four);
    }
}

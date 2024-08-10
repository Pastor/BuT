mod sample;


trait Engine {
    type State: Clone + Copy + Sized;
    type Input: Clone + Copy + Sized;
    type Output: Clone + Copy + Sized;

    fn current_state(&self) -> Self::State;
    fn tick(&mut self, input: Self::Input) -> Self::Output;
}

trait EngineMatrix<Input: Copy, Output: Copy, State: Copy> {
    type NextPredicateFn: Fn(Input) -> bool;
    type NextStateFn: FnMut(Input, State) -> State;
    type NextOutputFn: FnMut(Input, State) -> Output;
    type TickOutputFn: FnMut(Input, State) -> Output;
    fn register(&mut self, state: State, next: Self::NextPredicateFn, next_state: State, next_output: Output);
    fn next(&self, input: Input, state: State) -> (State, Output);
}

struct PathDecorator<T: Engine> {
    engine: T,
    paths: Vec<(T::State, T::Input, T::State, T::Output)>,
}

impl<T: Engine> Engine for PathDecorator<T> {
    type State = T::State;
    type Input = T::Input;
    type Output = T::Output;

    fn current_state(&self) -> Self::State {
        self.engine.current_state()
    }

    fn tick(&mut self, input: Self::Input) -> Self::Output {
        let state0 = self.engine.current_state();
        let output = self.engine.tick(input);
        let state1 = self.engine.current_state();
        self.paths.push((state0, input, state1, output));
        output
    }
}

impl<T: Engine> PathDecorator<T> {
    fn new(engine: T) -> Self {
        Self {
            engine,
            paths: vec![],
        }
    }
}

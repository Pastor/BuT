pub mod builder;
pub mod context;
pub mod executor;
pub mod expr;
pub mod ltl;
pub mod machine;
pub mod value;

pub use builder::{build_all, build_machine, BuildError};
pub use context::SimContext;
pub use executor::Simulator;
pub use machine::{Machine, MachineKind, State, Transition};
pub use value::Value;

#[cfg(test)]
mod tests {
    use but_grammar::parse;
    use crate::builder::build_all;
    use crate::executor::Simulator;
    use crate::value::Value;

    fn simulate_source(src: &str, steps: usize, port_name: &str, port_val: Value) -> String {
        let (unit, _) = parse(src, 0).expect("ошибка разбора");
        let machines = build_all(&unit).expect("ошибка построения");
        let mut sim = Simulator::new(machines.into_iter().next().expect("нет автомата"));
        sim.set_port(port_name, port_val);
        sim.run_n(steps);
        sim.current_state().unwrap_or("").to_string()
    }

    #[test]
    fn test_delay_none_to_one() {
        let src = r#"
model Delay {
    state One {
        ref None: input = 0;
        ref One : input = 1;
        enter -> { output = 1; }
    }
    state None {
        ref One : input = 1;
        ref None: input = 0;
        enter -> { output = 0; }
    }
    state End { }
    start -> None;
}
port input : bit = 0x01;
port output: bit = 0x02;
"#;
        // Начало в None, input=1 → переход в One
        let state = simulate_source(src, 1, "input", Value::Int(1));
        assert_eq!(state, "One");
    }

    #[test]
    fn test_delay_stays_in_none() {
        let src = r#"
model Delay {
    state One {
        ref None: input = 0;
        ref One : input = 1;
        enter -> { output = 1; }
    }
    state None {
        ref One : input = 1;
        ref None: input = 0;
        enter -> { output = 0; }
    }
    state End { }
    start -> None;
}
port input : bit = 0x01;
port output: bit = 0x02;
"#;
        // Начало в None, input=0 → остаётся в None
        let state = simulate_source(src, 1, "input", Value::Int(0));
        assert_eq!(state, "None");
    }
}

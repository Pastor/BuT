use crate::context::SimContext;
use crate::expr::{eval_condition, exec_statement};
use crate::ltl::print_ltl_info;
use crate::machine::{Machine, MachineKind};
use crate::value::Value;

/// FSM simulator: executes a `MachineKind` step by step.
pub struct Simulator {
    pub machine: MachineKind,
}

impl Simulator {
    pub fn new(machine: MachineKind) -> Self {
        Self { machine }
    }

    /// Execute one simulation step.
    /// Returns `true` if a state transition occurred.
    pub fn step(&mut self) -> bool {
        step_machine(&mut self.machine)
    }

    /// Execute up to `n` steps. Returns the number of steps with transitions.
    pub fn run_n(&mut self, n: usize) -> usize {
        let mut transitions = 0;
        for _ in 0..n {
            if self.step() {
                transitions += 1;
            }
        }
        transitions
    }

    /// Get the current state of the top-level machine.
    pub fn current_state(&self) -> Option<&str> {
        self.machine.current_state()
    }

    /// Get an immutable reference to the execution context.
    pub fn context(&self) -> Option<&SimContext> {
        self.machine.context()
    }

    /// Set a port value (external input).
    pub fn set_port(&mut self, name: &str, val: Value) {
        if let Some(ctx) = self.machine.context_mut() {
            ctx.set_port(name, val);
        }
    }

    /// Print the current state of the machine.
    pub fn print_state(&self) {
        match &self.machine {
            MachineKind::Single(m) => print_machine_state(m),
            MachineKind::Sequence(ms) => {
                for m in ms {
                    if let MachineKind::Single(machine) = m {
                        print_machine_state(machine);
                    }
                }
            }
            MachineKind::Parallel(ms) => {
                for m in ms {
                    if let MachineKind::Single(machine) = m {
                        print_machine_state(machine);
                    }
                }
            }
        }
    }

    /// Print LTL specifications for all machines.
    pub fn print_ltl(&self) {
        self.for_each_machine(|m| {
            print_ltl_info(&m.name, &m.ltl_formulas);
        });
    }

    /// Visit all single machines.
    fn for_each_machine(&self, mut f: impl FnMut(&Machine)) {
        visit_machine(&self.machine, &mut f);
    }
}

fn visit_machine(mk: &MachineKind, f: &mut impl FnMut(&Machine)) {
    match mk {
        MachineKind::Single(m) => f(m),
        MachineKind::Sequence(ms) | MachineKind::Parallel(ms) => {
            for m in ms {
                visit_machine(m, f);
            }
        }
    }
}

fn print_machine_state(m: &Machine) {
    println!("[{}] state = {}", m.name, m.current);
    for (k, v) in m.context.vars() {
        println!("  var {}: {}", k, v);
    }
    for (k, v) in m.context.ports() {
        println!("  port {}: {}", k, v);
    }
}

/// Recursively execute one step for a MachineKind.
fn step_machine(mk: &mut MachineKind) -> bool {
    match mk {
        MachineKind::Single(m) => step_single(m),
        MachineKind::Sequence(ms) => {
            // Execute the first unfinished machine
            for m in ms.iter_mut() {
                if step_machine(m) {
                    return true;
                }
            }
            false
        }
        MachineKind::Parallel(ms) => {
            // Execute all machines simultaneously
            let mut any = false;
            for m in ms.iter_mut() {
                if step_machine(m) {
                    any = true;
                }
            }
            any
        }
    }
}

/// Execute one step for a single machine.
fn step_single(m: &mut Machine) -> bool {
    // Execute global enter handlers (every tick)
    let model_enter = m.model_enter.clone();
    for stmt in &model_enter {
        let _ = exec_statement(stmt, &mut m.context);
    }

    let current_name = m.current.clone();

    // Get the enter handlers for the current state (every tick)
    let state_enter = m
        .states
        .get(&current_name)
        .map(|s| s.enter.clone())
        .unwrap_or_default();

    for stmt in &state_enter {
        let _ = exec_statement(stmt, &mut m.context);
    }

    // Evaluate transitions
    let transitions = m
        .states
        .get(&current_name)
        .map(|s| s.transitions.clone())
        .unwrap_or_default();

    for tr in &transitions {
        let should_transition = match &tr.condition {
            None => true,
            Some(cond) => eval_condition(cond, &m.context)
                .map(|v| v.is_truthy())
                .unwrap_or(false),
        };

        if should_transition {
            // Execute exit handlers
            let exit_stmts = m
                .states
                .get(&current_name)
                .map(|s| s.exit.clone())
                .unwrap_or_default();
            for stmt in &exit_stmts {
                let _ = exec_statement(stmt, &mut m.context);
            }

            // Execute before handlers of the target state
            let before_stmts = m
                .states
                .get(&tr.target)
                .map(|s| s.before.clone())
                .unwrap_or_default();
            for stmt in &before_stmts {
                let _ = exec_statement(stmt, &mut m.context);
            }

            // Perform the transition
            let target = tr.target.clone();
            m.current = target;
            return true;
        }
    }

    false
}

use crate::context::SimContext;
use crate::expr::{eval_condition, exec_statement};
use crate::ltl::print_ltl_info;
use crate::machine::{Machine, MachineKind};
use crate::value::Value;

/// Симулятор FSM: пошагово выполняет `MachineKind`.
pub struct Simulator {
    pub machine: MachineKind,
}

impl Simulator {
    pub fn new(machine: MachineKind) -> Self {
        Self { machine }
    }

    /// Выполнить один шаг симуляции.
    /// Возвращает `true`, если произошёл переход состояния.
    pub fn step(&mut self) -> bool {
        step_machine(&mut self.machine)
    }

    /// Выполнить до `n` шагов. Возвращает количество шагов с переходами.
    pub fn run_n(&mut self, n: usize) -> usize {
        let mut transitions = 0;
        for _ in 0..n {
            if self.step() {
                transitions += 1;
            }
        }
        transitions
    }

    /// Получить текущее состояние верхнего уровня автомата.
    pub fn current_state(&self) -> Option<&str> {
        self.machine.current_state()
    }

    /// Получить неизменяемую ссылку на контекст выполнения.
    pub fn context(&self) -> Option<&SimContext> {
        self.machine.context()
    }

    /// Установить значение порта (внешний ввод).
    pub fn set_port(&mut self, name: &str, val: Value) {
        if let Some(ctx) = self.machine.context_mut() {
            ctx.set_port(name, val);
        }
    }

    /// Напечатать текущее состояние автомата.
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

    /// Напечатать LTL-спецификации всех автоматов.
    pub fn print_ltl(&self) {
        self.for_each_machine(|m| {
            print_ltl_info(&m.name, &m.ltl_formulas);
        });
    }

    /// Обойти все одиночные автоматы.
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

/// Рекурсивно выполнить шаг для MachineKind.
fn step_machine(mk: &mut MachineKind) -> bool {
    match mk {
        MachineKind::Single(m) => step_single(m),
        MachineKind::Sequence(ms) => {
            // Выполнить первый незавершённый автомат
            for m in ms.iter_mut() {
                if step_machine(m) {
                    return true;
                }
            }
            false
        }
        MachineKind::Parallel(ms) => {
            // Выполнить все автоматы одновременно
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

/// Выполнить один шаг для одиночного автомата.
fn step_single(m: &mut Machine) -> bool {
    // Выполнить глобальные обработчики enter (каждый такт)
    let model_enter = m.model_enter.clone();
    for stmt in &model_enter {
        let _ = exec_statement(stmt, &mut m.context);
    }

    let current_name = m.current.clone();

    // Получить обработчики enter текущего состояния (каждый такт)
    let state_enter = m
        .states
        .get(&current_name)
        .map(|s| s.enter.clone())
        .unwrap_or_default();

    for stmt in &state_enter {
        let _ = exec_statement(stmt, &mut m.context);
    }

    // Вычислить переходы
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
            // Выполнить обработчики exit
            let exit_stmts = m
                .states
                .get(&current_name)
                .map(|s| s.exit.clone())
                .unwrap_or_default();
            for stmt in &exit_stmts {
                let _ = exec_statement(stmt, &mut m.context);
            }

            // Выполнить обработчики before целевого состояния
            let before_stmts = m
                .states
                .get(&tr.target)
                .map(|s| s.before.clone())
                .unwrap_or_default();
            for stmt in &before_stmts {
                let _ = exec_statement(stmt, &mut m.context);
            }

            // Выполнить переход
            let target = tr.target.clone();
            m.current = target;
            return true;
        }
    }

    false
}

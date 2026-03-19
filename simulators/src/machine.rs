use but_grammar::ast;
use indexmap::IndexMap;
use crate::context::SimContext;

/// Transition between states (a `ref` declaration).
#[derive(Debug, Clone)]
pub struct Transition {
    /// Name of the target state.
    pub target: String,
    /// Transition condition. `None` means unconditional (always true).
    pub condition: Option<ast::Condition>,
}

/// A state machine state at runtime.
#[derive(Debug, Clone)]
pub struct State {
    /// State name.
    pub name: String,
    /// Outgoing transitions (ref declarations).
    pub transitions: Vec<Transition>,
    /// Handlers executed every tick while in this state.
    pub enter: Vec<ast::Statement>,
    /// Handlers executed when leaving this state.
    pub exit: Vec<ast::Statement>,
    /// Handlers executed before transitioning into this state.
    pub before: Vec<ast::Statement>,
    /// Nested sub-machine (for composite states).
    pub submachine: Option<Box<MachineKind>>,
}

impl State {
    pub fn new(name: String) -> Self {
        Self {
            name,
            transitions: vec![],
            enter: vec![],
            exit: vec![],
            before: vec![],
            submachine: None,
        }
    }
}

/// A concrete finite state machine instance.
#[derive(Debug, Clone)]
pub struct Machine {
    /// Machine name.
    pub name: String,
    /// All states indexed by name.
    pub states: IndexMap<String, State>,
    /// Current state.
    pub current: String,
    /// Execution context (variables + ports).
    pub context: SimContext,
    /// Global enter handlers (executed every tick).
    pub model_enter: Vec<ast::Statement>,
    /// Global machine completion handlers.
    pub model_end: Vec<ast::Statement>,
    /// LTL formulas extracted from annotations and formula blocks.
    pub ltl_formulas: Vec<String>,
}

impl Machine {
    pub fn new(name: String, start: String, context: SimContext) -> Self {
        Self {
            name,
            states: IndexMap::new(),
            current: start,
            context,
            model_enter: vec![],
            model_end: vec![],
            ltl_formulas: vec![],
        }
    }

    pub fn current_state(&self) -> Option<&State> {
        self.states.get(&self.current)
    }
}

/// A machine can be single, sequential, or parallel.
#[derive(Debug, Clone)]
pub enum MachineKind {
    /// A single flat finite state machine.
    Single(Machine),
    /// Sequential composition: machines execute one after another.
    Sequence(Vec<MachineKind>),
    /// Parallel composition: machines execute simultaneously.
    Parallel(Vec<MachineKind>),
}

impl MachineKind {
    /// Get the name of the current state (for single machines).
    pub fn current_state(&self) -> Option<&str> {
        match self {
            MachineKind::Single(m) => Some(m.current.as_str()),
            MachineKind::Sequence(ms) => ms.first().and_then(|m| m.current_state()),
            MachineKind::Parallel(ms) => ms.first().and_then(|m| m.current_state()),
        }
    }

    /// Get the machine name (for single machines).
    pub fn name(&self) -> Option<&str> {
        match self {
            MachineKind::Single(m) => Some(m.name.as_str()),
            _ => None,
        }
    }

    /// Get a mutable reference to the context (for single machines).
    pub fn context_mut(&mut self) -> Option<&mut SimContext> {
        match self {
            MachineKind::Single(m) => Some(&mut m.context),
            _ => None,
        }
    }

    /// Get a reference to the context (for single machines).
    pub fn context(&self) -> Option<&SimContext> {
        match self {
            MachineKind::Single(m) => Some(&m.context),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper: create an empty context
    fn empty_context() -> SimContext {
        SimContext::new()
    }

    // --- State::new ---

    #[test]
    fn state_new_name_is_stored() {
        let s = State::new("Idle".to_string());
        assert_eq!(s.name, "Idle");
    }

    #[test]
    fn state_new_transitions_are_empty() {
        let s = State::new("A".to_string());
        assert!(s.transitions.is_empty());
    }

    #[test]
    fn state_new_handlers_are_empty() {
        let s = State::new("B".to_string());
        assert!(s.enter.is_empty());
        assert!(s.exit.is_empty());
        assert!(s.before.is_empty());
    }

    #[test]
    fn state_new_submachine_is_absent() {
        let s = State::new("C".to_string());
        assert!(s.submachine.is_none());
    }

    // --- Machine::new ---

    #[test]
    fn machine_new_name_is_stored() {
        let m = Machine::new("Delay".to_string(), "None".to_string(), empty_context());
        assert_eq!(m.name, "Delay");
    }

    #[test]
    fn machine_new_initial_state_is_stored() {
        let m = Machine::new("X".to_string(), "Start".to_string(), empty_context());
        assert_eq!(m.current, "Start");
    }

    #[test]
    fn machine_new_states_are_empty() {
        let m = Machine::new("X".to_string(), "S0".to_string(), empty_context());
        assert!(m.states.is_empty());
    }

    #[test]
    fn machine_new_handlers_are_empty() {
        let m = Machine::new("X".to_string(), "S0".to_string(), empty_context());
        assert!(m.model_enter.is_empty());
        assert!(m.model_end.is_empty());
    }

    #[test]
    fn machine_new_ltl_formulas_are_empty() {
        let m = Machine::new("X".to_string(), "S0".to_string(), empty_context());
        assert!(m.ltl_formulas.is_empty());
    }

    #[test]
    fn machine_current_state_returns_none_when_no_states() {
        let m = Machine::new("X".to_string(), "Ghost".to_string(), empty_context());
        assert!(m.current_state().is_none());
    }

    #[test]
    fn machine_current_state_returns_state() {
        let mut m = Machine::new("X".to_string(), "Active".to_string(), empty_context());
        m.states.insert("Active".to_string(), State::new("Active".to_string()));
        let s = m.current_state();
        assert!(s.is_some());
        assert_eq!(s.unwrap().name, "Active");
    }

    // --- MachineKind::current_state ---

    #[test]
    fn machinekind_single_current_state() {
        let m = Machine::new("M".to_string(), "Running".to_string(), empty_context());
        let kind = MachineKind::Single(m);
        assert_eq!(kind.current_state(), Some("Running"));
    }

    #[test]
    fn machinekind_sequence_current_state_from_first() {
        let m1 = Machine::new("A".to_string(), "S1".to_string(), empty_context());
        let m2 = Machine::new("B".to_string(), "S2".to_string(), empty_context());
        let seq = MachineKind::Sequence(vec![
            MachineKind::Single(m1),
            MachineKind::Single(m2),
        ]);
        assert_eq!(seq.current_state(), Some("S1"));
    }

    #[test]
    fn machinekind_sequence_empty_returns_none() {
        let seq = MachineKind::Sequence(vec![]);
        assert_eq!(seq.current_state(), None);
    }

    #[test]
    fn machinekind_parallel_current_state_from_first() {
        let m1 = Machine::new("P".to_string(), "PA".to_string(), empty_context());
        let par = MachineKind::Parallel(vec![MachineKind::Single(m1)]);
        assert_eq!(par.current_state(), Some("PA"));
    }

    #[test]
    fn machinekind_parallel_empty_returns_none() {
        let par = MachineKind::Parallel(vec![]);
        assert_eq!(par.current_state(), None);
    }

    // --- MachineKind::name ---

    #[test]
    fn machinekind_single_name_returns_name() {
        let m = Machine::new("Delay".to_string(), "None".to_string(), empty_context());
        let kind = MachineKind::Single(m);
        assert_eq!(kind.name(), Some("Delay"));
    }

    #[test]
    fn machinekind_sequence_name_none() {
        let seq = MachineKind::Sequence(vec![]);
        assert_eq!(seq.name(), None);
    }

    #[test]
    fn machinekind_parallel_name_none() {
        let par = MachineKind::Parallel(vec![]);
        assert_eq!(par.name(), None);
    }
}

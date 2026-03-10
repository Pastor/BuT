use but_grammar::ast;
use indexmap::IndexMap;
use crate::context::SimContext;

/// Переход между состояниями (объявление ref).
#[derive(Debug, Clone)]
pub struct Transition {
    /// Имя целевого состояния.
    pub target: String,
    /// Условие перехода. `None` — безусловный переход (всегда истинно).
    pub condition: Option<ast::Condition>,
}

/// Состояние автомата в режиме выполнения.
#[derive(Debug, Clone)]
pub struct State {
    /// Имя состояния.
    pub name: String,
    /// Исходящие переходы (объявления ref).
    pub transitions: Vec<Transition>,
    /// Обработчики, выполняемые на каждом такте в этом состоянии.
    pub enter: Vec<ast::Statement>,
    /// Обработчики, выполняемые при выходе из состояния.
    pub exit: Vec<ast::Statement>,
    /// Обработчики, выполняемые перед переходом в новое состояние.
    pub before: Vec<ast::Statement>,
    /// Вложенный подавтомат (для составных состояний).
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

/// Конкретный экземпляр конечного автомата.
#[derive(Debug, Clone)]
pub struct Machine {
    /// Имя автомата.
    pub name: String,
    /// Все состояния, индексированные по имени.
    pub states: IndexMap<String, State>,
    /// Текущее состояние.
    pub current: String,
    /// Контекст выполнения (переменные + порты).
    pub context: SimContext,
    /// Глобальные обработчики enter (выполняются на каждом такте).
    pub model_enter: Vec<ast::Statement>,
    /// Глобальные обработчики завершения автомата.
    pub model_end: Vec<ast::Statement>,
    /// LTL-формулы, извлечённые из аннотаций и блоков formula.
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

/// Автомат может быть одиночным, последовательным или параллельным.
#[derive(Debug, Clone)]
pub enum MachineKind {
    /// Одиночный плоский конечный автомат.
    Single(Machine),
    /// Последовательная композиция: автоматы выполняются один за другим.
    Sequence(Vec<MachineKind>),
    /// Параллельная композиция: автоматы выполняются одновременно.
    Parallel(Vec<MachineKind>),
}

impl MachineKind {
    /// Получить имя текущего состояния (для одиночных автоматов).
    pub fn current_state(&self) -> Option<&str> {
        match self {
            MachineKind::Single(m) => Some(m.current.as_str()),
            MachineKind::Sequence(ms) => ms.first().and_then(|m| m.current_state()),
            MachineKind::Parallel(ms) => ms.first().and_then(|m| m.current_state()),
        }
    }

    /// Получить имя автомата (для одиночных автоматов).
    pub fn name(&self) -> Option<&str> {
        match self {
            MachineKind::Single(m) => Some(m.name.as_str()),
            _ => None,
        }
    }

    /// Получить изменяемую ссылку на контекст (для одиночных автоматов).
    pub fn context_mut(&mut self) -> Option<&mut SimContext> {
        match self {
            MachineKind::Single(m) => Some(&mut m.context),
            _ => None,
        }
    }

    /// Получить ссылку на контекст (для одиночных автоматов).
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

    // Вспомогательная функция: создать пустой контекст
    fn пустой_контекст() -> SimContext {
        SimContext::new()
    }

    // --- State::new ---

    #[test]
    fn state_new_имя_сохраняется() {
        let s = State::new("Idle".to_string());
        assert_eq!(s.name, "Idle");
    }

    #[test]
    fn state_new_переходы_пусты() {
        let s = State::new("A".to_string());
        assert!(s.transitions.is_empty());
    }

    #[test]
    fn state_new_обработчики_пусты() {
        let s = State::new("B".to_string());
        assert!(s.enter.is_empty());
        assert!(s.exit.is_empty());
        assert!(s.before.is_empty());
    }

    #[test]
    fn state_new_подавтомат_отсутствует() {
        let s = State::new("C".to_string());
        assert!(s.submachine.is_none());
    }

    // --- Machine::new ---

    #[test]
    fn machine_new_имя_сохраняется() {
        let m = Machine::new("Delay".to_string(), "None".to_string(), пустой_контекст());
        assert_eq!(m.name, "Delay");
    }

    #[test]
    fn machine_new_начальное_состояние_сохраняется() {
        let m = Machine::new("X".to_string(), "Start".to_string(), пустой_контекст());
        assert_eq!(m.current, "Start");
    }

    #[test]
    fn machine_new_состояния_пусты() {
        let m = Machine::new("X".to_string(), "S0".to_string(), пустой_контекст());
        assert!(m.states.is_empty());
    }

    #[test]
    fn machine_new_обработчики_пусты() {
        let m = Machine::new("X".to_string(), "S0".to_string(), пустой_контекст());
        assert!(m.model_enter.is_empty());
        assert!(m.model_end.is_empty());
    }

    #[test]
    fn machine_new_ltl_формулы_пусты() {
        let m = Machine::new("X".to_string(), "S0".to_string(), пустой_контекст());
        assert!(m.ltl_formulas.is_empty());
    }

    #[test]
    fn machine_current_state_возвращает_none_если_нет_состояний() {
        let m = Machine::new("X".to_string(), "Ghost".to_string(), пустой_контекст());
        assert!(m.current_state().is_none());
    }

    #[test]
    fn machine_current_state_возвращает_состояние() {
        let mut m = Machine::new("X".to_string(), "Active".to_string(), пустой_контекст());
        m.states.insert("Active".to_string(), State::new("Active".to_string()));
        let s = m.current_state();
        assert!(s.is_some());
        assert_eq!(s.unwrap().name, "Active");
    }

    // --- MachineKind::current_state ---

    #[test]
    fn machinekind_single_current_state() {
        let m = Machine::new("M".to_string(), "Running".to_string(), пустой_контекст());
        let kind = MachineKind::Single(m);
        assert_eq!(kind.current_state(), Some("Running"));
    }

    #[test]
    fn machinekind_sequence_current_state_из_первого() {
        let m1 = Machine::new("A".to_string(), "S1".to_string(), пустой_контекст());
        let m2 = Machine::new("B".to_string(), "S2".to_string(), пустой_контекст());
        let seq = MachineKind::Sequence(vec![
            MachineKind::Single(m1),
            MachineKind::Single(m2),
        ]);
        assert_eq!(seq.current_state(), Some("S1"));
    }

    #[test]
    fn machinekind_sequence_пустая_возвращает_none() {
        let seq = MachineKind::Sequence(vec![]);
        assert_eq!(seq.current_state(), None);
    }

    #[test]
    fn machinekind_parallel_current_state_из_первого() {
        let m1 = Machine::new("P".to_string(), "PA".to_string(), пустой_контекст());
        let par = MachineKind::Parallel(vec![MachineKind::Single(m1)]);
        assert_eq!(par.current_state(), Some("PA"));
    }

    #[test]
    fn machinekind_parallel_пустая_возвращает_none() {
        let par = MachineKind::Parallel(vec![]);
        assert_eq!(par.current_state(), None);
    }

    // --- MachineKind::name ---

    #[test]
    fn machinekind_single_name_возвращает_имя() {
        let m = Machine::new("Delay".to_string(), "None".to_string(), пустой_контекст());
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

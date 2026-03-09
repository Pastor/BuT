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

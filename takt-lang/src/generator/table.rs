//! Отношение переходов модели - Один носитель на все цели.
//!
//! # Что решает цель, а что - носитель
//!
//! Носитель отвечает на вопрос "какие строки и в каком порядке". Цель отвечает на
//! вопросы своего языка: как назвать состояние, как выразить предикат завершения
//! реализации ([`Row::done`]) и как напечатать условие ребра. Реализацию носитель
//! отдаёт **как есть** ([`StateExtend`]) - предикат у каждой цели свой:
//! `M_is_done(&model->x)` в C, `self.x.is_done()` в Rust, `x.is_done` в ST.

use std::cell::RefCell;
use std::rc::Rc;

use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::minimap::{Element, Name, StateExtend};
use crate::semantic::{ConditionNode, StateNode};

/// Источник состояний модели: у каждой цели своя карта, а вопросы к ней одни.
pub(crate) trait StateSource {
    /// Элемент карты по имени состояния; `None` - состояние недостижимо.
    fn state_element(&self, name: Name) -> Option<Element>;
    /// Узел состояния по имени.
    fn state_node(&self, name: Name) -> Result<Rc<RefCell<StateNode>>, Diagnostic>;
}

/// Куда ведёт строка таблицы.
pub(crate) enum RowTarget {
    /// Именованное состояние модели.
    State(Name),
    /// Терминальное состояние модели (`END`).
    End,
}

/// Строка таблицы переходов.
pub(crate) struct Row {
    /// Состояние-источник.
    pub(crate) from: Name,
    /// Реализация состояния-источника: её завершение - часть стража.
    ///
    /// `None` у простого состояния: завершать в нём нечего. Предикат печатает цель -
    /// форма у каждой своя.
    pub(crate) done: Option<(Name, StateExtend)>,
    /// Условие ребра и его позиция в исходнике. `None` - безусловное ребро.
    ///
    /// Позиция нужна отказу печатника: он оборачивает причину и обязан указать на
    /// **ребро**, а не на место в генераторе.
    pub(crate) cond: Option<(ConditionNode, Location)>,
    /// Состояние-источник: его блоки `exit` исполняются при переходе.
    pub(crate) exit_state: Rc<RefCell<StateNode>>,
    /// Состояние-приёмник: его блоки `enter` исполняются при переходе. `None` у
    /// перехода в `END` - у синтетического состояния блоков нет.
    pub(crate) enter_state: Option<Rc<RefCell<StateNode>>>,
    /// Состояние-приёмник.
    pub(crate) to: RowTarget,
}

impl Row {
    /// Имя состояния-приёмника, если это не `END`.
    pub(crate) fn target_name(&self) -> Option<&Name> {
        match &self.to {
            RowTarget::State(name) => Some(name),
            RowTarget::End => None,
        }
    }
}

/// Собирает строки таблицы переходов модели.
///
/// Порядок - тот же, в котором цель печатает переходы формой `switch`: состояния в
/// порядке `states`, внутри состояния - рёбра `ref` в порядке объявления, затем `next`,
/// затем `END`.
///
/// **Безусловное ребро закрывает цепочку строк только у простого состояния**. У
/// состояния с реализацией строка всё равно проверяется предикатом завершения, и
/// следующая строка (`next`/`END`) остаётся достижимой - ровно так же, как в форме
/// `switch`, где они печатаются внутри `if (реализация завершена)`.
pub(crate) fn rows(model: &Element, src: &dyn StateSource) -> Result<Vec<Row>, Diagnostic> {
    let Element::Model { states, .. } = model else {
        return Err(Diagnostic::error(
            Location::Codegen,
            "Элемент не является моделью".to_string(),
        )
        .with_code("CC-006"));
    };
    let mut collected = Vec::new();
    for state_name in states.iter() {
        let raw = src.state_node(state_name.clone())?;
        let Some(element) = src.state_element(state_name.clone()) else {
            continue; // недостижимое состояние - как и в форме `switch`
        };
        let (done, next) = match &element {
            Element::State { .. } => (None, None),
            Element::StateExtend { extend, next, .. } => (
                Some((state_name.clone(), extend.clone())),
                Some(next.clone()),
            ),
            Element::Model { .. } => {
                unreachable!("карта отдаёт только State/StateExtend (фильтр is_state)")
            }
        };
        let closed = push_reference_rows(&mut collected, &raw, state_name, &done, states, src)?;
        if closed {
            continue;
        }
        let has_references = !raw.borrow().references().is_empty();
        match next {
            Some(next) if !next.local().is_empty() => {
                let target = src.state_node(next.clone())?;
                collected.push(Row {
                    from: state_name.clone(),
                    done: done.clone(),
                    cond: None,
                    exit_state: Rc::clone(&raw),
                    enter_state: Some(target),
                    to: RowTarget::State(next),
                });
            }
            // `END` подставляется только состоянию без переходов: состояние с рёбрами,
            // ни одно из которых не сработало, остаётся на месте.
            Some(_) if has_references => {}
            Some(_) => collected.push(end_row(state_name, &done, &raw)),
            None => {
                let terminated = raw.borrow().is_terminated();
                if terminated && !state_name.local().to_uppercase().eq("END") {
                    collected.push(end_row(state_name, &done, &raw));
                }
            }
        }
    }
    Ok(collected)
}

/// Строка "в терминальное состояние модели": `exit` источника, затем `END`.
fn end_row(
    from: &Name,
    done: &Option<(Name, StateExtend)>,
    raw_state: &Rc<RefCell<StateNode>>,
) -> Row {
    Row {
        from: from.clone(),
        done: done.clone(),
        cond: None,
        exit_state: Rc::clone(raw_state),
        enter_state: None,
        to: RowTarget::End,
    }
}

/// Строки по рёбрам `ref` состояния.
///
/// Возвращает `true`, если цепочка закрыта безусловным ребром простого состояния.
fn push_reference_rows(
    out: &mut Vec<Row>,
    raw_state: &Rc<RefCell<StateNode>>,
    from: &Name,
    done: &Option<(Name, StateExtend)>,
    states: &[Name],
    src: &dyn StateSource,
) -> Result<bool, Diagnostic> {
    let references = raw_state.borrow().references().to_vec();
    for reference in references {
        let Some(target) = states.iter().find(|n| n.local() == reference.name).cloned() else {
            continue; // целевое состояние недостижимо - как и в форме `switch`
        };
        if src.state_element(target.clone()).is_none() {
            continue;
        }
        let target_raw = src.state_node(target.clone())?;
        let unconditional = reference.cond.is_unconditional();
        let cond = if unconditional {
            None
        } else {
            Some((reference.cond.clone(), reference.location))
        };
        out.push(Row {
            from: from.clone(),
            done: done.clone(),
            cond,
            exit_state: Rc::clone(raw_state),
            enter_state: Some(target_raw),
            to: RowTarget::State(target),
        });
        if unconditional && done.is_none() {
            return Ok(true);
        }
    }
    Ok(false)
}

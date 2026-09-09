//! Состояния и переходы: единственный `start`, достижимость, полнота.
//!
//! Часть модуля `validate`.

use crate::diagnostics::lang::keys;
use crate::msg;
use super::*;

/// Проверяет, что модель содержит ровно одно начальное состояние.
///
/// Если в модели нет состояний вообще (например, модуль с только объявлениями типов или
/// переменных), проверка пропускается.
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`], если модель содержит состояния, но начальных состояний не
/// ровно одно (0 или >= 2).
pub(super) fn model_only_one_start_state(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let borrowed = model.borrow();

    // Модель без состояний допустима (только переменные/типы/условия)
    if borrowed.states.is_empty() {
        return Vec::new();
    }

    let name = borrowed.name.clone().unwrap_or_default();
    let start_count = borrowed
        .states
        .values()
        .filter(|state| {
            matches!(
                state,
                StateNode::Simple {
                    kind: StateNodeKind::Start,
                    ..
                } | StateNode::Implement {
                    kind: StateNodeKind::Start,
                    ..
                }
            )
        })
        .count();

    if start_count != 1 {
        return vec![
            Diagnostic::error(
                borrowed.loc,
                msg!(keys::SE_011_MULTIPLE_START_STATES, name = name, count = start_count),
            )
            .with_code("SE-011"),
        ];
    }
    Vec::new()
}

pub(super) fn validate_state_references(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let borrowed = model.borrow();
    let mut out = Vec::new();
    for state in borrowed.states.values() {
        match state {
            StateNode::Simple { references, .. } | StateNode::Implement { references, .. } => {
                // Накопление по рёбрам: каждое ребро - своё нарушение, и второе не
                // является следствием первого.
                for reference in references {
                    out.extend(validate_reference(reference, model.clone()).err());
                }
            }
            StateNode::Unresolved => {}
        }
    }
    out
}

/// Проверяет полноту и достижимость переходов в модели конечного автомата.
///
/// ## Правила Ce5
///
/// 1. **Предупреждение**: из состояния нет пути к терминальному состоянию
///    (состоянию без исходящих переходов), и само оно не терминальное.
/// 2. **Предупреждение**: в модели нет ни одного терминального состояния.
/// 3. **Предупреждение**: в состоянии есть `ref`-переходы совместно с `next`
///    (код после `next` недостижим).
/// 4. **Ошибка**: в одном состоянии несколько `next` (уже проверяется парсером,
///    но дублируем на семантическом уровне для явности).
///
/// Функция рекурсивно обходит все вложенные модели.
///
/// # Возвращаемое значение
///
/// Вектор [`Diagnostic`] (предупреждения и ошибки). Пустой вектор означает отсутствие
/// нарушений.
pub fn check_transition_completeness(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    collect_transition_completeness(&model, &mut diags);
    diags
}

/// Рекурсивно собирает диагностики Ce5 для модели и всех вложенных моделей.
fn collect_transition_completeness(model: &Rc<RefCell<ModelNode>>, out: &mut Vec<Diagnostic>) {
    let borrowed = model.borrow();

    // Если состояний нет - модуль без автомата, пропускаем
    if borrowed.states.is_empty() {
        // Рекурсивный спуск во вложенные модели
        let nested: Vec<Rc<RefCell<ModelNode>>> = borrowed.models.values().map(Rc::clone).collect();
        drop(borrowed);
        for m in nested {
            collect_transition_completeness(&m, out);
        }
        return;
    }

    let model_name = borrowed.name.clone().unwrap_or_default();
    let model_loc = borrowed.loc;

    // Строит префикс для сообщений: "модель 'M'" или пустую строку для корня
    let model_prefix = if model_name.is_empty() {
        String::new()
    } else {
        msg!(keys::WHAT_MODEL_PREFIX, name = model_name)
    };

    // Вычисляем множество терминальных состояний (без исходящих переходов)
    let terminal_states: std::collections::HashSet<String> = borrowed
        .states
        .iter()
        .filter_map(|(name, state)| {
            // Правило одно на проект (`semantic::terminal`): тело считается наравне с
            // рёбрами, и `always` без переходов автомат не завершает.
            let is_terminal = state.is_terminated();
            if is_terminal {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect();

    // Правило Ce5.2: нет терминальных состояний вообще
    if terminal_states.is_empty() {
        out.push(
            Diagnostic::warning(
                model_loc,
                msg!(keys::SE_010_NO_TERMINAL_STATES, prefix = model_prefix),
            )
            .with_code("SE-010"),
        );
    }

    // Строим граф переходов: имя_состояния -> список целей
    let mut graph: std::collections::BTreeMap<String, Vec<String>> =
        std::collections::BTreeMap::new();
    for (name, state) in borrowed.states.iter() {
        let targets: Vec<String> = match state {
            StateNode::Simple { references, .. } => {
                references.iter().map(|r| r.name.clone()).collect()
            }
            StateNode::Implement {
                references, next, ..
            } => {
                let mut t: Vec<String> = references.iter().map(|r| r.name.clone()).collect();
                if let Some(nr) = next {
                    t.push(nr.name.clone());
                }
                t
            }
            StateNode::Unresolved => vec![],
        };
        graph.insert(name.clone(), targets);
    }

    // Правило Ce5.1: из состояния нет пути к терминальному Используем BFS/DFS от
    // каждого нетерминального состояния
    if !terminal_states.is_empty() {
        for (state_name, state) in borrowed.states.iter() {
            // Терминальные состояния сами по себе достижимы
            if terminal_states.contains(state_name) {
                continue;
            }
            // BFS от state_name
            let can_reach = {
                let mut visited = std::collections::HashSet::new();
                let mut queue = std::collections::VecDeque::new();
                queue.push_back(state_name.clone());
                let mut found = false;
                while let Some(cur) = queue.pop_front() {
                    if terminal_states.contains(&cur) {
                        found = true;
                        break;
                    }
                    if visited.contains(&cur) {
                        continue;
                    }
                    visited.insert(cur.clone());
                    if let Some(targets) = graph.get(&cur) {
                        for t in targets {
                            queue.push_back(t.clone());
                        }
                    }
                }
                found
            };
            if !can_reach {
                out.push(
                    Diagnostic::warning(
                        state.loc(),
                        msg!(
                            keys::SE_010_NO_PATH_TO_TERMINAL,
                            prefix = model_prefix,
                            name = state_name
                        ),
                    )
                    .with_code("SE-010"),
                );
            }
        }
    }

    // Правила Ce5.3 и Ce5.4: проверка next совместно с ref
    for state in borrowed.states.values() {
        if let StateNode::Implement {
            name,
            references,
            next,
            ..
        } = state
        {
            // Правило Ce5.3: ref + next одновременно -> предупреждение
            if next.is_some() && !references.is_empty() {
                out.push(
                    Diagnostic::warning(
                        state.loc(),
                        msg!(keys::SE_012_REF_WITH_NEXT, prefix = model_prefix, name = name),
                    )
                    .with_code("SE-012"),
                );
            }
        }
    }

    // Рекурсивный спуск во вложенные модели
    let nested: Vec<Rc<RefCell<ModelNode>>> = borrowed.models.values().map(Rc::clone).collect();
    drop(borrowed);

    for m in nested {
        collect_transition_completeness(&m, out);
    }
}

/// Предупреждает о состояниях, недостижимых из начального состояния.
///
/// Обходит граф переходов (BFS) начиная со стартового состояния. Состояние считается
/// недостижимым, если на него нет ни одного перехода `ref` или `next` из любого
/// достижимого состояния.
///
/// Функция рекурсивно обходит все вложенные модели.
pub fn check_unreachable_states(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    collect_unreachable_states(&model, &mut diags);
    diags
}

fn collect_unreachable_states(model: &Rc<RefCell<ModelNode>>, out: &mut Vec<Diagnostic>) {
    let borrowed = model.borrow();

    let states = &borrowed.states;
    if states.is_empty() {
        drop(borrowed);
        let nested: Vec<_> = model.borrow().models.values().map(Rc::clone).collect();
        for m in nested {
            collect_unreachable_states(&m, out);
        }
        return;
    }

    // Имя стартового состояния
    let start_name = states
        .values()
        .find(|s| s.kind() == StateNodeKind::Start)
        .map(|s| get_state_name(s).to_string());

    if let Some(start) = start_name {
        let mut reachable = std::collections::HashSet::new();
        let mut queue = std::collections::VecDeque::new();
        queue.push_back(start.clone());
        reachable.insert(start);

        while let Some(current) = queue.pop_front() {
            if let Some(state) = states.get(&current) {
                for target in reachable_targets(state) {
                    if !reachable.contains(&target) {
                        reachable.insert(target.clone());
                        queue.push_back(target);
                    }
                }
            }
        }

        for (name, state) in states {
            if !reachable.contains(name.as_str()) {
                let loc = get_state_loc(state);
                out.push(
                    Diagnostic::warning(
                        loc,
                        msg!(keys::SE_010_STATE_UNREACHABLE, name = name),
                    )
                    .with_code("SE-046"),
                );
            }
        }
    }

    drop(borrowed);
    let nested: Vec<_> = model.borrow().models.values().map(Rc::clone).collect();
    for m in nested {
        collect_unreachable_states(&m, out);
    }
}

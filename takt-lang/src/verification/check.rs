//! Проверка пустоты произведения и контрпример-лассо.
//!
//! Язык автомата Бюхи непуст ⟺ существует **достижимый принимающий цикл**. Ищется
//! классическим **вложенным поиском в глубину** (nested DFS,
//! Courcoubetis-Vardi-Wolper-Yannakakis):
//!
//! 1. Внешний DFS обходит достижимые состояния; когда обход состояния
//!    завершён (постпорядок) и состояние принимающее - запускается внутренний
//!    DFS из него.
//! 2. Внутренний DFS ищет путь обратно в это же (принимающее) состояние. Нашёл
//!    ⟹ есть цикл через принимающее состояние, достижимый из начального.
//!
//! Постпорядок существен: он гарантирует, что внутренний DFS не пропустит цикл из-за
//! уже посещённых состояний (метки внутреннего DFS переиспользуются между запусками - в
//! этом вся экономия алгоритма).
//!
//! Найденный путь возвращается как **лассо** ([`Lasso`]): конечный префикс до
//! принимающего состояния плюс цикл через него. Это и есть контрпример - бесконечный
//! прогон модели, нарушающий свойство.

use crate::verification::kripke::Kripke;
use crate::verification::product::Product;
use std::collections::BTreeSet;

/// Контрпример: бесконечный прогон вида "префикс, затем цикл навсегда".
///
/// Индексы - состояния [`Product`]; человекочитаемая проекция на имена состояний FSM -
/// [`Lasso::state_names`] и [`Lasso::trace`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lasso {
    /// Путь от начального состояния произведения до входа в цикл (последнее состояние
    /// префикса - первое состояние цикла).
    pub prefix: Vec<usize>,
    /// Цикл: путь от первого состояния цикла обратно в него же.
    ///
    /// Первый элемент - принимающее состояние, последний - переход, замыкающий цикл на
    /// него (то есть `cycle.last()` ведёт в `cycle[0]`).
    pub cycle: Vec<usize>,
}

impl Lasso {
    /// Имена состояний FSM на префиксе и на цикле (проекция на разметку Крипке).
    ///
    /// Проекция "как есть": соседние повторы **не схлопываются**, потому что на этом
    /// уровне они значимы - два подряд идущих одинаковых имени суть два состояния
    /// произведения над одной вершиной Крипке. Приведение трассы к читаемому виду -
    /// дело потребителя
    /// ([`Counterexample`](crate::verification::verify::Counterexample)).
    pub fn state_names(&self, kripke: &Kripke, product: &Product) -> (Vec<String>, Vec<String>) {
        let project = |path: &[usize]| -> Vec<String> {
            path.iter()
                .map(|&s| product.state_name(kripke, s).to_string())
                .collect()
        };
        (project(&self.prefix), project(&self.cycle))
    }
}

/// Ищет достижимый принимающий цикл в произведении.
///
/// `None` - язык пуст; `Some(lasso)` -
/// контрпример.
pub fn emptiness(product: &Product) -> Option<Lasso> {
    let mut outer_visited = BTreeSet::new();
    let mut inner_visited = BTreeSet::new();
    for &start in &product.initial {
        if outer_visited.contains(&start) {
            continue;
        }
        if let Some(lasso) = outer_dfs(product, start, &mut outer_visited, &mut inner_visited) {
            return Some(lasso);
        }
    }
    None
}

/// Кадр обхода: состояние, его преемники и позиция в них.
///
/// Заменяет кадр вызова: обходы здесь **итеративны** - рекурсия исчерпывала стек на
/// модели в ~16000 состояний, отказывая `SIGABRT`'ом без диагностики. Позиция нужна
/// потому, что оба обхода делают работу **после** потомков (постпорядок у внешнего,
/// откат у внутреннего), а такой обход в простой список задач не сворачивается - нужен
/// разбор кадра по шагам.
struct Frame {
    state: usize,
    successors: Vec<usize>,
    next_index: usize,
}

impl Frame {
    fn new(product: &Product, state: usize) -> Self {
        Frame {
            state,
            successors: product.successors(state).iter().copied().collect(),
            next_index: 0,
        }
    }
}

/// Внешний DFS: обход достижимых состояний; из принимающих - внутренний DFS в
/// постпорядке.
///
/// `path` - текущий путь от начального состояния; он же становится префиксом лассо.
/// Постпорядок существен (см. модуль): его нарушение сделало бы проверку пустоты
/// неверной, а не медленной.
fn outer_dfs(
    product: &Product,
    start: usize,
    outer_visited: &mut BTreeSet<usize>,
    inner_visited: &mut BTreeSet<usize>,
) -> Option<Lasso> {
    let mut path: Vec<usize> = vec![start];
    let mut stack: Vec<Frame> = vec![Frame::new(product, start)];
    outer_visited.insert(start);

    while let Some(frame) = stack.last_mut() {
        if frame.next_index < frame.successors.len() {
            let next = frame.successors[frame.next_index];
            frame.next_index += 1;
            if !outer_visited.contains(&next) {
                outer_visited.insert(next);
                path.push(next);
                stack.push(Frame::new(product, next));
            }
            continue;
        }

        // Постпорядок: обход из `state` завершён - преемники исчерпаны.
        let state = frame.state;
        if product.accepting.contains(&state) {
            let mut cycle = Vec::new();
            if inner_dfs(product, state, inner_visited, &mut cycle) {
                return Some(Lasso {
                    prefix: path.clone(),
                    cycle,
                });
            }
        }
        stack.pop();
        path.pop();
    }

    None
}

/// Внутренний DFS: есть ли путь из `seed` обратно в `seed`.
///
/// При успехе `cycle` содержит путь `seed -> ... -> (ребро в seed)`.
fn inner_dfs(
    product: &Product,
    seed: usize,
    inner_visited: &mut BTreeSet<usize>,
    cycle: &mut Vec<usize>,
) -> bool {
    inner_visited.insert(seed);
    cycle.push(seed);
    let mut stack: Vec<Frame> = vec![Frame::new(product, seed)];

    while let Some(frame) = stack.last_mut() {
        if frame.next_index < frame.successors.len() {
            let next = frame.successors[frame.next_index];
            frame.next_index += 1;
            if next == seed {
                // Ребро замыкает цикл на принимающее состояние.
                return true;
            }
            if !inner_visited.contains(&next) {
                inner_visited.insert(next);
                cycle.push(next);
                stack.push(Frame::new(product, next));
            }
            continue;
        }

        // Ветка тупиковая: снимаем состояние с пути (откат рекурсии).
        stack.pop();
        cycle.pop();
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    /// Произведение из линейной цепочки без принимающих состояний.
    fn linear_no_accepting() -> Product {
        Product {
            states: vec![(0, 0), (1, 0)],
            initial: BTreeSet::from([0]),
            transitions: BTreeMap::from([(0, BTreeSet::from([1])), (1, BTreeSet::from([1]))]),
            accepting: BTreeSet::new(),
        }
    }

    /// Самопетля на принимающем состоянии, достижимая из начального.
    fn accepting_self_loop() -> Product {
        Product {
            states: vec![(0, 0), (1, 0)],
            initial: BTreeSet::from([0]),
            transitions: BTreeMap::from([(0, BTreeSet::from([1])), (1, BTreeSet::from([1]))]),
            accepting: BTreeSet::from([1]),
        }
    }

    fn kripke_ab() -> Kripke {
        Kripke {
            states: vec!["A".to_string(), "B".to_string()],
            initial: 0,
            transitions: BTreeMap::from([(0, BTreeSet::from([1])), (1, BTreeSet::from([1]))]),
            labels: Vec::new(),
        }
    }

    /// Без принимающих состояний язык пуст.
    #[test]
    fn no_accepting_states_means_empty() {
        assert_eq!(emptiness(&linear_no_accepting()), None);
    }

    /// Достижимый принимающий цикл найден, лассо указывает на него.
    #[test]
    fn reachable_accepting_cycle_is_found() {
        let p = accepting_self_loop();
        let lasso = emptiness(&p).expect("принимающая самопетля — принимающий цикл");
        assert_eq!(lasso.prefix, vec![0, 1]);
        assert_eq!(lasso.cycle, vec![1]);
        let (prefix, cycle) = lasso.state_names(&kripke_ab(), &p);
        assert_eq!(prefix, vec!["A".to_string(), "B".to_string()]);
        assert_eq!(cycle, vec!["B".to_string()]);
    }

    /// Недостижимый принимающий цикл языка не даёт.
    #[test]
    fn unreachable_accepting_cycle_is_not_a_counterexample() {
        let p = Product {
            states: vec![(0, 0), (1, 0)],
            initial: BTreeSet::from([0]),
            transitions: BTreeMap::from([(0, BTreeSet::from([0])), (1, BTreeSet::from([1]))]),
            // Принимающий цикл есть - но в состоянии 1, недостижимом из 0.
            accepting: BTreeSet::from([1]),
        };
        assert_eq!(emptiness(&p), None);
    }

    /// Принимающее состояние без цикла (тупик обхода) - не контрпример.
    #[test]
    fn accepting_state_without_cycle_is_not_a_counterexample() {
        let p = Product {
            states: vec![(0, 0), (1, 0)],
            initial: BTreeSet::from([0]),
            // 1 принимающее, но из него никуда: цикла через него нет.
            transitions: BTreeMap::from([(0, BTreeSet::from([1])), (1, BTreeSet::new())]),
            accepting: BTreeSet::from([1]),
        };
        assert_eq!(emptiness(&p), None);
    }

    /// Цикл длиной больше одного состояния собирается целиком.
    #[test]
    fn multi_state_cycle_is_reported_in_full() {
        // 0 -> 1 -> 2 -> 1, принимающее - 1.
        let p = Product {
            states: vec![(0, 0), (1, 0), (0, 1)],
            initial: BTreeSet::from([0]),
            transitions: BTreeMap::from([
                (0, BTreeSet::from([1])),
                (1, BTreeSet::from([2])),
                (2, BTreeSet::from([1])),
            ]),
            accepting: BTreeSet::from([1]),
        };
        let lasso = emptiness(&p).expect("цикл 1 -> 2 -> 1 проходит через принимающее");
        assert_eq!(lasso.prefix, vec![0, 1]);
        assert_eq!(lasso.cycle, vec![1, 2]);
    }

    /// Проекция на имена состояний - "как есть", повторы сохраняются: два состояния
    /// произведения над вершиной Fault дают два имени Fault.
    #[test]
    fn state_names_keep_repeats() {
        let kripke = Kripke {
            states: vec!["Idle".to_string(), "Fault".to_string()],
            initial: 0,
            transitions: BTreeMap::from([(0, BTreeSet::from([1])), (1, BTreeSet::from([1]))]),
            labels: Vec::new(),
        };
        let p = Product {
            states: vec![(0, 0), (1, 0), (1, 1)],
            initial: BTreeSet::from([0]),
            transitions: BTreeMap::from([
                (0, BTreeSet::from([1])),
                (1, BTreeSet::from([2])),
                (2, BTreeSet::from([1])),
            ]),
            accepting: BTreeSet::from([1]),
        };
        let lasso = emptiness(&p).unwrap();
        let (prefix, cycle) = lasso.state_names(&kripke, &p);
        assert_eq!(prefix, vec!["Idle".to_string(), "Fault".to_string()]);
        assert_eq!(cycle, vec!["Fault".to_string(), "Fault".to_string()]);
    }

    /// Детерминизм: один и тот же контрпример на десяти прогонах.
    #[test]
    fn emptiness_is_deterministic() {
        let p = accepting_self_loop();
        let first = emptiness(&p);
        for _ in 0..10 {
            assert_eq!(emptiness(&p), first);
        }
    }
}

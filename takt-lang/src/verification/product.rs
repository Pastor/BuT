//! Синхронное произведение структуры Крипке с автоматом Бюхи.
//!
//! Стандартный автоматный подход к model checking: для свойства `φ` строится автомат
//! `A_¬φ`, берётся произведение `K x A_¬φ`, и проверяется **пустота** языка
//! произведения (задача [`super::check`]). Пусто ⟹ ни один прогон модели не нарушает
//! `φ` ⟹ свойство держится.
//!
//! # Разметка автомата
//!
//! `BuchiAutomaton` размечен **по состояниям, а не по рёбрам**: множество
//! формул `states[q]` - это обязательства, ожидаемые в той позиции слова, где автомат
//! **входит** в `q`. Литералы (`Atom`/`!Atom`) в этом множестве и есть ограничение на
//! букву; остальные формулы - темпоральные обязательства, за которые отвечает условие
//! принятия, а не буква. Отсюда [`state_literals`] фильтрует ровно литералы.
//!
//! Соответственно пара `(k, q)` **согласована**, если разметка вершины Крипке `k` (её
//! имя - единственный истинный атом) удовлетворяет литералам `q`.

use crate::verification::buchi::BuchiAutomaton;
use crate::verification::kripke::Kripke;
use crate::verification::ltl::Ltl;
use std::collections::{BTreeMap, BTreeSet};

/// Произведение `K x A`: состояния - согласованные пары `(вершина Крипке, состояние
/// автомата)`, достижимые из начальных.
///
/// Строится [`product`] обходом от начальных пар: недостижимые пары в произведение не
/// попадают (проверять их пустоту незачем).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Product {
    /// Пары `(k, q)`; индекс в векторе - идентификатор состояния произведения.
    pub states: Vec<(usize, usize)>,
    /// Индексы начальных состояний произведения.
    pub initial: BTreeSet<usize>,
    /// Отношение переходов произведения.
    pub transitions: BTreeMap<usize, BTreeSet<usize>>,
    /// Индексы принимающих состояний: пара `(k, q)`, где `q` принимающее в `A`.
    pub accepting: BTreeSet<usize>,
}

impl Product {
    /// Преемники состояния произведения.
    pub fn successors(&self, s: usize) -> &BTreeSet<usize> {
        static EMPTY: std::sync::OnceLock<BTreeSet<usize>> = std::sync::OnceLock::new();
        self.transitions
            .get(&s)
            .unwrap_or_else(|| EMPTY.get_or_init(BTreeSet::new))
    }

    /// Проекция состояния произведения на имя состояния FSM - то, что видит
    /// пользователь в контрпримере.
    pub fn state_name<'a>(&self, kripke: &'a Kripke, s: usize) -> &'a str {
        kripke.states[self.states[s].0].as_str()
    }

    /// Текстовый дамп произведения для отладки (`taktc verify --trace`).
    pub fn trace(&self, kripke: &Kripke) -> String {
        let mut out = format!(
            "=== Произведение K x A_!phi ({} состояний, {} принимающих) ===\n",
            self.states.len(),
            self.accepting.len()
        );
        for (s, &(k, q)) in self.states.iter().enumerate() {
            let marks = match (self.initial.contains(&s), self.accepting.contains(&s)) {
                (true, true) => " (нач., принимающее)",
                (true, false) => " (нач.)",
                (false, true) => " (принимающее)",
                (false, false) => "",
            };
            let succ: Vec<String> = self
                .successors(s)
                .iter()
                .map(|&t| {
                    let (tk, tq) = self.states[t];
                    format!("({},q{})", kripke.states[tk], tq)
                })
                .collect();
            out.push_str(&format!(
                "  ({},q{}){} -> {}\n",
                kripke.states[k],
                q,
                marks,
                succ.join(", ")
            ));
        }
        out
    }
}

/// Литералы состояния `q` автомата: требуемые истинными (`pos`) и требуемые ложными
/// (`neg`) атомы.
///
/// Нелитеральные формулы состояния (`Until`, `And`, ...) - темпоральные обязательства,
/// а не ограничения на текущую букву: их игнорируем (за них отвечает условие принятия
/// автомата). Формула в NNF, поэтому `Not` стоит только над атомом.
pub fn state_literals(
    automaton: &BuchiAutomaton,
    q: usize,
) -> (BTreeSet<String>, BTreeSet<String>) {
    let mut pos = BTreeSet::new();
    let mut neg = BTreeSet::new();
    let Some(formulas) = automaton.states.get(q) else {
        return (pos, neg);
    };
    for f in formulas {
        match f.as_ref() {
            Ltl::Atom(a) => {
                pos.insert(a.clone());
            }
            Ltl::Not(inner) => {
                if let Ltl::Atom(a) = inner.as_ref() {
                    neg.insert(a.clone());
                }
            }
            _ => {}
        }
    }
    (pos, neg)
}

/// Согласована ли вершина Крипке `k` с литералами состояния автомата `q`.
///
/// Вершина Крипке несёт ровно один истинный атом - своё имя. Поэтому пара с двумя
/// разными `pos`-атомами несогласована автоматически (состояние не может называться
/// двумя именами) - так автомат для `p & q` даёт пустое произведение, что и требуется.
fn consistent(kripke: &Kripke, k: usize, automaton: &BuchiAutomaton, q: usize) -> bool {
    let (pos, neg) = state_literals(automaton, q);
    pos.iter().all(|a| kripke.atom_holds(k, a)) && neg.iter().all(|a| !kripke.atom_holds(k, a))
}

/// Строит синхронное произведение `K x A`.
///
/// Начальные состояния - согласованные пары `(K.initial, q0)` для всех `q0 ∈
/// A.initial_states`; переход `(k,q) -> (k',q')` существует ⟺ `k -> k'` в `K`, `q ->
/// q'` в `A`, и пара `(k',q')` согласована.
///
/// Обход - от начальных состояний в порядке `BTreeSet`, поэтому нумерация состояний
/// произведения детерминирована (проверка 0048).
pub fn product(kripke: &Kripke, automaton: &BuchiAutomaton) -> Product {
    let mut states: Vec<(usize, usize)> = Vec::new();
    let mut index: BTreeMap<(usize, usize), usize> = BTreeMap::new();
    let mut initial = BTreeSet::new();
    let mut transitions: BTreeMap<usize, BTreeSet<usize>> = BTreeMap::new();
    let mut accepting = BTreeSet::new();

    let intern = |pair: (usize, usize),
                  states: &mut Vec<(usize, usize)>,
                  index: &mut BTreeMap<(usize, usize), usize>|
     -> usize {
        if let Some(&id) = index.get(&pair) {
            return id;
        }
        let id = states.len();
        states.push(pair);
        index.insert(pair, id);
        id
    };

    let mut queue: Vec<usize> = Vec::new();
    for &q0 in &automaton.initial_states {
        if consistent(kripke, kripke.initial, automaton, q0) {
            let id = intern((kripke.initial, q0), &mut states, &mut index);
            if initial.insert(id) {
                queue.push(id);
            }
        }
    }

    while let Some(s) = queue.pop() {
        let (k, q) = states[s];
        if automaton.accepting.contains(&q) {
            accepting.insert(s);
        }
        let empty = BTreeSet::new();
        let q_succ = automaton.transitions.get(&q).unwrap_or(&empty);
        let mut succ = BTreeSet::new();
        for &k2 in kripke.successors(k) {
            for &q2 in q_succ {
                if !consistent(kripke, k2, automaton, q2) {
                    continue;
                }
                let known = index.contains_key(&(k2, q2));
                let id = intern((k2, q2), &mut states, &mut index);
                succ.insert(id);
                if !known {
                    queue.push(id);
                }
            }
        }
        transitions.insert(s, succ);
    }

    Product {
        states,
        initial,
        transitions,
        accepting,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::verification::buchi::build_buchi;
    use crate::verification::ltl::parse_ltl;
    use std::rc::Rc;

    /// Крипке из двух вершин: A -> B -> B.
    fn a_then_b() -> Kripke {
        Kripke {
            states: vec!["A".to_string(), "B".to_string()],
            initial: 0,
            transitions: BTreeMap::from([(0, BTreeSet::from([1])), (1, BTreeSet::from([1]))]),
            labels: Vec::new(),
        }
    }

    /// Литералы извлекаются из множества формул состояния.
    #[test]
    fn literals_are_extracted_from_state_formulas() {
        let automaton = BuchiAutomaton {
            states: vec![BTreeSet::from([
                Rc::new(Ltl::Atom("A".to_string())),
                Rc::new(Ltl::Not(Rc::new(Ltl::Atom("B".to_string())))),
                // Темпоральное обязательство - не ограничение на букву.
                Rc::new(Ltl::Until(
                    Rc::new(Ltl::True),
                    Rc::new(Ltl::Atom("C".to_string())),
                )),
            ])],
            initial_states: BTreeSet::from([0]),
            transitions: BTreeMap::new(),
            accepting: BTreeSet::new(),
        };
        let (pos, neg) = state_literals(&automaton, 0);
        assert_eq!(pos, BTreeSet::from(["A".to_string()]));
        assert_eq!(neg, BTreeSet::from(["B".to_string()]));
    }

    /// Пара согласована, только если имя вершины Крипке удовлетворяет литералам.
    #[test]
    fn product_keeps_only_consistent_pairs() {
        let kripke = a_then_b();
        let automaton = build_buchi(&Ltl::Globally(Rc::new(Ltl::Atom("A".to_string()))));
        let p = product(&kripke, &automaton);
        // G A на модели A -> B -> B: после перехода в B согласованных пар нет.
        assert!(
            p.states.iter().all(|&(k, _)| k == 0),
            "вершина B не может быть согласована с литералом A: {:?}",
            p.states
        );
    }

    /// Произведение с недостижимыми парами не строится (обход от начальных).
    #[test]
    fn product_is_reachable_only() {
        let kripke = a_then_b();
        let automaton = build_buchi(&parse_ltl("F p"));
        let p = product(&kripke, &automaton);
        for &(k, _) in &p.states {
            assert!(k < kripke.states.len());
        }
    }

    /// Детерминизм (проверка 0048): построение повторяемо.
    #[test]
    fn product_is_deterministic() {
        let kripke = a_then_b();
        let automaton = build_buchi(&Ltl::Finally(Rc::new(Ltl::Atom("B".to_string()))));
        let first = product(&kripke, &automaton);
        for _ in 0..10 {
            let again = product(
                &kripke,
                &build_buchi(&Ltl::Finally(Rc::new(Ltl::Atom("B".to_string())))),
            );
            assert_eq!(again, first);
        }
    }
}

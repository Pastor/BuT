//! Структура Крипке из семантического дерева.
//!
//! **Абстракция управляющего графа** (Option A -
//! 2026-07-16): вершины структуры Крипке - состояния FSM, рёбра - `ref`-переходы
//! (плюс `next` состояния-реализации). Разметка вершины - её **имя**: атом LTL
//! `S` истинен в вершине `k` ⟺ `k` - состояние с именем `S`.
//!
//! Следствия абстракции (фиксируются здесь, потому что от них зависит смысл вердикта):
//!
//! - **Гарантии рёбер абстрагированы.** Условие перехода (`ref Next: x = 1;`)
//!   игнорируется: любой `ref` считается **возможным** недетерминированным
//!   переходом. Это over-approximation - множество прогонов Крипке ⊇ множества
//!   реальных прогонов, поэтому "свойство держится" - надёжный ответ, а
//!   контрпример может оказаться недостижим по данным (помечается "абстракция
//!   управления", см. [`super::check::Lasso`]).
//! - **Данные - в этой Крипке - вне охвата.** Разметка вершины - имя состояния;
//!   `var`/порты/`cond` в неё не входят. Атом-предикат над данными обслуживает
//!   **отдельная** Крипке ([`super::data_kripke`]), где вершина -
//!   пара `(состояние, оценка)`. Атом, не имя состояния и не отслеживаемый
//!   предикат, не молча ложен, а даёт `Verdict::Unsupported`
//!   (см. [`super::verify::verify_model`]).
//! - **Самопетля там, где автомат может задержаться** ([`may_stutter`]) -
//!   не только у тупика. Два повода: (1) конечное состояние обрывает
//!   прогон, а семантика Бюхи определена на бесконечных словах; (2) такт, в
//!   котором ни один guard не сработал, **состояния не меняет** (эталон -
//!   порождённый C), поэтому состояние без безусловного `ref` вправе стоять на
//!   месте вечно. Оба прогона реальны, и пропуск второго делал бы Крипке
//!   **недо**-аппроксимацией - то есть ломал бы надёжность `Holds`.

use crate::diagnostics::lang::keys;
use crate::msg;
use crate::semantic::{ModelNode, ReferenceNode, StateNode, StateNodeKind};
use std::collections::{BTreeMap, BTreeSet};

/// Структура Крипке управляющего графа модели.
///
/// Строится [`build_kripke`]. Порядок вершин детерминирован (алфавитный - от `BTreeMap`
/// в `ModelNode::states`), поэтому вердикт воспроизводим побайтно.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Kripke {
    /// Имена состояний FSM; индекс в векторе - идентификатор вершины.
    pub states: Vec<String>,
    /// Индекс стартовой вершины (состояние `start`).
    pub initial: usize,
    /// Отношение переходов: из вершины `u` в вершины `transitions[u]`.
    ///
    /// Тотально: у каждой вершины непустое множество преемников - вершина без рёбер
    /// получает самопетлю (см.
    pub transitions: BTreeMap<usize, BTreeSet<usize>>,
    /// Разметка атомов по вершинам.
    ///
    /// **Пусто** -> чистая абстракция управления: истинный атом вершины -
    /// её имя состояния ([`atom_holds`](Kripke::atom_holds) сравнивает имена).
    /// **Непусто** (`len == states.len()`) -> путь данных: `labels[k]` - множество
    /// **всех** истинных в вершине `k` атомов (имя состояния + предикаты над
    /// данными, вычисленные точно по оценке отслеживаемых переменных). См.
    /// [`super::data_kripke::build_data_kripke`].
    pub labels: Vec<BTreeSet<String>>,
}

impl Kripke {
    /// Оценка атома в вершине.
    ///
    /// - **Путь управления** (`labels` пусто): атом `S` истинен в `k` ⟺
    ///   `states[k] == S`. Атом, не совпадающий ни с одним именем состояния,
    ///   ложен **во всех** вершинах. Молча полагаться на это нельзя (иначе
    ///   `F temp` тихо "не держится" вместо честного отказа) - вызывающий обязан
    ///   сперва отсеять такие атомы через [`unknown_atoms`].
    /// - **Путь данных** (`labels` непусто): атом истинен в `k` ⟺ он
    ///   в `labels[k]`. Так предикаты над данными (`cond`, булев `var`) получают
    ///   значение по оценке вершины, а имя состояния по-прежнему истинно там, где
    ///   вершина в этом состоянии (оно кладётся в `labels[k]` при построении).
    pub fn atom_holds(&self, k: usize, atom: &str) -> bool {
        if self.labels.is_empty() {
            self.states.get(k).is_some_and(|name| name == atom)
        } else {
            self.labels.get(k).is_some_and(|l| l.contains(atom))
        }
    }

    /// Преемники вершины `k` (пусто - только если `k` вне графа).
    pub fn successors(&self, k: usize) -> &BTreeSet<usize> {
        static EMPTY: std::sync::OnceLock<BTreeSet<usize>> = std::sync::OnceLock::new();
        self.transitions
            .get(&k)
            .unwrap_or_else(|| EMPTY.get_or_init(BTreeSet::new))
    }

    /// Число рёбер графа.
    ///
    /// Потолок верификации по данным считается **по рёбрам**: стоимость проверки идёт
    /// по ним, а не по вершинам, и множитель произведения доменов входит в число рёбер
    /// квадратично ([`data_kripke`](super::data_kripke)).
    pub fn edge_count(&self) -> usize {
        self.transitions.values().map(BTreeSet::len).sum()
    }

    /// Имена атомов, не являющихся именами состояний, - для честного отказа.
    pub fn unknown_atoms<'a, I: IntoIterator<Item = &'a String>>(&self, atoms: I) -> Vec<String> {
        let known: BTreeSet<&str> = self.states.iter().map(String::as_str).collect();
        atoms
            .into_iter()
            .filter(|a| !known.contains(a.as_str()))
            .cloned()
            .collect()
    }

    /// Текстовый дамп графа для отладки (`taktc verify --trace`).
    pub fn trace(&self) -> String {
        let mut out = msg!(keys::VERIFY_TRACE_KRIPKE_HEADER) + "\n";
        for (k, name) in self.states.iter().enumerate() {
            let mark = if k == self.initial { " (start)" } else { "" };
            let succ: Vec<&str> = self
                .successors(k)
                .iter()
                .map(|&t| self.states[t].as_str())
                .collect();
            out.push_str(&format!("  {name}{mark} -> {}\n", succ.join(", ")));
        }
        out
    }
}

/// Строит структуру Крипке управляющего графа модели.
///
/// Возвращает `None`, если у модели **нет стартового состояния**: проверять нечего, а
/// молчаливый вердикт "держится" на пустом графе был бы ложью (принцип "громкий
/// отказ"). Вызывающий обязан сообщить об этом явно - см.
/// [`Verdict::NoStartState`](super::Verdict::NoStartState).
///
/// Вложенные модели состояний-реализаций (`state S = M;`) **не разворачиваются**: в
/// абстракции управления такое состояние - одна вершина (её имя - атом).
pub fn build_kripke(model: &ModelNode) -> Option<Kripke> {
    let states: Vec<String> = model.states.keys().cloned().collect();
    let index: BTreeMap<&str, usize> = states
        .iter()
        .enumerate()
        .map(|(i, name)| (name.as_str(), i))
        .collect();

    let initial = model
        .states
        .values()
        .find(|s| s.kind() == StateNodeKind::Start)
        .and_then(|s| index.get(s.name()).copied())?;

    let mut transitions: BTreeMap<usize, BTreeSet<usize>> = BTreeMap::new();
    for (k, name) in states.iter().enumerate() {
        let state = &model.states[name];
        let mut targets: BTreeSet<usize> = crate::semantic::validate::reachable_targets(state)
            .iter()
            // Висячая ссылка невозможна в валидной модели (её ловит SE-013 при
            // построении дерева); если всё же встретилась - ребра в никуда нет.
            .filter_map(|t| index.get(t.as_str()).copied())
            .collect();
        // Самопетля там, где автомат может задержаться ещё на такт: тупик или
        // состояние, все выходы которого под условием.
        if may_stutter(state) {
            targets.insert(k);
        }
        transitions.insert(k, targets);
    }

    Some(Kripke {
        states,
        initial,
        transitions,
        // Путь управления: разметка по имени состояния - см.
        labels: Vec::new(),
    })
}

/// Может ли автомат остаться в состоянии ещё на такт (нужна ли самопетля)?
///
/// **Эталон - порождённый C** (`c_model.rs::generate_state_transitions`):
///
/// ```c
/// case S: {
///     if (cond) { model->state = NEXT; break; }
///     break;                 // <- ни один guard не сработал: состояние прежнее
/// }
/// ```
///
/// Такт, в котором ни одно условие не выполнилось, **оставляет состояние
/// прежним**. Значит гарантированно покинуть состояние можно только по
/// **безусловному** `ref`; если все выходы под guard'ом, прогон "стоим на месте
/// вечно" существует по-настоящему, и Крипке обязана его содержать.
///
/// Пропустить эту самопетлю - значит сделать Крипке **недо**-аппроксимацией и сломать
/// главную гарантию фичи: `Verdict::Holds` перестаёт быть надёжным (проверка `F Done`
/// на `start A { ref Done: x = 1; }` объявляла бы свойство доказанным, хотя автомат
/// вправе стоять в `A` вечно). Ошибаться дозволено только в сторону лишних прогонов -
/// они дают ложные срабатывания, а не пропущенные нарушения.
fn may_stutter(state: &StateNode) -> bool {
    match state {
        StateNode::Unresolved => true,
        StateNode::Simple { references, .. } => !leaves_unconditionally(references),
        // `next` состояния-реализации (`state S = M { next T; }`) срабатывает, лишь
        // когда вложенная модель дошла до конца, - а она может не дойти никогда.
        // Безусловным выходом он не является, и в `references` не попадает (`tree.rs`
        // переносит туда только `next` состояния **без** реализации, уже как
        // безусловный `ref`).
        StateNode::Implement { references, .. } => !leaves_unconditionally(references),
    }
}

/// Есть ли выход, который срабатывает на любом такте (guard'а нет)?
///
/// Только `ConditionNode::None` - "условия нет". Всё прочее, включая
/// [`ConditionNode::Unresolved`] (условие `ref`-ребра по инварианту проекта не
/// разрешается), считается guard'ом: он **может** не выполниться. Ошибка в эту сторону
/// безопасна - лишняя самопетля даёт лишние прогоны.
fn leaves_unconditionally(references: &[ReferenceNode<StateNode>]) -> bool {
    // Решение "ребро безусловно" - у одного носителя.
    references.iter().any(|r| r.cond.is_unconditional())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use crate::semantic::test_constants::tests::model_node;
    use crate::semantic::tree::construct_model;

    fn kripke_of(src: &str) -> Kripke {
        let (ast, _) = parse(src, 0).unwrap();
        let model = construct_model(&ast, None, &[]).unwrap();
        let k = build_kripke(&model.borrow());
        k.expect("модель со стартовым состоянием даёт структуру Крипке")
    }

    /// Вершины = состояния FSM, рёбра = `ref`-переходы.
    #[test]
    fn vertices_are_states_edges_are_refs() {
        let k = kripke_of("start A { ref B; } state B { ref A; }");
        assert_eq!(k.states, vec!["A".to_string(), "B".to_string()]);
        assert_eq!(k.initial, 0);
        assert_eq!(k.successors(0), &BTreeSet::from([1]));
        assert_eq!(k.successors(1), &BTreeSet::from([0]));
    }

    /// Условие перехода абстрагировано: ребро есть независимо от guard'а.
    #[test]
    fn guarded_ref_is_a_possible_edge() {
        let k = kripke_of("var x: bit := false; start A { ref B: x = 1; } state B;");
        assert!(k.successors(0).contains(&1));
    }

    /// Состояние, все выходы которого под guard'ом, получает самопетлю.
    ///
    /// Эталон - порождённый C: такт без сработавшего условия оставляет состояние
    /// прежним (`if (cond) {...break;} break;`). Без этой петли Крипке теряет реальный
    /// прогон "стоим в A вечно", становится недо-аппроксимацией, и `F B` объявлялось бы
    /// доказанным - хотя автомат вправе никогда не уйти из A.
    #[test]
    fn state_with_only_guarded_exits_gets_self_loop() {
        let k = kripke_of("var x: bit := false; start A { ref B: x = 1; } state B;");
        assert!(
            k.successors(0).contains(&0),
            "guard может не сработать — прогон A^ω реален и обязан быть в Крипке: {:?}",
            k.successors(0)
        );
    }

    /// Безусловный `ref` гарантирует выход - самопетли быть не должно.
    ///
    /// Иначе абстракция обрастает прогонами, которых нет, и верификатор начинает
    /// выдавать ложные контрпримеры на ровном месте.
    #[test]
    fn state_with_unconditional_ref_has_no_self_loop() {
        let k = kripke_of("start A { ref B; } state B { ref A; }");
        assert_eq!(
            k.successors(0),
            &BTreeSet::from([1]),
            "безусловный ref уводит из A на каждом такте: самопетля была бы ложным прогоном"
        );
    }

    /// Хотя бы один безусловный выход среди условных - выход гарантирован.
    #[test]
    fn unconditional_ref_among_guarded_ones_prevents_self_loop() {
        let k =
            kripke_of("var x: bit := false; start A { ref B: x = 1; ref C; } state B; state C;");
        assert!(
            !k.successors(0).contains(&0),
            "`ref C;` без условия срабатывает всегда: задержаться в A нельзя"
        );
    }

    /// `next` состояния-реализации ждёт конца вложенной модели, а она может не
    /// закончиться никогда - значит это не безусловный выход.
    #[test]
    fn implement_state_next_is_not_an_unconditional_exit() {
        let k = kripke_of("model M { start S { ref S; } } start A = M { next B; } state B;");
        let a = k.states.iter().position(|s| s == "A").unwrap();
        assert!(
            k.successors(a).contains(&a),
            "вложенная модель может крутиться вечно — прогон A^ω реален: {:?}",
            k.successors(a)
        );
    }

    /// `next` состояния без реализации - безусловный переход (`tree.rs` превращает его
    /// в `ref` с `ConditionNode::None`), самопетли нет.
    #[test]
    fn plain_state_next_is_unconditional() {
        let k = kripke_of("start A { next B; } state B;");
        let a = k.states.iter().position(|s| s == "A").unwrap();
        assert!(!k.successors(a).contains(&a));
    }

    /// Тупиковое (конечное) состояние получает самопетлю.
    ///
    /// Состояние без `ref` - конечное по построению дерева (`tree.rs`: `kind = End`,
    /// если ссылок нет), отдельного ключевого слова `end` в языке нет.
    #[test]
    fn terminal_state_gets_self_loop() {
        let k = kripke_of("start A { ref Done; } state Done;");
        let done = k.states.iter().position(|s| s == "Done").unwrap();
        assert_eq!(
            k.successors(done),
            &BTreeSet::from([done]),
            "конечное состояние обязано иметь самопетлю: иначе нет бесконечных прогонов"
        );
    }

    /// Стартовая вершина - состояние `start`, а не первое по алфавиту.
    #[test]
    fn initial_is_the_start_state_not_the_first_alphabetically() {
        let k = kripke_of("state A { ref Z; } start Z { ref A; }");
        assert_eq!(k.states, vec!["A".to_string(), "Z".to_string()]);
        assert_eq!(k.initial, 1, "стартовая вершина — Z (start), а не A");
    }

    /// Модель без стартового состояния не даёт Крипке (громкий отказ).
    ///
    /// Через `construct_model` такая модель не проходит (SE-011: "должно быть только
    /// одно начальное состояние"), поэтому узел собирается вручную - ветка `None`
    /// защитная, но обязана оставаться рабочей.
    #[test]
    fn no_start_state_yields_none() {
        use crate::diagnostics::Location;
        use crate::semantic::StateNode;

        let model = model_node("M", None);
        model.borrow_mut().states.insert(
            "A".to_string(),
            StateNode::Simple {
                upper: None,
                loc: Location::Implicit,
                named_blocks: vec![],
                name: "A".to_string(),
                references: vec![],
                kind: StateNodeKind::Simple,
                formulas: vec![],
            },
        );
        assert!(build_kripke(&model.borrow()).is_none());
    }

    /// Оценка атома - сравнение имени состояния.
    #[test]
    fn atom_holds_compares_state_name() {
        let k = kripke_of("start A { ref B; } state B;");
        assert!(k.atom_holds(0, "A"));
        assert!(!k.atom_holds(0, "B"));
        assert!(
            !k.atom_holds(0, "x"),
            "атом-переменная ложен во всех вершинах"
        );
    }

    /// Неизвестные атомы перечисляются для честного отказа.
    #[test]
    fn unknown_atoms_are_reported() {
        let k = kripke_of("var temp: u8 := 0; start A { ref B; } state B;");
        let atoms = vec!["A".to_string(), "temp".to_string()];
        assert_eq!(k.unknown_atoms(&atoms), vec!["temp".to_string()]);
    }

    /// Детерминизм: два построения дают идентичную структуру.
    #[test]
    fn build_is_deterministic() {
        let src = "start A { ref B; ref C; } state B { ref C; } state C { ref A; }";
        let first = kripke_of(src);
        for _ in 0..10 {
            assert_eq!(kripke_of(src), first);
        }
    }

    /// Состояние-реализация: `next` - такое же ребро графа, вложенная модель
    /// **не разворачивается** (её состояния вершинами не становятся).
    #[test]
    fn implement_state_next_is_an_edge() {
        let k = kripke_of("model M { start S; } start A = M { next B; } state B;");
        let a = k.states.iter().position(|s| s == "A").unwrap();
        let b = k.states.iter().position(|s| s == "B").unwrap();
        assert!(k.successors(a).contains(&b), "next — ребро A → B");
        assert_eq!(
            k.states,
            vec!["A".to_string(), "B".to_string()],
            "состояние S вложенной модели M не должно попасть в вершины"
        );
        // Самопетля A (ожидание конца M) проверяется отдельно -
        // implement_state_next_is_not_an_unconditional_exit.
    }
}

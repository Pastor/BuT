//! Адресация цепочек `+` внутри составного состояния.
//!
//! Цепочка `A + B` ведётся машиной шагов, и у машины должно быть **своё** место
//! хранения: счётчик у цели `st`, регистр шага у цели `sv`, поле группы у цели `c`.
//! Пока цепочка была одна - верхнеуровневая, - местом служило имя несущего состояния.
//! Вложенная цепочка (`(A + B) | C`) ломает это допущение: цепочек в одном состоянии
//! бывает несколько, и различать их обязано **место в дереве композиции**, а не имя
//! состояния.
//!
//! Здесь живёт одно правило: **путь** - последовательность индексов от корня `extend`
//! состояния (у `Parallel` и у `Concatenation` элемент `i` получает `path + [i]`), а
//! имя строится из пути суфисправлением. Правило одно на цели, потому что расхождение здесь
//! тихое: объявленный регистр и напечатанное имя разошлись бы, и вывод перестал бы
//! собираться у чужого инструмента при нулевом коде возврата `taktc`.
//!
//! Обход и печать строят путь **по одному правилу, но порознь**: обход ([`chains`])
//! заводит места хранения, печать наращивает путь по ходу рекурсии. Тест здесь -
//! инструмент цели: не объявленный регистр даёт у `verilator` "Can't find definition of
//! variable".

use crate::semantic::minimap::StateExtend;

/// Найденная цепочка `+`: путь до её узла и число шагов.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Chain {
    /// Путь от корня `extend` состояния; пустой - цепочка верхнего уровня.
    pub path: Vec<usize>,
    /// Число шагов цепочки.
    pub len: usize,
}

impl Chain {
    /// Цепочка вложена (то есть не является `extend` самого состояния).
    pub fn nested(&self) -> bool {
        !self.path.is_empty()
    }
}

/// Суфисправление имени места хранения по пути: пустой путь - пустая строка.
///
/// Форма `_c<i>` на каждый шаг пути: имя остаётся идентификатором целевых языков.
pub(crate) fn suffix(path: &[usize]) -> String {
    let mut out = String::new();
    for i in path {
        out.push_str(&format!("_c{}", i));
    }
    out
}

/// Все цепочки составного состояния - верхнеуровневая и вложенные.
///
/// Порядок обхода детерминирован: сначала сам узел, затем его элементы слева направо.
pub(crate) fn chains(extend: &StateExtend) -> Vec<Chain> {
    let mut out = Vec::new();
    walk(extend, &mut Vec::new(), &mut out);
    out
}

fn walk(extend: &StateExtend, path: &mut Vec<usize>, out: &mut Vec<Chain>) {
    match extend {
        StateExtend::Concatenation(items) => {
            out.push(Chain {
                path: path.clone(),
                len: items.len(),
            });
            descend(items, path, out);
        }
        StateExtend::Parallel(items) => descend(items, path, out),
        StateExtend::Model(_, _) | StateExtend::None => {}
    }
}

fn descend(items: &[StateExtend], path: &mut Vec<usize>, out: &mut Vec<Chain>) {
    for (i, item) in items.iter().enumerate() {
        path.push(i);
        walk(item, path, out);
        path.pop();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::minimap::Name;

    fn model(name: &str) -> StateExtend {
        StateExtend::Model(Name::new(name.to_string(), name.to_string()), Vec::new())
    }

    #[test]
    fn suffix_of_empty_path_is_empty() {
        assert_eq!(suffix(&[]), "");
        assert_eq!(suffix(&[0]), "_c0");
        assert_eq!(suffix(&[1, 0]), "_c1_c0");
    }

    #[test]
    fn top_level_chain_has_empty_path() {
        let ex = StateExtend::Concatenation(vec![model("A"), model("B")]);
        assert_eq!(
            chains(&ex),
            vec![Chain {
                path: vec![],
                len: 2
            }]
        );
    }

    #[test]
    fn nested_chain_inside_parallel_is_addressed_by_position() {
        // `(A + B) | C`: цепочка стоит нулевым элементом параллели.
        let ex = StateExtend::Parallel(vec![
            StateExtend::Concatenation(vec![model("A"), model("B")]),
            model("C"),
        ]);
        let found = chains(&ex);
        assert_eq!(
            found,
            vec![Chain {
                path: vec![0],
                len: 2
            }]
        );
        assert!(found[0].nested(), "вложенная цепочка обязана отличаться");
    }

    #[test]
    fn parallel_without_chain_yields_nothing() {
        let ex = StateExtend::Parallel(vec![model("A"), model("B")]);
        assert!(chains(&ex).is_empty());
    }

    #[test]
    fn chain_inside_chain_step_is_found_too() {
        // `(A + (B + C))`: шаг цепочки сам цепочка.
        let ex = StateExtend::Concatenation(vec![
            model("A"),
            StateExtend::Concatenation(vec![model("B"), model("C")]),
        ]);
        assert_eq!(
            chains(&ex),
            vec![
                Chain {
                    path: vec![],
                    len: 2
                },
                Chain {
                    path: vec![1],
                    len: 2
                },
            ]
        );
    }
}

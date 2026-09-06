//! Когда состояние завершает автомат.
//!
//! # Правило
//!
//! Состояние завершает работу автомата, если у него **нет исходящих переходов И нет
//! тела**: `state Done;` - конец, потому что делать в нём нечего. Состояние С телом
//! переходов не требует: `start Run { always { lamp := 1; } }` горит вечно, а не гаснет
//! на втором такте.

/// Удерживает ли блок автомат в состоянии.
///
/// Удерживает то, что исполняется каждый такт: `always`, `every`, вставка в вывод цели.
/// Именно они и означают "состояние работает".
///
/// `enter` и `exit` - Одноразовы и потому не удерживают: первый про вход, второй про
/// уход, а не про пребывание. Конечное состояние вправе прибраться на входе (`state
/// Done { enter { ... } }` - обычная запись корпуса) и остаться концом; состояние с
/// одним лишь `exit` иначе получило бы мёртвый блок, который никогда не выполнится.
pub fn holds_machine(block: &str) -> bool {
    !matches!(block, "enter" | "exit")
}

/// Конец ли это состояние - по записи автора (рёбра и элементы тела).
///
/// Спрашивается на построении дерева, где узла ещё нет: вид "конец" присваивается там
/// же, где собираются элементы.
pub fn is_end<T>(references: &[T], elements: &[crate::parser::ast::StateElement]) -> bool {
    is_terminal(!references.is_empty(), elements_hold(elements))
}

/// Есть ли у состояния тело, исполняемое каждым тактом, - по записи автора.
///
/// Спрашивается на построении дерева, где узлов ещё нет: вид "конец" присваивается там
/// же, где собираются элементы. Правило одно с [`holds_machine`] - двух ответов на один
/// вопрос быть не должно.
pub fn elements_hold(elements: &[crate::parser::ast::StateElement]) -> bool {
    use crate::parser::ast::StateElement;
    elements.iter().any(|element| match element {
        StateElement::NamedBlockCode(block) => block
            .name
            .as_ref()
            .is_none_or(|name| holds_machine(&name.name)),
        StateElement::Every(_) | StateElement::Assembly(_) => true,
        _ => false,
    })
}

/// Держат ли автомат уже построенные блоки состояния.
///
/// Тот же вопрос, что у [`elements_hold`], но по узлам дерева: спрашивают печатники
/// целей и `SE-010`, у которых записи автора под рукой уже нет.
pub fn blocks_hold(
    blocks: &[crate::semantic::named_code_block::NamedCodeBlockDefinitionNode],
) -> bool {
    blocks.iter().any(|block| holds_machine(block.name()))
}

/// Завершает ли автомат узел состояния.
///
/// Правило одно на проект, и спрашивают его пятеро: построение дерева (через
/// [`elements_hold`]), признак узла, предупреждение `SE-010`, печатники четырёх целей и
/// эталон. Пока решение принимали порознь, менять его значило править пять ветвей -
/// забытая давала бы молчаливое расхождение эталона с целями, худший класс проекта.
pub fn node_is_terminal(state: &crate::semantic::StateNode) -> bool {
    use crate::semantic::StateNode;
    match state {
        StateNode::Unresolved => false,
        StateNode::Simple {
            references,
            named_blocks,
            ..
        } => is_terminal(!references.is_empty(), blocks_hold(named_blocks)),
        StateNode::Implement {
            references,
            next,
            named_blocks,
            ..
        } => is_terminal(
            !references.is_empty() || next.is_some(),
            blocks_hold(named_blocks),
        ),
    }
}

/// Завершает ли состояние автомат.
///
/// `has_edges` - есть ли исходящие переходы (`ref`, `next`); `has_body` - есть ли
/// именованные блоки (`always`, `enter`, `exit`).
///
/// Формулы телом не считаются: инвариант и охранная формула ничего не делают, они
/// судят. Состояние с одной формулой и без блоков - по-прежнему конец.
pub fn is_terminal(has_edges: bool, has_body: bool) -> bool {
    !has_edges && !has_body
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_state_with_a_body_never_ends_the_machine() {
        // Предмет - Правило, а не число: автор пишет "всегда", и получить "однажды" он
        // не должен ни у эталона, ни у одной из восьми целей.
        assert!(is_terminal(false, false), "пустое состояние — конец");
        assert!(!is_terminal(false, true), "тело без рёбер работает вечно");
        assert!(!is_terminal(true, false), "рёбра ведут дальше");
        assert!(!is_terminal(true, true), "и то и другое — не конец");
    }
}

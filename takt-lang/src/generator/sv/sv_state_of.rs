//! Форма `S(Модель) = Состояние` в цели `sv`.
//!
//! Отдельный модуль, потому что `sv_expr.rs` пришпилен лимитом размера, а знание "как
//! адресуется состояние соседа" самостоятельно: оно повторяет правило именования
//! регистров из `sv_fsm` и обязано меняться вместе с ним.

use super::sv_expr::Scope;
use crate::semantic::ConditionNode;

/// Разбирает форму `S(Модель) = Состояние` и даёт имена сигнала и варианта.
///
/// Возвращает `None`, если условие к этой форме не относится - тогда печатается обычное
/// сравнение.
pub(in crate::generator::sv) fn state_comparison(
    left: &ConditionNode,
    right: &ConditionNode,
) -> Option<(String, String)> {
    let model = crate::semantic::condition::state_of::state_of_model(left)?;
    let state = crate::semantic::condition::state_of::compared_state_name(right)?;
    let name = crate::semantic::minimap::Name::from(std::rc::Rc::clone(model));
    let is_root = model.borrow().upper.is_none();
    let reg = if is_root {
        "state".to_string()
    } else {
        format!("{}_state", name.unique_lowercase_snakecase())
    };
    let variant = format!(
        "{}_{}",
        name.unique_uppercase_snakecase(),
        crate::semantic::naming::normalize_lowercase_snakecase(state).to_uppercase()
    );
    Some((reg, variant))
}

/// Печатает сравнение состояния под-модели, если условие имеет форму `S(Модель) =
/// Состояние` (или `!=`); иначе - `None`.
///
/// Чтение идёт через [`Scope::read`], то есть из рабочей копии `_next`: наблюдатель
/// обязан увидеть переход соседа **на том же такте**, как в эталоне и в цели `c`.
/// Чтение регистра дало бы состояние предыдущего такта - модуль при этом валиден и
/// синтезируем, а трасса разъезжается, и поймать это может только потактовая сверка.
pub(in crate::generator::sv) fn print(node: &ConditionNode, scope: &Scope) -> Option<String> {
    let (left, right, op) = match node {
        ConditionNode::Equal(l, r) => (l, r, "=="),
        ConditionNode::NotEqual(l, r) => (l, r, "!="),
        _ => return None,
    };
    let (reg, variant) = state_comparison(left, right)?;
    Some(format!("({} {} {})", scope.read(&reg), op, variant))
}

//! Столкновения имён, неразрешимые в пространстве имён цели.
//!
//! # Обе позиции обязательны
//!
//! Диагностика называет **что с чем** столкнулось: без второй позиции автор снова
//! пойдёт разбираться в порождённый код - ровно то, ради устранения чего фича и
//! заведена.

use crate::diagnostics::Diagnostic;
use crate::diagnostics::lang::keys;
use crate::msg;
use crate::semantic::extend::Extend;
use crate::semantic::{ModelNode, StateNode};
use std::cell::RefCell;
use std::rc::Rc;

/// Нормализует имя так, как его видит пространство имён цели.
///
/// Цель `c` печатает и порт, и состояние в `UPPER_SNAKE_CASE` (`out settled` и `state
/// Settled` дают один `<ROOT>_M_SETTLED`), поэтому сравнивать "как написано"
/// бессмысленно: столкновение возникает **после** нормализации.
fn normalized(name: &str) -> String {
    name.to_uppercase()
}

/// **`SE-100`:** имя состояния совпадает с именем **дочерней** модели того же
/// владельца, которую это состояние реализует.
///
/// Имя файла к этому отношения не имеет: `start Pid = Pid;` ломается в любом файле, и
/// ломаются все четыре цели - карта моделей теряет запись, `rust` отвечает `RS-012`,
/// `sv` - `SV-002`, `c` печатает несуществующий тип, `iec2c` отвергает вывод `st`.
///
/// **Условие "дочерняя того же владельца" - не перестраховка, а граница правила.**
/// Состояние-тёзка соседней модели работает и переводится: `model Toggle { state Ping =
/// Ping; ... }` при `model Ping` уровнем выше даёт валидный C. Без этого условия
/// проверка отвергает законную запись.
fn state_model_collisions(model: &Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let borrowed = model.borrow();
    let mut found = Vec::new();
    for state in borrowed.states.values() {
        let StateNode::Implement {
            name,
            loc,
            implements,
            ..
        } = state
        else {
            continue;
        };
        let Extend::Model(target, use_loc, _) = implements else {
            continue;
        };
        let target_name = target.borrow().name.clone().unwrap_or_default();
        if target_name.is_empty() || normalized(&target_name) != normalized(name) {
            continue;
        }
        // Столкновение возникает, только если модель - Дочерняя той же модели, где
        // объявлено состояние: тогда имя состояния (поле) и имя модели (тип и поле)
        // попадают в одно пространство. Тёзка соседней модели законна.
        if !borrowed.models.contains_key(&target_name) {
            continue;
        }
        found.push(
            Diagnostic::error(
                *loc,
                msg!(
                    keys::SE_100_STATE_NAMED_AS_MODEL,
                    name = name,
                    model = target_name
                ),
            )
            .with_code("SE-100")
            .with_note(
                *use_loc,
                msg!(keys::NOTE_MODEL_USED_HERE, name = target_name),
            ),
        );
    }
    found
}

/// Проверка для модели (без рекурсии - вложенные обходит вызывающий).
pub(super) fn check_name_collisions(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    state_model_collisions(&model)
}

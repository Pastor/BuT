//! Модель без состояний в реализации состояния: `SE-106`.
//!
//! # Что различается
//!
//! "Состояний нет" - три разных случая, и отвечать на них надо разными сообщениями:
//!
//! - у модели состояния **есть**, а `start` среди них нет -> [`SE-011`] (забытая
//!   пометка);
//! - состояний нет **во всём файле**, поданном как вход исполнения -> `SE-102`
//!   (это библиотека, а не автомат);
//! - состояний нет у модели, **поставленной в реализацию** (`start S = M;`,
//!   `= A | B`, `= A + B`, `model M = A { ... }`) -> **`SE-106`**, этот модуль.
//!
//! Третий случай до не судил никто, и решение о такой программе принимал каждый
//! потребитель сам. Молчаливый успех и был худшим из ответов - текст `CC-005` его не
//! лечит.
//!
//! # Ловушка именования, ради которой нужна сноска
//!
//! `import "helper.takt";` вносит модель по имени **файла**, и если состояния файла
//! лежат у его **вложенной** модели, то снаружи видна обёртка - без состояний.
//! Вложенная модель при этом недоступна вовсе (`SE-001`), поэтому чинится это переносом
//! состояний на верхний уровень подключаемого файла. Голое "модель не содержит
//! состояний" автор такого файла принял бы за ложь: `start` у него написан. Поэтому
//! сноска называет вложенные модели, у которых состояния есть.
//!
//! [`SE-011`]: super::states::model_only_one_start_state

use super::*;
use crate::diagnostics::lang::keys;
use crate::msg;
use crate::semantic::extend::Extend;

/// Проверяет реализации состояний модели (`SE-106`).
///
/// Накапливает **по использованию**: `start App = E1 | E2;` с двумя пустыми моделями
/// даёт две диагностики, а не первую попавшуюся.
pub(super) fn validate_implemented_models(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let borrowed = model.borrow();
    let mut out = Vec::new();
    for state in borrowed.states.values() {
        if let StateNode::Implement { implements, .. } = state {
            collect_empty_models(implements, &mut out);
        }
    }
    out
}

/// Обходит выражение реализации, собирая диагностики по каждой ссылке на модель.
///
/// Спуск в состояния встреченной модели **не нужен**: в дереве, доходящем до
/// `validate`, реализация **плоская** - `A + B` остаётся `Extend::Concatenation` со
/// списком элементов, и каждый элемент этот обход видит сам. Упаковка цепочки в
/// синтетическую модель со ступенями отвергнута решением (её безусловный `next` между
/// ступенями дал бы неверный тайминг), а её реализация снята как мёртвая.
///
/// Заведут такую упаковку заново - обход придётся дополнить спуском: синтетической
/// модели нет в `models` родителя, рекурсия `validate_model_all` до её состояний не
/// дойдёт, и пустая модель внутри `A + B` уедет в цели молча.
fn collect_empty_models(extend: &Extend, out: &mut Vec<Diagnostic>) {
    match extend {
        Extend::Model(target, loc, _) => {
            if let Some(diagnostic) = empty_model_diagnostic(target, *loc) {
                out.push(diagnostic);
            }
        }
        Extend::Parentless(inner) => collect_empty_models(inner, out),
        Extend::Concatenation(items) | Extend::Parallel(items) => {
            for item in items {
                collect_empty_models(item, out);
            }
        }
        // `Unresolved` до `validate` не доживает (его снимает stage1), `None` -
        // отсутствие реализации у обычного состояния.
        Extend::None | Extend::Unresolved(_) => {}
    }
}

/// Строит `SE-106`, если у модели нет ни одного состояния.
fn empty_model_diagnostic(target: &Rc<RefCell<ModelNode>>, usage: Location) -> Option<Diagnostic> {
    let target = target.borrow();
    if !target.states.is_empty() {
        return None;
    }
    let name = target.name.clone().unwrap_or_default();
    let diagnostic = Diagnostic::error(usage, msg!(keys::SE_106_MODEL_WITHOUT_STATES, name = name))
        .with_code("SE-106");
    Some(match nested_with_states(&target) {
        Some(nested) => diagnostic.with_note(
            target.loc,
            format!(
                "модель '{name}' объявлена здесь; состояния есть у вложенной модели \
                 '{nested}', но обёртка их не наследует — перенесите состояния на \
                 верхний уровень"
            ),
        ),
        None => diagnostic.with_note(
            target.loc,
            msg!(keys::NOTE_MODEL_DECLARED_HERE, name = name),
        ),
    })
}

/// Имя первой вложенной модели, у которой состояния есть.
///
/// Нужно сноске: это ловушка именования при `import` (см. док модуля). Обход - по
/// `BTreeMap`, то есть детерминированный.
fn nested_with_states(model: &ModelNode) -> Option<String> {
    model
        .models
        .iter()
        .find(|(_, nested)| !nested.borrow().states.is_empty())
        .map(|(name, _)| name.clone())
}

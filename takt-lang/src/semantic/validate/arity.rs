//! Число аргументов вызова обязано совпадать с числом параметров - `SE-122`.

use super::*;
use crate::semantic::FunctionDefinitionNode;

/// Проверяет число аргументов вызова; `None` - вызов согласован.
///
/// Неразрешённое определение (`Unresolved`) проверяется **по сырому АСД**: тела функций
/// строятся стадией 5, а судьи `validate` идут после неё - но в ячейке ссылки лежит
/// снимок, снятый при разрешении имени (засада 0204).
pub(super) fn check_call(
    def: &Rc<RefCell<FunctionDefinitionNode>>,
    args: &[ExpressionNode],
) -> Option<Diagnostic> {
    let (name, expected, loc) = match &*def.borrow() {
        FunctionDefinitionNode::Builtin(name, params, _) => {
            ((*name).to_string(), params.len(), Location::Builtin)
        }
        FunctionDefinitionNode::Local { raw, .. } => (
            raw.name.as_ref()?.name.clone(),
            raw.params.len(),
            raw.name.as_ref()?.loc,
        ),
        FunctionDefinitionNode::External { name, params, .. } => {
            (name.clone(), params.len(), Location::Builtin)
        }
        FunctionDefinitionNode::Unresolved(raw) => (
            raw.name.as_ref()?.name.clone(),
            raw.params.len(),
            raw.name.as_ref()?.loc,
        ),
        // Имя не разрешилось ни во что: об этом говорит своя диагностика (`SE-004`)
        // ниже по конвейеру, и второй ответ на тот же вход здесь был бы шумом.
        FunctionDefinitionNode::None => return None,
    };
    if expected == args.len() {
        return None;
    }
    Some(
        Diagnostic::error(
            loc,
            format!(
                "функция '{name}': объявлено параметров {expected}, передано аргументов {}. \
                 Прежде такой вызов принимался молча: цель 'c' печатала код, который \
                 отвергает 'cc', а эталон останавливал прогон уже в такте",
                args.len()
            ),
        )
        .with_code("SE-122"),
    )
}

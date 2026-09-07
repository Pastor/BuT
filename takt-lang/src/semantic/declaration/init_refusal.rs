//! Отказы на **инициализаторе объявления**: почему запись, которую компилятор вычислить
//! не может, не доезжает до потребителей.
//!
//! Правил здесь четыре, и все они об одном месте - выражении, которое считается
//! **до первого такта**:
//!
//! | Код | Что запрещено |
//! |---|---|
//! | `SE-109` | ссылка **вперёд** на объявление ниже по тексту |
//! | `SE-114` | дробная арифметика, требующая **округления** |
//! | `SE-084` | вызов **внешней** функции |
//! | `SE-084` | вызов функции, которую вычислитель исполнить не смог |
//!
//! Граница модуля - ответственность: свёртка решает, чем стало значение, а эти судьи -
//! законна ли сама запись. Тексты диагностик живут рядом с признаками, которые их
//! порождают.

use super::{collect_identifiers, declaration_position, is_literal};
use crate::diagnostics::{Diagnostic, Location};
use crate::semantic::{ModelNode, VariableNode};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

/// Несвёрнутая дробная арифметика в инициализаторе - `SE-114`.
pub(super) fn initializer_calls_extern(
    source: &crate::parser::ast::Expression,
    model: &Rc<RefCell<ModelNode>>,
) -> Option<String> {
    use crate::parser::ast::Expression as E;
    // Форм внешней функции в узле две, и на этой стадии встречается вторая: тела
    // функций разрешаются стадией 5, а свёртка идёт стадией 2, поэтому в ячейке лежит
    // `Unresolved(FunctionDefine { external: true, ... })`. Пока проверялась только
    // форма `External`, ветвь была фактически мёртвой - отказ выдавал обход `validate`
    // (тот же код, свой текст), и находка эта принадлежит: до неё разницы не было
    // видно, потому что оба пути молчали об одном и том же входе одинаково.
    if let E::Function(_, id, _) = source
        && let Some(def) = model.borrow().search_func(&id.name)
        && match &*def.borrow() {
            crate::semantic::FunctionDefinitionNode::External { .. } => true,
            crate::semantic::FunctionDefinitionNode::Unresolved(raw) => raw.external,
            _ => false,
        }
    {
        return Some(id.name.clone());
    }
    let (left, right) = source.components();
    left.and_then(|e| initializer_calls_extern(e, model))
        .or_else(|| right.and_then(|e| initializer_calls_extern(e, model)))
}

/// Есть ли в инициализаторе вызов функции (любой, включая вложенный).
///
/// Признак **синтаксический**: спрашивается форма записи, а не разрешённое имя. Так и
/// надо - отказ выдаётся ровно тогда, когда автор **написал** вызов, а вычислитель на
/// нём споткнулся; имя, которое никуда не разрешилось, судит своя диагностика
/// (`SE-004`) ниже по конвейеру. Области видимости признак не спрашивает **намеренно**:
/// имя, которое никуда не разрешилось, - не предмет этого правила, и передавать сюда
/// модель значило бы обещать разбор, которого нет.
pub(super) fn initializer_calls_function(source: &crate::parser::ast::Expression) -> bool {
    use crate::parser::ast::Expression as E;
    if matches!(source, E::Function(_, _, _)) {
        return true;
    }
    let (left, right) = source.components();
    left.is_some_and(initializer_calls_function) || right.is_some_and(initializer_calls_function)
}

/// Инициализатор зовёт функцию, которую компилятор исполнить не смог, - `SE-084` с
/// **причиной** вычислителя.
pub(super) fn unfoldable_call(name: &str, cause: &Diagnostic, loc: Location) -> Diagnostic {
    // Цитируется причина, а не всё сообщение: общая вводная вычислителя ("выражение не
    // вычисляется при компиляции") здесь уже сказана своими словами, и оставленная
    // целиком она читалась бы как заикание. Вводная вида "функция 'f' не
    // вычисляется..." снимается тоже - имя функции в ней полезно, но повторяет то, что
    // автор видит в самой записи.
    let reason = cause
        .message
        .split_once("не вычисляется при компиляции: ")
        .map_or(cause.message.as_str(), |(_, tail)| tail);
    Diagnostic::error(
        loc,
        format!(
            "инициализатор '{name}' зовёт функцию, которую компилятор вычислить не может: \
             {reason}. Начальное значение выставляется до первого такта, и прежде \
             потребители расходились молча: эталон оставлял ноль, а цель 'st' теряла \
             инициализатор без единого слова. Присвойте в теле состояния — \
             'always {{ {name} := …; }}'"
        ),
    )
    .with_code("SE-084")
}

pub(super) fn unfoldable_fractional(
    source: &crate::parser::ast::Expression,
    name: &str,
    ty: Option<&crate::semantic::type_node::TypeNode>,
    loc: Location,
) -> Option<Diagnostic> {
    use crate::semantic::type_node::TypeNode;
    if is_literal(source) {
        return None;
    }
    if !matches!(ty, Some(TypeNode::Rational | TypeNode::Fixed { .. })) {
        return None;
    }
    if !contains_arithmetic(source) {
        return None;
    }
    Some(
        Diagnostic::error(
            loc,
            format!(
                "инициализатор '{name}' — дробное выражение, которое компилятор не может \
                 вычислить точно: округление дробных задано эталоном симулятора, и \
                 посчитав здесь, компилятор дал бы значение, которого симулятор не \
                 вычислит (прогон показал бы ноль, а прошивка — своё число, и молча). \
                 Задайте готовый литерал — например 'var {name}: … := 0.333;' — либо \
                 вычисляйте в теле состояния: 'always {{ {name} := …; }}'"
            ),
        )
        .with_code("SE-114"),
    )
}

/// Есть ли в выражении бинарная арифметика.
///
/// Обход идёт общим разбором [`ast::Expression::components`], поэтому новый узел АСД
/// сам собой попадает под спуск, а список арифметических форм остаётся коротким и
/// явным.
fn contains_arithmetic(expr: &crate::parser::ast::Expression) -> bool {
    use crate::parser::ast::Expression as E;
    if matches!(
        expr,
        E::Power(..)
            | E::Multiply(..)
            | E::Divide(..)
            | E::Modulo(..)
            | E::Add(..)
            | E::Subtract(..)
    ) {
        return true;
    }
    let (left, right) = expr.components();
    left.is_some_and(contains_arithmetic) || right.is_some_and(contains_arithmetic)
}

/// Ссылка вперёд: имя переменной, объявленной ниже по тексту.
///
/// Возвращает диагностику `SE-109`, если инициализатор `source` упоминает переменную
/// (`var`) той же карты объявлений, чьё объявление стоит после объявления `owner`.
///
/// # Что проверяется точно, а что оставлено законным
///
/// - имя в инициализаторе значит начальное значение и ссылается только назад
///   по тексту.
///
/// **Константы исключены намеренно:** у них ссылка вперёд разрешается проходами до
/// неподвижной точки и даёт согласованный результат у эталона и целей. Запрет сломал бы
/// работающие входы.
///
/// **Прочие невычислимые формы (порт, вызов функции, обращение к полю) остаются
/// законными:** правило судит ссылку вперёд, а не вычислимость.
pub(super) fn forward_reference(
    source: &crate::parser::ast::Expression,
    owner: &str,
    variables: &BTreeMap<String, VariableNode>,
) -> Option<Diagnostic> {
    let after = declaration_position(&variables[owner]);
    let mut names = Vec::new();
    collect_identifiers(source, &mut names);
    for (name, loc) in names {
        let Some(other) = variables.get(&name) else {
            continue;
        };
        // Только переменные: константа вперёд законна (см. заголовок функции).
        if !matches!(other, VariableNode::Simple { .. }) {
            continue;
        }
        if declaration_position(other) <= after {
            continue;
        }
        return Some(
            Diagnostic::error(
                loc,
                format!(
                    "переменная '{name}' объявлена ниже: в инициализаторе имя значит \
                     НАЧАЛЬНОЕ значение и ссылается только назад по тексту. \
                     Переставьте объявления либо возьмите константу"
                ),
            )
            .with_code("SE-109"),
        );
    }
    None
}

//! Отказ цели `c` на неподдерживаемой конструкции - единая воронка.
//!
//! # Что различается
//!
//! У цели `c` два разных повода отказать, и путать их нельзя:
//!
//! - **`CC-022`** (этот модуль) - конструкция в языке **есть**, а цель её не
//!   переводит. Это ответ **автору модели**: он написал то, что цель не умеет.
//! - **`CC-023`** ([`c_unresolved`](super::c_unresolved)) - узел не
//!   прошёл семантическое понижение. Это сообщение о **дефекте инструмента**,
//!   из корректной программы недостижимое.
//!
//! # Устройство
//!
//! Вид конструкции - перечисление, а не строка: тест обязан **перечислить** виды и
//! упасть списком, если какой-то потеряет текст или причину. Образец - `UnresolvedNode`
//! и `format::unsupported`.

use crate::diagnostics::lang::keys;
use crate::diagnostics::{Diagnostic, Location};
use crate::msg;

/// Конструкция языка, которую цель `c` не переводит.
///
/// Названия - **по-русски и по существу**: в тексте диагностики не должно быть имён
/// вариантов АСД, которые автор модели не видел никогда.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::generator::c) enum UnsupportedNode {
    /// Ссылка на модель в позиции выражения (`lvl := Ctl.ctrl;`).
    Model,
    /// Срез массива (`mem[1:2]`).
    ArraySlice,
    /// Блок кода в позиции выражения.
    CodeBlock,
    /// Именованная функция как значение.
    NamedFunction,
    /// Список параметров в позиции выражения.
    ParameterList,
    /// Тип в позиции выражения.
    Type,
    /// Адресный литерал в позиции выражения (`0x105:0`).
    Address,
    /// Встроенная функция, кода не порождающая (`debug`, `S`).
    Builtin(&'static str),
    /// Неизвестная встроенная функция.
    UnknownBuiltin,
    /// Разряд за пределом бит-вектора (`[bit;96]` и разряд 200).
    BitBeyondVector,
    /// Операция над широким бит-вектором, не выразимая по словам.
    WideBitVector(&'static str),
    /// Инициализатор широкого бит-вектора, не выразимый по словам.
    WideBitVectorInitializer,
}

impl UnsupportedNode {
    /// Название конструкции для текста диагностики.
    pub(in crate::generator::c) fn phrase(self) -> String {
        match self {
            UnsupportedNode::Model => msg!(keys::CC_022_NODE_MODEL),
            UnsupportedNode::ArraySlice => msg!(keys::CC_022_NODE_ARRAY_SLICE),
            UnsupportedNode::CodeBlock => msg!(keys::CC_022_NODE_CODE_BLOCK),
            UnsupportedNode::NamedFunction => msg!(keys::CC_022_NODE_NAMED_FUNCTION),
            UnsupportedNode::ParameterList => msg!(keys::CC_022_NODE_PARAMETER_LIST),
            UnsupportedNode::Type => msg!(keys::CC_022_NODE_TYPE),
            UnsupportedNode::Address => msg!(keys::CC_022_NODE_ADDRESS),
            UnsupportedNode::Builtin(name) => msg!(keys::CC_022_NODE_BUILTIN, name = name),
            UnsupportedNode::UnknownBuiltin => msg!(keys::CC_022_NODE_UNKNOWN_BUILTIN),
            UnsupportedNode::BitBeyondVector => msg!(keys::CC_022_NODE_BIT_BEYOND_VECTOR),
            UnsupportedNode::WideBitVector(op) => msg!(keys::CC_022_NODE_WIDE_BIT_VECTOR, op = op),
            UnsupportedNode::WideBitVectorInitializer => msg!(
                keys::CC_022_NODE_WIDE_BIT_VECTOR,
                op = msg!(keys::GEN_WHAT_INITIALIZER)
            ),
        }
    }

    /// Причина, по которой цель `c` конструкцию не переводит.
    ///
    /// Пустая строка означает "причина в самой конструкции и добавить нечего"; у
    /// остальных причина названа, как это делают `ST-011` и `RS-011`.
    pub(in crate::generator::c) fn reason(self) -> String {
        match self {
            UnsupportedNode::Model => msg!(keys::CC_022_WHY_MODEL),
            UnsupportedNode::ArraySlice => msg!(keys::CC_022_WHY_ARRAY_SLICE),
            UnsupportedNode::Builtin(_) => msg!(keys::CC_022_WHY_BUILTIN),
            UnsupportedNode::BitBeyondVector => msg!(keys::CC_022_WHY_BIT_BEYOND_VECTOR),
            UnsupportedNode::WideBitVector(_) | UnsupportedNode::WideBitVectorInitializer => {
                msg!(keys::CC_022_WHY_WIDE_BIT_VECTOR)
            }
            _ => String::new(),
        }
    }

    /// Все виды - для теста (перечисление обязано быть полным).
    #[cfg(test)]
    pub(in crate::generator::c) const ALL: [UnsupportedNode; 12] = [
        UnsupportedNode::Model,
        UnsupportedNode::ArraySlice,
        UnsupportedNode::CodeBlock,
        UnsupportedNode::NamedFunction,
        UnsupportedNode::ParameterList,
        UnsupportedNode::Type,
        UnsupportedNode::Address,
        UnsupportedNode::Builtin("debug"),
        UnsupportedNode::UnknownBuiltin,
        UnsupportedNode::BitBeyondVector,
        UnsupportedNode::WideBitVector("+"),
        UnsupportedNode::WideBitVectorInitializer,
    ];
}

/// Строит отказ цели `c` на неподдерживаемой конструкции - диагностику **`CC-022`**.
///
/// `loc` - позиция узла
/// ([`ExpressionNode::loc`](crate::semantic::ExpressionNode::loc)). Там, где узел
/// позиции не несёт (литералы, ссылки на объявления), она вырождается в
/// [`Location::Builtin`], и сообщение остаётся без координаты - это **названная**
/// граница, а не забывчивость: позиции у такого узла нет в дереве вовсе.
pub(in crate::generator::c) fn refuse(node: UnsupportedNode, loc: Location) -> Diagnostic {
    let reason = node.reason();
    let message = if reason.is_empty() {
        msg!(keys::CC_022_REFUSAL, what = node.phrase())
    } else {
        msg!(
            keys::CC_022_REFUSAL_WITH_REASON,
            what = node.phrase(),
            reason = reason
        )
    };
    Diagnostic::error(loc, message).with_code("CC-022")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Имена вариантов АСД, которые печатались в текстах до фичи.
    const AST_NAMES: [&str; 7] = [
        "ArraySlice",
        "CodeBlock",
        "NamedFunctionBox",
        "List",
        "Type",
        "Address",
        "Model",
    ];

    /// **Тест класса.** Каждый вид конструкции назван по-русски, отдаёт
    /// `CC-022` и не тащит в текст имя варианта АСД.
    ///
    /// Падает **списком**: новый вид, забытый в `phrase`, называется поимённо.
    #[test]
    fn every_unsupported_kind_is_named_in_russian_and_coded() {
        let mut seen: Vec<String> = Vec::new();
        let mut broken: Vec<String> = Vec::new();
        for node in UnsupportedNode::ALL {
            let diagnostic = refuse(node, Location::Codegen);
            if diagnostic.code.as_deref() != Some("CC-022") {
                broken.push(format!("{node:?}: код {:?}", diagnostic.code));
            }
            let phrase = node.phrase();
            if !diagnostic.message.contains(&phrase) {
                broken.push(format!("{node:?}: текст не называет вид конструкции"));
            }
            if let Some(found) = AST_NAMES
                .iter()
                .find(|name| diagnostic.message.contains(*name))
            {
                broken.push(format!(
                    "{node:?}: в тексте имя варианта АСД '{found}' (класс 0231)"
                ));
            }
            if !phrase.chars().any(|c| ('а'..='я').contains(&c)) {
                broken.push(format!("{node:?}: название не по-русски: {phrase}"));
            }
            if seen.contains(&phrase) {
                broken.push(format!("{node:?}: название не отличает вид от других"));
            }
            seen.push(phrase);
        }
        assert!(broken.is_empty(), "виды без корректного отказа: {broken:?}");
    }

    /// Позиция узла доезжает до диагностики: у `CC-022` координата - свойство
    /// сообщения, а не украшение.
    #[test]
    fn position_of_the_node_is_kept() {
        let probe = Location::Source(3, 17, 21);
        assert_eq!(refuse(UnsupportedNode::Model, probe).loc, probe);
    }
}

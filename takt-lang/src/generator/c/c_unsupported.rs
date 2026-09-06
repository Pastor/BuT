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

use crate::diagnostics::{Diagnostic, Location};

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
}

impl UnsupportedNode {
    /// Название конструкции для текста диагностики.
    pub(in crate::generator::c) fn phrase(self) -> String {
        match self {
            UnsupportedNode::Model => "ссылка на модель в выражении".to_string(),
            UnsupportedNode::ArraySlice => "срез массива".to_string(),
            UnsupportedNode::CodeBlock => "блок кода в позиции выражения".to_string(),
            UnsupportedNode::NamedFunction => "именованная функция как значение".to_string(),
            UnsupportedNode::ParameterList => "список параметров в позиции выражения".to_string(),
            UnsupportedNode::Type => "тип в позиции выражения".to_string(),
            UnsupportedNode::Address => "адресный литерал в выражении".to_string(),
            UnsupportedNode::Builtin(name) => format!("встроенная функция '{name}'"),
            UnsupportedNode::UnknownBuiltin => "неизвестная встроенная функция".to_string(),
            UnsupportedNode::BitBeyondVector => "разряд за пределом бит-вектора".to_string(),
            UnsupportedNode::WideBitVector(op) => {
                format!("операция '{op}' над бит-вектором шире 64 бит")
            }
        }
    }

    /// Причина, по которой цель `c` конструкцию не переводит.
    ///
    /// Пустая строка означает "причина в самой конструкции и добавить нечего"; у
    /// остальных причина названа, как это делают `ST-011` и `RS-011`.
    pub(in crate::generator::c) fn reason(self) -> &'static str {
        match self {
            UnsupportedNode::Model => {
                "обращение к переменной под-модели через имя модели языком не поддержано \
                 ни одним потребителем: значение под-модели читают через её порт"
            }
            UnsupportedNode::ArraySlice => {
                "в C нет операции среза, а тип-владелец у среза в Takt отсутствует"
            }
            UnsupportedNode::Builtin(_) => "она служит отладке и кода не порождает",
            UnsupportedNode::BitBeyondVector => {
                "разрядов за объявленной шириной у вектора нет, а доступ за границу \
                 массива слов — неопределённое поведение в порождённой прошивке"
            }
            UnsupportedNode::WideBitVector(_) => {
                "вектор шире 64 бит представлен массивом слов, и такой операции над \
                 словами не существует; её не поддерживает и эталон (SIM-005) — \
                 работайте с отдельными разрядами либо разбейте вектор на поля"
            }
            _ => "",
        }
    }

    /// Все виды - для теста (перечисление обязано быть полным).
    #[cfg(test)]
    pub(in crate::generator::c) const ALL: [UnsupportedNode; 11] = [
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
        format!("{} не транслируется в C целью 'c'", node.phrase())
    } else {
        format!("{} не транслируется в C целью 'c': {reason}", node.phrase())
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

//! Покрытие конструкций языка примерами.
//!
//! ## Что считается конструкцией
//!
//! Вариант перечисления АСД (`Statement::Match`, `Expression::Cast`,
//! `ModelElement::Clock`, ...) плюс несколько форм, отдельным вариантом не
//! являющихся, но наблюдаемых автором: ветка `else`, размещение порта `at`,
//! реализация модели через `=`, тело функции против `extern`. Признак -
//! **синтаксический**: покрытие меряется по разобранному дереву, а не по тексту
//! (слово в комментарии конструкцией не является).
//!
//! ## Устройство
//!
//! Обход - итеративный, поверх существующего раскрытия узла в дочерние
//! ([`crate::parser::depth`]): второго обхода дерева в проекте заводить нельзя, он
//! разошёлся бы с первым. Классификация - [`construct`], разбор там исчерпывающий
//! (`deny(clippy::wildcard_enum_match_arm)`), поэтому новый узел языка валит сборку, а
//! не выпадает из счёта молча.

mod construct;

use super::ast;
use super::depth::{NodeRef, push_children};
use std::collections::BTreeSet;

/// Формы, конструкцией языка являющиеся, а вариантом АСД - нет.
///
/// Автор их пишет и видит, но в дереве они выражены полем, а не отдельным узлом:
/// реализация через `=`, ветка `else`, размещение порта `at`, тело функции против
/// `extern`. Перечень публичен, потому что его берёт проверка покрытия: свой список он
/// завёл бы вторым носителем одного набора.
pub const FLAT_CONSTRUCTS: &[&str] = &[
    "FunctionDefine::extern",
    "FunctionDefine::local",
    "Model::implements",
    "StateDefine::implements",
    "Statement::If::else",
    "VariableDefine::Port::at",
];

/// Виды конструкций, встреченных в модели и её комментариях.
///
/// Множество упорядочено (`BTreeSet`): отчёт проверки должен быть детерминирован - список
/// непокрытого читает человек, и порядок в нём не должен плясать от прогона к прогону.
///
/// # Примеры
///
/// ```
/// use takt_lang::parse;
/// use takt_lang::parser::coverage;
///
/// let (model, comments) = parse("model M { start S { next Done; } state Done; }", 0).unwrap();
/// let kinds = coverage::constructs_of(&model, &comments);
/// assert!(kinds.contains("StateKind::Start"));
/// assert!(kinds.contains("StateElement::Next"));
/// assert!(!kinds.contains("Statement::Match"));
/// ```
pub fn constructs_of(model: &ast::Model, comments: &[ast::Comment]) -> BTreeSet<&'static str> {
    let mut found = BTreeSet::new();
    let mut kinds = Vec::new();
    let mut stack = vec![NodeRef::Model(model)];
    while let Some(node) = stack.pop() {
        kinds.clear();
        construct::classify(node, &mut kinds);
        found.extend(kinds.iter().copied());
        push_children(node, &mut stack);
    }
    found.extend(comments.iter().map(construct::comment_kind));
    found
}

#[cfg(test)]
mod tests {
    use super::constructs_of;
    use crate::parse;

    /// Разбирает исходник и возвращает виды встреченных конструкций.
    fn kinds(source: &str) -> Vec<&'static str> {
        let (model, comments) = parse(source, 0).expect("исходник пробы обязан разбираться");
        constructs_of(&model, &comments).into_iter().collect()
    }

    #[test]
    fn plain_model_covers_state_kinds() {
        let found = kinds("model M { start S { next Done; } state Done; }");
        assert!(found.contains(&"StateKind::Start"), "{found:?}");
        assert!(found.contains(&"StateElement::Next"), "{found:?}");
    }

    /// Плоские свойства - не варианты перечисления, и без явного добора они терялись
    /// бы: направление порта, ветка `else`, вид цикла, `_` в `match`.
    #[test]
    fn flat_properties_are_collected() {
        let found = kinds(
            "model M {\n\
             inout io: bit at 0x40;\n\
             start S {\n\
             always {\n\
             if io.0 { io.0 := 0; } else { io.0 := 1; }\n\
             while io.0 { break; }\n\
             match io.0 { 1 => { continue; } _ => { } }\n\
             }\n\
             }\n\
             }",
        );
        for expected in [
            "PortDirection::InOut",
            "VariableDefine::Port::at",
            "Statement::If::else",
            "LoopKeyword::While",
            "MatchPattern::Wildcard",
            "MatchPattern::Value",
            "Statement::Break",
            "Statement::Continue",
        ] {
            assert!(found.contains(&expected), "нет {expected}: {found:?}");
        }
    }

    /// Комментарий живёт рядом с деревом, а не в нём: без отдельного сбора формы
    /// комментария не считались бы вовсе.
    #[test]
    fn comment_forms_are_collected() {
        let found = kinds("/// док\n// строка\n/* блок */\nmodel M { start S; }");
        assert!(found.contains(&"Comment::DocLine"), "{found:?}");
        assert!(found.contains(&"Comment::Line"), "{found:?}");
        assert!(found.contains(&"Comment::Block"), "{found:?}");
    }

    /// Контроль: то, чего в исходнике нет, покрытым не считается - иначе "покрыто всё"
    /// означало бы лишь, что множество наполняется без разбора.
    #[test]
    fn absent_constructs_are_not_reported() {
        let found = kinds("model M { start S; }");
        for absent in [
            "Statement::Match",
            "Expression::Cast",
            "ModelElement::Clock",
            "PortDirection::InOut",
            "Comment::Line",
        ] {
            assert!(!found.contains(&absent), "лишнее {absent}: {found:?}");
        }
    }
}

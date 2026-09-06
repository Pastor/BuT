//! Блоки, адресованные не самой модели: `formula` и `assembly`.
//!
//! # Устройство
//!
//! - `formula [диалект] { ... }` - обязательство **внешнему анализатору**:
//!   компилятор его не переводит и не проверяет, цели и эталон пропускают.
//! - `assembly ["цель"] { ... }` - операторы Takt, попадающие в вывод названной
//!   цели; без имени - во все, и тогда их исполняет эталон.
//!
//! Имя цели - **язык вывода**, а не режим сборки: `c-hal` и `c` печатают один язык, и
//! метка у них общая (`"c"`). Режимное имя отвергается с подсказкой - молчаливо
//! недействующая метка хуже отказа.

use crate::diagnostics::Diagnostic;
#[cfg(test)]
use crate::diagnostics::Location;
use crate::parser::ast;
use crate::semantic::{
    FunctionDefinitionNode, ModelNode, NamedCodeBlockDefinitionNode, StateNode, StatementNode,
};
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

/// Языки вывода, которые можно назвать в `assembly`.
///
/// Список - языки, а не цели CLI: режимы `c-hal`, `st-at` и `sv-mmio` печатают тот же
/// язык, что и базовая цель, и своей метки не имеют.
const TARGET_LANGUAGES: &[&str] = &["c", "st", "rust", "sv", "plantuml"];

/// Режимные имена целей: язык у них тот же, метка - базовая.
const TARGET_MODES: &[(&str, &str)] = &[("c-hal", "c"), ("st-at", "st"), ("sv-mmio", "sv")];

/// Проверяет имя цели в `assembly "имя"` и возвращает его.
///
/// # Ошибки
/// [`SE-129`], если имя не является языком вывода. Без проверки опечатка
/// (`assembly "С"` с кириллической буквой) молча выключала бы вставку во всех
/// целях - то есть автор получал бы пустой вывод при нулевом коде возврата.
pub(crate) fn check_target(dialect: &ast::StringLiteral) -> Result<String, Diagnostic> {
    let name = dialect.string.clone();
    if TARGET_LANGUAGES.contains(&name.as_str()) {
        return Ok(name);
    }
    let hint = match TARGET_MODES.iter().find(|(mode, _)| *mode == name) {
        Some((mode, base)) => format!(
            "'{mode}' — режим сборки, а не язык вывода: тот же язык печатает цель '{base}'. \
             Напишите \"{base}\""
        ),
        None => format!("допустимые имена: {}", TARGET_LANGUAGES.join(", ")),
    };
    Err(Diagnostic::error(
        dialect.loc,
        format!("вставка assembly адресована неизвестной цели '{name}': {hint}"),
    )
    .with_code("SE-129"))
}

/// Печатает ли цель с языком вывода `language` тело вставки `target`.
///
/// Безымянная вставка принадлежит всем целям; именованная - только своей.
pub(crate) fn emits_for(target: Option<&str>, language: &str) -> bool {
    match target {
        None => true,
        Some(name) => name == language,
    }
}

/// Язык вывода цели - метка, вставки которой она печатает.
///
/// Режимы печатают язык базовой цели: `c-hal` - тот же C, `sv-mmio` - тот же
/// SystemVerilog. Поэтому меток пять, а целей восемь.
pub(crate) fn label_of(language: &crate::generator::Language) -> &'static str {
    use crate::generator::Language;
    match language {
        Language::C => "c",
        Language::ST => "st",
        Language::Rust => "rust",
        Language::SV | Language::SvMmio => "sv",
        Language::PlantUML => "plantuml",
    }
}

/// Оставляет в дереве только те вставки, которые печатает язык `language`.
///
/// Отбор живёт **в семантике**, а не в печатниках, и это не вкус. Признак "переменная
/// используется" считается по дереву (`semantic::unused`), и он не знает цели: оставь
/// мы вставку чужой цели в дереве, у `c` переменная, упомянутая только в ней,
/// объявлялась бы и не использовалась, а без вставки - печаталась бы в теле, не будучи
/// объявленной (`cc`: "no member named ..."). Обе половины этого класса пойманы пробой
/// 2026-09-02.
///
/// Своя вставка **разворачивается в тело**, чужая - исчезает; после прохода узла
/// [`StatementNode::Assembly`] в дереве цели нет. Ветви печатников остаются защитой в
/// глубину: через библиотечный API проход можно и не позвать.
pub(crate) fn prune(model: &Rc<RefCell<ModelNode>>, language: &str) {
    let mut visited = HashSet::new();
    prune_model(model, language, &mut visited);
}

fn prune_model(
    model: &Rc<RefCell<ModelNode>>,
    language: &str,
    visited: &mut HashSet<*const RefCell<ModelNode>>,
) {
    if !visited.insert(Rc::as_ptr(model)) {
        return; // разделяемая под-модель уже обработана
    }
    // Ссылки на вложенные модели снимаются до мутирующего заимствования.
    let nested: Vec<Rc<RefCell<ModelNode>>> = model.borrow().models.values().cloned().collect();
    {
        let mut b = model.borrow_mut();
        let names: Vec<String> = b.functions.keys().cloned().collect();
        for name in names {
            let mut f = b.functions.get(&name).cloned().unwrap();
            prune_function(&mut f, language);
            b.functions.insert(name, f);
        }
        for blk in b.named_blocks.iter_mut() {
            prune_block(blk, language);
        }
        for st in b.states.values_mut() {
            prune_state(st, language);
        }
    }
    for child in &nested {
        prune_model(child, language, visited);
    }
}

fn prune_state(state: &mut StateNode, language: &str) {
    match state {
        StateNode::Simple { named_blocks, .. } | StateNode::Implement { named_blocks, .. } => {
            for blk in named_blocks.iter_mut() {
                prune_block(blk, language);
            }
        }
        StateNode::Unresolved => {}
    }
}

fn prune_block(block: &mut NamedCodeBlockDefinitionNode, language: &str) {
    match block {
        NamedCodeBlockDefinitionNode::Enter { body, .. }
        | NamedCodeBlockDefinitionNode::Exit { body, .. }
        | NamedCodeBlockDefinitionNode::Always { body, .. }
        | NamedCodeBlockDefinitionNode::Unknown { body, .. }
        | NamedCodeBlockDefinitionNode::Every { body, .. } => prune_stmt(body, language),
        NamedCodeBlockDefinitionNode::None | NamedCodeBlockDefinitionNode::Unresolved(_, _) => {}
    }
}

fn prune_function(f: &mut FunctionDefinitionNode, language: &str) {
    // Тело правится у **разрешённой** формы: у `Unresolved` и `External` операторов
    // нет, а АСД-копия `raw` служит константному вычислителю и печати не касается.
    if let FunctionDefinitionNode::Local { body, .. } = f {
        prune_stmt(body, language);
    }
}

/// Заменяет вставки в операторе: своя - телом, чужая - пустым оператором.
fn prune_stmt(stmt: &mut StatementNode, language: &str) {
    match stmt {
        StatementNode::Assembly { target, body, .. } => {
            let mine = emits_for(target.as_deref(), language);
            prune_stmt(body, language);
            *stmt = if mine {
                std::mem::replace(body.as_mut(), StatementNode::None)
            } else {
                StatementNode::None
            };
        }
        StatementNode::Block(items) => items.iter_mut().for_each(|s| prune_stmt(s, language)),
        StatementNode::If { then_, else_, .. } => {
            prune_stmt(then_, language);
            if let Some(alt) = else_ {
                prune_stmt(alt, language);
            }
        }
        StatementNode::Loop { body, .. } => prune_stmt(body, language),
        StatementNode::For { init, body, .. } => {
            if let Some(init) = init {
                prune_stmt(init, language);
            }
            prune_stmt(body, language);
        }
        StatementNode::Match { arms, .. } => {
            arms.iter_mut()
                .for_each(|a| prune_stmt(&mut a.body, language));
        }
        StatementNode::None
        | StatementNode::Unresolved(_)
        | StatementNode::Expression(_, _)
        | StatementNode::Variable(_, _, _, _)
        | StatementNode::Return(_, _)
        | StatementNode::Continue(_)
        | StatementNode::Break(_)
        | StatementNode::Formula(_)
        | StatementNode::InlineFormula(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn literal(text: &str) -> ast::StringLiteral {
        ast::StringLiteral {
            loc: Location::Codegen,
            unicode: false,
            string: text.to_string(),
        }
    }

    /// Языки вывода принимаются, и все пять (тест падает списком).
    #[test]
    fn output_languages_are_accepted() {
        let refused: Vec<_> = TARGET_LANGUAGES
            .iter()
            .filter(|name| check_target(&literal(name)).is_err())
            .collect();
        assert!(refused.is_empty(), "отвергнуты языки вывода: {refused:?}");
    }

    /// Режимное имя отвергается, и отказ называет базовую цель.
    ///
    /// Подсказка здесь - не вежливость: `c-hal` печатает тот же C, и автор, написавший
    /// режим, ожидал вставки именно в него.
    #[test]
    fn build_mode_is_refused_with_hint() {
        for (mode, base) in TARGET_MODES {
            let err = check_target(&literal(mode)).expect_err("режим — не язык вывода");
            assert_eq!(err.code.as_deref(), Some("SE-129"));
            assert!(
                err.message.contains(base),
                "отказ обязан назвать базовую цель: {}",
                err.message
            );
        }
    }

    /// Неизвестное имя отвергается и перечисляет допустимые.
    #[test]
    fn unknown_target_is_refused() {
        let err = check_target(&literal("С")).expect_err("кириллическая 'С' — не язык вывода");
        assert_eq!(err.code.as_deref(), Some("SE-129"));
        assert!(err.message.contains("rust"), "{}", err.message);
    }

    /// Безымянная вставка принадлежит каждой цели, именованная - своей.
    #[test]
    fn ownership_follows_the_label() {
        assert!(emits_for(None, "c") && emits_for(None, "sv"));
        assert!(emits_for(Some("c"), "c"));
        assert!(!emits_for(Some("c"), "rust"));
    }
}

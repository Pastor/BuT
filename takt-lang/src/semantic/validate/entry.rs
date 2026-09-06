//! Файл в позиции входа исполнения: `SE-102`.
//!
//! # Что различается
//!
//! Единица компиляции без единого состояния - **библиотека**: набор типов, функций и
//! переменных, предназначенный для подключения `import`. Такой файл законен и строится
//! семантикой полностью; неверно лишь **применить его как вход** - скомпилировать в
//! цель или подать симулятору. Автомата в нём нет, исполнять нечего.
//!
//! Признак - **отсутствие состояний вовсе**, а не отсутствие стартового. Файл, где
//! состояния есть, но `start` среди них нет, - это забытая пометка, и о ней говорит
//! `SE-011` ("должно быть только одно начальное состояние"). Спутать эти два случая
//! значит ответить автору библиотеки сообщением о пропущенном `start`, которого он не
//! писал, а автору автомата - "файл библиотечный", хотя он писал автомат.

use crate::diagnostics::Diagnostic;
use crate::semantic::ModelNode;
use std::cell::RefCell;
use std::rc::Rc;

/// Отвергает библиотечный файл в позиции входа исполнения (`SE-102`).
///
/// Возвращает `None`, если у модели есть хоть одно состояние, - то есть файл описывает
/// автомат и применён по назначению.
pub fn validate_entry_model(model: &Rc<RefCell<ModelNode>>) -> Option<Diagnostic> {
    let borrowed = model.borrow();
    if !borrowed.states.is_empty() {
        return None;
    }
    Some(
        Diagnostic::error(
            borrowed.loc,
            "файл не содержит ни одного состояния: это библиотека — набор типов, \
             функций и переменных для подключения через 'import', а не автомат. \
             Скомпилируйте (или подайте симулятору) файл, который её импортирует, \
             либо добавьте сюда стартовое состояние ('start Имя;')"
                .to_string(),
        )
        .with_code("SE-102"),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    fn build(src: &str) -> Rc<RefCell<ModelNode>> {
        let (ast, _) = crate::parse(src, 0).expect("разбор фикстуры");
        construct_model(&ast, None, &[]).expect("построение дерева")
    }

    #[test]
    fn library_without_states_is_rejected_as_entry() {
        let model = build("struct P {\n    x: u8\n}\n\nfn f(p: P) -> P {\n    return p;\n}\n");
        let d = validate_entry_model(&model).expect("библиотека входом быть не может");
        assert_eq!(d.code.as_deref(), Some("SE-102"));
        assert!(d.message.contains("import"), "{}", d.message);
    }

    #[test]
    fn model_with_state_is_accepted() {
        let model = build("var v: u8 := 0;\nstart S {\n    always {\n        v := 1;\n    }\n}\n");
        assert!(
            validate_entry_model(&model).is_none(),
            "файл с автоматом — законный вход"
        );
    }
}

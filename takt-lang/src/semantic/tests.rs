//! Тесты семантических узлов (вынесены из `semantic/mod.rs` задачей 0535-03).
//!
//! Причина выноса — правило размера модуля: `mod.rs` стоит сверх предела
//! (реестр долга `scripts/module-size-baseline.txt`), и расти ему нельзя.
//! Приём тот же, которым фичи 0088 и 0225 делили `expression.rs` и
//! `statement.rs`: «логика / тесты» отдельными файлами.

use super::*;
use crate::diagnostics::Diagnostic;
use crate::parse;
use crate::semantic::tree::construct_model;

// ─── Тесты: отсутствие циклических сильных ссылок (SA8) ──────────────────

/// Корневая модель с переменными не создаёт сильных циклов.
///
/// После построения модели счётчик сильных ссылок на корневой Rc должен
/// быть равен 1 — только наш handle. Если бы `upper` переменных был Rc,
/// он увеличил бы счётчик до 1 + N (по количеству переменных).
#[test]
fn model_with_vars_has_no_strong_cycle() {
    let (ast, _) = parse("var a: bit := false; var b: bit := false; start S;", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    // Единственный сильный владелец — наша переменная `root`
    assert_eq!(
        Rc::strong_count(&root),
        1,
        "корневой Rc должен иметь счётчик 1 (нет циклических Rc-ссылок)"
    );
}

/// Вложенная модель не создаёт сильных циклов через upper.
///
/// Модель M имеет `upper` → корень через Weak. Счётчик корня = 1.
#[test]
fn nested_model_has_no_strong_cycle() {
    let (ast, _) = parse("model M { start S; } start Main = M;", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    assert_eq!(
        Rc::strong_count(&root),
        1,
        "корень с вложенной моделью должен иметь счётчик 1"
    );
}

/// При удалении корневой Rc все Weak-ссылки из переменных становятся недействительными.
///
/// Это доказывает, что циклов нет: дерево освобождается корректно.
#[test]
fn upper_weak_invalidated_after_drop() {
    let (ast, _) = parse("var x: bit := false; start S;", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    // Получаем переменную и сохраняем Weak через upper
    let var_x = root
        .borrow()
        .search_var("x")
        .expect("x должна быть найдена");
    let weak_upper = match var_x {
        VariableNode::Simple { ref upper, .. } => upper.clone(),
        _ => panic!("ожидался Simple"),
    };
    // Пока root жив — upgrade() работает
    assert!(
        weak_upper.as_ref().and_then(|w| w.upgrade()).is_some(),
        "upper должен быть жив, пока root существует"
    );
    // Удаляем root — Weak должен стать недействительным
    drop(root);
    assert!(
        weak_upper.as_ref().and_then(|w| w.upgrade()).is_none(),
        "upper должен стать недействительным после drop(root)"
    );
}

/// `upper()` метод переменной возвращает Some, если модель жива.
#[test]
fn variable_upper_returns_some_while_model_alive() {
    let (ast, _) = parse("var x: bit := false;", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    let var_x = root
        .borrow()
        .search_var("x")
        .expect("x должна быть найдена");
    assert!(
        var_x.upper().is_some(),
        "upper() переменной должен возвращать Some пока модель жива"
    );
}

/// Вложенная модель имеет upper() указывающий на родителя.
#[test]
fn nested_model_upper_points_to_parent() {
    let (ast, _) = parse("model Inner { start S; } start Main = Inner;", 0).unwrap();
    let root = construct_model(&ast, None, &[]).unwrap();
    let inner = root
        .borrow()
        .search_model("Inner")
        .expect("Inner не найдена");
    let parent = inner.borrow().upper.as_ref().and_then(|w| w.upgrade());
    assert!(
        parent.is_some(),
        "Inner должна иметь upper → родительскую модель"
    );
    // Родитель — анонимная корневая модель (name = None)
    assert_eq!(
        parent.unwrap().borrow().name,
        None,
        "родитель Inner должен быть анонимной корневой моделью"
    );
}

// ─── Diagnostic ──────────────────────────────────────────────────────

/// Debug-вывод Diagnostic не паникует.
#[test]
fn diagnostic_debug() {
    let d = Diagnostic::error(crate::diagnostics::Location::Codegen, "ошибка".to_string());
    let _ = format!("{:?}", d);
}

// ─── ModelNode ───────────────────────────────────────────────────────

/// ModelNode по умолчанию не содержит состояний.
#[test]
fn model_node_default_has_no_states() {
    let node = ModelNode::default();
    assert!(!node.has_states());
}

/// ModelNode с одним состоянием: has_states() → true.
#[test]
fn model_node_with_state_has_states() {
    let mut node = ModelNode::default();
    node.states.insert("S".to_string(), StateNode::default());
    assert!(node.has_states());
}

/// Debug-вывод ModelNode не паникует.
#[test]
fn model_node_debug() {
    let node = ModelNode::default();
    let _ = format!("{:?}", node);
}

/// Поиск именованного блока в ModelNode.
#[test]
fn model_node_get_named_block() {
    let mut node = ModelNode::default();
    node.named_blocks
        .push(NamedCodeBlockDefinitionNode::Always {
            upper: None,
            body: StatementNode::None,
        });
    assert!(node.get_named_block("always").is_some());
    assert!(node.get_named_block("enter").is_none());
}

// ─── StateNode ───────────────────────────────────────────────────────

/// StateNode::default() равен Unresolved.
#[test]
fn state_node_default_is_unresolved() {
    assert_eq!(StateNode::default(), StateNode::Unresolved);
}

/// Поиск именованного блока в StateNode.
#[test]
fn state_node_get_named_block() {
    let state = StateNode::Simple {
        upper: None,
        name: "S".to_string(),
        named_blocks: vec![NamedCodeBlockDefinitionNode::Enter {
            upper: None,
            body: StatementNode::None,
        }],
        references: vec![],
        kind: StateNodeKind::Simple,
        loc: Default::default(),
        formulas: vec![],
    };
    assert!(state.get_named_block("enter").is_some());
    assert!(state.get_named_block("exit").is_none());
    assert_eq!(state.name(), "S");
}

// ─── Reference ──────────────────────────────────────────────────────

/// Создание Reference<StateNode> с Unresolved-объектом.
#[test]
fn reference_unresolved() {
    let r: ReferenceNode<StateNode> = ReferenceNode {
        location: Default::default(),
        name: "X".to_string(),
        cond: ConditionNode::None,
        object: Box::new(StateNode::Unresolved),
    };
    assert_eq!(r.name, "X");
    assert_eq!(r.cond, ConditionNode::None);
    assert_eq!(*r.object, StateNode::Unresolved);
}

/// Reference по умолчанию (Default).
#[test]
fn reference_default() {
    let r: ReferenceNode<StateNode> = ReferenceNode::default();
    assert!(r.name.is_empty());
}

// ─── Condition ──────────────────────────────────────────────────────

/// Condition::default() равен None.
#[test]
fn condition_default_is_none() {
    assert_eq!(ConditionNode::default(), ConditionNode::None);
}

/// Заглушки-узлы реализуют Default.
#[test]
fn stub_nodes_default() {
    let _ = NamedCodeBlockDefinitionNode::default();
    let _ = FunctionNode::default();
    let _ = VariableNode::default();
    let _ = TypeNode::default();
    let _ = ConditionDefinitionNode::default();
}

/// NamedCodeBlock методы name() и statement().
#[test]
fn named_code_block_methods() {
    let nb = NamedCodeBlockDefinitionNode::Always {
        upper: None,
        body: StatementNode::Continue(Location::Codegen),
    };
    assert_eq!(nb.name(), "always");
    assert_eq!(
        nb.statement(),
        Some(&StatementNode::Continue(Location::Codegen))
    );

    let nb_none = NamedCodeBlockDefinitionNode::None;
    assert_eq!(nb_none.name(), "");
    assert_eq!(nb_none.statement(), None);
}

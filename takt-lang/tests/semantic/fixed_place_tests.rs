//! Понижение q-литерала в поле и элементе: предпосылки правки.
//!
//! Значения тестыт потактовая сверка
//! (`takt-sim/tests/conformance/conformance_fixed_place_tests.rs`). Здесь - два
//! утверждения **об устройстве**, на которых стоит правка и которые иначе проверить
//! нечем.

use std::cell::RefCell;
use std::rc::Rc;

use takt_lang::semantic::tree::construct_model;
use takt_lang::semantic::{ConditionNode, ModelNode, StateNode};

const SRC: &str = r#"
struct Gains {
    kp: q(8, 8),
    ki: q(8, 8)
}

var g: Gains := {0.5, 0.25};
var n: u8 := 0;

out live: u8 at 0;

start Run {
    always {
        n := n + 1;
        g.kp := 2.0;
        live := g.kp as u8;
    }
    ref Done: g.ki > 4.0;
    ref Run: n < 100;
}

state Done { }
"#;

fn build(src: &str) -> Rc<RefCell<ModelNode>> {
    let (ast, _) = takt_lang::parse(src, 0).expect("разбор");
    construct_model(&ast, None, &[]).expect("семантика")
}

fn edge_conditions(model: &Rc<RefCell<ModelNode>>) -> Vec<ConditionNode> {
    let b = model.borrow();
    b.states
        .values()
        .flat_map(|state| match state {
            StateNode::Simple { references, .. } | StateNode::Implement { references, .. } => {
                references
                    .iter()
                    .map(|r| r.cond.clone())
                    .collect::<Vec<_>>()
            }
            StateNode::Unresolved => Vec::new(),
        })
        .collect()
}

/// Условие ребра со сравнением доезжает до понижения **разрешённым**.
///
/// Это предпосылка, а не косметика. обходила ещё и сырой `ast::Condition`, полагаясь на
/// правило "условия рёбер `ref` не разрешаются". Правило верно для **одной формы** -
/// паттерна `S(Модель) = Состояние`; остальные разрешает стадия 6, и
/// `resolve_condition` отдаёт `Unresolved` ровно на неразрешённом **имени**, где ни
/// литерала, ни сравнения не бывает. Зонд 2026-08-22 подтвердил: обход сырого АСД не
/// сработал ни разу, и он снят.
///
/// Тест падает, если проход понижения переедет **выше** стадии 6 - тогда условие ребра
/// снова станет сырым, а понижение молча перестанет работать.
#[test]
fn edge_condition_is_resolved_before_lowering() {
    let model = build(SRC);
    let conds = edge_conditions(&model);
    assert_eq!(conds.len(), 2, "у состояния Run два ребра: {conds:?}");
    for cond in &conds {
        assert!(
            !matches!(cond, ConditionNode::Unresolved(_)),
            "условие ребра обязано быть разрешено к моменту понижения: {cond:?}"
        );
    }
}

/// Литерал понижён во всех местах, где приёмник - поле (значения - у сверки).
#[test]
fn field_receiver_lowers_literal() {
    let model = build(SRC);
    let printed = format!("{:?}", edge_conditions(&model));
    // 4.0 в q(8, 8) - это 1024; сырой литерал остался бы `Rational("4.0")`.
    assert!(
        printed.contains("1024"),
        "литерал сравнения с полем обязан быть понижен: {printed}"
    );
    assert!(
        !printed.contains("Rational"),
        "сырого дробного литерала в условии быть не должно: {printed}"
    );
}

/// Знание о спуске по типу - Одно: проход понижения своего разбора не ведёт.
///
/// Греп, а не типы: вторая копия правила "структура -> поле, массив -> элемент" -
/// обычный `match`, и компилятор её не запретит. Носитель -
/// `semantic::validate::base_type`.
#[test]
fn lowering_pass_has_no_own_type_descent() {
    let src = include_str!("../../src/semantic/type_node/fixed_body.rs");
    let code: String = src
        .lines()
        .filter(|l| !l.trim_start().starts_with("//"))
        .collect::<Vec<_>>()
        .join("\n");
    for forbidden in ["search_struct", "TypeNode::Struct", "TypeNode::Array"] {
        assert!(
            !code.contains(forbidden),
            "в проходе понижения появилось собственное знание о типе места \
             ('{forbidden}'); спуск принадлежит semantic::validate::base_type"
        );
    }
}

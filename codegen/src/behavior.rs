/// Вспомогательные функции для работы со свойствами `behavior` и `end`.
use but_grammar::ast::{Expression, ModelDefinition, ModelPart, Property, StatePart};

/// Тип компоновки конечных автоматов (значение свойства `behavior`).
///
/// Примеры в языке BuT:
/// ```text
/// behavior -> sequential(ModelA, ModelB, ModelC);
/// behavior -> parallel(ModelA, ModelB);
/// behavior -> choice(ModelA, ModelB);
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum BehaviorKind {
    /// Последовательное выполнение: M1 завершается → запускается M2 и т.д.
    Sequential(Vec<String>),
    /// Параллельное выполнение: все модели работают одновременно до завершения всех.
    Parallel(Vec<String>),
    /// Выбор одной из моделей (берётся первая готовая к выполнению).
    Choice(Vec<String>),
}

impl BehaviorKind {
    /// Возвращает список имён моделей, участвующих в компоновке.
    pub fn models(&self) -> &[String] {
        match self {
            BehaviorKind::Sequential(m) | BehaviorKind::Parallel(m) | BehaviorKind::Choice(m) => m,
        }
    }
}

/// Найти и разобрать свойство `behavior` в модели.
///
/// Возвращает `None`, если свойство отсутствует или имеет неизвестный тип компоновки.
pub fn find_behavior(model: &ModelDefinition) -> Option<BehaviorKind> {
    for part in &model.parts {
        if let ModelPart::PropertyDefinition(pd) = part {
            if pd.name.as_ref().map(|n| n.name.as_str()) == Some("behavior") {
                if let Property::Expression(expr) = &pd.value {
                    if let Expression::FunctionCall(_, func_id, args) = expr {
                        let names: Vec<String> = args
                            .iter()
                            .filter_map(|a| {
                                if let Expression::Variable(id) = a {
                                    Some(id.name.clone())
                                } else {
                                    None
                                }
                            })
                            .collect();
                        return match func_id.name.as_str() {
                            "sequential" => Some(BehaviorKind::Sequential(names)),
                            "parallel" => Some(BehaviorKind::Parallel(names)),
                            "choice" => Some(BehaviorKind::Choice(names)),
                            _ => None,
                        };
                    }
                }
            }
        }
    }
    None
}

/// Найти свойство `end` в модели.
///
/// Возвращает ссылку на `Property`, если свойство присутствует.
pub fn find_end_property(model: &ModelDefinition) -> Option<&Property> {
    for part in &model.parts {
        if let ModelPart::PropertyDefinition(pd) = part {
            if pd.name.as_ref().map(|n| n.name.as_str()) == Some("end") {
                return Some(&pd.value);
            }
        }
    }
    None
}

/// Найти терминальные состояния модели — состояния без исходящих переходов (`ref`).
///
/// Терминальное состояние означает завершение работы автомата.
pub fn find_terminal_states(model: &ModelDefinition) -> Vec<String> {
    model
        .parts
        .iter()
        .filter_map(|p| {
            if let ModelPart::StateDefinition(sd) = p {
                let name = sd.name.as_ref()?.name.clone();
                let has_refs =
                    sd.parts.iter().any(|sp| matches!(sp, StatePart::Reference(..)));
                if !has_refs { Some(name) } else { None }
            } else {
                None
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> but_grammar::ast::SourceUnit {
        but_grammar::parse(src, 0).expect("parse ok").0
    }

    #[test]
    fn find_behavior_sequential() {
        let src = parse(r#"
model Comp {
    behavior -> sequential(ModelA, ModelB);
    start -> Comp;
}
model Comp { state Comp{} start -> Comp; }
"#);
        // First model has behavior
        let unit = &src.0;
        if let but_grammar::ast::SourceUnitPart::ModelDefinition(md) = &unit[0] {
            let bk = find_behavior(md);
            assert!(matches!(bk, Some(BehaviorKind::Sequential(_))));
            if let Some(BehaviorKind::Sequential(names)) = bk {
                assert_eq!(names, vec!["ModelA", "ModelB"]);
            }
        }
    }

    #[test]
    fn find_behavior_parallel() {
        let src = parse(r#"
model Comp { behavior -> parallel(M1, M2, M3); start -> Comp; }
"#);
        if let but_grammar::ast::SourceUnitPart::ModelDefinition(md) = &src.0[0] {
            let bk = find_behavior(md);
            assert!(matches!(bk, Some(BehaviorKind::Parallel(_))));
            if let Some(BehaviorKind::Parallel(names)) = bk {
                assert_eq!(names.len(), 3);
            }
        }
    }

    #[test]
    fn find_behavior_choice() {
        let src = parse(r#"
model Comp { behavior -> choice(A, B); start -> Comp; }
"#);
        if let but_grammar::ast::SourceUnitPart::ModelDefinition(md) = &src.0[0] {
            assert!(matches!(find_behavior(md), Some(BehaviorKind::Choice(_))));
        }
    }

    #[test]
    fn find_behavior_none() {
        let src = parse(r#"
model Plain { state S {} start -> S; }
"#);
        if let but_grammar::ast::SourceUnitPart::ModelDefinition(md) = &src.0[0] {
            assert!(find_behavior(md).is_none());
        }
    }

    #[test]
    fn find_end_property_present() {
        let src = parse(r#"
model M { state A {} start -> A; end -> { done = 1; } }
"#);
        if let but_grammar::ast::SourceUnitPart::ModelDefinition(md) = &src.0[0] {
            assert!(find_end_property(md).is_some());
        }
    }

    #[test]
    fn find_end_property_absent() {
        let src = parse(r#"
model M { state A {} start -> A; }
"#);
        if let but_grammar::ast::SourceUnitPart::ModelDefinition(md) = &src.0[0] {
            assert!(find_end_property(md).is_none());
        }
    }

    #[test]
    fn find_terminal_states_basic() {
        let src = parse(r#"
model M {
    state Working { ref Done: done; }
    state Done { }
    start -> Working;
}
"#);
        if let but_grammar::ast::SourceUnitPart::ModelDefinition(md) = &src.0[0] {
            let terms = find_terminal_states(md);
            assert_eq!(terms, vec!["Done"]);
        }
    }

    #[test]
    fn find_terminal_states_none_when_all_have_refs() {
        let src = parse(r#"
model M {
    state A { ref B: x; }
    state B { ref A: y; }
    start -> A;
}
"#);
        if let but_grammar::ast::SourceUnitPart::ModelDefinition(md) = &src.0[0] {
            assert!(find_terminal_states(md).is_empty());
        }
    }

    #[test]
    fn behavior_kind_models() {
        let seq = BehaviorKind::Sequential(vec!["A".into(), "B".into()]);
        assert_eq!(seq.models(), &["A", "B"]);
        let par = BehaviorKind::Parallel(vec!["X".into()]);
        assert_eq!(par.models(), &["X"]);
    }
}

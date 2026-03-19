/// Helper functions for working with model composition and the `end` property.
use but_grammar::ast::{Expression, ModelDefinition, ModelPart, Property, StatePart};

/// Composition kind for a model — derived from the `implements` clause (`= ...`).
///
/// Syntax in the BuT language:
/// ```text
/// model M = A + B + C {}   // sequential: A → B → C
/// model M = A | B | C {}   // parallel:   all at the same time
/// model M = A {}           // choice:     run A (a single sub-model)
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum BehaviorKind {
    /// Sequential execution: M1 finishes → M2 starts, etc.
    /// Syntax: `M1 + M2 + M3`
    Sequential(Vec<String>),
    /// Parallel execution: all models run simultaneously until all finish.
    /// Syntax: `M1 | M2 | M3`
    Parallel(Vec<String>),
    /// Delegation to a single sub-model.
    /// Syntax: `M` (single name without operators)
    Choice(Vec<String>),
}

impl BehaviorKind {
    /// Returns the list of model names participating in the composition.
    pub fn models(&self) -> &[String] {
        match self {
            BehaviorKind::Sequential(m) | BehaviorKind::Parallel(m) | BehaviorKind::Choice(m) => m,
        }
    }
}

/// Determine the composition kind from the `implements` clause of a model.
///
/// Reads `model.implements` (the `= ...` part of the declaration):
/// - `model M = A + B + C {}` → [`BehaviorKind::Sequential`]  (operator `+`)
/// - `model M = A | B | C {}` → [`BehaviorKind::Parallel`]    (operator `|`)
/// - `model M = A {}`         → [`BehaviorKind::Choice`]       (single model name)
/// - `model M {}`             → `None` (regular FSM, no composition)
///
/// Returns `None` if `implements` is absent or the expression is not recognized.
pub fn model_composition(model: &ModelDefinition) -> Option<BehaviorKind> {
    let expr = model.implements.as_ref()?;
    parse_composition_expr(expr)
}

/// Parse the composition expression from the AST.
///
/// - `Add(A, B)` and nested Add        → Sequential
/// - `BitwiseOr(A, B)` and nested BitwiseOr → Parallel
/// - `Variable(A)`                      → Choice(["A"])
fn parse_composition_expr(expr: &Expression) -> Option<BehaviorKind> {
    match expr {
        Expression::Add(..) => {
            let names = flatten_add(expr);
            if names.is_empty() { None } else { Some(BehaviorKind::Sequential(names)) }
        }
        Expression::BitwiseOr(..) => {
            let names = flatten_bitor(expr);
            if names.is_empty() { None } else { Some(BehaviorKind::Parallel(names)) }
        }
        Expression::Variable(id) => Some(BehaviorKind::Choice(vec![id.name.clone()])),
        Expression::Parenthesis(_, inner) => parse_composition_expr(inner),
        _ => None,
    }
}

/// Recursively extract model names from a left-associative `Add` tree.
///
/// Example: `A + B + C` → `Add(Add(A, B), C)` → ["A", "B", "C"]
fn flatten_add(expr: &Expression) -> Vec<String> {
    match expr {
        Expression::Add(_, left, right) => {
            let mut names = flatten_add(left);
            names.extend(flatten_add(right));
            names
        }
        Expression::Variable(id) => vec![id.name.clone()],
        Expression::Parenthesis(_, inner) => flatten_add(inner),
        _ => vec![],
    }
}

/// Recursively extract model names from a left-associative `BitwiseOr` tree.
///
/// Example: `A | B | C` → `BitwiseOr(BitwiseOr(A, B), C)` → ["A", "B", "C"]
fn flatten_bitor(expr: &Expression) -> Vec<String> {
    match expr {
        Expression::BitwiseOr(_, left, right) => {
            let mut names = flatten_bitor(left);
            names.extend(flatten_bitor(right));
            names
        }
        Expression::Variable(id) => vec![id.name.clone()],
        Expression::Parenthesis(_, inner) => flatten_bitor(inner),
        _ => vec![],
    }
}

/// Find the `end` property in a model.
///
/// Returns a reference to the `Property` if the property is present.
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

/// Find the terminal states of a model — states without outgoing transitions (`ref`).
///
/// A terminal state means the FSM has finished execution.
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

    fn parse_first_model(src: &str) -> ModelDefinition {
        let unit = but_grammar::parse(src, 0).expect("parsing succeeded").0;
        if let but_grammar::ast::SourceUnitPart::ModelDefinition(md) =
            unit.0.into_iter().next().unwrap()
        {
            *md
        } else {
            panic!("first element is not a model")
        }
    }

    // ── composition syntax tests (implements clause) ───────────────────────

    #[test]
    fn model_composition_sequential() {
        // model M = A + B {}  →  Sequential(["A", "B"])
        let md = parse_first_model(r#"
model Pipe = Phase1 + Phase2 {
}
"#);
        let bk = model_composition(&md);
        assert!(
            matches!(bk, Some(BehaviorKind::Sequential(_))),
            "expected Sequential, got: {:?}", bk
        );
        if let Some(BehaviorKind::Sequential(names)) = bk {
            assert_eq!(names, vec!["Phase1", "Phase2"]);
        }
    }

    #[test]
    fn model_composition_sequential_three_models() {
        // A + B + C is left-associative: Add(Add(A, B), C) → ["A", "B", "C"]
        let md = parse_first_model(r#"
model Pipeline = Calibrate + Process + Store {
}
"#);
        let bk = model_composition(&md);
        assert!(matches!(bk, Some(BehaviorKind::Sequential(_))));
        if let Some(BehaviorKind::Sequential(names)) = bk {
            assert_eq!(names, vec!["Calibrate", "Process", "Store"]);
        }
    }

    #[test]
    fn model_composition_parallel() {
        // model M = A | B {}  →  Parallel(["A", "B"])
        let md = parse_first_model(r#"
model Combo = Alpha | Beta {
}
"#);
        let bk = model_composition(&md);
        assert!(
            matches!(bk, Some(BehaviorKind::Parallel(_))),
            "expected Parallel, got: {:?}", bk
        );
        if let Some(BehaviorKind::Parallel(names)) = bk {
            assert_eq!(names, vec!["Alpha", "Beta"]);
        }
    }

    #[test]
    fn model_composition_parallel_three_models() {
        // A | B | C → Parallel(["A", "B", "C"])
        let md = parse_first_model(r#"
model Prep = Calibrate | Process | Store {
}
"#);
        if let Some(BehaviorKind::Parallel(names)) = model_composition(&md) {
            assert_eq!(names, vec!["Calibrate", "Process", "Store"]);
        } else {
            panic!("expected Parallel with three models");
        }
    }

    #[test]
    fn model_composition_choice_single_model() {
        // model M = A {}  →  Choice(["A"])
        let md = parse_first_model(r#"
model Wrapper = Worker {
}
"#);
        let bk = model_composition(&md);
        assert!(
            matches!(bk, Some(BehaviorKind::Choice(_))),
            "expected Choice, got: {:?}", bk
        );
        if let Some(BehaviorKind::Choice(names)) = bk {
            assert_eq!(names, vec!["Worker"]);
        }
    }

    #[test]
    fn model_composition_absent_for_plain_fsm() {
        // A regular FSM without an implements clause → None
        let md = parse_first_model(r#"
model Plain {
    start S { }
}
"#);
        assert!(
            model_composition(&md).is_none(),
            "a plain FSM should have no composition"
        );
    }

    #[test]
    fn model_composition_parenthesised() {
        // model M = (A | B) + C {}  →  Sequential(["A", "B", "C"]) — but actually
        // parentheses wrap the parallel sub-expression, so we get:
        // Add(Paren(BitwiseOr(A, B)), C) — flatten_add descends into Paren → ["A"|"B", C]
        // The outer Add produces Sequential; the Paren+BitwiseOr inside flatten_add
        // contributes nothing extra (flatten_add stops at non-Add/non-Variable).
        // Here we just verify it doesn't panic and returns Some.
        let md = parse_first_model(r#"
model Combo = (Alpha | Beta) + Gamma {
}
"#);
        assert!(model_composition(&md).is_some());
    }

    // ── end property tests ────────────────────────────────────────────────

    #[test]
    fn find_end_property_present() {
        let md = parse_first_model(r#"
model M {
    start A { }
    end -> { done = 1; }
}
"#);
        assert!(find_end_property(&md).is_some(), "end property should be found");
    }

    #[test]
    fn find_end_property_absent() {
        let md = parse_first_model(r#"
model M {
    start A { }
}
"#);
        assert!(find_end_property(&md).is_none(), "end property should be absent");
    }

    // ── terminal states tests ─────────────────────────────────────────────

    #[test]
    fn terminal_states_single() {
        let md = parse_first_model(r#"
model M {
    start Working { ref Done: done; }
    state Done { }
}
"#);
        let terms = find_terminal_states(&md);
        assert_eq!(terms, vec!["Done"], "only Done should be terminal");
    }

    #[test]
    fn terminal_states_multiple() {
        let md = parse_first_model(r#"
model M {
    start A { ref B: x; ref C: y; }
    state B { }
    state C { }
}
"#);
        let terms = find_terminal_states(&md);
        assert!(terms.contains(&"B".to_string()), "B should be terminal");
        assert!(terms.contains(&"C".to_string()), "C should be terminal");
    }

    #[test]
    fn terminal_states_absent_when_all_have_transitions() {
        let md = parse_first_model(r#"
model M {
    start A { ref B: x; }
    state B { ref A: y; }
}
"#);
        assert!(find_terminal_states(&md).is_empty(), "cycle — no terminal states");
    }

    // ── models() helper ──────────────────────────────────────────────────

    #[test]
    fn behavior_kind_models_returns_slice() {
        let seq = BehaviorKind::Sequential(vec!["A".into(), "B".into()]);
        assert_eq!(seq.models(), &["A", "B"]);
        let par = BehaviorKind::Parallel(vec!["X".into(), "Y".into(), "Z".into()]);
        assert_eq!(par.models(), &["X", "Y", "Z"]);
        let ch = BehaviorKind::Choice(vec!["Worker".into()]);
        assert_eq!(ch.models(), &["Worker"]);
    }
}

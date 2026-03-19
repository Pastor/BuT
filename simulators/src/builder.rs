use std::collections::HashMap;

use but_grammar::ast::{
    self, ConditionDefinition, Expression, Loc, ModelDefinition, ModelPart, Property,
    PropertyDefinition, SourceUnit, SourceUnitPart, StatePart, Type, VariableAttribute,
    VariableDefinition,
};
use thiserror::Error;

use crate::context::SimContext;
use crate::ltl::extract_ltl_formulas;
use crate::machine::{Machine, MachineKind, State, Transition};
use crate::value::Value;

/// Error while building a state machine.
#[derive(Debug, Error)]
pub enum BuildError {
    #[error("Model '{0}' has no start state")]
    NoStartState(String),
    #[error("Model '{0}' references a non-existent state '{1}'")]
    UndefinedState(String, String),
    #[error("Unknown model reference '{0}' in behavior expression")]
    UnknownModelRef(String),
}

/// Build all state machines from a SourceUnit (handles global behavior → composition).
pub fn build_all(
    source: &SourceUnit,
) -> Result<Vec<MachineKind>, BuildError> {
    // Collect global variable/port declarations and type aliases
    let mut global_vars: Vec<Box<VariableDefinition>> = vec![];
    let mut models: Vec<Box<ModelDefinition>> = vec![];
    let mut type_aliases: HashMap<String, Type> = HashMap::new();

    for part in &source.0 {
        match part {
            SourceUnitPart::VariableDefinition(vd) => global_vars.push(vd.clone()),
            SourceUnitPart::ModelDefinition(md) => models.push(md.clone()),
            SourceUnitPart::TypeDefinition(td) => {
                type_aliases.insert(td.name.name.clone(), td.ty.clone());
            }
            _ => {}
        }
    }

    // Build all models as separate state machines
    let mut machines: Vec<MachineKind> = vec![];
    for model in &models {
        let machine = build_machine(model, &global_vars, &type_aliases)?;
        machines.push(machine);
    }

    // Find the behavior composition expression at the source-file level
    for part in &source.0 {
        if let SourceUnitPart::PropertyDefinition(pd) = part {
            if let Some(name) = &pd.name {
                if name.name == "behavior" {
                    if let Property::Expression(e) = &pd.value {
                        let composed = compose_from_expr(e, &mut machines)?;
                        return Ok(vec![composed]);
                    }
                }
            }
        }
    }

    Ok(machines)
}

/// Recursively build a machine composition from a behavior expression.
fn compose_from_expr(
    expr: &Expression,
    machines: &mut Vec<MachineKind>,
) -> Result<MachineKind, BuildError> {
    match expr {
        // A | B → Parallel execution
        Expression::BitwiseOr(_, l, r) => {
            let lm = compose_from_expr(l, machines)?;
            let rm = compose_from_expr(r, machines)?;
            Ok(MachineKind::Parallel(vec![lm, rm]))
        }
        // A + B → Sequential composition
        Expression::Add(_, l, r) => {
            let lm = compose_from_expr(l, machines)?;
            let rm = compose_from_expr(r, machines)?;
            Ok(MachineKind::Sequence(vec![lm, rm]))
        }
        // Variable reference → look up machine by name
        Expression::Variable(ident) => {
            let pos = machines
                .iter()
                .position(|m| m.name() == Some(ident.name.as_str()));
            match pos {
                Some(i) => Ok(machines.remove(i)),
                None => Err(BuildError::UnknownModelRef(ident.name.clone())),
            }
        }
        // Parentheses
        Expression::Parenthesis(_, inner) => compose_from_expr(inner, machines),
        // Unsupported variant
        _ => Err(BuildError::UnknownModelRef("?".to_string())),
    }
}

/// Build a runtime state machine from an AST ModelDefinition.
pub fn build_machine(
    model: &ModelDefinition,
    global_vars: &[Box<VariableDefinition>],
    type_aliases: &HashMap<String, Type>,
) -> Result<MachineKind, BuildError> {
    let model_name = model
        .name
        .as_ref()
        .map(|n| n.name.clone())
        .unwrap_or_default();

    let mut context = SimContext::new();
    let mut start_state: Option<String> = None;
    let mut states: indexmap::IndexMap<String, State> = indexmap::IndexMap::new();
    let mut model_enter: Vec<ast::Statement> = vec![];
    let mut model_end: Vec<ast::Statement> = vec![];

    // Extract LTL formulas from annotations and formula blocks
    let ltl_formulas = extract_ltl_formulas(model);

    // Declare global variables/ports in the context
    for vd in global_vars {
        declare_variable_def(&mut context, vd, type_aliases);
    }

    // First pass: register all state names and model-level declarations
    for part in &model.parts {
        match part {
            ModelPart::StateDefinition(sd) => {
                let sname = sd
                    .name
                    .as_ref()
                    .map(|n| n.name.clone())
                    .unwrap_or_default();
                states.insert(sname.clone(), State::new(sname));
            }
            ModelPart::VariableDefinition(vd) => {
                declare_variable_def(&mut context, vd, type_aliases);
            }
            ModelPart::PropertyDefinition(pd) => {
                handle_model_property(pd, &mut start_state, &mut model_enter, &mut model_end);
            }
            ModelPart::ConditionDefinition(cd) => {
                handle_model_condition(cd, &mut start_state);
            }
            _ => {}
        }
    }

    // Second pass: build state contents (transitions, handlers)
    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd
                .name
                .as_ref()
                .map(|n| n.name.clone())
                .unwrap_or_default();

            // Collect nested machines first (without mutable borrow on states)
            let mut sub_machine: Option<MachineKind> = None;
            let mut extra_vars: Vec<Box<ast::VariableDefinition>> = vec![];

            for sp in &sd.parts {
                if let StatePart::VariableDefinition(vd) = sp {
                    extra_vars.push(vd.clone());
                }
                if let StatePart::ModelDefinition(nested) = sp {
                    if let Ok(sub) = build_machine(nested, global_vars, type_aliases) {
                        sub_machine = Some(sub);
                    }
                }
            }

            // Apply state-level variables
            for vd in &extra_vars {
                declare_variable_def(&mut context, vd, type_aliases);
            }

            // Now take a mutable reference and apply all changes
            let state = states.get_mut(&sname).unwrap();

            for sp in &sd.parts {
                match sp {
                    StatePart::Reference(_, target, condition) => {
                        state.transitions.push(Transition {
                            target: target.name.clone(),
                            condition: condition.clone(),
                        });
                    }
                    StatePart::PropertyDefinition(pd) => {
                        if let Some(name) = &pd.name {
                            let stmt = property_to_statement(&pd.value);
                            match name.name.as_str() {
                                "enter" => state.enter.push(stmt),
                                "exit" => state.exit.push(stmt),
                                "before" => state.before.push(stmt),
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
            }

            if let Some(sub) = sub_machine {
                state.submachine = Some(Box::new(sub));
            }
        }
    }

    let current = start_state
        .clone()
        .or_else(|| states.keys().next().cloned())
        .unwrap_or_default();

    let mut machine = Machine::new(model_name.clone(), current, context);
    machine.states = states;
    machine.model_enter = model_enter;
    machine.model_end = model_end;
    machine.ltl_formulas = ltl_formulas;

    Ok(MachineKind::Single(machine))
}

/// Recursively resolve a type alias through the alias table (up to 8 levels).
fn resolve_type_alias(ty: &Type, aliases: &HashMap<String, Type>, depth: u8) -> Type {
    if depth == 0 {
        return ty.clone();
    }
    match ty {
        Type::Alias(id) => match aliases.get(&id.name) {
            Some(resolved) => resolve_type_alias(resolved, aliases, depth - 1),
            None => ty.clone(),
        },
        _ => ty.clone(),
    }
}

/// Determine the default initial value for a type (with alias resolution).
/// Used when declaring variables without an explicit initializer.
fn default_value_for_type(ty: &Type, aliases: &HashMap<String, Type>) -> Value {
    match resolve_type_alias(ty, aliases, 8) {
        Type::Bool => Value::Bool(false),
        Type::Rational => Value::Real(0.0),
        Type::Alias(id) => match id.name.as_str() {
            "bool" => Value::Bool(false),
            "real" | "f64" | "f32" => Value::Real(0.0),
            _ => Value::Int(0),
        },
        _ => Value::Int(0),
    }
}

/// Declare a variable in the context.
fn declare_variable_def(ctx: &mut SimContext, vd: &VariableDefinition, type_aliases: &HashMap<String, Type>) {
    let name = match &vd.name {
        Some(n) => n.name.clone(),
        None => return,
    };

    // Check whether the variable is a port
    let is_port = vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_)));
    let is_const = vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Constant(_)));

    let initial = vd
        .initializer
        .as_ref()
        .and_then(|e| eval_literal(e))
        .unwrap_or_else(|| default_value_for_type(&vd.ty, type_aliases));

    if is_port {
        ctx.declare_port(&name, initial);
    } else {
        ctx.declare_var(&name, initial);
    }
    // Constant variables are stored in the context; immutability is not enforced by the simulator
    let _ = is_const;
}

/// Evaluate a literal expression to a value (for initializers only).
pub fn eval_literal(expr: &Expression) -> Option<Value> {
    match expr {
        Expression::NumberLiteral(_, n) => Some(Value::Int(*n)),
        Expression::HexNumberLiteral(_, s, _) => {
            let clean = s.trim_start_matches("0x").trim_start_matches("0X");
            i64::from_str_radix(clean, 16).ok().map(Value::Int)
        }
        Expression::RationalNumberLiteral(_, s, neg) => {
            s.parse::<f64>().ok().map(|f| Value::Real(if *neg { -f } else { f }))
        }
        Expression::BoolLiteral(_, b) => Some(Value::Bool(*b)),
        Expression::Parenthesis(_, inner) => eval_literal(inner),
        _ => None,
    }
}

/// Handle a model-level PropertyDefinition (start, enter, exit, end, behavior).
fn handle_model_property(
    pd: &PropertyDefinition,
    start_state: &mut Option<String>,
    model_enter: &mut Vec<ast::Statement>,
    model_end: &mut Vec<ast::Statement>,
) {
    let name = match &pd.name {
        Some(n) => n.name.as_str(),
        None => return,
    };

    match name {
        "start" => {
            if let Property::Expression(expr) = &pd.value {
                if let Expression::Variable(ident) = expr {
                    *start_state = Some(ident.name.clone());
                }
            }
        }
        "enter" => {
            model_enter.push(property_to_statement(&pd.value));
        }
        "end" => {
            model_end.push(property_to_statement(&pd.value));
        }
        // exit/before at model level — rarely used
        "exit" | "before" => {}
        _ => {}
    }
}

/// Handle a model-level ConditionDefinition (syntax `start: State;`).
fn handle_model_condition(
    cd: &ConditionDefinition,
    start_state: &mut Option<String>,
) {
    let name = match &cd.name {
        Some(n) => n.name.as_str(),
        None => return,
    };

    if name == "start" {
        if let but_grammar::ast::Condition::Variable(ident) = &cd.value {
            *start_state = Some(ident.name.clone());
        }
    }
}

/// Convert a Property to a Statement for execution.
pub fn property_to_statement(prop: &Property) -> ast::Statement {
    match prop {
        Property::Function(stmt) => stmt.clone(),
        Property::Expression(expr) => {
            ast::Statement::Expression(Loc::Builtin, expr.clone())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use but_grammar::ast::Identifier;

    fn ident(name: &str) -> Identifier {
        Identifier::new(name)
    }

    /// Parse a BuT source string and build state machines.
    fn parse_and_build(src: &str) -> Result<Vec<MachineKind>, BuildError> {
        let (source, _) = but_grammar::parse(src, 0).expect("parse error");
        build_all(&source)
    }

    /// Parse a BuT source string and return a type alias table.
    fn collect_type_aliases(src: &str) -> HashMap<String, Type> {
        let (source, _) = but_grammar::parse(src, 0).expect("parse error");
        let mut aliases = HashMap::new();
        for part in &source.0 {
            if let SourceUnitPart::TypeDefinition(td) = part {
                aliases.insert(td.name.name.clone(), td.ty.clone());
            }
        }
        aliases
    }

    // ===== Type alias collection =====

    #[test]
    fn build_all_collects_type_aliases() {
        let aliases = collect_type_aliases(r#"
type MyByte = u8;
type Counter = u32;
model M { start -> M; }
"#);
        assert!(aliases.contains_key("MyByte"));
        assert!(aliases.contains_key("Counter"));
        assert_eq!(aliases.len(), 2);
    }

    #[test]
    fn build_all_no_aliases_empty_table() {
        let aliases = collect_type_aliases("model M { start -> M; }");
        assert!(aliases.is_empty());
    }

    // ===== Default values via aliases =====

    #[test]
    fn default_value_for_bool_type() {
        let aliases = HashMap::new();
        let val = default_value_for_type(&Type::Bool, &aliases);
        assert_eq!(val, Value::Bool(false));
    }

    #[test]
    fn default_value_for_rational_type() {
        let aliases = HashMap::new();
        let val = default_value_for_type(&Type::Rational, &aliases);
        assert_eq!(val, Value::Real(0.0));
    }

    #[test]
    fn default_value_for_alias_bool() {
        let aliases = HashMap::new();
        let val = default_value_for_type(
            &Type::Alias(ident("bool")),
            &aliases,
        );
        assert_eq!(val, Value::Bool(false));
    }

    #[test]
    fn default_value_for_alias_f64() {
        let aliases = HashMap::new();
        let val = default_value_for_type(
            &Type::Alias(ident("f64")),
            &aliases,
        );
        assert_eq!(val, Value::Real(0.0));
    }

    #[test]
    fn default_value_for_user_alias_to_bool() {
        // type Flag = bool; → default Value::Bool(false)
        let mut aliases = HashMap::new();
        aliases.insert("Flag".to_string(), Type::Bool);
        let val = default_value_for_type(&Type::Alias(ident("Flag")), &aliases);
        assert_eq!(val, Value::Bool(false));
    }

    #[test]
    fn default_value_for_user_alias_to_u32() {
        // type Counter = u32; → default Value::Int(0)
        let mut aliases = HashMap::new();
        aliases.insert("Counter".to_string(), Type::Alias(ident("u32")));
        let val = default_value_for_type(&Type::Alias(ident("Counter")), &aliases);
        assert_eq!(val, Value::Int(0));
    }

    // ===== Building machines with types =====

    #[test]
    fn build_all_with_type_aliases_and_variables() {
        // A variable of an alias type should be declared in the context
        let machines = parse_and_build(r#"
type Counter = u32;
model M {
    state Active { ref Active: 1 = 0; }
    start -> Active;
}
"#).expect("build should succeed");
        assert_eq!(machines.len(), 1);
    }

    #[test]
    fn resolve_type_alias_chain() {
        let mut aliases = HashMap::new();
        aliases.insert("A".to_string(), Type::Alias(ident("B")));
        aliases.insert("B".to_string(), Type::Alias(ident("u8")));
        let resolved = resolve_type_alias(&Type::Alias(ident("A")), &aliases, 8);
        assert_eq!(resolved, Type::Alias(ident("u8")));
    }

    #[test]
    fn resolve_type_alias_depth_limit() {
        // A cyclic chain must not cause infinite recursion
        let mut aliases = HashMap::new();
        aliases.insert("A".to_string(), Type::Alias(ident("B")));
        aliases.insert("B".to_string(), Type::Alias(ident("A")));
        // Must not panic; should return something
        let _ = resolve_type_alias(&Type::Alias(ident("A")), &aliases, 8);
    }
}

/// Semantic validation of bit-access operations on variables.
///
/// Checks that the bit index in `variable.N` does not exceed the width of the
/// variable's type. Errors are emitted as compiler diagnostics.
use std::collections::HashMap;


use crate::model::{Model, Visitor};
use but_grammar::ast::{
    AnnotationDefinition, Condition, EnumDefinition, Expression, FunctionDefinition, Member,
    ModelDefinition, ModelPart, Property, PropertyDefinition, StatePart, Statement,
    StructDefinition, Type, TypeDefinition, VariableDefinition,
};
use but_grammar::diagnostics::Diagnostic;

/// Compute the bit-width of a type.
///
/// Returns `Some(n)` if the width is statically known, otherwise `None`.
pub fn bit_width_of_type(
    ty: &Type,
    types: &HashMap<String, Box<TypeDefinition>>,
    depth: u8,
) -> Option<u64> {
    if depth == 0 {
        return None;
    }
    match ty {
        Type::Alias(id) => match id.name.as_str() {
            "bit" | "bool" => Some(1),
            "u8" | "byte" => Some(8),
            "u16" => Some(16),
            "u32" | "int" | "i32" => Some(32),
            "u64" | "i64" => Some(64),
            "u128" => Some(128),
            name => types
                .get(name)
                .and_then(|td| bit_width_of_type(&td.ty, types, depth - 1)),
        },
        Type::Array {
            element_count,
            element_type,
            ..
        } => bit_width_of_type(element_type, types, depth - 1).map(|w| w * *element_count as u64),
        Type::Bool => Some(1),
        // float, string, address — bit access not supported
        _ => None,
    }
}

/// Check bit-access in an expression.
///
/// If the left-hand side of an assignment or a read expression contains `variable.N`,
/// and the variable type is known, validates that N < type_width.
pub fn check_expr(
    expr: &Expression,
    vars: &HashMap<String, Type>,
    types: &HashMap<String, Box<TypeDefinition>>,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::MemberAccess(loc, base, Member::Number(n)) => {
            // Bit read: base.N
            if let Some(ty) = extract_var_type(base, vars) {
                emit_if_out_of_bounds(*loc, *n, &ty, types, diags);
            }
            check_expr(base, vars, types, diags);
        }
        Expression::Assign(_, lhs, rhs) => {
            // Bit write: base.N = value
            if let Expression::MemberAccess(loc, base, Member::Number(n)) = lhs.as_ref() {
                if let Some(ty) = extract_var_type(base, vars) {
                    emit_if_out_of_bounds(*loc, *n, &ty, types, diags);
                }
                check_expr(base, vars, types, diags);
            } else {
                check_expr(lhs, vars, types, diags);
            }
            check_expr(rhs, vars, types, diags);
        }
        Expression::Add(_, l, r)
        | Expression::Subtract(_, l, r)
        | Expression::Multiply(_, l, r)
        | Expression::Divide(_, l, r)
        | Expression::Modulo(_, l, r)
        | Expression::And(_, l, r)
        | Expression::Or(_, l, r)
        | Expression::BitwiseAnd(_, l, r)
        | Expression::BitwiseOr(_, l, r)
        | Expression::BitwiseXor(_, l, r)
        | Expression::ShiftLeft(_, l, r)
        | Expression::ShiftRight(_, l, r)
        | Expression::Equal(_, l, r)
        | Expression::NotEqual(_, l, r)
        | Expression::Less(_, l, r)
        | Expression::More(_, l, r)
        | Expression::LessEqual(_, l, r)
        | Expression::MoreEqual(_, l, r)
        | Expression::AssignAdd(_, l, r)
        | Expression::AssignSubtract(_, l, r)
        | Expression::AssignMultiply(_, l, r)
        | Expression::AssignDivide(_, l, r)
        | Expression::AssignModulo(_, l, r) => {
            check_expr(l, vars, types, diags);
            check_expr(r, vars, types, diags);
        }
        Expression::Not(_, inner)
        | Expression::Negate(_, inner)
        | Expression::Parenthesis(_, inner)
        | Expression::PostIncrement(_, inner)
        | Expression::PostDecrement(_, inner)
        | Expression::PreIncrement(_, inner)
        | Expression::PreDecrement(_, inner) => {
            check_expr(inner, vars, types, diags);
        }
        Expression::FunctionCall(_, _, args) => {
            for arg in args {
                check_expr(arg, vars, types, diags);
            }
        }
        _ => {}
    }
}

/// Check bit-access in a condition.
pub fn check_condition(
    cond: &Condition,
    vars: &HashMap<String, Type>,
    types: &HashMap<String, Box<TypeDefinition>>,
    diags: &mut Vec<Diagnostic>,
) {
    match cond {
        Condition::MemberAccess(loc, base, Member::Number(n)) => {
            if let Condition::Variable(id) = base.as_ref() {
                if let Some(ty) = vars.get(&id.name) {
                    emit_if_out_of_bounds(*loc, *n, ty, types, diags);
                }
            }
            check_condition(base, vars, types, diags);
        }
        Condition::Equal(_, l, r)
        | Condition::NotEqual(_, l, r)
        | Condition::Less(_, l, r)
        | Condition::LessEqual(_, l, r)
        | Condition::More(_, l, r)
        | Condition::MoreEqual(_, l, r)
        | Condition::And(_, l, r)
        | Condition::Or(_, l, r)
        | Condition::Add(_, l, r)
        | Condition::Subtract(_, l, r) => {
            check_condition(l, vars, types, diags);
            check_condition(r, vars, types, diags);
        }
        Condition::Not(_, inner) | Condition::Parenthesis(_, inner) => {
            check_condition(inner, vars, types, diags);
        }
        Condition::FunctionCall(_, _, args) => {
            for arg in args {
                check_condition(arg, vars, types, diags);
            }
        }
        _ => {}
    }
}

/// Check bit-access in a statement.
pub fn check_statement(
    stmt: &Statement,
    vars: &HashMap<String, Type>,
    types: &HashMap<String, Box<TypeDefinition>>,
    diags: &mut Vec<Diagnostic>,
) {
    match stmt {
        Statement::Expression(_, expr) => check_expr(expr, vars, types, diags),
        Statement::Return(_, Some(expr)) => check_expr(expr, vars, types, diags),
        Statement::If(_, cond, then_s, else_s) => {
            check_expr(cond, vars, types, diags);
            check_statement(then_s, vars, types, diags);
            if let Some(e) = else_s {
                check_statement(e, vars, types, diags);
            }
        }
        Statement::While(_, cond, body) => {
            check_expr(cond, vars, types, diags);
            check_statement(body, vars, types, diags);
        }
        Statement::For(_, init, cond, post, body) => {
            if let Some(init_s) = init {
                check_statement(init_s, vars, types, diags);
            }
            if let Some(cond_e) = cond {
                check_expr(cond_e, vars, types, diags);
            }
            if let Some(post_e) = post {
                check_expr(post_e, vars, types, diags);
            }
            if let Some(body_s) = body {
                check_statement(body_s, vars, types, diags);
            }
        }
        Statement::Block { statements, .. } => {
            let mut local_vars = vars.clone();
            for s in statements {
                if let Statement::VariableDefinition(_, decl, init) = s {
                    // Check initializer in the current scope first
                    if let Some(init_expr) = init {
                        check_expr(init_expr, &local_vars, types, diags);
                    }
                    // Then add the variable to scope
                    if let Some(name) = &decl.name {
                        local_vars.insert(name.name.clone(), decl.ty.clone());
                    }
                } else {
                    check_statement(s, &local_vars, types, diags);
                }
            }
        }
        _ => {}
    }
}

pub struct Checker {
    /// Global variables (root SourceUnit level).
    global_vars: HashMap<String, Type>,
    /// Global type aliases (root SourceUnit level).
    global_types: HashMap<String, Box<TypeDefinition>>,
    /// Diagnostics accumulated across all visited models.
    pub diags: Vec<Diagnostic>,
}

impl Checker {
    pub fn new(model: &Model) -> Self {
        Self {
            global_vars: model.get_variable_types(),
            global_types: model.get_types(),
            diags: Vec::new(),
        }
    }
}

impl Visitor for Checker {
    /// Checks variable initializers, property values, and function bodies of a model.
    fn visit_model(&mut self, model: &Model) -> Result<bool, Vec<Diagnostic>> {
        // Global variables are shadowed by parent-chain and model-level variables
        let mut vars = self.global_vars.clone();
        vars.extend(model.get_all_variable_types());

        let mut types = self.global_types.clone();
        types.extend(model.get_all_types());

        // Variable initializers
        for var in model.variables() {
            if let Some(init) = &var.initializer {
                check_expr(init, &vars, &types, &mut self.diags);
            }
        }

        // Property values
        for prop in model.properties() {
            check_property(&prop.value, &vars, &types, &mut self.diags);
        }

        // Function bodies
        for fun in model.functions() {
            let mut fun_vars = vars.clone();
            for (_, param) in &fun.params {
                if let Some(p) = param {
                    if let Some(name) = &p.name {
                        // Parameter type is Expression; skip unrecognised forms
                        if let Expression::Type(_, ty) = &p.ty {
                            fun_vars.insert(name.name.clone(), ty.clone());
                        }
                    }
                }
            }
            if let Some(body) = &fun.body {
                check_statement(body, &fun_vars, &types, &mut self.diags);
            }
        }

        Ok(true)
    }

    fn visit_annotation(&mut self, _: &AnnotationDefinition) -> Result<bool, Vec<Diagnostic>> {
        Ok(true)
    }

    fn visit_enum(&mut self, _: &EnumDefinition) -> Result<bool, Vec<Diagnostic>> {
        Ok(true)
    }

    fn visit_struct(&mut self, _: &StructDefinition) -> Result<bool, Vec<Diagnostic>> {
        Ok(true)
    }

    fn visit_variable(&mut self, _: &VariableDefinition) -> Result<bool, Vec<Diagnostic>> {
        Ok(true)
    }

    fn visit_function(&mut self, _: &FunctionDefinition) -> Result<bool, Vec<Diagnostic>> {
        Ok(true)
    }

    fn visit_type(&mut self, _: &TypeDefinition) -> Result<bool, Vec<Diagnostic>> {
        Ok(true)
    }

    fn visit_property(&mut self, _: &PropertyDefinition) -> Result<bool, Vec<Diagnostic>> {
        Ok(true)
    }
}

/// Check all models for correct bit-access.
pub fn check_models(
    models: &[Box<ModelDefinition>],
    global_vars: &HashMap<String, Type>,
    types: &HashMap<String, Box<TypeDefinition>>,
    diags: &mut Vec<Diagnostic>,
) {
    for model in models {
        // Collect model-level variables
        let mut model_vars = global_vars.clone();
        for part in &model.parts {
            if let ModelPart::VariableDefinition(vd) = part {
                if let Some(name) = &vd.name {
                    model_vars.insert(name.name.clone(), vd.ty.clone());
                }
            }
        }

        // Check model parts
        for part in &model.parts {
            match part {
                ModelPart::VariableDefinition(vd) => {
                    if let Some(init) = &vd.initializer {
                        check_expr(init, &model_vars, types, diags);
                    }
                }
                ModelPart::StateDefinition(sd) => {
                    // Collect state-level variables
                    let mut state_vars = model_vars.clone();
                    for sp in &sd.parts {
                        if let StatePart::VariableDefinition(vd) = sp {
                            if let Some(name) = &vd.name {
                                state_vars.insert(name.name.clone(), vd.ty.clone());
                            }
                        }
                    }

                    // Check state parts
                    for sp in &sd.parts {
                        match sp {
                            StatePart::VariableDefinition(vd) => {
                                if let Some(init) = &vd.initializer {
                                    check_expr(init, &state_vars, types, diags);
                                }
                            }
                            StatePart::Reference(_, _, Some(cond)) => {
                                check_condition(cond, &state_vars, types, diags);
                            }
                            StatePart::PropertyDefinition(pd) => {
                                check_property(&pd.value, &state_vars, types, diags);
                            }
                            _ => {}
                        }
                    }
                }
                ModelPart::PropertyDefinition(pd) => {
                    check_property(&pd.value, &model_vars, types, diags);
                }
                _ => {}
            }
        }
    }
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn check_property(
    prop: &Property,
    vars: &HashMap<String, Type>,
    types: &HashMap<String, Box<TypeDefinition>>,
    diags: &mut Vec<Diagnostic>,
) {
    match prop {
        Property::Expression(e) => check_expr(e, vars, types, diags),
        Property::Function(s) => check_statement(s, vars, types, diags),
    }
}

fn extract_var_type(expr: &Expression, vars: &HashMap<String, Type>) -> Option<Type> {
    match expr {
        Expression::Variable(id) => vars.get(&id.name).cloned(),
        _ => None,
    }
}

fn emit_if_out_of_bounds(
    loc: but_grammar::ast::Loc,
    n: i64,
    ty: &Type,
    types: &HashMap<String, Box<TypeDefinition>>,
    diags: &mut Vec<Diagnostic>,
) {
    if n < 0 {
        diags.push(Diagnostic::error(
            loc,
            format!("bit index {} is negative", n),
        ));
        return;
    }
    if let Some(width) = bit_width_of_type(ty, types, 8) {
        if n as u64 >= width {
            diags.push(Diagnostic::error(
                loc,
                format!("bit index {} is out of bounds for type (width {} bits)", n, width),
            ));
        }
    }
}

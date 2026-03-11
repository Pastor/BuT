/// Модуль семантической проверки корректности доступа к битам переменных.
///
/// Проверяет, что индекс бита в операции `переменная.N` не выходит за границы
/// ширины типа переменной. Ошибка фиксируется как диагностика компилятора.
use std::collections::HashMap;

use but_grammar::ast::{
    Condition, Expression, Member, ModelDefinition, ModelPart, Property,
    Statement, StatePart, Type, TypeDefinition,
};
use but_grammar::diagnostics::Diagnostic;

/// Вычислить ширину типа в битах.
///
/// Возвращает `Some(n)` если ширина известна статически, иначе `None`.
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
        Type::Array { element_count, element_type, .. } => {
            bit_width_of_type(element_type, types, depth - 1)
                .map(|w| w * *element_count as u64)
        }
        Type::Bool => Some(1),
        // float, string, address — не поддерживают побитовый доступ
        _ => None,
    }
}

/// Проверить доступ к биту в выражении.
///
/// Если левая часть присваивания или читаемое выражение содержит `переменная.N`,
/// и тип переменной известен, проверяется, что N < ширина_типа.
pub fn check_expr(
    expr: &Expression,
    vars: &HashMap<String, Type>,
    types: &HashMap<String, Box<TypeDefinition>>,
    diags: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::MemberAccess(loc, base, Member::Number(n)) => {
            // Чтение бита: base.N
            if let Some(ty) = extract_var_type(base, vars) {
                emit_if_out_of_bounds(*loc, *n, &ty, types, diags);
            }
            check_expr(base, vars, types, diags);
        }
        Expression::Assign(_, lhs, rhs) => {
            // Запись в бит: base.N = value
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

/// Проверить доступ к биту в условии (condition).
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

/// Проверить доступ к битам в операторе (Statement).
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
                    // Сначала проверяем инициализатор в текущем контексте
                    if let Some(init_expr) = init {
                        check_expr(init_expr, &local_vars, types, diags);
                    }
                    // Затем добавляем переменную в контекст
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

/// Проверить все модели на корректность доступа к битам.
pub fn check_models(
    models: &[Box<ModelDefinition>],
    global_vars: &HashMap<String, Type>,
    types: &HashMap<String, Box<TypeDefinition>>,
    diags: &mut Vec<Diagnostic>,
) {
    for model in models {
        // Собираем переменные уровня модели
        let mut model_vars = global_vars.clone();
        for part in &model.parts {
            if let ModelPart::VariableDefinition(vd) = part {
                if let Some(name) = &vd.name {
                    model_vars.insert(name.name.clone(), vd.ty.clone());
                }
            }
        }

        // Проверяем части модели
        for part in &model.parts {
            match part {
                ModelPart::VariableDefinition(vd) => {
                    if let Some(init) = &vd.initializer {
                        check_expr(init, &model_vars, types, diags);
                    }
                }
                ModelPart::StateDefinition(sd) => {
                    // Собираем переменные уровня состояния
                    let mut state_vars = model_vars.clone();
                    for sp in &sd.parts {
                        if let StatePart::VariableDefinition(vd) = sp {
                            if let Some(name) = &vd.name {
                                state_vars.insert(name.name.clone(), vd.ty.clone());
                            }
                        }
                    }

                    // Проверяем части состояния
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

// ── Вспомогательные функции ────────────────────────────────────────────────────

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
            format!("индекс бита {} отрицателен", n),
        ));
        return;
    }
    if let Some(width) = bit_width_of_type(ty, types, 8) {
        if n as u64 >= width {
            diags.push(Diagnostic::error(
                loc,
                format!(
                    "обращение к биту {} выходит за границы типа (ширина {} бит)",
                    n, width
                ),
            ));
        }
    }
}

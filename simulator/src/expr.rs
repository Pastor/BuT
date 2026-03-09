use but_grammar::ast::{Condition, Expression, Statement};
use thiserror::Error;

use crate::context::SimContext;
use crate::value::Value;

/// Ошибка при вычислении выражения.
#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Неизвестная переменная или порт: '{0}'")]
    UnknownVariable(String),
    #[error("Ошибка типа: невозможно применить оператор к {0:?} и {1:?}")]
    TypeError(Value, Value),
    #[error("Деление на ноль")]
    DivisionByZero,
    #[error("Неподдерживаемое выражение")]
    Unsupported,
}

/// Вычислить условие `Condition` (используется в переходах между состояниями).
pub fn eval_condition(cond: &Condition, ctx: &SimContext) -> Result<Value, EvalError> {
    match cond {
        Condition::BoolLiteral(_, b) => Ok(Value::Bool(*b)),
        Condition::NumberLiteral(_, n) => Ok(Value::Int(*n)),
        Condition::RationalNumberLiteral(_, s, neg) => {
            let f: f64 = s.parse().unwrap_or(0.0);
            Ok(Value::Real(if *neg { -f } else { f }))
        }
        Condition::HexNumberLiteral(_, s, _) => {
            let clean = s.trim_start_matches("0x").trim_start_matches("0X");
            let n = i64::from_str_radix(clean, 16).unwrap_or(0);
            Ok(Value::Int(n))
        }
        Condition::StringLiteral(lits) => {
            let s = lits.iter().map(|l| l.string.as_str()).collect::<String>();
            Ok(Value::Str(s))
        }
        Condition::Variable(ident) => ctx
            .get(&ident.name)
            .cloned()
            .ok_or_else(|| EvalError::UnknownVariable(ident.name.clone())),

        // Unary
        Condition::Not(_, inner) => {
            let v = eval_condition(inner, ctx)?;
            Ok(Value::Bool(!v.is_truthy()))
        }

        // Binary: Equal (= in BuT is equality test in conditions)
        Condition::Equal(_, l, r) => {
            let lv = eval_condition(l, ctx)?;
            let rv = eval_condition(r, ctx)?;
            Ok(Value::Bool(values_equal(&lv, &rv)))
        }
        Condition::NotEqual(_, l, r) => {
            let lv = eval_condition(l, ctx)?;
            let rv = eval_condition(r, ctx)?;
            Ok(Value::Bool(!values_equal(&lv, &rv)))
        }
        Condition::Less(_, l, r) => {
            let lv = eval_condition(l, ctx)?;
            let rv = eval_condition(r, ctx)?;
            Ok(Value::Bool(compare_lt(&lv, &rv)?))
        }
        Condition::LessEqual(_, l, r) => {
            let lv = eval_condition(l, ctx)?;
            let rv = eval_condition(r, ctx)?;
            Ok(Value::Bool(!compare_lt(&rv, &lv)?))
        }
        Condition::More(_, l, r) => {
            let lv = eval_condition(l, ctx)?;
            let rv = eval_condition(r, ctx)?;
            Ok(Value::Bool(compare_lt(&rv, &lv)?))
        }
        Condition::MoreEqual(_, l, r) => {
            let lv = eval_condition(l, ctx)?;
            let rv = eval_condition(r, ctx)?;
            Ok(Value::Bool(!compare_lt(&lv, &rv)?))
        }

        // Логические операции
        Condition::And(_, l, r) => {
            let lv = eval_condition(l, ctx)?;
            if !lv.is_truthy() {
                return Ok(Value::Bool(false));
            }
            let rv = eval_condition(r, ctx)?;
            Ok(Value::Bool(rv.is_truthy()))
        }
        Condition::Or(_, l, r) => {
            let lv = eval_condition(l, ctx)?;
            if lv.is_truthy() {
                return Ok(Value::Bool(true));
            }
            let rv = eval_condition(r, ctx)?;
            Ok(Value::Bool(rv.is_truthy()))
        }

        // Арифметика (can appear in conditions)
        Condition::Add(_, l, r) => {
            let lv = eval_condition(l, ctx)?;
            let rv = eval_condition(r, ctx)?;
            add_values(&lv, &rv)
        }
        Condition::Subtract(_, l, r) => {
            let lv = eval_condition(l, ctx)?;
            let rv = eval_condition(r, ctx)?;
            sub_values(&lv, &rv)
        }

        // Parenthesis
        Condition::Parenthesis(_, inner) => eval_condition(inner, ctx),

        // Array subscript / member access: not fully supported in simulation
        Condition::ArraySubscript(_, ident, idx) => {
            // Попытка найти базовую переменную; возвращаем Unit если не найдено
            let _ = (ident, idx);
            Ok(Value::Unit)
        }
        Condition::MemberAccess(_, base, _member) => {
            eval_condition(base, ctx)
        }

        // Function calls: special handling for S() (state checking)
        Condition::FunctionCall(_, ident, args) => {
            match ident.name.as_str() {
                "S" => {
                    // S(MachineName) = StateName — handled at transition level
                    // Возвращаем символический заполнитель; фактическое сравнение — выше
                    if let Some(arg) = args.first() {
                        if let Condition::Variable(v) = arg {
                            return Ok(Value::Str(format!("__state__{}", v.name)));
                        }
                    }
                    Ok(Value::Unit)
                }
                _ => {
                    // Неизвестная функция: возвращаем Unit
                    Ok(Value::Unit)
                }
            }
        }

        Condition::HexLiteral(lits) => {
            if let Some(lit) = lits.first() {
                let clean = lit.hex.trim_start_matches("hex\"").trim_end_matches('"');
                let n = i64::from_str_radix(clean, 16).unwrap_or(0);
                Ok(Value::Int(n))
            } else {
                Ok(Value::Unit)
            }
        }
    }
}

/// Вычислить выражение `Expression` (используется в операторах действий).
pub fn eval_expr(expr: &Expression, ctx: &mut SimContext) -> Result<Value, EvalError> {
    match expr {
        Expression::NumberLiteral(_, n) => Ok(Value::Int(*n)),
        Expression::HexNumberLiteral(_, s, _) => {
            let clean = s.trim_start_matches("0x").trim_start_matches("0X");
            let n = i64::from_str_radix(clean, 16).unwrap_or(0);
            Ok(Value::Int(n))
        }
        Expression::RationalNumberLiteral(_, s, neg) => {
            let f: f64 = s.parse().unwrap_or(0.0);
            Ok(Value::Real(if *neg { -f } else { f }))
        }
        Expression::BoolLiteral(_, b) => Ok(Value::Bool(*b)),
        Expression::StringLiteral(lits) => {
            let s = lits.iter().map(|l| l.string.as_str()).collect::<String>();
            Ok(Value::Str(s))
        }

        Expression::Variable(ident) => ctx
            .get(&ident.name)
            .cloned()
            .ok_or_else(|| EvalError::UnknownVariable(ident.name.clone())),

        Expression::Parenthesis(_, inner) => eval_expr(inner, ctx),

        // Присваивание: lhs = rhs
        Expression::Assign(_, lhs, rhs) => {
            let val = eval_expr(rhs, ctx)?;
            assign_expr(lhs, val.clone(), ctx)?;
            Ok(val)
        }

        // Составные присваивания
        Expression::AssignAdd(_, lhs, rhs) => compound_assign(lhs, rhs, ctx, add_values),
        Expression::AssignSubtract(_, lhs, rhs) => compound_assign(lhs, rhs, ctx, sub_values),
        Expression::AssignMultiply(_, lhs, rhs) => compound_assign(lhs, rhs, ctx, mul_values),
        Expression::AssignDivide(_, lhs, rhs) => compound_assign(lhs, rhs, ctx, div_values),
        Expression::AssignModulo(_, lhs, rhs) => compound_assign(lhs, rhs, ctx, mod_values),

        // Арифметика
        Expression::Add(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            add_values(&lv, &rv)
        }
        Expression::Subtract(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            sub_values(&lv, &rv)
        }
        Expression::Multiply(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            mul_values(&lv, &rv)
        }
        Expression::Divide(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            div_values(&lv, &rv)
        }
        Expression::Modulo(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            mod_values(&lv, &rv)
        }
        Expression::Power(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            pow_values(&lv, &rv)
        }

        // Сравнение
        Expression::Equal(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            Ok(Value::Bool(values_equal(&lv, &rv)))
        }
        Expression::NotEqual(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            Ok(Value::Bool(!values_equal(&lv, &rv)))
        }
        Expression::Less(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            Ok(Value::Bool(compare_lt(&lv, &rv)?))
        }
        Expression::LessEqual(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            Ok(Value::Bool(!compare_lt(&rv, &lv)?))
        }
        Expression::More(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            Ok(Value::Bool(compare_lt(&rv, &lv)?))
        }
        Expression::MoreEqual(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            Ok(Value::Bool(!compare_lt(&lv, &rv)?))
        }

        // Логические операции
        Expression::And(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            if !lv.is_truthy() {
                return Ok(Value::Bool(false));
            }
            let rv = eval_expr(r, ctx)?;
            Ok(Value::Bool(rv.is_truthy()))
        }
        Expression::Or(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            if lv.is_truthy() {
                return Ok(Value::Bool(true));
            }
            let rv = eval_expr(r, ctx)?;
            Ok(Value::Bool(rv.is_truthy()))
        }
        Expression::Not(_, inner) => {
            let v = eval_expr(inner, ctx)?;
            Ok(Value::Bool(!v.is_truthy()))
        }
        Expression::Negate(_, inner) => {
            let v = eval_expr(inner, ctx)?;
            match v {
                Value::Int(n) => Ok(Value::Int(-n)),
                Value::Real(r) => Ok(Value::Real(-r)),
                _ => Err(EvalError::Unsupported),
            }
        }

        // Инкремент / декремент
        Expression::PostIncrement(_, inner) | Expression::PreIncrement(_, inner) => {
            let val = eval_expr(inner, ctx)?;
            let new_val = match &val {
                Value::Int(n) => Value::Int(n + 1),
                Value::Real(r) => Value::Real(r + 1.0),
                _ => return Err(EvalError::Unsupported),
            };
            assign_expr(inner, new_val, ctx)?;
            Ok(val)
        }
        Expression::PostDecrement(_, inner) | Expression::PreDecrement(_, inner) => {
            let val = eval_expr(inner, ctx)?;
            let new_val = match &val {
                Value::Int(n) => Value::Int(n - 1),
                Value::Real(r) => Value::Real(r - 1.0),
                _ => return Err(EvalError::Unsupported),
            };
            assign_expr(inner, new_val, ctx)?;
            Ok(val)
        }

        // Function calls: best-effort (call known builtins, skip unknown)
        Expression::FunctionCall(_, ident, _args) => {
            // Встроенные функции: пока возвращаем Unit
            eprintln!("[sim] вызов функции '{}' не реализован", ident.name);
            Ok(Value::Unit)
        }

        // Условный оператор: cond ? then : else
        Expression::ConditionalOperator(_, cond, then_e, else_e) => {
            let cv = eval_expr(cond, ctx)?;
            if cv.is_truthy() {
                eval_expr(then_e, ctx)
            } else {
                eval_expr(else_e, ctx)
            }
        }

        // Побитовые операции (трактуются как арифметика для Int)
        Expression::BitwiseAnd(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            match (&lv, &rv) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
                _ => Ok(Value::Unit),
            }
        }
        Expression::BitwiseOr(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            match (&lv, &rv) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
                _ => Ok(Value::Unit),
            }
        }
        Expression::BitwiseXor(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            match (&lv, &rv) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
                _ => Ok(Value::Unit),
            }
        }
        Expression::ShiftLeft(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            match (&lv, &rv) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a << b)),
                _ => Ok(Value::Unit),
            }
        }
        Expression::ShiftRight(_, l, r) => {
            let lv = eval_expr(l, ctx)?;
            let rv = eval_expr(r, ctx)?;
            match (&lv, &rv) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a >> b)),
                _ => Ok(Value::Unit),
            }
        }

        // Варианты массивов и присваиваний
        Expression::AssignOr(_, lhs, rhs) => compound_assign(lhs, rhs, ctx, |l, r| {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
                _ => Ok(Value::Unit),
            }
        }),
        Expression::AssignAnd(_, lhs, rhs) => compound_assign(lhs, rhs, ctx, |l, r| {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
                _ => Ok(Value::Unit),
            }
        }),
        Expression::AssignXor(_, lhs, rhs) => compound_assign(lhs, rhs, ctx, |l, r| {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
                _ => Ok(Value::Unit),
            }
        }),
        Expression::AssignShiftLeft(_, lhs, rhs) => compound_assign(lhs, rhs, ctx, |l, r| {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a << b)),
                _ => Ok(Value::Unit),
            }
        }),
        Expression::AssignShiftRight(_, lhs, rhs) => compound_assign(lhs, rhs, ctx, |l, r| {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a >> b)),
                _ => Ok(Value::Unit),
            }
        }),

        // Всё остальное: возвращаем Unit
        _ => {
            eprintln!("[sim] неподдерживаемое выражение: {:?}", expr);
            Ok(Value::Unit)
        }
    }
}

/// Выполнить оператор (изменяет контекст).
pub fn exec_statement(stmt: &Statement, ctx: &mut SimContext) -> Result<(), EvalError> {
    match stmt {
        Statement::Block { statements, .. } => {
            for s in statements {
                exec_statement(s, ctx)?;
            }
        }
        Statement::Expression(_, expr) => {
            eval_expr(expr, ctx)?;
        }
        Statement::If(_, cond, then_body, else_body) => {
            let cv = eval_expr(cond, ctx)?;
            if cv.is_truthy() {
                exec_statement(then_body, ctx)?;
            } else if let Some(else_s) = else_body {
                exec_statement(else_s, ctx)?;
            }
        }
        Statement::While(_, cond, body) => {
            let mut limit = 10_000usize;
            loop {
                let cv = eval_expr(cond, ctx)?;
                if !cv.is_truthy() || limit == 0 {
                    break;
                }
                limit -= 1;
                exec_statement(body, ctx)?;
            }
        }
        Statement::For(_, init, cond, post, body) => {
            if let Some(init_s) = init {
                exec_statement(init_s, ctx)?;
            }
            let mut limit = 10_000usize;
            loop {
                if let Some(c) = cond {
                    let cv = eval_expr(c, ctx)?;
                    if !cv.is_truthy() || limit == 0 {
                        break;
                    }
                    limit -= 1;
                }
                if let Some(b) = body {
                    exec_statement(b, ctx)?;
                }
                if let Some(p) = post {
                    eval_expr(p, ctx)?;
                }
            }
        }
        Statement::Return(_, _) => {
            // Return не поддерживается в обработчиках верхнего уровня
        }
        Statement::VariableDefinition(_, decl, init) => {
            let name = decl.name.as_ref().map(|n| n.name.as_str()).unwrap_or("");
            let val = if let Some(e) = init {
                eval_expr(e, ctx).unwrap_or(Value::Unit)
            } else {
                Value::Unit
            };
            ctx.declare_var(name, val);
        }
        Statement::Continue(_) | Statement::Break(_) => {}
        _ => {}
    }
    Ok(())
}

/// Присвоить значение l-выражению.
fn assign_expr(lhs: &Expression, val: Value, ctx: &mut SimContext) -> Result<(), EvalError> {
    match lhs {
        Expression::Variable(ident) => {
            ctx.set(&ident.name, val);
            Ok(())
        }
        Expression::Parenthesis(_, inner) => assign_expr(inner, val, ctx),
        _ => {
            eprintln!("[sim] неподдерживаемый lvalue: {:?}", lhs);
            Ok(())
        }
    }
}

/// Вспомогательная функция для составного присваивания.
fn compound_assign(
    lhs: &Expression,
    rhs: &Expression,
    ctx: &mut SimContext,
    op: impl Fn(&Value, &Value) -> Result<Value, EvalError>,
) -> Result<Value, EvalError> {
    let lv = eval_expr(lhs, ctx)?;
    let rv = eval_expr(rhs, ctx)?;
    let result = op(&lv, &rv)?;
    assign_expr(lhs, result.clone(), ctx)?;
    Ok(result)
}

// ── Арифметические вспомогательные функции ─────────────────────────────────────

pub fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Real(x), Value::Real(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Bit(x), Value::Bit(y)) => x == y,
        (Value::Int(x), Value::Real(y)) => (*x as f64) == *y,
        (Value::Real(x), Value::Int(y)) => *x == (*y as f64),
        (Value::Int(x), Value::Bit(y)) => (*x != 0) == *y,
        (Value::Bit(x), Value::Int(y)) => *x == (*y != 0),
        _ => false,
    }
}

pub fn compare_lt(a: &Value, b: &Value) -> Result<bool, EvalError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(x < y),
        (Value::Real(x), Value::Real(y)) => Ok(x < y),
        (Value::Int(x), Value::Real(y)) => Ok((*x as f64) < *y),
        (Value::Real(x), Value::Int(y)) => Ok(*x < (*y as f64)),
        _ => Err(EvalError::TypeError(a.clone(), b.clone())),
    }
}

pub fn add_values(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
        (Value::Real(x), Value::Real(y)) => Ok(Value::Real(x + y)),
        (Value::Int(x), Value::Real(y)) => Ok(Value::Real(*x as f64 + y)),
        (Value::Real(x), Value::Int(y)) => Ok(Value::Real(x + *y as f64)),
        (Value::Str(x), Value::Str(y)) => Ok(Value::Str(format!("{}{}", x, y))),
        (Value::Str(x), other) => Ok(Value::Str(format!("{}{}", x, other))),
        _ => Err(EvalError::TypeError(a.clone(), b.clone())),
    }
}

pub fn sub_values(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x - y)),
        (Value::Real(x), Value::Real(y)) => Ok(Value::Real(x - y)),
        (Value::Int(x), Value::Real(y)) => Ok(Value::Real(*x as f64 - y)),
        (Value::Real(x), Value::Int(y)) => Ok(Value::Real(x - *y as f64)),
        _ => Err(EvalError::TypeError(a.clone(), b.clone())),
    }
}

pub fn mul_values(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x * y)),
        (Value::Real(x), Value::Real(y)) => Ok(Value::Real(x * y)),
        (Value::Int(x), Value::Real(y)) => Ok(Value::Real(*x as f64 * y)),
        (Value::Real(x), Value::Int(y)) => Ok(Value::Real(x * *y as f64)),
        _ => Err(EvalError::TypeError(a.clone(), b.clone())),
    }
}

pub fn div_values(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => {
            if *y == 0 {
                Err(EvalError::DivisionByZero)
            } else {
                Ok(Value::Int(x / y))
            }
        }
        (Value::Real(x), Value::Real(y)) => Ok(Value::Real(x / y)),
        (Value::Int(x), Value::Real(y)) => Ok(Value::Real(*x as f64 / y)),
        (Value::Real(x), Value::Int(y)) => Ok(Value::Real(x / *y as f64)),
        _ => Err(EvalError::TypeError(a.clone(), b.clone())),
    }
}

pub fn mod_values(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => {
            if *y == 0 {
                Err(EvalError::DivisionByZero)
            } else {
                Ok(Value::Int(x % y))
            }
        }
        _ => Err(EvalError::TypeError(a.clone(), b.clone())),
    }
}

pub fn pow_values(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => {
            if *y >= 0 {
                Ok(Value::Int(x.pow(*y as u32)))
            } else {
                Ok(Value::Real((*x as f64).powi(*y as i32)))
            }
        }
        (Value::Real(x), Value::Real(y)) => Ok(Value::Real(x.powf(*y))),
        (Value::Int(x), Value::Real(y)) => Ok(Value::Real((*x as f64).powf(*y))),
        (Value::Real(x), Value::Int(y)) => Ok(Value::Real(x.powi(*y as i32))),
        _ => Err(EvalError::TypeError(a.clone(), b.clone())),
    }
}

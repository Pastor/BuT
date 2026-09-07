//! Что функции нужно сверх её объявленных параметров.
//!
//! ## Задача
//!
//! Функция Takt читает состояние модели, хотя в параметрах его нет:
//!
//! ```lam
//! in pos_stack: u8 at 0x200:0;          // порт
//! var x: u8 := 1;                        // переменная
//!
//! fn travel_time(to_stack: u8) -> u8 {
//!     if pos_stack > to_stack { ... }      // читает порт
//! }
//! fn is_collected(c: Constant, v: u8) -> bool {
//!     if x.2 = 0 { ... }                   // читает переменную
//! }
//! ```
//!
//! Цель `c` решает это одним приёмом: функция получает `const Stacker *model` и читает
//! через него и переменные (`model->x`), и порты (`model->read_numeric(...)`). Причём
//! вызывать её может и под-модель - тогда передаётся `main`.
//!
//! В Rust такого "одного указателя на всё" нет: `self` у свободной функции
//! отсутствует, а сделать функцию методом нельзя - её вызывает и под-модель,
//! у которой доступа к корню нет (тот же запрет двойного заимствования, что и
//! у `tick`). Поэтому нужда раскладывается на две составляющие и передаётся
//! **параметрами**:
//!
//! | Что читает функция | Что получает |
//! |---|---|
//! | порт, `extern fn`, `debug` | `hal: &mut H` |
//! | переменную модели | параметр по значению |
//!
//! Это ровно тот же приём, которым `tick` получает общие переменные корня
//! (`rust_model::shared_variables`), и та же развилка, которую цель `st` решает через
//! `VAR_IN_OUT`.
//!
//! ## Транзитивность
//!
//! Если `f` вызывает `g`, то `f` обязана получить всё, что нужно `g`, - иначе ей нечего
//! будет передать. Рекурсия в Takt запрещена (`SE-053`), поэтому граф вызовов - Даг и
//! обход завершается; защита `seen` оставлена как страховка, а не как необходимость.

use crate::diagnostics::Diagnostic;
use crate::semantic::type_node::TypeNode;
use crate::semantic::{
    ConditionNode, ExpressionNode, FunctionDefinitionNode, ModelNode, StatementNode, VariableNode,
};
use std::collections::{BTreeMap, BTreeSet};

/// Что функции нужно сверх объявленных параметров.
#[derive(Default, Debug, PartialEq, Eq)]
pub(crate) struct FnNeeds {
    /// Нужен ли доступ к HAL (порт, `extern fn` либо `debug`).
    pub(crate) hal: bool,
    /// Читаемые переменные модели: имя -> тип.
    ///
    /// `BTreeMap` - порядок параметров обязан быть воспроизводимым: он же задаёт
    /// порядок аргументов на каждом вызове.
    pub(crate) vars: BTreeMap<String, TypeNode>,
}

impl FnNeeds {
    /// Вливает нужду вызываемой функции в нужду вызывающей.
    fn merge(&mut self, other: FnNeeds) {
        self.hal |= other.hal;
        self.vars.extend(other.vars);
    }
}

/// Считает, что нужно функции `def` сверх её параметров.
pub(crate) fn function_needs(
    def: &FunctionDefinitionNode,
    model: &ModelNode,
    seen: &mut BTreeSet<String>,
) -> Result<FnNeeds, Diagnostic> {
    let FunctionDefinitionNode::Local {
        name, params, body, ..
    } = def
    else {
        // У `extern fn` тела нет, у встроенной - тем более: читать нечего.
        return Ok(FnNeeds::default());
    };
    if !seen.insert(name.clone()) {
        // Цикл невозможен (`SE-053` запрещает рекурсию), но обход обязан завершаться
        // при любом входе.
        return Ok(FnNeeds::default());
    }
    let locals: BTreeSet<String> = params.iter().map(|(p, _)| p.clone()).collect();
    let mut needs = FnNeeds::default();
    walk_statement(body, model, &mut locals.clone(), &mut needs, seen)?;
    Ok(needs)
}

/// Считает нужду функции по её имени, разрешая имя по цепочке моделей.
///
/// `search_func` поднимается к родителям - это важно: `travel_time` объявлена в корне,
/// а вызывает её под-модель.
pub(crate) fn needs_of_call(
    name: &str,
    model: &ModelNode,
    seen: &mut BTreeSet<String>,
) -> Result<FnNeeds, Diagnostic> {
    let Some(def) = model.search_func(name) else {
        return Ok(FnNeeds::default());
    };
    let borrowed = def.borrow();
    match &*borrowed {
        // `extern fn` и `debug` сами по себе требуют HAL.
        FunctionDefinitionNode::External { .. } => Ok(FnNeeds {
            hal: true,
            vars: BTreeMap::new(),
        }),
        FunctionDefinitionNode::Builtin(builtin, _, _) => Ok(FnNeeds {
            hal: *builtin == "debug",
            vars: BTreeMap::new(),
        }),
        local @ FunctionDefinitionNode::Local { .. } => function_needs(local, model, seen),
        FunctionDefinitionNode::None | FunctionDefinitionNode::Unresolved(_) => {
            Ok(FnNeeds::default())
        }
    }
}

/// Учитывает обращение к переменной.
fn note_variable(var: &VariableNode, locals: &BTreeSet<String>, needs: &mut FnNeeds) {
    match var {
        // Порт читается через HAL - значит функции нужен `hal`.
        VariableNode::Port { .. } => needs.hal = true,
        VariableNode::Simple { name, ty, .. } => {
            if !locals.contains(name) {
                needs.vars.insert(name.clone(), ty.clone());
            }
        }
        // Константы живут на уровне модуля и передачи не требуют.
        VariableNode::Const { .. } | VariableNode::Unresolved => {}
    }
}

/// Обходит оператор.
fn walk_statement(
    stmt: &StatementNode,
    model: &ModelNode,
    locals: &mut BTreeSet<String>,
    needs: &mut FnNeeds,
    seen: &mut BTreeSet<String>,
) -> Result<(), Diagnostic> {
    match stmt {
        StatementNode::Block(items) => {
            for item in items {
                walk_statement(item, model, locals, needs, seen)?;
            }
        }
        StatementNode::Expression(expr, _) => walk_expression(expr, model, locals, needs, seen)?,
        StatementNode::Variable(name, _, init, _) => {
            if let Some(init) = init {
                walk_expression(init, model, locals, needs, seen)?;
            }
            // Регистрируется после обхода инициализатора: `var x := x;` читает внешний
            // `x`.
            locals.insert(name.clone());
        }
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            walk_expression(cond, model, locals, needs, seen)?;
            walk_statement(then_, model, locals, needs, seen)?;
            if let Some(alt) = else_ {
                walk_statement(alt, model, locals, needs, seen)?;
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(cond) = cond {
                walk_expression(cond, model, locals, needs, seen)?;
            }
            walk_statement(body, model, locals, needs, seen)?;
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(init) = init {
                walk_statement(init, model, locals, needs, seen)?;
            }
            if let Some(cond) = cond {
                walk_expression(cond, model, locals, needs, seen)?;
            }
            if let Some(step) = step {
                walk_expression(step, model, locals, needs, seen)?;
            }
            walk_statement(body, model, locals, needs, seen)?;
        }
        StatementNode::Match { expr, arms, .. } => {
            walk_expression(expr, model, locals, needs, seen)?;
            for arm in arms {
                for pattern in &arm.patterns {
                    if let crate::semantic::MatchPatternNode::Value(value) = pattern {
                        walk_expression(value, model, locals, needs, seen)?;
                    }
                }
                walk_statement(&arm.body, model, locals, needs, seen)?;
            }
        }
        StatementNode::Return(Some(expr), _) => walk_expression(expr, model, locals, needs, seen)?,
        // Вставка: нужды создаёт лишь тело, которое эта цель печатает.
        StatementNode::Assembly { target, body, .. } => {
            if crate::semantic::target_block::emits_for(target.as_deref(), "rust") {
                walk_statement(body, model, locals, needs, seen)?;
            }
        }
        StatementNode::Return(None, _)
        | StatementNode::Continue(_)
        | StatementNode::Break(_)
        | StatementNode::Formula(_)
        | StatementNode::InlineFormula(_)
        | StatementNode::None
        | StatementNode::Unresolved(_) => {}
    }
    Ok(())
}

/// Обходит выражение.
///
/// Ветки `_` нет намеренно: новый узел `ExpressionNode` обязан валить сборку.
/// Пропустить узел здесь - значит недосчитать параметр и получить расхождение сигнатуры
/// с вызовом, то есть код, который не собирается у пользователя.
fn walk_expression(
    expr: &ExpressionNode,
    model: &ModelNode,
    locals: &mut BTreeSet<String>,
    needs: &mut FnNeeds,
    seen: &mut BTreeSet<String>,
) -> Result<(), Diagnostic> {
    match expr {
        ExpressionNode::Variable(var) => note_variable(&var.borrow(), locals, needs),
        ExpressionNode::Function(def, args) => {
            for arg in args {
                walk_expression(arg, model, locals, needs, seen)?;
            }
            let borrowed = def.borrow();
            match &*borrowed {
                FunctionDefinitionNode::Builtin(name, _, _) => {
                    if *name == "debug" {
                        needs.hal = true;
                    }
                }
                FunctionDefinitionNode::External { .. } => needs.hal = true,
                // Вызываемая функция тянет свою нужду в вызывающую: иначе той нечего
                // будет передать.
                local @ FunctionDefinitionNode::Local { .. } => {
                    needs.merge(function_needs(local, model, &mut seen.clone())?)
                }
                FunctionDefinitionNode::None | FunctionDefinitionNode::Unresolved(_) => {}
            }
        }
        // База - выражение: обходится тем же сборщиком нужд.
        ExpressionNode::ArraySubscript(base, index) => {
            walk_expression(base, model, locals, needs, seen)?;
            walk_expression(index, model, locals, needs, seen)?;
        }
        ExpressionNode::ArraySlice(base, _, _) => {
            walk_expression(base, model, locals, needs, seen)?
        }
        ExpressionNode::Parenthesis(a)
        | ExpressionNode::Not(a)
        | ExpressionNode::BitwiseNot(a)
        | ExpressionNode::UnaryPlus(a)
        | ExpressionNode::Negate(a)
        | ExpressionNode::BitAccess(a, _)
        | ExpressionNode::Cast(a, _) => walk_expression(a, model, locals, needs, seen)?,
        ExpressionNode::CodeBlock(a, stmt) => {
            walk_expression(a, model, locals, needs, seen)?;
            walk_statement(stmt, model, locals, needs, seen)?;
        }
        ExpressionNode::NamedFunctionBox(a, _) => walk_expression(a, model, locals, needs, seen)?,
        ExpressionNode::Power(a, b)
        | ExpressionNode::Multiply(a, b)
        | ExpressionNode::Divide(a, b)
        | ExpressionNode::Modulo(a, b)
        | ExpressionNode::Add(a, b)
        | ExpressionNode::Subtract(a, b)
        | ExpressionNode::ShiftLeft(a, b)
        | ExpressionNode::ShiftRight(a, b)
        | ExpressionNode::BitwiseAnd(a, b)
        | ExpressionNode::BitwiseXor(a, b)
        | ExpressionNode::BitwiseOr(a, b)
        | ExpressionNode::Less(a, b)
        | ExpressionNode::More(a, b)
        | ExpressionNode::LessEqual(a, b)
        | ExpressionNode::MoreEqual(a, b)
        | ExpressionNode::Equal(a, b)
        | ExpressionNode::NotEqual(a, b)
        | ExpressionNode::And(a, b)
        | ExpressionNode::Or(a, b)
        | ExpressionNode::Assign(a, b) => {
            walk_expression(a, model, locals, needs, seen)?;
            walk_expression(b, model, locals, needs, seen)?;
        }
        ExpressionNode::ConditionalOperator(a, b, c) => {
            walk_expression(a, model, locals, needs, seen)?;
            walk_expression(b, model, locals, needs, seen)?;
            walk_expression(c, model, locals, needs, seen)?;
        }
        ExpressionNode::Array(items) | ExpressionNode::Initializer(items) => {
            for item in items {
                walk_expression(item, model, locals, needs, seen)?;
            }
        }
        ExpressionNode::Condition(cond) => {
            walk_condition(&cond.borrow().value, model, locals, needs, seen)?
        }
        ExpressionNode::None
        | ExpressionNode::Unresolved(_)
        | ExpressionNode::Number(_)
        | ExpressionNode::Duration(_)
        | ExpressionNode::Rational(_, _)
        | ExpressionNode::String(_)
        | ExpressionNode::Type(_)
        | ExpressionNode::Address(_, _)
        | ExpressionNode::AnonPort(_)
        | ExpressionNode::Bool(_)
        | ExpressionNode::Model(_)
        | ExpressionNode::List(_) => {}
    }
    Ok(())
}

/// Обходит условие (именованные `cond` могут читать состояние модели).
fn walk_condition(
    cond: &ConditionNode,
    model: &ModelNode,
    locals: &mut BTreeSet<String>,
    needs: &mut FnNeeds,
    seen: &mut BTreeSet<String>,
) -> Result<(), Diagnostic> {
    match cond {
        ConditionNode::Variable(var, _) => note_variable(&var.borrow(), locals, needs),
        ConditionNode::ArraySubscript(base, index) => {
            walk_condition(base, model, locals, needs, seen)?;
            walk_condition(index, model, locals, needs, seen)?;
        }
        ConditionNode::Function(def, args, _) => {
            for arg in args {
                walk_condition(arg, model, locals, needs, seen)?;
            }
            let borrowed = def.borrow();
            match &*borrowed {
                FunctionDefinitionNode::Builtin(name, _, _) => {
                    if *name == "debug" {
                        needs.hal = true;
                    }
                }
                FunctionDefinitionNode::External { .. } => needs.hal = true,
                local @ FunctionDefinitionNode::Local { .. } => {
                    needs.merge(function_needs(local, model, &mut seen.clone())?)
                }
                FunctionDefinitionNode::None | FunctionDefinitionNode::Unresolved(_) => {}
            }
        }
        ConditionNode::Parenthesis(a) | ConditionNode::Not(a) | ConditionNode::BitAccess(a, _) => {
            walk_condition(a, model, locals, needs, seen)?
        }
        ConditionNode::Add(a, b)
        | ConditionNode::Subtract(a, b)
        | ConditionNode::And(a, b)
        | ConditionNode::Or(a, b)
        | ConditionNode::Less(a, b)
        | ConditionNode::More(a, b)
        | ConditionNode::LessEqual(a, b)
        | ConditionNode::MoreEqual(a, b)
        | ConditionNode::Equal(a, b)
        | ConditionNode::NotEqual(a, b) => {
            walk_condition(a, model, locals, needs, seen)?;
            walk_condition(b, model, locals, needs, seen)?;
        }
        // Вычисляемая выдержка: её операнды - переменные и порты, то есть нужды HAL
        // считаются по вложенному условию.
        ConditionNode::AfterExpr(inner) => {
            walk_condition(inner, model, locals, needs, seen)?;
        }
        ConditionNode::None
        | ConditionNode::Unresolved(_)
        | ConditionNode::Number(_)
        | ConditionNode::Duration(_)
        | ConditionNode::After(_)
        | ConditionNode::AfterTicks(_)
        | ConditionNode::Rational(_, _)
        | ConditionNode::String(_)
        | ConditionNode::Bool(_)
        | ConditionNode::AnonPort(_)
        | ConditionNode::Model(_, _)
        | ConditionNode::State(..)
        | ConditionNode::EnumVariant(_, _, _) => {}
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::tree::construct_model;

    /// Строит модель из исходника и считает нужду функции по имени.
    fn needs_of(src: &str, fn_name: &str) -> FnNeeds {
        let (ast, _) = crate::parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let model = model_rc.borrow();
        needs_of_call(fn_name, &model, &mut BTreeSet::new()).unwrap()
    }

    /// Функция, читающая только свои параметры, ничего сверх них не требует.
    #[test]
    fn pure_function_needs_nothing() {
        let needs = needs_of(
            "fn add(a: u8, b: u8) -> u8 { return a + b; } start S;",
            "add",
        );
        assert!(!needs.hal, "HAL не нужен: портов функция не трогает");
        assert!(needs.vars.is_empty(), "переменных модели функция не читает");
    }

    /// Функция, читающая переменную модели, требует её параметром.
    #[test]
    fn function_reading_variable_needs_it() {
        let needs = needs_of(
            "var x: u8 := 1; fn f(a: u8) -> u8 { return a + x; } start S;",
            "f",
        );
        assert!(!needs.hal);
        assert!(
            needs.vars.contains_key("x"),
            "переменная 'x' обязана попасть в параметры: {:?}",
            needs.vars
        );
    }

    /// Локальная `var` с тем же именем переменной модели её не затеняет наружу.
    ///
    /// `var x := 5;` в теле - это `let`, а не обращение к модели: требовать `x`
    /// параметром было бы неверно (и дало бы неиспользуемый параметр).
    #[test]
    fn local_var_is_not_a_model_variable() {
        let needs = needs_of(
            "var x: u8 := 1; fn f() -> u8 { var x: u8 := 5; return x; } start S;",
            "f",
        );
        assert!(
            needs.vars.is_empty(),
            "локальная переменная не требует параметра: {:?}",
            needs.vars
        );
    }

    /// Функция, читающая порт, требует HAL.
    ///
    /// Реальный случай корпуса: `travel_time` в `stacker.takt` читает `pos_stack`.
    #[test]
    fn function_reading_port_needs_hal() {
        let needs = needs_of(
            "in p: u8 at 0x10:0; fn f(a: u8) -> u8 { return a + p; } start S;",
            "f",
        );
        assert!(needs.hal, "чтение порта обязано требовать HAL");
        assert!(
            needs.vars.is_empty(),
            "порт передаётся через HAL, а не параметром: {:?}",
            needs.vars
        );
    }

    /// Функция, вызывающая `extern fn`, требует HAL.
    #[test]
    fn function_calling_extern_needs_hal() {
        let needs = needs_of(
            "extern fn ext(v: u8) -> u8; fn f(a: u8) -> u8 { return ext(a); } start S;",
            "f",
        );
        assert!(needs.hal, "extern fn идёт методом HAL");
    }

    /// **Транзитивность:** `f` вызывает `g`, читающую переменную - нужда у обеих.
    ///
    /// Без переноса `f` нечего было бы передать `g`, и сигнатура разошлась бы с
    /// вызовом.
    #[test]
    fn needs_are_transitive_through_calls() {
        let needs = needs_of(
            "var x: u8 := 1; \
             fn g(a: u8) -> u8 { return a + x; } \
             fn f(b: u8) -> u8 { return g(b); } \
             start S;",
            "f",
        );
        assert!(
            needs.vars.contains_key("x"),
            "нужда вызываемой функции обязана переноситься в вызывающую: {:?}",
            needs.vars
        );
    }

    /// Транзитивность работает и для HAL.
    #[test]
    fn hal_need_is_transitive() {
        let needs = needs_of(
            "in p: u8 at 0x10:0; \
             fn g() -> u8 { return p; } \
             fn f() -> u8 { return g(); } \
             start S;",
            "f",
        );
        assert!(needs.hal, "нужда в HAL обязана переноситься через вызов");
    }

    /// Константа параметром не передаётся - она живёт на уровне модуля.
    #[test]
    fn constant_needs_no_parameter() {
        let needs = needs_of(
            "const MAX: u8 := 10; fn f(a: u8) -> u8 { return a + MAX; } start S;",
            "f",
        );
        assert!(
            needs.vars.is_empty(),
            "константа не требует параметра: {:?}",
            needs.vars
        );
        assert!(!needs.hal);
    }
}

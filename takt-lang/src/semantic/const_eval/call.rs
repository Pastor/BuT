//! Вызов **константной функции** при компиляции.
//!
//! `D := calculate_parameter(U + 67)` - значение параметра даёт функция, исполненная
//! компилятором. Константность **выводится**, а не объявляется (положение 2 ): функция
//! константна ровно тогда, когда её тело удалось исполнить, опираясь только на
//! аргументы и константы. Всякая причина невозможности **называется**: чтение
//! переменной, порт, `extern fn`, неподдержанный оператор, исчерпанный бюджет.
//!
//! ## Второй интерпретатор рядом с эталоном
//!
//! Семантика вычислений в проекте живёт в `takt-sim/src/eval/`, но переиспользовать её
//! компилятор не может: зависимость направлена `takt-sim -> takt-lang`. Значит это
//! **вторая** реализация одного смысла, и проверять её обязана сверка **значений**: та
//! же функция, исполненная симулятором, даёт то же число
//! (`takt-lang/tests/const_eval_tests.rs` +
//! `takt-sim/tests/conformance_const_param_tests.rs`).

use crate::diagnostics::lang::keys;
use crate::msg;
use super::{Budget, ConstValue, Locals, eval_in, expr_loc, not_constant};
use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast;
use crate::semantic::{FunctionDefinitionNode, ModelNode};
use std::cell::RefCell;
use std::rc::Rc;

/// `SE-084` - функция не константна; причина названа.
fn not_const_fn(loc: Location, name: &str, reason: impl AsRef<str>) -> Diagnostic {
    Diagnostic::error(
        loc,
        msg!(keys::SE_084_FUNCTION_NOT_CONSTANT, name = name, reason = reason.as_ref()),
    )
    .with_code("SE-084")
}

/// Что произошло при исполнении оператора тела.
enum Flow {
    /// Управление идёт дальше.
    Next,
    /// Сработал `return`.
    Return(Option<ConstValue>),
}

/// Вычисляет вызов функции.
pub(super) fn eval_call(
    id: &ast::Identifier,
    args: &[ast::Expression],
    loc: Location,
    scope: &Rc<RefCell<ModelNode>>,
    locals: &Locals,
    budget: &mut Budget,
) -> Result<ConstValue, Diagnostic> {
    let name = id.name.as_str();
    // Берётся **сырое** определение (`raw_functions`): к стадии 5 узел `functions`
    // перезаписан разрешённым, и АСД тела из него не достать. Опираться на стадию было
    // бы хрупко - вычислитель зовут и из stage1 (аргументы инстанцирования), и по
    // готовому дереву (LSP, тесты).
    let define = search_raw_function(scope, name).ok_or_else(|| {
        // Функция в области видимости есть, но тела у неё нет: назвать причину точно
        // полезнее, чем "не найдена".
        match scope.borrow().search_func(name).map(|f| f.borrow().clone()) {
            Some(FunctionDefinitionNode::External { .. }) => not_const_fn(
                loc,
                name,
                "объявлена как extern — её значение даёт внешний код во время работы",
            ),
            Some(FunctionDefinitionNode::Builtin(_, _, _)) => not_const_fn(
                loc,
                name,
                "встроенная функция языка при компиляции не исполняется",
            ),
            _ => not_constant(
                loc,
                msg!(keys::CONST_FUNCTION_NOT_FOUND, name = name),
            ),
        }
    })?;
    if define.external {
        return Err(not_const_fn(
            loc,
            name,
            "объявлена как extern — её значение даёт внешний код во время работы",
        ));
    }

    // Аргументы вычисляются в области видимости **вызывающего**: они его выражения, а
    // не тела функции.
    let mut frame = Locals::default();
    let params = parameter_names(&define, loc, name)?;
    if params.len() != args.len() {
        return Err(not_const_fn(
            loc,
            name,
            format!(
                "передано аргументов: {}, объявлено параметров: {}",
                args.len(),
                params.len()
            ),
        ));
    }
    for (param, arg) in params.iter().zip(args) {
        let value = eval_in(arg, scope, locals, budget)?;
        frame.declare(param, value);
    }

    let body = define
        .body
        .as_ref()
        .ok_or_else(|| not_const_fn(loc, name, "у функции нет тела"))?;

    budget.deeper(loc)?;
    let flow = exec(body, scope, &mut frame, budget);
    budget.shallower();
    match flow? {
        Flow::Return(Some(value)) => Ok(value),
        Flow::Return(None) => Err(not_const_fn(
            loc,
            name,
            "выполнен 'return' без значения — параметру нечего присвоить",
        )),
        Flow::Next => Err(not_const_fn(
            loc,
            name,
            "тело завершилось без 'return' — значения нет",
        )),
    }
}

/// Достаёт **сырое** определение функции - то, что видит остальная семантика.
///
/// Поиск идёт единственным `search_func`: своей карты вычислитель не ведёт, иначе он
/// звал бы другую функцию, чем та, которую разрешает семантика. Сырой АСД берётся из
/// узла: до стадии 5 он лежит в `Unresolved`, после - в поле `raw` разрешённого
/// `Local`.
fn search_raw_function(scope: &Rc<RefCell<ModelNode>>, name: &str) -> Option<ast::FunctionDefine> {
    let found = scope.borrow().search_func(name)?;
    let node = found.borrow().clone();
    match node {
        FunctionDefinitionNode::Unresolved(define) => Some(define),
        FunctionDefinitionNode::Local { raw, .. } => Some(*raw),
        _ => None,
    }
}

/// Имена параметров функции по её объявлению.
fn parameter_names(
    define: &ast::FunctionDefine,
    loc: Location,
    name: &str,
) -> Result<Vec<String>, Diagnostic> {
    let mut names = Vec::with_capacity(define.params.len());
    for (param_loc, param) in &define.params {
        let param = param
            .as_ref()
            .ok_or_else(|| not_const_fn(*param_loc, name, "параметр объявлен без имени"))?;
        match &param.name {
            Some(id) => names.push(id.name.clone()),
            None => return Err(not_const_fn(loc, name, "параметр объявлен без имени")),
        }
    }
    Ok(names)
}

/// Исполняет оператор тела.
fn exec(
    stmt: &ast::Statement,
    scope: &Rc<RefCell<ModelNode>>,
    locals: &mut Locals,
    budget: &mut Budget,
) -> Result<Flow, Diagnostic> {
    use ast::Statement as S;
    match stmt {
        S::Block { statements, .. } => {
            for inner in statements {
                match exec(inner, scope, locals, budget)? {
                    Flow::Next => {}
                    ret @ Flow::Return(_) => return Ok(ret),
                }
            }
            Ok(Flow::Next)
        }
        S::Return(loc, expr) => {
            budget.step(*loc)?;
            match expr {
                Some(expr) => {
                    let value = eval_in(expr, scope, locals, budget)?;
                    Ok(Flow::Return(Some(value)))
                }
                None => Ok(Flow::Return(None)),
            }
        }
        S::If(loc, cond, then_, else_) => {
            budget.step(*loc)?;
            if truthy(&eval_in(cond, scope, locals, budget)?, *loc)? {
                exec(then_, scope, locals, budget)
            } else {
                match else_ {
                    Some(branch) => exec(branch, scope, locals, budget),
                    None => Ok(Flow::Next),
                }
            }
        }
        // Цикл исполняется, но под бюджетом шагов: бесконечный `loop` без него повесил
        // бы компилятор и LSP.
        S::Loop(loc, cond, body, _) => loop {
            budget.step(*loc)?;
            let go = match cond {
                Some(cond) => truthy(&eval_in(cond, scope, locals, budget)?, *loc)?,
                None => true,
            };
            if !go {
                return Ok(Flow::Next);
            }
            match exec(body, scope, locals, budget)? {
                Flow::Next => {}
                ret @ Flow::Return(_) => return Ok(ret),
            }
        },
        // Объявление локального значения. Инициализатор обязателен: без него значение
        // неизвестно, а "ноль по умолчанию" - догадка.
        S::Variable(loc, define, extra) => {
            budget.step(*loc)?;
            let (name, init) = local_declaration(define, extra.as_ref(), *loc)?;
            let value = eval_in(&init, scope, locals, budget)?;
            locals.declare(&name, value);
            Ok(Flow::Next)
        }
        // Присваивание **локальному** имени. Присваивание переменной модели - побочное
        // действие: у константного вычисления его быть не может.
        S::Expression(loc, ast::Expression::Assign(_, target, value)) => {
            budget.step(*loc)?;
            let ast::Expression::Variable(id) = target.as_ref() else {
                return Err(not_constant(
                    *loc,
                    "слева от ':=' в константном вычислении обязано стоять имя",
                ));
            };
            let value = eval_in(value, scope, locals, budget)?;
            if locals.assign(&id.name, value) {
                Ok(Flow::Next)
            } else {
                Err(not_constant(
                    id.loc,
                    format!(
                        "'{}' не локальное имя функции: при компиляции менять \
                         состояние модели нельзя",
                        id.name
                    ),
                ))
            }
        }
        // Оператор-выражение без присваивания: значение отбрасывается, то есть смысл
        // его - побочное действие, которого здесь быть не может.
        S::Expression(loc, _) => Err(not_constant(
            *loc,
            "оператор-выражение без присваивания в константном вычислении бессмыслен",
        )),
        other => Err(not_constant(
            other.loc(),
            "оператор в константном вычислении не поддержан",
        )),
    }
}

/// Имя и инициализатор локального объявления.
fn local_declaration(
    define: &ast::VariableDefine,
    extra: Option<&ast::Expression>,
    loc: Location,
) -> Result<(String, ast::Expression), Diagnostic> {
    use ast::VariableDefine as V;
    let (name, init) = match define {
        V::Variable {
            name, initializer, ..
        } => (name, initializer.clone().or_else(|| extra.cloned())),
        V::Constant {
            name, initializer, ..
        } => (name, Some(initializer.clone())),
        V::Port { .. } | V::Parameter { .. } => {
            return Err(not_constant(
                loc,
                "внутри функции объявляются только локальные значения",
            ));
        }
    };
    let name = name
        .as_ref()
        .map(|id| id.name.clone())
        .ok_or_else(|| not_constant(loc, "объявление без имени"))?;
    let init = init.ok_or_else(|| {
        not_constant(
            loc,
            msg!(keys::CONST_NO_INITIALIZER, name = name),
        )
    })?;
    Ok((name, init))
}

/// Истинность значения в условии.
fn truthy(value: &ConstValue, loc: Location) -> Result<bool, Diagnostic> {
    match value {
        ConstValue::Bool(v) => Ok(*v),
        ConstValue::Int(v) => Ok(*v != 0),
        other => Err(not_constant(
            loc,
            msg!(keys::CONST_NOT_A_CONDITION, kind = kind_of(other)),
        )),
    }
}

/// Вид значения - для текста диагностики.
fn kind_of(value: &ConstValue) -> &'static str {
    match value {
        ConstValue::Int(_) => "целое",
        ConstValue::Bool(_) => "булево",
        ConstValue::Duration(_) => "длительность",
        ConstValue::Rational(_, _) => "дробное",
        ConstValue::List(_) => "агрегат",
    }
}

/// Позиция выражения - реэкспорт для удобства чтения выше.
#[allow(dead_code)]
fn loc_of(expr: &ast::Expression) -> Location {
    expr_loc(expr)
}

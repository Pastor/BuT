//! Аргументы инстанцирования модели: `M(ИМЯ := ВЫРАЖЕНИЕ, ...)`.
//!
//! Разбирает список аргументов при имени модели в выражении реализации, проверяет его
//! **структурно** (форма аргумента, существование параметра, отсутствие повторов) и
//! **вычисляет значение** константным вычислителем (`semantic::const_eval`). В дерево
//! попадает литерал: применяет его потребитель (/05).
//!
//! Каждая ошибка употребления получает **свой** код: `M(unknown := 1)` и `M(acc := 1)` -
//! разные ошибки автора (опечатка против попытки задать переменную), и общий текст
//! "плохой аргумент" заставил бы гадать.

use crate::diagnostics::lang::keys;
use crate::msg;
use crate::diagnostics::{Diagnostic, Location};
use crate::parser::ast;
use crate::semantic::ModelNode;
use crate::semantic::const_eval;
use crate::semantic::expression::construct_expression;
use crate::semantic::extend::ParameterArgument;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

/// Разбирает аргументы инстанцирования и сверяет их с параметрами модели.
///
/// `target` - модель, которую инстанцируют; `call_loc` - позиция вызова целиком (для
/// диагностик, у которых своей позиции нет).
pub(super) fn parse_arguments(
    target: &Rc<RefCell<ModelNode>>,
    model_name: &str,
    args: &[ast::Expression],
    call_loc: Location,
    scope: &Rc<RefCell<ModelNode>>,
) -> Result<Vec<ParameterArgument>, Diagnostic> {
    let mut parsed: Vec<ParameterArgument> = Vec::with_capacity(args.len());
    // Позиция первого вхождения имени - чтобы повтор указал на оба места.
    let mut seen: BTreeMap<String, Location> = BTreeMap::new();

    for arg in args {
        let (loc, name, value) = destructure(arg, call_loc)?;
        check_declared(target, model_name, &name, loc)?;
        // Значение вычисляется **здесь и сейчас**: параметр задаётся при сборке
        // автомата, поэтому за границей семантики выражения аргумента не существует - в
        // дерево попадает литерал. Вычисляется в области видимости места
        // инстанцирования (`scope`), а не целевой модели: имена `Y`/`U` в `M(X := Y +
        // 1)` пишет тот, кто инстанцирует.
        let literal = const_eval::fold_to_literal(&value, scope)?;
        // Понижение - тем же `construct_expression`, что и у объявлений: печать
        // значения в целях идёт их обычным печатником выражений, а не отдельной веткой
        // "а вот аргумент печатается так".
        let value = construct_expression(literal, Vec::new(), Rc::clone(scope))?;
        if let Some(first) = seen.get(&name) {
            return Err(Diagnostic::error(
                loc,
                msg!(keys::SE_080_ARGUMENT_GIVEN_TWICE, name = name),
            )
            .with_code("SE-080")
            .with_note(*first, format!("первое задание '{name}'")));
        }
        seen.insert(name.clone(), loc);
        parsed.push(ParameterArgument { name, loc, value });
    }
    Ok(parsed)
}

/// Понижает q-литералы аргументов в представление - режим `assign`.
///
/// Проход зовётся только когда специализации не было. При `--parameters=specialize`
/// значение аргумента уезжает в инициализатор копии (`specialize::set_initializer`), и
/// понижает его общий проход объявлений: понизь проход литерал и здесь, тот прошёл бы
/// масштабирование дважды и дал `SE-058` на всех восьми целях.
///
/// Аргумент - такая же позиция приёмника, как присваивание и вызов: тип параметра
/// известен, и за границей семантики дробного литерала быть не должно - иначе цели
/// расходятся (`sv` отвечает `SV-002`, прочие печатают 2 вместо 640).
pub(crate) fn lower_argument_literals(model: &Rc<RefCell<ModelNode>>) -> Result<(), Diagnostic> {
    let mut visited = std::collections::HashSet::new();
    lower_in_model(model, &mut visited)
}

fn lower_in_model(
    model: &Rc<RefCell<ModelNode>>,
    visited: &mut std::collections::HashSet<*const RefCell<ModelNode>>,
) -> Result<(), Diagnostic> {
    if !visited.insert(Rc::as_ptr(model)) {
        return Ok(());
    }
    let nested: Vec<Rc<RefCell<ModelNode>>> =
        model.borrow().models.values().map(Rc::clone).collect();
    let states: Vec<String> = model.borrow().states.keys().cloned().collect();
    for state in states {
        let extend = {
            let m = model.borrow();
            match m.states.get(&state) {
                Some(crate::semantic::StateNode::Implement { implements, .. }) => {
                    Some(implements.clone())
                }
                _ => None,
            }
        };
        let Some(extend) = extend else { continue };
        let mut lowered = extend.clone();
        if lower_in_extend(&mut lowered)? {
            let mut m = model.borrow_mut();
            if let Some(crate::semantic::StateNode::Implement { implements, .. }) =
                m.states.get_mut(&state)
            {
                *implements = lowered;
            }
        }
    }
    for child in &nested {
        lower_in_model(child, visited)?;
    }
    Ok(())
}

/// Понижает литералы в аргументах одного выражения реализации.
///
/// Возвращает `true`, если что-то изменилось.
fn lower_in_extend(extend: &mut crate::semantic::extend::Extend) -> Result<bool, Diagnostic> {
    use crate::semantic::extend::Extend;
    match extend {
        Extend::Model(target, _, args) => {
            let mut changed = false;
            for arg in args.iter_mut() {
                let Some((m, n)) = parameter_fixed_format(target, &arg.name) else {
                    continue;
                };
                if let Some(repr) = crate::semantic::type_node::type_fixed::lower_fixed_literal(
                    &arg.value, m, n, arg.loc,
                )? {
                    arg.value = crate::semantic::ExpressionNode::Number(repr);
                    changed = true;
                }
            }
            Ok(changed)
        }
        Extend::Parallel(items) | Extend::Concatenation(items) => {
            let mut changed = false;
            for item in items.iter_mut() {
                changed |= lower_in_extend(item)?;
            }
            Ok(changed)
        }
        _ => Ok(false),
    }
}

/// Формат `q(m, n)` параметра `name` целевой модели; `None` - тип иной.
fn parameter_fixed_format(target: &Rc<RefCell<ModelNode>>, name: &str) -> Option<(u8, u8)> {
    let model = target.borrow();
    // Тип параметра живёт при его объявлении (`variables`), а список `parameters` несёт
    // лишь имя и признак изменяемости.
    match model.variables.get(name) {
        Some(crate::semantic::VariableNode::Simple { ty, .. })
        | Some(crate::semantic::VariableNode::Const { ty, .. }) => match ty {
            crate::semantic::type_node::TypeNode::Fixed { m, n, .. } => Some((*m, *n)),
            _ => None,
        },
        _ => None,
    }
}

/// Разбирает один аргумент в тройку "позиция имени, имя, значение".
///
/// Единственная допустимая форма - `ИМЯ := ВЫРАЖЕНИЕ`. Позиционных аргументов нет
/// намеренно: у параметров есть значения по умолчанию, поэтому позиция ничего не
/// значит, а порядок объявления автор менять вправе.
fn destructure(
    arg: &ast::Expression,
    call_loc: Location,
) -> Result<(Location, String, ast::Expression), Diagnostic> {
    let ast::Expression::Assign(assign_loc, target, value) = arg else {
        return Err(Diagnostic::error(
            arg_loc(arg).unwrap_or(call_loc),
            msg!(keys::SE_076_ARGUMENT_FORM),
        )
        .with_code("SE-076"));
    };
    match target.as_ref() {
        ast::Expression::Variable(id) => Ok((id.loc, id.name.clone(), (**value).clone())),
        other => Err(Diagnostic::error(
            arg_loc(other).unwrap_or(*assign_loc),
            msg!(keys::SE_076_ARGUMENT_NAME_EXPECTED),
        )
        .with_code("SE-076")),
    }
}

/// Проверяет, что имя обозначает **параметр** целевой модели.
fn check_declared(
    target: &Rc<RefCell<ModelNode>>,
    model_name: &str,
    name: &str,
    loc: Location,
) -> Result<(), Diagnostic> {
    let target = target.borrow();
    if target.parameters.iter().any(|p| p.name == name) {
        return Ok(());
    }
    // Модель параметров не объявляет вовсе - автор, скорее всего, перепутал модель, а
    // не имя.
    if target.parameters.is_empty() {
        return Err(Diagnostic::error(
            loc,
            msg!(keys::SE_077_MODEL_WITHOUT_PARAMETERS, model = model_name, name = name),
        )
        .with_code("SE-077"));
    }
    // Имя в модели есть, но это переменная, константа или порт: задавать их при
    // инстанцировании нельзя - это величины такта, а не настройка сборки.
    if target.variables.contains_key(name) {
        return Err(Diagnostic::error(
            loc,
            msg!(keys::SE_079_NOT_A_PARAMETER, name = name, model = model_name),
        )
        .with_code("SE-079"));
    }
    let known = target
        .parameters
        .iter()
        .map(|p| p.name.as_str())
        .collect::<Vec<_>>()
        .join(", ");
    Err(Diagnostic::error(
        loc,
        msg!(
            keys::SE_078_PARAMETER_NOT_FOUND,
            model = model_name,
            name = name,
            known = known
        ),
    )
    .with_code("SE-078"))
}

/// Позиция выражения, если она у варианта есть.
///
/// Нужна только для сообщений: у аргумента неверной формы своей позиции может и не
/// быть, тогда указываем на вызов целиком.
fn arg_loc(expr: &ast::Expression) -> Option<Location> {
    match expr {
        ast::Expression::Variable(id) => Some(id.loc),
        ast::Expression::Assign(loc, _, _)
        | ast::Expression::Number(loc, _)
        | ast::Expression::Function(loc, _, _)
        | ast::Expression::Parenthesis(loc, _) => Some(*loc),
        _ => None,
    }
}

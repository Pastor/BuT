use but_grammar::ast::{
    self, ConditionDefinition, Expression, Loc, ModelDefinition, ModelPart, Property,
    PropertyDefinition, SourceUnit, SourceUnitPart, StatePart, VariableAttribute,
    VariableDefinition,
};
use thiserror::Error;

use crate::context::SimContext;
use crate::ltl::extract_ltl_formulas;
use crate::machine::{Machine, MachineKind, State, Transition};
use crate::value::Value;

/// Ошибка при построении автомата.
#[derive(Debug, Error)]
pub enum BuildError {
    #[error("Модель '{0}' не имеет начального состояния")]
    NoStartState(String),
    #[error("Модель '{0}' ссылается на несуществующее состояние '{1}'")]
    UndefinedState(String, String),
    #[error("Неизвестная ссылка на модель '{0}' в выражении поведения")]
    UnknownModelRef(String),
}

/// Построить все автоматы из SourceUnit (обрабатывает глобальное поведение → композицию).
pub fn build_all(
    source: &SourceUnit,
) -> Result<Vec<MachineKind>, BuildError> {
    // Собрать глобальные объявления переменных/портов
    let mut global_vars: Vec<Box<VariableDefinition>> = vec![];
    let mut models: Vec<Box<ModelDefinition>> = vec![];

    for part in &source.0 {
        match part {
            SourceUnitPart::VariableDefinition(vd) => global_vars.push(vd.clone()),
            SourceUnitPart::ModelDefinition(md) => models.push(md.clone()),
            _ => {}
        }
    }

    // Построить все модели как отдельные автоматы
    let mut machines: Vec<MachineKind> = vec![];
    for model in &models {
        let machine = build_machine(model, &global_vars)?;
        machines.push(machine);
    }

    // Найти выражение композиции поведения на уровне исходного файла
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

/// Рекурсивно построить композицию автоматов из выражения поведения.
fn compose_from_expr(
    expr: &Expression,
    machines: &mut Vec<MachineKind>,
) -> Result<MachineKind, BuildError> {
    match expr {
        // A | B → Параллельное выполнение
        Expression::BitwiseOr(_, l, r) => {
            let lm = compose_from_expr(l, machines)?;
            let rm = compose_from_expr(r, machines)?;
            Ok(MachineKind::Parallel(vec![lm, rm]))
        }
        // A + B → Последовательная композиция
        Expression::Add(_, l, r) => {
            let lm = compose_from_expr(l, machines)?;
            let rm = compose_from_expr(r, machines)?;
            Ok(MachineKind::Sequence(vec![lm, rm]))
        }
        // Ссылка на переменную → поиск автомата по имени
        Expression::Variable(ident) => {
            let pos = machines
                .iter()
                .position(|m| m.name() == Some(ident.name.as_str()));
            match pos {
                Some(i) => Ok(machines.remove(i)),
                None => Err(BuildError::UnknownModelRef(ident.name.clone())),
            }
        }
        // Скобки
        Expression::Parenthesis(_, inner) => compose_from_expr(inner, machines),
        // Неподдерживаемый вариант
        _ => Err(BuildError::UnknownModelRef("?".to_string())),
    }
}

/// Построить автомат времени выполнения из AST ModelDefinition.
pub fn build_machine(
    model: &ModelDefinition,
    global_vars: &[Box<VariableDefinition>],
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

    // Извлечь LTL-формулы из аннотаций и блоков formula
    let ltl_formulas = extract_ltl_formulas(model);

    // Объявить глобальные переменные/порты в контексте
    for vd in global_vars {
        declare_variable_def(&mut context, vd);
    }

    // Первый проход: зарегистрировать все имена состояний и объявления уровня модели
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
                declare_variable_def(&mut context, vd);
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

    // Второй проход: построить содержимое состояний (переходы, обработчики)
    for part in &model.parts {
        if let ModelPart::StateDefinition(sd) = part {
            let sname = sd
                .name
                .as_ref()
                .map(|n| n.name.clone())
                .unwrap_or_default();

            // Сначала собрать вложенные автоматы (без изменяемой ссылки на states)
            let mut sub_machine: Option<MachineKind> = None;
            let mut extra_vars: Vec<Box<ast::VariableDefinition>> = vec![];

            for sp in &sd.parts {
                if let StatePart::VariableDefinition(vd) = sp {
                    extra_vars.push(vd.clone());
                }
                if let StatePart::ModelDefinition(nested) = sp {
                    if let Ok(sub) = build_machine(nested, global_vars) {
                        sub_machine = Some(sub);
                    }
                }
            }

            // Применить переменные состояния
            for vd in &extra_vars {
                declare_variable_def(&mut context, vd);
            }

            // Теперь взять изменяемую ссылку и применить все изменения
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

/// Объявить переменную в контексте.
fn declare_variable_def(ctx: &mut SimContext, vd: &VariableDefinition) {
    let name = match &vd.name {
        Some(n) => n.name.clone(),
        None => return,
    };

    // Проверить, является ли переменная портом
    let is_port = vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Portable(_)));
    let is_const = vd.attrs.iter().any(|a| matches!(a, VariableAttribute::Constant(_)));

    let initial = vd
        .initializer
        .as_ref()
        .and_then(|e| eval_literal(e))
        .unwrap_or(Value::Unit);

    if is_port {
        ctx.declare_port(&name, initial);
    } else {
        ctx.declare_var(&name, initial);
    }
    // Константные переменные хранятся в контексте; неизменяемость не проверяется в симуляторе
    let _ = is_const;
}

/// Вычислить литеральное выражение в значение (только для инициализаторов).
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

/// Обработать PropertyDefinition уровня модели (start, enter, exit, end, behavior).
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
        // exit/before на уровне модели — редко используются
        "exit" | "before" => {}
        _ => {}
    }
}

/// Обработать ConditionDefinition уровня модели (синтаксис `start: State;`).
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

/// Преобразовать Property в Statement для выполнения.
pub fn property_to_statement(prop: &Property) -> ast::Statement {
    match prop {
        Property::Function(stmt) => stmt.clone(),
        Property::Expression(expr) => {
            ast::Statement::Expression(Loc::Builtin, expr.clone())
        }
    }
}

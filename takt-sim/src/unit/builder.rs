use crate::context::Context;
use crate::eval::value::Value;
use crate::predicate::create_predicate;
use crate::unit::blocks::model_level_executions;
use crate::unit::statement::compile_block_body;
use crate::unit::{Execution, Predicate, Unit, UnitKind};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use takt_lang::diagnostics::{Diagnostic, Location};
use takt_lang::semantic::extend::{Extend, ParameterArgument};
use takt_lang::semantic::type_node::TypeNode;
use takt_lang::semantic::{
    ExpressionNode, ModelNode, ReferenceNode, StateNode, StateNodeKind, VariableNode,
};

use crate::unit::context_model::ModelNodeContext;
use crate::unit::initial::eval_expr;

type Executions = HashMap<String, Vec<Execution>>;

pub(crate) fn var_expr(var: &VariableNode) -> &ExpressionNode {
    match var {
        VariableNode::Simple { expr, .. } | VariableNode::Const { expr, .. } => expr,
        // У порта берётся **начальное значение**, а не адрес: прежде оба жили в одном
        // поле, и симулятор брал адрес за начальное значение - `in P: bit := 0x100;`
        // стартовал со значением 0x100. Здесь это и есть выставление значения "до
        // первого такта": эталону нечего эмитить, он им стартует. Совпадение с целями
        // проверяет потактовая сверка `takt-sim/tests/conformance_port_init_tests.rs` -
        // трасса начинается с состояния порта до такта.
        VariableNode::Port { init, .. } => init,
        VariableNode::Unresolved => &ExpressionNode::None,
    }
}

/// Объявленный тип переменной (или `None` для `Unresolved`).
pub(crate) fn var_type(var: &VariableNode) -> Option<&TypeNode> {
    match var {
        VariableNode::Simple { ty, .. }
        | VariableNode::Port { ty, .. }
        | VariableNode::Const { ty, .. } => Some(ty),
        VariableNode::Unresolved => None,
    }
}

/// Приводит начальное значение к типу `q(m, n)`, чтобы переменная хранилась как
/// [`Value::Fixed`]. Иначе арифметика над ней пошла бы целочисленным путём
/// (представление без сдвига у `*`/`/`) -> молча неверный результат.
///
/// Для **прочих** типов начальное значение оставляем как есть: историческое поведение
/// симулятора (усечение init по типу не выполнялось, а его добавление - смена поведения
/// вне объёма этой фичи). Литерал `q` уже понижен грамматикой в представление, поэтому
/// `coerce_to_type(Number, Fixed)` трактует его как сырой repr - двойного
/// масштабирования нет.
pub(crate) fn coerce_initial(value: Value, var: &VariableNode, model: &ModelNode) -> Value {
    match var_type(var) {
        Some(ty @ TypeNode::Fixed { .. }) => {
            crate::eval::coerce_to_type(value.clone(), ty).unwrap_or(value)
        }
        // Структура: инициализатор `{...}` (пришёл как `Array`) приводится к
        // `Value::Struct` по определению из модели. При неудаче (арность/тип) значение
        // остаётся `Array` - ошибка тогда всплывёт при доступе к полю (`read_member` ->
        // `FieldOfNonStruct`), а не молча: та же консервативность, что и у `Fixed` выше
        // (get_value ленив и Result не возвращает - задача сузить его контракт
        // вынесена, см.
        Some(ty @ TypeNode::Struct(_)) => {
            crate::eval::coerce_to_type_with(value.clone(), ty, &ModelStructs(model))
                .unwrap_or(value)
        }
        // Массив: список-инициализатор `{...}`/`[...]` (пришёл как `Array`) приводится
        // поэлементно к типу элемента с проверкой длины (`coerce_array`). При неудаче -
        // значение как есть: **скалярный** инициализатор массива (`[u8;4] := 0`) не
        // приводится и остаётся скаляром (в C он вовсе не выразим, CC-017; определить
        // его - вопрос семантики 0078, не этой фичи). Та же консервативность, что у
        // `Fixed`/`Struct`.
        Some(ty @ TypeNode::Array(..)) => {
            crate::eval::coerce_to_type_with(value.clone(), ty, &ModelStructs(model))
                .unwrap_or(value)
        }
        _ => value,
    }
}

/// Значение поля по умолчанию (нулевое) по его типу - для структуры без инициализатора.
/// Совпадает с default-init полей структуры в C.
pub(crate) fn default_field(ty: &TypeNode, model: &ModelNode) -> Value {
    match ty {
        TypeNode::Bool => Value::Boolean(false),
        TypeNode::Rational => Value::Real(0.0),
        // Длительность несёт свой вид значения: целочисленный ноль делал `d := d + 1s;`
        // ошибкой `SIM-005` "операция '+' не определена для операндов целое и
        // длительность" - при том, что все восемь целей тот же вход переводят. Единица
        // носителя - наносекунды.
        TypeNode::Duration => Value::Duration(0),
        // Длительность несёт свой вид значения: целочисленный ноль делал `d := d + 1s;`
        // ошибкой `SIM-005` "операция '+' не определена для операндов целое и
        // длительность" - при том, что все восемь целей тот же вход переводят. Единица
        // носителя - наносекунды.
        TypeNode::Fixed { m, n, sat } => Value::Fixed {
            repr: 0,
            m: *m,
            n: *n,
            sat: *sat,
        },
        TypeNode::Array(size, elem) => {
            Value::Array((0..*size).map(|_| default_field(elem, model)).collect())
        }
        TypeNode::Struct(name) => model
            .search_struct(name)
            .map(|def| Value::Struct {
                name: name.clone(),
                fields: def
                    .fields
                    .iter()
                    .map(|(f, t)| (f.clone(), default_field(t, model)))
                    .collect(),
            })
            .unwrap_or(Value::Number(0)),
        // Перечисление - Первый по тексту вариант: ноль может не принадлежать набору, и
        // тогда автомат стартует со значения, о котором не знает ни один `match`.
        // Носитель правила один на эталон и три цели - `semantic::enum_default`.
        TypeNode::Enum(name) => model
            .search_enum(name)
            .and_then(|def| takt_lang::semantic::enum_default(&def.variants))
            .map_or(Value::Number(0), |(_, value)| Value::Number(value)),
        // Integer/Bit/Address/прочее - целочисленный ноль.
        _ => Value::Number(0),
    }
}

/// Реестр структур над семантической моделью: поиск учитывает родительские модели через
/// [`ModelNode::search_struct`].
struct ModelStructs<'a>(&'a ModelNode);

impl crate::eval::StructRegistry for ModelStructs<'_> {
    fn find_struct(&self, name: &str) -> Option<takt_lang::semantic::StructDefinitionNode> {
        self.0.search_struct(name)
    }
}

// -- Точка входа ---------------------------------------------------------------

/// Строит дерево [`Unit`] из семантической модели.
pub fn build(model: Rc<RefCell<ModelNode>>) -> Result<Unit, Diagnostic> {
    build_impl(model, None, &[], Location::Builtin)
}

fn build_impl(
    model: Rc<RefCell<ModelNode>>,
    shared_parent: Option<Rc<RefCell<dyn Context>>>,
    args: &[ParameterArgument],
    call_loc: Location,
) -> Result<Unit, Diagnostic> {
    let has_states = model.borrow().has_states();
    if has_states {
        // Если модель содержит ровно одно состояние-реализацию без переходов (например,
        // `start Stacker = CR | MC | LC;`), делегируем в build_extend.
        let single_compound = {
            let borrowed = model.borrow();
            if borrowed.states.len() == 1 {
                borrowed.states.values().next().and_then(|state| {
                    if let StateNode::Implement {
                        implements,
                        references,
                        next,
                        ..
                    } = state
                    {
                        // Свёртка допустима, только пока у состояния нет собственных
                        // блоков: `Parallel` их не хранит, и `always`/`exit`
                        // состояния-композиции терялись молча - замер 2026-08-23 дал
                        // `hits = 0` против `1`/`2` у прошивки цели `c`. Со своими
                        // блоками модель строится обычным узлом: там блоки состояния
                        // живут в `state_executions` и работают механизмом 0181.
                        if references.is_empty()
                            && next.is_none()
                            && state.named_blocks().is_empty()
                        {
                            Some(implements.clone())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
            } else {
                None
            }
        };
        if let Some(implements) = single_compound {
            reject_unsupported_arguments(&model, args, call_loc)?;
            build_composition(model, &implements, shared_parent)
        } else {
            build_node(model, shared_parent, args)
        }
    } else {
        // Модель без своих состояний (`model M = A | B`): своего контекста у неё не
        // возникает - значения записывать некуда. Отказ вместо молчания.
        reject_unsupported_arguments(&model, args, call_loc)?;
        let extends = model.borrow().implements.clone();
        build_composition(model, &extends, shared_parent)
    }
}

/// Строит узел-композицию, **не теряя тела модели-владельца**.
fn build_composition(
    model: Rc<RefCell<ModelNode>>,
    extends: &Extend,
    shared_parent: Option<Rc<RefCell<dyn Context>>>,
) -> Result<Unit, Diagnostic> {
    let ctx: Rc<RefCell<dyn Context>> =
        Rc::new(RefCell::new(if let Some(shared) = shared_parent {
            ModelNodeContext::new_with_parent(model.clone(), Some(shared))
        } else {
            ModelNodeContext::new(model.clone())
        }));
    let mut unit = build_extend(extends, Some(ctx.clone()))?;
    let owner = model_level_executions(&model, ctx);
    if owner.is_empty() {
        return Ok(unit);
    }
    match unit.kind_mut() {
        UnitKind::Parallel { executions, .. } | UnitKind::Sequential { executions, .. } => {
            for (name, fns) in owner {
                executions.entry(name).or_default().extend(fns);
            }
        }
        // Композиция из одной ветви сворачивается в `Node` самой ветви; её `executions`
        // принадлежат чужой модели, и дописывать туда тело владельца нельзя - блок
        // исполнился бы в контексте ветви. Такой вход language-уровня не порождает
        // (`|`/`+` требуют двух операндов), ветвь существует для полноты разбора.
        UnitKind::Node { .. } | UnitKind::None => {}
    }
    Ok(unit)
}

/// Отказывает, если аргументы заданы модели, у которой нет своего контекста.
///
/// Такая модель (композиция без собственных состояний) значений параметров хранить
/// негде: их некуда записать, и молча потерять настройку - тот класс дефекта, ради
/// которого держит теста `SE-082`.
fn reject_unsupported_arguments(
    model: &Rc<RefCell<ModelNode>>,
    args: &[ParameterArgument],
    call_loc: Location,
) -> Result<(), Diagnostic> {
    match args.first() {
        None => Ok(()),
        Some(first) => Err(Diagnostic::error(
            first.loc,
            format!(
                "Модель '{}' — композиция без собственных состояний: задать её \
                 параметр '{}' при инстанцировании нельзя (значение хранить негде)",
                model.borrow().name.clone().unwrap_or_default(),
                first.name
            ),
        )
        .with_code("SIM-034")
        .with_note(call_loc, "инстанцирование здесь".to_string())),
    }
}

// -- Unit::Node ----------------------------------------------------------------

fn build_node(
    model: Rc<RefCell<ModelNode>>,
    shared_parent: Option<Rc<RefCell<dyn Context>>>,
    args: &[ParameterArgument],
) -> Result<Unit, Diagnostic> {
    let start_name = {
        let borrowed = model.borrow();
        borrowed
            .states
            .iter()
            .find_map(|(name, state)| match state {
                StateNode::Simple {
                    kind: StateNodeKind::Start,
                    ..
                }
                | StateNode::Implement {
                    kind: StateNodeKind::Start,
                    ..
                } => Some(name.clone()),
                _ => None,
            })
            .ok_or_else(|| {
                Diagnostic::error(
                    Location::Builtin,
                    "Нет начального состояния в модели".to_string(),
                )
            })
    }?;

    let states_snapshot: Vec<(String, StateNode)> = {
        let borrowed = model.borrow();
        borrowed
            .states
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    };

    // Контекст: используем общий родительский если передан, иначе строим иерархию.
    let ctx_rc: Rc<RefCell<dyn Context>> =
        Rc::new(RefCell::new(if let Some(shared) = shared_parent {
            ModelNodeContext::new_with_parent(model.clone(), Some(shared))
        } else {
            ModelNodeContext::new(model.clone())
        }));

    // Значения параметров этого экземпляра: записываются в контекст **сразу после его
    // создания** - до первого чтения и до первого такта. Ровно то же место, что у цели
    // `c`, где присваивание идёт после вызова `_init`.
    for arg in args {
        let value = eval_expr(&arg.value).ok_or_else(|| {
            Diagnostic::error(
                arg.loc,
                format!("Значение параметра '{}' не вычислено", arg.name),
            )
            .with_code("SIM-035")
        })?;
        let coerced = {
            let borrowed = model.borrow();
            match borrowed.variables.get(&arg.name) {
                Some(var) => coerce_initial(value, var, &borrowed),
                None => value,
            }
        };
        ctx_rc.borrow_mut().set_value(&arg.name, coerced);
    }

    let mut state_transitions: HashMap<String, Vec<(String, Predicate)>> = HashMap::new();
    let mut state_executions: HashMap<String, Executions> = HashMap::new();
    let mut state_every: HashMap<String, Vec<(i64, Vec<Execution>)>> = HashMap::new();
    // 0181 (закрытие исправления ): реализации состояний-реализаций.
    let mut state_impls: HashMap<String, Rc<RefCell<Unit>>> = HashMap::new();
    // 0044: инварианты модели (проверяются каждый такт) и по состояниям.
    let mut guards = crate::unit::Guards {
        model: build_guards(&model.borrow().formulas),
        per_state: HashMap::new(),
    };

    for (name, state_node) in &states_snapshot {
        state_transitions.insert(name.clone(), build_transitions(state_node)?);
        // Реализация состояния (`state P = A + B`) - дочерний юнит с контекстом этого
        // узла в качестве общего родителя. Общий контекст - не деталь: без него шаги
        // `+` не видят переменных друг друга, и значение, записанное шагом A,
        // проваливается в 0 при переходе к B.
        if let StateNode::Implement { implements, .. } = state_node {
            let inner = build_extend(implements, Some(ctx_rc.clone()))?;
            if !matches!(inner.kind(), UnitKind::None) {
                state_impls.insert(name.clone(), Rc::new(RefCell::new(inner)));
            }
        }
        let state_guards = build_guards(state_node.formulas());
        if !state_guards.is_empty() {
            guards.per_state.insert(name.clone(), state_guards);
        }

        let mut execs: Executions = HashMap::new();
        let mut every_blocks: Vec<(i64, Vec<Execution>)> = Vec::new();
        for block in state_node.named_blocks() {
            // `every Nms { ... }`: периодический блок. Собирается отдельно от обычных
            // (`always`/`enter`/`exit`) - несёт период, а не имя-ключ, и все `every`
            // одного состояния делят ключ иначе слиплись бы.
            if let Some((period_nanos, _)) = block.every_period() {
                if let Some(body) = block.statement() {
                    let fns = compile_block_body(body, ctx_rc.clone());
                    every_blocks.push((period_nanos, fns));
                }
                continue;
            }
            let kind = block.name();
            if kind.is_empty() {
                continue;
            }
            if let Some(body) = block.statement() {
                let fns = compile_block_body(body, ctx_rc.clone());
                if !fns.is_empty() {
                    execs.entry(kind.to_string()).or_default().extend(fns);
                }
            }
        }
        state_executions.insert(name.clone(), execs);
        if !every_blocks.is_empty() {
            state_every.insert(name.clone(), every_blocks);
        }
    }

    // именованные блоки **уровня модели** (`always` вне состояния). Компиляция вынесена
    // в `blocks`: тем же наполнением пользуется узел-композиция, а копия правила
    // разъехалась бы.
    let executions = model_level_executions(&model, ctx_rc.clone());

    // Стартовое состояние - в общий реестр прогона: условие `S(Модель) = Состояние`
    // спрашивает о модели, которая может не сделать ни одного перехода, и без этой
    // записи её состояние было бы неизвестно до первого перехода - то есть проверка
    // "модель ещё в старте" отвечала бы отказом "модель не запущена".
    if let Some(name) = model.borrow().name.clone() {
        ctx_rc.borrow().set_model_state(&name, &start_name);
    }

    Ok(Unit::from_kind(UnitKind::Node {
        time_ns: 0,
        ticks_in_state: 0,
        state_entered_ns: 0,
        entered_initial: false,
        exited_terminal: false,
        model_name: model.borrow().name.clone(),
        context: Some(ctx_rc),
        state_transitions,
        state_executions,
        state_every,
        state_impls,
        every_consumed: Vec::new(),
        state: Some(start_name),
        executions,
        guards,
        invariant_violations: Vec::new(),
        last_transition: None,
    }))
}

/// Строит проверяемые обязательства из формул. `Formula::Guard` -> предикат условия +
/// имя инварианта; `Formula::LTL` - статика, симулятором игнорируется (проверяется
/// верификатором, не здесь); `None`/`Formulas` разворачиваются/пропускаются.
fn build_guards(formulas: &[takt_lang::semantic::formula::Formula]) -> Vec<crate::unit::Guard> {
    use takt_lang::semantic::formula::Formula;
    let mut out = Vec::new();
    for f in formulas {
        match f {
            Formula::Guard(cond, name, _) => out.push((create_predicate(cond), name.clone())),
            Formula::Formulas(inner) => out.extend(build_guards(inner)),
            Formula::LTL(_, _) | Formula::None => {}
        }
    }
    out
}

fn build_transitions(state: &StateNode) -> Result<Vec<(String, Predicate)>, Diagnostic> {
    let to_transition = |r: &ReferenceNode<StateNode>| {
        let pred = if r.cond.is_unconditional() {
            Predicate::new("Always", |_| Ok(true))
        } else {
            create_predicate(&r.cond)
        };
        (r.name.clone(), pred)
    };
    let mut out: Vec<(String, Predicate)> = state.references().iter().map(to_transition).collect();
    // Переход `next` состояния-реализации. Живёт отдельным полем
    // `StateNode::Implement::next`, а не среди `references`, и потому прежде в переходы
    // симулятора не попадал вовсе: `start P = A + B { next Done; }` застревал в `P`
    // навсегда.
    //
    // Идёт последним: `next` безусловен, и впереди `ref`-рёбер он затенил бы их все.
    // Проверяется он лишь после того, как реализация состояния завершилась
    // (`tick_node`, шаг 1a) - эталон цели `c`, где `generate_extend_transition` эмитит
    // переход внутри ветви `is_done`.
    if let StateNode::Implement { next: Some(r), .. } = state {
        out.push(to_transition(r));
    }
    Ok(out)
}

// -- Unit из Extend ------------------------------------------------------------

fn build_extend(
    extend: &Extend,
    shared_parent: Option<Rc<RefCell<dyn Context>>>,
) -> Result<Unit, Diagnostic> {
    match extend {
        Extend::None | Extend::Unresolved(_) => Ok(Unit::default()),
        // Аргументы инстанцирования: значения параметров этого экземпляра. Пустой
        // список - вызов без аргументов, поведение прежнее.
        Extend::Model(rc, loc, args) => build_impl(Rc::clone(rc), shared_parent, args, *loc),
        Extend::Parentless(inner) => build_extend(inner, shared_parent),
        Extend::Concatenation(items) => {
            // Шаги `+` делят общий родительский контекст ровно так же, как ветви `|`.
            let shared = shared_context(shared_parent, items);
            items.iter().try_fold(Unit::default(), |acc, item| {
                Ok(acc.add(&build_extend(item, shared.clone())?))
            })
        }
        Extend::Parallel(items) => {
            // Все параллельные подмодели разделяют один общий родительский контекст -
            // это позволяет передавать shared-переменные (busy, tgt_*, lift_*) между
            // ними.
            let shared = shared_context(shared_parent, items);
            items.iter().try_fold(Unit::default(), |acc, item| {
                Ok(acc.union(&build_extend(item, shared.clone())?))
            })
        }
    }
}

/// Общий родительский контекст ветвей композиции: переданный сверху либо, если его нет,
/// свежий контекст родительской модели первой ветви.
///
/// Одна функция на `|` и `+` намеренно: разъехавшись, они дали бы разную видимость
/// переменных для двух форм композиции одного языка - ровно тот дефект, что закрывает.
fn shared_context(
    shared_parent: Option<Rc<RefCell<dyn Context>>>,
    items: &[Box<Extend>],
) -> Option<Rc<RefCell<dyn Context>>> {
    if shared_parent.is_some() {
        return shared_parent;
    }
    items
        .first()
        .and_then(|first| extract_parent_model(first))
        .map(|parent_model| {
            Rc::new(RefCell::new(ModelNodeContext::new(parent_model))) as Rc<RefCell<dyn Context>>
        })
}

/// Извлекает родительскую модель из Extend (нужна для построения shared-контекста).
fn extract_parent_model(extend: &Extend) -> Option<Rc<RefCell<ModelNode>>> {
    match extend {
        Extend::Model(rc, _, _) => rc.borrow().upper.as_ref()?.upgrade(),
        Extend::Parentless(inner) => extract_parent_model(inner),
        _ => None,
    }
}

// -- Тесты --------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use takt_lang::parse;
    use takt_lang::semantic::tree::construct_model;

    // -- ModelNodeContext ------------------------------------------------------

    #[test]
    fn test_model_node_context_missing_returns_none() {
        let model = Rc::new(RefCell::new(ModelNode::default()));
        let ctx = ModelNodeContext::new(model);
        assert!(ctx.get_value("x").is_none());
    }

    #[test]
    fn test_model_node_context_number_var() {
        let (ast, _) = parse("var x: u8 := 42;", 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let ctx = ModelNodeContext::new(Rc::clone(&model_rc));
        assert!(matches!(ctx.get_value("x"), Some(Value::Number(42))));
    }

    #[test]
    fn test_model_node_context_bool_var() {
        let (ast, _) = parse("var flag: bool := false;", 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let ctx = ModelNodeContext::new(Rc::clone(&model_rc));
        assert!(matches!(ctx.get_value("flag"), Some(Value::Boolean(false))));
    }

    #[test]
    fn test_model_node_context_cache_takes_priority() {
        let (ast, _) = parse("var x: u8 := 10;", 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let ctx = ModelNodeContext::new(Rc::clone(&model_rc));
        assert!(matches!(ctx.get_value("x"), Some(Value::Number(10))));
        ctx.cache
            .borrow_mut()
            .insert("x".to_string(), Value::Number(99));
        assert!(matches!(ctx.get_value("x"), Some(Value::Number(99))));
        assert!(model_rc.borrow().variables.contains_key("x"));
    }

    #[test]
    fn test_model_node_context_parent_hierarchy() {
        let (ast, _) = parse("var outer_var: u8 := 77; model Inner { start S; }", 0).unwrap();
        let root_rc = construct_model(&ast, None, &[]).unwrap();
        let inner_rc = root_rc.borrow().search_model("Inner").unwrap();
        let ctx = ModelNodeContext::new(Rc::clone(&inner_rc));
        let val = ctx.get_value("outer_var");
        assert!(
            matches!(val, Some(Value::Number(77))),
            "переменная из родительской модели должна быть доступна через иерархию контекста"
        );
    }

    #[test]
    fn test_model_node_context_set_value_delegates_to_parent() {
        let (ast, _) = parse("var shared: u8 := 0; model Inner { start S; }", 0).unwrap();
        let root_rc = construct_model(&ast, None, &[]).unwrap();
        let inner_rc = root_rc.borrow().search_model("Inner").unwrap();

        // Создаём shared parent context (как при построении Parallel)
        let shared_parent: Rc<RefCell<dyn Context>> =
            Rc::new(RefCell::new(ModelNodeContext::new(Rc::clone(&root_rc))));

        let mut inner_ctx =
            ModelNodeContext::new_with_parent(inner_rc, Some(shared_parent.clone()));

        // Записываем через inner_ctx - должно делегироваться в shared_parent
        inner_ctx.set_value("shared", Value::Number(42));

        // Читаем через shared_parent - должно вернуть 42
        assert!(
            matches!(
                shared_parent.borrow().get_value("shared"),
                Some(Value::Number(42))
            ),
            "set_value должен делегировать в shared parent для не-локальных переменных"
        );
    }

    // -- eval_expr -------------------------------------------------------------

    #[test]
    fn test_eval_expr_number() {
        assert!(matches!(
            eval_expr(&ExpressionNode::Number(7)),
            Some(Value::Number(7))
        ));
    }

    #[test]
    fn test_eval_expr_bool() {
        assert!(matches!(
            eval_expr(&ExpressionNode::Bool(true)),
            Some(Value::Boolean(true))
        ));
    }

    #[test]
    fn test_eval_expr_negate() {
        assert!(matches!(
            eval_expr(&ExpressionNode::Negate(Box::new(ExpressionNode::Number(5)))),
            Some(Value::Number(-5))
        ));
    }

    #[test]
    fn test_eval_expr_none_returns_none() {
        assert!(eval_expr(&ExpressionNode::None).is_none());
    }

    // -- build -----------------------------------------------------------------

    #[test]
    fn test_build_empty_model_returns_none() {
        let model = Rc::new(RefCell::new(ModelNode::default()));
        assert!(matches!(build(model).unwrap().kind(), UnitKind::None));
    }

    #[test]
    fn test_build_single_state_model() {
        let (ast, _) = parse("start S;", 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let result = build(model_rc).unwrap();
        let UnitKind::Node { state: Some(s), .. } = result.kind() else {
            panic!("ожидался Unit::Node");
        };
        assert_eq!(s, "S");
    }

    #[test]
    fn test_build_model_without_start_state_returns_err() {
        let mut model = ModelNode::default();
        model.states.insert(
            "S".to_string(),
            StateNode::Simple {
                upper: None,
                loc: Location::Implicit,
                named_blocks: vec![],
                name: "S".to_string(),
                references: vec![],
                kind: StateNodeKind::Simple,
                formulas: vec![],
            },
        );
        assert!(build(Rc::new(RefCell::new(model))).is_err());
    }

    #[test]
    fn test_build_unconditional_transition() {
        let (ast, _) = parse("start A { ref B; } state B;", 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let result = build(model_rc).unwrap();
        let UnitKind::Node {
            state_transitions, ..
        } = result.kind()
        else {
            panic!("ожидался Unit::Node");
        };
        let trans = state_transitions.get("A").unwrap();
        assert_eq!(trans.len(), 1);
        assert_eq!(trans[0].0, "B");
        assert!(trans[0].1.evaluate(&mut Unit::default()).unwrap());
    }

    #[test]
    fn test_build_node_has_context_with_variable() {
        let (ast, _) = parse("var x: u8 := 5; start S;", 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let result = build(model_rc).unwrap();
        assert!(matches!(result.get_value("x"), Some(Value::Number(5))));
    }

    /// 0032: у узла нет собственной карты значений - запись через `set_value` уходит в
    /// контекст модели и читается оттуда же (единый источник истины).
    #[test]
    fn test_build_node_set_value_routes_to_context() {
        let (ast, _) = parse("var x: u8 := 5; start S;", 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let mut result = build(model_rc).unwrap();
        assert!(matches!(result.get_value("x"), Some(Value::Number(5))));
        result.set_value("x", Value::Number(99));
        assert!(matches!(result.get_value("x"), Some(Value::Number(99))));
    }

    #[test]
    fn test_build_concatenation_produces_sequential() {
        let src = "model A { start S; } model B { start S; } start Entry = A + B;";
        let (ast, _) = parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let entry = model_rc.borrow().search_state("Entry").unwrap();
        let entry_ref = entry.borrow();
        let StateNode::Implement { implements, .. } = &*entry_ref else {
            panic!("Entry должен быть Implement");
        };
        let result = build_extend(implements, None).unwrap();
        let UnitKind::Sequential { units, .. } = result.kind() else {
            panic!("ожидался Unit::Sequential");
        };
        assert_eq!(units.len(), 2);
    }

    #[test]
    fn test_build_parallel_produces_parallel() {
        let src = "model A { start S; } model B { start S; } start Entry = A | B;";
        let (ast, _) = parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let entry = model_rc.borrow().search_state("Entry").unwrap();
        let entry_ref = entry.borrow();
        let StateNode::Implement { implements, .. } = &*entry_ref else {
            panic!("Entry должен быть Implement");
        };
        let result = build_extend(implements, None).unwrap();
        let UnitKind::Parallel { units, .. } = result.kind() else {
            panic!("ожидался Unit::Parallel");
        };
        assert_eq!(units.len(), 2);
    }

    /// Enter-блок корректно записывает переменную через shared-контекст.
    #[test]
    fn test_build_enter_block_executes_on_transition() {
        let src = "var x: u8 := 0; start A { ref B; } state B { enter { x := 99; } }";
        let (ast, _) = parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let mut unit = build(model_rc).unwrap();

        // До тика: x = 0 (умолчание)
        assert!(matches!(unit.get_value("x"), Some(Value::Number(0))));

        // Тик: A -> B (enter { x = 99; } должен выполниться)
        unit.tick();

        // После тика: x = 99
        assert!(
            matches!(unit.get_value("x"), Some(Value::Number(99))),
            "enter-блок должен установить x=99 при входе в состояние B"
        );
    }

    /// Shared-переменные доступны между параллельными моделями.
    #[test]
    fn test_build_parallel_shared_variable() {
        // Модель A устанавливает shared в 1 при входе в B. Модель B читает shared через
        // свой контекст.
        let src = r#"
            var shared: u8 := 0;
            model A { start Idle { ref Done; } state Done { enter { shared := 7; } } }
            model B { start Check { ref End: shared = 7; } state End; }
            start Root = A | B;
        "#;
        let (ast, _) = parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let mut unit = build(model_rc).unwrap();

        // Тик 1: A: Idle->Done (enter shared=7), B: Check (shared=7 -> End) A тикает
        // первым, B тикает вторым и видит shared=7
        unit.tick();

        // shared должен быть 7
        assert!(
            matches!(unit.get_value("shared"), Some(Value::Number(7))),
            "shared-переменная должна быть доступна между параллельными моделями"
        );
    }

    #[test]
    fn test_enter_writes_output_port_via_context() {
        let src = r#"
            out cmd_ack: bit;
            in task_valid: bit;
            var busy: bit := 0;
            model CR {
                start Waiting { ref Accepting: task_valid; }
                state Accepting { enter { cmd_ack := 1; busy := 1; } next Done; }
                state Done;
            }
            start Main = CR;
        "#;
        let (ast, _) = parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let mut unit = build(model_rc).unwrap();

        // Устанавливаем task_valid=1
        unit.set_value("task_valid", Value::Number(1));

        // До тика: cmd_ack должен быть 0 (умолчание)
        assert!(
            matches!(unit.get_value("cmd_ack"), Some(Value::Number(0)) | None),
            "cmd_ack должен быть 0 до тика, получено {:?}",
            unit.get_value("cmd_ack")
        );

        // Тик: Waiting->Accepting (enter: cmd_ack=1, busy=1)
        unit.tick();

        // После тика: cmd_ack = 1
        assert!(
            matches!(unit.get_value("cmd_ack"), Some(Value::Number(1))),
            "cmd_ack должен быть 1 после тика, получено {:?}",
            unit.get_value("cmd_ack")
        );
        assert!(
            matches!(unit.get_value("busy"), Some(Value::Number(1))),
            "busy должен быть 1 после тика, получено {:?}",
            unit.get_value("busy")
        );
    }

    /// Параллельная модель с Address-портами: enter-блок CR пишет cmd_ack=1.
    #[test]
    fn test_parallel_address_port_enter_write() {
        let src = r#"
            out cmd_ack: bit at 0x600:0;
            in task_valid: bit at 0x100:0;
            var busy: bit := 0;
            model CR {
                start Waiting { ref Accepting: task_valid; }
                state Accepting { enter { cmd_ack := 1; busy := 1; } }
            }
            model MC {
                start Idle { ref Active: busy; }
                state Active;
            }
            start Main = CR | MC;
        "#;
        let (ast, _) = parse(src, 0).unwrap();
        let model_rc = construct_model(&ast, None, &[]).unwrap();
        let mut unit = build(model_rc).unwrap();

        unit.set_value("task_valid", Value::Number(1));
        unit.tick();

        assert!(
            matches!(unit.get_value("cmd_ack"), Some(Value::Number(1))),
            "cmd_ack должен быть 1 после тика, получено {:?}",
            unit.get_value("cmd_ack")
        );
        assert!(
            matches!(unit.get_value("busy"), Some(Value::Number(1))),
            "busy должен быть 1 после тика, получено {:?}",
            unit.get_value("busy")
        );
    }
}

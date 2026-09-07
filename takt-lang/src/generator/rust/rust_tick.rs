//! Эмиссия такта автомата в цель Rust: `_tick` и переходы.
//!
//! Здесь - тело `tick`: диспетчеризация состояний, вход в стартовое, охранные формулы и
//! все виды переходов (простые и составные `= Модель`, `|`, `+`).

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::rust::rust_blocks::{emit_model_named_blocks, emit_named_blocks};
use crate::generator::rust::rust_chain::{
    concat_steps, seq_enum_name, seq_field_name, step_prefix,
};
use crate::generator::rust::rust_ctx::{ModelEmit, StateEmit};
use crate::generator::rust::rust_expr::{Scope, condition_as_bool, unwrap_outer};
use crate::generator::rust::rust_map::RustMap;
use crate::generator::rust::rust_model::{Instance, StateTable, needs_hal, submodel_name};
use crate::generator::rust::rust_name::rust_value_name;
use crate::generator::rust::rust_shared::{shared_type_name, shared_variables};
use crate::generator::rust::rust_stmt::StmtOutput;
use crate::semantic::minimap::{Element, Name, StateExtend};
use crate::semantic::{Formula, StateNode};
use std::collections::BTreeSet;

pub(crate) fn emit_tick(
    p: &mut Printer,
    ctx: &ModelEmit,
    warnings: &mut Vec<Diagnostic>,
) -> Result<(), Diagnostic> {
    // Снимок распаковывается в те же имена, что были параметрами: тело печатника не
    // менялось.
    let (map, name, model, element, table, instances, start, states, shared) = (
        ctx.map,
        ctx.name,
        ctx.model,
        ctx.element,
        ctx.table,
        ctx.instances,
        ctx.start,
        ctx.states,
        ctx.shared,
    );
    let (is_root, uses_hal, ports) = (ctx.is_root, ctx.uses_hal, ctx.ports);
    // Поле `hal` есть только у корня; под-модель получает HAL параметром. Параметр
    // даётся, только если он действительно нужен: неиспользуемый параметр - такое же
    // `-D warnings`, как неиспользуемое поле. Признак считается на функцию: HAL, нужный
    // только начальному значению порта, в такте не используется, и `rustc` под
    // `-D warnings` отвечает "unused variable: `hal`". Тот же приём у цели `c`.
    let needs_hal_param = !is_root
        && uses_hal
        && crate::generator::rust::rust_model::needs_hal_in_tick(
            map,
            ctx.name,
            &mut BTreeSet::new(),
        );
    // Под-модель получает общие переменные одним параметром `&mut Shared`, а не по
    // одному. Корень владеет полем `self.shared` и параметра не получает. `hal` -
    // Последним.
    let needs_shared_param = !is_root && !shared.is_empty();
    let mut params = String::new();
    if needs_shared_param {
        params.push_str(&format!(", shared: &mut {}", shared_type_name(map)));
    }
    if needs_hal_param {
        params.push_str(", hal: &mut H");
    }
    let generics = if needs_hal_param { "<H: Hal>" } else { "" };
    let vis = if is_root { "pub " } else { "" };
    let _ = ports;

    let hal_access = match (uses_hal, is_root) {
        (false, _) => "",
        (true, true) => "self.hal",
        (true, false) => "hal",
    };
    // Изменяемость локальных `var` считается по всем телам модели заранее: печать
    // потоковая, и в точке `let` будущие присваивания ещё не видны.
    let mut assigned = BTreeSet::new();
    for state_name in states {
        if let Ok(raw) = map.raw_state_at(state_name.clone()) {
            for block in raw.borrow().named_blocks() {
                if let Some(stmt) = block.statement() {
                    crate::generator::rust::rust_assigned::collect_assigned(stmt, &mut assigned);
                }
            }
        }
    }
    // присваивания из model-level `always` тоже участвуют в расчёте мутабельности
    // локальных `let` (печать потоковая - будущие записи не видны).
    for block in model.get_named_blocks("always") {
        if let Some(stmt) = block.statement() {
            crate::generator::rust::rust_assigned::collect_assigned(stmt, &mut assigned);
        }
    }
    let mut scope = Scope {
        model,
        shared: shared.iter().map(|(n, _)| n.clone()).collect(),
        // Композирующая модель (корень) владеет полем `self.shared`; под-модель
        // получает `Shared` параметром -> `shared.x`.
        shared_via_self: is_root,
        locals: Vec::new(),
        // По ссылке в методах модели ничего не приходит: массивы там - поля.
        by_ref: Vec::new(),
        assigned,
        hal: hal_access.to_string(),
        has_self: true,
        // У корня `hal` - поле `H`, у под-модели - параметр `&mut H`.
        hal_is_ref: !is_root,
        // Карта "под-модель -> поле" для спецформы `S(Модель) = Состояние`.
        instances: instances
            .iter()
            .flat_map(|(_, list)| list.iter())
            .map(|i| (i.unique.clone(), i.field.clone()))
            .collect(),
        time_profile: map.time_profile(),
        return_type: None,
        // Подсказка о приёмнике степени ставится в `coerce_to`.
        power_target: None,
        guard_enable: map.guard_enable(),
    };

    // Параметров такта теперь всегда <= 3 (`self` + `&mut Shared?` + `&mut H?`): общие
    // переменные свёрнуты в одну структуру, поэтому заглушки
    // `#[allow(clippy::too_many_arguments)]` больше нет - политика (а) без исключений.
    p.ident(&format!(
        "{}fn tick{}(&mut self{}) {{",
        vis, generics, params
    ))
    .nl();
    p.up();

    let mut out = StmtOutput::default();

    // Guard-формулы модели - первыми, как в цели `c`.
    if map.guard_enable() {
        for formula in &model.formulas {
            emit_guard(p, formula, &scope)?;
        }
    }

    // -- Вход в стартовое состояние -------------------------------------------
    //
    // `if` до `match`, без выхода из такта: вход в стартовое состояние не расходует
    // такт. У цели `c` того же добивается отсутствие `break`, здесь - `match`, читающий
    // свежезаписанное `self.state`.
    let start_raw = map.raw_state_at(start.clone())?;
    p.ident(&format!("if self.state == {}::Init {{", table.enum_name))
        .nl();
    p.up();
    emit_named_blocks(p, &start_raw.borrow(), "enter", &mut scope, &mut out)?;
    p.ident(&format!("self.state = {};", table.path_of(start)?))
        .nl();
    // Латч метки времени входа в стартовое состояние: отсчёт выдержки - от входа "до
    // такта 1", а не от нуля абсолютного времени.
    crate::generator::rust::rust_time::emit_first_entry_latch(p, map, model, hal_access);
    p.down();
    p.ident("}").nl();

    // model-level `always` (вне состояния) - каждый такт до `match`, безусловно по
    // состоянию (эталон - шаг 2 `execution("always")` симулятора).
    emit_model_named_blocks(p, model, "always", &mut scope, &mut out)?;

    // -- Разбор состояний -----------------------------------------------------
    p.ident("match self.state {").nl();
    p.up();
    for state_name in states {
        let Some(state) = map.state_at(state_name.clone()) else {
            continue;
        };
        let raw = map.raw_state_at(state_name.clone())?;
        let raw = &*raw.borrow();
        p.ident(&format!("{} => {{", table.path_of(state_name)?))
            .nl();
        p.up();

        if map.guard_enable() {
            for formula in raw.formulas() {
                emit_guard(p, formula, &scope)?;
            }
        }
        emit_named_blocks(p, raw, "always", &mut scope, &mut out)?;
        // Периодические блоки `every` - после `always`, как в симуляторе
        // (`execute_every` следом за `execution("always")`).
        crate::generator::rust::rust_every::emit_state_body(
            p,
            ctx,
            state_name.local(),
            hal_access,
            &mut scope,
            &mut out,
        )?;

        match &state {
            // Табличная форма: переходы простого состояния - строки таблицы, и в теле
            // ветви `match` их не печатается вовсе.
            Element::State { .. } if map.fsm_table() => {}
            Element::State { .. } => {
                let emitted = emit_transitions(p, raw, map, table, states, &mut scope, &mut out)?;
                // Терминальное состояние: переходов нет - уходим в End. Состояние, само
                // являющееся End, самоперехода не получает.
                if raw.is_terminated() && !emitted && table.variant_of(state_name)? != "End" {
                    emit_named_blocks(p, raw, "exit", &mut scope, &mut out)?;
                    p.ident(&format!("self.state = {};", table.end_path())).nl();
                }
            }
            Element::StateExtend { extend, next, .. } => {
                emit_extend(
                    p,
                    ctx,
                    &StateEmit {
                        name: state_name,
                        raw,
                        extend,
                        next,
                    },
                    &mut scope,
                    &mut out,
                )?;
            }
            Element::Model { .. } => {
                return Err(Diagnostic::error(
                    Location::Codegen,
                    "Модель в позиции состояния".to_string(),
                )
                .with_code("RS-012"));
            }
        }
        p.down();
        p.ident("}").nl();
    }
    if table.emit_end {
        p.ident(&format!("{} => {{}}", table.end_path())).nl();
    }
    p.ident(&format!("{}::Init => {{}}", table.enum_name)).nl();
    p.down();
    p.ident("}").nl();

    // Табличная форма: переходы просматривает диспетчер - После тел состояний и до
    // обновления счётчика выдержки, ровно там, где стоял переход внутри ветви `match`.
    if map.fsm_table() {
        crate::generator::rust::rust_table::emit_dispatch_call(p, ctx)?;
    }

    // Обновление счётчика/метки времени в конце такта: одним сравнением с
    // `takt_prev_state`, как `c_time::emit_state_time_update`.
    crate::generator::rust::rust_time::emit_tick_update(p, map, model, hal_access)?;

    p.down();
    p.ident("}").nl().nl();

    // Табличная форма: стражи, действия и диспетчер печатаются в том же `impl` сразу за
    // тактом - им нужен тот же контекст печати (`scope`), что и телу такта, и второго
    // его построения быть не должно.
    if map.fsm_table() {
        crate::generator::rust::rust_table::emit_methods(p, ctx, &mut scope, warnings)?;
    }

    let _ = (name, element);
    warnings.append(&mut out.warnings);
    Ok(())
}

/// Печатает guard-формулу как `assert!`.
///
/// Имя инварианта попадает в сообщение - в цели `c` генератор его игнорирует, здесь оно
/// бесплатно и полезно.
pub(crate) fn emit_guard(
    p: &mut Printer,
    formula: &Formula,
    scope: &Scope,
) -> Result<(), Diagnostic> {
    match formula {
        Formula::Guard(cond, label, _) => {
            let text = condition_as_bool(cond, scope)?;
            let text = unwrap_outer(&text);
            match label {
                Some(label) => {
                    p.ident(&format!(
                        "assert!({}, \"нарушен инвариант '{}'\");",
                        text, label
                    ))
                    .nl();
                }
                None => {
                    p.ident(&format!("assert!({});", text)).nl();
                }
            }
            Ok(())
        }
        Formula::Formulas(items) => {
            for item in items {
                emit_guard(p, item, scope)?;
            }
            Ok(())
        }
        // LTL описывает бесконечные прогоны - предмет `taktc verify`, а не прошивки.
        Formula::LTL(_, _) | Formula::None => Ok(()),
    }
}

/// Печатает переходы состояния. Возвращает `true`, если эмитирован безусловный.
///
/// Цепочка `if`/`else if` вместо C-шного "`if (c) {...break;}` подряд": `break` в C
/// означает "такт окончен", то есть следующие `if` при сработавшем первом недостижимы.
/// Семантика та же, но недостижимого кода нет - а он валит проверка
fn emit_transitions(
    p: &mut Printer,
    raw: &StateNode,
    map: &RustMap,
    table: &StateTable,
    states: &[Name],
    scope: &mut Scope,
    out: &mut StmtOutput,
) -> Result<bool, Diagnostic> {
    // Соседние рёбра в одно и то же состояние сливаются в одно условие через `||`.
    // Слияние семантику сохраняет - при одинаковых телах "первое сработавшее"
    // неотличимо от дизъюнкции.
    //
    // Сливаются только соседние: между рёбрами в разные состояния порядок значим
    // (первое сработавшее выигрывает), и переставлять их нельзя. Реальный случай -
    // `LiftOperating` в `stacker.takt`: захват и укладка ведут в одно `LiftDone` по
    // разным условиям.
    let mut edges: Vec<(Name, Vec<&crate::semantic::ReferenceNode<StateNode>>)> = Vec::new();
    for reference in raw.references() {
        let Some(target) = states.iter().find(|n| n.local() == reference.name).cloned() else {
            continue;
        };
        if map.state_at(target.clone()).is_none() {
            continue;
        }
        match edges.last_mut() {
            Some((last, group)) if last.unique() == target.unique() => group.push(reference),
            _ => edges.push((target, vec![reference])),
        }
    }

    let mut first = true;
    for (target, group) in edges {
        let reference = group[0];
        // Решение "ребро безусловно" - у одного носителя: пять копий этого правила уже
        // разъехались, и цель `rust` считала безусловным ещё и `Unresolved`, то есть
        // условное ребро срабатывало всегда - валидный вывод, другой автомат. Переход
        // объявляет своё место: условие ребра - не оператор, и отказ печатника
        // выражений приходил без координаты.
        crate::generator::site::enter(reference.location);
        let unconditional = group.iter().any(|r| r.cond.is_unconditional());
        if unconditional {
            // Безусловный переход. Всё, что за ним, недостижимо - в C это молча, в Rust
            // валит `-D warnings`. Поэтому эмиссия рёбер прекращается.
            if first {
                emit_named_blocks(p, raw, "exit", scope, out)?;
                emit_enter_of(p, map, &target, scope, out)?;
                p.ident(&format!("self.state = {};", table.path_of(&target)?))
                    .nl();
            } else {
                p.ident("} else {").nl();
                p.up();
                emit_named_blocks(p, raw, "exit", scope, out)?;
                emit_enter_of(p, map, &target, scope, out)?;
                p.ident(&format!("self.state = {};", table.path_of(&target)?))
                    .nl();
                p.down();
                p.ident("}").nl();
            }
            return Ok(true);
        }
        // Условия группы объединяются через `||` - тела у них совпадают.
        let mut parts = Vec::new();
        for r in &group {
            parts.push(condition_as_bool(&r.cond, scope).map_err(|di| {
                Diagnostic::error_with_note(
                    r.location,
                    format!(
                        "условный переход в состояние '{}' не переводится в Rust: {}",
                        target.local(),
                        di.message
                    ),
                    di.loc,
                    match &di.code {
                        Some(code) => format!("причина [{}]: {}", code, di.message),
                        None => format!("причина: {}", di.message),
                    },
                )
                .with_code("RS-020")
            })?);
        }
        let cond = if parts.len() == 1 {
            parts.remove(0)
        } else {
            format!("({})", parts.join(" || "))
        };
        let _unused = |di: Diagnostic| -> Diagnostic {
            Diagnostic::error_with_note(
                reference.location,
                format!(
                    "условный переход в состояние '{}' не переводится в Rust: {}",
                    target.local(),
                    di.message
                ),
                di.loc,
                match &di.code {
                    Some(code) => format!("причина [{}]: {}", code, di.message),
                    None => format!("причина: {}", di.message),
                },
            )
            .with_code("RS-020")
        };
        let head = if first { "if" } else { "} else if" };
        first = false;
        p.ident(&format!("{} {} {{", head, unwrap_outer(&cond)))
            .nl();
        p.up();
        emit_named_blocks(p, raw, "exit", scope, out)?;
        emit_enter_of(p, map, &target, scope, out)?;
        p.ident(&format!("self.state = {};", table.path_of(&target)?))
            .nl();
        p.down();
    }
    if !first {
        p.ident("}").nl();
    }
    Ok(false)
}

/// Печатает блоки `enter` целевого состояния перехода.
fn emit_enter_of(
    p: &mut Printer,
    map: &RustMap,
    target: &Name,
    scope: &mut Scope,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    let raw = map.raw_state_at(target.clone())?;
    emit_named_blocks(p, &raw.borrow(), "enter", scope, out)
}

/// Печатает такт составного состояния (`= Модель`, `A | B`, `A + B`).
fn emit_extend(
    p: &mut Printer,
    ctx: &ModelEmit,
    state: &StateEmit,
    scope: &mut Scope,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    let (map, instances) = (ctx.map, ctx.instances);
    let (state_name, extend) = (state.name, state.extend);
    if !instances
        .iter()
        .any(|(n, _)| n.unique() == state_name.unique())
    {
        return Ok(());
    }
    let prefix = state_name.local_lowercase_snakecase();
    let done = emit_node_tick(p, ctx, state, extend, &mut Vec::new(), &prefix, scope, out)?;
    // У цепочки состояния переход печатает её последний шаг - здесь остаётся только
    // параллель и одиночная модель.
    if matches!(extend, StateExtend::Concatenation(_)) || done.is_empty() {
        return Ok(());
    }
    // Табличная форма: внешний переход печатает таблица - здесь остаётся только тик
    // ветвей.
    if map.fsm_table() {
        return Ok(());
    }
    p.ident(&format!("if {} {{", done.join(" && "))).nl();
    p.up();
    emit_extend_transition(p, ctx, state.raw, state.next, scope, out)?;
    p.down();
    p.ident("}").nl();
    Ok(())
}

/// Печатает такт узла композиции и возвращает условия его готовности.
///
/// Рекурсия идёт по дереву, а место узла задаёт `path` - тот же адрес, каким цепочки
/// пользуются у целей `c`, `st` и `sv` ([`chain_site`](crate::generator::chain_site)).
/// Цепочка глубже (`((A + B) | C) + E`) машины шагов не получала, и её ветви тикали
/// разом - валидный Rust с другим автоматом.
#[allow(clippy::too_many_arguments)]
fn emit_node_tick(
    p: &mut Printer,
    ctx: &ModelEmit,
    state: &StateEmit,
    node: &StateExtend,
    path: &mut Vec<usize>,
    prefix: &str,
    scope: &mut Scope,
    out: &mut StmtOutput,
) -> Result<Vec<String>, Diagnostic> {
    match node {
        StateExtend::None => Ok(Vec::new()),
        // Одиночная под-модель: тикает всегда, готовность - её собственная.
        StateExtend::Model(_, _) => {
            let field = rust_value_name(prefix, Location::Codegen)?;
            let instance = instance_at(ctx, state.name, &field)?;
            let args = call_args(ctx.map, instance, scope, ctx.is_root)?;
            p.ident(&format!("self.{}.tick({});", field, args)).nl();
            Ok(vec![format!("self.{}.is_done()", field)])
        }
        // Параллельная композиция: тикают все ветви каждый такт, в порядке объявления -
        // как в цели `c` (ветвь, уже завершённая, тикает в свой `End` вхолостую).
        // Порядок обязан совпадать с C, иначе потактовая сверка разъедется.
        StateExtend::Parallel(items) => {
            let mut done = Vec::new();
            for (idx, item) in items.iter().enumerate() {
                let sub = step_prefix(prefix, item, idx);
                path.push(idx);
                let result = emit_node_tick(p, ctx, state, item, path, &sub, scope, out);
                path.pop();
                done.extend(result?);
            }
            Ok(done)
        }
        // Последовательная композиция: шаги идут по очереди, внутри шага (параллельная
        // группа) - одновременно.
        StateExtend::Concatenation(items) => {
            emit_chain_tick(p, ctx, state, items, path, prefix, scope, out)
        }
    }
}

/// Печатает машину шагов одной цепочки и возвращает условие её готовности.
///
/// Форма одна у цепочки состояния и у вложенной; отличается только конец последнего
/// шага: состояние уходит переходом, вложенная цепочка ставит терминальный вариант
/// `Done` - вмещающая параллель обязана узнать, что ветвь кончилась, а выхода из
/// состояния у неё нет.
#[allow(clippy::too_many_arguments)]
fn emit_chain_tick(
    p: &mut Printer,
    ctx: &ModelEmit,
    state: &StateEmit,
    items: &[StateExtend],
    path: &mut Vec<usize>,
    prefix: &str,
    scope: &mut Scope,
    out: &mut StmtOutput,
) -> Result<Vec<String>, Diagnostic> {
    let steps = concat_steps(items, prefix, state.name)?;
    if steps.is_empty() {
        return Ok(Vec::new());
    }
    let nested = !path.is_empty();
    let field = seq_field_name(state.name, path)?;
    let seq = seq_enum_name(ctx.name, state.name, path)?;
    // Индекс шага в исходном списке: пустые элементы шагами не становятся, а путь
    // адресует дерево - расхождение сдвинуло бы имена полей.
    let sites: Vec<usize> = items
        .iter()
        .enumerate()
        .filter(|(_, item)| !matches!(item, StateExtend::None))
        .map(|(idx, _)| idx)
        .collect();
    for (order, step) in steps.iter().enumerate() {
        let head = if order == 0 { "if" } else { "} else if" };
        p.ident(&format!(
            "{} self.{} == {}::{} {{",
            head, field, seq, step.variant
        ))
        .nl();
        p.up();
        path.push(sites[order]);
        let result = emit_node_tick(p, ctx, state, &step.node, path, &step.prefix, scope, out);
        path.pop();
        let done = result?;
        if done.is_empty() {
            p.down();
            continue;
        }
        p.ident(&format!("if {} {{", done.join(" && "))).nl();
        p.up();
        match steps.get(order + 1) {
            // Передача хода следующему шагу стоит такта - как в C, где за установкой
            // варианта стоит `break`. Здесь такт кончается сам: цепочка `else if` уже
            // вошла в эту ветвь, и следующий шаг в этом же такте не тикнет.
            Some(next_step) => {
                for instance in &next_step.instances {
                    p.ident(&format!("self.{}.init();", instance.field)).nl();
                }
                p.ident(&format!("self.{} = {}::{};", field, seq, next_step.variant))
                    .nl();
            }
            // Последний шаг завершён. Вложенная цепочка объявляет себя готовой, цепочка
            // состояния уходит из него; в табличной форме этот переход печатает
            // таблица.
            None if nested => {
                p.ident(&format!("self.{} = {}::Done;", field, seq)).nl();
            }
            None if ctx.map.fsm_table() => {}
            None => emit_extend_transition(p, ctx, state.raw, state.next, scope, out)?,
        }
        p.down();
        p.ident("}").nl();
        p.down();
    }
    p.ident("}").nl();
    if nested {
        return Ok(vec![format!("self.{} == {}::Done", field, seq)]);
    }
    Ok(Vec::new())
}

/// Экземпляр состояния по имени поля - аргументы вызова живут в нём.
fn instance_at<'a>(
    ctx: &'a ModelEmit,
    state: &Name,
    field: &str,
) -> Result<&'a Instance, Diagnostic> {
    ctx.instances
        .iter()
        .find(|(n, _)| n.unique() == state.unique())
        .and_then(|(_, list)| list.iter().find(|i| i.field == field))
        .ok_or_else(|| {
            Diagnostic::error(
                Location::Codegen,
                format!(
                    "Экземпляр '{}' состояния '{}' не найден",
                    field,
                    state.local()
                ),
            )
            .with_code("RS-012")
        })
}

/// Печатает переход по завершении реализации состояния.
fn emit_extend_transition(
    p: &mut Printer,
    ctx: &ModelEmit,
    raw: &StateNode,
    next: &Name,
    scope: &mut Scope,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    let (map, table, states) = (ctx.map, ctx.table, ctx.states);
    // Собственные рёбра состояния-композиции - Перед `next`/`END`, как у эталона: он
    // проверяет `ref` в порядке объявления, а `next` берёт последним.
    if emit_transitions(p, raw, map, table, states, scope, out)? {
        // Безусловное ребро завершает цепочку: дальше недостижимо.
        return Ok(());
    }
    // Состояние С телом автомат не завершает, и `exit` в нём не наступает вовсе: выхода
    // нет. Проверка стоит до печати `exit` - иначе он исполнялся бы каждый такт после
    // завершения композиции, тогда как эталон не исполняет его ни разу.
    if next.local().is_empty() && !raw.is_terminated() {
        return Ok(());
    }
    emit_named_blocks(p, raw, "exit", scope, out)?;
    // `END` - только состоянию без переходов: у эталона узел завершается при пустом
    // списке переходов, а при несработавших остаётся в состоянии.
    if next.local().is_empty() && !raw.references().is_empty() {
        return Ok(());
    }
    if next.local().is_empty() {
        p.ident(&format!("self.state = {};", table.end_path())).nl();
        return Ok(());
    }
    emit_enter_of(p, map, next, scope, out)?;
    p.ident(&format!("self.state = {};", table.path_of(next)?))
        .nl();
    Ok(())
}

/// Строит список аргументов вызова `tick` под-модели.
///
/// Аргументы обязаны совпадать с параметрами, которые печатает [`emit_tick`] для
/// **той же** модели: и то и другое считается по одним предикатам
/// ([`needs_hal`], [`shared_variables`]). Разойдись они - порождённый код не
/// собрался бы, а в худшем случае связал бы не те переменные.
fn call_args(
    map: &RustMap,
    instance: &Instance,
    scope: &Scope,
    is_root: bool,
) -> Result<String, Diagnostic> {
    let sub_name = submodel_name(map, &instance.unique).ok_or_else(|| {
        Diagnostic::error(
            Location::Codegen,
            format!("Под-модель '{}' не найдена в карте", instance.unique),
        )
        .with_code("RS-012")
    })?;

    let mut args = Vec::new();
    // Общие переменные - Одним аргументом `&mut Shared`, в порядке сигнатуры (shared,
    // затем hal). У корня - `&mut self.shared`; у под-модели - ретрансляция полученного
    // параметра `shared` (метод-вызов перезаимствует его автоматически). Передаётся,
    // только если вызываемой модели он нужен.
    if !shared_variables(map, &sub_name).is_empty() {
        if is_root {
            args.push("&mut self.shared".to_string());
        } else {
            args.push("shared".to_string());
        }
    }
    // HAL - Последним, тем же предикатом, каким `emit_tick` решает, объявлять ли
    // параметр. Предикат тот же, каким `emit_tick` объявляет параметр: разъедься они -
    // "this method takes 0 arguments but 1 was supplied".
    if !scope.hal.is_empty()
        && needs_hal(map, &sub_name, false, &mut BTreeSet::new())
        && crate::generator::rust::rust_model::needs_hal_in_tick(
            map,
            &sub_name,
            &mut BTreeSet::new(),
        )
    {
        args.push(scope.hal_argument("вызов такта под-модели")?);
    }
    Ok(args.join(", "))
}

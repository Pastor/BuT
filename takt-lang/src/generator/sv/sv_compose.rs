//! Композиция состояний в цели SystemVerilog: параллельная `|` и последовательная `+`.
//!
//! ## Общий принцип: инлайн внутрь ветви `case` родителя
//!
//! Логика под-модели инлайнится в `always_comb` **внутри** `case`-ветви несущего
//! состояния (эталон - `stacker.c`: `_tick` под-моделей зовётся из `case` родителя).
//! Вынос наружу изменил бы модель: под-модели продолжали бы работать после выхода
//! родителя.
//!
//! ## Готовность шага читает `_next`, а не регистр
//!
//! В C `_is_done` вызывается после `_tick` и видит только что записанное
//! значение. В `always_comb` рабочая копия - `state_next`, поэтому эквивалент -
//! `(<sub>_state_next == <SUB>_END)`. Чтение регистра дало бы значение
//! **предыдущего** такта - ровно тот сдвиг, который осуждает. Здесь жил
//! дефект SV-`|` (см. CLAUDE.md).
//!
//! ## Последовательная `+`: регистр шага
//!
//! Каждой цепочке `+` дан независимый регистр `<state>_step` (сброс `STEP_0`,
//! ). В `always_comb` внутри ветви несущего состояния - вложенный
//! `unique case (<step>)`, где активен ровно **один** шаг. При завершении шага
//! `<step>_next` продвигается к следующему; на последнем шаге переход выполняет
//! **родительское** состояние. Тайминг совпадает с C `break`: следующий шаг
//! тикается на такте **после** завершения предыдущего (регистр защёлкивает
//! `<step>_next`).

use crate::diagnostics::Diagnostic;
use crate::diagnostics::lang::keys;
use crate::generator::indent::Printer;
use crate::generator::sv::sv_blocks::emit_named_blocks;
use crate::generator::sv::sv_expr::sv002;
use crate::generator::sv::sv_fsm::{Fsm, emit_model_body, end_variant};
use crate::generator::sv::sv_map::SvMap;
use crate::generator::sv::sv_names::{step_done_variant, step_reg_name, step_variant};
use crate::msg;
use crate::semantic::StateNode;
use crate::semantic::minimap::{Name, StateExtend};

/// Печатает состояние-реализацию (`S = A | B` или `S = A + B`).
#[allow(clippy::too_many_arguments)]
/// Печатает тело составного состояния.
///
/// В табличной форме возвращает **предикат готовности** реализации: переход наружу
/// печатает таблица, а условие у него то же, при котором его печатает форма `unique
/// case`. В форме по умолчанию возвращает `None` - переход уже напечатан.
pub(crate) fn emit_extend(
    p: &mut Printer,
    map: &SvMap,
    fsm: &Fsm,
    state_name: &Name,
    state: &StateNode,
    model: &Name,
    extend: &StateExtend,
    next: &Name,
    states: &[Name],
) -> Result<Option<String>, Diagnostic> {
    match extend {
        StateExtend::None => Ok(None),
        // Последовательная композиция: шаг-регистр + инлайн активного шага.
        StateExtend::Concatenation(items) => {
            let exit = ChainExit::Parent {
                state,
                model,
                next,
                states,
            };
            emit_chain(p, map, fsm, state_name, items, &mut Vec::new(), exit)
        }
        // Параллельная композиция (и вырожденный случай одной модели): под-модели
        // работают одновременно, родитель уходит дальше, когда завершились все.
        StateExtend::Parallel(_) | StateExtend::Model(_, _) => {
            let done_exprs = inline_composed(p, map, fsm, state_name, extend, &mut Vec::new())?;
            if done_exprs.is_empty() {
                return Ok(None);
            }
            if map.fsm_table() {
                return Ok(Some(done_exprs.join(" && ")));
            }
            p.ident(&format!("if ({}) begin", done_exprs.join(" && ")))
                .nl();
            p.up();
            emit_parent_transition(p, map, fsm, state, model, next, states)?;
            p.down();
            p.ident("end").nl();
            Ok(None)
        }
    }
}

/// Что делать по завершении последнего шага цепочки.
enum ChainExit<'a> {
    /// Цепочка верхнего уровня: последний шаг уводит родительское состояние.
    Parent {
        state: &'a StateNode,
        model: &'a Name,
        next: &'a Name,
        states: &'a [Name],
    },
    /// Вложенная цепочка: выхода из состояния у неё нет - она переходит в собственное
    /// терминальное состояние, а его читает вмещающая композиция.
    Done,
}

/// Печатает цепочку `+`: `unique case (<step>)`, по одному активному шагу.
///
/// `path` - место цепочки в дереве композиции (носитель
/// [`chain_site`](crate::generator::chain_site)): по нему адресуются регистр шага и его
/// перечисление. У цепочки верхнего уровня путь пуст.
fn emit_chain(
    p: &mut Printer,
    map: &SvMap,
    fsm: &Fsm,
    state_name: &Name,
    items: &[StateExtend],
    path: &mut Vec<usize>,
    exit: ChainExit<'_>,
) -> Result<Option<String>, Diagnostic> {
    let step = step_reg_name(state_name, path);
    // `unique case`: варианты `STEP_0..STEP_{N-1}` покрыты все - ветвь по шагу на
    // каждый элемент, значит CASEINCOMPLETE не возникает. У вложенной цепочки к ним
    // добавляется терминальный `DONE`, и он тоже обязан иметь ветвь. Условие "цепочка
    // на последнем шаге" - регистр шага, а не `_next`: в комбинационном блоке он не
    // меняется, и диспетчер таблицы видит то же значение, что ветвь `unique case`.
    let step_at_last = format!(
        "{} == {}",
        step,
        step_variant(state_name, path, items.len().saturating_sub(1))
    );
    let mut parent_ready: Option<String> = None;
    p.ident(&format!("unique case ({})", step)).nl();
    p.up();
    for (i, item) in items.iter().enumerate() {
        p.ident(&format!("{}: begin", step_variant(state_name, path, i)))
            .nl();
        p.up();
        path.push(i);
        let done_exprs = inline_composed(p, map, fsm, state_name, item, path)?;
        path.pop();
        // Пустой шаг (`None`) не продвигается - но в цепочке `+` его не бывает.
        let cond = if done_exprs.is_empty() {
            "1'b0".to_string()
        } else {
            done_exprs.join(" && ")
        };
        p.ident(&format!("if ({}) begin", cond)).nl();
        p.up();
        if i + 1 == items.len() {
            match &exit {
                // Последний шаг завершён - переход родительского состояния. В табличной
                // форме его печатает таблица: здесь запоминается лишь условие "цепочка
                // на последнем шаге и он завершён".
                ChainExit::Parent { .. } if map.fsm_table() => {
                    parent_ready = Some(format!("({}) && ({})", step_at_last, cond));
                }
                ChainExit::Parent {
                    state,
                    model,
                    next,
                    states,
                } => emit_parent_transition(p, map, fsm, state, model, next, states)?,
                // Завершение вложенной цепочки - её собственное состояние, а не "все
                // шаги готовы": шаг, до которого очередь не дошла, готовности не
                // выставлял ни разу.
                ChainExit::Done => {
                    p.ident(&format!(
                        "{}_next = {};",
                        step,
                        step_done_variant(state_name, path)
                    ))
                    .nl();
                }
            }
        } else {
            // Иначе - продвижение шага; следующий тикается на такте после (регистр
            // защёлкивает `_next`), как `break` в C.
            p.ident(&format!(
                "{}_next = {};",
                step,
                step_variant(state_name, path, i + 1)
            ))
            .nl();
        }
        p.down();
        p.ident("end").nl();
        p.down();
        p.ident("end").nl();
    }
    if matches!(exit, ChainExit::Done) {
        // Ветвь терминала обязательна: `unique case` без неё даёт CASEINCOMPLETE, а
        // проверка цели считает предупреждение ошибкой.
        p.ident(&format!(
            "{}: begin end",
            step_done_variant(state_name, path)
        ))
        .nl();
    }
    p.down();
    p.ident("endcase").nl();
    Ok(parent_ready)
}

/// Инлайнит тело одного шага/ветви и возвращает done-выражения (на `_next`).
///
/// `Model` -> инлайн такта под-модели + одно done-выражение; `Parallel` -> инлайн всех
/// ветвей + конъюнкция done; `Concatenation` -> **вложенная цепочка** со своей машиной
/// шагов, готовность которой читается по её терминальному состоянию.
fn inline_composed(
    p: &mut Printer,
    map: &SvMap,
    fsm: &Fsm,
    state_name: &Name,
    item: &StateExtend,
    path: &mut Vec<usize>,
) -> Result<Vec<String>, Diagnostic> {
    match item {
        StateExtend::Model(sub, _) => {
            emit_model_body(p, map, fsm, sub)?;
            let sub_reg = fsm
                .state_reg
                .get(sub.unique())
                .ok_or_else(|| sv002(&msg!(keys::SV_WHAT_SUBMODEL_STATE_REGISTER, name = sub)))?;
            // `_next`, а не регистр: в C `_is_done` читает значение, только что
            // записанное тиком. Регистр дал бы значение предыдущего такта.
            Ok(vec![format!("({}_next == {})", sub_reg, end_variant(sub))])
        }
        StateExtend::Parallel(inner) => {
            let mut done_exprs = Vec::new();
            for (i, it) in inner.iter().enumerate() {
                path.push(i);
                let done = inline_composed(p, map, fsm, state_name, it, path);
                path.pop();
                done_exprs.extend(done?);
            }
            Ok(done_exprs)
        }
        // Вложенная цепочка внутри параллели или внутри шага другой цепочки.
        StateExtend::Concatenation(inner) => {
            emit_chain(p, map, fsm, state_name, inner, path, ChainExit::Done)?;
            let step = step_reg_name(state_name, path);
            // Готовность читается по `_next` - тот же довод, что у под-модели: регистр
            // отдал бы значение предыдущего такта.
            Ok(vec![format!(
                "({}_next == {})",
                step,
                step_done_variant(state_name, path)
            )])
        }
        StateExtend::None => Ok(Vec::new()),
    }
}

/// Печатает переход несущего (родительского) состояния после завершения композиции:
/// `exit` текущего -> `enter` следующего -> `state_next = NEXT` (или `END`, если
/// следующего нет). Общий хвост для `|` и последнего шага `+`.
fn emit_parent_transition(
    p: &mut Printer,
    map: &SvMap,
    fsm: &Fsm,
    state: &StateNode,
    model: &Name,
    next: &Name,
    states: &[Name],
) -> Result<(), Diagnostic> {
    // Собственные рёбра состояния-композиции - Перед `next`/`END`, как у эталона: `ref`
    // в порядке объявления, `next` последним.
    if crate::generator::sv::sv_fsm::emit_transitions(p, map, fsm, state, model, states)? {
        return Ok(());
    }
    // `END` - только состоянию без рёбер: у эталона узел завершается при пустом списке
    // переходов, а при несработавших остаётся в состоянии.
    if next.local().is_empty() && !state.references().is_empty() {
        return Ok(());
    }
    // Состояние С телом автомат не завершает, и `exit` в нём не наступает: выхода нет.
    // Проверка стоит до печати `exit` - иначе он шёл бы каждый такт после завершения
    // композиции, тогда как эталон не исполняет его ни разу.
    if next.local().is_empty() && !state.is_terminated() {
        return Ok(());
    }
    emit_named_blocks(p, state, fsm, "exit")?;
    let reg = fsm
        .state_reg
        .get(model.unique())
        .ok_or_else(|| sv002(&msg!(keys::SV_WHAT_MODEL_STATE_REGISTER, name = model)))?;
    if next.local().is_empty() {
        p.ident(&format!("{}_next = {};", reg, end_variant(model)))
            .nl();
    } else {
        let next_rc = map.raw_state_at(next.clone())?;
        emit_named_blocks(p, &next_rc.borrow(), fsm, "enter")?;
        p.ident(&format!(
            "{}_next = {};",
            reg,
            next.unique_uppercase_snakecase()
        ))
        .nl();
    }
    Ok(())
}

//! Табличная форма автомата у цели `st`.
//!
//! # Идиома принадлежит целевому языку
//!
//! Строки таблицы собирает общий носитель [`generator::table`](crate::generator::table) -
//! он же обслуживает цели `c` и `rust`. Форма у ST своя, и выбрана она **прогоном
//! `iec2c`**, а не чтением стандарта:
//!
//! - строки - **четыре параллельных массива** в `VAR CONSTANT`
//!   (`TAKT_FROM`, `TAKT_GUARD`, `TAKT_ACTION`, `TAKT_TO`): массив структур
//!   MatIEC в `VAR CONSTANT` не принимает (тот же отказ, что у инициализатора
//!   массива структур - );
//! - диспетчер - `WHILE` по строкам с `CASE` по номерам стража и действия:
//!   **указателей на функции в IEC нет вовсе**, и номер + `CASE` - единственная
//!   форма "переходы данными", какая в этом языке существует.
//!
//! **Пустой `CASE` недопустим** ("invalid statement in case element"): если у модели
//! нет ни одного стража, `CASE` по стражам не печатается вовсе - признак `takt_ok`
//! остаётся истинным.
//!
//! **Момент проверки завершения цепочки сохранён.** В форме `CASE`
//! состояние-цепочка уходит наружу из ветви `<число шагов>`, то есть на скане
//! **после** того, как последний шаг завершился. Диспетчер же стоит в конце
//! скана, и наивный страж `счётчик = N` сработал бы на скан раньше. Поэтому
//! признак **защёлкивается** в начале ветви состояния (`takt_ready_...`) - так
//! таблица повторяет момент, в который переход печатает форма `CASE`.

use crate::diagnostics::Diagnostic;
use crate::generator::indent::Printer;
use crate::generator::table::{self, Row, RowTarget};
use crate::semantic::ModelNode;
use crate::semantic::minimap::{Element, Name};
use crate::semantic::type_node::TypeNode;

use super::st_edges::edge_guard;
use super::st_map::StMap;
use super::st_model::{BodyOutput, StateTable, emit_block, unknown_state};
use crate::diagnostics::lang::keys;
use crate::msg;

/// Имя массива состояний-источников.
const FROM: &str = "TAKT_TRANS_FROM";
/// Имя массива номеров стражей (`-1` - страж не нужен).
const GUARD: &str = "TAKT_TRANS_GUARD";
/// Имя массива номеров действий (`-1` - действия нет).
const ACTION: &str = "TAKT_TRANS_ACTION";
/// Имя массива состояний-приёмников.
const TO: &str = "TAKT_TRANS_TO";
/// Переменная-курсор диспетчера.
const ROW: &str = "takt_trans_row";
/// Признак "строка сработала".
const FIRED: &str = "takt_trans_fired";
/// Признак "страж строки истинен".
const OK: &str = "takt_trans_ok";

/// Объявления таблицы: константные массивы строк.
///
/// Пусто, если переходов у модели нет: `ARRAY [0..-1]` в IEC невыразим.
pub(crate) fn constants(
    map: &StMap,
    element: &Element,
    model: &ModelNode,
    table: &StateTable,
) -> Result<Vec<(String, String, String)>, Diagnostic> {
    let rows = table::rows(element, map)?;
    if rows.is_empty() {
        return Ok(Vec::new());
    }
    let last = rows.len() - 1;
    let mut from = Vec::new();
    let mut guard = Vec::new();
    let mut action = Vec::new();
    let mut to = Vec::new();
    for (index, row) in rows.iter().enumerate() {
        from.push(number_of(table, &row.from)?.to_string());
        guard.push(if has_guard(row) {
            index.to_string()
        } else {
            "-1".to_string()
        });
        action.push(if has_action(row, model) {
            index.to_string()
        } else {
            "-1".to_string()
        });
        to.push(target_number(table, row)?.to_string());
    }
    let range = format!("ARRAY [0..{last}] OF");
    Ok(vec![
        (
            FROM.to_string(),
            format!("{range} USINT"),
            format!("[{}]", from.join(", ")),
        ),
        (
            GUARD.to_string(),
            format!("{range} INT"),
            format!("[{}]", guard.join(", ")),
        ),
        (
            ACTION.to_string(),
            format!("{range} INT"),
            format!("[{}]", action.join(", ")),
        ),
        (
            TO.to_string(),
            format!("{range} USINT"),
            format!("[{}]", to.join(", ")),
        ),
    ])
}

/// Печатает диспетчер таблицы - после `END_CASE` тела такта.
pub(crate) fn emit_dispatcher(
    p: &mut Printer,
    map: &StMap,
    element: &Element,
    model: &ModelNode,
    table: &StateTable,
    out: &mut BodyOutput,
) -> Result<(), Diagnostic> {
    let rows = table::rows(element, map)?;
    if rows.is_empty() {
        return Ok(());
    }
    let last = rows.len() - 1;
    hoist(out, ROW, 16, true);
    hoist(out, FIRED, 1, false);

    let guards = guard_texts(map, model, &rows)?;
    let actions: Vec<usize> = rows
        .iter()
        .enumerate()
        .filter(|(_, row)| has_action(row, model))
        .map(|(index, _)| index)
        .collect();
    if !guards.is_empty() {
        hoist(out, OK, 1, false);
    }

    p.ident(&format!("{ROW} := 0;")).nl();
    p.ident(&format!("{FIRED} := FALSE;")).nl();
    p.ident(&format!("WHILE ({ROW} <= {last}) AND (NOT {FIRED}) DO"))
        .nl();
    p.up();
    p.ident(&format!("IF {FROM}[{ROW}] = state THEN")).nl();
    p.up();
    if !guards.is_empty() {
        p.ident(&format!("{OK} := TRUE;")).nl();
        p.ident(&format!("CASE {GUARD}[{ROW}] OF")).nl();
        p.up();
        for (index, text) in &guards {
            p.ident(&format!("{index}: {OK} := {text};")).nl();
        }
        p.down();
        p.ident("END_CASE;").nl();
        p.ident(&format!("IF {OK} THEN")).nl();
    } else {
        p.ident("IF TRUE THEN").nl();
    }
    p.up();
    if !actions.is_empty() {
        p.ident(&format!("CASE {ACTION}[{ROW}] OF")).nl();
        p.up();
        for index in &actions {
            p.ident(&format!("{index}:")).nl();
            p.up();
            emit_action(p, &rows[*index], model, out)?;
            p.down();
        }
        p.down();
        p.ident("END_CASE;").nl();
    }
    p.ident(&format!("state := {TO}[{ROW}];")).nl();
    p.ident(&format!("{FIRED} := TRUE;")).nl();
    p.down();
    p.ident("END_IF;").nl();
    p.down();
    p.ident("END_IF;").nl();
    p.ident(&format!("{ROW} := {ROW} + 1;")).nl();
    p.down();
    p.ident("END_WHILE;").nl();
    let _ = table;
    Ok(())
}

/// Печатает действие строки: `exit` источника, затем `enter` приёмника.
///
/// Пустым действие быть не может: строки без блоков в `CASE` не попадают
/// (`has_action`), а пустая ветвь `CASE` в IEC недопустима.
fn emit_action(
    p: &mut Printer,
    row: &Row,
    model: &ModelNode,
    out: &mut BodyOutput,
) -> Result<(), Diagnostic> {
    emit_block(p, &row.exit_state.borrow(), "exit", model, &mut out.stmt)?;
    if let Some(enter) = &row.enter_state {
        emit_block(p, &enter.borrow(), "enter", model, &mut out.stmt)?;
    }
    Ok(())
}

/// Тексты стражей: номер строки -> выражение `BOOL`.
fn guard_texts(
    map: &StMap,
    model: &ModelNode,
    rows: &[Row],
) -> Result<Vec<(usize, String)>, Diagnostic> {
    let mut out = Vec::new();
    for (index, row) in rows.iter().enumerate() {
        let done = row.done.as_ref().map(|(state, _)| ready_flag(state));
        let cond = match &row.cond {
            None => None,
            Some(_) => Some(edge_condition(map, model, row)?),
        };
        let text = match (done, cond) {
            (None, None) => continue,
            (Some(done), None) => done,
            (None, Some(cond)) => cond,
            (Some(done), Some(cond)) => format!("({done}) AND ({cond})"),
        };
        out.push((index, text));
    }
    Ok(out)
}

/// Страж ребра - тем же носителем, что печатает его форма `CASE`.
///
/// Выдержка `after` в профиле "часы" опирается на таймер ребра; его имя строит
/// `st_time::timer_name` по состоянию и **порядковому номеру ребра в состоянии** - тот
/// же номер, что у формы `CASE`.
fn edge_condition(map: &StMap, model: &ModelNode, row: &Row) -> Result<String, Diagnostic> {
    let raw = row.exit_state.borrow();
    let references = raw.references();
    let target = row
        .target_name()
        .map(|n| n.local().to_string())
        .unwrap_or_default();
    let position = references
        .iter()
        .position(|r| r.name == target)
        .ok_or_else(|| unknown_state(&msg!(keys::ST_EDGE_NOT_IN_TRANSITIONS, target = target)))?;
    let timer = if crate::generator::st::st_time::is_clock(map) {
        Some(crate::generator::st::st_time::timer_name(
            &row.from, position,
        ))
    } else {
        None
    };
    edge_guard(map, model, &references[position], timer.as_deref())
}

/// Имя защёлки "реализация состояния завершена".
///
/// Значение выставляется в начале ветви состояния (см. шапку модуля), поэтому таблица
/// видит его таким же, каким видит форма `CASE`.
pub(crate) fn ready_flag(state: &Name) -> String {
    format!("takt_ready_{}", state.local_lowercase_snakecase())
}

/// Есть ли у строки страж (условие ребра либо завершение реализации).
fn has_guard(row: &Row) -> bool {
    row.done.is_some() || row.cond.is_some()
}

/// Есть ли у строки действие (блоки `exit` источника либо `enter` приёмника).
fn has_action(row: &Row, model: &ModelNode) -> bool {
    let _ = model;
    let exit = !row.exit_state.borrow().get_named_blocks("exit").is_empty();
    let enter = row
        .enter_state
        .as_ref()
        .is_some_and(|s| !s.borrow().get_named_blocks("enter").is_empty());
    exit || enter
}

/// Номер состояния-источника строки.
fn number_of(table: &StateTable, name: &Name) -> Result<usize, Diagnostic> {
    table
        .number_of(name.unique())
        .ok_or_else(|| unknown_state(name.unique()))
}

/// Номер состояния-приёмника строки.
fn target_number(table: &StateTable, row: &Row) -> Result<usize, Diagnostic> {
    match &row.to {
        RowTarget::State(name) => number_of(table, name),
        RowTarget::End => Ok(table.end),
    }
}

/// Поднимает служебную переменную диспетчера в объявления.
fn hoist(out: &mut BodyOutput, name: &str, bits: u8, integer: bool) {
    let ty = if integer {
        TypeNode::Integer { bits, signed: true }
    } else {
        TypeNode::Bool
    };
    if out.stmt.hoisted.iter().any(|h| h.name == name) {
        return;
    }
    out.stmt
        .hoisted
        .push(crate::generator::st::st_stmt::Hoisted {
            name: name.to_string(),
            ty,
        });
}

/// Печатает защёлку готовности реализации состояния.
///
/// Зовётся в начале ветви состояния - до тика реализации: страж строки обязан видеть то
/// же значение, что видит ветвь `CASE` формы по умолчанию.
pub(crate) fn emit_ready_latch(
    p: &mut Printer,
    state: &Name,
    predicate: &str,
    out: &mut BodyOutput,
) {
    let flag = ready_flag(state);
    hoist(out, &flag, 1, false);
    p.ident(&format!("{flag} := {predicate};")).nl();
}

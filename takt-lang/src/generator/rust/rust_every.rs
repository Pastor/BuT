//! Периодические блоки `every` цели `rust`.
//!
//! Зеркало `c/c_every.rs` в rust-идиомах. Скрытое состояние - поле `takt_every<N>`
//! (поглощённое срабатываниями `elapsed`-время); срабатывание:
//! `elapsed.wrapping_sub(consumed) >= период` -> тело, `consumed += период`. `elapsed`
//! берётся из инфраструктуры длительностного `after`: метка `takt_entry_ms` (профиль
//! "часы") либо счётчик `takt_dwell` (профиль "такты").
//!
//! `-D warnings`: поле `takt_every<N>` эмитится **только** при наличии `every` и всегда
//! читается в теле - иначе неиспользуемое приватное поле завалит сборку.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::rust::rust_expr::Scope;
use crate::generator::rust::rust_map::RustMap;
use crate::generator::rust::rust_stmt::{StmtOutput, print_statement};
use crate::generator::rust::rust_time::{DWELL_FIELD, ENTRY_MS_FIELD, NOW_MS_METHOD, dwell_bits};
use crate::semantic::duration::{TimeProfile, units_or_diagnostic};
use crate::semantic::{ModelNode, StatementNode};

/// Ссылка на `every`-блок с глобальным индексом.
struct EveryRef<'a> {
    idx: usize,
    state: String,
    period_nanos: i64,
    body: &'a StatementNode,
}

/// Перечисляет `every`-блоки модели с глобальным индексом (детерминированно).
fn every_blocks(model: &ModelNode) -> Vec<EveryRef<'_>> {
    let mut out = Vec::new();
    let mut idx = 0usize;
    for (name, state) in &model.states {
        for block in state.named_blocks() {
            if let Some((period_nanos, _)) = block.every_period()
                && let Some(body) = block.statement()
            {
                out.push(EveryRef {
                    idx,
                    state: name.clone(),
                    period_nanos,
                    body,
                });
                idx += 1;
            }
        }
    }
    out
}

fn field(idx: usize) -> String {
    format!("takt_every{idx}")
}

/// Тип поля-аккумулятора: `u64` (профиль "часы", как `takt_entry_ms`) либо
/// `u{dwell_bits}` (профиль "такты", как `takt_dwell`).
fn field_ty(map: &RustMap, model: &ModelNode) -> Result<String, Diagnostic> {
    Ok(match map.time_profile() {
        TimeProfile::Clock => "u64".to_string(),
        TimeProfile::Ticks { .. } => format!("u{}", dwell_bits(map, model)?),
    })
}

/// Имена аккумуляторов `every`, которые модель получит, - в порядке печати.
///
/// Перечень блоков берётся у того же `every_blocks`, что и печать: набор занятых имён
/// цели обязан видеть ровно те поля, что печатаются.
pub(super) fn field_names(model: &ModelNode) -> Vec<String> {
    every_blocks(model)
        .into_iter()
        .map(|e| field(e.idx))
        .collect()
}

/// Печатает поля-аккумуляторы `every` в объявление `struct`.
pub(super) fn emit_struct_fields(
    p: &mut Printer,
    map: &RustMap,
    model: &ModelNode,
) -> Result<(), Diagnostic> {
    let ty = field_ty(map, model)?;
    for e in every_blocks(model) {
        let _ = e.period_nanos;
        p.ident(&format!("{}: {ty},", field(e.idx))).nl();
    }
    Ok(())
}

/// Печатает начальные значения полей `every` в литерале `Self { ... }` (`fn new`).
pub(super) fn emit_new_fields(p: &mut Printer, model: &ModelNode) {
    for e in every_blocks(model) {
        p.ident(&format!("{}: 0,", field(e.idx))).nl();
    }
}

/// Обнуляет аккумуляторы `every` - вход в состояние и `fn init`.
pub(super) fn emit_reset(p: &mut Printer, model: &ModelNode) {
    for e in every_blocks(model) {
        p.ident(&format!("self.{} = 0;", field(e.idx))).nl();
    }
}

/// Печатает периодические блоки `every` состояния в теле `tick`, после `always`.
///
/// `hal_access` - получатель HAL-вызова (`self.hal`/`hal`); нужен для `elapsed` профиля
/// "часы".
pub(super) fn emit_state_body(
    p: &mut Printer,
    ctx: &crate::generator::rust::rust_ctx::ModelEmit,
    state_local: &str,
    hal_access: &str,
    scope: &mut Scope,
    out: &mut StmtOutput,
) -> Result<(), Diagnostic> {
    let (map, model) = (ctx.map, ctx.model);
    let profile = map.time_profile();
    for e in every_blocks(model)
        .iter()
        .filter(|e| e.state == state_local)
    {
        let units =
            units_or_diagnostic(e.period_nanos, profile, Location::Codegen, "период 'every'")?;
        let f = field(e.idx);
        let elapsed = match profile {
            TimeProfile::Ticks { .. } => format!("self.{DWELL_FIELD}"),
            TimeProfile::Clock => {
                format!("{hal_access}.{NOW_MS_METHOD}().wrapping_sub(self.{ENTRY_MS_FIELD})")
            }
        };
        p.ident(&format!("let takt_elapsed = {elapsed};")).nl();
        p.ident(&format!(
            "if takt_elapsed.wrapping_sub(self.{f}) >= {units} {{"
        ))
        .up()
        .nl();
        print_statement(e.body, scope, p, out)?;
        p.ident(&format!("self.{f} = self.{f}.wrapping_add({units});"))
            .nl()
            .down()
            .ident("}")
            .nl();
    }
    Ok(())
}

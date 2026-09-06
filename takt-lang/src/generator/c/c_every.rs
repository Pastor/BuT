//! Периодические блоки `every` цели C (фича 0134-09).
//!
//! `every Nms { … }` — сахар над механизмом времени (правило 12 ADR 0134): тело
//! исполняется, пока автомат в состоянии-владельце, каждые `N` единиц профиля.
//! Скрытое состояние — аккумулятор `takt_every<N>` (поглощённое срабатываниями
//! `elapsed`-время); срабатывание: `elapsed - consumed >= период` → тело,
//! `consumed += период`. `elapsed` берётся из ТОЙ ЖЕ инфраструктуры, что и
//! длительностный `after`: метка входа `takt_entry_ms` (профиль «часы») или
//! счётчик `takt_dwell` (профиль «такты»), поэтому `c_time` уже завёл её поля.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::c::c_expr::condition::{DWELL_FIELD, ENTRY_MS_FIELD};
use crate::generator::c::c_expr::generate_code_block;
use crate::generator::c::c_map::CMap;
use crate::generator::c::c_time;
use crate::generator::indent::Printer;
use crate::semantic::duration::{TimeProfile, units_or_diagnostic};
use crate::semantic::minimap::Element;
use crate::semantic::{ModelNode, StatementNode};

/// Ссылка на `every`-блок с глобальным индексом в модели.
struct EveryRef<'a> {
    /// Глобальный индекс (детерминирован — состояния в `BTreeMap`-порядке).
    idx: usize,
    /// Имя состояния-владельца.
    state: String,
    /// Период в наносекундах.
    period_nanos: i64,
    /// Тело блока.
    body: &'a StatementNode,
}

/// Перечисляет `every`-блоки модели с глобальным индексом.
///
/// Порядок — обход `states` (`BTreeMap`, детерминизм 0048) и блоков состояния;
/// индекс сквозной, поэтому имя поля `takt_every<idx>` уникально в структуре.
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

/// Имя поля-аккумулятора `every`-блока.
fn field(idx: usize) -> String {
    format!("takt_every{idx}")
}

/// Ширина аккумулятора: делит её с меткой/счётчиком времени (одно место —
/// `c_time`), иначе `elapsed - consumed` переполнился бы раньше срабатывания.
fn bits(map: &CMap, model: &ModelNode) -> Result<u8, Diagnostic> {
    match map.time_profile() {
        TimeProfile::Clock => c_time::clock_marker_bits(map),
        TimeProfile::Ticks { .. } => Ok(crate::semantic::duration::counter_bits(
            c_time::counter_ticks(map, model)?,
        )
        .unwrap_or(64)),
    }
}

/// Печатает поля-аккумуляторы `every` в структуру модели (фича 0134-09).
pub(super) fn emit_fields(
    printer: &mut Printer,
    map: &CMap,
    model: &ModelNode,
) -> Result<(), Diagnostic> {
    let blocks = every_blocks(model);
    if blocks.is_empty() {
        return Ok(());
    }
    let b = bits(map, model)?;
    for e in &blocks {
        printer.ident(&format!("uint{b}_t {};", field(e.idx))).nl();
    }
    Ok(())
}

/// Обнуляет аккумуляторы `every` — вызывается при входе в состояние и в `_init`
/// (фича 0134-09). Отсчёт периода ведётся заново от входа, как и `elapsed`.
pub(super) fn emit_reset(printer: &mut Printer, model: &ModelNode) {
    for e in every_blocks(model) {
        printer.ident(&format!("model->{} = 0;", field(e.idx))).nl();
    }
}

/// Печатает периодические блоки `every` состояния `state_local` в теле такта
/// (фича 0134-09), после `always`.
///
/// Гейт читает `elapsed` из инфраструктуры длительностного `after`: разность
/// `now_ms() - takt_entry_ms` (профиль «часы») либо счётчик `takt_dwell` (профиль
/// «такты»). Арифметика беззнаковая с усечением к ширине поля — как у `after`.
pub(super) fn emit_state_body(
    printer: &mut Printer,
    map: &CMap,
    owner: &Element,
    model: &ModelNode,
    state_local: &str,
) -> Result<(), Diagnostic> {
    let blocks = every_blocks(model);
    if blocks.is_empty() {
        return Ok(());
    }
    let b = bits(map, model)?;
    let profile = map.time_profile();
    for e in blocks.iter().filter(|e| e.state == state_local) {
        let units =
            units_or_diagnostic(e.period_nanos, profile, Location::Codegen, "период 'every'")?;
        let elapsed = elapsed_expr(map, owner, b)?;
        let f = field(e.idx);
        printer.ident("{").up().nl();
        printer
            .ident(&format!("uint{b}_t takt_elapsed = {elapsed};"))
            .nl()
            .ident(&format!(
                "if ((uint{b}_t)(takt_elapsed - model->{f}) >= {units}) {{"
            ))
            .up()
            .nl();
        generate_code_block(printer, map, owner, vec![], e.body, true)?;
        printer
            .ident(&format!("model->{f} = (uint{b}_t)(model->{f} + {units});"))
            .nl()
            .down()
            .ident("}")
            .nl()
            .down()
            .ident("}")
            .nl();
    }
    Ok(())
}

/// Выражение `elapsed` (прошло с входа) в единицах профиля.
///
/// Совпадает с тем, что печатает `after_condition`: «часы» — разность
/// `now_ms() - takt_entry_ms` (HAL на корне — `model`, у под-модели — `main`);
/// «такты» — счётчик `takt_dwell`.
fn elapsed_expr(map: &CMap, owner: &Element, b: u8) -> Result<String, Diagnostic> {
    match map.time_profile() {
        TimeProfile::Ticks { .. } => Ok(format!("model->{DWELL_FIELD}")),
        TimeProfile::Clock => {
            let hal = if owner.name().eq(&map.root_name()) {
                "model"
            } else {
                "main"
            };
            let now = format!(
                "{hal}->{}({hal}->userdata)",
                crate::generator::c::FUNCTION_TIME_NOW_MS
            );
            Ok(format!(
                "(uint{b}_t)((uint{b}_t){now} - model->{ENTRY_MS_FIELD})"
            ))
        }
    }
}

//! Механизм времени цели `st` (IEC 61131-3).
//!
//! Два профиля:
//! - **"часы"** - штатный `TON` (IEC-таймер): экземпляр в `VAR` владельца,
//!   `dwell(IN := TRUE, PT := T#Nms);` каждый скан в состоянии, условие `dwell.Q`,
//!   сброс `IN := FALSE` при **любом** выходе (иначе выдержка "прилипнет").
//! - **"такты"** - счётчик сканов `takt_dwell` (как цель `c`), условие `>= D`.
//!
//! MatIEC: экземпляр `TON` - это `VAR` (не `VAR CONSTANT`), порядок секций сохраняется
//! автоматически (VAR идёт раньше). Имя экземпляра уникализируется состоянием и
//! индексом ребра - чтобы не столкнуться с `var` пользователя.

use crate::generator::st::st_map::StMap;
use crate::semantic::duration::TimeProfile;
use crate::semantic::minimap::Name;
use crate::semantic::time_ast::{
    model_uses_duration_after, model_uses_every, model_uses_tick_after,
};
use crate::semantic::{ModelNode, StatementNode, type_node::TypeNode};

/// Имя поля-счётчика сканов, проведённых в текущем состоянии.
pub(super) const DWELL_FIELD: &str = "takt_dwell";
/// Имя поля "состояние на конец предыдущего скана".
pub(super) const PREV_STATE_FIELD: &str = "takt_prev_state";
/// Стандартный тип IEC таймера включения.
pub(super) const TON_TYPE: &str = "TON";

/// Профиль модели - "часы"?
pub(super) fn is_clock(map: &StMap) -> bool {
    matches!(map.time_profile(), TimeProfile::Clock)
}

/// Длительностный `after Nms` **или** периодический `every Nms`: обе величины меряются
/// длительностью; в профиле "такты" им нужен счётчик сканов.
fn uses_duration_time(model: &ModelNode) -> bool {
    model_uses_duration_after(model) || model_uses_every(model)
}

/// Нужен ли счётчик сканов `takt_dwell`: тактовая выдержка `after Nt` (в любом профиле)
/// либо длительностный `after Nms`/`every Nms` в профиле "такты".
pub(super) fn needs_dwell(map: &StMap, model: &ModelNode) -> bool {
    model_uses_tick_after(model) || (!is_clock(map) && uses_duration_time(model))
}

/// Имя переменной-аккумулятора `every`-блока (профиль "такты").
pub(super) fn every_field(idx: usize) -> String {
    format!("takt_every{idx}")
}

/// Имя экземпляра самосбрасывающегося `TON` для `every`-блока (профиль "часы").
pub(super) fn every_timer(idx: usize) -> String {
    format!("takt_every_ton{idx}")
}

/// Тип аккумулятора `every` - как `takt_dwell` (`UDINT`).
pub(super) fn every_field_type() -> TypeNode {
    dwell_type()
}

/// Периодический блок `every` модели: индекс, состояние, период, тело.
pub(super) struct EveryBlock<'a> {
    pub(super) idx: usize,
    pub(super) state: String,
    pub(super) period_nanos: i64,
    pub(super) body: &'a StatementNode,
}

/// Перечисляет `every`-блоки модели со сквозным индексом (детерминированно).
pub(super) fn model_every(model: &ModelNode) -> Vec<EveryBlock<'_>> {
    let mut out = Vec::new();
    let mut idx = 0usize;
    for (name, state) in &model.states {
        for block in state.named_blocks() {
            if let Some((period_nanos, _)) = block.every_period()
                && let Some(body) = block.statement()
            {
                out.push(EveryBlock {
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

/// Тип счётчика `takt_dwell` - `UDINT` (32 бита): диапазон с запасом, тип IEC без риска
/// `ST-013` (в отличие от 64-битного `LINT`).
pub(super) fn dwell_type() -> TypeNode {
    TypeNode::Integer {
        bits: 32,
        signed: false,
    }
}

/// Тип метки предыдущего состояния - `USINT` (как поле `state`).
pub(super) fn prev_state_type() -> TypeNode {
    TypeNode::Integer {
        bits: 8,
        signed: false,
    }
}

/// Имя экземпляра `TON` для выдержки состояния (профиль "часы").
///
/// Уникально по состоянию и индексу ребра: `<state>_dwell<idx>` - суфисправленияция, как у
/// прочих синтетических имён st (`main_step`, `a0`). Столкнуться с `var` пользователя
/// не может: у пользовательских имён нет суфисправления `_dwellN`.
pub(super) fn timer_name(state: &Name, idx: usize) -> String {
    format!("{}_dwell{}", state.local_lowercase_snakecase(), idx)
}

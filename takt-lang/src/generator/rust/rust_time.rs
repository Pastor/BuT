//! Механизм времени цели `rust`.
//!
//! Зеркало `c/c_time.rs` в rust-идиомах: счётчик/метка - поля структуры модели,
//! `takt_prev_state` типизирован **самим enum-ом состояния** (не `unsigned`, как в C),
//! сравнение выдержки в профиле "часы" - беззнаковой обёрткой `now.wrapping_sub(t0)`
//! (обычное вычитание паникует в debug - ). Арифметика длительности - не здесь, а в
//! общем слое `semantic::duration`.
//!
//! Предикаты `needs_dwell`/`needs_entry_ms` обязаны совпадать во всех точках (поле ↔
//! init ↔ обновление ↔ трейт): в Rust неиспользуемое приватное поле - ошибка `-D
//! warnings`, а не молчаливая трата, как в C.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::indent::Printer;
use crate::generator::rust::rust_map::RustMap;
use crate::semantic::ModelNode;
use crate::semantic::duration::{TimeProfile, counter_bits, units_or_diagnostic};
use crate::semantic::time_ast::{
    model_tree_uses_duration_after, model_tree_uses_every, model_uses_duration_after,
    model_uses_every, model_uses_tick_after,
};

/// Длительностная выдержка `after Nms` **или** периодический блок `every Nms`: обе
/// меряются длительностью и требуют одну инфраструктуру.
fn uses_duration_time(model: &ModelNode) -> bool {
    model_uses_duration_after(model) || model_uses_every(model)
}

/// Имя поля-счётчика тактов, проведённых в текущем состоянии.
pub(super) const DWELL_FIELD: &str = "takt_dwell";
/// Имя поля-метки времени входа в состояние (профиль "часы").
pub(super) const ENTRY_MS_FIELD: &str = "takt_entry_ms";
/// Имя поля "состояние на конец предыдущего такта".
pub(super) const PREV_STATE_FIELD: &str = "takt_prev_state";
/// Метод трейта `Hal` - внешний источник времени (профиль "часы").
pub(super) const NOW_MS_METHOD: &str = "now_ms";

/// Профиль модели - "часы"?
fn is_clock(map: &RustMap) -> bool {
    matches!(map.time_profile(), TimeProfile::Clock)
}

/// Нужен ли счётчик тактов `takt_dwell`: тактовая выдержка `after Nt` (в любом профиле)
/// либо длительностная `after Nms` в профиле "такты".
pub(super) fn needs_dwell(map: &RustMap, model: &ModelNode) -> bool {
    model_uses_tick_after(model) || (!is_clock(map) && uses_duration_time(model))
}

/// Нужна ли метка времени `takt_entry_ms`: профиль "часы" + длительностный `after Nms`
/// или `every Nms`.
pub(super) fn needs_entry_ms(map: &RustMap, model: &ModelNode) -> bool {
    is_clock(map) && uses_duration_time(model)
}

/// Нужен ли метод `now_ms` трейту `Hal` (профиль "часы" + длительностный `after` или
/// `every` в дереве).
///
/// Метод на трейте один на файл, а длительностная выдержка бывает во вложенной
/// под-модели композиции - решение по всему дереву корня.
pub(super) fn needs_now_ms(map: &RustMap, model: &ModelNode) -> bool {
    is_clock(map) && (model_tree_uses_duration_after(model) || model_tree_uses_every(model))
}

/// Разрядность счётчика тактов `takt_dwell` - по максимуму `after`/`every` модели.
pub(super) fn dwell_bits(map: &RustMap, model: &ModelNode) -> Result<u8, Diagnostic> {
    Ok(counter_bits(max_units(map, model)?).unwrap_or(64))
}

/// Максимум единиц профиля по выдержкам `after` **этой** модели.
fn max_units(map: &RustMap, model: &ModelNode) -> Result<u64, Diagnostic> {
    let profile = map.time_profile();
    let mut max = 0u64;
    for state in model.states.values() {
        for reference in state.references() {
            // Вычисляемая выдержка: значение известно лишь в такте, поэтому счётчик
            // обязан вмещать любое представимое - иначе в Rust сравнение `u8 >= u32` не
            // скомпилируется (`E0308`), а в C молча усечёт.
            if matches!(reference.cond, crate::semantic::ConditionNode::AfterExpr(_)) {
                max = max.max(u64::from(u32::MAX));
            }
            if let crate::semantic::ConditionNode::After(nanos) = reference.cond {
                let units =
                    units_or_diagnostic(nanos, profile, Location::Codegen, "выдержка 'after'")?;
                max = max.max(units);
            }
        }
        // Периоды `every` делят ширину счётчика - учитываем и их.
        for block in state.named_blocks() {
            if let Some((period_nanos, _)) = block.every_period() {
                let units = units_or_diagnostic(
                    period_nanos,
                    profile,
                    Location::Codegen,
                    "период 'every'",
                )?;
                max = max.max(units);
            }
        }
    }
    Ok(max)
}

/// Печатает поля структуры для механизма времени (в объявлении `struct`).
///
/// `enum_name` - тип enum-а состояния (для `takt_prev_state`).
pub(super) fn emit_struct_fields(
    p: &mut Printer,
    map: &RustMap,
    model: &ModelNode,
    enum_name: &str,
) -> Result<(), Diagnostic> {
    for field in field_names(map, model) {
        // Метка - исправлениеированный u64: `now_ms` отдаёт u64, а обёртка `wrapping_sub`
        // верна на любой ширине (сужать ради байтов здесь не стоит риска рассинхрона).
        let ty = match field {
            f if f == DWELL_FIELD => format!("u{}", dwell_bits(map, model)?),
            f if f == ENTRY_MS_FIELD => "u64".to_string(),
            _ => enum_name.to_string(),
        };
        p.ident(&format!("{field}: {ty},")).nl();
    }
    Ok(())
}

/// Имена полей времени, которые модель получит, - в порядке печати.
///
/// Условия эмиссии живут здесь и только здесь: их читает и печать полей, и набор
/// занятых имён цели. Вторая копия условий разошлась бы с печатью молча - и отказ
/// `RS-026` перестал бы срабатывать ровно там, где поле всё-таки печатается.
pub(super) fn field_names(map: &RustMap, model: &ModelNode) -> Vec<&'static str> {
    let mut out = Vec::new();
    if needs_dwell(map, model) {
        out.push(DWELL_FIELD);
    }
    if needs_entry_ms(map, model) {
        out.push(ENTRY_MS_FIELD);
    }
    if needs_dwell(map, model) || needs_entry_ms(map, model) {
        out.push(PREV_STATE_FIELD);
    }
    out
}

/// Печатает начальные значения полей времени в литерале `Self { ... }` (`fn new`).
///
/// Метку не латчим здесь (в конструкторе `hal` может быть недоступен) - это делает `fn
/// init` через [`emit_init`], как `_init` цели `c` (вход стартового "до такта 1").
pub(super) fn emit_new_fields(
    p: &mut Printer,
    map: &RustMap,
    model: &ModelNode,
    enum_name: &str,
) -> Result<(), Diagnostic> {
    if needs_dwell(map, model) {
        p.ident(&format!("{DWELL_FIELD}: 0,")).nl();
    }
    if needs_entry_ms(map, model) {
        p.ident(&format!("{ENTRY_MS_FIELD}: 0,")).nl();
    }
    if needs_dwell(map, model) || needs_entry_ms(map, model) {
        p.ident(&format!("{PREV_STATE_FIELD}: {enum_name}::Init,"))
            .nl();
    }
    Ok(())
}

/// Печатает сброс полей времени в `fn init` (в 0 / `Init`).
///
/// Метку `now_ms` здесь не латчим: под-модель композиции не имеет доступа к HAL в
/// `init(&mut self)` (поле `hal` - только у корня, под-модель получает `hal` параметром
/// `tick`). Настоящий латч метки делает INIT-диспетчер такта
/// ([`emit_first_entry_latch`]), где HAL доступен всем.
pub(super) fn emit_init(p: &mut Printer, map: &RustMap, model: &ModelNode, enum_name: &str) {
    if needs_dwell(map, model) {
        p.ident(&format!("self.{DWELL_FIELD} = 0;")).nl();
    }
    if needs_entry_ms(map, model) {
        p.ident(&format!("self.{ENTRY_MS_FIELD} = 0;")).nl();
    }
    if needs_dwell(map, model) || needs_entry_ms(map, model) {
        p.ident(&format!("self.{PREV_STATE_FIELD} = {enum_name}::Init;"))
            .nl();
    }
}

/// Печатает латч метки `now_ms` в INIT-диспетчере такта: вход в стартовое состояние "до
/// такта 1" - метка обязана отсчитываться от него, а не от нуля абсолютного времени.
/// HAL здесь доступен всем (у такта под-модели он - параметр). Дублирование с концом
/// такта безвредно (то же значение).
pub(super) fn emit_first_entry_latch(
    p: &mut Printer,
    map: &RustMap,
    model: &ModelNode,
    hal_access: &str,
) {
    if needs_entry_ms(map, model) {
        p.ident(&format!(
            "self.{ENTRY_MS_FIELD} = {hal_access}.{NOW_MS_METHOD}();"
        ))
        .nl();
    }
}

/// Печатает обновление счётчика/метки в конце такта (одним сравнением с
/// `takt_prev_state`, как `c_time::emit_state_time_update`).
pub(super) fn emit_tick_update(
    p: &mut Printer,
    map: &RustMap,
    model: &ModelNode,
    hal_access: &str,
) -> Result<(), Diagnostic> {
    let dwell = needs_dwell(map, model);
    let entry = needs_entry_ms(map, model);
    if !dwell && !entry {
        return Ok(());
    }
    p.ident(&format!("if self.state != self.{PREV_STATE_FIELD} {{"))
        .up()
        .nl();
    if dwell {
        p.ident(&format!("self.{DWELL_FIELD} = 1;")).nl();
    }
    if entry {
        p.ident(&format!(
            "self.{ENTRY_MS_FIELD} = {hal_access}.{NOW_MS_METHOD}();"
        ))
        .nl();
    }
    p.ident(&format!("self.{PREV_STATE_FIELD} = self.state;"))
        .nl();
    // Аккумуляторы `every` обнуляются при входе - период с нуля.
    crate::generator::rust::rust_every::emit_reset(p, model);
    p.down();
    if dwell {
        p.ident("} else {")
            .up()
            .nl()
            .ident(&format!(
                "self.{DWELL_FIELD} = self.{DWELL_FIELD}.wrapping_add(1);"
            ))
            .nl()
            .down();
    }
    p.ident("}").nl();
    Ok(())
}

/// Строит выражение-условие выдержки `after Nms` (профиль "часы") - сравнение разностью
/// с обёрткой. `hal` - получатель HAL-вызова (`self.hal`/`hal`).
pub(super) fn clock_after_expr(hal: &str, units: u64) -> String {
    format!("{hal}.{NOW_MS_METHOD}().wrapping_sub(self.{ENTRY_MS_FIELD}) >= {units}")
}

/// Строит выражение-условие тактовой выдержки (`takt_dwell >= N`).
pub(super) fn dwell_after_expr(units: u64) -> String {
    format!("self.{DWELL_FIELD} >= {units}")
}

/// То же для **вычисляемой** выдержки: справа стоит выражение в миллисекундах, уже
/// переведённое вызывающим в единицы счётчика.
pub(super) fn dwell_after_dynamic(expr: &str) -> String {
    format!("self.{DWELL_FIELD} >= {expr}")
}

/// Вычисляемая выдержка в профиле "часы": разность меток сравнивается с выражением в
/// миллисекундах.
///
/// Выражение **расширяется до `u64`**: метка времени исправлениеирована в `u64` (`now_ms`
/// отдаёт его), а `duration` в целях - целое **32-битное** число миллисекунд. Без
/// расширения `rustc` отвечает `E0308: expected u64, found u32` при нулевом коде
/// возврата `taktc` (замер 0459).
///
/// Расширение, а не приведение метки вниз: усечение `now_ms` до 32 бит поменяло бы
/// момент переполнения счётчика, то есть **поведение**.
pub(super) fn clock_after_dynamic(hal: &str, expr: &str) -> String {
    format!("{hal}.{NOW_MS_METHOD}().wrapping_sub(self.{ENTRY_MS_FIELD}) >= u64::from({expr})")
}

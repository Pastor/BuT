//! Механизм времени цели `sv` (синтезируемый SystemVerilog).
//!
//! Два профиля:
//! - **"часы"** - служебный вход `time_ms` (внешний источник, как `clk`/`en`);
//!   метка входа `<lvl>_takt_entry` латчит `time_ms`, условие - разностью.
//! - **"такты"** - счётчик тактов `<lvl>_takt_dwell` (как цель `c`), условие `>= D`.
//!
//! **Капкан цели:** `always_comb` вычисляет `_next` счётчика и метки из регистров
//! `state` и `prev_state`, а не из `state_next` - иначе выходит комбинационная петля
//! `UNOPTFLAT`, - тогда как условие выдержки читает `_next`: оно уже учитывает текущий
//! такт, и чтение регистра сдвинуло бы выдержку на такт молча. Роль `prev_state` та же,
//! что у `takt_prev_state` в цели `c`: разорвать зависимость обнаружения входа от
//! `state_next`.
//!
//! Задержек `#` и обращений к `$time` в выводе нет никогда.

use crate::diagnostics::Diagnostic;
use crate::diagnostics::lang::keys;
use crate::generator::indent::Printer;
use crate::generator::sv::sv_fsm::Reg;
use crate::generator::sv::sv_map::SvMap;
use crate::msg;
use crate::semantic::ModelNode;
use crate::semantic::duration::{TimeProfile, counter_bits, units_or_diagnostic};
use crate::semantic::minimap::Name;
use crate::semantic::time_ast::{
    model_tree_uses_duration_after, model_tree_uses_every, model_uses_duration_after,
    model_uses_every, model_uses_tick_after,
};

/// Профиль модели - "часы"?
pub(crate) fn is_clock(map: &SvMap) -> bool {
    matches!(map.time_profile(), TimeProfile::Clock)
}

/// Длительностный `after Nms` **или** периодический `every Nms`: обе величины -
/// длительности, требуют одну инфраструктуру времени уровня.
fn uses_duration_time(model: &ModelNode) -> bool {
    model_uses_duration_after(model) || model_uses_every(model)
}

/// Нужен ли счётчик тактов у уровня: тактовая выдержка `after Nt` (любой профиль) либо
/// длительностная `after Nms`/`every Nms` в профиле "такты".
pub(crate) fn needs_dwell(map: &SvMap, model: &ModelNode) -> bool {
    model_uses_tick_after(model) || (!is_clock(map) && uses_duration_time(model))
}

/// Нужна ли метка времени у уровня: профиль "часы" + `after Nms`/`every Nms`.
pub(crate) fn needs_entry(map: &SvMap, model: &ModelNode) -> bool {
    is_clock(map) && uses_duration_time(model)
}

/// Нужен ли служебный вход `time_ms` модулю: профиль "часы" + длительностная
/// выдержка/период где-либо в дереве (вход один на модуль после уплощения).
pub(crate) fn needs_time_port(map: &SvMap, root: &ModelNode) -> bool {
    is_clock(map) && (model_tree_uses_duration_after(root) || model_tree_uses_every(root))
}

/// Разрядность метки/счётчика по максимуму `after` **дерева** модели.
///
/// Один источник для объявления регистра, входа `time_ms` и сравнения: разойдись они,
/// поле оказалось бы уже сравнения - и выдержка молча переполнилась бы.
pub(crate) fn time_bits(map: &SvMap) -> Result<u8, Diagnostic> {
    let max = match map.root_model_node() {
        Some(model) => max_units_in_tree(map, &model.borrow())?,
        None => 0,
    };
    Ok(counter_bits(max).unwrap_or(64))
}

/// Максимум единиц профиля по выдержкам `after` этой модели и вложенных.
fn max_units_in_tree(map: &SvMap, model: &ModelNode) -> Result<u64, Diagnostic> {
    let mut max = 0u64;
    for state in model.states.values() {
        for reference in state.references() {
            // Вычисляемая выдержка: порог появляется лишь в такте, поэтому регистр
            // обязан вмещать любое представимое значение - иначе сравнение усекло бы
            // старшие биты **молча** (в RTL шире/уже - не ошибка, а тихое обрезание).
            if matches!(reference.cond, crate::semantic::ConditionNode::AfterExpr(_)) {
                max = max.max(u64::from(u32::MAX));
            }
            if let crate::semantic::ConditionNode::After(nanos) = reference.cond {
                max = max.max(units_or_diagnostic(
                    nanos,
                    map.time_profile(),
                    crate::diagnostics::Location::Codegen,
                    &msg!(keys::GEN_WHAT_AFTER),
                )?);
            }
        }
        // Периоды `every` делят ширину регистров времени - учитываем.
        for block in state.named_blocks() {
            if let Some((period_nanos, _)) = block.every_period() {
                max = max.max(units_or_diagnostic(
                    period_nanos,
                    map.time_profile(),
                    crate::diagnostics::Location::Codegen,
                    &msg!(keys::GEN_WHAT_EVERY_PERIOD),
                )?);
            }
        }
    }
    for nested in model.models.values() {
        max = max.max(max_units_in_tree(map, &nested.borrow())?);
    }
    Ok(max)
}

/// Имя регистра счётчика тактов уровня.
pub(crate) fn dwell_reg(model: &Name) -> String {
    format!("{}_takt_dwell", model.unique_lowercase_snakecase())
}

/// Имя регистра метки времени входа уровня (профиль "часы").
pub(crate) fn entry_reg(model: &Name) -> String {
    format!("{}_takt_entry", model.unique_lowercase_snakecase())
}

/// Имя регистра "состояние предыдущего такта" уровня.
pub(crate) fn prev_state_reg(model: &Name) -> String {
    format!("{}_takt_prev_state", model.unique_lowercase_snakecase())
}

/// Служебный вход времени модуля.
pub(crate) const TIME_MS_PORT: &str = "time_ms";

/// Имя регистра-аккумулятора `every`-блока уровня.
pub(crate) fn every_reg(model: &Name, idx: usize) -> String {
    format!("{}_takt_every{idx}", model.unique_lowercase_snakecase())
}

/// Периодический блок `every` модели: глобальный (по модели) индекс, состояние, период,
/// тело.
pub(crate) struct EveryBlock<'a> {
    pub(crate) idx: usize,
    pub(crate) state: String,
    pub(crate) period_nanos: i64,
    pub(crate) body: &'a crate::semantic::StatementNode,
}

/// Перечисляет `every`-блоки модели с индексом (детерминированно - `states` в
/// `BTreeMap`-порядке, блоки в порядке объявления). Индекс - сквозной по модели.
pub(crate) fn model_every(model: &ModelNode) -> Vec<EveryBlock<'_>> {
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

/// Уровень (модель) с механизмом времени: имена регистров и профиль.
pub(crate) struct TimeLevel {
    /// Модель-уровень (для префикса имён регистров).
    model: Name,
    /// Имя регистра состояния этого уровня.
    state_reg: String,
    /// Нужен ли счётчик тактов (`<lvl>_takt_dwell`).
    dwell: bool,
    /// Нужна ли метка времени входа (`<lvl>_takt_entry`, профиль "часы").
    entry: bool,
    /// Разрядность метки/счётчика.
    bits: u8,
    /// Регистры-аккумуляторы `every` уровня - для сброса при входе.
    every_regs: Vec<String>,
}

/// Заводит регистры времени уровня: счётчик/метка + метка предыдущего состояния
/// (детекция входа). Получают объявление, `_next`, сброс и защёлкивание генериком `Reg` -
/// как `<state>_step`. Имена уровня передаются готовыми
/// (`enum_name`/`end_var`/`state_reg`), чтобы не тянуть в `sv_time` их построители.
#[allow(clippy::too_many_arguments)]
pub(crate) fn push_time_regs(
    regs: &mut Vec<Reg>,
    registered: &mut std::collections::BTreeSet<String>,
    time_levels: &mut Vec<TimeLevel>,
    map: &SvMap,
    name: &Name,
    model: &ModelNode,
    enum_name: &str,
    end_var: &str,
    state_reg: &str,
) -> Result<(), Diagnostic> {
    let dwell = needs_dwell(map, model);
    let entry = needs_entry(map, model);
    let every = model_every(model);
    if !dwell && !entry && every.is_empty() {
        return Ok(());
    }
    let bits = time_bits(map)?;
    let word = format!("logic [{}:0]", bits.saturating_sub(1));
    let mut push = |sig: String, prefix: String, reset: String| {
        registered.insert(sig.clone());
        regs.push(Reg {
            name: sig,
            prefix,
            suffix: String::new(),
            reset,
            declare_reg: true,
            leaves: Vec::new(),
            default: None,
        });
    };
    if dwell {
        push(dwell_reg(name), word.clone(), "'0".to_string());
    }
    if entry {
        push(entry_reg(name), word.clone(), "'0".to_string());
    }
    // Аккумуляторы `every`: регистр на блок, сброс '0.
    let mut every_regs = Vec::new();
    for e in &every {
        let reg = every_reg(name, e.idx);
        push(reg.clone(), word.clone(), "'0".to_string());
        every_regs.push(reg);
    }
    // `prev_state` сбрасывается в END-сентинел (не в стартовое): sv без INIT, и на
    // Первом такте `state(start) != prev(END)` даёт вход.
    push(
        prev_state_reg(name),
        enum_name.to_string(),
        end_var.to_string(),
    );
    time_levels.push(TimeLevel {
        model: name.clone(),
        state_reg: state_reg.to_string(),
        dwell,
        entry,
        bits,
        every_regs,
    });
    Ok(())
}

/// Перекрывает умолчание `_next` регистров времени явной комбинационной формулой. Вход
/// в состояние - `state != prev_state` (оба регистры): счётчик сбрасывается в 1 и
/// растёт, метка латчит `time_ms`.
pub(crate) fn emit_time_updates(p: &mut Printer, levels: &[TimeLevel]) -> Result<(), Diagnostic> {
    for lvl in levels {
        let state = &lvl.state_reg;
        let prev = prev_state_reg(&lvl.model);
        let entered = format!("{} != {}", state, prev);
        p.ident(&format!("{}_next = {};", prev, state)).nl();
        if lvl.dwell {
            let dwell = dwell_reg(&lvl.model);
            p.ident(&format!(
                "{dwell}_next = ({entered}) ? {b}'d1 : {dwell} + {b}'d1;",
                b = lvl.bits
            ))
            .nl();
        }
        if lvl.entry {
            let entry = entry_reg(&lvl.model);
            p.ident(&format!(
                "{entry}_next = ({entered}) ? {TIME_MS_PORT} : {entry};"
            ))
            .nl();
        }
        // Аккумуляторы `every`: умолчание `_next` - сброс '0 при входе, иначе
        // удержание. Срабатывание переопределит его в ветви состояния.
        for reg in &lvl.every_regs {
            p.ident(&format!("{reg}_next = ({entered}) ? '0 : {reg};"))
                .nl();
        }
    }
    if !levels.is_empty() {
        p.nl();
    }
    Ok(())
}

/// Выражение `elapsed` уровня, читающее `_next` (как `after_guard`): "часы" - `time_ms -
/// <entry>_next`, "такты" - `<dwell>_next`. `None`, если у уровня нет инфраструктуры
/// времени (не должно случаться при наличии `every`).
pub(crate) fn elapsed_next_expr(levels: &[TimeLevel], map: &SvMap, model: &Name) -> Option<String> {
    let level = levels.iter().find(|l| l.model.unique() == model.unique())?;
    if is_clock(map) {
        Some(format!(
            "({TIME_MS_PORT} - {}_next)",
            entry_reg(&level.model)
        ))
    } else {
        Some(format!("{}_next", dwell_reg(&level.model)))
    }
}

/// Печатает проверка срабатывания `every`-блока в ветви состояния `always_comb`: `if
/// ((elapsed - reg_next) >= period) begin ... reg_next += period; end`. Тело печатает
/// `emit_body` (замыкание вызывающего - у него доступ к `Scope`).
pub(crate) fn emit_every_gate(
    p: &mut Printer,
    levels: &[TimeLevel],
    map: &SvMap,
    model: &Name,
    idx: usize,
    period_nanos: i64,
    emit_body: impl FnOnce(&mut Printer) -> Result<(), Diagnostic>,
) -> Result<(), Diagnostic> {
    let Some(elapsed) = elapsed_next_expr(levels, map, model) else {
        return Ok(());
    };
    let units = units_or_diagnostic(
        period_nanos,
        map.time_profile(),
        crate::diagnostics::Location::Codegen,
        &msg!(keys::GEN_WHAT_EVERY_PERIOD),
    )?;
    let reg = every_reg(model, idx);
    p.ident(&format!("if (({elapsed} - {reg}_next) >= {units}) begin"))
        .up()
        .nl();
    emit_body(p)?;
    p.ident(&format!("{reg}_next = {reg}_next + {units};"))
        .nl()
        .down()
        .ident("end")
        .nl();
    Ok(())
}

/// Строит guard выдержки `after` уровня, читая `_next` счётчика/метки.
///
/// `Some(guard)` для `After`/`AfterTicks`; `None` для прочих условий (их печатает общий
/// `print_condition`). Читается именно `_next`: оно уже учло текущий такт - чтение
/// регистра сдвинуло бы выдержку молча.
pub(crate) fn after_guard(
    levels: &[TimeLevel],
    map: &SvMap,
    model: &Name,
    cond: &crate::semantic::ConditionNode,
    scope: &super::sv_expr::Scope,
) -> Option<Result<String, Diagnostic>> {
    use crate::semantic::ConditionNode;
    let level = levels.iter().find(|l| l.model.unique() == model.unique())?;
    match cond {
        ConditionNode::After(nanos) => {
            let units = match units_or_diagnostic(
                *nanos,
                map.time_profile(),
                crate::diagnostics::Location::Codegen,
                &msg!(keys::GEN_WHAT_AFTER),
            ) {
                Ok(u) => u,
                Err(e) => return Some(Err(e)),
            };
            if is_clock(map) {
                Some(Ok(format!(
                    "({TIME_MS_PORT} - {entry}_next) >= {units}",
                    entry = entry_reg(&level.model)
                )))
            } else {
                Some(Ok(format!(
                    "{dwell}_next >= {units}",
                    dwell = dwell_reg(&level.model)
                )))
            }
        }
        ConditionNode::AfterTicks(ticks) => Some(Ok(format!(
            "{dwell}_next >= {ticks}",
            dwell = dwell_reg(&level.model)
        ))),
        // Вычисляемая выдержка: справа - выражение в миллисекундах. Читается `_next`,
        // как и у константной формы: чтение регистра сдвинуло бы выдержку на такт
        // **молча** (главный капкан цели).
        ConditionNode::AfterExpr(inner) => {
            let expr = match super::sv_expr::print_condition(inner, scope) {
                Ok(e) => e,
                Err(e) => return Some(Err(e)),
            };
            let multiplier = match crate::semantic::duration::ticks_per_milli(
                map.time_profile(),
                crate::diagnostics::Location::Codegen,
            ) {
                Ok(m) => m,
                Err(e) => return Some(Err(e)),
            };
            let dwell = dwell_reg(&level.model);
            match multiplier {
                Some(1) => Some(Ok(format!("{dwell}_next >= ({expr})"))),
                Some(k) => Some(Ok(format!("{dwell}_next >= ({expr}) * {k}"))),
                None => Some(Ok(format!(
                    "({TIME_MS_PORT} - {entry}_next) >= ({expr})",
                    entry = entry_reg(&level.model)
                ))),
            }
        }
        _ => None,
    }
}

//! Механизм времени цели `c`.
//!
//! Здесь - то, что цель `c` знает о времени: разрядность счётчика выдержки и правила
//! его обновления. Арифметика длительности **не здесь**: пересчёт "наносекунды ->
//! единицы профиля" живёт в общем слое
//! [`semantic::duration`](crate::semantic::duration) - иначе цели дали бы разное число
//! тактов для одного текста.
//!
//! Вынесено отдельным модулем не по вкусу: `c_header.rs` вместе с этим кодом давал 1004
//! строки при лимите 1000, и проверка размера отказал.

use crate::diagnostics::{Diagnostic, Location};
use crate::generator::c::c_expr::condition::{DWELL_FIELD, ENTRY_MS_FIELD, PREV_STATE_FIELD};
use crate::generator::c::c_map::CMap;
use crate::generator::indent::Printer;
use crate::semantic::ModelNode;
use crate::semantic::time_ast::{
    model_uses_duration_after, model_uses_every, model_uses_tick_after,
};

/// Профиль модели - "часы" (внешний источник времени)?
pub(in crate::generator::c) fn is_clock_profile(map: &CMap) -> bool {
    matches!(
        map.time_profile(),
        crate::semantic::duration::TimeProfile::Clock
    )
}

/// Использует ли модель длительностную выдержку `after Nms` **или** периодический блок
/// `every Nms`. Оба меряются длительностью, поэтому требуют одну инфраструктуру
/// времени: счётчик тактов (профиль "такты") либо метку входа + `now_ms` (профиль
/// "часы"). Тактовая `after Nt` идёт отдельным путём.
pub(in crate::generator::c) fn uses_duration_time(model: &ModelNode) -> bool {
    model_uses_duration_after(model) || model_uses_every(model)
}

/// Нужен ли колбэк `now_ms`: профиль "часы" + длительностная выдержка `after Nms`
/// где-либо в дереве модели (колбэк - на корневой структуре, а выдержка бывает во
/// вложенной под-модели). Тактовая `after Nt` не требует.
pub(super) fn needs_now_ms(map: &CMap, model: &ModelNode) -> bool {
    is_clock_profile(map)
        && (crate::semantic::time_ast::model_tree_uses_duration_after(model)
            || crate::semantic::time_ast::model_tree_uses_every(model))
}

/// Печатает поля структуры для механизма времени.
///
/// Профиль "такты" -> `takt_dwell` (счётчик); профиль "часы" -> `takt_entry_ms` (метка
/// входа, сравнение разностью с `now_ms`). Тактовая выдержка `after Nt` считает
/// `takt_dwell` в любом профиле. Общее поле `takt_prev_state` - для распознавания входа
/// одним сравнением в конце такта.
pub(super) fn emit_state_time_fields(
    printer: &mut Printer,
    map: &CMap,
    model: &ModelNode,
) -> Result<(), Diagnostic> {
    let clock = is_clock_profile(map);
    let uses_dur = uses_duration_time(model);
    let uses_tick = model_uses_tick_after(model);
    let needs_dwell = uses_tick || (!clock && uses_dur);
    let needs_entry_ms = clock && uses_dur;
    if needs_dwell {
        let bits =
            crate::semantic::duration::counter_bits(counter_ticks(map, model)?).unwrap_or(64);
        printer.ident(&format!("uint{bits}_t {DWELL_FIELD};")).nl();
    }
    if needs_entry_ms {
        let bits = clock_marker_bits(map)?;
        printer
            .ident(&format!("uint{bits}_t {ENTRY_MS_FIELD};"))
            .nl();
    }
    if needs_dwell || needs_entry_ms {
        printer.ident(&format!("unsigned {PREV_STATE_FIELD};")).nl();
    }
    Ok(())
}

/// Печатает обновление счётчика/метки времени в конце такта.
///
/// Вход в состояние (`state != prev`) сбрасывает счётчик тактов в 1 и латчит метку
/// `now_ms` (профиль "часы"); иначе счётчик тактов растёт. Стоит **одним** сравнением,
/// а не рядом с каждым присваиванием `model->state`, - второй экземпляр логики
/// неминуемо разошёлся бы с первым. Для профиля "такты" вывод байт-в-байт прежний.
pub(super) fn emit_state_time_update(
    printer: &mut Printer,
    map: &CMap,
    model: &ModelNode,
    hal_ptr: &str,
) -> Result<(), Diagnostic> {
    let clock = is_clock_profile(map);
    let uses_dur = uses_duration_time(model);
    let uses_tick = model_uses_tick_after(model);
    let needs_dwell = uses_tick || (!clock && uses_dur);
    let needs_entry_ms = clock && uses_dur;
    if !needs_dwell && !needs_entry_ms {
        return Ok(());
    }
    printer
        .ident(&format!(
            "if ((unsigned)model->state != model->{PREV_STATE_FIELD}) {{"
        ))
        .up()
        .nl();
    if needs_dwell {
        printer.ident(&format!("model->{DWELL_FIELD} = 1;")).nl();
    }
    if needs_entry_ms {
        let bits = clock_marker_bits(map)?;
        printer
            .ident(&format!(
                "model->{ENTRY_MS_FIELD} = (uint{bits}_t){hal_ptr}->{}({hal_ptr}->userdata);",
                crate::generator::c::FUNCTION_TIME_NOW_MS
            ))
            .nl();
    }
    printer
        .ident(&format!(
            "model->{PREV_STATE_FIELD} = (unsigned)model->state;"
        ))
        .nl();
    // Аккумуляторы `every` обнуляются при входе - период отсчитывается заново, как и
    // `elapsed` от только что залатченной метки/сброшенного счётчика.
    crate::generator::c::c_every::emit_reset(printer, model);
    printer.down();
    if needs_dwell {
        printer
            .ident("} else {")
            .up()
            .nl()
            .ident(&format!("model->{DWELL_FIELD}++;"))
            .nl()
            .down();
    }
    printer.ident("}").nl();
    Ok(())
}

/// Печатает инициализацию счётчика/метки времени в `_init`.
///
/// Стартовое состояние входит "до такта 1", поэтому метку `now_ms` латчим здесь: иначе
/// первая выдержка в профиле "часы" мерилась бы от нуля абсолютного времени, а не от
/// входа. `takt_prev_state` совпадает с начальным состоянием - вход в стартовое на
/// такте 1 распознаётся сменой состояния.
pub(super) fn emit_state_time_init(
    printer: &mut Printer,
    map: &CMap,
    model: &ModelNode,
    init_const: &str,
    hal_ptr: &str,
) -> Result<(), Diagnostic> {
    let clock = is_clock_profile(map);
    let uses_dur = uses_duration_time(model);
    let uses_tick = model_uses_tick_after(model);
    let needs_dwell = uses_tick || (!clock && uses_dur);
    let needs_entry_ms = clock && uses_dur;
    if !needs_dwell && !needs_entry_ms {
        return Ok(());
    }
    if needs_dwell {
        printer.ident(&format!("model->{DWELL_FIELD} = 0;")).nl();
    }
    if needs_entry_ms {
        let bits = clock_marker_bits(map)?;
        printer
            .ident(&format!(
                "model->{ENTRY_MS_FIELD} = (uint{bits}_t){hal_ptr}->{}({hal_ptr}->userdata);",
                crate::generator::c::FUNCTION_TIME_NOW_MS
            ))
            .nl();
    }
    printer
        .ident(&format!(
            "model->{PREV_STATE_FIELD} = (unsigned){init_const};"
        ))
        .nl();
    // Аккумуляторы `every` стартуют с нуля - как счётчик/метка.
    crate::generator::c::c_every::emit_reset(printer, model);
    Ok(())
}

/// Максимальная выдержка модели в единицах профиля - для выбора разрядности.
///
/// Разрядность счётчика выбирает **компилятор** (T7 отчёта `DIFF.md`): счётчик,
/// объявленный узко, молча зациклил бы выдержку. Пересчёт идёт через общий слой
/// `semantic::duration`, а не своей арифметикой.
pub(super) fn counter_ticks(
    map: &CMap,
    model: &crate::semantic::ModelNode,
) -> Result<u64, Diagnostic> {
    let profile = map.time_profile();
    let mut max = 0u64;
    for state in model.states.values() {
        for reference in state.references() {
            // Вычисляемая выдержка значения не имеет до такта, поэтому счётчик обязан
            // вмещать **любое** представимое: иначе сравнение либо усечёт значение,
            // либо (в цели `rust`) не скомпилируется из-за разной ширины операндов.
            if matches!(
                &reference.cond,
                crate::semantic::ConditionNode::AfterExpr(_)
            ) {
                max = max.max(u64::from(u32::MAX));
            }
            let nanos = match &reference.cond {
                crate::semantic::ConditionNode::After(nanos) => Some(*nanos),
                _ => None,
            };
            if let Some(nanos) = nanos {
                let units = crate::semantic::duration::units_or_diagnostic(
                    nanos,
                    profile,
                    Location::Codegen,
                    "выдержка 'after'",
                )?;
                max = max.max(units);
            }
        }
        // Периоды `every` тоже определяют ширину: аккумулятор делит её с
        // меткой/счётчиком, а `elapsed - consumed` обязан не переполниться раньше
        // срабатывания.
        for block in state.named_blocks() {
            if let Some((period_nanos, _)) = block.every_period() {
                let units = crate::semantic::duration::units_or_diagnostic(
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

/// Ширина метки времени профиля "часы" - по максимуму `after` корневой модели,
/// `counter_bits` от него.
///
/// Объявление поля [`ENTRY_MS_FIELD`](super::c_expr::condition::ENTRY_MS_FIELD) и
/// условие сравнения обязаны брать ширину из **одного** места: разойдись они, поле
/// оказалось бы уже сравнения - и выдержка стала бы молча неверной. Отсюда общий
/// помощник, а не два независимых расчёта.
///
/// Берётся от **корня**: вложенная композиция с `after` глубже корня шириной не покрыта
/// (в корпусе такого нет - время цели `c` живёт на плоских моделях).
pub(super) fn clock_marker_bits(map: &CMap) -> Result<u8, Diagnostic> {
    let max_ms = match map.root_model_node() {
        Some(model) => max_units_in_tree(map, &model.borrow())?,
        None => 0,
    };
    Ok(crate::semantic::duration::counter_bits(max_ms).unwrap_or(64))
}

/// Максимум единиц выдержки `after` по всему дереву модели (сама + вложенные).
///
/// Метка времени и поле лежат на разных структурах композиции, но ширину обязаны брать
/// одну - по максимуму дерева, иначе вложенная выдержка получила бы поле уже сравнения
/// и стала бы молча неверной.
fn max_units_in_tree(map: &CMap, model: &ModelNode) -> Result<u64, Diagnostic> {
    let mut max = counter_ticks(map, model)?;
    for nested in model.models.values() {
        max = max.max(max_units_in_tree(map, &nested.borrow())?);
    }
    Ok(max)
}

/// Отпечаток контракта частоты для заголовка C, готовый блок.
///
/// `Some(текст)` - модель объявила `clock`, значит контракт частоты подтверждён
/// (кодоген сюда доходит только после `resolve_profile`, вернувшего `Ticks`), и его
/// нужно закрепить статическим утверждением. `#ifndef`-умолчание держит проверка `cc -c`
/// зелёным (частота по умолчанию совпадает); интегратор, задав `TAKT_TICK_HZ` иным
/// значением, ловит несовпадение при сборке прошивки - там, где частоту задаёт уже
/// система тактирования, а не компилятор. `None` - частоту задал лишь `--tick-hz` без
/// объявления в модели: закреплять нечего.
pub(super) fn clock_contract_block(map: &CMap) -> Option<String> {
    let hertz = map.root_model_node().and_then(|m| m.borrow().clock_hz)?;
    Some(format!(
        "/* Контракт частоты Takt (clock): объявленная моделью частота. */\n\
         #define TAKT_REQUIRED_CLOCK_HZ {hertz}u\n\
         #ifndef TAKT_TICK_HZ\n\
         #define TAKT_TICK_HZ TAKT_REQUIRED_CLOCK_HZ\n\
         #endif\n\
         _Static_assert(TAKT_TICK_HZ == TAKT_REQUIRED_CLOCK_HZ,\n    \
         \"частота тактирования не совпадает с объявленной моделью Takt\");\n\n"
    ))
}

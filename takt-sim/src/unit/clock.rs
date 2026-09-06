//! Модельные часы узла симуляции.
//!
//! Вынесено из `unit/mod.rs`: файл вместе с этим кодом давал 1048 строк при лимите
//! 1000, а часы - самостоятельная тема. Дочерний модуль по отношению к `unit`, поэтому
//! приватные поля `UnitKind::Node` ему видны.
//!
//! Здесь **нет** часов реального мира: время ставит `runner` (`set_time_ns`), а счётчик
//! тактов растёт в конце такта. Иначе трасса перестала бы воспроизводиться, и все
//! потактовые сверки стали бы мигающими.

use super::{Unit, UnitKind};

impl Unit {
    /// Ставит модельное время (наносекунды) во **все** узлы дерева.
    ///
    /// Рекурсивно, как `set_value`: ветви композиции живут в одном времени - иначе
    /// выдержка в одной ветви шла бы по своим часам, и трасса перестала бы быть
    /// воспроизводимой.
    pub fn set_time_ns(&mut self, now_ns: i64) {
        match &mut self.0 {
            UnitKind::Node { time_ns, .. } => *time_ns = now_ns,
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => {
                for unit in units {
                    unit.borrow_mut().set_time_ns(now_ns);
                }
            }
            UnitKind::None => {}
        }
    }

    /// Ставит стенд внешних функций во **все** узлы дерева.
    ///
    /// Рекурсивно, как `set_time_ns`, и по той же причине: ветви композиции живут в
    /// одном шаге сценария - иначе одна ветвь читала бы стенд, а другая отвечала бы
    /// `SIM-019`.
    pub(crate) fn set_extern_stubs(&mut self, stubs: crate::context::ExternStubs) {
        match &mut self.0 {
            UnitKind::Node { context, .. } => {
                if let Some(ctx) = context {
                    ctx.borrow_mut().set_extern_stubs(stubs);
                }
            }
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => {
                for unit in units {
                    unit.borrow_mut().set_extern_stubs(stubs.clone());
                }
            }
            UnitKind::None => {}
        }
    }

    /// Сколько модельного времени прошло с входа в текущее состояние.
    pub(crate) fn since_state_entry_ns(&self) -> i64 {
        match &self.0 {
            UnitKind::Node {
                time_ns,
                state_entered_ns,
                ..
            } => time_ns.saturating_sub(*state_entered_ns),
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => units
                .first()
                .map_or(0, |u| u.borrow().since_state_entry_ns()),
            UnitKind::None => 0,
        }
    }

    /// Тактов с входа в текущее состояние.
    pub(crate) fn ticks_in_state(&self) -> u64 {
        match &self.0 {
            UnitKind::Node { ticks_in_state, .. } => *ticks_in_state,
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => {
                units.first().map_or(0, |u| u.borrow().ticks_in_state())
            }
            UnitKind::None => 0,
        }
    }

    /// Отмечает вход в состояние текущим модельным временем.
    pub(super) fn mark_state_entry(&mut self) {
        if let UnitKind::Node {
            time_ns,
            state_entered_ns,
            ticks_in_state,
            every_consumed,
            ..
        } = &mut self.0
        {
            *state_entered_ns = *time_ns;
            // Такт входа - нулевой: на нём с момента входа не прошло ни одного такта
            // (как и модельного времени).
            *ticks_in_state = 0;
            // Периодические блоки `every` отсчитываются заново от входа: очистка -
            // сигнал `execute_every` переинициализировать аккумулятор под число блоков
            // нового состояния.
            every_consumed.clear();
        }
    }

    /// Увеличивает счётчик тактов, проведённых в состоянии.
    ///
    /// Зовётся в **конце** такта: значение, видимое условиям на такте M, равно числу
    /// тактов с входа - ровно как счётчик `takt_dwell` порождённого C.
    pub(super) fn advance_state_ticks(&mut self) {
        match &mut self.0 {
            UnitKind::Node { ticks_in_state, .. } => {
                *ticks_in_state = ticks_in_state.saturating_add(1);
            }
            UnitKind::Parallel { units, .. } | UnitKind::Sequential { units, .. } => {
                for unit in units {
                    unit.borrow_mut().advance_state_ticks();
                }
            }
            UnitKind::None => {}
        }
    }
}

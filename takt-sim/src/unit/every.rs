//! Периодические блоки `every` симулятора.
//!
//! Вынесено из `unit/mod.rs` (лимит размера модуля). Дочерний модуль по отношению к
//! `unit`, поэтому приватные поля `UnitKind::Node` (`state_every`/`every_consumed`) ему
//! видны. Скрытое состояние `every_consumed` - эталон для целей: тело срабатывает,
//! когда прошедшее с входа время догоняет очередной период.

use super::{Execution, Unit, UnitKind};
use takt_lang::diagnostics::Diagnostic;

impl Unit {
    /// Исполняет периодические блоки `every` текущего состояния.
    ///
    /// Каждый блок несёт период `P` (нс) и поглощённое время `consumed[i]` (сброшено
    /// при входе). Прошло с входа `elapsed = since_state_entry_ns()`; если `elapsed -
    /// consumed[i] >= P`, тело исполняется и `consumed[i] += P`. Один запуск за такт
    /// (как таймерное прерывание), скрытое состояние - `consumed`, видимо в трассе.
    /// Рекурсия в дочерние - как у `execution`.
    pub fn execute_every(&mut self) -> Result<(), Diagnostic> {
        let elapsed = self.since_state_entry_ns();
        // Клонируем тела текущего состояния, не удерживая заимствование self.
        let blocks: Vec<(i64, Vec<Execution>)> = match &self.0 {
            UnitKind::Node {
                state: Some(s),
                state_every,
                ..
            } => state_every.get(s.as_str()).cloned().unwrap_or_default(),
            _ => Vec::new(),
        };
        if !blocks.is_empty() {
            // Размер аккумулятора приводим к числу блоков состояния: вход в состояние
            // очищает вектор (`mark_state_entry`), поэтому первое исполнение после
            // входа инициализирует его нулями.
            if let UnitKind::Node { every_consumed, .. } = &mut self.0
                && every_consumed.len() != blocks.len()
            {
                *every_consumed = vec![0; blocks.len()];
            }
            for (i, (period, body)) in blocks.iter().enumerate() {
                let consumed = match &self.0 {
                    UnitKind::Node { every_consumed, .. } => every_consumed[i],
                    _ => 0,
                };
                if *period > 0 && elapsed.saturating_sub(consumed) >= *period {
                    for f in body {
                        f(self)?;
                    }
                    if let UnitKind::Node { every_consumed, .. } = &mut self.0 {
                        every_consumed[i] = every_consumed[i].saturating_add(*period);
                    }
                }
            }
        }
        // Рекурсия в дочерние - ошибка ребёнка поднимается наверх (как execution).
        match &self.0 {
            UnitKind::Parallel { units, .. } => {
                let units = units.clone();
                for u in units.iter() {
                    u.borrow_mut().execute_every()?;
                }
            }
            UnitKind::Sequential { units, index, .. } => {
                if *index < units.len() {
                    units[*index].clone().borrow_mut().execute_every()?;
                }
            }
            UnitKind::Node { .. } | UnitKind::None => {}
        }
        Ok(())
    }
}

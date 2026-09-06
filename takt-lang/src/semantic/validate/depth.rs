//! Ограничение глубины рекурсии семантических обходов.
//!
//! ## Как устроено
//!
//! Счётчик глубины - потоковый (`thread_local`): библиотеку зовут из LSP, где потоков
//! несколько, и общий счётчик считал бы чужую работу. Вход в рекурсию - [`enter`];
//! выход учитывается **автоматически** при разрушении [`DepthGuard`], поэтому ранний
//! возврат по `?` не "залипает" на счётчике.

use std::cell::Cell;

use crate::diagnostics::{Diagnostic, Location};

/// Предельная глубина вложенности выражений, условий и операторов.
///
/// Обоснование значения - в описании модуля (замер стека).
pub const MAX_NESTING_DEPTH: usize = 32;

thread_local! {
    /// Текущая глубина рекурсии семантического обхода в этом потоке.
    static DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// Отметка о входе в рекурсию: уменьшает счётчик при разрушении.
///
/// Значение возвращается вызывающему намеренно - держать его до конца рекурсивного
/// вызова и есть учёт выхода.
#[derive(Debug)]
#[must_use = "глубина учитывается, пока жива эта отметка"]
pub struct DepthGuard(());

impl Drop for DepthGuard {
    fn drop(&mut self) {
        DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
    }
}

/// Входит на уровень глубже; отдаёт диагностику `SE-062` при превышении предела.
///
/// `loc` - место в исходнике, если оно известно. У операторов позиция есть не у всех
/// узлов АСД, поэтому параметр необязателен: сообщение без координат честнее выдуманных
/// (позиции `Implicit` координат не имеют).
pub fn enter(loc: Option<Location>) -> Result<DepthGuard, Diagnostic> {
    DEPTH.with(|d| {
        let next = d.get() + 1;
        if next > MAX_NESTING_DEPTH {
            return Err(Diagnostic::error(
                loc.unwrap_or(Location::Implicit),
                format!(
                    "превышен предел вложенности ({MAX_NESTING_DEPTH}): \
                     выражение, условие или оператор вложены слишком глубоко"
                ),
            )
            .with_code("SE-062"));
        }
        d.set(next);
        Ok(DepthGuard(()))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Счётчик обязан возвращаться к нулю: иначе следующая компиляция в том же потоке
    /// (а в LSP их поток переиспользуется) упрётся в чужой остаток.
    #[test]
    fn guard_releases_depth_on_drop() {
        {
            let _a = enter(None).expect("уровень 1");
            let _b = enter(None).expect("уровень 2");
            assert_eq!(DEPTH.with(Cell::get), 2);
        }
        assert_eq!(DEPTH.with(Cell::get), 0);
    }

    /// Предел срабатывает ровно на `MAX_NESTING_DEPTH + 1`, а не "примерно там".
    #[test]
    fn limit_triggers_exactly_after_max_depth() {
        let mut held = Vec::new();
        for level in 1..=MAX_NESTING_DEPTH {
            held.push(enter(None).unwrap_or_else(|_| panic!("уровень {level} обязан пройти")));
        }
        let err = enter(None).expect_err("уровень сверх предела обязан дать диагностику");
        assert_eq!(err.code.as_deref(), Some("SE-062"));
        drop(held);
        assert_eq!(DEPTH.with(Cell::get), 0);
    }

    /// Парный тест к интеграционным тестам (`tests/deep_nesting_tests.rs`): там
    /// предел продублирован числом, потому что модуль `pub(crate)`. Если константу
    /// изменить, покраснеет этот тест - и станет видно, что вторую сторону надо править
    /// тоже.
    #[test]
    fn constant_matches_documented_limit() {
        assert_eq!(
            MAX_NESTING_DEPTH, 32,
            "предел изменён: обновите tests/deep_nesting_tests.rs и обоснование в шапке модуля"
        );
    }

    /// Ранний возврат по `?` не должен "залипать" на счётчике.
    #[test]
    fn early_return_does_not_leak_depth() {
        fn inner() -> Result<(), Diagnostic> {
            let _g = enter(None)?;
            Err(Diagnostic::error(
                Location::Implicit,
                "ранний выход".to_string(),
            ))
        }
        let _ = inner();
        assert_eq!(DEPTH.with(Cell::get), 0);
    }
}

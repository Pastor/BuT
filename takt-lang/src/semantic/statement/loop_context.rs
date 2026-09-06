//! Признак "мы внутри цикла" на построении тел - `SE-132`.
//!
//! # Что и почему
//!
//! `break` и `continue` вне цикла компилятор принимал молча, и дальше потребители
//! расходились.
//!
//! ```takt
//! start Run {
//!     always { x := x + 1; break; }
//!     ref Next: x >= 1;
//! }
//! ```
//!
//! | Потребитель | Что делает |
//! |---|---|
//! | эталон | прерывает тело и **проверяет переходы**: на первом шаге уже в `Next` |
//! | цель `c` | печатает `break;` внутри ветви `case` - управление выходит из `switch` **мимо присваивания состояния**, и автомат не уходит из `Run` никогда |
//!
//! Код возврата `taktc` - нулевой, диагностики нет ни у компилятора, ни у эталона:
//! молчаливое расхождение эталона и цели, то есть класс, ради которого в проекте
//! заведены потактовые сверки.
//!
//! # Как устроено
//!
//! Счётчик вложенности циклов - **потоковый** (`thread_local`), как у
//! [`validate::depth`](crate::semantic::validate::depth): библиотеку зовут из LSP, где
//! потоков несколько, и общий счётчик считал бы чужую работу. Выход из цикла
//! учитывается разрушением [`LoopGuard`], поэтому ранний возврат по `?` не "залипает".

use std::cell::Cell;

use crate::diagnostics::{Diagnostic, Location};

thread_local! {
    /// Глубина вложенности циклов в текущем построении тела.
    static LOOP_DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// Страж тела цикла: пока жив, `break` и `continue` законны.
#[derive(Debug)]
pub struct LoopGuard;

impl Drop for LoopGuard {
    fn drop(&mut self) {
        LOOP_DEPTH.with(|depth| depth.set(depth.get().saturating_sub(1)));
    }
}

/// Вход в тело цикла.
pub fn enter() -> LoopGuard {
    LOOP_DEPTH.with(|depth| depth.set(depth.get() + 1));
    LoopGuard
}

/// Есть ли вокруг цикл.
pub fn inside() -> bool {
    LOOP_DEPTH.with(Cell::get) > 0
}

/// Отказ `SE-132`: прерывание вне цикла.
///
/// `keyword` - слово, которое написал автор (`break` либо `continue`): в тексте
/// диагностики должно стоять оно, а не обобщение - автор ищет свою строку.
pub fn refuse(keyword: &str, loc: Location) -> Diagnostic {
    Diagnostic::error(
        loc,
        format!(
            "'{keyword}' вне цикла: прерывать нечего. Оператор допустим только в \
             теле 'while', 'loop' или 'for'; тело блока и функции завершается \
             само"
        ),
    )
    .with_code("SE-132")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// L1: вне цикла признака нет, внутри - есть, после выхода снова нет.
    #[test]
    fn guard_tracks_nesting() {
        assert!(!inside(), "до входа цикла нет");
        {
            let _outer = enter();
            assert!(inside(), "внутри цикла признак взведён");
            {
                let _inner = enter();
                assert!(inside(), "вложенный цикл признак держит");
            }
            assert!(inside(), "выход из вложенного не снимает внешний");
        }
        assert!(!inside(), "после выхода признак снят");
    }

    /// L2: текст отказа называет слово автора, а не обобщение.
    #[test]
    fn refusal_names_the_keyword() {
        let text = refuse("continue", Location::default()).message;
        assert!(
            text.contains("'continue'"),
            "в тексте нет слова автора: {text}"
        );
    }
}

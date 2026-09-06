//! Пачка диагностик: порядок, уникальность и печать (фича 0130).
//!
//! Пока инструмент сообщал **одну** ошибку за прогон, ни порядок, ни повторы
//! значения не имели. Как только сообщений становится несколько, оба вопроса
//! становятся содержательными:
//!
//! - **порядок** — обход модели идёт по `BTreeMap` (фича 0048), то есть по
//!   алфавиту имён, а не по тексту: в файле с ошибками в состояниях `S` и `Done`
//!   первым сообщается `Done`. Читателю нужен порядок **сверху вниз по файлу**;
//! - **повторы** — одна и та же ошибка может прийти из двух проверок, и
//!   печатать её дважды значит заставить искать несуществующее второе место.
//!
//! Часть модуля `diagnostics` (деление по логике: `mod.rs` упирается в лимит
//! размера).

use super::lang::keys;
use super::{Diagnostic, Location, position_prefix};
use crate::msg;

/// Упорядочивает диагностики по позиции в тексте и убирает точные повторы.
///
/// Порядок — `(файл, смещение начала)`; сортировка **устойчивая**, поэтому у
/// диагностик с одинаковой позицией сохраняется порядок обхода (а он
/// детерминирован — фича 0048). Диагностики без позиции (`Codegen`, `Implicit`,
/// `Builtin`, `CommandLine`) идут **в конец**: они не про место в тексте, и
/// ставить их между привязанными значило бы сбивать чтение сверху вниз.
///
/// Повтором считается полное совпадение тройки «позиция + код + сообщение».
/// Совпадение только текста повтором **не** считается: одна и та же ошибка в
/// двух местах — это две ошибки.
pub fn normalize(mut diagnostics: Vec<Diagnostic>) -> Vec<Diagnostic> {
    diagnostics.sort_by_key(sort_key);
    diagnostics.dedup_by(|a, b| identity(a) == identity(b));
    diagnostics
}

/// Ключ сортировки: файл, затем смещение. Без позиции — в конец.
fn sort_key(diagnostic: &Diagnostic) -> (u32, u32) {
    match diagnostic.loc {
        Location::Source(file_no, start, _) => (file_no, start),
        _ => (u32::MAX, u32::MAX),
    }
}

/// Признак тождественности двух диагностик для дедупликации.
fn identity(diagnostic: &Diagnostic) -> (Location, Option<&str>, &str) {
    (
        diagnostic.loc,
        diagnostic.code.as_deref(),
        diagnostic.message.as_str(),
    )
}

/// Готовая к печати строка ошибки компиляции: позиция, код, сообщение, заметки.
///
/// ⚠️ Формат живёт в **библиотеке**, а не в бинарнике, по той же причине, по
/// которой там же живёт [`position_prefix`] (фича 0053): вид диагностики — её
/// собственное свойство. Копия формата в `taktc` уже расходилась однажды
/// (задача 0028-01: цели `c`/`st` печатали сообщение без кода, `c-hal`/`st-at` —
/// с кодом, а заметки не печатал никто).
///
/// Функция **не печатает** — возвращает текст. Печать остаётся за вызывающим:
/// библиотека, пишущая в `stderr`, лишает его выбора (этот дефект известен по
/// предупреждениям цели `sv`, фича 0064).
pub fn format_compile_error(diagnostic: &Diagnostic) -> String {
    let mut text = format!(
        "{}{}",
        position_prefix(diagnostic),
        msg!(
            keys::DIAG_COMPILE_ERROR,
            code = diagnostic.code.as_deref().unwrap_or("?"),
            message = diagnostic.message,
        )
    );
    text.push_str(&format_notes(diagnostic));
    text
}

/// Заметки диагностики, готовые к печати: по строке на заметку.
///
/// ⚠️ **Носитель один на всех потребителей** (фича 0279). Прежде цикл по
/// заметкам жил только здесь, а `takt-sim` строил текст диагностики своей
/// функцией — и заметки у него **терялись**: на одном входе `taktc` печатал
/// сноску `SE-106` про вложенную модель, а эталон молчал. Это тот самый класс,
/// о котором предупреждает док-строка выше («копия формата в `taktc` уже
/// расходилась однажды»), — вторая копия жила в другом крейте.
///
/// Фича 0243: заметка печатается **со своей** позицией; у заметки, указывающей
/// в другой файл, координата опускается (чужую строку под своим путём
/// показывать нельзя), и строка остаётся прежней.
pub fn format_notes(diagnostic: &Diagnostic) -> String {
    let mut text = String::new();
    for note in &diagnostic.notes {
        text.push_str(&format!(
            "\n  {}: {}{}",
            msg!(keys::DIAG_NOTE_LABEL),
            super::note_position_prefix(diagnostic, note),
            note.message
        ));
    }
    text
}

/// Готовая к печати строка **предупреждения**: позиция, код, текст.
///
/// Спутник [`format_compile_error`] и живёт по той же причине (ADR 0053): вид
/// диагностики — её собственное свойство, а не свойство бинарника. До фичи 0226
/// этот формат был написан прямо в `compile_cli` литералом, и второй потребитель
/// (`taktc fmt`) завёл бы копию — ровно так уже расходился формат ошибки
/// (задача 0028-01).
pub fn format_warning(diagnostic: &Diagnostic) -> String {
    format!(
        "{}{}",
        position_prefix(diagnostic),
        msg!(
            keys::DIAG_WARNING,
            code = diagnostic.code.as_deref().unwrap_or("?"),
            message = diagnostic.message,
        )
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn diag(start: u32, code: &str, message: &str) -> Diagnostic {
        Diagnostic::error(Location::Source(0, start, start + 1), message.to_string())
            .with_code(code)
    }

    /// Порядок — по позиции в тексте, а не по порядку появления.
    #[test]
    fn sorted_by_position() {
        let got = normalize(vec![
            diag(50, "SE-003", "вторая"),
            diag(10, "SE-002", "первая"),
        ]);
        let messages: Vec<&str> = got.iter().map(|d| d.message.as_str()).collect();
        assert_eq!(messages, ["первая", "вторая"]);
    }

    /// Точный повтор убирается, а одинаковый текст в разных местах — нет.
    #[test]
    fn exact_duplicates_are_removed_but_distinct_places_kept() {
        let got = normalize(vec![
            diag(10, "SE-003", "одна и та же"),
            diag(10, "SE-003", "одна и та же"),
            diag(20, "SE-003", "одна и та же"),
        ]);
        assert_eq!(
            got.len(),
            2,
            "две разные позиции — две диагностики: {got:?}"
        );
    }

    /// Диагностика без позиции уходит в конец, а не разрывает чтение.
    #[test]
    fn positionless_diagnostics_go_last() {
        let mut without = diag(0, "CC-001", "без позиции");
        without.loc = Location::Codegen;
        let got = normalize(vec![without, diag(10, "SE-002", "с позицией")]);
        assert_eq!(got[0].message, "с позицией");
        assert_eq!(got[1].message, "без позиции");
    }

    /// Порядок диагностик с одинаковой позицией сохраняется (устойчивость).
    #[test]
    fn equal_positions_keep_input_order() {
        let got = normalize(vec![
            diag(10, "SE-002", "раньше"),
            diag(10, "SE-003", "позже"),
        ]);
        let messages: Vec<&str> = got.iter().map(|d| d.message.as_str()).collect();
        assert_eq!(messages, ["раньше", "позже"]);
    }

    /// Формат ошибки содержит код и сообщение; заметки идут отдельными строками.
    #[test]
    fn format_includes_code_message_and_notes() {
        let mut d = diag(10, "SE-002", "ссылка не найдена");
        d.notes.push(super::super::Note {
            loc: Location::Source(0, 0, 1),
            message: "импортировано здесь".to_string(),
        });
        let text = format_compile_error(&d);
        assert!(text.contains("[SE-002]"), "{text}");
        assert!(text.contains("ссылка не найдена"), "{text}");
        assert!(text.contains("  примечание: импортировано здесь"), "{text}");
    }
}

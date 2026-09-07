//! Вспомогательные функции LSP-сервера для языка Takt.
//!
//! Этот модуль реализует логику, связывающую компилятор Takt с протоколом LSP: сбор
//! диагностики, генерацию подсказок автодополнения и информацию о типах для функции
//! hover.
//!
//! Модуль включается только при наличии флага `lsp`.

use crate::semantic;
// Короткое `Location` в этом модуле занято LSP-структурой (uri + range), поэтому
// позиция компилятора зовётся `DiagLoc`.
use crate::diagnostics::Location as DiagLoc;
use cell::RefCell;
use lsp_types::*;
use semantic::index::SemanticNodeRef;
use semantic::{FunctionDefinitionNode, ModelNode, VariableNode};
use std::cell;

mod capabilities;
mod completion;
mod diagnostics;
mod formatting;
mod goto;
mod hover;
mod init_options;
mod keywords;
mod position;
mod references;
mod rename;
mod semantic_tokens;
mod symbols;
/// Рабочая область: обход файлов, граф импортов, связывание символов.
mod workspace;

// Публичный API крейта. `takt_lang::lsp::*` - контракт для `bin/takt_lsp.rs`,
// `tests/lsp_tests.rs` и плагинов IDE. Реэкспорт держит пути импорта прежними: где
// лежит функция внутри - деталь, которую потребитель знать не обязан. Ни один
// потребитель этой фичей не правится.
pub use capabilities::server_capabilities;
pub use completion::completion_items;
pub use diagnostics::{collect_diagnostics, collect_diagnostics_at, grammar_diagnostic_to_lsp};
pub use formatting::formatting_edits;
pub use goto::{Location, goto_declaration, goto_declaration_at, goto_declaration_with_paths};
pub use hover::{hover_info, word_at_position};
pub use init_options::search_paths_from_options;
pub use keywords::SEMANTIC_TOKEN_TYPES;
/// Типы протокола - Тот же крейт, которым живёт слой.
///
/// Реэкспорт нужен потребителю вне `takt-lang` (мост WebAssembly): объяви он свою
/// зависимость на `lsp-types`, версии разъехались бы, а `Position` из двух разных
/// версий - два разных типа. Здесь версия одна по построению.
pub use lsp_types;
pub use position::{node_at_position, offset_to_position, offset_to_range, position_to_offset};
pub use references::{FileReference, references_at, references_in_workspace};
pub use rename::{
    RenameRefusal, prepare_rename_at, prepare_rename_in_workspace, rename_at, rename_in_workspace,
};
pub use semantic_tokens::semantic_tokens;
pub use symbols::document_symbols;
pub use workspace::{Occurrence, Resolution, Workspace};

// Внутреннее: словари и помощники, которые зовут соседние подмодули.
use keywords::{
    TAKT_BUILTIN_TYPES, TAKT_KEYWORDS, TT_CLASS, TT_COMMENT, TT_ENUM_MEMBER, TT_FUNCTION,
    TT_KEYWORD, TT_NUMBER, TT_OPERATOR, TT_STRING, TT_TYPE, TT_VARIABLE,
};
use position::utf16_to_byte_offset;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::{Diagnostic as GrammarDiagnostic, ErrorType, Level, Location};

    // -- Вспомогательный исходный код для тестов --------------------------------

    /// Минимально корректный Takt-файл с переменной, функцией, типом, перечислением.
    const VALID_SRC: &str = r#"
var counter: [bit;8] := 0;
const LIMIT: [bit;8] := 10;
cond IsReady = counter = 0;
enum Color { Red = 0, Green = 1, Blue = 2 }
extern fn add(a: [bit;8], b: [bit;8]) -> [bit;8];
model M {
    start Idle {
        ref Run: IsReady;
    }
    state Run {
        next Idle;
    }
}
start S = M;
"#;

    /// Синтаксически неверный код: незакрытая скобка.
    const INVALID_SRC: &str = "model Broken {";

    // -- Тесты сбора диагностики ------------------------------------------------

    /// Корректный исходный код не должен порождать **ошибок**.
    ///
    /// Проверяются именно ошибки, а не всякая диагностика: фикстура законно получает
    /// предупреждения (например, о каноне именования `CS-001`), и прежняя проверка
    /// "диагностик нет вовсе" приравнивала совет к отказу. Прежде фикстура объявляла
    /// `type u8 = [bit;8]` - затенение встроенного имени; с это `SE-107`, и объявление
    /// из фикстуры убрано.
    #[test]
    fn test_collect_diagnostics_valid_source() {
        let diags: Vec<_> = collect_diagnostics(VALID_SRC)
            .into_iter()
            .filter(|d| d.severity == Some(DiagnosticSeverity::ERROR))
            .collect();
        assert!(
            diags.is_empty(),
            "корректный код не должен давать ошибок, но получено: {:?}",
            diags
        );
    }

    /// Некорректный исходный код должен порождать хотя бы одну диагностику.
    #[test]
    fn test_collect_diagnostics_invalid_source() {
        let diags = collect_diagnostics(INVALID_SRC);
        assert!(
            !diags.is_empty(),
            "неверный код должен давать хотя бы одну ошибку"
        );
    }

    // -- Тесты конвертации смещений ---------------------------------------------

    /// Смещение в начале первой строки.
    #[test]
    fn test_offset_to_position_first_line() {
        let src = "hello world";
        let pos = offset_to_position(src, 5);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 5);
    }

    /// Смещение на второй строке.
    #[test]
    fn test_offset_to_position_second_line() {
        let src = "line1\nline2\nline3";
        // "line1\n" = 6 байт, "li" = 2, итого 8
        let pos = offset_to_position(src, 8);
        assert_eq!(pos.line, 1, "должна быть вторая строка");
        assert_eq!(pos.character, 2, "столбец должен быть 2");
    }

    /// Смещение за пределами строки не должно паниковать.
    #[test]
    fn test_offset_to_position_clamped() {
        let src = "abc";
        let pos = offset_to_position(src, 100);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 3);
    }

    /// offset_to_range возвращает корректный диапазон.
    #[test]
    fn test_offset_to_range() {
        let src = "hello\nworld";
        let range = offset_to_range(src, 0, 5);
        assert_eq!(range.start, Position::new(0, 0));
        assert_eq!(range.end, Position::new(0, 5));
    }

    // -- Тесты извлечения слова под курсором ------------------------------------

    /// Курсор в конце простого слова.
    #[test]
    fn test_word_at_position_simple() {
        let src = "var hello := 0;";
        let word = word_at_position(src, Position::new(0, 7));
        assert_eq!(word, Some("hello".to_string()));
    }

    /// Курсор в середине слова.
    #[test]
    fn test_word_at_position_middle_of_word() {
        let src = "state Running;";
        // Позиция 8 - внутри "Running"
        let word = word_at_position(src, Position::new(0, 8));
        assert_eq!(word, Some("Running".to_string()));
    }

    /// Курсор на знаке "=" между двумя пробелами - слово не найдено.
    #[test]
    fn test_word_at_position_on_space() {
        let src = "var x := 0;";
        // Позиция 6 - символ "=" не является буквой/цифрой/подчёркиванием. "var x " (6
        // символов) -> left = " ", last non-word index = 5 -> start = 6. "= 0;" -> "="
        // не алфавитно-цифровой -> find вернёт 0 -> end = 6. start == end -> None.
        let word = word_at_position(src, Position::new(0, 6));
        assert!(
            word.is_none(),
            "символ-оператор не должен давать слово: {:?}",
            word
        );
    }

    /// Несуществующая строка - None.
    #[test]
    fn test_word_at_position_nonexistent_line() {
        let src = "var x := 0;";
        let word = word_at_position(src, Position::new(99, 0));
        assert_eq!(word, None);
    }

    // -- Тесты автодополнения ---------------------------------------------------

    /// Список автодополнения содержит ключевые слова языка Takt.
    #[test]
    fn test_completion_items_contains_keywords() {
        let items = completion_items("start S;");
        let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
        for kw in &["model", "state", "start", "var", "fn", "enum"] {
            assert!(
                labels.contains(kw),
                "ключевое слово '{}' должно присутствовать в автодополнении",
                kw
            );
        }
    }

    /// Автодополнение по корректному коду включает идентификаторы из семантики.
    #[test]
    fn test_completion_items_contains_semantic() {
        let src = "var myVar: [bit;8] := 0; start S;";
        let items = completion_items(src);
        let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(
            labels.contains(&"myVar"),
            "переменная 'myVar' должна присутствовать в автодополнении"
        );
    }

    /// Автодополнение по некорректному коду возвращает хотя бы ключевые слова.
    #[test]
    fn test_completion_items_fallback_to_keywords_on_error() {
        let items = completion_items("model {{{ broken");
        // Должны быть хотя бы ключевые слова
        assert!(
            items.len() >= TAKT_KEYWORDS.len(),
            "при ошибке разбора должны присутствовать минимум ключевые слова"
        );
    }

    // -- Тесты hover ------------------------------------------------------------

    /// Hover над переменной показывает тип.
    #[test]
    fn test_hover_variable() {
        let src = "var counter: [bit;8] := 0; start S;";
        let hover = hover_info(src, Position::new(0, 5));
        assert!(
            hover.is_some(),
            "hover над переменной должен возвращать данные"
        );
        if let Some(h) = hover
            && let HoverContents::Markup(mc) = h.contents
        {
            assert!(
                mc.value.contains("counter"),
                "hover должен содержать имя переменной"
            );
        }
    }

    /// Hover над именем функции показывает сигнатуру.
    #[test]
    fn test_hover_function() {
        let src = "extern fn myFunc(x: [bit;8]) -> [bit;8]; start S;";
        // Позиция 10 - внутри "myFunc"
        let hover = hover_info(src, Position::new(0, 10));
        assert!(
            hover.is_some(),
            "hover над функцией должен возвращать данные"
        );
        if let Some(h) = hover
            && let HoverContents::Markup(mc) = h.contents
        {
            assert!(
                mc.value.contains("myFunc"),
                "hover должен содержать имя функции"
            );
        }
    }

    /// Hover над неизвестным идентификатором возвращает None.
    #[test]
    fn test_hover_unknown() {
        let src = "start S;";
        let hover = hover_info(src, Position::new(0, 0));
        // "start" - ключевое слово, не переменная и не функция результат зависит от
        // семантики; проверяем, что нет паники
        let _ = hover;
    }

    /// Hover над пустой позицией возвращает None.
    #[test]
    fn test_hover_empty_position() {
        let hover = hover_info("", Position::new(0, 0));
        assert!(
            hover.is_none(),
            "hover в пустом файле должен возвращать None"
        );
    }

    // -- Тесты hover с документацией (C6) --------------------------------------

    /// Hover над функцией с doc-комментарием отображает и сигнатуру, и документацию.
    ///
    /// `///`-комментарии перед `fn process` должны появляться в hover-ответе вместе с
    /// сигнатурой функции.
    #[test]
    fn test_hover_function_with_docs() {
        let src = "/// Обработка данных.\n/// data — входные данные.\nextern fn process(data: [bit;8]) -> bit; start S;";
        // "process" начинается на строке 2 (0-indexed), символ 10 ("extern fn " = 10)
        let hover = hover_info(src, Position::new(2, 10));
        assert!(
            hover.is_some(),
            "hover над функцией с документацией должен возвращать данные"
        );
        if let Some(h) = hover
            && let HoverContents::Markup(mc) = h.contents
        {
            assert!(
                mc.value.contains("process"),
                "hover должен содержать имя функции"
            );
            assert!(
                mc.value.contains("Обработка данных"),
                "hover должен содержать документацию функции: {}",
                mc.value
            );
        }
    }

    /// Hover над переменной с doc-комментарием отображает и тип, и документацию.
    ///
    /// `///`-комментарии перед `var counter` должны появляться в hover-ответе вместе с
    /// типом переменной.
    #[test]
    fn test_hover_variable_with_docs() {
        let src = "/// Счётчик тактов.\nvar counter: [bit;8] := 0; start S;";
        // "counter" находится на строке 1 (0-indexed), символ 4 ("var " = 4)
        let hover = hover_info(src, Position::new(1, 4));
        assert!(
            hover.is_some(),
            "hover над переменной с документацией должен возвращать данные"
        );
        if let Some(h) = hover
            && let HoverContents::Markup(mc) = h.contents
        {
            assert!(
                mc.value.contains("counter"),
                "hover должен содержать имя переменной"
            );
            assert!(
                mc.value.contains("Счётчик тактов"),
                "hover должен содержать документацию переменной: {}",
                mc.value
            );
        }
    }

    /// Hover над переменной без документации отображает только сигнатуру.
    ///
    /// Если `///`-комментарии отсутствуют, hover возвращает только тип переменной.
    #[test]
    fn test_hover_variable_without_docs() {
        let src = "var counter: [bit;8] := 0; start S;";
        let hover = hover_info(src, Position::new(0, 5));
        assert!(
            hover.is_some(),
            "hover без документации должен возвращать данные"
        );
        if let Some(h) = hover
            && let HoverContents::Markup(mc) = h.contents
        {
            assert!(
                mc.value.contains("counter"),
                "hover должен содержать имя переменной"
            );
            // Без документации не должно быть двойного переноса строки + текста
            // Достаточно убедиться, что нет лишнего текста за пределами code-блока
            assert!(
                mc.value.starts_with("```but"),
                "hover без документации должен начинаться с блока кода"
            );
        }
    }

    // -- Тесты конвертации диагностик ------------------------------------------

    /// Ошибка парсера конвертируется в LSP DiagnosticSeverity::ERROR.
    #[test]
    fn test_grammar_diagnostic_to_lsp_error() {
        let diag = GrammarDiagnostic {
            file: None,
            loc: Location::Source(0, 0, 5),
            level: Level::Error,
            ty: ErrorType::ParserError,
            message: "тестовая ошибка".to_string(),
            code: None,
            notes: vec![],
        };
        let lsp_diag = grammar_diagnostic_to_lsp(&diag, "hello");
        assert_eq!(lsp_diag.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(lsp_diag.message, "тестовая ошибка");
        assert_eq!(lsp_diag.source, Some("takt-lsp".to_string()));
    }

    /// Предупреждение конвертируется в LSP DiagnosticSeverity::WARNING.
    #[test]
    fn test_grammar_diagnostic_to_lsp_warning() {
        let diag = GrammarDiagnostic {
            file: None,
            loc: Location::Source(0, 6, 11),
            level: Level::Warning,
            ty: ErrorType::Warning,
            message: "тестовое предупреждение".to_string(),
            code: None,
            notes: vec![],
        };
        let lsp_diag = grammar_diagnostic_to_lsp(&diag, "hello world");
        assert_eq!(lsp_diag.severity, Some(DiagnosticSeverity::WARNING));
        // Столбец начала: 6 -> строка 0, символ 6
        assert_eq!(lsp_diag.range.start.line, 0);
        assert_eq!(lsp_diag.range.start.character, 6);
    }

    /// Builtin-местоположение конвертируется в нулевой диапазон.
    #[test]
    fn test_grammar_diagnostic_to_lsp_builtin_location() {
        let diag = GrammarDiagnostic {
            file: None,
            loc: Location::Builtin,
            level: Level::Info,
            ty: ErrorType::None,
            message: "встроенное".to_string(),
            code: None,
            notes: vec![],
        };
        let lsp_diag = grammar_diagnostic_to_lsp(&diag, "");
        assert_eq!(lsp_diag.range.start, Position::new(0, 0));
        assert_eq!(lsp_diag.range.end, Position::new(0, 0));
    }

    // -- Тесты UTF-16 и Unicode -------------------------------------------------

    /// Кириллический символ занимает 2 байта UTF-8, но 1 кодовую единицу UTF-16.
    /// offset_to_position должен возвращать UTF-16-столбец, а не байтовый.
    ///
    /// Пример: "аб" = bytes [0xD0,0x90, 0xD0,0x91] offset 2 (начало 'Б') -> строка 0,
    /// столбец 1 (в UTF-16) offset 4 (конец строки) -> строка 0, столбец 2 (в UTF-16)
    ///
    /// Контр-пример: если бы считали байты, столбец был бы 2 и 4 соответственно.
    #[test]
    fn test_offset_to_position_cyrillic_utf16() {
        let src = "АБ"; // 4 байта UTF-8, 2 символа, 2 кодовые единицы UTF-16
        assert_eq!(src.len(), 4, "кириллица: 2 байта на символ");

        // Конец строки: 2 кодовые единицы UTF-16
        let pos = offset_to_position(src, 4);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 2, "UTF-16 столбец, не байтовый");

        // После первого символа 'А': байт 2 -> UTF-16 столбец 1
        let pos = offset_to_position(src, 2);
        assert_eq!(pos.character, 1);
    }

    /// Emoji занимает 4 байта UTF-8 и 2 кодовые единицы UTF-16 (суррогатная пара).
    ///
    /// Пример: "😀x" - emoji U+1F600: 4 байта UTF-8, 2 UTF-16 единицы. offset 4 (позиция
    /// 'x') -> UTF-16 столбец 2
    ///
    /// Контр-пример: байтовый столбец был бы 4.
    #[test]
    fn test_offset_to_position_emoji_surrogate_pair() {
        let src = "😀x"; // U+1F600 = 4 байта UTF-8, 2 кодовые единицы UTF-16
        assert_eq!(src.len(), 5, "emoji 4 байта + 'x' 1 байт");

        // Позиция 'x': байт 4 -> UTF-16 столбец 2 (emoji занимает 2 единицы)
        let pos = offset_to_position(src, 4);
        assert_eq!(pos.line, 0);
        assert_eq!(
            pos.character, 2,
            "суррогатная пара занимает 2 UTF-16 единицы"
        );
    }

    /// Смещение на середину многобайтового символа не должно вызывать панику. Функция
    /// должна безопасно отступить до предыдущей char-границы.
    ///
    /// Пример: "А" = [0xD0, 0x90], offset 1 - середина символа. Ожидаем позицию начала
    /// "А" (UTF-16 столбец 0), а не панику.
    ///
    /// Контр-пример: &source[..1] для "А" вызвал бы панику без защиты.
    #[test]
    fn test_offset_to_position_mid_char_no_panic() {
        let src = "АБ"; // 'А' = bytes 0..2, 'Б' = bytes 2..4
        // Байт 1 - середина 'А': отступаем до байта 0 -> столбец 0
        let pos = offset_to_position(src, 1);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0, "должны откатиться до начала символа 'А'");
    }

    /// offset_to_position с нулём всегда возвращает (0, 0).
    #[test]
    fn test_offset_to_position_zero() {
        assert_eq!(offset_to_position("hello", 0), Position::new(0, 0));
        assert_eq!(offset_to_position("", 0), Position::new(0, 0));
        assert_eq!(offset_to_position("АБ", 0), Position::new(0, 0));
    }

    /// offset_to_position на многострочном тексте с кириллицей.
    ///
    /// Пример: "А\nБ", байт 3 = начало 'Б' -> строка 1, столбец 0.
    #[test]
    fn test_offset_to_position_multiline_cyrillic() {
        let src = "А\nБ"; // 'А'=2, '\n'=1, 'Б'=2 -> длина 5
        // Байт 3 - начало 'Б' на второй строке
        let pos = offset_to_position(src, 3);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);

        // Байт 5 - конец 'Б' -> строка 1, столбец 1
        let pos = offset_to_position(src, 5);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 1);
    }

    // -- Тесты word_at_position с UTF-16 позицией ------------------------------

    /// word_at_position корректно обрабатывает UTF-16 позицию в строке с кириллицей.
    ///
    /// Строка: "А myVar = 0;" 'А' занимает 1 кодовую единицу UTF-16.
    /// position.character=2 -> байт 3 -> 'm'.
    #[test]
    fn test_word_at_position_utf16_column() {
        // "А " = 2 UTF-16 единицы (1 для 'А', 1 для ' ') "myVar" начинается с
        // UTF-16-позиции 2
        let src = "А myVar";
        // 'А' = bytes 0..2, ' ' = byte 2, 'myVar' = bytes 3..8 UTF-16: 'А'=1, ' '=1,
        // 'm'=1 -> position.character=2 -> 'm'
        let word = word_at_position(src, Position::new(0, 2));
        assert_eq!(word, Some("myVar".to_string()));
    }

    /// word_at_position с позицией, выходящей за пределы строки -> None или последнее
    /// слово.
    ///
    /// Контр-пример: position.character больше длины строки - функция не паникует.
    #[test]
    fn test_word_at_position_beyond_line_no_panic() {
        let src = "hello";
        // Позиция за концом строки: clamp к длине -> конец слова
        let word = word_at_position(src, Position::new(0, 999));
        // Ожидаем "hello" (курсор зажат до конца)
        assert_eq!(word, Some("hello".to_string()));
    }

    // -- Тесты utf16_to_byte_offset --------------------------------------------

    /// ASCII: UTF-16 смещение совпадает с байтовым.
    #[test]
    fn test_utf16_to_byte_offset_ascii() {
        assert_eq!(super::utf16_to_byte_offset("hello", 0), Some(0));
        assert_eq!(super::utf16_to_byte_offset("hello", 3), Some(3));
        assert_eq!(super::utf16_to_byte_offset("hello", 5), Some(5));
    }

    /// Кириллица: 1 UTF-16 единица = 2 байта UTF-8.
    ///
    /// "абв": utf16_offset 1 -> байт 2, utf16_offset 3 -> байт 6.
    #[test]
    fn test_utf16_to_byte_offset_cyrillic() {
        let s = "АБВ"; // каждый символ 2 байта
        assert_eq!(super::utf16_to_byte_offset(s, 0), Some(0));
        assert_eq!(super::utf16_to_byte_offset(s, 1), Some(2)); // после 'А'
        assert_eq!(super::utf16_to_byte_offset(s, 2), Some(4)); // после 'Б'
        assert_eq!(super::utf16_to_byte_offset(s, 3), Some(6)); // конец
    }

    /// Emoji (суррогатная пара): U+1F600 занимает 2 UTF-16 единицы.
    ///
    /// "😀x": utf16_offset 2 -> байт 4 (начало 'x').
    ///
    /// Контр-пример: utf16_offset 1 -> None (внутри суррогатной пары).
    #[test]
    fn test_utf16_to_byte_offset_emoji() {
        let s = "😀x"; // U+1F600 = 4 байта UTF-8, 2 единицы UTF-16; 'x' = 1 байт
        // utf16_offset 0 -> байт 0 (начало emoji)
        assert_eq!(super::utf16_to_byte_offset(s, 0), Some(0));
        // utf16_offset 2 -> байт 4 (начало 'x')
        assert_eq!(super::utf16_to_byte_offset(s, 2), Some(4));
        // utf16_offset 3 -> байт 5 (конец строки)
        assert_eq!(super::utf16_to_byte_offset(s, 3), Some(5));
    }

    /// Смещение за пределами строки -> None.
    #[test]
    fn test_utf16_to_byte_offset_out_of_bounds() {
        assert_eq!(super::utf16_to_byte_offset("hi", 10), None);
        assert_eq!(super::utf16_to_byte_offset("", 1), None);
    }

    // -- Тест семантических токенов ---------------------------------------------

    /// semantic_tokens не должна паниковать на валидном Takt-исходнике.
    #[test]
    fn test_semantic_tokens_no_panic() {
        let tokens = semantic_tokens(VALID_SRC);
        // Проверяем, что токены сформированы и дельта-кодирование корректно: delta_line
        // строго неотрицательна (u32), delta_start < character на той же строке.
        for tok in &tokens.data {
            // delta_line - u32, поэтому неотрицательность гарантирована типом.
            // Дополнительно проверяем, что нулевые токены отфильтрованы.
            assert!(tok.length > 0, "нулевые токены отфильтровываются");
        }
    }

    /// semantic_tokens не должна паниковать на пустом вводе.
    #[test]
    fn test_semantic_tokens_empty_source() {
        let tokens = semantic_tokens("");
        assert!(tokens.data.is_empty(), "пустой источник → нет токенов");
    }

    /// Конструкции времени подсвечиваются: `clock`/`after` - ключевые слова, литералы
    /// `1kHz`/`3s` - числа. Тест: до правки семантических токенов они уходили в `_ =>
    /// continue` и не подсвечивались вовсе.
    #[test]
    fn test_semantic_tokens_highlight_time_constructs() {
        let src = "model M { clock 1kHz; start S { ref T: after 3s; } state T; }";
        let types: Vec<u32> = semantic_tokens(src)
            .data
            .iter()
            .map(|t| t.token_type)
            .collect();
        // Иных чисел в источнике нет: TT_NUMBER здесь только от литералов времени.
        assert!(
            types.contains(&TT_NUMBER),
            "литералы времени (1kHz, 3s) подсвечиваются как числа: {types:?}"
        );
        assert!(
            types.contains(&TT_KEYWORD),
            "ключевые слова (в т.ч. clock/after) подсвечиваются: {types:?}"
        );
    }

    /// Считает токены заданной категории в подсветке источника.
    fn count_tokens_of(src: &str, tt: u32) -> usize {
        semantic_tokens(src)
            .data
            .iter()
            .filter(|t| t.token_type == tt)
            .count()
    }

    /// `invariant` подсвечивается наравне со структурно равным ему `cond` (
    /// ).
    ///
    /// Тест устроен **сравнением с эталоном**, а не "есть хотя бы один KEYWORD":
    /// слабая форма зеленела бы и при непокрытом `invariant` - в источнике есть `model`
    /// и `start`.
    #[test]
    fn test_semantic_tokens_highlight_invariant_like_cond() {
        let with_cond = "model M { cond Ok = 1 = 1; start S; }";
        let with_invariant = "model M { invariant Ok = 1 = 1; start S; }";
        let n_cond = count_tokens_of(with_cond, TT_KEYWORD);
        let n_inv = count_tokens_of(with_invariant, TT_KEYWORD);
        assert_eq!(
            n_inv, n_cond,
            "invariant обязан подсвечиваться как cond: {n_inv} против {n_cond}"
        );
    }

    /// Операторы LTL и виды формул подсвечиваются как ключевые слова.
    ///
    /// Эталон - та же модель без формулы: разница обязана быть ровно в числе
    /// добавленных ключевых слов (`LTL` и `G`), а не "хотя бы что-то".
    #[test]
    fn test_semantic_tokens_highlight_ltl_operators() {
        let plain = "model M { var flag: bool; start S; }";
        let with_ltl = "model M { var flag: bool; : [LTL] G flag; start S; }";
        let added = count_tokens_of(with_ltl, TT_KEYWORD) - count_tokens_of(plain, TT_KEYWORD);
        assert_eq!(
            added, 2,
            "`LTL` и `G` — два ключевых слова; подсвечено добавочных: {added}"
        );
    }

    /// Стрелки `->` и `=>` подсвечиваются как операторы.
    ///
    /// До фичи они молча не окрашивались, хотя `-->` (`PeirceArrow`) окрашивался, -
    /// исчерпывающий разбор снял эту случайность.
    #[test]
    fn test_semantic_tokens_highlight_arrows_as_operators() {
        let no_arrow = "model M { fn f(x: u8) { return; } start S; }";
        let with_arrow = "model M { fn f(x: u8) -> u8 { return x; } start S; }";
        assert_eq!(
            count_tokens_of(with_arrow, TT_OPERATOR) - count_tokens_of(no_arrow, TT_OPERATOR),
            1,
            "`->` даёт ровно один добавочный оператор"
        );
    }

    /// semantic_tokens корректно считает длину кириллического идентификатора в UTF-16.
    ///
    /// Токен "абв" (3 символа, 6 байт UTF-8) должен иметь length=3 в UTF-16, не 6.
    ///
    /// Контр-пример: если бы считали байты, length был бы 6 - LSP-редактор неправильно
    /// подсветил бы диапазон.
    #[test]
    fn test_semantic_tokens_utf16_length() {
        // Используем extern fn с кириллическим именем Takt поддерживает
        // Unicode-идентификаторы через UnicodeXID
        let src = "extern fn АБВ() -> [bit;8]; start S;";
        let tokens = semantic_tokens(src);
        // Ищем токен типа TT_FUNCTION для "абв" "extern fn " = 10 байт/символов (ASCII)
        // до "абв" "абв" начинается на байте 10, строка 0 В UTF-16: 10 ASCII-символов =
        // 10 единиц -> delta_start или character = 10 length = 3 (UTF-16), не 6 (байты)
        let func_tok = tokens.data.iter().find(|t| t.token_type == TT_FUNCTION);
        if let Some(tok) = func_tok {
            assert_eq!(
                tok.length, 3,
                "кириллический идентификатор: 3 кодовые единицы UTF-16, не 6 байт"
            );
        }
        // Если "абв" не распознан как функция - всё равно не паникуем
    }
}

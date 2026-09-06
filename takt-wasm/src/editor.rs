//! Операции редактора: те же функции, что у языкового сервера.

use serde::Serialize;
use takt_lang::lsp;
use takt_lang::lsp::lsp_types::{HoverContents, MarkedString, Position, Range};

use crate::reply;

/// Диагностика в форме редактора: диапазон уже посчитан слоем.
#[derive(Debug, Serialize)]
struct EditorDiagnosticJson {
    code: Option<String>,
    severity: &'static str,
    message: String,
    range: RangeJson,
}

/// Диагностики документа - то, что редактор подчёркивает.
///
/// Берутся у слоя LSP, а не у компилятора напрямую: слой добавляет к ошибкам
/// предупреждения и канон именования (`CS-001`), и именно этот набор видит автор в
/// редакторе. Взяв другой, браузер показывал бы не то, что показывает `takt-lsp`.
pub fn diagnostics(source: &str) -> String {
    #[derive(Serialize)]
    struct Reply {
        diagnostics: Vec<EditorDiagnosticJson>,
    }
    reply::ok(Reply {
        diagnostics: lsp::collect_diagnostics(source)
            .into_iter()
            .map(|d| EditorDiagnosticJson {
                code: d.code.map(|c| match c {
                    lsp::lsp_types::NumberOrString::String(s) => s,
                    lsp::lsp_types::NumberOrString::Number(n) => n.to_string(),
                }),
                severity: match d.severity {
                    Some(lsp::lsp_types::DiagnosticSeverity::ERROR) => "error",
                    Some(lsp::lsp_types::DiagnosticSeverity::WARNING) => "warning",
                    Some(lsp::lsp_types::DiagnosticSeverity::INFORMATION) => "information",
                    Some(lsp::lsp_types::DiagnosticSeverity::HINT) => "hint",
                    _ => "error",
                },
                message: d.message,
                range: RangeJson::of(d.range),
            })
            .collect(),
    })
}

/// Семантические токены - то, чем красится текст.
///
/// Отдаются в форме LSP (`data` пятёрками) вместе со списком типов: страница красит по
/// индексу в этом списке, а не по своему словарю.
pub fn tokens(source: &str) -> String {
    #[derive(Serialize)]
    struct Reply {
        token_types: Vec<String>,
        data: Vec<u32>,
    }
    let tokens = lsp::semantic_tokens(source);
    reply::ok(Reply {
        token_types: lsp::SEMANTIC_TOKEN_TYPES
            .iter()
            .map(|t| t.as_str().to_string())
            .collect(),
        data: tokens
            .data
            .iter()
            .flat_map(|t| {
                [
                    t.delta_line,
                    t.delta_start,
                    t.length,
                    t.token_type,
                    t.token_modifiers_bitset,
                ]
            })
            .collect(),
    })
}

/// Подсказка при наведении.
pub fn hover(source: &str, line: u32, character: u32) -> String {
    #[derive(Serialize)]
    struct Reply {
        contents: Option<String>,
    }
    let hover = lsp::hover_info(source, Position { line, character });
    reply::ok(Reply {
        contents: hover.map(|h| match h.contents {
            HoverContents::Markup(markup) => markup.value,
            HoverContents::Scalar(marked) => marked_text(&marked),
            HoverContents::Array(items) => items
                .iter()
                .map(marked_text)
                .collect::<Vec<_>>()
                .join("\n\n"),
        }),
    })
}

/// Текст элемента `MarkedString` - обе его формы.
fn marked_text(marked: &MarkedString) -> String {
    match marked {
        MarkedString::String(text) => text.clone(),
        MarkedString::LanguageString(ls) => ls.value.clone(),
    }
}

/// Переход к объявлению.
pub fn goto(source: &str, line: u32, character: u32) -> String {
    #[derive(Serialize)]
    struct Reply {
        range: Option<RangeJson>,
    }
    let range = lsp::goto_declaration(source, Position { line, character });
    reply::ok(Reply {
        range: range.map(RangeJson::of),
    })
}

/// Использования символа в документе.
pub fn references(source: &str, line: u32, character: u32) -> String {
    #[derive(Serialize)]
    struct Reply {
        ranges: Vec<RangeJson>,
    }
    // Объявление входит в список: редактор показывает его наравне с использованиями, и
    // "найти всё" без него было бы неполным.
    let ranges = lsp::references_at(source, Position { line, character }, true);
    reply::ok(Reply {
        ranges: ranges
            .unwrap_or_default()
            .into_iter()
            .map(RangeJson::of)
            .collect(),
    })
}

/// Переименование символа.
///
/// Отказ переименования - не ошибка модуля: слой отвечает "полнота или отказ", и
/// причину обязан увидеть автор, а не проглотить мост.
pub fn rename(source: &str, line: u32, character: u32, new_name: &str) -> String {
    #[derive(Serialize)]
    struct Reply {
        edits: Vec<EditJson>,
    }
    match lsp::rename_at(source, Position { line, character }, new_name) {
        Ok(edits) => reply::ok(Reply {
            edits: edits
                .into_iter()
                .map(|e| EditJson {
                    range: RangeJson::of(e.range),
                    new_text: e.new_text,
                })
                .collect(),
        }),
        // Текст отказа даёт сам слой: свой здесь разошёлся бы с тем, что показывает
        // редактор на машине.
        Err(refusal) => reply::refused(refusal.message()),
    }
}

/// Форматирование документа - канон `taktc fmt`.
pub fn format(source: &str) -> String {
    #[derive(Serialize)]
    struct Reply {
        text: Option<String>,
    }
    match lsp::formatting_edits(source) {
        // Правок нет - документ уже в каноне: страница не трогает буфер.
        Ok(None) => reply::ok(Reply { text: None }),
        Ok(Some(edits)) => reply::ok(Reply {
            // Форматтер отдаёт одну правку на весь документ; вторая форма (список
            // точечных правок) слоем не порождается, и собирать её здесь было бы
            // догадкой.
            text: edits.into_iter().next().map(|e| e.new_text),
        }),
        Err(error) => reply::refused(error.to_string()),
    }
}

/// Структура документа - то, что редактор показывает деревом.
pub fn symbols(source: &str) -> String {
    #[derive(Serialize)]
    struct SymbolJson {
        name: String,
        kind: u8,
        range: RangeJson,
        selection_range: RangeJson,
    }
    #[derive(Serialize)]
    struct Reply {
        symbols: Vec<SymbolJson>,
    }
    reply::ok(Reply {
        symbols: lsp::document_symbols(source)
            .into_iter()
            .map(|s| SymbolJson {
                name: s.name,
                // `SymbolKind` в LSP - число протокола; страница показывает значок по
                // нему, и своего перечисления у неё нет.
                kind: symbol_kind(s.kind),
                range: RangeJson::of(s.range),
                selection_range: RangeJson::of(s.selection_range),
            })
            .collect(),
    })
}

/// Автодополнение.
///
/// Позиция слою не нужна: список у Takt пока не зависит от места курсора
/// (`completion_items` её не принимает). Мост её не выдумывает - иначе браузер обещал
/// бы контекстное дополнение, которого нет.
pub fn completion(source: &str) -> String {
    #[derive(Serialize)]
    struct ItemJson {
        label: String,
        detail: Option<String>,
    }
    #[derive(Serialize)]
    struct Reply {
        items: Vec<ItemJson>,
    }
    reply::ok(Reply {
        items: lsp::completion_items(source)
            .into_iter()
            .map(|i| ItemJson {
                label: i.label,
                detail: i.detail,
            })
            .collect(),
    })
}

/// Числовое значение вида символа по протоколу LSP.
fn symbol_kind(kind: lsp::lsp_types::SymbolKind) -> u8 {
    // `SymbolKind` - типизированная обёртка над числом протокола; сериализуется она как
    // число, и обратный путь честнее взять у сериализации, чем перечислять двадцать
    // шесть видов руками.
    serde_json::to_value(kind)
        .ok()
        .and_then(|v| v.as_u64())
        .and_then(|n| u8::try_from(n).ok())
        .unwrap_or(0)
}

/// Диапазон в форме страницы: строки и колонки с нуля, как в LSP.
#[derive(Debug, Serialize)]
pub struct RangeJson {
    start_line: u32,
    start_character: u32,
    end_line: u32,
    end_character: u32,
}

impl RangeJson {
    fn of(range: Range) -> Self {
        Self {
            start_line: range.start.line,
            start_character: range.start.character,
            end_line: range.end.line,
            end_character: range.end.character,
        }
    }
}

/// Правка текста.
#[derive(Debug, Serialize)]
struct EditJson {
    range: RangeJson,
    new_text: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    fn json(text: &str) -> Value {
        serde_json::from_str(text).expect("ответ моста — JSON")
    }

    const MODEL: &str =
        "var level: u8 := 0;\n\nstart Run {\n    always {\n        level := level + 1;\n    }\n}\n";

    /// Диагностики приходят и на недописанном файле - редактор видит такой чаще всего.
    #[test]
    fn diagnostics_answer_on_incomplete_source() {
        for source in ["start S {", "var x: ", "model M {\n  start"] {
            let reply = json(&diagnostics(source));
            assert_eq!(reply["ok"], Value::Bool(true), "{source}: {reply}");
            assert!(reply["diagnostics"].is_array());
        }
    }

    /// Токены приходят со списком типов: страница не заводит своего словаря.
    #[test]
    fn tokens_carry_their_type_names() {
        let reply = json(&tokens(MODEL));
        let types = reply["token_types"].as_array().unwrap();
        assert!(!types.is_empty(), "список типов пуст: {reply}");
        assert_eq!(
            reply["data"].as_array().unwrap().len() % 5,
            0,
            "данные токенов идут пятёрками"
        );
    }

    /// Наведение на объявление даёт текст.
    #[test]
    fn hover_answers_on_declaration() {
        let reply = json(&hover(MODEL, 0, 5));
        assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
    }

    /// Переименование отдаёт правки, а отказ - назван.
    #[test]
    fn rename_answers_edits_or_named_refusal() {
        let reply = json(&rename(MODEL, 0, 5, "height"));
        if reply["ok"] == Value::Bool(true) {
            assert!(
                !reply["edits"].as_array().unwrap().is_empty(),
                "переименование без правок — не переименование: {reply}"
            );
        } else {
            assert!(
                !reply["error"]["message"].as_str().unwrap().is_empty(),
                "отказ обязан назвать причину"
            );
        }
    }

    /// Форматирование: канон уже канон - правок нет.
    #[test]
    fn formatting_of_canonical_source_has_no_edits() {
        let reply = json(&format(MODEL));
        assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
        assert!(reply["text"].is_null(), "канон не требует правки: {reply}");
    }

    /// Форматирование неканонического текста даёт текст целиком.
    #[test]
    fn formatting_returns_whole_document() {
        let reply = json(&format("var   x:u8:=0;\nstart S;\n"));
        assert_eq!(reply["ok"], Value::Bool(true), "{reply}");
        assert!(
            reply["text"].as_str().is_some_and(|t| t.contains("var x")),
            "ожидался канонический текст: {reply}"
        );
    }

    /// Структура документа непуста на модели с объявлениями.
    #[test]
    fn symbols_are_listed() {
        let reply = json(&symbols(MODEL));
        assert!(
            !reply["symbols"].as_array().unwrap().is_empty(),
            "у модели есть объявления: {reply}"
        );
    }
}

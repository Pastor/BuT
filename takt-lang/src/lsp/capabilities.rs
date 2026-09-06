//! Объявляемые возможности сервера (`ServerCapabilities`) -.
//!
//! Вынесено из `bin/takt_lsp.rs` не ради размера, а ради **проверяемости**: бинарник
//! юнит-тестами не покрыть, поэтому "какие возможности объявлены" проверялось бы
//! чтением исходника глазами. Здесь же список - обычное значение, и тест сравнивает его
//! с ожидаемым.
//!
//! Часть модуля `lsp`.

use super::*;

/// Возможности, которые сервер объявляет клиенту при инициализации.
///
/// **`definition` и `declaration` объявляются вместе**. В Takt объявление и определение -
/// одно и то же (`var x := 0;` - и объявление, и определение), поэтому разделять их
/// нечего; а редакторы расходятся в том, какой метод шлёт F12: VS Code - `definition`,
/// Zed - `declaration`. Объявив только второе, сервер оставлял "Go to Definition"
/// **нерабочим** у первого.
pub fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                ..Default::default()
            },
        )),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![" ".to_string(), ".".to_string(), ":".to_string()]),
            resolve_provider: Some(false),
            ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        declaration_provider: Some(DeclarationCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        // поиск использований идёт по слою `semantic::usages` - индекс тел блоков и
        // функций не видит.
        references_provider: Some(OneOf::Left(true)),
        // переименование с `prepareRename` - редактор обязан узнать об отказе до ввода
        // нового имени, иначе пользователь введёт его зря.
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: Default::default(),
        })),
        document_symbol_provider: Some(OneOf::Left(true)),
        // канонический форматтер. То же ядро, что у `taktc fmt`, - расхождение стилей
        // между CLI и редактором невозможно по построению.
        document_formatting_provider: Some(OneOf::Left(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: SEMANTIC_TOKEN_TYPES.to_vec(),
                    token_modifiers: vec![],
                },
                full: Some(SemanticTokensFullOptions::Bool(true)),
                range: None,
                work_done_progress_options: Default::default(),
            },
        )),
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Обе возможности перехода объявлены - иначе F12 не работает в половине
    /// редакторов.
    #[test]
    fn declaration_and_definition_are_both_advertised() {
        let caps = server_capabilities();
        assert_eq!(
            caps.declaration_provider,
            Some(DeclarationCapability::Simple(true)),
            "declaration_provider обязан быть объявлен"
        );
        assert_eq!(
            caps.definition_provider,
            Some(OneOf::Left(true)),
            "definition_provider обязан быть объявлен (F12 в VS Code)"
        );
    }

    /// Прежние возможности не потеряны при выносе списка из бинарника.
    #[test]
    fn previously_advertised_capabilities_are_kept() {
        let caps = server_capabilities();
        assert!(caps.hover_provider.is_some(), "hover");
        assert!(caps.completion_provider.is_some(), "completion");
        assert!(caps.document_symbol_provider.is_some(), "documentSymbol");
        assert!(caps.document_formatting_provider.is_some(), "formatting");
        assert!(caps.semantic_tokens_provider.is_some(), "semanticTokens");
        assert!(
            caps.text_document_sync.is_some(),
            "синхронизация документов"
        );
    }
}

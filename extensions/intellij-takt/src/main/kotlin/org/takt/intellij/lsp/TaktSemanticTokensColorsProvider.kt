package org.takt.intellij.lsp

import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.psi.PsiFile
import com.redhat.devtools.lsp4ij.features.semanticTokens.SemanticTokensColorsProvider
import org.takt.intellij.highlight.TaktHighlighterColors

/**
 * Отображение типов семантических токенов `takt-lsp` в цвета редактора.
 *
 *
 * Сервер отдаёт токены с типами из легенды `takt_lang::lsp::SEMANTIC_TOKEN_TYPES`
 * (10 типов). Провайдер сопоставляет **каждый** тип ключу [TaktHighlighterColors],
 * чтобы семантический слой уважал палитру и настройки цветов пользователя и
 * не спорил с лексическим по цвету. Наслаивается **поверх** лексики: цвет
 * меняется только у идентификаторов, которые лексер красит одинаково.
 *
 * Набор ключей обязан покрывать легенду **целиком**: тип без маппинга молча
 * потеряет цвет. Контроль - `TaktSemanticTokensColorsProviderTest`, читающий
 * `SEMANTIC_TOKEN_TYPES` из Rust-исходника (приём `TaktKeywordSyncTest`).
 */
class TaktSemanticTokensColorsProvider : SemanticTokensColorsProvider {

    override fun getTextAttributesKey(
        tokenType: String,
        tokenModifiers: List<String>,
        file: PsiFile,
    ): TextAttributesKey? = keyFor(tokenType)

    companion object {
        /**
         * Ключ цвета для типа токена легенды LSP (имена - как в
         * `lsp_types::SemanticTokenType`). `null` - тип вне легенды (цвет не
         * навязывается).
         */
        fun keyFor(tokenType: String): TextAttributesKey? = when (tokenType) {
            "keyword" -> TaktHighlighterColors.KEYWORD
            "variable" -> TaktHighlighterColors.IDENTIFIER
            "function" -> TaktHighlighterColors.FUNCTION
            "type" -> TaktHighlighterColors.TYPE
            "enumMember" -> TaktHighlighterColors.ENUM_MEMBER
            "string" -> TaktHighlighterColors.STRING
            "number" -> TaktHighlighterColors.NUMBER
            "comment" -> TaktHighlighterColors.LINE_COMMENT
            "operator" -> TaktHighlighterColors.OPERATOR
            "class" -> TaktHighlighterColors.CLASS
            else -> null
        }
    }
}

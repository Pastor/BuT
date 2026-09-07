package org.takt.intellij.highlight

import com.intellij.lexer.Lexer
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType
import org.takt.intellij.lexer.TaktLexer
import org.takt.intellij.psi.TaktTokenTypes

/**
 * Подсветка синтаксиса Takt: сопоставляет токены [TaktLexer] цветовым ключам
 * [TaktHighlighterColors].
 */
class TaktSyntaxHighlighter : SyntaxHighlighterBase() {

    override fun getHighlightingLexer(): Lexer = TaktLexer()

    override fun getTokenHighlights(tokenType: IElementType): Array<TextAttributesKey> {
        val key = when (tokenType) {
            TaktTokenTypes.KEYWORD -> TaktHighlighterColors.KEYWORD
            TaktTokenTypes.IDENTIFIER -> TaktHighlighterColors.IDENTIFIER
            TaktTokenTypes.NUMBER -> TaktHighlighterColors.NUMBER
            TaktTokenTypes.STRING -> TaktHighlighterColors.STRING
            TaktTokenTypes.LINE_COMMENT -> TaktHighlighterColors.LINE_COMMENT
            TaktTokenTypes.DOC_COMMENT -> TaktHighlighterColors.DOC_COMMENT
            TaktTokenTypes.BLOCK_COMMENT -> TaktHighlighterColors.BLOCK_COMMENT

            TaktTokenTypes.OP_ASSIGN,
            TaktTokenTypes.OP_EQ,
            TaktTokenTypes.OP_LE,
            TaktTokenTypes.OP_GE,
            TaktTokenTypes.OP_LT,
            TaktTokenTypes.OP_GT,
            TaktTokenTypes.OPERATOR,
            TaktTokenTypes.COLON -> TaktHighlighterColors.OPERATOR

            TaktTokenTypes.SEMICOLON -> TaktHighlighterColors.SEMICOLON
            TaktTokenTypes.COMMA -> TaktHighlighterColors.COMMA
            TaktTokenTypes.DOT -> TaktHighlighterColors.DOT
            TaktTokenTypes.LPAREN, TaktTokenTypes.RPAREN -> TaktHighlighterColors.PARENTHESES
            TaktTokenTypes.LBRACE, TaktTokenTypes.RBRACE -> TaktHighlighterColors.BRACES
            TaktTokenTypes.LBRACKET, TaktTokenTypes.RBRACKET -> TaktHighlighterColors.BRACKETS

            TokenType.BAD_CHARACTER -> TaktHighlighterColors.BAD_CHARACTER
            else -> null
        }
        return if (key == null) TextAttributesKey.EMPTY_ARRAY else pack(key)
    }
}

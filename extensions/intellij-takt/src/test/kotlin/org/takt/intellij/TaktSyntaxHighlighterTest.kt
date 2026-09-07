package org.takt.intellij

import com.intellij.psi.TokenType
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.takt.intellij.highlight.TaktHighlighterColors
import org.takt.intellij.highlight.TaktSyntaxHighlighter
import org.takt.intellij.psi.TaktTokenTypes

/**
 * Проверки маппинга токен -> цветовой ключ (задача 0022-02; критерий A2).
 */
class TaktSyntaxHighlighterTest : BasePlatformTestCase() {

    private val hl = TaktSyntaxHighlighter()

    private fun keyOf(type: com.intellij.psi.tree.IElementType) =
        hl.getTokenHighlights(type).single()

    fun testCategoriesMapToExpectedColors() {
        assertEquals(TaktHighlighterColors.KEYWORD, keyOf(TaktTokenTypes.KEYWORD))
        assertEquals(TaktHighlighterColors.NUMBER, keyOf(TaktTokenTypes.NUMBER))
        assertEquals(TaktHighlighterColors.STRING, keyOf(TaktTokenTypes.STRING))
        assertEquals(TaktHighlighterColors.LINE_COMMENT, keyOf(TaktTokenTypes.LINE_COMMENT))
        assertEquals(TaktHighlighterColors.DOC_COMMENT, keyOf(TaktTokenTypes.DOC_COMMENT))
        assertEquals(TaktHighlighterColors.BLOCK_COMMENT, keyOf(TaktTokenTypes.BLOCK_COMMENT))
        assertEquals(TaktHighlighterColors.BRACES, keyOf(TaktTokenTypes.LBRACE))
        assertEquals(TaktHighlighterColors.BRACES, keyOf(TaktTokenTypes.RBRACE))
        assertEquals(TaktHighlighterColors.PARENTHESES, keyOf(TaktTokenTypes.LPAREN))
        assertEquals(TaktHighlighterColors.BRACKETS, keyOf(TaktTokenTypes.LBRACKET))
    }

    fun testAllOperatorsShareOperatorColor() {
        for (op in listOf(
            TaktTokenTypes.OP_ASSIGN, TaktTokenTypes.OP_EQ, TaktTokenTypes.OP_LE,
            TaktTokenTypes.OP_GE, TaktTokenTypes.OP_LT, TaktTokenTypes.OP_GT,
            TaktTokenTypes.OPERATOR, TaktTokenTypes.COLON,
        )) {
            assertEquals(TaktHighlighterColors.OPERATOR, keyOf(op))
        }
    }

    fun testBadCharacterHighlighted() {
        assertEquals(TaktHighlighterColors.BAD_CHARACTER, keyOf(TokenType.BAD_CHARACTER))
    }
}

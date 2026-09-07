package org.takt.intellij

import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.takt.intellij.editor.TaktBraceMatcher
import org.takt.intellij.editor.TaktCommenter
import org.takt.intellij.psi.TaktTokenTypes

/**
 * Проверки эргономики редактора:
 * комментирование и парные скобки.
 */
class TaktEditorSupportTest : BasePlatformTestCase() {

    fun testCommenterPrefixes() {
        val c = TaktCommenter()
        assertEquals("//", c.lineCommentPrefix)
        assertEquals("/*", c.blockCommentPrefix)
        assertEquals("*/", c.blockCommentSuffix)
    }

    fun testBracePairs() {
        val pairs = TaktBraceMatcher().pairs
        assertEquals(3, pairs.size)

        val braces = pairs.single { it.leftBraceType == TaktTokenTypes.LBRACE }
        assertEquals(TaktTokenTypes.RBRACE, braces.rightBraceType)
        assertTrue("Фигурные скобки должны быть структурными", braces.isStructural)

        val parens = pairs.single { it.leftBraceType == TaktTokenTypes.LPAREN }
        assertEquals(TaktTokenTypes.RPAREN, parens.rightBraceType)

        val brackets = pairs.single { it.leftBraceType == TaktTokenTypes.LBRACKET }
        assertEquals(TaktTokenTypes.RBRACKET, brackets.rightBraceType)
    }
}

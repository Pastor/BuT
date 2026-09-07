package org.takt.intellij

import com.intellij.psi.TokenType
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.takt.intellij.highlight.TaktColorSettingsPage
import org.takt.intellij.highlight.TaktSyntaxHighlighter
import org.takt.intellij.lexer.TaktLexer

/**
 * Проверки страницы настройки цветов.
 */
class TaktColorSettingsPageTest : BasePlatformTestCase() {

    private val page = TaktColorSettingsPage()

    fun testMetadata() {
        assertEquals("Takt", page.displayName)
        assertNotNull(page.icon)
        assertTrue(page.highlighter is TaktSyntaxHighlighter)
        assertTrue("Должны быть дескрипторы атрибутов", page.attributeDescriptors.isNotEmpty())
        assertTrue(page.demoText.isNotBlank())
    }

    fun testDemoTextHasNoBadCharacters() {
        // Демонстрационный фрагмент обязан быть валидным Takt: без BAD_CHARACTER
        // (в частности, без выведенного `==`).
        val lexer = TaktLexer()
        lexer.start(page.demoText)
        while (lexer.tokenType != null) {
            assertNotSame(
                "Демо-текст страницы цветов содержит некорректный токен: " +
                    "'${page.demoText.substring(lexer.tokenStart, lexer.tokenEnd)}'",
                TokenType.BAD_CHARACTER,
                lexer.tokenType,
            )
            lexer.advance()
        }
    }

    fun testEveryDescriptorKeyIsUnique() {
        val keys = page.attributeDescriptors.map { it.key }
        assertEquals(keys.size, keys.toSet().size)
    }
}

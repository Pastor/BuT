package org.takt.intellij

import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.takt.intellij.lexer.TaktLexer
import org.takt.intellij.psi.TaktTokenTypes

/**
 * Проверки лексера Takt.
 * Ключевой контрпример - `==` подсвечивается как BAD_CHARACTER.
 */
class TaktLexerTest : BasePlatformTestCase() {

    /** Токены без пробелов: (тип, текст). */
    private fun lex(text: String): List<Pair<IElementType, String>> {
        val lexer = TaktLexer()
        lexer.start(text)
        val result = ArrayList<Pair<IElementType, String>>()
        while (lexer.tokenType != null) {
            val type = lexer.tokenType!!
            if (type != TokenType.WHITE_SPACE) {
                result.add(type to text.substring(lexer.tokenStart, lexer.tokenEnd))
            }
            lexer.advance()
        }
        return result
    }

    private fun types(text: String) = lex(text).map { it.first }

    fun testAssignOperator() {
        assertEquals(
            listOf(TaktTokenTypes.IDENTIFIER, TaktTokenTypes.OP_ASSIGN, TaktTokenTypes.NUMBER, TaktTokenTypes.SEMICOLON),
            types("x := 1;"),
        )
    }

    fun testEqualityOperator() {
        // `cond C = x = y;` - оба `=` это сравнение (OP_EQ), а не присваивание.
        val eq = lex("a = b").filter { it.first == TaktTokenTypes.OP_EQ }
        assertEquals(1, eq.size)
        assertEquals("=", eq[0].second)
    }

    fun testRelationalLessEqual() {
        val le = lex("x <= 3").filter { it.first == TaktTokenTypes.OP_LE }
        assertEquals(1, le.size)
        assertEquals("<=", le[0].second)
    }

    fun testDoubleEqualsIsBadCharacter() {
        // Контрпример: `==` выведено из языка - это не валидный оператор.
        val bad = lex("x == y").filter { it.first == TokenType.BAD_CHARACTER }
        assertEquals(1, bad.size)
        assertEquals("==", bad[0].second)
    }

    fun testArbitraryNonAlphaIsBadCharacter() {
        // Произвольный неалфавитный символ
        // вне операторов/пунктуации языка - BAD_CHARACTER. Лексер покрывает весь
        // ввод, не "проглатывая" чужой символ (инвариант подсветки).
        for (ch in listOf("@", "$", "`", "\\", "№")) {
            val bad = lex(ch).filter { it.first == TokenType.BAD_CHARACTER }
            assertEquals("символ '$ch' должен быть BAD_CHARACTER", 1, bad.size)
            assertEquals(ch, bad[0].second)
        }
    }

    fun testKeywordsHighlighted() {
        val src = "model state start ref next cond var fn type enum struct import from as if else"
        assertTrue(types(src).all { it == TaktTokenTypes.KEYWORD })
    }

    fun testLtlKeywords() {
        // Односимвольные LTL-операторы X F G U R и LTL/Guard - ключевые слова.
        assertTrue(types("X F G U R LTL Guard").all { it == TaktTokenTypes.KEYWORD })
    }

    fun testIdentifierVsKeyword() {
        assertEquals(listOf(TaktTokenTypes.IDENTIFIER), types("myState"))
        assertEquals(listOf(TaktTokenTypes.KEYWORD), types("state"))
    }

    fun testNumbers() {
        assertEquals(TaktTokenTypes.NUMBER, types("42").single())
        assertEquals(TaktTokenTypes.NUMBER, types("0xFF").single())
        assertEquals(TaktTokenTypes.NUMBER, types("3.14").single())
        assertEquals(TaktTokenTypes.NUMBER, types("1e10").single())
        assertEquals(TaktTokenTypes.NUMBER, types("2.5E-3").single())
    }

    fun testStringLiteral() {
        val toks = lex("""import "util.takt";""")
        val str = toks.single { it.first == TaktTokenTypes.STRING }
        assertEquals("\"util.takt\"", str.second)
    }

    fun testComments() {
        assertEquals(TaktTokenTypes.LINE_COMMENT, types("// комментарий").single())
        assertEquals(TaktTokenTypes.DOC_COMMENT, types("/// doc").single())
        assertEquals(TaktTokenTypes.BLOCK_COMMENT, types("/* блок */").single())
    }

    fun testBlockCommentMultiline() {
        assertEquals(TaktTokenTypes.BLOCK_COMMENT, types("/* строка1\nстрока2 */").single())
    }

    fun testBracesParensBrackets() {
        // start Start = A + B + (C | D) + E { next Next; }
        val src = "A + (C | D) { [x] };"
        val types = types(src)
        assertTrue(types.contains(TaktTokenTypes.LPAREN))
        assertTrue(types.contains(TaktTokenTypes.RPAREN))
        assertTrue(types.contains(TaktTokenTypes.LBRACE))
        assertTrue(types.contains(TaktTokenTypes.RBRACE))
        assertTrue(types.contains(TaktTokenTypes.LBRACKET))
        assertTrue(types.contains(TaktTokenTypes.RBRACKET))
        assertTrue(types.contains(TaktTokenTypes.SEMICOLON))
        assertTrue(types.contains(TaktTokenTypes.OPERATOR)) // + и |
    }

    fun testTokenOffsetsCoverInput() {
        // Лексер обязан покрыть весь ввод без разрывов (инвариант подсветки).
        val text = "var x := 0xFF; // c\nnext End;"
        val lexer = TaktLexer()
        lexer.start(text)
        var pos = 0
        while (lexer.tokenType != null) {
            assertEquals(pos, lexer.tokenStart)
            assertTrue(lexer.tokenEnd > lexer.tokenStart)
            pos = lexer.tokenEnd
            lexer.advance()
        }
        assertEquals(text.length, pos)
    }
}

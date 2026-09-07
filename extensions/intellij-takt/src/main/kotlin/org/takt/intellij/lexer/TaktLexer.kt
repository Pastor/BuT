package org.takt.intellij.lexer

import com.intellij.lexer.LexerBase
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType
import org.takt.intellij.psi.TaktTokenTypes

/**
 * Лексер языка Takt для подсветки синтаксиса (задача 0022-02).
 *
 * Рукописная реализация [LexerBase], точно зеркалящая правила Rust-лексера
 * `takt-lang/src/parser/lexer.rs` (операторы фичи 0021, числа, строки,
 * комментарии). JFlex/grammarkit-кодогенерация сознательно не используется -
 * чтобы сборка плагина была самодостаточной (без внешнего генератора лексеров),
 * см. карточку задачи 0022-02.
 *
 * Особенности пост-0021:
 * - `:=` -> [TaktTokenTypes.OP_ASSIGN] (присваивание);
 * - `=` -> [TaktTokenTypes.OP_EQ] (сравнение на равенство);
 * - `<=`/`>=` -> реляционные; `==` -> [TokenType.BAD_CHARACTER] (оператор выведен
 *   из языка - не легализуем визуально).
 *
 * Состояние не требуется: блочный комментарий выдаётся одним токеном от `/*` до
 * `*/`, поэтому перезапуск инкрементального разбора всегда попадает на границу
 * токена (getState тождественно 0).
 */
class TaktLexer : LexerBase() {
    private var buffer: CharSequence = ""
    private var bufferEnd: Int = 0
    private var tokenStart: Int = 0
    private var tokenEnd: Int = 0
    private var currentToken: IElementType? = null

    override fun start(buffer: CharSequence, startOffset: Int, endOffset: Int, initialState: Int) {
        this.buffer = buffer
        this.bufferEnd = endOffset
        this.tokenStart = startOffset
        this.tokenEnd = startOffset
        advance()
    }

    override fun getState(): Int = 0
    override fun getTokenType(): IElementType? = currentToken
    override fun getTokenStart(): Int = tokenStart
    override fun getTokenEnd(): Int = tokenEnd
    override fun getBufferSequence(): CharSequence = buffer
    override fun getBufferEnd(): Int = bufferEnd

    override fun advance() {
        tokenStart = tokenEnd
        if (tokenStart >= bufferEnd) {
            currentToken = null
            return
        }
        val c = buffer[tokenStart]
        when {
            c.isWhitespace() -> {
                currentToken = TokenType.WHITE_SPACE
                tokenEnd = scanWhile(tokenStart) { it.isWhitespace() }
            }
            c == '/' && la(tokenStart, 1) == '/' -> scanLineComment()
            c == '/' && la(tokenStart, 1) == '*' -> {
                currentToken = TaktTokenTypes.BLOCK_COMMENT
                tokenEnd = scanBlockComment(tokenStart)
            }
            c == '"' -> {
                currentToken = TaktTokenTypes.STRING
                tokenEnd = scanString(tokenStart)
            }
            c.isDigit() -> {
                currentToken = TaktTokenTypes.NUMBER
                tokenEnd = scanNumber(tokenStart)
            }
            c == '.' && la(tokenStart, 1).isDigit() -> {
                currentToken = TaktTokenTypes.NUMBER
                tokenEnd = scanNumber(tokenStart)
            }
            isIdentStart(c) -> scanIdentifier()
            else -> scanOperatorOrPunct(c)
        }
    }

    // --- сканеры ---------------------------------------------------------

    private fun scanLineComment() {
        // `///...` - документационный, `//...` - обычный строчный комментарий.
        val doc = la(tokenStart, 2) == '/'
        currentToken = if (doc) TaktTokenTypes.DOC_COMMENT else TaktTokenTypes.LINE_COMMENT
        var i = tokenStart + 2
        while (i < bufferEnd && buffer[i] != '\n') i++
        tokenEnd = i
    }

    private fun scanBlockComment(from: Int): Int {
        var i = from + 2
        while (i < bufferEnd) {
            if (buffer[i] == '*' && i + 1 < bufferEnd && buffer[i + 1] == '/') {
                return i + 2
            }
            i++
        }
        return bufferEnd // незакрытый комментарий — до конца буфера
    }

    private fun scanString(from: Int): Int {
        var i = from + 1
        while (i < bufferEnd) {
            val ch = buffer[i]
            when {
                ch == '\\' && i + 1 < bufferEnd -> i += 2 // экранированный символ
                ch == '"' -> return i + 1
                ch == '\n' -> return i // незакрытая строка не «съедает» остаток файла
                else -> i++
            }
        }
        return bufferEnd
    }

    private fun scanNumber(from: Int): Int {
        var i = from
        // Шестнадцатеричный литерал 0x... (в Rust-лексере - префикс строчного `x`).
        if (buffer[i] == '0' && la(i, 1) == 'x') {
            i += 2
            while (i < bufferEnd && (isHexDigit(buffer[i]) || buffer[i] == '_')) i++
            return i
        }
        if (buffer[i] == '.') {
            i++
            i = scanDigits(i)
        } else {
            i = scanDigits(i)
            // Дробная часть 3.14 (точка + цифра).
            if (i < bufferEnd && buffer[i] == '.' && la(i, 1).isDigit()) {
                i = scanDigits(i + 1)
            }
        }
        // Показатель степени 1e10, 2.5E-3.
        if (i < bufferEnd && (buffer[i] == 'e' || buffer[i] == 'E')) {
            var j = i + 1
            if (j < bufferEnd && (buffer[j] == '+' || buffer[j] == '-')) j++
            if (j < bufferEnd && buffer[j].isDigit()) {
                i = scanDigits(j)
            }
        }
        return i
    }

    private fun scanDigits(from: Int): Int {
        var i = from
        while (i < bufferEnd && (buffer[i].isDigit() || buffer[i] == '_')) i++
        return i
    }

    private fun scanIdentifier() {
        var i = tokenStart + 1
        while (i < bufferEnd && isIdentPart(buffer[i])) i++
        val text = buffer.subSequence(tokenStart, i).toString()
        currentToken = if (text in TaktTokenTypes.KEYWORDS) TaktTokenTypes.KEYWORD else TaktTokenTypes.IDENTIFIER
        tokenEnd = i
    }

    private fun scanOperatorOrPunct(c: Char) {
        val three = slice(tokenStart, 3)
        if (three == "-->") {
            emit(TaktTokenTypes.OPERATOR, 3)
            return
        }
        when (slice(tokenStart, 2)) {
            ":=" -> return emit(TaktTokenTypes.OP_ASSIGN, 2)
            "==" -> return emit(TokenType.BAD_CHARACTER, 2) // 0021: `==` выведен
            "<=" -> return emit(TaktTokenTypes.OP_LE, 2)
            ">=" -> return emit(TaktTokenTypes.OP_GE, 2)
            "=>", "<<", ">>", "->", "&&", "||", "!=", "**" -> return emit(TaktTokenTypes.OPERATOR, 2)
        }
        val type = when (c) {
            '=' -> TaktTokenTypes.OP_EQ
            '<' -> TaktTokenTypes.OP_LT
            '>' -> TaktTokenTypes.OP_GT
            '+', '-', '*', '/', '%', '!', '|', '&', '^', '~', '?', '#' -> TaktTokenTypes.OPERATOR
            '.' -> TaktTokenTypes.DOT
            ',' -> TaktTokenTypes.COMMA
            ';' -> TaktTokenTypes.SEMICOLON
            ':' -> TaktTokenTypes.COLON
            '(' -> TaktTokenTypes.LPAREN
            ')' -> TaktTokenTypes.RPAREN
            '{' -> TaktTokenTypes.LBRACE
            '}' -> TaktTokenTypes.RBRACE
            '[' -> TaktTokenTypes.LBRACKET
            ']' -> TaktTokenTypes.RBRACKET
            else -> TokenType.BAD_CHARACTER
        }
        emit(type, 1)
    }

    // --- вспомогательные -------------------------------------------------

    private fun emit(type: IElementType, len: Int) {
        currentToken = type
        tokenEnd = minOf(tokenStart + len, bufferEnd)
    }

    private inline fun scanWhile(from: Int, pred: (Char) -> Boolean): Int {
        var i = from
        while (i < bufferEnd && pred(buffer[i])) i++
        return i
    }

    /** Символ на позиции `pos + k` или ` `, если вне буфера. */
    private fun la(pos: Int, k: Int): Char {
        val idx = pos + k
        return if (idx < bufferEnd) buffer[idx] else ' '
    }

    /** Подстрока `[pos, pos+n)`, усечённая по границе буфера. */
    private fun slice(pos: Int, n: Int): String {
        val end = minOf(pos + n, bufferEnd)
        return if (end > pos) buffer.subSequence(pos, end).toString() else ""
    }

    private fun isIdentStart(c: Char): Boolean = c == '_' || c.isLetter()
    private fun isIdentPart(c: Char): Boolean = c == '_' || c.isLetterOrDigit()
    private fun isHexDigit(c: Char): Boolean =
        c.isDigit() || (c in 'a'..'f') || (c in 'A'..'F')
}

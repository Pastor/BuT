package org.takt.intellij.psi

/**
 * Категории лексических токенов Takt для подсветки.
 *
 * Источник истины по набору ключевых слов и операторов - Rust-лексер
 * `takt-lang/src/parser/lexer.rs` (таблица `KEYWORDS`) и операторы языка
 * (`:=` присваивание, `=` сравнение, `<=` реляционный; `==` выведен из языка -
 * подсвечивается как `BAD_CHARACTER`). Соответствие [KEYWORDS] эталону из
 * `parser/lexer.rs` проверяется регресс-тестом `TaktKeywordSyncTest`.
 */
object TaktTokenTypes {
    @JvmField val IDENTIFIER = TaktTokenType("IDENTIFIER")
    @JvmField val KEYWORD = TaktTokenType("KEYWORD")

    @JvmField val NUMBER = TaktTokenType("NUMBER")
    @JvmField val STRING = TaktTokenType("STRING")

    @JvmField val LINE_COMMENT = TaktTokenType("LINE_COMMENT")
    @JvmField val DOC_COMMENT = TaktTokenType("DOC_COMMENT")
    @JvmField val BLOCK_COMMENT = TaktTokenType("BLOCK_COMMENT")

    // Операторы языка - различимы для тестов; все раскрашиваются
    // как "знак операции".
    @JvmField val OP_ASSIGN = TaktTokenType("OP_ASSIGN") // :=
    @JvmField val OP_EQ = TaktTokenType("OP_EQ")         // =
    @JvmField val OP_LE = TaktTokenType("OP_LE")         // <=
    @JvmField val OP_GE = TaktTokenType("OP_GE")         // >=
    @JvmField val OP_LT = TaktTokenType("OP_LT")         // <
    @JvmField val OP_GT = TaktTokenType("OP_GT")         // >

    // Прочие операторы: + - * ** / % ! != && || | & ^ ~ -> --> => << >> ? #
    @JvmField val OPERATOR = TaktTokenType("OPERATOR")

    @JvmField val SEMICOLON = TaktTokenType("SEMICOLON")
    @JvmField val COMMA = TaktTokenType("COMMA")
    @JvmField val DOT = TaktTokenType("DOT")
    @JvmField val COLON = TaktTokenType("COLON")

    // Скобки - раздельные типы для открывающих/закрывающих, чтобы работал
    // подсветчик парных скобок (`TaktBraceMatcher`).
    @JvmField val LPAREN = TaktTokenType("LPAREN")     // (
    @JvmField val RPAREN = TaktTokenType("RPAREN")     // )
    @JvmField val LBRACE = TaktTokenType("LBRACE")     // {
    @JvmField val RBRACE = TaktTokenType("RBRACE")     // }
    @JvmField val LBRACKET = TaktTokenType("LBRACKET") // [
    @JvmField val RBRACKET = TaktTokenType("RBRACKET") // ]

    /**
     * Ключевые слова Takt - зеркало таблицы `KEYWORDS` из
     * `takt-lang/src/parser/lexer.rs` (правило: источник истины - лексер языка).
     * При добавлении/удалении ключевого слова в языке этот набор обязан
     * измениться синхронно (ловится `TaktKeywordSyncTest`).
     */
    @JvmField
    val KEYWORDS: Set<String> = setOf(
        "break", "const", "continue", "else", "false", "for", "fn", "if", "match",
        "_", "import", "loop", "while", "return", "true", "type", "as",
        "assembly", "formula", "in", "out", "inout", "address", "model", "state",
        "start", "ref", "cond", "var", "next", "extern", "enum",
        "struct", "from", "X", "F", "G", "U", "R", "LTL", "Guard",
        // `invariant` - сахар над `cond`+Guard; был пропущен в плагине.
        "invariant",
        // Ключевые слова времени: объявление частоты и выдержки.
        "clock", "after", "every",
        // Параметр модели - третья форма объявления наряду с
        // `var` и `const`.
        "parameter",
        // Размещение порта: `out led: bit at 0x40:2 := 0;`. Слово
        // необязательное - адрес может прийти оператором `address` или внешней
        // картой, - но ключевое: переменная с именем `at` незаконна.
        "at",
    )
}

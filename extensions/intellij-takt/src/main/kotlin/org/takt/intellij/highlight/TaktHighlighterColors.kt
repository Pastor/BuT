package org.takt.intellij.highlight

import com.intellij.openapi.editor.DefaultLanguageHighlighterColors
import com.intellij.openapi.editor.HighlighterColors
import com.intellij.openapi.editor.colors.TextAttributesKey

/**
 * Ключи атрибутов текста для подсветки Takt.
 *
 * Каждый ключ наследует цвет от стандартного [DefaultLanguageHighlighterColors],
 * поэтому подсветка работает в любой цветовой схеме "из коробки". Страница
 * настройки цветов (`TaktColorSettingsPage`) переиспользует эти
 * же ключи, давая пользователю переопределять цвета по группам.
 */
object TaktHighlighterColors {
    private fun key(name: String, fallback: TextAttributesKey): TextAttributesKey =
        TextAttributesKey.createTextAttributesKey(name, fallback)

    @JvmField val KEYWORD = key("TAKT_KEYWORD", DefaultLanguageHighlighterColors.KEYWORD)
    @JvmField val IDENTIFIER = key("TAKT_IDENTIFIER", DefaultLanguageHighlighterColors.IDENTIFIER)
    @JvmField val NUMBER = key("TAKT_NUMBER", DefaultLanguageHighlighterColors.NUMBER)
    @JvmField val STRING = key("TAKT_STRING", DefaultLanguageHighlighterColors.STRING)
    @JvmField val LINE_COMMENT = key("TAKT_LINE_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT)
    @JvmField val DOC_COMMENT = key("TAKT_DOC_COMMENT", DefaultLanguageHighlighterColors.DOC_COMMENT)
    @JvmField val BLOCK_COMMENT = key("TAKT_BLOCK_COMMENT", DefaultLanguageHighlighterColors.BLOCK_COMMENT)
    @JvmField val OPERATOR = key("TAKT_OPERATOR", DefaultLanguageHighlighterColors.OPERATION_SIGN)
    @JvmField val SEMICOLON = key("TAKT_SEMICOLON", DefaultLanguageHighlighterColors.SEMICOLON)
    @JvmField val COMMA = key("TAKT_COMMA", DefaultLanguageHighlighterColors.COMMA)
    @JvmField val DOT = key("TAKT_DOT", DefaultLanguageHighlighterColors.DOT)
    @JvmField val PARENTHESES = key("TAKT_PARENTHESES", DefaultLanguageHighlighterColors.PARENTHESES)
    @JvmField val BRACES = key("TAKT_BRACES", DefaultLanguageHighlighterColors.BRACES)
    @JvmField val BRACKETS = key("TAKT_BRACKETS", DefaultLanguageHighlighterColors.BRACKETS)
    @JvmField val BAD_CHARACTER = key("TAKT_BAD_CHARACTER", HighlighterColors.BAD_CHARACTER)

    // ── Семантические ключи: различают имена по смыслу, чего
    // лексический слой дать не может (лексер видит любое имя как IDENTIFIER).
    // Накладываются поверх лексики LSP4IJ-слоем (TaktSemanticTokensColorsProvider),
    // наследуют цвет от стандартных семантических категорий платформы.
    @JvmField val FUNCTION = key("TAKT_FUNCTION", DefaultLanguageHighlighterColors.FUNCTION_DECLARATION)
    @JvmField val TYPE = key("TAKT_TYPE", DefaultLanguageHighlighterColors.CLASS_REFERENCE)
    @JvmField val ENUM_MEMBER = key("TAKT_ENUM_MEMBER", DefaultLanguageHighlighterColors.STATIC_FIELD)
    @JvmField val CLASS = key("TAKT_CLASS", DefaultLanguageHighlighterColors.CLASS_NAME)
}

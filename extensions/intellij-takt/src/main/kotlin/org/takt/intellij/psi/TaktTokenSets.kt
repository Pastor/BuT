package org.takt.intellij.psi

import com.intellij.psi.TokenType
import com.intellij.psi.tree.TokenSet

/**
 * Наборы токенов для [org.takt.intellij.parser.TaktParserDefinition].
 *
 * Комментарии/строки/пробелы объявляются платформе, чтобы флаговый PSI-разбор
 * корректно относил их к тривиям и строковым литералам (важно для навигации и
 * пропуска тривий при поиске значимых токенов).
 */
object TaktTokenSets {
    val COMMENTS: TokenSet = TokenSet.create(
        TaktTokenTypes.LINE_COMMENT,
        TaktTokenTypes.DOC_COMMENT,
        TaktTokenTypes.BLOCK_COMMENT,
    )

    val STRINGS: TokenSet = TokenSet.create(TaktTokenTypes.STRING)

    val WHITESPACES: TokenSet = TokenSet.create(TokenType.WHITE_SPACE)
}

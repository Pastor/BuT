package org.takt.intellij.editor

import com.intellij.lang.BracePair
import com.intellij.lang.PairedBraceMatcher
import com.intellij.psi.PsiFile
import com.intellij.psi.tree.IElementType
import org.takt.intellij.psi.TaktTokenTypes

/**
 * Подсветка парных скобок Takt: `{}`, `()`, `[]` (задача 0022-03).
 * Фигурные скобки - структурные (влияют на навигацию по блокам).
 */
class TaktBraceMatcher : PairedBraceMatcher {
    override fun getPairs(): Array<BracePair> = PAIRS

    override fun isPairedBracesAllowedBeforeType(lbrace: IElementType, contextType: IElementType?): Boolean = true

    override fun getCodeConstructStart(file: PsiFile?, openingBraceOffset: Int): Int = openingBraceOffset

    private companion object {
        val PAIRS = arrayOf(
            BracePair(TaktTokenTypes.LBRACE, TaktTokenTypes.RBRACE, true),
            BracePair(TaktTokenTypes.LPAREN, TaktTokenTypes.RPAREN, false),
            BracePair(TaktTokenTypes.LBRACKET, TaktTokenTypes.RBRACKET, false),
        )
    }
}

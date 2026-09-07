package org.takt.intellij.psi

import com.intellij.psi.tree.IElementType
import org.takt.intellij.TaktLanguage
import org.jetbrains.annotations.NonNls

/**
 * Базовый тип лексического токена языка Takt.
 *
 * Конкретные токены (ключевые слова, операторы `:=`/`=`/`<=`, литералы,
 * комментарии) объявляются в задаче 0022-02 - источник истины по набору
 * см. `takt-lang/src/parser/lexer.rs` (таблица `KEYWORDS`) и фичу 0021.
 */
class TaktTokenType(@NonNls debugName: String) : IElementType(debugName, TaktLanguage) {
    override fun toString(): String = "TaktTokenType." + super.toString()
}

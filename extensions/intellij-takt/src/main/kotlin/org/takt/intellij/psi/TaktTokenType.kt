package org.takt.intellij.psi

import com.intellij.psi.tree.IElementType
import org.takt.intellij.TaktLanguage
import org.jetbrains.annotations.NonNls

/**
 * Базовый тип лексического токена языка Takt.
 *
 * Конкретные токены (ключевые слова, операторы `:=`/`=`/`<=`, литералы,
 * комментарии) объявляются здесь же - источник истины по набору
 * см. `takt-lang/src/parser/lexer.rs`, таблица `KEYWORDS`.
 */
class TaktTokenType(@NonNls debugName: String) : IElementType(debugName, TaktLanguage) {
    override fun toString(): String = "TaktTokenType." + super.toString()
}

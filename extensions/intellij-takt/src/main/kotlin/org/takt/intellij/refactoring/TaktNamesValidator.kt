package org.takt.intellij.refactoring

import com.intellij.lang.refactoring.NamesValidator
import com.intellij.openapi.project.Project
import org.takt.intellij.psi.TaktTokenTypes

/**
 * Валидатор имён для rename Takt.
 *
 * Отвергает ключевые слова Takt как имена (в т.ч. жёсткое `address`) - набор
 * берётся из [TaktTokenTypes.KEYWORDS] (контроль синхронизации с лексером языка -
 * `TaktKeywordSyncTest`). Идентификатор: буква/`_` в начале, далее буквы/цифры/`_`.
 */
class TaktNamesValidator : NamesValidator {
    override fun isKeyword(name: String, project: Project?): Boolean =
        name in TaktTokenTypes.KEYWORDS

    override fun isIdentifier(name: String, project: Project?): Boolean {
        if (name.isEmpty() || name in TaktTokenTypes.KEYWORDS) return false
        if (!(name[0].isLetter() || name[0] == '_')) return false
        return name.all { it.isLetterOrDigit() || it == '_' }
    }
}

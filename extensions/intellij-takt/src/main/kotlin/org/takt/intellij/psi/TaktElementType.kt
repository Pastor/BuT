package org.takt.intellij.psi

import com.intellij.psi.tree.IElementType
import org.takt.intellij.TaktLanguage
import org.jetbrains.annotations.NonNls

/**
 * Базовый тип узла PSI-дерева языка Takt.
 *
 * Каркас под будущее PSI (навигация/инспекции - отдельные фичи). Для
 * лексической подсветки PSI не требуется, но тип заведён заранее,
 * чтобы точки расширения были единообразны.
 */
class TaktElementType(@NonNls debugName: String) : IElementType(debugName, TaktLanguage)

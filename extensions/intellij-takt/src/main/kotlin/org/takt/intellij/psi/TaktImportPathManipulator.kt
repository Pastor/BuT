package org.takt.intellij.psi

import com.intellij.openapi.util.TextRange
import com.intellij.psi.AbstractElementManipulator
import com.intellij.psi.impl.source.tree.LeafElement

/**
 * Манипулятор текста узла [TaktImportPath].
 *
 * `FileReference.rename`/`bindToElement` меняет путь через
 * `ElementManipulators.getManipulator(element).handleContentChange(...)` - без
 * зарегистрированного манипулятора rename-on-move бросил бы "Cannot find
 * manipulator". Здесь дочерний листовой токен `STRING` заменяется на новый с
 * подставленным содержимым.
 */
class TaktImportPathManipulator : AbstractElementManipulator<TaktImportPath>() {
    override fun handleContentChange(
        element: TaktImportPath,
        range: TextRange,
        newContent: String,
    ): TaktImportPath {
        val oldText = element.text
        val newText = oldText.substring(0, range.startOffset) + newContent + oldText.substring(range.endOffset)
        // Замена текста существующего листа: replaceWithText сохраняет CharTable/
        // контекст дерева (создание нового LeafPsiElement валит ассерт "old
        // indentation must be defined").
        (element.node.firstChildNode as? LeafElement)?.replaceWithText(newText)
        return element
    }
}

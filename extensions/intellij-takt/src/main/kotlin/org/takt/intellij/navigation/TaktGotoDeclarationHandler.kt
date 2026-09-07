package org.takt.intellij.navigation

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiElement
import org.takt.intellij.psi.TaktFile
import org.takt.intellij.psi.TaktTokenTypes

/**
 * Переход к декларации имени Takt.
 *
 * По идентификатору под кареткой ищет одноимённое объявление
 * ([TaktSymbolScanner]) в том же файле и отдаёт платформе его листовой элемент
 * как цель `Go to Declaration` (`Ctrl/Cmd+B`, `Ctrl/Cmd+Click`). На самой
 * декларации и на не-идентификаторах молчит, не мешая штатному поведению.
 *
 * Дополнительно обрабатывает строки-пути директив `import` ([TaktImports]):
 * `Ctrl/Cmd+Click` по пути открывает соответствующий файл `.takt`.
 */
class TaktGotoDeclarationHandler : GotoDeclarationHandler {

    override fun getGotoDeclarationTargets(
        sourceElement: PsiElement?,
        offset: Int,
        editor: Editor?,
    ): Array<PsiElement>? {
        val element = sourceElement ?: return null

        // Путь import -> сам файл.
        if (element.node?.elementType == TaktTokenTypes.STRING) {
            val path = TaktImports.pathOf(element) ?: return null
            val target = TaktImports.resolve(element, path) ?: return null
            return arrayOf(target)
        }

        if (element.node?.elementType != TaktTokenTypes.IDENTIFIER) return null
        val file = element.containingFile as? TaktFile ?: return null

        val name = element.text
        val caretStart = element.textRange.startOffset
        val targets = TaktSymbolScanner.scan(file.text)
            .asSequence()
            .filter { it.name == name }
            // Исключаем декларацию, на которой уже стоит каретка (переход в себя не нужен).
            .filter { !it.range.contains(caretStart) }
            .mapNotNull { file.findElementAt(it.range.startOffset) }
            .toList()

        return if (targets.isEmpty()) null else targets.toTypedArray()
    }
}

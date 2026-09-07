package org.takt.intellij.psi

import com.intellij.extapi.psi.PsiFileBase
import com.intellij.openapi.fileTypes.FileType
import com.intellij.psi.FileViewProvider
import org.takt.intellij.TaktFileType
import org.takt.intellij.TaktLanguage

/**
 * PSI-файл языка Takt.
 *
 * Дерево - "плоское" (см. [org.takt.intellij.parser.TaktParserDefinition]): все
 * токены лексера становятся листьями под корнем. Полноценного синтаксического
 * дерева нет - его достаточно, чтобы у
 * элементов под кареткой были реальные `PsiElement` для навигации к декларации
 * и ссылок на файлы `import`.
 */
class TaktFile(viewProvider: FileViewProvider) : PsiFileBase(viewProvider, TaktLanguage) {
    override fun getFileType(): FileType = TaktFileType
    override fun toString(): String = "Takt File"
}

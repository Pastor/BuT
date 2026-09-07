package org.takt.intellij.psi

import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.lang.ASTNode
import com.intellij.psi.PsiFileSystemItem
import com.intellij.psi.PsiReference
import com.intellij.psi.impl.source.resolve.reference.impl.providers.FileReferenceSet

/**
 * Композитный PSI-узел строки-пути `import`.
 *
 * Несёт настоящую файловую ссылку [FileReferenceSet] прямо через
 * [getReferences] - на плоском листе это невозможно. Даёт
 * `Ctrl+Click` к файлу и **rename-on-move**: при переименовании/перемещении
 * целевого файла средствами IDEA путь в `.takt` обновляется (через
 * `TaktImportPathManipulator`, регистрируемый в `plugin.xml`).
 *
 * Резолв пути - штатным `FileReferenceSet` относительно каталога импортирующего
 * файла (паритет с ядром: импорт ищется рядом с импортирующим).
 */
class TaktImportPath(node: ASTNode) : ASTWrapperPsiElement(node) {

    /** Содержимое пути (между кавычками) или `null`, если строка пуста/без кавычки. */
    fun pathContent(): String? {
        val t = text
        if (t.length < 2 || t[0] != '"') return null
        val end = if (t.last() == '"') t.length - 1 else t.length
        if (end <= 1) return null
        return t.substring(1, end)
    }

    override fun getReferences(): Array<PsiReference> {
        val content = pathContent() ?: return PsiReference.EMPTY_ARRAY
        // startInElement = 1 - сразу за открывающей кавычкой.
        @Suppress("UNCHECKED_CAST")
        return TaktImportFileReferenceSet(content, this).allReferences as Array<PsiReference>
    }
}

/**
 * `FileReferenceSet`, резолвящий путь `import` от каталога **импортирующего**
 * файла, как в ядре, а не от корней контента по умолчанию.
 */
private class TaktImportFileReferenceSet(str: String, element: TaktImportPath) :
    FileReferenceSet(str, element, 1, null, true) {

    override fun computeDefaultContexts(): Collection<PsiFileSystemItem> {
        val dir = element.containingFile?.originalFile?.containingDirectory
        return if (dir != null) listOf(dir) else super.computeDefaultContexts()
    }
}

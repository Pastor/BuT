package org.takt.intellij.navigation

import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFileSystemItem
import com.intellij.psi.PsiManager
import org.takt.intellij.psi.TaktElementTypes
import org.takt.intellij.psi.TaktTokenTypes

/**
 * Разрешение путей в директивах `import`.
 *
 *
 * Строка-путь `import` оборачивается парсером в композит
 * [TaktElementTypes.IMPORT_PATH] - носитель настоящей `PsiReference`. Ctrl+Click
 * работает и через ссылку, и через `GotoDeclarationHandler`
 * ([TaktGotoDeclarationHandler]) - платформа дедуплицирует одинаковую цель-файл.
 * `TaktImports` остаётся источником резолва пути для обоих путей.
 */
object TaktImports {

    /**
     * Является ли элемент строкой-путём `import`. Признак структурный:
     * листовой токен `STRING`, чей родитель - [TaktElementTypes.IMPORT_PATH]
     * (парсер оборачивает только пути `import`/`from`, поэтому строка в `formula`
     * родителя-`IMPORT_PATH` не имеет).
     */
    fun isImportPathElement(element: PsiElement): Boolean {
        if (element.node?.elementType != TaktTokenTypes.STRING) return false
        return element.parent?.node?.elementType == TaktElementTypes.IMPORT_PATH
    }

    /** Содержимое строки-пути (между кавычками) или `null`, если это не путь/пусто. */
    fun pathOf(element: PsiElement): String? {
        if (!isImportPathElement(element)) return null
        val text = element.text
        if (text.length < 2 || text[0] != '"') return null
        val contentEnd = if (text.last() == '"') text.length - 1 else text.length
        if (contentEnd <= 1) return null
        return text.substring(1, contentEnd)
    }

    /**
     * Резолвит путь: сначала относительно каталога текущего файла, затем от
     * корней контента проекта. Директории игнорируются (нужен файл).
     */
    fun resolve(element: PsiElement, path: String): PsiFileSystemItem? {
        if (path.isBlank()) return null
        val target = resolveVirtualFile(element, path) ?: return null
        return PsiManager.getInstance(element.project).findFile(target)
    }

    private fun resolveVirtualFile(element: PsiElement, path: String): VirtualFile? {
        val currentFile = element.containingFile?.originalFile?.virtualFile
            ?: element.containingFile?.virtualFile
        currentFile?.parent?.findFileByRelativePath(path)?.let { if (!it.isDirectory) return it }

        for (root in ProjectRootManager.getInstance(element.project).contentRoots) {
            root.findFileByRelativePath(path)?.let { if (!it.isDirectory) return it }
        }
        return null
    }
}

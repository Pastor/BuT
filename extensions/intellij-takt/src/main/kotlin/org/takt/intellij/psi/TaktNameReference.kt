package org.takt.intellij.psi

import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiReferenceBase
import org.takt.intellij.navigation.TaktSymbolScanner

/**
 * Ссылка использования имени Takt на его декларацию в том же файле.
 *
 * Резолв - эвристикой `TaktSymbolScanner` (первая одноимённая декларация файла; без
 * областей видимости - названная граница разбора). **Мягкая** (`soft = true`):
 * неразрешённое имя (кросс-файловое, имя состояния, встроенное) не подсвечивается
 * ошибкой. Даёт навигацию, когда сервер недоступен: это тихая деградация.
 *
 * **Переименование эта ссылка не обслуживает**: его делает
 * сервер через `LSPRenameHandler`, потому что у него есть области видимости и
 * рабочая область, а здесь - эвристика одного файла. Возвращать сюда
 * `handleElementRename` нельзя: вместе с ним вернётся и `PsiNamedElement` у
 * декларации, а он **перекрывает** серверный путь.
 */
class TaktNameReference(element: TaktNameRef) :
    PsiReferenceBase<TaktNameRef>(element, TextRange(0, element.textLength), /* soft = */ true) {

    override fun resolve(): PsiElement? {
        val file = element.containingFile ?: return null
        val name = element.text
        val selfStart = element.textRange.startOffset
        val declRange = TaktSymbolScanner.scan(file.text)
            .firstOrNull { it.name == name && it.range.startOffset != selfStart }
            ?: return null
        val leaf = file.findElementAt(declRange.range.startOffset) ?: return null
        return leaf.parent as? TaktNameDecl ?: leaf
    }
}

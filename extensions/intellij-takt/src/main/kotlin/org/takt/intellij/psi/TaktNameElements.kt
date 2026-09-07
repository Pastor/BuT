package org.takt.intellij.psi

import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.lang.ASTNode
import com.intellij.psi.PsiReference
import com.intellij.psi.impl.source.tree.LeafElement

/**
 * Композитные узлы имён Takt (переименование отдано серверу
 * языка).
 *
 * Каждый узел оборачивает **один** листовой токен `IDENTIFIER`. Роль
 * (декларация/использование) определяет **парсер** по эвристике
 * `TaktSymbolScanner` (единый источник форм `kw <Id>`/`Import`/`enum`), а не сам
 * узел - второй реализации грамматики не появляется.
 *
 * **Декларация не реализует `PsiNameIdentifierOwner` - и это несущее
 * решение, а не упрощение**. Реализуй его плагин, и IDEA считала
 * доступным штатный `PsiElementRenameHandler`, а `LSPRenameHandler` из LSP4IJ
 * **уступал**: его предикат требует, чтобы других доступных обработчиков не
 * было (либо чтобы все они были `VariableInplaceRenameHandler`). То есть
 * серверный `rename` - с областями видимости, рабочей областью и правилом
 * "полнота или отказ" - не включился бы **никогда**, а работала
 * эвристика "первая одноимённая декларация файла" без областей видимости.
 *
 * Узлы и ссылки при этом сохранены: на них держится навигация, когда сервер
 * недоступен: это тихая деградация.
 */

/** Общая база узлов-идентификаторов. */
sealed class TaktIdentifierElement(node: ASTNode) : ASTWrapperPsiElement(node)

/**
 * Идентификатор-декларация: цель навигации и разрешения ссылок.
 *
 * Именованным элементом IDEA (`PsiNamedElement`) **намеренно не является** - см.
 * предупреждение выше.
 */
class TaktNameDecl(node: ASTNode) : TaktIdentifierElement(node)

/** Идентификатор-использование: несёт `PsiReference` к декларации в файле. */
class TaktNameRef(node: ASTNode) : TaktIdentifierElement(node) {
    override fun getReference(): PsiReference = TaktNameReference(this)
    override fun getReferences(): Array<PsiReference> = arrayOf(reference)
}

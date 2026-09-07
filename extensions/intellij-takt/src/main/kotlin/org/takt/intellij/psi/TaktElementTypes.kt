package org.takt.intellij.psi

/**
 * Композитные узлы PSI-дерева Takt.
 *
 * Дерево остаётся **почти плоским**: в композиты оборачиваются
 * **только** те одиночные токены, что несут ссылки/имена - иначе `PsiReference`
 * и `PsiNamedElement` невозможны: контрибьютор и ссылка не
 * привязываются к листовому `LeafPsiElement`). Выражения/условия/типы/приоритеты
 * не оборачиваются и грамматику не дублируют.
 */
object TaktElementTypes {
    /** Строка-путь директивы `import` (носитель файловой `PsiReference`). */
    @JvmField val IMPORT_PATH = TaktElementType("IMPORT_PATH")

    /** Идентификатор-**декларация** имени Takt (цель навигации; не `PsiNamedElement`). */
    @JvmField val NAME_DECL = TaktElementType("NAME_DECL")

    /** Идентификатор-**использование** имени Takt (носитель `PsiReference`). */
    @JvmField val NAME_REF = TaktElementType("NAME_REF")
}

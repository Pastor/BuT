package org.takt.intellij.psi

/**
 * Композитные узлы PSI-дерева Takt (фича 0067, Option B ADR 0067).
 *
 * Дерево остаётся **почти плоским** (ADR 0023): в композиты оборачиваются
 * **только** те одиночные токены, что несут ссылки/имена - иначе `PsiReference`
 * и `PsiNamedElement` невозможны (проба 0067 доказала: контрибьютор/ссылка не
 * привязываются к листовому `LeafPsiElement`). Выражения/условия/типы/приоритеты
 * не оборачиваются и грамматику не дублируют.
 */
object TaktElementTypes {
    /** Строка-путь директивы `import` (носитель файловой `PsiReference`, R5). */
    @JvmField val IMPORT_PATH = TaktElementType("IMPORT_PATH")

    /** Идентификатор-**декларация** имени Takt (цель навигации; с 0154 - не `PsiNamedElement`). */
    @JvmField val NAME_DECL = TaktElementType("NAME_DECL")

    /** Идентификатор-**использование** имени Takt (носитель `PsiReference`, R3). */
    @JvmField val NAME_REF = TaktElementType("NAME_REF")
}

package org.takt.intellij

import com.intellij.refactoring.rename.PsiElementRenameHandler
import com.intellij.refactoring.util.CommonRefactoringUtil
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.takt.intellij.psi.TaktNameDecl
import org.takt.intellij.refactoring.TaktNamesValidator

/**
 * Переименование отдано серверу (фича 0154).
 *
 * Прежде эти тесты проверяли **свой** rename плагина: декларация была
 * `PsiNameIdentifierOwner`, использования - ссылками, и IDEA правила текст сама.
 * Реализация снята: она перекрывала серверный `textDocument/rename`, у которого
 * есть области видимости и вся рабочая область (0131, 0153), тогда как здесь
 * работала эвристика "первая одноимённая декларация файла".
 *
 * **Что именно тут проверяется.** Живой сервер в тестах не поднимается,
 * поэтому "сервер переименовал" здесь недоказуемо - это ручная проверка в IDE
 * (отчёт фичи). Зато проверяется **условие**, при котором LSP4IJ вообще берёт
 * запрос: его `LSPRenameHandler.isAvailableOnDataContext` требует, чтобы других
 * доступных обработчиков не было (либо чтобы все они были
 * `VariableInplaceRenameHandler`). Пока условие выполнено - путь к серверу
 * свободен; сломается оно - сломается и переименование, молча.
 */
class TaktRenameTest : BasePlatformTestCase() {

    /**
     * главный сторож: наш символ нативным рефакторингом **не переименовывается**
     * - значит Shift+F6 на нём достаётся серверу.
     *
     * Проверяется именно `canRename` на нашем узле, а не пустота списка
     * `RenameHandlerRegistry.getRenameHandlers`. Зонд 2026-08-18 показал, почему:
     * в синтетическом `DataContext` теста `PsiElementRenameHandler` доступен даже
     * для `probe.txt`, где PSI плагина нет вовсе, - он предлагает переименовать
     * **файл**. Сторож на пустоту списка проверял бы наличие файла, а не наше
     * устройство, и был бы красным всегда.
     *
     * Мутация, которую тест обязан ловить: вернуть `TaktNameDecl` реализацию
     * `PsiNameIdentifierOwner`. Тогда `canRename` станет истинным, нативный
     * рефакторинг перехватит запрос, и переименование снова уйдёт в эвристику
     * "первая одноимённая декларация файла" - без единого сообщения.
     */
    fun testDeclarationIsNotRenamableNatively() {
        myFixture.configureByText("test.takt", "model Produ<caret>cer { }\nstart Main = Producer { }\n")
        val at = myFixture.file.findElementAt(myFixture.editor.caretModel.offset)
        val decl = generateSequence(at) { it.parent }.filterIsInstance<TaktNameDecl>().firstOrNull()
        assertNotNull("узел-декларация на месте: навигация на нём держится", decl)
        val renamable = try {
            PsiElementRenameHandler.canRename(project, myFixture.editor, decl)
        } catch (_: CommonRefactoringUtil.RefactoringErrorHintException) {
            false
        }
        assertFalse(
            "нативный рефакторинг не должен браться за символ Takt — иначе он перекроет сервер",
            renamable,
        )
    }

    /**
     * Декларация перестала быть именованным элементом IDEA - это и есть причина,
     * по которой нативный обработчик недоступен.
     */
    fun testDeclarationIsNotNamedElement() {
        myFixture.configureByText("test.takt", "model Produ<caret>cer { }\n")
        val at = myFixture.file.findElementAt(myFixture.editor.caretModel.offset)
        val decl = generateSequence(at) { it.parent }.filterIsInstance<TaktNameDecl>().firstOrNull()
        assertNotNull("узел-декларация на месте: навигация на нём держится", decl)
        // Проверка идёт через `Any`, а не напрямую: на статическом типе
        // компилятор Kotlin сворачивает её в "always false" и валит сборку
        // (`-Werror`). Свёртка - тоже доказательство, но только для этого типа;
        // здесь же сторож смотрит на фактический класс узла и переживёт, если
        // интерфейс добавят предку.
        val asAny: Any? = decl
        assertFalse(
            "декларация не должна быть PsiNamedElement — иначе она перекроет сервер",
            asAny is com.intellij.psi.PsiNamedElement,
        )
    }

    /**
     * Навигация без сервера сохранена: ссылка использования по-прежнему
     * разрешается в декларацию того же файла (тихая деградация 0038).
     */
    fun testReferenceStillResolvesForNavigation() {
        myFixture.configureByText("test.takt", "model Producer { }\nstart Main = Produ<caret>cer { }\n")
        val at = myFixture.file.findElementAt(myFixture.editor.caretModel.offset)
        val ref = generateSequence(at) { it.parent }.mapNotNull { it.reference }.firstOrNull()
        assertNotNull("ссылка на использовании обязана остаться", ref)
        assertNotNull("ссылка обязана разрешаться в декларацию", ref!!.resolve())
    }


    /**
     * Валидатор имён остаётся: `lang.namesValidator` - точка расширения **языка**,
     * её спрашивает инфраструктура рефакторинга при вводе нового имени, а не
     * конкретный обработчик.
     */
    fun testNamesValidatorRejectsKeywords() {
        val v = TaktNamesValidator()
        assertTrue(v.isKeyword("model", project))
        assertTrue(v.isKeyword("address", project))
        assertFalse(v.isKeyword("Producer", project))
        assertFalse("ключевое слово — не идентификатор", v.isIdentifier("state", project))
        assertTrue(v.isIdentifier("Producer", project))
        assertTrue(v.isIdentifier("_x1", project))
        assertFalse(v.isIdentifier("1abc", project))
        assertFalse(v.isIdentifier("", project))
    }
}

package org.takt.intellij

import com.intellij.psi.PsiFile
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.takt.intellij.navigation.TaktGotoDeclarationHandler

/**
 * Проверки навигации по директивам `import` к файлу (фича 0023, критерии A4/A5).
 *
 * Навигация ведётся `GotoDeclarationHandler` (см. [org.takt.intellij.navigation.TaktImports]).
 */
class TaktImportReferenceTest : BasePlatformTestCase() {

    private val handler = TaktGotoDeclarationHandler()

    /** Цель перехода из строки-пути под кареткой (маркер `<caret>`). */
    private fun importTargetAtCaret(mainCode: String, addShared: Boolean = true): PsiFile? {
        if (addShared) myFixture.addFileToProject("shared.takt", "model SharedModel { }\n")
        myFixture.configureByText("main.takt", mainCode)
        val element = myFixture.file.findElementAt(myFixture.caretOffset)
        val targets = handler.getGotoDeclarationTargets(element, myFixture.caretOffset, myFixture.editor)
        return targets?.singleOrNull() as? PsiFile
    }

    fun testPlainImportResolvesToFile() {
        val file = importTargetAtCaret("""import "sha<caret>red.takt";""")
        assertNotNull(file)
        assertEquals("shared.takt", file!!.name)
    }

    fun testFromImportResolvesToFile() {
        val file = importTargetAtCaret("""import { SharedModel as M } from "sha<caret>red.takt";""")
        assertNotNull(file)
        assertEquals("shared.takt", file!!.name)
    }

    fun testImportAsResolvesToFile() {
        val file = importTargetAtCaret("""import "sha<caret>red.takt" as S;""")
        assertNotNull(file)
        assertEquals("shared.takt", file!!.name)
    }

    fun testMissingFileHasNoTarget() {
        // Файла нет - цели нет, без исключений.
        val file = importTargetAtCaret("""import "no_su<caret>ch.takt";""", addShared = false)
        assertNull(file)
    }

    fun testNonImportStringHasNoTarget() {
        // Строка вне import (в formula) не должна давать файловую навигацию.
        myFixture.addFileToProject("shared.takt", "model SharedModel { }\n")
        myFixture.configureByText("main.takt", """model M { always { formula "sha<caret>red.takt"; } }""")
        val element = myFixture.file.findElementAt(myFixture.caretOffset)
        val targets = handler.getGotoDeclarationTargets(element, myFixture.caretOffset, myFixture.editor)
        assertNull(targets)
    }
}

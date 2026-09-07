package org.takt.intellij

import com.intellij.psi.PsiFile
import com.intellij.testFramework.fixtures.BasePlatformTestCase

/**
 * Проверки настоящей `PsiReference` на строке-пути `import` (фича 0067, R5).
 *
 * В отличие от навигации через `GotoDeclarationHandler` (0023, тест
 * [TaktImportReferenceTest]), здесь проверяется именно **ссылка**: `resolve()` во
 * всех формах `import` и **rename-on-move** (переименование файла обновляет путь).
 */
class TaktImportPsiReferenceTest : BasePlatformTestCase() {

    /** Ссылка под кареткой (маркер `<caret>`), либо `null`. */
    private fun refResolvesTo(mainCode: String, addShared: Boolean = true): PsiFile? {
        if (addShared) myFixture.addFileToProject("shared.takt", "model SharedModel { }\n")
        myFixture.configureByText("main.takt", mainCode)
        val ref = myFixture.getReferenceAtCaretPosition() ?: return null
        return ref.resolve() as? PsiFile
    }

    fun testPlainImportResolves() {
        val f = refResolvesTo("""import "sha<caret>red.takt";""")
        assertNotNull("ссылка должна резолвиться в файл", f)
        assertEquals("shared.takt", f!!.name)
    }

    fun testImportAsResolves() {
        val f = refResolvesTo("""import "sha<caret>red.takt" as S;""")
        assertNotNull(f); assertEquals("shared.takt", f!!.name)
    }

    fun testStarFromImportResolves() {
        val f = refResolvesTo("""import * as S from "sha<caret>red.takt";""")
        assertNotNull(f); assertEquals("shared.takt", f!!.name)
    }

    fun testNamedFromImportResolves() {
        val f = refResolvesTo("""import { SharedModel as M } from "sha<caret>red.takt";""")
        assertNotNull(f); assertEquals("shared.takt", f!!.name)
    }

    /** R5.2: переименование целевого файла обновляет строку-путь в тексте import. */
    fun testRenameTargetFileUpdatesImportPath() {
        val target = myFixture.addFileToProject("shared.takt", "model SharedModel { }\n")
        myFixture.configureByText("main.takt", """import "sha<caret>red.takt";""")
        assertNotNull("до rename ссылка должна быть", myFixture.getReferenceAtCaretPosition())

        myFixture.renameElement(target, "renamed.takt")

        val text = myFixture.file.text
        assertTrue("путь должен стать renamed.takt, получили: $text",
            text.contains("""import "renamed.takt";"""))
    }

    /** R5.3: битый путь - resolve() == null, без исключений. */
    fun testMissingFileResolvesToNull() {
        val f = refResolvesTo("""import "no_su<caret>ch.takt";""", addShared = false)
        assertNull(f)
    }

    /** Контрпример: строка вне import (в formula) ссылки не несёт (регресс 0023). */
    fun testNonImportStringHasNoReference() {
        myFixture.addFileToProject("shared.takt", "model SharedModel { }\n")
        myFixture.configureByText(
            "main.takt",
            """model M { always { formula "sha<caret>red.takt"; } }""",
        )
        assertNull(myFixture.getReferenceAtCaretPosition())
    }
}

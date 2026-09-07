package org.takt.intellij

import com.intellij.psi.PsiElement
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.takt.intellij.navigation.TaktGotoDeclarationHandler

/**
 * Проверки перехода к декларации (фича 0023, критерии A2/A3).
 */
class TaktGotoDeclarationTest : BasePlatformTestCase() {

    private val handler = TaktGotoDeclarationHandler()

    /** Цели перехода из позиции каретки (маркер `<caret>` в тексте). */
    private fun targetsAtCaret(code: String): Array<PsiElement>? {
        myFixture.configureByText("test.takt", code)
        val element = myFixture.file.findElementAt(myFixture.caretOffset)
        return handler.getGotoDeclarationTargets(element, myFixture.caretOffset, myFixture.editor)
    }

    fun testJumpFromUsageToModelDeclaration() {
        val targets = targetsAtCaret(
            """
            model Producer { }
            start Main = Produ<caret>cer { }
            """.trimIndent(),
        )
        assertNotNull(targets)
        val target = targets!!.single()
        assertEquals("Producer", target.text)
        // Цель - имя в объявлении `model Producer`, т.е. первое вхождение.
        assertTrue(target.textRange.startOffset < myFixture.caretOffset)
    }

    fun testJumpFromTypeUsageToTypeDeclaration() {
        val targets = targetsAtCaret(
            """
            type Speed = bit;
            var v: Spe<caret>ed := 0;
            """.trimIndent(),
        )
        assertNotNull(targets)
        assertEquals("Speed", targets!!.single().text)
    }

    fun testJumpToImportedAlias() {
        val targets = targetsAtCaret(
            """
            import { SharedModel as M } from "shared.takt";
            start Entry = <caret>M { }
            """.trimIndent(),
        )
        assertNotNull(targets)
        assertEquals("M", targets!!.single().text)
    }

    fun testJumpFromBitAccessPortToDeclaration() {
        // Порт как часть выражения `port.N` (BitAccess): каретка на имени порта.
        val targets = targetsAtCaret(
            """
            in sensors_cab: u8 := 0x10000009;
            cond Occupied = sensors_<caret>cab.0;
            """.trimIndent(),
        )
        assertNotNull(targets)
        assertEquals("sensors_cab", targets!!.single().text)
        assertTrue(targets.single().textRange.startOffset < myFixture.caretOffset)
    }

    fun testJumpFromEnumVariantUsageToDeclaration() {
        val targets = targetsAtCaret(
            """
            enum Action { Idle = 670, Closing }
            var a: Action := Clos<caret>ing;
            """.trimIndent(),
        )
        assertNotNull(targets)
        assertEquals("Closing", targets!!.single().text)
        assertTrue(targets.single().textRange.startOffset < myFixture.caretOffset)
    }

    fun testNoTargetOnKeyword() {
        assertNull(targetsAtCaret("mod<caret>el Foo { }"))
    }

    fun testNoTargetOnDeclarationItself() {
        // На самом объявлении (нет другого одноимённого) переход отсутствует.
        assertNull(targetsAtCaret("model Fo<caret>o { }"))
    }

    fun testNoTargetForUnknownIdentifier() {
        assertNull(targetsAtCaret("start Main = Unkno<caret>wn { }"))
    }
}

package org.takt.intellij

import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.junit.Assert.assertNotEquals

/**
 * Проверки каркаса плагина:
 * тип файла `.takt` заведён и корректно связан с языком Takt.
 */
class TaktFileTypeTest : BasePlatformTestCase() {

    fun testDefaultExtensionIsLam() {
        assertEquals("takt", TaktFileType.getDefaultExtension())
    }

    fun testFileTypeName() {
        assertEquals("Takt", TaktFileType.name)
    }

    fun testFileTypeBoundToLamLanguage() {
        assertSame(TaktLanguage, TaktFileType.language)
        assertEquals("Takt", TaktLanguage.id)
    }

    fun testLamFileIsRecognizedAsLamType() {
        val file = myFixture.configureByText("sample.takt", "start Start { next End; }")
        assertEquals(TaktFileType, file.virtualFile.fileType)
        assertNotEquals("PLAIN_TEXT", file.virtualFile.fileType.name)
    }

    fun testIconLoads() {
        // Иконка типа файла должна загружаться из ресурсов (icons/takt.svg).
        assertNotNull(TaktFileType.icon)
    }
}

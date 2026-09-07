package org.takt.intellij

import junit.framework.TestCase
import org.takt.intellij.lsp.TaktLspSettings

/**
 * Тесты состояния настроек инструментов (фича 0125).
 *
 * Без GUI/платформы: используем прямой конструктор [TaktLspSettings] (не
 * `getInstance`, требующий `ApplicationManager`). Проверяем, что новые поля
 * (`compilerPath`/`simulatorPath`/`includeDirs`/`compilerArgs`/`outputDir`)
 * читаются/пишутся через аксессоры и переносятся `loadState` - иначе настройки
 * не сериализовались бы `PersistentStateComponent`.
 */
class TaktLspSettingsTest : TestCase() {

    /** Умолчания пусты (аддитивность: без настройки поведение прежнее). */
    fun testDefaultsAreEmpty() {
        val s = TaktLspSettings()
        assertEquals("", s.serverPath)
        assertEquals("", s.compilerPath)
        assertEquals("", s.simulatorPath)
        assertTrue(s.includeDirs.isEmpty())
        assertEquals("", s.compilerArgs)
        assertEquals("", s.outputDir)
    }

    /** Все новые поля переживают перенос состояния `getState` -> `loadState`. */
    fun testStateRoundTrip() {
        val src = TaktLspSettings()
        src.serverPath = "/bin/takt-lsp"
        src.compilerPath = "/bin/taktc"
        src.simulatorPath = "/bin/takt-sim"
        src.includeDirs = mutableListOf("/lib", "/shared")
        src.compilerArgs = "--float-embedded"
        src.outputDir = "/out"

        val dst = TaktLspSettings()
        dst.loadState(src.getState())

        assertEquals("/bin/takt-lsp", dst.serverPath)
        assertEquals("/bin/taktc", dst.compilerPath)
        assertEquals("/bin/takt-sim", dst.simulatorPath)
        assertEquals(listOf("/lib", "/shared"), dst.includeDirs)
        assertEquals("--float-embedded", dst.compilerArgs)
        assertEquals("/out", dst.outputDir)
    }
}

package org.takt.intellij

import junit.framework.TestCase
import org.takt.intellij.lsp.TaktInitOptions

/**
 * Тесты сборки `initializationOptions` из каталогов `-I` (фича 0125).
 *
 * Чистая логика без GUI (драйвер 5 ADR 0038): проверяем, что каталоги импортов
 * превращаются в `{ "searchPaths": [...] }` (контракт 0072), пустой список даёт
 * `null` (прежнее поведение - аддитивность), а построчный разбор поля настроек
 * согласован с обратной сборкой.
 */
class TaktInitOptionsTest : TestCase() {

    /** Пустой список ⇒ `null` (опции не слать - прежнее поведение). */
    fun testBuildEmptyGivesNull() {
        assertNull(TaktInitOptions.build(emptyList()))
    }

    /** Список только из пустых/пробельных ⇒ тоже `null`. */
    fun testBuildBlankOnlyGivesNull() {
        assertNull(TaktInitOptions.build(listOf("", "   ", "\t")))
    }

    /** Непустой список ⇒ `{ "searchPaths": [...] }` с теми же путями и порядком. */
    fun testBuildNonEmpty() {
        val opts = TaktInitOptions.build(listOf("/lib", "/shared"))
        assertNotNull(opts)
        assertEquals(listOf("/lib", "/shared"), opts!![TaktInitOptions.SEARCH_PATHS])
    }

    /** Пробелы по краям тримятся, пустые записи отбрасываются, порядок сохранён. */
    fun testBuildTrimsAndDropsBlank() {
        val opts = TaktInitOptions.build(listOf("  /lib  ", "", "  ", "/x"))
        assertEquals(listOf("/lib", "/x"), opts!![TaktInitOptions.SEARCH_PATHS])
    }

    /** Разбор построчного текста: тримминг, отбрасывание пустых, сохранение порядка. */
    fun testParseDirs() {
        assertEquals(
            listOf("/a", "/b", "/c"),
            TaktInitOptions.parseDirs("/a\n  /b  \n\n/c\n"),
        )
    }

    /** `joinDirs` ∘ `parseDirs` даёт исходный набор путей (round-trip). */
    fun testJoinParseRoundTrip() {
        val dirs = listOf("/a", "/b/c", "rel/dir")
        assertEquals(dirs, TaktInitOptions.parseDirs(TaktInitOptions.joinDirs(dirs)))
    }

    /** Пустой текст поля ⇒ пустой список ⇒ `build` даёт `null`. */
    fun testEmptyTextGivesNull() {
        assertNull(TaktInitOptions.build(TaktInitOptions.parseDirs("")))
    }
}

package org.takt.intellij

import junit.framework.TestCase
import org.takt.intellij.lsp.TaktLspBinary
import java.io.File

/**
 * Тесты резолвинга пути к `takt-lsp` (фича 0038, задача 0038-01, критерий A7).
 *
 * Чистая логика без GUI (драйвер 5 ADR): проверяем приоритет источников и тихую
 * деградацию (не найден -> `null`, без исключений). Живой запуск сервера - только
 * визуально в `runIde` (A11), в CI недостижим.
 */
class TaktLspBinaryTest : TestCase() {

    private lateinit var tempDir: File

    override fun setUp() {
        super.setUp()
        tempDir = File.createTempFile("takt-lsp-test", "").let {
            it.delete()
            it.mkdirs()
            it
        }
    }

    override fun tearDown() {
        try {
            tempDir.deleteRecursively()
        } finally {
            super.tearDown()
        }
    }

    /** Создаёт исполняемый файл `name` в каталоге `dir`. */
    private fun makeExecutable(dir: File, name: String): File {
        val f = File(dir, name)
        f.writeText("#!/bin/sh\n")
        assertTrue("не удалось сделать файл исполняемым", f.setExecutable(true))
        return f
    }

    /** Явная настройка на существующий исполняемый файл -> он и возвращается. */
    fun testExplicitPathResolves() {
        val exe = makeExecutable(tempDir, "takt-lsp")
        val resolved = TaktLspBinary.resolve(exe.absolutePath, pathEnv = null)
        assertEquals(exe.absolutePath, resolved?.absolutePath)
    }

    /** Явный путь на несуществующий файл -> `null` (деградация, без исключения). */
    fun testExplicitMissingPathIsNull() {
        val missing = File(tempDir, "nope").absolutePath
        assertNull(TaktLspBinary.resolve(missing, pathEnv = null))
    }

    /** Явный путь на неисполняемый файл -> `null`. */
    fun testExplicitNonExecutableIsNull() {
        val f = File(tempDir, "takt-lsp")
        f.writeText("not executable")
        f.setExecutable(false)
        assertNull(TaktLspBinary.resolve(f.absolutePath, pathEnv = null))
    }

    /** Пустая настройка + бинарник в `PATH` -> находится автопоиском. */
    fun testAutodiscoveryOnPath() {
        val exe = makeExecutable(tempDir, TaktLspBinary.EXECUTABLE)
        val resolved = TaktLspBinary.resolve(configuredPath = "", pathEnv = tempDir.absolutePath)
        assertEquals(exe.absolutePath, resolved?.absolutePath)
    }

    /** Пустая настройка + бинарника в `PATH` нет -> `null`. */
    fun testAutodiscoveryMissingIsNull() {
        assertNull(TaktLspBinary.resolve(configuredPath = null, pathEnv = tempDir.absolutePath))
    }

    /** Явная настройка имеет приоритет над `PATH`. */
    fun testExplicitBeatsPath() {
        val onPath = makeExecutable(tempDir, TaktLspBinary.EXECUTABLE)
        val explicitDir = File(tempDir, "explicit").apply { mkdirs() }
        val explicit = makeExecutable(explicitDir, "takt-lsp")
        val resolved = TaktLspBinary.resolve(explicit.absolutePath, pathEnv = tempDir.absolutePath)
        assertEquals(explicit.absolutePath, resolved?.absolutePath)
        assertFalse("должна победить явная настройка, не PATH", resolved?.absolutePath == onPath.absolutePath)
    }
}

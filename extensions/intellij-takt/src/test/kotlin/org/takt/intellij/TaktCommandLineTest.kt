package org.takt.intellij

import junit.framework.TestCase
import org.takt.intellij.run.TaktCommandLine
import org.takt.intellij.run.TaktCommandLine.Mode
import org.takt.intellij.run.TaktCommandLine.Params
import org.takt.intellij.run.TaktCommandLine.Result
import org.takt.intellij.run.TaktCommandLine.Tools

/**
 * Сборка командной строки инструментов Takt (фича 0158).
 *
 * Тесты идут на **чистую функцию**, а не на диалог IDE: плагин вне
 * `precheck.sh`, и проверяемо здесь ровно то, что не требует окна. Что процесс
 * действительно стартовал и вывод попал в консоль - ручная проверка (отчёт).
 */
class TaktCommandLineTest : TestCase() {

    private val tools = Tools(
        compilerPath = "/opt/takt/taktc",
        simulatorPath = "/opt/takt/takt-sim",
        includeDirs = listOf("/lib/a", "/lib/b"),
    )

    private fun ready(result: Result): List<String> = when (result) {
        is Result.Ready -> result.command
        is Result.Refused -> throw AssertionError("ожидалась команда, получен отказ: ${result.message}")
    }

    /** A2: компиляция - полный список аргументов, включая порядок. */
    fun testCompileCommandLine() {
        val command = ready(
            TaktCommandLine.build(
                Mode.COMPILE,
                Params(filePath = "/p/model.takt", target = "sv", outputDir = "/p/out"),
                tools,
            )
        )
        assertEquals(
            listOf(
                "/opt/takt/taktc", "compile",
                "-t", "sv",
                "-I", "/lib/a",
                "-I", "/lib/b",
                "-o", "/p/out",
                "/p/model.takt",
            ),
            command,
        )
    }

    /** A3: симуляция - свой набор флагов. */
    fun testSimulateCommandLine() {
        val command = ready(
            TaktCommandLine.build(
                Mode.SIMULATE,
                Params(filePath = "/p/model.takt", scenario = "/p/s.json", steps = "20"),
                tools,
            )
        )
        assertEquals(
            listOf(
                "/opt/takt/takt-sim",
                "-I", "/lib/a",
                "-I", "/lib/b",
                "-s", "/p/s.json",
                "-n", "20",
                "/p/model.takt",
            ),
            command,
        )
    }

    /**
     * A4: незаданное поле - это отсутствие флага, а не флаг с пустой строкой.
     *
     * Иначе инструмент получил бы `-o ""` и создал каталог с пустым именем
     * либо отказал - на входе, где пользователь просто ничего не указал.
     */
    fun testEmptyFieldsProduceNoFlags() {
        val command = ready(
            TaktCommandLine.build(Mode.COMPILE, Params(filePath = "/p/m.takt"), Tools(compilerPath = "taktc"))
        )
        assertEquals(listOf("taktc", "compile", "/p/m.takt"), command)
        assertFalse("пустых аргументов быть не должно", command.any { it.isBlank() })
    }

    /**
     * A2: свободные флаги пользователя не разрывают пару `-o путь`.
     *
     * Порядок закреплён намеренно: файл идёт последним, а `-o` - сразу перед
     * ним, чтобы вставка чужих флагов не оказалась между флагом и его значением.
     */
    fun testExtraArgsDoNotBreakOutputPair() {
        val command = ready(
            TaktCommandLine.build(
                Mode.COMPILE,
                Params(filePath = "/p/m.takt", outputDir = "/out", extraArgs = "  --quiet   --float-width=32 "),
                Tools(compilerPath = "taktc"),
            )
        )
        assertEquals(
            listOf("taktc", "compile", "--quiet", "--float-width=32", "-o", "/out", "/p/m.takt"),
            command,
        )
    }

    /** A5: путь к инструменту не задан - сообщение, а не исключение. */
    fun testMissingBinaryIsRefusedWithMessage() {
        val result = TaktCommandLine.build(Mode.COMPILE, Params(filePath = "/p/m.takt"), Tools())
        assertTrue("ожидался отказ", result is Result.Refused)
        val message = (result as Result.Refused).message
        assertTrue("отказ обязан называть, что делать: $message", message.contains("Settings"))
        assertTrue("и какой инструмент: $message", message.contains("taktc"))
    }

    /** Симулятору нужен свой путь: отказ говорит именно о нём. */
    fun testMissingSimulatorNamesTheRightTool() {
        val result = TaktCommandLine.build(
            Mode.SIMULATE, Params(filePath = "/p/m.takt"), Tools(compilerPath = "taktc"),
        )
        val message = (result as Result.Refused).message
        assertTrue("отказ обязан называть симулятор: $message", message.contains("takt-sim"))
    }

    /** Файл не выбран - тоже внятный отказ, а не пустой аргумент. */
    fun testMissingFileIsRefused() {
        val result = TaktCommandLine.build(Mode.COMPILE, Params(filePath = "  "), tools)
        assertTrue(result is Result.Refused)
    }

    /**
     * Каталоги импортов идут **повторяемым** `-I`, а не одной строкой через
     * разделитель: разделитель у компилятора платформозависим (`:` против `;`),
     * и собирать его в плагине значило бы завести второе знание о нём.
     */
    fun testIncludeDirsAreRepeatedFlags() {
        val command = ready(
            TaktCommandLine.build(
                Mode.COMPILE,
                Params(filePath = "/p/m.takt"),
                Tools(compilerPath = "taktc", includeDirs = listOf("/a", " ", "/b")),
            )
        )
        assertEquals(2, command.count { it == "-I" })
        assertFalse("пробельный каталог не должен попадать", command.contains(" "))
    }
}

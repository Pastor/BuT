package org.takt.intellij.run

/**
 * Сборка командной строки инструментов Takt (фича 0158).
 *
 * **Чистая функция и ничего кроме.** Плагин лежит вне `precheck.sh`, поэтому
 * всё, что можно проверить Gradle-тестом без окна IDE, обязано быть свободным от
 * UI (драйвер 5 ADR 0038). Здесь нет ни `Project`, ни `Editor`, ни настроек как
 * сервиса - только данные, поданные вызывающим.
 *
 * Плагин **не толкует** флаги: смысл `-t`, `-I`, `-o` остаётся у `taktc` и
 * `takt-sim`. Здесь только порядок и подстановка.
 */
object TaktCommandLine {

    /** Что запускаем. */
    enum class Mode {
        /** `taktc compile` - генерация кода. */
        COMPILE,

        /** `takt-sim` - прогон модели по тактам. */
        SIMULATE,
    }

    /** Параметры конкретного запуска (то, что задал пользователь в конфигурации). */
    data class Params(
        /** Путь к `.takt`-файлу - обязателен. */
        val filePath: String,
        /** Цель генерации (`-t`); пусто ⇒ умолчание компилятора (`c`). */
        val target: String = "",
        /** Выходной каталог (`-o`); пусто ⇒ флаг не подставляется. */
        val outputDir: String = "",
        /** Файл сценария (`-s`, только симуляция); пусто ⇒ без сценария. */
        val scenario: String = "",
        /** Число шагов (`-n`, только симуляция); пусто ⇒ до терминального состояния. */
        val steps: String = "",
        /** Свободные флаги пользователя, как строка. */
        val extraArgs: String = "",
    )

    /** Настройки плагина, нужные для запуска (пути и общие каталоги). */
    data class Tools(
        val compilerPath: String = "",
        val simulatorPath: String = "",
        val includeDirs: List<String> = emptyList(),
    )

    /** Результат сборки: либо строка запуска, либо причина отказа для пользователя. */
    sealed interface Result {
        data class Ready(val command: List<String>) : Result
        data class Refused(val message: String) : Result
    }

    /**
     * Собирает командную строку либо объясняет, почему не может.
     *
     * Порядок аргументов закреплён: файл идёт **последним**, как в справке
     * инструментов, а свободные флаги пользователя вставляются перед `-o`, чтобы
     * не разорвать пару "флаг - значение".
     *
     * Пустое поле не даёт пустого аргумента: незаданная цель - это отсутствие
     * `-t` вовсе, а не `-t ""`. Иначе инструмент получил бы пустую строку там,
     * где ждёт имя.
     */
    fun build(mode: Mode, params: Params, tools: Tools): Result {
        val binary = when (mode) {
            Mode.COMPILE -> tools.compilerPath
            Mode.SIMULATE -> tools.simulatorPath
        }.trim()
        if (binary.isEmpty()) {
            val what = if (mode == Mode.COMPILE) "компилятора taktc" else "симулятора takt-sim"
            return Result.Refused(
                "Путь к $what не задан. Укажите его в «Settings | Languages & Frameworks | Takt»."
            )
        }
        val file = params.filePath.trim()
        if (file.isEmpty()) {
            return Result.Refused("Не выбран файл .takt для запуска.")
        }

        val command = mutableListOf(binary)
        when (mode) {
            Mode.COMPILE -> {
                command += "compile"
                addFlag(command, "-t", params.target)
                addIncludeDirs(command, tools.includeDirs)
                command += splitArgs(params.extraArgs)
                addFlag(command, "-o", params.outputDir)
            }
            Mode.SIMULATE -> {
                addIncludeDirs(command, tools.includeDirs)
                addFlag(command, "-s", params.scenario)
                addFlag(command, "-n", params.steps)
                command += splitArgs(params.extraArgs)
                addFlag(command, "-o", params.outputDir)
            }
        }
        command += file
        return Result.Ready(command)
    }

    /** Добавляет пару "флаг - значение", если значение непусто. */
    private fun addFlag(command: MutableList<String>, flag: String, value: String) {
        val trimmed = value.trim()
        if (trimmed.isNotEmpty()) {
            command += flag
            command += trimmed
        }
    }

    /**
     * Каталоги импортов - **повторяемым** `-I`, а не одним списком через
     * разделитель: разделитель у компилятора платформозависим (`:` против `;`),
     * и собирать его в плагине значило бы завести второе знание о нём.
     */
    private fun addIncludeDirs(command: MutableList<String>, dirs: List<String>) {
        dirs.map { it.trim() }.filter { it.isNotEmpty() }.forEach {
            command += "-I"
            command += it
        }
    }

    /** Разбивает свободные флаги пользователя по пробелам, пустые отбрасывая. */
    private fun splitArgs(raw: String): List<String> =
        raw.trim().split(Regex("\\s+")).filter { it.isNotEmpty() }
}

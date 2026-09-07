package org.takt.intellij.lsp

/**
 * Сборка `initializationOptions` LSP из настроек плагина (фича 0125).
 *
 * Чистая, тестируемая **без GUI** логика (драйвер 5 ADR 0038; тот же приём, что у
 * серверного `init_options::search_paths_from_options`, 0072). Каталоги импортов
 * (`-I`) из [TaktLspSettings.includeDirs] отдаются серверу как
 * `initializationOptions = { "searchPaths": [<dirs>] }`; сервер сам разрешает
 * относительные пути от корня рабочей области (0072) и находит импорт из общих
 * библиотек - иначе диагностика/автодополнение/переход видят только каталог
 * документа.
 *
 * Пустой список ⇒ `null` (опции не слать) - **эквивалентно прежнему поведению**
 * (сервер получит `searchPaths = &[]`), что сохраняет аддитивность (правило 11).
 * Хранение каталогов в UI - построчный текст (по одному пути на строку);
 * преобразования [parseDirs]/[joinDirs] держат текст поля и список в согласии.
 */
object TaktInitOptions {

    /** Ключ массива путей поиска импортов в `initializationOptions` (контракт 0072). */
    const val SEARCH_PATHS: String = "searchPaths"

    /**
     * Собирает `initializationOptions` из каталогов `-I`.
     *
     * @param includeDirs каталоги импортов (могут содержать пустые/пробельные -
     *   отбрасываются)
     * @return `{"searchPaths": [<непустые dirs>]}` либо `null`, если непустых нет
     *   (не слать опции - прежнее поведение сервера)
     */
    fun build(includeDirs: List<String>): Map<String, Any>? {
        val dirs = includeDirs.map { it.trim() }.filter { it.isNotEmpty() }
        return if (dirs.isEmpty()) null else mapOf(SEARCH_PATHS to dirs)
    }

    /**
     * Разбирает построчный текст поля настроек в список каталогов (по строке на
     * путь; пустые строки и пробелы по краям отбрасываются, порядок сохраняется).
     */
    fun parseDirs(text: String): List<String> =
        text.lines().map { it.trim() }.filter { it.isNotEmpty() }

    /** Обратно: список каталогов -> построчный текст поля (по строке на путь). */
    fun joinDirs(dirs: List<String>): String = dirs.joinToString("\n")
}

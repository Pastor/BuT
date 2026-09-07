package org.takt.intellij.run

import com.intellij.execution.filters.Filter
import com.intellij.execution.filters.OpenFileHyperlinkInfo
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.LocalFileSystem

/**
 * Ссылки на место в файле по выводу инструментов Takt (фича 0158).
 *
 * Инструменты печатают позицию единым форматом `путь:строка:колонка: ` -
 * `diagnostics::position_prefix` (0053). Этот фильтр находит её в консоли
 * запуска и делает кликабельной.
 *
 * **Колонка приходит в символах, а не в байтах** - в `.takt` законна
 * кириллица, и проект считает колонку символами намеренно. IDEA же адресует
 * позицию **смещением в документе**, то есть тоже в символах UTF-16 - поэтому
 * пересчёт нужен не из байтов, а из индекса символа в **кодовые единицы**:
 * строка с эмодзи или иным символом вне BMP сдвинула бы ссылку. Пересчёт делает
 * [charColumnToOffset].
 */
class TaktOutputFilter(private val project: Project) : Filter {

    override fun applyFilter(line: String, entireLength: Int): Filter.Result? {
        val match = PATTERN.find(line) ?: return null
        val (path, lineNo, columnNo) = match.destructured
        val file = LocalFileSystem.getInstance().findFileByPath(path) ?: return null

        val lineIndex = (lineNo.toIntOrNull() ?: return null) - 1
        val columnChars = (columnNo.toIntOrNull() ?: return null) - 1
        if (lineIndex < 0 || columnChars < 0) return null

        val info = OpenFileHyperlinkInfo(project, file, lineIndex, columnChars)
        val start = entireLength - line.length + match.range.first
        val end = entireLength - line.length + match.range.last + 1
        return Filter.Result(start, end, info)
    }

    companion object {
        /**
         * `путь:строка:колонка` в начале сообщения.
         *
         * Путь берётся жадно до последних двух двоеточий: в нём самом могут
         * быть двоеточия (macOS их допускает), а строка и колонка - всегда
         * последние два числа перед двоеточием-разделителем.
         */
        private val PATTERN = Regex("""(\S.*?\.takt):(\d+):(\d+):""")

        /**
         * Смещение в строке документа по **символьной** колонке инструмента.
         *
         * Нужно потому, что колонка Takt считается в символах Unicode, а
         * документ IDEA адресуется кодовыми единицами UTF-16. На строке из
         * кириллицы они совпадают, на строке с символом вне BMP (эмодзи в
         * комментарии) - расходятся, и ссылка уводит не туда.
         *
         * @param text текст строки файла
         * @param column номер колонки, считая с 1, в символах Unicode
         */
        fun charColumnToOffset(text: String, column: Int): Int {
            if (column <= 1) return 0
            var chars = 1
            var offset = 0
            while (offset < text.length && chars < column) {
                offset += Character.charCount(text.codePointAt(offset))
                chars++
            }
            return offset
        }
    }
}

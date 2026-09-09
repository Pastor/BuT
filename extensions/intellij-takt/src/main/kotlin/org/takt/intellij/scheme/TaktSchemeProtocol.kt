package org.takt.intellij.scheme

import java.io.File

/**
 * Разговор панели схемы с холстом: что уходит наружу и что приходит обратно.
 *
 * Чистая, тестируемая **без редактора** логика: панель строит сообщения и
 * разбирает ответы холста здесь, а редактор занимается окном и файлами. Форма
 * сообщений задана страницей (`web/static/scheme-host.js`) - второй её описи в
 * плагине нет.
 */
object TaktSchemeProtocol {

    /** Расширение файла раскладки: он лежит рядом с моделью. */
    const val LAYOUT_EXTENSION: String = ".takt-ui"

    /** Расширение файла модели. */
    const val MODEL_EXTENSION: String = ".takt"

    /**
     * Файл раскладки для файла модели; `null` - это не модель.
     *
     * Правило то же, что у страницы: имя модели плюс своё расширение. Разойдись
     * оно - редактор и страница писали бы раскладку в разные файлы.
     */
    fun layoutFileOf(model: File): File? {
        if (!model.name.endsWith(MODEL_EXTENSION)) return null
        return File(model.parentFile, model.name.removeSuffix(MODEL_EXTENSION) + LAYOUT_EXTENSION)
    }

    /** Сообщение холсту: граф модели. Тело графа - готовый JSON от сервера. */
    fun graphMessage(graphJson: String): String = """{"type":"graph","graph":$graphJson}"""

    /** Сообщение холсту: модель не разбирается, показать пустой лист. */
    fun noGraphMessage(): String = """{"type":"graph","graph":null}"""

    /** Сообщение холсту: содержимое файла раскладки. */
    fun layoutMessage(text: String): String = """{"type":"layout","text":${quote(text)}}"""

    /** Сообщение холсту: курсор в редакторе встал на строку и колонку. */
    fun cursorMessage(line: Int, character: Int): String =
        """{"type":"cursor","line":$line,"character":$character}"""

    /** Сообщение холсту: язык оболочки. */
    fun langMessage(lang: String): String = """{"type":"lang","lang":${quote(lang)}}"""

    /**
     * Строка в форме JSON.
     *
     * Своя, а не библиотечная: панель шлёт четыре сообщения, и зависимость ради
     * них дороже пятнадцати строк. Экранируются кавычка, косая черта, перевод
     * строки и управляющие знаки - раскладка приходит текстом файла.
     */
    fun quote(text: String): String {
        val out = StringBuilder(text.length + 2)
        out.append('"')
        for (ch in text) {
            when {
                ch == '"' -> out.append("\\\"")
                ch == '\\' -> out.append("\\\\")
                ch == '\n' -> out.append("\\n")
                ch == '\r' -> out.append("\\r")
                ch == '\t' -> out.append("\\t")
                ch < ' ' -> out.append("\\u%04x".format(ch.code))
                else -> out.append(ch)
            }
        }
        out.append('"')
        return out.toString()
    }

    /** Разобранное сообщение холста. */
    sealed interface Incoming {
        /** Холст готов принимать данные. */
        data object Ready : Incoming

        /** Раскладка изменилась: записать текст в файл рядом с моделью. */
        data class Layout(val text: String) : Incoming

        /** Выбран узел: поставить курсор в редакторе. */
        data class Select(val line: Int, val character: Int) : Incoming
    }

    /**
     * Разбирает сообщение холста.
     *
     * Разбор ручной по той же причине, что и сборка: сообщений три, формы их
     * известны, а неизвестное сообщение - не отказ, а `null`.
     */
    fun parse(message: String): Incoming? = when (field(message, "type")) {
        "ready" -> Incoming.Ready
        "layout" -> Incoming.Layout(field(message, "text") ?: "")
        "select" -> {
            val line = number(message, "line")
            val character = number(message, "character")
            if (line == null || character == null) null else Incoming.Select(line, character)
        }
        else -> null
    }

    /** Строковое поле объекта JSON; `null` - поля нет. */
    fun field(message: String, name: String): String? {
        val at = message.indexOf("\"$name\"")
        if (at < 0) return null
        var i = message.indexOf(':', at)
        if (i < 0) return null
        i++
        while (i < message.length && message[i].isWhitespace()) i++
        if (i >= message.length || message[i] != '"') return null
        i++
        val out = StringBuilder()
        while (i < message.length) {
            val ch = message[i]
            if (ch == '"') return out.toString()
            if (ch == '\\' && i + 1 < message.length) {
                i++
                when (val esc = message[i]) {
                    'n' -> out.append('\n')
                    'r' -> out.append('\r')
                    't' -> out.append('\t')
                    'u' -> {
                        val code = message.substring(i + 1, i + 5).toInt(16)
                        out.append(code.toChar())
                        i += 4
                    }
                    else -> out.append(esc)
                }
            } else {
                out.append(ch)
            }
            i++
        }
        return null
    }

    /** Числовое поле объекта JSON; `null` - поля нет или оно не целое. */
    fun number(message: String, name: String): Int? {
        val at = message.indexOf("\"$name\"")
        if (at < 0) return null
        var i = message.indexOf(':', at)
        if (i < 0) return null
        i++
        while (i < message.length && message[i].isWhitespace()) i++
        val start = i
        while (i < message.length && (message[i].isDigit() || (i == start && message[i] == '-'))) i++
        return message.substring(start, i).toIntOrNull()
    }
}

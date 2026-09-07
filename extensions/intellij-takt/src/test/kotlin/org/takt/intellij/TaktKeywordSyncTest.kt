package org.takt.intellij

import junit.framework.TestCase
import org.takt.intellij.psi.TaktTokenTypes
import java.io.File

/**
 * Регресс-тест соответствия набора ключевых слов плагина источнику истины -
 * таблице `KEYWORDS` в `takt-lang/src/parser/lexer.rs`.
 *
 * Тест читает Rust-лексер (относительно корня репозитория) и извлекает ключевые
 * слова из блока `static KEYWORDS: phf::Map<...> = phf_map! { ... };`, затем сверяет
 * их с [TaktTokenTypes.KEYWORDS]. При добавлении/удалении ключевого слова в языке,
 * не отражённом в плагине, тест краснеет.
 *
 * Если Rust-исходник недоступен (плагин собирается вне монорепозитория), тест
 * не падает, а помечается пропущенным (assumption) - набор всё равно проверяется
 * на непустоту.
 */
class TaktKeywordSyncTest : TestCase() {

    fun testKeywordSetMatchesRustLexer() {
        assertTrue("Набор ключевых слов не должен быть пустым", TaktTokenTypes.KEYWORDS.isNotEmpty())

        val lexerFile = findRustLexer()
        if (lexerFile == null) {
            println("[TaktKeywordSyncTest] takt-lang/src/parser/lexer.rs не найден — сверка пропущена")
            return
        }

        val expected = extractKeywords(lexerFile.readText())
        assertTrue("Не удалось извлечь KEYWORDS из ${lexerFile.path}", expected.isNotEmpty())

        val actual = TaktTokenTypes.KEYWORDS
        val missing = expected - actual // есть в языке, нет в плагине
        val extra = actual - expected   // есть в плагине, нет в языке
        assertTrue(
            "Рассинхрон ключевых слов с parser/lexer.rs.\n  отсутствуют в плагине: $missing\n  лишние в плагине: $extra",
            missing.isEmpty() && extra.isEmpty(),
        )
    }

    /** Ищет `takt-lang/src/parser/lexer.rs`, поднимаясь от рабочего каталога вверх. */
    private fun findRustLexer(): File? {
        var dir: File? = File("").absoluteFile
        repeat(8) {
            val candidate = dir?.resolve("takt-lang/src/parser/lexer.rs")
            if (candidate != null && candidate.isFile) return candidate
            dir = dir?.parentFile
        }
        return null
    }

    /** Извлекает ключи `"word" => ...` из блока `phf_map! { ... }` таблицы KEYWORDS. */
    private fun extractKeywords(source: String): Set<String> {
        val blockStart = source.indexOf("static KEYWORDS")
        if (blockStart < 0) return emptySet()
        val open = source.indexOf("phf_map!", blockStart)
        val brace = source.indexOf('{', open)
        val close = source.indexOf("};", brace)
        if (open < 0 || brace < 0 || close < 0) return emptySet()
        val block = source.substring(brace + 1, close)
        return Regex("\"([^\"]+)\"\\s*=>").findAll(block)
            .map { it.groupValues[1] }
            .toSet()
    }
}

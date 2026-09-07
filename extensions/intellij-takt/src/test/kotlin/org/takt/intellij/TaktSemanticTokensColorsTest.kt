package org.takt.intellij

import junit.framework.TestCase
import org.takt.intellij.lsp.TaktSemanticTokensColorsProvider
import java.io.File

/**
 * Полнота отображения типов семантических токенов в цвета.
 *
 *
 * Каждый из 10 типов легенды получает `TextAttributesKey` (нет `null`).
 * Набор отображения **синхронизирован** с источником истины
 * `takt-lang/src/lsp/keywords.rs` (`SEMANTIC_TOKEN_TYPES`): тест читает Rust-исходник,
 * извлекает имена типов и краснеет, если в легенду добавили тип без маппинга (тот
 * молча потерял бы цвет). Приём - как у `TaktKeywordSyncTest`.
 */
class TaktSemanticTokensColorsTest : TestCase() {

    /** 10 типов легенды (имена LSP), захваченные из кода. */
    private val legend = listOf(
        "keyword", "variable", "function", "type", "enumMember",
        "string", "number", "comment", "operator", "class",
    )

    /** Каждый тип легенды сопоставлен ключу цвета (нет несопоставленных). */
    fun testEveryLegendTypeHasKey() {
        for (t in legend) {
            assertNotNull("тип токена '$t' обязан иметь ключ цвета", TaktSemanticTokensColorsProvider.keyFor(t))
        }
    }

    /** Неизвестный тип -> `null` (цвет не навязывается). */
    fun testUnknownTypeIsNull() {
        assertNull(TaktSemanticTokensColorsProvider.keyFor("namespace"))
        assertNull(TaktSemanticTokensColorsProvider.keyFor(""))
    }

    /**
     * Набор типов отображения совпадает с `SEMANTIC_TOKEN_TYPES` из Rust. Если
     * Rust-исходник недоступен (сборка вне монорепозитория) - сверка пропускается,
     * но локальный набор всё равно проверен выше.
     */
    fun testLegendMatchesRustSource() {
        val keywordsFile = findRustFile("takt-lang/src/lsp/keywords.rs")
        if (keywordsFile == null) {
            println("[TaktSemanticTokensColorsTest] takt-lang/src/lsp/keywords.rs не найден — сверка пропущена")
            return
        }
        val expected = extractLegend(keywordsFile.readText())
        assertTrue("не удалось извлечь SEMANTIC_TOKEN_TYPES из ${keywordsFile.path}", expected.isNotEmpty())

        // Множество совпадает с локальным.
        assertEquals("рассинхрон легенды с keywords.rs", expected.toSortedSet(), legend.toSortedSet())
        // И каждый тип из Rust сопоставлен ключу (тип без маппинга -> null -> красный).
        for (t in expected) {
            assertNotNull("тип '$t' из легенды Rust без маппинга цвета", TaktSemanticTokensColorsProvider.keyFor(t))
        }
    }

    /** Ищет файл относительно корня репозитория, поднимаясь от рабочего каталога. */
    private fun findRustFile(rel: String): File? {
        var dir: File? = File("").absoluteFile
        repeat(8) {
            val candidate = dir?.resolve(rel)
            if (candidate != null && candidate.isFile) return candidate
            dir = dir?.parentFile
        }
        return null
    }

    /**
     * Извлекает имена типов легенды из блока
     * `SEMANTIC_TOKEN_TYPES: &[SemanticTokenType] = &[ ... ];` (записи
     * `SemanticTokenType::ENUM_MEMBER`) и переводит их в имена LSP
     * (`ENUM_MEMBER` -> `enumMember`).
     */
    private fun extractLegend(source: String): Set<String> {
        val start = source.indexOf("SEMANTIC_TOKEN_TYPES")
        if (start < 0) return emptySet()
        val open = source.indexOf("= &[", start)
        val close = source.indexOf("];", open)
        if (open < 0 || close < 0) return emptySet()
        val block = source.substring(open, close)
        return Regex("SemanticTokenType::([A-Z_]+)")
            .findAll(block)
            .map { screamingSnakeToLowerCamel(it.groupValues[1]) }
            .toSet()
    }

    /** `ENUM_MEMBER` -> `enumMember`, `KEYWORD` -> `keyword`. */
    private fun screamingSnakeToLowerCamel(name: String): String {
        val parts = name.lowercase().split('_')
        return parts.first() + parts.drop(1).joinToString("") { it.replaceFirstChar(Char::uppercase) }
    }
}

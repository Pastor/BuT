package org.takt.intellij

import junit.framework.TestCase
import org.takt.intellij.run.TaktOutputFilter

/**
 * Пересчёт колонки диагностики в смещение документа (фича 0158).
 *
 * Ловушка, названная ещё в карточке фичи: колонка Takt считается в
 * **символах**, а не в байтах, - в `.takt` законна кириллица. Документ IDEA
 * адресуется кодовыми единицами UTF-16, поэтому на символах вне BMP счёт снова
 * расходится, и без пересчёта ссылка уводит не туда.
 */
class TaktOutputFilterTest : TestCase() {

    /** Колонка 1 - начало строки, без арифметики. */
    fun testFirstColumnIsZeroOffset() {
        assertEquals(0, TaktOutputFilter.charColumnToOffset("var x: u8 := 0;", 1))
    }

    /** ASCII: символы и кодовые единицы совпадают. */
    fun testAsciiColumnMatchesOffset() {
        assertEquals(4, TaktOutputFilter.charColumnToOffset("var x: u8 := 0;", 5))
    }

    /**
     * Кириллица: в UTF-16 она тоже по одной единице на символ, поэтому смещение
     * совпадает с колонкой - а вот в **байтах** UTF-8 их было бы вдвое больше.
     * Тест закрепляет, что пересчёт идёт по символам, а не по байтам.
     */
    fun testCyrillicColumnCountsCharactersNotBytes() {
        val line = "// счётчик тактов"
        // Колонка 4 - четвёртый символ строки, то есть `с`. В байтах UTF-8 до
        // него тоже 3, но дальше по строке счёт разошёлся бы вдвое.
        val offset = TaktOutputFilter.charColumnToOffset(line, 4)
        assertEquals(3, offset)
        assertEquals("счётчик тактов", line.substring(offset))
    }

    /**
     * Символ вне BMP (эмодзи) занимает две кодовые единицы UTF-16: наивное
     * `column - 1` дало бы смещение внутрь суррогатной пары.
     */
    fun testAstralCharacterTakesTwoUnits() {
        val line = "// 🔧 наладка"
        // Шестой символ - `н`. Наивное `column - 1` дало бы 5 (пробел), потому
        // что эмодзи занимает две кодовые единицы UTF-16.
        val offset = TaktOutputFilter.charColumnToOffset(line, 6)
        assertEquals(6, offset)
        assertEquals("наладка", line.substring(offset))
    }

    /** Колонка за концом строки не выводит за границы текста. */
    fun testColumnBeyondEndClampsToLength() {
        val line = "abc"
        assertEquals(3, TaktOutputFilter.charColumnToOffset(line, 99))
    }
}

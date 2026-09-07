package org.takt.intellij.highlight

import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.fileTypes.SyntaxHighlighter
import com.intellij.openapi.options.colors.AttributesDescriptor
import com.intellij.openapi.options.colors.ColorDescriptor
import com.intellij.openapi.options.colors.ColorSettingsPage
import org.takt.intellij.TaktIcons
import javax.swing.Icon

/**
 * Страница "Settings -> Editor -> Color Scheme -> Takt" (задача 0022-03, R4/A5).
 *
 * Даёт пользователю переопределять цвета по группам и показывает результат на
 * демонстрационном фрагменте Takt (пост-0021 синтаксис `:=`/`=`/`<=`).
 */
class TaktColorSettingsPage : ColorSettingsPage {

    override fun getDisplayName(): String = "Takt"

    override fun getIcon(): Icon = TaktIcons.FILE

    override fun getHighlighter(): SyntaxHighlighter = TaktSyntaxHighlighter()

    override fun getDemoText(): String = DEMO_TEXT

    override fun getAdditionalHighlightingTagToDescriptorMap(): Map<String, TextAttributesKey>? = null

    override fun getAttributeDescriptors(): Array<AttributesDescriptor> = DESCRIPTORS

    override fun getColorDescriptors(): Array<ColorDescriptor> = ColorDescriptor.EMPTY_ARRAY

    private companion object {
        val DESCRIPTORS = arrayOf(
            AttributesDescriptor("Ключевое слово", TaktHighlighterColors.KEYWORD),
            AttributesDescriptor("Идентификатор", TaktHighlighterColors.IDENTIFIER),
            AttributesDescriptor("Число", TaktHighlighterColors.NUMBER),
            AttributesDescriptor("Строка", TaktHighlighterColors.STRING),
            AttributesDescriptor("Оператор", TaktHighlighterColors.OPERATOR),
            AttributesDescriptor("Строчный комментарий", TaktHighlighterColors.LINE_COMMENT),
            AttributesDescriptor("Документационный комментарий", TaktHighlighterColors.DOC_COMMENT),
            AttributesDescriptor("Блочный комментарий", TaktHighlighterColors.BLOCK_COMMENT),
            AttributesDescriptor("Точка с запятой", TaktHighlighterColors.SEMICOLON),
            AttributesDescriptor("Запятая", TaktHighlighterColors.COMMA),
            AttributesDescriptor("Точка", TaktHighlighterColors.DOT),
            AttributesDescriptor("Круглые скобки", TaktHighlighterColors.PARENTHESES),
            AttributesDescriptor("Фигурные скобки", TaktHighlighterColors.BRACES),
            AttributesDescriptor("Квадратные скобки", TaktHighlighterColors.BRACKETS),
            AttributesDescriptor("Некорректный символ", TaktHighlighterColors.BAD_CHARACTER),
            // Семантические ключи (фича 0038; выведены сюда фиксом 0196-01).
            // Без них пользователь не может ни увидеть, ни настроить цвет
            // имени типа: ключ есть, цвет наследуется от платформенной
            // категории без собственного значения, а в панели его нет - то
            // есть подсветка типов существует только на бумаге.
            AttributesDescriptor("Имя типа", TaktHighlighterColors.TYPE),
            AttributesDescriptor("Имя функции", TaktHighlighterColors.FUNCTION),
            AttributesDescriptor("Имя модели или состояния", TaktHighlighterColors.CLASS),
            AttributesDescriptor("Вариант перечисления", TaktHighlighterColors.ENUM_MEMBER),
        )

        val DEMO_TEXT = """
            // Модель светофора (демо подсветки Takt)
            import "common.takt";

            /// Счётчик тактов автомата
            model Traffic {
                var counter: u8 := 0;
                const LIMIT := 10;

                start Red = Warmup + (Idle | Ready) {
                    next Green;
                    : counter <= LIMIT;
                }

                state Green {
                    ref Red: counter = LIMIT;   /* переход по достижению предела */
                }
            }
        """.trimIndent()
    }
}

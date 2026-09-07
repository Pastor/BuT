package org.takt.intellij.parser

import com.intellij.lang.ASTNode
import com.intellij.lang.PsiBuilder
import com.intellij.lang.PsiParser
import com.intellij.psi.tree.IElementType
import org.takt.intellij.navigation.TaktSymbolScanner
import org.takt.intellij.psi.TaktElementTypes
import org.takt.intellij.psi.TaktTokenTypes

/**
 * Почти плоский разборщик Takt.
 *
 * Все токены - листья под корнем, **кроме** одиночных токенов, несущих ссылки/имена:
 * - строка-путь `import` (предыдущий значимый токен - `import`/`from`) -> композит
 *   [TaktElementTypes.IMPORT_PATH] (файловая `PsiReference`);
 * - идентификатор-**декларация** -> [TaktElementTypes.NAME_DECL] (цель навигации; переименование - у сервера);
 * - идентификатор-**использование** -> [TaktElementTypes.NAME_REF] (носитель `PsiReference`).
 *
 * Декларации отличаются от использований **эвристикой `TaktSymbolScanner`** (единый
 * источник форм `kw <Id>`/`Import`/`enum`) - множество стартовых смещений деклараций
 * считается один раз по тексту файла. Больше ничего структурного не строится:
 * выражения/условия/типы остаются плоскими, грамматика не дублируется. Разбор всегда
 * успешен и не теряет текст (композит группирует те же листья).
 *
 * "Предыдущий значимый токен" - предыдущая итерация цикла: `PsiBuilder` пропускает
 * пробелы/комментарии, поэтому `builder.tokenType` отдаёт только значимые токены.
 */
class TaktParser : PsiParser {
    override fun parse(root: IElementType, builder: PsiBuilder): ASTNode {
        val declStarts: Set<Int> =
            TaktSymbolScanner.scan(builder.originalText).mapTo(HashSet()) { it.range.startOffset }
        val rootMarker = builder.mark()
        var prevType: IElementType? = null
        var prevText: String? = null
        while (!builder.eof()) {
            val type = builder.tokenType
            val text = builder.tokenText
            val offset = builder.currentOffset
            when {
                type == TaktTokenTypes.STRING && prevType == TaktTokenTypes.KEYWORD &&
                    (prevText == "import" || prevText == "from") ->
                    wrap(builder, TaktElementTypes.IMPORT_PATH)

                type == TaktTokenTypes.IDENTIFIER ->
                    wrap(builder, if (offset in declStarts) TaktElementTypes.NAME_DECL else TaktElementTypes.NAME_REF)

                else -> builder.advanceLexer()
            }
            prevType = type
            prevText = text
        }
        rootMarker.done(root)
        return builder.treeBuilt
    }

    /** Оборачивает текущий токен в композит `elementType` (один лист внутри). */
    private fun wrap(builder: PsiBuilder, elementType: IElementType) {
        val marker = builder.mark()
        builder.advanceLexer()
        marker.done(elementType)
    }
}

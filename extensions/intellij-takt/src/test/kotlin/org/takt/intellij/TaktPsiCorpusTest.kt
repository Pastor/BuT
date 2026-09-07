package org.takt.intellij

import com.intellij.psi.PsiFileFactory
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import com.intellij.psi.PsiFile
import java.io.File

/**
 * Сверка PSI с корпусом `.takt` - защита от расхождения.
 *
 * По образцу `takt-lang/tests/format_tests.rs` (форматтер по всему корпусу): парсер
 * прогоняется по **всем** `.takt` из `examples/` и `takt-lang/tests/data/` и обязан
 * (а) **round-trip байт-в-байт** - конкатенация текстов листьев PSI равна
 * исходнику (оборачивание одиночных токенов в композиты `IMPORT_PATH`/`NAME_DECL`/
 * `NAME_REF` не теряет и не переставляет текст); (б) **не давать `PsiErrorElement`**.
 *
 * Сверка **вердикта** с оракулом `taktc` для плоского дерева
 * **вырождена**: парсер тотальный - разбор всегда успешен, он принимает
 * любой поток токенов) и `PsiErrorElement` не порождает в принципе. Синтаксическую
 * валидность плагин не заявляет - это работа `taktc` и LSP. Поэтому реальный
 * контроль здесь - круговой рейс: он ловит потерю и перестановку текста при будущем
 * углублении PSI (новый узел без покрытия завалит этот тест).
 */
class TaktPsiCorpusTest : BasePlatformTestCase() {

    fun testRoundTripAndNoErrorsOverCorpus() {
        val root = repoRoot() ?: run {
            fail("корень репозитория не найден (нет examples/ и takt-lang/tests/data/)")
            return
        }
        val files = corpusFiles(root)
        assertTrue("корпус .takt неожиданно мал: ${files.size}", files.size >= 150)

        // Порог выше проходит и без корпуса матрицы - поэтому, когда каталог
        // порождён, спрашивается ещё и он: иначе прогон молча шёл бы
        // по прежней витрине, а сочетания матрицы остались бы непроверенными.
        val matrix = root.resolve("target/matrix-corpus")
        if (matrix.isDirectory) {
            val fromMatrix = files.count { it.startsWith(matrix) }
            assertTrue(
                "корпус матрицы порождён, но в сверку не попал: $fromMatrix файлов",
                fromMatrix >= 200,
            )
        }

        val factory = PsiFileFactory.getInstance(project)
        val roundTripFailures = ArrayList<String>()
        val errorFailures = ArrayList<String>()

        for (file in files) {
            val text = file.readText()
            val rel = file.relativeTo(root).path
            val psi: PsiFile = factory.createFileFromText(file.name, TaktFileType, text)

            // Текст корневого AST-узла = конкатенация всех листьев в порядке дерева
            // (авторитетный round-trip: потеря/перестановка токена изменила бы его).
            if (psi.node.text != text) roundTripFailures.add(rel)
            if (PsiTreeUtil.hasErrorElements(psi)) errorFailures.add(rel)
        }

        assertTrue("round-trip PSI разошёлся с исходником: $roundTripFailures", roundTripFailures.isEmpty())
        assertTrue("неожиданные PsiErrorElement (парсер тотальный): $errorFailures", errorFailures.isEmpty())
    }

    /** Корень репозитория: подъём от рабочего каталога до `examples/` + `takt-lang/`. */
    private fun repoRoot(): File? {
        var dir: File? = File("").absoluteFile
        repeat(8) {
            if (dir != null && dir!!.resolve("examples").isDirectory &&
                dir!!.resolve("takt-lang/tests/data").isDirectory
            ) {
                return dir
            }
            dir = dir?.parentFile
        }
        return null
    }

    /**
     * Все `.takt` корпуса: `examples/`, `takt-lang/tests/data/` и - если он
     * порождён - корпус перебора.
     *
     * Матрица даёт сочетания, которых в витрине языка нет: порт
     * перечислимого типа с адресом из внешней карты, вложенная композиция с
     * параметром, транзитивный импорт. Порождается он тестом
     * `matrix_corpus_export_tests` крейта `takt-lang` (`cargo test`), поэтому
     * каталог может отсутствовать - тогда сверка идёт по прежнему корпусу, и
     * это не ошибка: плагин собирается отдельно от Rust-дерева.
     */
    private fun corpusFiles(root: File): List<File> {
        val matrix = root.resolve("target/matrix-corpus")
        val roots = listOfNotNull(
            root.resolve("examples"),
            root.resolve("takt-lang/tests/data"),
            matrix.takeIf { it.isDirectory },
        )
        return roots.flatMap { base ->
            base.walkTopDown().filter { it.isFile && it.extension == "takt" }.toList()
        }.sorted()
    }
}

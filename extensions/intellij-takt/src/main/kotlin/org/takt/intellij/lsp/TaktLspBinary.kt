package org.takt.intellij.lsp

import java.io.File

/**
 * Резолвинг пути к исполняемому `takt-lsp` (фича 0038, задача 0038-01).
 *
 * Чистая, тестируемая **без GUI** логика (драйвер 5 ADR: центр тяжести проверок -
 * вне редактора). Приоритет источников:
 *  1. явная настройка плагина ([TaktLspSettings.serverPath]);
 *  2. автопоиск исполняемого `takt-lsp` в `PATH`;
 *  3. не найден / не исполняемый -> `null` (семантический слой не включается,
 *     подсветка 0022 остаётся - тихая деградация, R3).
 *
 * Бинарник собирается только с флагом: `cargo build --features lsp --bin takt-lsp`.
 */
object TaktLspBinary {

    /** Имя исполняемого файла сервера. */
    const val EXECUTABLE: String = "takt-lsp"

    /**
     * Разрешает путь к `takt-lsp`: явная настройка -> `PATH` -> `null`.
     *
     * @param configuredPath значение настройки плагина (может быть пустым/`null`)
     * @param pathEnv содержимое переменной `PATH` (вынесено параметром ради теста)
     * @return исполняемый файл сервера либо `null`, если не найден
     */
    @JvmOverloads
    fun resolve(configuredPath: String?, pathEnv: String? = System.getenv("PATH")): File? {
        // 1. Явная настройка имеет приоритет над автопоиском.
        if (!configuredPath.isNullOrBlank()) {
            val file = File(configuredPath)
            return if (file.isFile && file.canExecute()) file else null
        }
        // 2. Автопоиск в каталогах PATH.
        return findOnPath(pathEnv)
    }

    /** Ищет исполняемый `takt-lsp` в каталогах `PATH`; `null`, если нет. */
    private fun findOnPath(pathEnv: String?): File? {
        if (pathEnv.isNullOrBlank()) return null
        for (dir in pathEnv.split(File.pathSeparatorChar)) {
            if (dir.isBlank()) continue
            val candidate = File(dir, EXECUTABLE)
            if (candidate.isFile && candidate.canExecute()) return candidate
        }
        return null
    }
}

package org.takt.intellij.lsp

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.components.State
import com.intellij.openapi.components.Storage

/**
 * Настройки инструментов Takt в плагине.
 *
 * Хранилище - [PersistentStateComponent] уровня приложения (пути к бинарникам
 * общие для всех проектов). Поля:
 *  - `serverPath` - путь к `takt-lsp` (пусто - автопоиск в `PATH`,
 *    [TaktLspBinary.resolve]);
 *  - `compilerPath`/`simulatorPath` - пути к компилятору `taktc`/симулятору
 *    `takt-sim` (**хранятся как задел** под действия компиляции и симуляции -
 *    их исполняет отдельная работа);
 *  - `includeDirs` - каталоги импортов (`-I`); **прокидываются в LSP-сервер** как
 *    `initializationOptions.searchPaths` - см. [TaktInitOptions];
 *  - `compilerArgs` - дополнительные параметры компилятора (свободные флаги,
 *    задел под действия);
 *  - `outputDir` - выходная директория генерации (задел под действия).
 *
 * Логика сборки `searchPaths` из [includeDirs] вынесена в [TaktInitOptions] -
 * чистая, тестируемая без GUI.
 */
@State(name = "TaktLspSettings", storages = [Storage("takt.xml")])
class TaktLspSettings : PersistentStateComponent<TaktLspSettings.State> {

    /** Сериализуемое состояние настроек. */
    class State {
        /** Явный путь к `takt-lsp` (пусто ⇒ автопоиск в `PATH`). */
        @JvmField
        var serverPath: String = ""

        /** Путь к компилятору `taktc` (задел под действия компиляции). */
        @JvmField
        var compilerPath: String = ""

        /** Путь к симулятору `takt-sim` (задел под действия симуляции). */
        @JvmField
        var simulatorPath: String = ""

        /**
         * Каталоги импортов (`-I`). Прокидываются в LSP как `searchPaths`.
         * `MutableList` сериализуется `XmlSerializer` как список строк.
         */
        @JvmField
        var includeDirs: MutableList<String> = mutableListOf()

        /** Дополнительные параметры компилятора (свободные флаги; задел). */
        @JvmField
        var compilerArgs: String = ""

        /** Выходная директория генерации (задел под действия). */
        @JvmField
        var outputDir: String = ""
    }

    private var state = State()

    override fun getState(): State = state

    override fun loadState(state: State) {
        this.state = state
    }

    /** Явный путь к серверу (пусто ⇒ автопоиск). */
    var serverPath: String
        get() = state.serverPath
        set(value) {
            state.serverPath = value
        }

    /** Путь к компилятору `taktc` (задел). */
    var compilerPath: String
        get() = state.compilerPath
        set(value) {
            state.compilerPath = value
        }

    /** Путь к симулятору `takt-sim` (задел). */
    var simulatorPath: String
        get() = state.simulatorPath
        set(value) {
            state.simulatorPath = value
        }

    /** Каталоги импортов (`-I`) -> `searchPaths` LSP. */
    var includeDirs: MutableList<String>
        get() = state.includeDirs
        set(value) {
            state.includeDirs = value
        }

    /** Дополнительные параметры компилятора (задел). */
    var compilerArgs: String
        get() = state.compilerArgs
        set(value) {
            state.compilerArgs = value
        }

    /** Выходная директория генерации (задел). */
    var outputDir: String
        get() = state.outputDir
        set(value) {
            state.outputDir = value
        }

    companion object {
        /** Экземпляр сервиса настроек уровня приложения. */
        fun getInstance(): TaktLspSettings =
            ApplicationManager.getApplication().getService(TaktLspSettings::class.java)
    }
}

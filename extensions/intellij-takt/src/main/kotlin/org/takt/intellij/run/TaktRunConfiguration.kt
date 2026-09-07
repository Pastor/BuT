package org.takt.intellij.run

import com.intellij.execution.Executor
import com.intellij.execution.configurations.CommandLineState
import com.intellij.execution.configurations.ConfigurationFactory
import com.intellij.execution.configurations.ConfigurationType
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.configurations.RunConfiguration
import com.intellij.execution.configurations.RunConfigurationBase
import com.intellij.execution.configurations.RunProfileState
import com.intellij.execution.configurations.RuntimeConfigurationError
import com.intellij.execution.process.KillableColoredProcessHandler
import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.process.ProcessTerminatedListener
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.NlsActions
import org.takt.intellij.TaktIcons
import org.takt.intellij.lsp.TaktLspSettings
import javax.swing.Icon

/**
 * Конфигурация запуска инструментов Takt.
 *
 * Один тип, две фабрики - **Compile** (`taktc compile`) и **Simulate**
 * (`takt-sim`): инструменты братья и делят половину полей (файл, `-I`, выходной
 * каталог), поэтому два независимых типа означали бы описывать общее дважды
 * плагина.
 *
 * Вся логика сборки команды живёт в [TaktCommandLine] - чистой функции без
 * GUI: плагин вне `precheck.sh`, и проверяемо здесь ровно то, что не требует
 * окна IDE.
 */
class TaktRunConfiguration(
    project: Project,
    factory: ConfigurationFactory,
    name: String,
    private val mode: TaktCommandLine.Mode,
) : RunConfigurationBase<TaktRunConfiguration.Options>(project, factory, name) {

    /** Сохраняемые параметры запуска. */
    class Options {
        @JvmField var filePath: String = ""
        @JvmField var target: String = ""
        @JvmField var outputDir: String = ""
        @JvmField var scenario: String = ""
        @JvmField var steps: String = ""
        @JvmField var extraArgs: String = ""
    }

    private val options = Options()

    /** Параметры запуска в виде, понятном [TaktCommandLine]. */
    fun params(): TaktCommandLine.Params = TaktCommandLine.Params(
        filePath = options.filePath,
        target = options.target,
        outputDir = options.outputDir,
        scenario = options.scenario,
        steps = options.steps,
        extraArgs = options.extraArgs,
    )

    /** Пути к инструментам и каталоги импортов - из общих настроек плагина. */
    private fun tools(): TaktCommandLine.Tools {
        val settings = TaktLspSettings.getInstance()
        return TaktCommandLine.Tools(
            compilerPath = settings.compilerPath,
            simulatorPath = settings.simulatorPath,
            includeDirs = settings.includeDirs,
        )
    }

    fun mutableOptions(): Options = options

    fun runMode(): TaktCommandLine.Mode = mode

    override fun getConfigurationEditor(): SettingsEditor<out RunConfiguration> =
        TaktRunConfigurationEditor(mode)

    /**
     * Отказ виден **до** запуска: IDEA покажет его в диалоге конфигурации, а не
     * исключением в логе (драйвер 4 ADR).
     */
    override fun checkConfiguration() {
        when (val result = TaktCommandLine.build(mode, params(), tools())) {
            is TaktCommandLine.Result.Refused -> throw RuntimeConfigurationError(result.message)
            is TaktCommandLine.Result.Ready -> {}
        }
    }

    override fun getState(executor: Executor, environment: ExecutionEnvironment): RunProfileState? {
        val built = TaktCommandLine.build(mode, params(), tools())
        val command = when (built) {
            is TaktCommandLine.Result.Ready -> built.command
            is TaktCommandLine.Result.Refused -> throw RuntimeConfigurationError(built.message)
        }
        return object : CommandLineState(environment) {
            override fun startProcess(): ProcessHandler {
                val cmd = GeneralCommandLine(command)
                    .withWorkDirectory(project.basePath)
                val handler = KillableColoredProcessHandler(cmd)
                ProcessTerminatedListener.attach(handler)
                return handler
            }
        }.apply {
            // Позиция в выводе становится ссылкой на место в файле.
            consoleBuilder.addFilter(TaktOutputFilter(project))
        }
    }
}

/** Тип конфигураций Takt: одна группа, две фабрики. */
class TaktRunConfigurationType : ConfigurationType {
    override fun getDisplayName(): String = "Takt"
    override fun getConfigurationTypeDescription(): String =
        "Запуск компилятора и симулятора Takt"
    override fun getIcon(): Icon = TaktIcons.FILE
    override fun getId(): String = "TaktRunConfiguration"
    override fun getConfigurationFactories(): Array<ConfigurationFactory> =
        arrayOf(CompileFactory(this), SimulateFactory(this))
}

/** Фабрика "Compile" - `taktc compile`. */
class CompileFactory(type: ConfigurationType) : ConfigurationFactory(type) {
    override fun getId(): String = "Compile"
    override fun getName(): @NlsActions.ActionText String = "Compile"
    override fun createTemplateConfiguration(project: Project): RunConfiguration =
        TaktRunConfiguration(project, this, "Takt Compile", TaktCommandLine.Mode.COMPILE)
}

/** Фабрика "Simulate" - `takt-sim`. */
class SimulateFactory(type: ConfigurationType) : ConfigurationFactory(type) {
    override fun getId(): String = "Simulate"
    override fun getName(): @NlsActions.ActionText String = "Simulate"
    override fun createTemplateConfiguration(project: Project): RunConfiguration =
        TaktRunConfiguration(project, this, "Takt Simulate", TaktCommandLine.Mode.SIMULATE)
}

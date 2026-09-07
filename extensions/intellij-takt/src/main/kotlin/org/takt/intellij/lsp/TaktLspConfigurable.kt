package org.takt.intellij.lsp

import com.intellij.openapi.options.Configurable
import com.intellij.openapi.ui.TextFieldWithBrowseButton
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import java.awt.Insets
import javax.swing.JComponent
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JTextArea
import javax.swing.JTextField

/**
 * Страница настроек инструментов Takt (фичи 0038, 0125).
 *
 * Settings -> Tools -> **Takt Language Server**. Поля:
 *  - путь к `takt-lsp` (0038; пусто ⇒ автопоиск в `PATH`, [TaktLspBinary.resolve]);
 *  - путь к компилятору `taktc` и симулятору `takt-sim` (0125; задел под действия);
 *  - каталоги импортов (`-I`, по одному на строку) - прокидываются в LSP как
 *    `searchPaths` ([TaktInitOptions], 0072);
 *  - дополнительные параметры компилятора (свободные флаги; задел);
 *  - выходная директория генерации (задел).
 *
 * Ручной Swing на `GridBagLayout` (без UI-DSL) - минимальная зависимость от
 * версии платформенного API (диапазон совместимости открыт, `untilBuild` пуст).
 */
class TaktLspConfigurable : Configurable {

    private var serverPathField: TextFieldWithBrowseButton? = null
    private var compilerPathField: TextFieldWithBrowseButton? = null
    private var simulatorPathField: TextFieldWithBrowseButton? = null
    private var includeDirsArea: JTextArea? = null
    private var compilerArgsField: JTextField? = null
    private var outputDirField: TextFieldWithBrowseButton? = null

    override fun getDisplayName(): String = "Takt Language Server"

    override fun createComponent(): JComponent {
        val panel = JPanel(GridBagLayout())
        var row = 0

        serverPathField = addPathRow(panel, row++, "Путь к takt-lsp (пусто → поиск в PATH): ")
        compilerPathField = addPathRow(panel, row++, "Путь к компилятору taktc: ")
        simulatorPathField = addPathRow(panel, row++, "Путь к симулятору takt-sim: ")

        // Каталоги импортов (-I) - многострочное поле (по одному пути на строку).
        val area = JTextArea(4, 30)
        includeDirsArea = area
        addAreaRow(panel, row++, "Каталоги импортов (-I, по одному на строку): ", area)

        val argsField = JTextField(30)
        compilerArgsField = argsField
        addComponentRow(panel, row++, "Доп. параметры компилятора: ", argsField)

        outputDirField = addPathRow(panel, row++, "Выходная директория генерации: ")

        // Растягивающая "пружина" снизу - прижимает строки к верху.
        val filler = GridBagConstraints().apply {
            gridx = 0
            gridy = row
            weighty = 1.0
            fill = GridBagConstraints.VERTICAL
        }
        panel.add(JPanel(), filler)

        reset()
        return panel
    }

    /** Строка "метка + [TextFieldWithBrowseButton]". */
    private fun addPathRow(panel: JPanel, row: Int, label: String): TextFieldWithBrowseButton {
        val field = TextFieldWithBrowseButton()
        addComponentRow(panel, row, label, field)
        return field
    }

    /** Строка "метка + произвольный компонент" (поле растягивается по ширине). */
    private fun addComponentRow(panel: JPanel, row: Int, label: String, component: JComponent) {
        panel.add(JLabel(label), labelConstraints(row))
        panel.add(component, fieldConstraints(row))
    }

    /** Строка "метка сверху + прокручиваемая область" (для многострочного `-I`). */
    private fun addAreaRow(panel: JPanel, row: Int, label: String, area: JTextArea) {
        panel.add(JLabel(label), labelConstraints(row))
        val scroll = JScrollPane(area)
        panel.add(scroll, fieldConstraints(row))
    }

    private fun labelConstraints(row: Int): GridBagConstraints = GridBagConstraints().apply {
        gridx = 0
        gridy = row
        anchor = GridBagConstraints.NORTHWEST
        insets = Insets(2, 2, 2, 8)
    }

    private fun fieldConstraints(row: Int): GridBagConstraints = GridBagConstraints().apply {
        gridx = 1
        gridy = row
        weightx = 1.0
        fill = GridBagConstraints.HORIZONTAL
        insets = Insets(2, 0, 2, 2)
    }

    override fun isModified(): Boolean {
        val s = TaktLspSettings.getInstance()
        return text(serverPathField) != s.serverPath ||
            text(compilerPathField) != s.compilerPath ||
            text(simulatorPathField) != s.simulatorPath ||
            TaktInitOptions.parseDirs(includeDirsArea?.text.orEmpty()) != s.includeDirs ||
            (compilerArgsField?.text ?: "") != s.compilerArgs ||
            text(outputDirField) != s.outputDir
    }

    override fun apply() {
        val s = TaktLspSettings.getInstance()
        s.serverPath = text(serverPathField)
        s.compilerPath = text(compilerPathField)
        s.simulatorPath = text(simulatorPathField)
        s.includeDirs = TaktInitOptions.parseDirs(includeDirsArea?.text.orEmpty()).toMutableList()
        s.compilerArgs = compilerArgsField?.text?.trim().orEmpty()
        s.outputDir = text(outputDirField)
    }

    override fun reset() {
        val s = TaktLspSettings.getInstance()
        serverPathField?.text = s.serverPath
        compilerPathField?.text = s.compilerPath
        simulatorPathField?.text = s.simulatorPath
        includeDirsArea?.text = TaktInitOptions.joinDirs(s.includeDirs)
        compilerArgsField?.text = s.compilerArgs
        outputDirField?.text = s.outputDir
    }

    override fun disposeUIResources() {
        serverPathField = null
        compilerPathField = null
        simulatorPathField = null
        includeDirsArea = null
        compilerArgsField = null
        outputDirField = null
    }

    /** Тримленное значение поля-пути (пусто, если поле не создано). */
    private fun text(field: TextFieldWithBrowseButton?): String = field?.text?.trim().orEmpty()
}

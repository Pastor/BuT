package org.takt.intellij.run

import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.ui.TextFieldWithBrowseButton
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import java.awt.Insets
import javax.swing.JComponent
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.JTextField

/**
 * Редактор конфигурации запуска (фича 0158).
 *
 * Поля различаются по режиму: цель генерации осмысленна только для компиляции,
 * сценарий и число шагов - только для симуляции. Общее (файл, выходной каталог,
 * свободные флаги) показывается всегда.
 *
 * Здесь только раскладка и перенос значений. Ни одной проверки и ни одного
 * решения о флагах: они в [TaktCommandLine], который тестируется без GUI.
 */
class TaktRunConfigurationEditor(
    private val mode: TaktCommandLine.Mode,
) : SettingsEditor<TaktRunConfiguration>() {

    private val fileField = TextFieldWithBrowseButton()
    private val targetField = JTextField()
    private val outputField = TextFieldWithBrowseButton()
    private val scenarioField = TextFieldWithBrowseButton()
    private val stepsField = JTextField()
    private val extraField = JTextField()

    override fun createEditor(): JComponent {
        val panel = JPanel(GridBagLayout())
        var row = 0
        fileField.addBrowseFolderListener(
            "Файл Takt", "Модель для запуска", null,
            FileChooserDescriptorFactory.createSingleFileDescriptor("takt"),
        )
        addRow(panel, row++, "Файл .takt:", fileField)
        if (mode == TaktCommandLine.Mode.COMPILE) {
            addRow(panel, row++, "Цель (-t):", targetField)
        } else {
            scenarioField.addBrowseFolderListener(
                "Сценарий", "JSON-файл сценария симуляции", null,
                FileChooserDescriptorFactory.createSingleFileDescriptor("json"),
            )
            addRow(panel, row++, "Сценарий (-s):", scenarioField)
            addRow(panel, row++, "Шагов (-n):", stepsField)
        }
        outputField.addBrowseFolderListener(
            "Выходной каталог", "Куда класть результат", null,
            FileChooserDescriptorFactory.createSingleFolderDescriptor(),
        )
        addRow(panel, row++, "Выход (-o):", outputField)
        addRow(panel, row, "Доп. флаги:", extraField)
        return panel
    }

    override fun resetEditorFrom(configuration: TaktRunConfiguration) {
        val options = configuration.mutableOptions()
        fileField.text = options.filePath
        targetField.text = options.target
        outputField.text = options.outputDir
        scenarioField.text = options.scenario
        stepsField.text = options.steps
        extraField.text = options.extraArgs
    }

    override fun applyEditorTo(configuration: TaktRunConfiguration) {
        val options = configuration.mutableOptions()
        options.filePath = fileField.text
        options.target = targetField.text
        options.outputDir = outputField.text
        options.scenario = scenarioField.text
        options.steps = stepsField.text
        options.extraArgs = extraField.text
    }

    private fun addRow(panel: JPanel, row: Int, label: String, field: JComponent) {
        val labelConstraints = GridBagConstraints().apply {
            gridx = 0
            gridy = row
            anchor = GridBagConstraints.WEST
            insets = Insets(4, 4, 4, 8)
        }
        val fieldConstraints = GridBagConstraints().apply {
            gridx = 1
            gridy = row
            weightx = 1.0
            fill = GridBagConstraints.HORIZONTAL
            insets = Insets(4, 0, 4, 4)
        }
        panel.add(JLabel(label), labelConstraints)
        panel.add(field, fieldConstraints)
    }
}

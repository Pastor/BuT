package org.takt.intellij

import com.intellij.openapi.fileTypes.LanguageFileType
import javax.swing.Icon

/**
 * Тип файла Takt - связывает расширение `.takt` с [TaktLanguage].
 *
 * Регистрируется в `plugin.xml` (`com.intellij.fileType`), после чего IDE
 * распознаёт `*.takt` как язык Takt (критерий приёмки A1, требование R1).
 */
object TaktFileType : LanguageFileType(TaktLanguage) {
    override fun getName(): String = "Takt"

    override fun getDescription(): String = "Takt FSM specification"

    override fun getDefaultExtension(): String = "takt"

    override fun getIcon(): Icon = TaktIcons.FILE
}

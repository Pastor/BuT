package org.takt.intellij.parser

import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.lang.ASTNode
import com.intellij.lang.ParserDefinition
import com.intellij.lang.PsiParser
import com.intellij.lexer.Lexer
import com.intellij.openapi.project.Project
import com.intellij.psi.FileViewProvider
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.tree.IFileElementType
import com.intellij.psi.tree.TokenSet
import org.takt.intellij.TaktLanguage
import org.takt.intellij.lexer.TaktLexer
import org.takt.intellij.psi.TaktElementTypes
import org.takt.intellij.psi.TaktFile
import org.takt.intellij.psi.TaktImportPath
import org.takt.intellij.psi.TaktNameDecl
import org.takt.intellij.psi.TaktNameRef
import org.takt.intellij.psi.TaktTokenSets

/**
 * Определение разбора файлов Takt (фича 0023, задача 0023-01).
 *
 * Разбор **плоский** ([TaktParser]): токены [TaktLexer] складываются листьями под
 * единственным корневым узлом [FILE]. Полноценного PSI-дерева нет (ADR 0023,
 * Option A) - цель лишь дать платформе реальные `PsiElement` под кареткой, чтобы
 * заработали `GotoDeclarationHandler` и `PsiReferenceContributor`. Подсветка
 * (0022) от этого не зависит и продолжает работать через `SyntaxHighlighter`.
 */
class TaktParserDefinition : ParserDefinition {
    override fun createLexer(project: Project?): Lexer = TaktLexer()

    override fun createParser(project: Project?): PsiParser = TaktParser()

    override fun getFileNodeType(): IFileElementType = FILE

    override fun getCommentTokens(): TokenSet = TaktTokenSets.COMMENTS

    override fun getStringLiteralElements(): TokenSet = TaktTokenSets.STRINGS

    override fun getWhitespaceTokens(): TokenSet = TaktTokenSets.WHITESPACES

    // Композит IMPORT_PATH (0067) -> узел-носитель файловой ссылки; прочие
    // (если появятся) - безопасная обёртка. FILE обрабатывается createFile.
    override fun createElement(node: ASTNode): PsiElement = when (node.elementType) {
        TaktElementTypes.IMPORT_PATH -> TaktImportPath(node)
        TaktElementTypes.NAME_DECL -> TaktNameDecl(node)
        TaktElementTypes.NAME_REF -> TaktNameRef(node)
        else -> ASTWrapperPsiElement(node)
    }

    override fun createFile(viewProvider: FileViewProvider): PsiFile = TaktFile(viewProvider)

    companion object {
        val FILE: IFileElementType = IFileElementType(TaktLanguage)
    }
}

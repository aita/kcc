package tools.aita.kcc.parsing

import tools.aita.kcc.lexer.Lexer
import tools.aita.kcc.lexer.TokenKind
import java.io.StringReader

data class SourceFile(val name: String, val source: String)


class Scanner(val file: SourceFile) {
    private val lexer = Lexer(StringReader(file.source))

    fun get(): Token {
        val pos = lexer.position()
        val loc = Location(file.name, pos.line, pos.column, pos.offset)
        val kind = lexer.yylex()
        return kind?.let { Token(kind, lexer.yytext(), loc) }
            ?: Token(TokenKind.EOF,"", loc)
    }
}
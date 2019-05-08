package tools.aita.kcc.parsing

import tools.aita.kcc.lexer.TokenKind
import tools.aita.kcc.lexer.TokenKind.*

val typeQualifierKeywords = listOf(CONST, RESTRICT, VOLATILE)
val storageClassSpecifierKeywords = listOf(TYPEDEF, EXTERN, STATIC, AUTO, REGISTER)
val functionSpecifierKeywords = listOf(INLINE)
val typeSpecifierKeywords = listOf(
    VOID,
    CHAR,
    SHORT,
    INT,
    LONG,
    FLOAT,
    DOUBLE,
    SIGNED,
    UNSIGNED,
    BOOL,
    COMPLEX,
    STRUCT,
    UNION
)

data class Location(val filename: String, val line: Int, val column: Int, val offset: Int) {
    override fun toString(): String {
        return "$filename:$line:$column"
    }
}

data class Token(val kind: TokenKind, val text: String, val location: Location) {
    override fun toString(): String {
        return text
    }
}

fun tokenName(kind: TokenKind): String =
    when(kind) {
        EOF -> "eof"
        IDENTIFIER -> "identifier"
        INTEGER_LITERAL -> "integer literal"
        FLOATING_LITERAL -> "floating literal"
        STRING_LITERAL -> "string literal"
        CHARACTER_LITERAL -> "character literal"
        AUTO -> "auto"
        BREAK -> "break"
        CASE -> "case"
        CHAR -> "char"
        CONST -> "const"
        CONTINUE -> "continue"
        DEFAULT -> "default"
        DO -> "do"
        DOUBLE -> "double"
        ELSE -> "else"
        ENUM -> "enum"
        EXTERN -> "extern"
        FLOAT -> "float"
        FOR -> "for"
        GOTO -> "goto"
        IF -> "if"
        INLINE -> "inline"
        INT -> "int"
        LONG -> "long"
        REGISTER -> "register"
        RESTRICT -> "restrict"
        RETURN -> "return"
        SHORT -> "short"
        SIGNED -> "signed"
        SIZEOF -> "sizeof"
        STATIC -> "static"
        STRUCT -> "struct"
        SWITCH -> "switch"
        TYPEDEF -> "typedef"
        UNION -> "union"
        UNSIGNED -> "unsigned"
        VOID -> "void"
        VOLATILE -> "volatile"
        WHILE -> "while"
        BOOL -> "_Bool"
        COMPLEX -> "_Complex"
        IMAGINARY -> "_Imaginary"
        LBRACKET -> "["
        RBRACKET -> "]"
        LPAREN -> "("
        RPAREN -> ")"
        LBRACE -> "{"
        RBRACE -> "}"
        PERIOD -> "."
        ARROW -> "->"
        PLUSPLUS -> "++"
        MINUSMINUS -> "--"
        AMP -> "&"
        STAR -> "*"
        PLUS -> "+"
        MINUS -> "-"
        TILDE -> "~"
        EXCLAIM -> "!"
        SLASH -> "/"
        PERCENT -> "%"
        LESSLESS -> "<<"
        GREATERGREATER -> ">>"
        LESS -> "<"
        GREATER -> ">"
        LESSEQUAL -> "<="
        GREATEREQUAL -> ">="
        EQUALEQUAL -> "=="
        EXCLAIMEQUAL -> "!="
        CARET -> "^"
        PIPE -> "|"
        AMPAMP -> "&&"
        PIPEPIPE -> "||"
        CONDITIONAL -> "?"
        COLON -> ":"
        SEMICOLON -> ";"
        ELLIPSIS -> "..."
        EQUAL -> "="
        STAREQUAL -> "*="
        SLASHEQUAL -> "/="
        PERCENTEQUAL -> "%="
        PLUSEQUAL -> "+="
        MINUSEQUAL -> "-="
        LESSLESSEQUAL -> "<<="
        GREATERGREATEREQUAL -> ">>="
        AMPEQUAL -> "&="
        CARETEQUAL -> "^="
        PIPEEQUAL -> "|="
        COMMA -> ","
        SHARP -> "#"
        SHARPSHARP -> "##"
    }

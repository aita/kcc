package tools.aita.kcc.lexer;

public enum TokenKind {
    EOF,

    IDENTIFIER,
    INTEGER_LITERAL,
    FLOATING_LITERAL,
    STRING_LITERAL,
    CHARACTER_LITERAL,

    // 6.4.1 Keywords
    AUTO,
    BREAK,
    CASE,
    CHAR,
    CONST,
    CONTINUE,
    DEFAULT,
    DO,
    DOUBLE,
    ELSE,
    ENUM,
    EXTERN,
    FLOAT,
    FOR,
    GOTO,
    IF,
    INLINE,
    INT,
    LONG,
    REGISTER,
    RESTRICT,
    RETURN,
    SHORT,
    SIGNED,
    SIZEOF,
    STATIC,
    STRUCT,
    SWITCH,
    TYPEDEF,
    UNION,
    UNSIGNED,
    VOID,
    VOLATILE,
    WHILE,
    BOOL,
    COMPLEX,
    IMAGINARY,

    // 6.4.6 Punctuators
    LBRACKET, // [
    RBRACKET, // ]
    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }
    PERIOD, // .
    ARROW, // ->
    PLUSPLUS, // ++
    MINUSMINUS, // --
    AMP, // &
    STAR, // *
    PLUS, // +
    MINUS, // -
    TILDE, // ~
    EXCLAIM, // !
    SLASH, // /
    PERCENT, // %
    LESSLESS, // <<
    GREATERGREATER, // >>
    LESS, // <
    GREATER, // >
    LESSEQUAL, // <=
    GREATEREQUAL, // >=
    EQUALEQUAL, // ==
    EXCLAIMEQUAL, // !=
    CARET, // ^
    PIPE, // |
    AMPAMP, // &&
    PIPEPIPE, // ||
    CONDITIONAL, // ?
    COLON, // :
    SEMICOLON, // ;
    ELLIPSIS, // ...
    EQUAL, // =
    STAREQUAL, // *=
    SLASHEQUAL, // /=
    PERCENTEQUAL, // %=
    PLUSEQUAL, // +=
    MINUSEQUAL, // -=
    LESSLESSEQUAL, // <<=
    GREATERGREATEREQUAL, // >>=
    AMPEQUAL, // &=
    CARETEQUAL, // ^=
    PIPEEQUAL, // |=
    COMMA, // ,
    SHARP, // #
    SHARPSHARP, //##
}

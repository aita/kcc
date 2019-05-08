package tools.aita.kcc.lexer;

import static tools.aita.kcc.lexer.TokenKind.*;

%%

%public
%class Lexer

%char
%line
%column
%unicode

%type TokenKind

%{

public static final class Position {
    public final int line;
    public final int column;
    public final int offset;

    public Position(int line, int column, int offset) {
        this.line = line + 1;
        this.column = column;
        this.offset = offset;
    }
}

public Position position() {
    return new Position(yyline, yycolumn, yychar);
}

%}

DIGIT             = [0-9]
LETTER            = [a-zA-Z_]
HEXADECIAML_DIGIT = [a-fA-F0-9]
EXPONENT          = ([Ee][+-]?{DIGIT}+)
BINARY_EXPONENT   = ([Pp][+-]?{DIGIT}+)
FLOATING_SUFFIX   = (f|F|l|L)
INTEGER_SUFFIX    = ((u|U)|(u|U)?(l|L|ll|LL)|(l|L|ll|LL)(u|U))

%%

"auto"          { return AUTO; }
"_Bool"         { return BOOL; }
"break"         { return BREAK; }
"case"          { return CASE; }
"char"          { return CHAR; }
"_Complex"      { return COMPLEX; }
"const"         { return CONST; }
"continue"      { return CONTINUE; }
"default"       { return DEFAULT; }
"do"            { return DO; }
"double"        { return DOUBLE; }
"else"          { return ELSE; }
"enum"          { return ENUM; }
"extern"        { return EXTERN; }
"float"         { return FLOAT; }
"for"           { return FOR; }
"goto"          { return GOTO; }
"if"            { return IF; }
"_Imaginary"    { return IMAGINARY; }
"inline"        { return INLINE; }
"int"           { return INT; }
"long"          { return LONG; }
"register"      { return REGISTER; }
"restrict"      { return RESTRICT; }
"return"        { return RETURN; }
"short"         { return SHORT; }
"signed"        { return SIGNED; }
"sizeof"        { return SIZEOF; }
"static"        { return STATIC; }
"struct"        { return STRUCT; }
"switch"        { return SWITCH; }
"typedef"       { return TYPEDEF; }
"union"         { return UNION; }
"unsigned"      { return UNSIGNED; }
"void"          { return VOID; }
"volatile"      { return VOLATILE; }
"while"         { return WHILE; }


"["     { return LBRACKET; }
"]"     { return RBRACKET; }
"("     { return LPAREN; }
")"     { return RPAREN; }
"{"     { return LBRACE; }
"}"     { return RBRACE; }
"."     { return PERIOD; }
"->"    { return ARROW; }
"++"    { return PLUSPLUS; }
"--"    { return MINUSMINUS; }
"&"     { return AMP; }
"*"     { return STAR; }
"+"     { return PLUS; }
"-"     { return MINUS; }
"~"     { return TILDE; }
"!"     { return EXCLAIM; }
"/"     { return SLASH; }
"%"     { return PERCENT; }
"<<"    { return LESSLESS; }
">>"    { return GREATERGREATER; }
"<"     { return LESS; }
">"     { return GREATER; }
"<="    { return LESSEQUAL; }
">="    { return GREATEREQUAL; }
"=="    { return EQUALEQUAL; }
"!="    { return EXCLAIMEQUAL; }
"^"     { return CARET; }
"|"     { return PIPE; }
"&&"    { return AMPAMP; }
"||"    { return PIPEPIPE; }
"?"     { return CONDITIONAL; }
":"     { return COLON; }
";"     { return SEMICOLON; }
"..."   { return ELLIPSIS; }
"="     { return EQUAL; }
"*="    { return STAREQUAL; }
"/="    { return SLASHEQUAL; }
"%="    { return PERCENTEQUAL; }
"+="    { return PLUSEQUAL; }
"-="    { return MINUSEQUAL; }
"<<="   { return LESSLESSEQUAL; }
">>="   { return GREATERGREATEREQUAL; }
"&="    { return AMPEQUAL; }
"^="    { return CARETEQUAL; }
"|="    { return PIPEEQUAL; }
","     { return COMMA; }
"#"     { return SHARP; }
"##"    { return SHARPSHARP; }


{LETTER}({LETTER}|{DIGIT})*     { return IDENTIFIER; }

0[xX]{HEXADECIAML_DIGIT}+{INTEGER_SUFFIX}?          { return INTEGER_LITERAL; }
0[0-7]*{INTEGER_SUFFIX}?                            { return INTEGER_LITERAL; }
[1-9]{DIGIT}*{INTEGER_SUFFIX}?                      { return INTEGER_LITERAL; }
LETTER?'(\\.|[^\\'\n])+'                            { return CHARACTER_LITERAL; }

{DIGIT}+{EXPONENT}{FLOATING_SUFFIX}?                                                    { return FLOATING_LITERAL; }
{DIGIT}*"."{DIGIT}+{EXPONENT}?{FLOATING_SUFFIX}?                                        { return FLOATING_LITERAL;  }
{DIGIT}+"."{DIGIT}*{EXPONENT}?{FLOATING_SUFFIX}?                                        { return FLOATING_LITERAL;  }
0[xX]{HEXADECIAML_DIGIT}+{BINARY_EXPONENT}{FLOATING_SUFFIX}?                            { return FLOATING_LITERAL; }
0[xX]{HEXADECIAML_DIGIT}*"."{HEXADECIAML_DIGIT}+{BINARY_EXPONENT}?{FLOATING_SUFFIX}?    { return FLOATING_LITERAL; }
0[xX]{HEXADECIAML_DIGIT}+"."{HEXADECIAML_DIGIT}*{BINARY_EXPONENT}?{FLOATING_SUFFIX}?    { return FLOATING_LITERAL; }


[ \r\n\t\f]     { /* ignore */ }
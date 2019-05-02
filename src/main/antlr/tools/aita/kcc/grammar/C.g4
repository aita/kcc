grammar C;

@header{
package tools.aita.kcc.grammar;
}

program
    : translationUnit EOF
    ;

// 6.5.1
primaryExpression
    : Identifier                #Identifier
    | '(' e=expression ')'      #Grouping
    // 6.4.4 Consants
    | IntegerConstant           #Integer
    // | FloatingConstant
    // | EnumerationConstant
    // | CharacterConstant
    // | StringLiteral
    ;

// 6.5.2
postfixExpression
    : primaryExpression                                                         #PrimaryExpr
    // TODO: 6.5.2.1 Array subscripting
    // | postfixExpression '[' expression ']'
    | function=postfixExpression '(' args=argumentExpressionList? ')'           #FuncCall
    // TODO: 6.5.2.3 Strucutre and union members
    // | postfixExpression '.' Identifier
    // | postfixExpression '->' Identifier
    // TODO: 6.5.2.4 Postfix increment and decrement operators
    // | postfixExpression '++'
    // | postfixExpression '--'
    // TODO: 6.5.2.5 Compound literals
    // | '(' typeName ')' '{' initializerList '}'
    // | '(' typeNmae ')' '{' initializerList ',' '}'
    ;

argumentExpressionList
    : exprs+=assignmentExpression (',' exprs+=assignmentExpression)*
    ;

// 6.5.3
unaryExpression
    : postfixExpression                                             #PostfixExpr
    | op=(PlusPlus|MinusMinus) e=unaryExpression                    #Increment
    | op=(Amp|Star|Plus|Minus|Tilde|Exclaim) e=castExpression       #UnaryOp
    // TODO: 6.5.3.4 The sizeof operator
    // | Sizeof unaryExpression
    // | Sizeof '(' typeName ')'
;

// 6.5.4
castExpression
    : unaryExpression                           #UnaryExpr
    | '(' ty=typeName ')' e=castExpression      #Cast
    ;

// 6.5.5
multiplicativeExpression
    : castExpression                                                               #CastExpr
    | left=multiplicativeExpression op=(Star|Slash|Percent) right=castExpression   #MultOp
    ;

// 6.5.6
additiveExpression
    : multiplicativeExpression                                                      #MultExpr
    | left=additiveExpression op=(Plus|Minus) right=multiplicativeExpression        #AddOp
    ;

// 6.5.7
shiftExpression
    : additiveExpression                                                                #AddExpr
    | left=shiftExpression op=(LessLess|GreaterGreater) right=additiveExpression        #ShiftOp
    ;

// 6.5.8
relationalExpression
    : shiftExpression                                                                               #ShiftExpr
    | left=relationalExpression op=(Less|Greater|LessEqual|GreaterEqual) right=shiftExpression      #RelOp
    ;

// 6.5.9
equalityExpression
    : relationalExpression                                                                  #RelExpr
    | left=equalityExpression op=(EqualEqual|ExclaimEqual) right=relationalExpression       #EqualOp
    ;

// 6.5.10
andExpression
    : equalityExpression                                        #EqualExpr
    | left=andExpression op=Amp right=equalityExpression        #AndOp
    ;

// 6.5.11
exclusiveOrExpression
    : andExpression                                                 #AndExpr
    | left=exclusiveOrExpression op=Caret right=andExpression       #XorOp
    ;

// 6.5.12
inclusiveOrExpression
    : exclusiveOrExpression                                                 #XorExpr
    | left=inclusiveOrExpression op=Pipe right=exclusiveOrExpression        #OrOp
    ;

// 6.5.13
logicalAndExpression
    : inclusiveOrExpression                                                 #OrExpr
    | left=logicalAndExpression op=AmpAmp right=inclusiveOrExpression       #LogicalAndOp
    ;

// 6.5.14
logicalOrExpression
    : logicalAndExpression                                                  #LogicalAndExpr
    | left=logicalOrExpression op=PipePipe right=logicalAndExpression       #LogicalOrOp
    ;

// 6.5.15
conditionalExpression
    : logicalOrExpression                                                       #LogicalOrExpr
    | e1=logicalOrExpression '?' e2=expression ':' e3=conditionalExpression     #ConditionalOp
    ;

// 6.5.16
assignmentExpression
    : conditionalExpression                                                 #ConditionalExpr
    | unaryExpression
      (Equal|StarEqual|SlashEqual|PercentEqual|PlusEqual|MinusEqual
        |LessLessEqual|GreaterGreaterEqual|AmpEqual|CaretEqual|PipeEqual)
      assignmentExpression                                                  #AssignOp
    ;

// 6.5.17
expression
    : assignmentExpression                                  #AssignExpr
    | left=expression ',' right=assignmentExpression        #CommaOp
    ;

// 6.6
/*
constantExpression
    : conditionalExpression
    ;
*/

// 6.7
declaration
    // : declarationSpecifiers initDeclaratorList? ';'
    : typeSpecifier (declarators+=declarator (',' declarators+=declarator)*)? ';'
    ;

/*
declarationSpecifiers
    : parseTypeSpecifier declarationSpecifiers?
    | typeQualifier declarationSpecifiers?
    | storageClassSpecifier declarationSpecifiers?
    | functionSpecifier declarationSpecifiers?
    ;

initDeclaratorList
    : decls+=initDeclarator (',' decls+=initDeclarator)*
    ;

initDeclarator
    : decl=declarator ('=' init=initializer)?
    ;
*/

// 6.7.1
/*
storageClassSpecifier
    : Extern
    | Static
    | Typedef
    | Auto
    | Register
    ;
*/

// 6.7.2
/*
parseTypeSpecifier
    : Void
    | Char
    | Short
    | Int
    | Long
    | Float
    | Double
    | Signed
    | Unsigned
    | Bool
    | Complex
    | Imaginary
    | structOrUnionSpecifier
    | enumSpecifier
    | typedefName
    ;
*/
typeSpecifier
    : type=Int
    ;

// TODO: 6.7.2.1 Structure and union specifiers
/*
specifierQualifierList
    : parseTypeSpecifier specifierQualifierList?
    // | typeQualifier specifierQualifierList?
    ;
*/

// TODO: 6.7.2.2 Enumeration specifiers

// TODO: 6.7.2.3 Tags

// 6.7.3
/*
typeQualifier
    : Const
    | Restrict
    | Volatile
    ;
*/

// 6.7.4
/*
functionSpecifier
    : Inline
    ;
*/

// 6.7.5
declarator
    : pointer? directDeclarator
    ;

/*
directDeclarator
    : Identifier
    | '(' declarator ')'
    | directDeclarator '(' parameterTypeList ')'
    | directDeclarator '(' identifierList? ')'
    ;
*/
directDeclarator
    : name=Identifier
    | name=Identifier '(' parameterList? ')'
    ;

/*
pointer
    : '*' typeQualifierList?
    | '*' typeQualifierList? pointer?
    ;

typeQualifierList
    : typeQualifier+
    ;
*/
pointer
    : '*'
    ;

/*
parameterTypeList
    : parameterList
    | parameterList ',' '...'
    ;
*/

parameterList
    : params+=parameterDeclaration (',' params+=parameterDeclaration)*
    ;

/*
parameterDeclaration
    : declarationSpecifiers declarator
    | declarationSpecifiers abstractDeclarator?
    ;
*/
parameterDeclaration
    : typeSpecifier declarator
    ;

identifierList
    : ids+=Identifier (',' ids+=Identifier)
    ;

// 6.7.6
typeName
    // : specifierQualifierList abstractDeclarator?
    : typeSpecifier abstractDeclarator?
    ;

abstractDeclarator
    : pointer
    // | pointer? directAbstractDeclarator
    ;

/*
directAbstractDeclarator
    : '(' abstractDeclarator ')'
    | directAbstractDeclarator? '[' assignmentExpression? ']'
    | directAbstractDeclarator? '[' '*' ']'
    | directAbstractDeclarator? '(' parameterTypeList? ')'
    ;
*/

// TODO: 6.7.7 Type definitions

// 6.7.8
initializer
    : assignmentExpression
    // | '{' initializerList '}'
    // | '{' initializerList ',' '}'
    ;

// 6.8
statement
    : labeledStatement
    | expressionStatement
    | compoundStatement
    | selectionStatement
    | iterationStatement
    | jumpStatement
    ;

// 6.8.1
// TODO: case, default
labeledStatement
    : label=Identifier ':' stmt=statement      #labelStmt
    ;

// 6.8.2
compoundStatement
    : '{' items=blockItemList? '}'
    ;

blockItemList
    : items+=blockItem+
    ;

blockItem
    : declaration   #BlockDecl
    | statement     #BlockStmt
    ;

// 6.8.3
expressionStatement
    : e=expression? ';'
    ;

// 6.8.4
// TODO: switch
selectionStatement
    : If '(' e=expression ')' thenStmt=statement (Else elseStmt=statement)?     #ifStmt
    ;

// 6.8.5
// TODO: do-while, for
iterationStatement
    : While '(' e=expression ')' stmt=statement    #whileStmt
    ;

// 6.8.6.4
jumpStatement
    : Goto id=Identifier ';'    #gotoStmt
    | Continue ';'              #continueStmt
    | Break ';'                 #breakStmt
    | Return e=expression? ';'  #returnStmt
    ;

// 6.9
translationUnit
    : decls+=externalDeclaration*
    ;

externalDeclaration
    : functionDefinition
    | declaration
    ;

// 6.9.1
functionDefinition
    // : declarationSpecifiers? declarator declarationList? compoundStatement
    : typeSpecifier? declarator compoundStatement
    ;

declarationList
    : decls+=declaration+
    ;


// 6.4.1 Keywords
Auto: 'auto';
Break: 'break';
Case: 'case';
Char: 'char';
Const: 'const';
Continue: 'continue';
Default: 'default';
Do: 'do';
Double: 'double';
Else: 'else';
Enum: 'enum';
Extern: 'extern';
Float: 'float';
For: 'for';
Goto: 'goto';
If: 'if';
Inline: 'inline';
Int: 'int';
Long: 'long';
Register: 'register';
Restrict: 'restrict';
Return: 'return';
Short: 'short';
Signed: 'signed';
Sizeof: 'sizeof';
Static: 'static';
Struct: 'struct';
Switch: 'switch';
Typedef: 'typedef';
Union: 'union';
Unsigned: 'unsigned';
Void: 'void';
Volatile: 'volatile';
While: 'while';
Bool: '_Bool';
Complex: '_Complex';
Imaginary: '_Imaginary';

// 6.4.2
Identifier
    : IdentifierNondigit (IdentifierNondigit | Digit)*
    ;

fragment IdentifierNondigit
    : Nondigit
    // | UniversalCharacterName
    // other implementation-defined characters
    ;

fragment Nondigit
    : [_a-zA-Z]
    ;

fragment Digit
    : [0-9]
;

// TODO: 6.4.3 Universal character names

// 6.4.4.1
IntegerConstant
    : DecimalConstant /* integerSuffix? */
    | OctalConstant /* integerSuffix? */
    | HexadecimalConstant /* integerSuffix? */
    ;

fragment DecimalConstant
    : NonzeroDigit Digit*
    ;

fragment OctalConstant
    : '0' OctalDigit*
    ;

fragment HexadecimalConstant
    : HexadecimalPrefix HexadecimalDigit+
    ;

fragment NonzeroDigit
    : [1-9]
    ;

fragment HexadecimalPrefix
    : '0' [xX]
    ;

fragment OctalDigit
    : [0-7]
    ;

fragment HexadecimalDigit
    : [0-9a-fA-F]
    ;

// TODO: integerSuffix

// 6.4.9
BlockComment
    : '/*' .*? '*/' -> skip
    ;

LineComment
    : '//' ~[\r\n]  -> skip
    ;


Plus: '+';
Minus: '-';
Star: '*';
Slash: '/';
Percent: '%';
Caret: '^';
Amp: '&';
Pipe: '|';
Tilde: '~';
Exclaim: '!';
Equal: '=';
Less: '<';
Greater: '>';
PlusEqual: '+=';
MinusEqual: '-=';
StarEqual: '*=';
SlashEqual: '/=';
PercentEqual: '%=';
CaretEqual: '^=';
AmpEqual: '&=';
PipeEqual: '|=';
LessLess: '<<';
GreaterGreater: '>>';
LessLessEqual: '>>=';
GreaterGreaterEqual: '>>=';
EqualEqual: '==';
ExclaimEqual: '!=';
LessEqual: '<=';
GreaterEqual: '>=';
AmpAmp: '&&';
PipePipe: '||';
PlusPlus: '++';
MinusMinus: '--';
Comma: ',';
ArrowStar: '->*';
Arrow: '->';
// Conditional: '?';

Whitespace: [ \t\r\n]+ -> skip;

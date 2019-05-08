package tools.aita.kcc.parsing

import tools.aita.kcc.lang.*
import tools.aita.kcc.lexer.TokenKind
import tools.aita.kcc.lexer.TokenKind.*

open class ParseException(message: String?): RuntimeException(message)
class MismatchedTokenException(message: String): ParseException(message)
class RecognitionException(message: String): ParseException(message)


open class TokenReader(val scanner: Scanner) {
    val markers = ArrayList<Int>()
    val lookahead = ArrayList<Token>()
    var p = 0

    fun LT(i: Int): Token {
        sync(i)
        return lookahead.get(p+i-1)
    }

    fun LA(i: Int): TokenKind {
        return LT(i).kind
    }

    fun match(x: TokenKind) {
        if (LA(1) == x)
            consume()
        else
            throw MismatchedTokenException("expected '${tokenName(x)}'")
    }

    fun sync(i: Int) {
        if (p+i-1 > lookahead.size - 1) {
            val n = (p+i-1) - (lookahead.size-1)
            fill(n)
        }
    }

    fun fill(n: Int) {
        for (i in 1..n)
            lookahead.add(scanner.get())
    }

    fun consume() {
        p++
        if (p == lookahead.size && !isSpeculating()) {
            p = 0
            lookahead.clear()
        }
        sync(1)
    }

    fun mark(): Int {
        markers.add(p)
        return p
    }

    fun release() {
        val marker = markers.last()
        markers.remove(markers.lastIndex)
        seek(marker)
    }

    fun seek(index: Int) {
        p = index
    }

    fun isSpeculating(): Boolean {
        return markers.size > 0
    }
}


class Parser(scanner: Scanner): TokenReader(scanner) {
    lateinit var errors: ArrayList<Error>
    lateinit var scopeStack: ArrayList<Scope>
    lateinit var currentScope: Scope

    fun error(loc: Location, message: String) {
        errors.add(Error(loc, message))
    }

    fun pushScope(scope: Scope) {
        scopeStack.add(scope)
        currentScope = scope
    }

    fun popScope() {
        currentScope = scopeStack.last()
        scopeStack.removeAt(scopeStack.lastIndex)
    }

    fun defineSymbol(location: Location, name: String) {
        if (!currentScope.canDeclare(name))
            error(location, "redefinition of '$name'")
        else
            currentScope.define(name)
    }

    fun parse(): TranslationUnit {
        errors = ArrayList<Error>()
        scopeStack = ArrayList()
        val globalScope = GlobalScope()
        pushScope(globalScope)

        val decls = ArrayList<Decl>()
        while (LA(1) != EOF) {
            decls.add(parseExternalDefinition())
        }
        return TranslationUnit(decls, globalScope, errors)
    }

    fun parseExpression(): Expr {
        return parseAdditiveExpression()
    }

    fun parsePrimaryExpression(): Expr {
        val tok = LT(1)
        val expr = when (tok.kind) {
            IDENTIFIER -> parseVarExpr()
            INTEGER_LITERAL -> {
                consume()
                IntegerLiteral(tok.location, tok.text)
            }
            LPAREN -> {
                consume()
                val e = parseExpression()
                match(RPAREN)
                ParenExpr(tok.location, e)
            }
            else -> throw MismatchedTokenException("expected identifier, constant or '('")
        }
        return expr
    }

    fun parseVarExpr(): VarExpr {
        val id = LT(1)
        match(IDENTIFIER)
        consume()
        if (currentScope.resolve(id.text) == null) {
            error(id.location, "use of undeclared identifier '${id.text}")
        }
        return VarExpr(id.location, Identifier(id.location, id.text))
    }

    fun parsePostfixExpression(): Expr {
        val primaryExpr = parsePrimaryExpression()
        return primaryExpr
    }

    fun parseUnaryExpression(): Expr {
        val tok = LT(1)
        return when (tok.kind) {
            PLUSPLUS, MINUSMINUS -> {
                consume()
                UnaryExpr(tok.location, unaryOp(tok.kind)!!, parsePostfixExpression())
            }
            else -> parsePostfixExpression()
        }
    }

    fun parseCastExpression(): Expr {
        return parseUnaryExpression()
    }

    fun parseMultiplicaiveExpression(): Expr {
        var expr = parseCastExpression()
        while (true) {
            val tok = LT(1)
            when (tok.kind) {
                STAR, SLASH, PERCENT -> {
                    consume()
                    var left = expr
                    var right = parseMultiplicaiveExpression()
                    expr = BinaryExpr(left.location, binaryOp(tok.kind)!!, left, right)
                }
                else -> return expr
            }
        }
    }

    fun parseAdditiveExpression(): Expr {
        var expr = parseMultiplicaiveExpression()
        while (true) {
            val kind = LA(1)
            when (kind) {
                PLUS, MINUS -> {
                    consume()
                    var left = expr
                    var right = parseAdditiveExpression()
                    expr = BinaryExpr(left.location, binaryOp(kind)!!, left, right)
                }
                else -> return expr
            }
        }
    }

    fun parseStatement(): Stmt {
        return when (LA(1)) {
            SEMICOLON -> parseNullStatement()
            LBRACE -> parseCompoundStatement()
            RETURN -> parseJumpStatement()
            else -> parseExpressionStatement()
        }
    }

    fun parseNullStatement(): NullStmt {
        val start = LT(1).location
        match(SEMICOLON)
        return NullStmt(start)
    }

    fun parseExpressionStatement(): ExprStmt {
        val expr = parseExpression()
        match(SEMICOLON)
        return ExprStmt(expr.location, expr)
    }

    fun parseJumpStatement(): Stmt {
        return when(LA(1)) {
            RETURN -> parseReturnStatement()
            else -> throw throw MismatchedTokenException("expected 'return'")
        }
    }

    fun parseReturnStatement(): ReturnStmt {
        val start = LT(1).location
        match(RETURN)
        return ReturnStmt(
            start,
            if (LA(1) == SEMICOLON) parseExpression() else null
        )
    }

    fun parseCompoundStatement(): CompoundStmt {
        val scope = LocalScope(currentScope)
        pushScope(scope)
        val start = LT(1).location
        match(LBRACE)
        val stmts = ArrayList<Stmt>()
        while (LA(1) != RBRACE) {
            stmts.add(parseBlockItem())
        }
        consume()
        val stmt = CompoundStmt(start, stmts, scope)
        popScope()
        return stmt
    }

    fun parseBlockItem(): Stmt {
        return if (speculateDeclaration()) {
            var start = LT(1).location
            DeclStmt(start, parseDeclaration())
        } else {
            parseStatement()
        }
    }

    fun speculateDeclaration(): Boolean {
        return when(LA(1)) {
            in typeSpecifierKeywords -> true
            in typeQualifierKeywords -> true
            in storageClassSpecifierKeywords -> true
            in functionSpecifierKeywords -> true
            else -> false
        }
    }

    fun parseDeclaration(): List<Decl> {
        val start = LT(1).location
        val type = parseTypeSpecifier()
        val decls = ArrayList<Decl>()
        while (true) {
            val id = LT(1)
            match(IDENTIFIER)
            decls.add(VarDecl(start, type, Identifier(id.location, id.text)))
            defineSymbol(id.location, id.text)

            if (LA(1) == COMMA) {
                consume()
            } else {
                match(SEMICOLON)
                break
            }
        }
        return decls
    }

    fun parseTypeSpecifier(): Type {
        match(INT)
        return Primitive(Primitive.Kind.INT)
    }

    fun parseExternalDefinition(): Decl {
        if (LA(1) == SEMICOLON) {
            return parseEmptyDeclaration()
        }
        return parseFunctionDeclaration()
    }

    fun parseEmptyDeclaration(): Decl {
        var start = LT(1).location
        match(SEMICOLON)
        return EmptyDecl(start)
    }

    fun parseFunctionDeclaration(): Decl {
        val id = LT(1)
        match(IDENTIFIER)
        match(LPAREN)
        match(RPAREN)
        val type = Function(Primitive(Primitive.Kind.INT), listOf())
        defineSymbol(id.location, id.text)

        val scope = FuncScope(currentScope)
        pushScope(scope)
        val body = FuncBody(scope, parseCompoundStatement())
        return FuncDecl(id.location, type, Identifier(id.location, id.text), listOf(), body)
    }
}

fun unaryOp(kind: TokenKind): UnaryOperator? {
    return when (kind) {
        PLUSPLUS -> UnaryOperator.INC
        MINUSMINUS -> UnaryOperator.DEC
        AMP -> UnaryOperator.AMP
        STAR -> UnaryOperator.STAR
        PLUS -> UnaryOperator.PLUS
        MINUS -> UnaryOperator.MINUS
        TILDE -> UnaryOperator.NOT
        EXCLAIM -> UnaryOperator.LNOT
        else -> null
    }
}

fun binaryOp(kind: TokenKind): BinaryOperator? {
    return when(kind) {
        STAR -> BinaryOperator.MUL
        SLASH -> BinaryOperator.DIV
        PERCENT -> BinaryOperator.MOD
        PLUS -> BinaryOperator.ADD
        MINUS -> BinaryOperator.SUB
        LESSLESS -> BinaryOperator.SLL
        GREATERGREATER -> BinaryOperator.SRL
        LESS -> BinaryOperator.LT
        GREATER -> BinaryOperator.GT
        LESSEQUAL -> BinaryOperator.LE
        GREATEREQUAL -> BinaryOperator.GE
        EQUALEQUAL -> BinaryOperator.EQ
        EXCLAIMEQUAL -> BinaryOperator.NEQ
        AMP -> BinaryOperator.AND
        CARET -> BinaryOperator.XOR
        PIPE -> BinaryOperator.OR
        AMPAMP -> BinaryOperator.LAND
        PIPEPIPE -> BinaryOperator.LOR
        else -> null
    }
}

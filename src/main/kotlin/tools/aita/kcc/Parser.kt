package tools.aita.kcc

import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.Token
import tools.aita.kcc.grammar.CBaseVisitor
import tools.aita.kcc.grammar.CLexer
import tools.aita.kcc.grammar.CParser

data class SourceFile(val name: String, val source: String)

fun parse(file: SourceFile): List<Decl> {
    val input = ANTLRInputStream(file.source)
    val lexer = CLexer(input)
    val stream = CommonTokenStream(lexer)
    val parser = CParser(stream)

    return parseTranslationUnit(parser.translationUnit()!!)
}


class ExprVisitor : CBaseVisitor<Expr>() {
    override fun visitIdentifier(ctx: CParser.IdentifierContext?): Expr {
        return Identifier(ctx!!.text)
    }

    override fun visitInteger(ctx: CParser.IntegerContext?): Expr {
        return IntegerConstant(ctx!!.text)
    }

    override fun visitGrouping(ctx: CParser.GroupingContext?): Expr {
        return visit(ctx!!.e)
    }

    override fun visitFuncCall(ctx: CParser.FuncCallContext?): Expr {
        val func = visit(ctx!!.function)
        val args = if (ctx.args == null) ArrayList<Expr>()
        else ctx.args.exprs.map { visit(it) }
        return CallExpr(func, args)
    }

    override fun visitIncrement(ctx: CParser.IncrementContext?): Expr {
        return UnaryExpr(unaryOp(ctx!!.op), visit(ctx.e))
    }

    override fun visitUnaryOp(ctx: CParser.UnaryOpContext?): Expr {
        return UnaryExpr(unaryOp(ctx!!.op), visit(ctx.e))
    }

    override fun visitCast(ctx: CParser.CastContext?): Expr {
        val type = parseTypeSpecifier(ctx!!.ty.typeSpecifier())
        return CastExpr(
            if (ctx.ty.abstractDeclarator()?.pointer() != null) Pointer(type) else type,
            visit(ctx.e)
        )
    }

    override fun visitMultOp(ctx: CParser.MultOpContext?): Expr {
        return BinaryExpr(binaryOp(ctx!!.op), visit(ctx.left), visit(ctx.right))
    }

    override fun visitAddOp(ctx: CParser.AddOpContext?): Expr {
        return BinaryExpr(binaryOp(ctx!!.op), visit(ctx.left), visit(ctx.right))
    }

    override fun visitShiftOp(ctx: CParser.ShiftOpContext?): Expr {
        return BinaryExpr(binaryOp(ctx!!.op), visit(ctx.left), visit(ctx.right))
    }

    override fun visitRelOp(ctx: CParser.RelOpContext?): Expr {
        return BinaryExpr(binaryOp(ctx!!.op), visit(ctx.left), visit(ctx.right))
    }

    override fun visitAndOp(ctx: CParser.AndOpContext?): Expr {
        return BinaryExpr(binaryOp(ctx!!.op), visit(ctx.left), visit(ctx.right))
    }

    override fun visitXorOp(ctx: CParser.XorOpContext?): Expr {
        return BinaryExpr(binaryOp(ctx!!.op), visit(ctx.left), visit(ctx.right))
    }

    override fun visitOrOp(ctx: CParser.OrOpContext?): Expr {
        return BinaryExpr(binaryOp(ctx!!.op), visit(ctx.left), visit(ctx.right))
    }

    override fun visitLogicalAndOp(ctx: CParser.LogicalAndOpContext?): Expr {
        return BinaryExpr(binaryOp(ctx!!.op), visit(ctx.left), visit(ctx.right))
    }

    override fun visitLogicalOrOp(ctx: CParser.LogicalOrOpContext?): Expr {
        return BinaryExpr(binaryOp(ctx!!.op), visit(ctx.left), visit(ctx.right))
    }

    override fun visitConditionalOp(ctx: CParser.ConditionalOpContext?): Expr {
        return ConditionalExpr(visit(ctx!!.e1), visit(ctx.e2), visit(ctx.e3))
    }

    override fun visitAssignOp(ctx: CParser.AssignOpContext?): Expr {
        return super.visitAssignOp(ctx)
    }

    override fun visitCommaOp(ctx: CParser.CommaOpContext?): Expr {
        return CommaExpr(visit(ctx!!.left), visit(ctx.right))
    }
}

fun unaryOp(tok: Token): UnaryOperator {
    return when (tok.type) {
        CParser.PlusPlus -> UnaryOperator.INC
        CParser.MinusMinus -> UnaryOperator.DEC
        CParser.Amp -> UnaryOperator.AMP
        CParser.Star -> UnaryOperator.STAR
        CParser.Plus -> UnaryOperator.PLUS
        CParser.Minus -> UnaryOperator.MINUS
        CParser.Tilde -> UnaryOperator.NOT
        CParser.Exclaim -> UnaryOperator.LNOT
        else -> throw RuntimeException("unknown unary operator: " + tok.text)
    }
}

fun binaryOp(tok: Token): BinaryOperator {
    return when(tok.type) {
        CParser.Star -> BinaryOperator.MUL
        CParser.Slash -> BinaryOperator.DIV
        CParser.Percent -> BinaryOperator.MOD
        CParser.Plus -> BinaryOperator.ADD
        CParser.Minus -> BinaryOperator.SUB
        CParser.LessLess -> BinaryOperator.SLL
        CParser.GreaterGreater -> BinaryOperator.SRL
        CParser.Less -> BinaryOperator.LT
        CParser.Greater -> BinaryOperator.GT
        CParser.LessEqual -> BinaryOperator.LE
        CParser.GreaterEqual -> BinaryOperator.GE
        CParser.EqualEqual -> BinaryOperator.EQ
        CParser.ExclaimEqual -> BinaryOperator.NEQ
        CParser.Amp -> BinaryOperator.AND
        CParser.Caret -> BinaryOperator.XOR
        CParser.Pipe -> BinaryOperator.OR
        CParser.AmpAmp -> BinaryOperator.LAND
        CParser.PipePipe -> BinaryOperator.LOR
        else -> throw RuntimeException("unknown binary operator: " + tok.text)
    }
}


class StmtVisitor : CBaseVisitor<Stmt>() {
    override fun visitLabelStmt(ctx: CParser.LabelStmtContext?): Stmt {
        return LabeledStmt(ctx!!.label.text, visit(ctx.stmt))
    }

    override fun visitCompoundStatement(ctx: CParser.CompoundStatementContext?): Stmt {
        return if (ctx!!.items != null) visit(ctx.items) else NullStmt()
    }

    override fun visitBlockItemList(ctx: CParser.BlockItemListContext?): Stmt {
        return CompoundStmt(ctx!!.items.map { visit(it) })
    }

    override fun visitBlockDecl(ctx: CParser.BlockDeclContext?): Stmt {
        return DeclStmt(parseDeclaration(ctx!!.declaration()))
    }

    override fun visitExpressionStatement(ctx: CParser.ExpressionStatementContext?): Stmt {
        return if (ctx!!.e != null) ExprStmt(ExprVisitor().visit(ctx.e)) else NullStmt()
    }

    override fun visitIfStmt(ctx: CParser.IfStmtContext?): Stmt {
        return IfStmt(
            ExprVisitor().visit(ctx!!.e),
            visit(ctx.thenStmt),
            if (ctx.elseStmt != null) visit(ctx.elseStmt) else null
        )
    }

    override fun visitWhileStmt(ctx: CParser.WhileStmtContext?): Stmt {
        return WhileStmt(ExprVisitor().visit(ctx!!.e), visit(ctx.stmt))
    }

    override fun visitGotoStmt(ctx: CParser.GotoStmtContext?): Stmt {
        return GotoStmt(ctx!!.id.text)
    }

    override fun visitContinueStmt(ctx: CParser.ContinueStmtContext?): Stmt {
        return ContinueStmt()
    }

    override fun visitBreakStmt(ctx: CParser.BreakStmtContext?): Stmt {
        return BreakStmt()
    }

    override fun visitReturnStmt(ctx: CParser.ReturnStmtContext?): Stmt {
        return ReturnStmt(if (ctx!!.e != null) ExprVisitor().visit(ctx.e) else null)
    }
}

fun parseTypeSpecifier(ctx: CParser.TypeSpecifierContext): Type {
    val t = ctx.type
    return Primitive(when (t.type) {
        CParser.Int -> Primitive.Kind.INT
        else -> throw RuntimeException("unknown type specifier: " + t.text)
    })
}

fun parseDeclaration(ctx: CParser.DeclarationContext): List<Decl> {
    val type = parseTypeSpecifier(ctx.typeSpecifier())
    return ctx.declarators.map{ parseDeclarator(it, type) }
}

fun parseDeclarator(ctx: CParser.DeclaratorContext, declType: Type): Decl {
    val d = ctx.directDeclarator()
    val name = d.name.text
    val type = if (ctx.pointer() != null) Pointer(declType) else declType
    if (d.getChild(1)?.text == "(") {
        val params = if (d.parameterList() != null) parseParameterList(d.parameterList())
            else ArrayList<ParamDecl>()
        val paramTypes = params.map{ it.type }
        return FuncDecl(Function(type, paramTypes), name, params, null)
    } else {
        return VarDecl(type, name)
    }
}

fun parseParameterList(ctx: CParser.ParameterListContext): List<ParamDecl> {
    return ctx.params.map {
        val type = parseTypeSpecifier(it.typeSpecifier())
        val decl = parseDeclarator(it.declarator(), type)
        when (decl) {
            is FuncDecl -> ParamDecl(decl.type, decl.name)
            is VarDecl -> ParamDecl(decl.type, decl.name)
            else -> throw RuntimeException("Unknown declaration: " + decl.toString())
        }
    }
}

fun parseTranslationUnit(ctx: CParser.TranslationUnitContext): List<Decl> {
    return ctx!!.decls.flatMap { parseExternalDecration(it) }
}

fun parseExternalDecration(ctx: CParser.ExternalDeclarationContext): List<Decl> {
    if (ctx.functionDefinition() != null) {
        return listOf(parseFunctionDefenition(ctx.functionDefinition()))
    } else {
        return parseDeclaration(ctx.declaration())
    }
}

fun parseFunctionDefenition(ctx: CParser.FunctionDefinitionContext): Decl {
    val declType = if (ctx.typeSpecifier() != null) parseTypeSpecifier(ctx.typeSpecifier())
        else Primitive(Primitive.Kind.INT)
    val decl = parseDeclarator(ctx.declarator(), declType)
    if (!(decl is FuncDecl))
        throw RuntimeException("the declarator is not a function definition")
    return FuncDecl(decl.type, decl.name, decl.params, StmtVisitor().visit(ctx.compoundStatement()) as CompoundStmt)
}
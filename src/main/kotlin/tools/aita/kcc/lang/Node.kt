package tools.aita.kcc.lang

import tools.aita.kcc.parsing.Location

interface Node {
    val location: Location
}

enum class UnaryOperator {
    INC,
    DEC,
    AMP,
    STAR,
    PLUS,
    MINUS,
    NOT,
    LNOT,
}

enum class BinaryOperator {
    EQ,
    NEQ,
    LT,
    GT,
    LE,
    GE,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    SLL,
    SRL,
    AND,
    OR,
    XOR,
    LAND,
    LOR,
}

enum class AssignOperator {
    ASSIGN,
    ADD_ASSIGN,
    SUB_ASSIGN,
    MUL_ASSIGN,
    DIV_ASSIGN,
    MOD_ASSIGN,
    SLL_ASSIGN,
    SRL_ASSIGN,
    AND_ASSIGN,
    OR_ASSIGN,
    XOR_ASSIGN,
}


data class Identifier(override val location: Location, val text: String): Node

sealed class Decl: Node
data class EmptyDecl(override val location: Location): Decl()
data class VarDecl(override val location: Location, val type: Type, val id: Identifier): Decl()
data class FuncDecl(override val location: Location, val type: Type, val id: Identifier, val params: List<ParamDecl>, val body: FuncBody?): Decl()
data class FuncBody(val scope: FuncScope, val body: CompoundStmt)
data class ParamDecl(override val location: Location, val type: Type, val id: Identifier?): Decl()

sealed class Stmt: Node
data class NullStmt(override val location: Location): Stmt()
data class ExprStmt(override val location: Location, val expr: Expr): Stmt()
data class LabeledStmt(override val location: Location, val label: Identifier, val stmt: Stmt): Stmt()
data class IfStmt(override val location: Location, val condition: Expr, val thenStmt: Stmt, val elseStmt: Stmt?): Stmt()
data class WhileStmt(override val location: Location, val condition: Expr, val stmt: Stmt): Stmt()
data class GotoStmt(override val location: Location, val label: Identifier): Stmt()
class ContinueStmt(override val location: Location): Stmt()
class BreakStmt(override val location: Location): Stmt()
data class ReturnStmt(override val location: Location, val expr: Expr?): Stmt()
data class CompoundStmt(override val location: Location, val stmts: List<Stmt>, val scope: LocalScope): Stmt()
data class DeclStmt(override val location: Location, val decls: List<Decl>): Stmt()

sealed class Expr: Node
data class IntegerLiteral(override val location: Location, val text: String): Expr()
data class VarExpr(override val location: Location, val id: Identifier): Expr()
data class ParenExpr(override val location: Location, val expr: Expr): Expr()
data class UnaryExpr(override val location: Location, val operator: UnaryOperator, val operand: Expr): Expr()
data class BinaryExpr(override val location: Location, val operator: BinaryOperator, val left: Expr, val right: Expr): Expr()
data class AssignExpr(override val location: Location, val operator: AssignExpr, val left: Expr, val right: Expr): Expr()
data class CallExpr(override val location: Location, val function: Expr, val args: List<Expr>): Expr()
data class CastExpr(override val location: Location, val type: Type, val operand: Expr): Expr()
data class CommaExpr(override val location: Location, val left: Expr, val right: Expr): Expr()
data class ConditionalExpr(override val location: Location, val first: Expr, val second: Expr, val third: Expr): Expr()


data class Error(val location: Location, val message: String) {
    override fun toString(): String {
        return "$location: error: $message"
    }
}

data class TranslationUnit(
    val decls: List<Decl>,
    val scope: GlobalScope,
    val errors: List<Error>
)

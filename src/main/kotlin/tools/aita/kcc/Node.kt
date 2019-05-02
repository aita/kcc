package tools.aita.kcc

interface Node {

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

sealed class Type
data class Primitive(val kind: Kind): Type() {
    enum class Kind {
        // VOID,
        // CHAR,
        // SHORT,
        INT,
        // LONG,
        // LONG_LONG,
        // FLOAT,
        // DOUBLE,
        // LONG_DOUBLE,
    }
}
data class Pointer(val pointerOf: Type?): Type()
data class Function(val returnType: Type, val paramTypes: List<Type>): Type()


sealed class Decl: Node
data class VarDecl(val type: Type, val name: String): Decl()
data class FuncDecl(val type: Type, val name: String, val params: List<ParamDecl>, val body: CompoundStmt?): Decl()
data class ParamDecl(val type: Type, val name: String?): Decl()

sealed class Stmt: Node
class NullStmt: Stmt()
data class ExprStmt(val expr: Expr): Stmt()
data class LabeledStmt(val label: String, val stmt: Stmt): Stmt()
data class IfStmt(val condition: Expr, val thenStmt: Stmt, val elseStmt: Stmt?): Stmt()
data class WhileStmt(val condition: Expr, val stmt: Stmt): Stmt()
data class GotoStmt(val label: String): Stmt()
class ContinueStmt: Stmt()
class BreakStmt: Stmt()
data class ReturnStmt(val expr: Expr?): Stmt()
data class CompoundStmt(val stmts: List<Stmt>): Stmt()
data class DeclStmt(val decls: List<Decl>): Stmt()

sealed class Expr: Node
data class IntegerConstant(val text: String): Expr()
data class Identifier(val text: String) : Expr()
data class UnaryExpr(val operator: UnaryOperator, val operand: Expr): Expr()
data class BinaryExpr(val operator: BinaryOperator, val left: Expr, val right: Expr): Expr()
data class AssignExpr(val operator: AssignExpr, val left: Expr, val right: Expr): Expr()
data class CallExpr(val function: Expr, val args: List<Expr>): Expr()
data class CastExpr(val type: Type, val expr: Expr): Expr()
data class CommaExpr(val left: Expr, val right: Expr): Expr()
data class ConditionalExpr(val first: Expr, val second: Expr, val third: Expr): Expr()

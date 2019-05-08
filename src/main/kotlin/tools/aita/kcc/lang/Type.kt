package tools.aita.kcc.lang

sealed class Type
data class Primitive(val kind: Kind): Type() {
    enum class Kind {
        VOID,
        CHAR,
        SHORT,
        INT,
        LONG,
        LONG_LONG,
        FLOAT,
        DOUBLE,
        LONG_DOUBLE,
    }
}
data class Pointer(val pointerOf: Type?): Type()
data class Function(val returnType: Type, val paramTypes: List<Type>): Type()


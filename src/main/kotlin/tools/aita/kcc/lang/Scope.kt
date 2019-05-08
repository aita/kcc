package tools.aita.kcc.lang


sealed class Scope(val parent: Scope? = null) {
    val symbols = mutableMapOf<String, Symbol>()
    var children = ArrayList<Scope>()

    init {
        if (parent != null)
            parent.addChild(this)
    }

    fun define(name: String) {
        symbols[name] = Symbol(name)
    }

    fun resolve(name: String): Symbol? {
        return symbols[name] ?: parent?.resolve(name)
    }

    open fun canDeclare(name: String): Boolean {
        return name in symbols
    }

    fun addChild(scope: Scope) {
        children.add(scope)
    }
}


class GlobalScope(parent: Scope? = null): Scope(parent)
class FuncScope(parent: Scope? = null): Scope(parent)
class LocalScope(parent: Scope? = null): Scope(parent) {
    override fun canDeclare(name: String): Boolean {
        return name in symbols || if (parent is FuncScope) parent.canDeclare(name) else false
    }
}

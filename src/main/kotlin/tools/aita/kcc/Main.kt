package tools.aita.kcc

fun main(args: Array<String>) {
    val file = SourceFile("", "main(){ return 1; }")
    val node = parse(file)
    println(node)
}

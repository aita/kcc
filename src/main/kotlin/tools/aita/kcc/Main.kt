package tools.aita.kcc

import tools.aita.kcc.parsing.Parser
import tools.aita.kcc.parsing.Scanner
import tools.aita.kcc.parsing.SourceFile
import java.io.File
import java.io.FileNotFoundException
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    if (args.size != 1) {
        println("kcc: error: no input file")
        exitProcess(1)
    }
    val filename = args[0]
    val file = try {
        val source = File(filename).readText()
        SourceFile(filename, source)
    } catch (e: FileNotFoundException) {
        println("kcc: error: no such file or directory: '$filename'")
        exitProcess(1)
    }
    val scanner = Scanner(file)
    val parser = Parser(scanner)
    val translationUnit = parser.parse()
    println(translationUnit)
    if (translationUnit.errors.isNotEmpty()) {
        for (error in translationUnit.errors) {
            println(error)
        }
        exitProcess(1)
    }
}

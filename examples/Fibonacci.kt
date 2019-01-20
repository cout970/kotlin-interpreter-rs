@file:Suppress("unused")

external fun println(x: Any)
external fun <T> listOf(vararg value: T): List<T>

fun main(args: Array<String>) {
    println(fibonacci(12))
//    listOf(1,2,3).map{}
}

fun fibonacci(x: Int): Int = when (x) {
    0, 1 -> 1
    else -> fibonacci(x - 1) + fibonacci(x - 2)
}


class MyBox<T>(private val value: T?) {

    fun get() = value!!

    fun isNull() = value == null

    fun <R> map(func: (T) -> R) = when (value) {
        null -> this
        else -> MyBox(func(value))
    }

    sealed class Result<T> {
        object Err : Result<Nothing>()
        class Ok<T>(val value: T) : Result<T>()
    }
}

val <T> List<T>.first: T get() = this[0]

/**
 * You can edit, run, and share this code.
 * play.kotlinlang.org
 */

lateinit var a: List<Int>



// var a = 0
// 	private in set


fun finally(a: () -> String): String? {
    println("The result is ${a()}")
    val b = object : MyClass({}) {}
    return null
}


class Test constructor() {

    fun test(){
        lateinit var myString: String
        data class Testing(val i: Int)
        myString = ""
    }
}

enum class Color {
    Red,
    inline Green,
    Blue
}

fun main() {
    Test().test()
    println("Hello, world!!!")
    finally{
        """Custom SQL:
        listOf(1,2,3,4)
        mapOf(a to 1, b to 2, c to 3)
    	["a","b","c"] from [table1("nid") join table2("node_ref")] where ["a" > 2, "b" < 2, "c" == count("c")] sortBy [desc("a"), asc("b")] limitedTo 10
    	"""
    }

//     val a = for(i in listOf(1, 2, 3)){}
//     val b = while(false){}
//     val c = do{}while(false)
//     val d = break
//     val e = continue
//     val f = return
//     val g = throw NullPointerException()


    true is Boolean
    val a = 5


    1 in listOf(1,2,3)
    when ('b'){
        'a' -> 1
        'b' -> if (5 > 3) {
            println("Here 1")
        }
        else -> println("Here 2")
    }

}

abstract class MyClass(val func: ()->Unit){
    open fun say(){

    }
}

open class Testing(func: ()->Unit) : MyClass(func){
    override fun say(){

    }
}

// class MyList<T> : ArrayList<T> { 1 + 2 }
// class MyList<T> : ArrayList<T> { 1 + 2 } { val age = 5 }

object TestinObj : Testing({}){
    init {
    }
}
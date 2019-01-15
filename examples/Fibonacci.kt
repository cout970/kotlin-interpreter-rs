external fun println(x: Any)

fun main() {
    println(fibonacci(12))
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
}
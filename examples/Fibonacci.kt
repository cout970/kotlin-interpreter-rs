fun main() {
    println(fibonacci(12))
}

fun fibonacci(x: Int): Int = when (x) {
    0, 1 -> 1
    else -> fibonacci(x - 1) + fibonacci(x - 2)
}
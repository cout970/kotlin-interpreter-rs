fun test() {
    when(5) {
        1 -> "1"
        2, 3 -> "2 or 3"
        in 1..2 -> "in range"
        !in 1..2 -> "not in range"
        is Int -> "is int"
        !is Int -> "not is int"
        1 + 2  -> "is 3"
        else -> "else"
    }

    when {
        it % 2 == 0 -> "is pair"
        true -> "is true"
        false -> "is not true"
    }
}
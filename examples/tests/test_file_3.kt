
private fun test1() {}

fun <T> test2() {}

fun test3(a: Int) {}

fun test4(a: Int): Boolean {}

fun test5(a: Int): Boolean = false

fun test6(a: Int, b: Int) {}

fun test7(a: Int, b: Int): List<Int> {}

fun Int.test8(a: Int): java.lang.List<Int> {}

fun kotlin.Int.test9(a: Int): java.lang.List<Int> {}

fun List<Int>.test9(a: Int): java.lang.List<Int> {}

fun kotlin.collections.List<Int>.test10(a: Int) {}

fun kotlin.collections.List<Int>.test11(a: kotlin.Int) {}

fun kotlin.collections.List<Int>.test12(vararg a: kotlin.Int) {}

fun test13(crossinline a: ()->Unit) {}

fun test14(a: (Int, kotlin.List<Int>)->Unit) {}

fun Int?.test15() {}

fun (() -> Int).test16() {}

fun (Int).test17() {}

fun (kotlin.Int).test18() {}

fun (List<kotlin.Int>).test19() {}

fun (kotlin.collection.List<kotlin.Int>).test20() {}

fun (kotlin.collection.Map<kotlin.String, kotlin.Int>).test21() {}

fun (() -> Int)?.test22() {}

fun (Int)?.test23() {}

fun (Int)?.test24(): Int? {}

fun (Int)?.test25(): (Int?) {}

fun (Int)?.test26(): (Int)? {}

fun <T: List<B>, B> T?.test27(): B {}
typealias A = Int
typealias B = List<Int>
typealias C<D> = List<D>
typealias E = List<*>

val a = 0
val b: Int = 0
val Int.b: Int get() = 1
val Int.b get() = 1
val Int.b: Int get(): Int = 1
val <A> A.b: Int get(): Int = 1
val <A> A.b: A get(): A = 1

val <A> A.b: A
    get(): A = 0 as A

val c: Int
    inline get

var d: Int
    private set

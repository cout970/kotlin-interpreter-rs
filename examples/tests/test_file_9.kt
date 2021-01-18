class A
class B {}
class C<T>
class D<T> : List
class E<T> : List<T>
class F<T>(): List<T> {}
class G<T>(): List<T> where T: Number {}
class H<T> private constructor(): List<T> where T: Number {}
class H<T> private constructor(): List<T> where T: Number, T: Int {}

public enum class A {}
#!/bin/bash
@file:Suppress("unused")

import TestinObj.A

external fun println(x: Any)
external fun <T> listOf(vararg value: T): List<T>

fun main(args: Array<String>) {
    println(fibonacci(12))
//    listOf(1,2,3).map{}


//    val a: collections.BooleanIterator? = null
//    val b: BooleanIterator? = null
}

fun t(vararg a: Int, b: Float) {

}

class F {
    var f: Int = 0
        get() = field + 1


    fun getF(): Int {
        val a = F::f
        val b = F::getF
        val c = {}

        abstract class I {
            abstract fun t()
        }

        val d = object : I() {
            override fun t() {

            }
        }
        val e = fun(): Int { return 1.and(TODO()) }

        val f = if (1 > 2) {
            3
        } else {
            4
        }

        val g = object {
            var t = 0
        }
        g.t = 1
        return 0
    }
}

//fun test_fib(a: Int) = if (a > 0) test_fib(a - 1) else 1
//fun test_fib(a: Int) = if (a > 0) 1 else test_fib(a - 1)

fun fibonacci(x: Int): Int = when (x) {
    0, 1 -> 1
    else -> fibonacci(x - 1) + fibonacci(x - 2)
}

fun type_of_null() = null

val a: Int
    get() = a

fun test() {
    val a: Int by 0
    var out = 0

//    val (c: Int, b: Float) = listOf(1, 2)
//    //
//    val aux = listOf(1, 2)
//    val c: Int = aux.component1()
//    val b: Int = aux.component2()

    operator fun Int.iterator(): Iterator<Int> = (0..(this - 1)).iterator()

    for (i in 10) {

    }


    out = 0

    when {
        3 > a -> {

        }
    }

    inline fun <reified T> T.test() = T::class

    0.test()
}

var test: Int
    inline get() = 5
    internal inline set(it) {}


operator fun Int.getValue(a: Int?, prop: kotlin.reflect.KProperty<*>): Int {
    return 0
}



class MyBox<T>(private val value: T?) {

    fun get() = value!!

    fun isNull() = value == null

    fun <R> map(func: (T) -> R) = when (value) {
        null -> this
        else -> MyBox(func(value))
    }
}

sealed class Result<A, B>
class Ok<T, E>(val value: T) : Result<T, E>()
class Err<T, E>(val value: T) : Result<T, E>()

sealed class Option<T>
class Some<T>(val value: T) : Option<T>()
object None : Option<Nothing>() {
    fun <T> cast(): Option<T> = None as Option<T>
}

fun getOpt(): Option<Int> = match(None as Option<Int>)

fun match(value: Option<Int>): Option<Int> = when(value){
    is Some -> Some(value.value)
    else -> None.cast()
}

fun match(value: Result<Int, Int> = Err(0)): Option<Int> = when(value){
    is Ok -> Some(value.value)
    is Err -> None.cast()
}


//var <T> List<T>.first: T? = null
//    get() = this[0]
//    private set

fun <T> T.apply(func: T.() -> Unit): T {
    func()
    return this
}


/**
 * You can edit, run, and share this code.
 * play.kotlinlang.org
 */

//lateinit var a: List<Int>

// var a = 0
// 	private in set

fun finally(a: () -> String): String? {
    println("The result is ${a()}")
    val b = object : MyClass({}) {}
    return null
}

// This
enum class Color(val rgb: Int) {
    Red(0xFF0000) {
        override fun asHex(): String = "#FF000"
    };

    abstract fun asHex(): String
}

// Becomes this
sealed class Color2(val rgb: Int) {

    companion object {
        val Red = `Color2$Red`
    }

    object `Color2$Red` : Color2(0xFF0000) {
        override fun asHex(): String = "#FF000"
    }

    abstract fun asHex(): String
}

class Test constructor() {

    fun test() {
        lateinit var myString: String

        data class Testing(val i: Int)
        myString = ""
    }
}

//enum class Color {
//    Red,
//    inline Green,
//    Blue
//}

fun main() {
    Test().test()
    println("Hello, world!!!")
    finally {
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
    val obj = 5

    operator fun String.invoke(vararg others: Int) {

    }

    ""(2, 3, 4)


    fun <T> t(arg: T): String {
        return "T"
    }
//    t<*>(0)


    1 in listOf(1, 2, 3)
    when ('b') {
        'a' -> 1
        'b' -> if (5 > 3) {
            println("Here 1")
        }
        else -> println("Here 2")
    }

}

abstract class MyClass(val func: () -> Unit) {
    open fun say() {

    }
}

open class Testing(func: () -> Unit) : MyClass(func) {
    override fun say() {

    }
}

// class MyList<T> : ArrayList<T> { 1 + 2 }
// class MyList<T> : ArrayList<T> { 1 + 2 } { val age = 5 }

object TestinObj : Testing({}) {

    object A {
        object B {
            init {

            }

            fun t() {

            }
        }
    }
}


fun <C, T> T.list(): C where T : C {
    A.B.t()
    TODO()
}

fun TODO(): Nothing {
    throw Throwable()
}

object B {

}
//
//package com.mcmacker4.raytracer.material
//
//import com.mcmacker4.raytracer.tracing.HitInfo
//import com.mcmacker4.raytracer.tracing.Ray
//import com.mcmacker4.raytracer.util.*
//import org.joml.Vector3f
//import org.joml.Vector3fc
//import java.lang.Math.pow
//
//data class MatResult(val scatter: Vector3fc, val attenuation: Vector3fc)
//
//interface Material {
//
//    fun scatter(ray: Ray, hit: HitInfo) : MatResult?
//
//}
//
//private fun randInUnitSphere() : Vector3fc {
//    var p: Vector3fc
//    do {
//        p = Vector3f(randf(), randf(), randf()) * 2f - 1f
//    } while(p.dot(p) >= 1f)
//    return p
//}
//
//class Lambertian(val albedo: Vector3fc) : Material {
//
//    override fun scatter(ray: Ray, hit: HitInfo): MatResult? {
//        val scattered = (hit.pos + hit.normal + randInUnitSphere()).normalize()
//        val attenuation = albedo
//        return MatResult(scattered, attenuation)
//    }
//
//}
//
//class Metallic(val albedo: Vector3fc, val roughness: Float) : Material {
//
//    private fun randRoughnessVec() : Vector3f {
//        return if(roughness > 0f)
//            randInUnitSphere() * roughness
//        else
//            Vector3f(0f)
//    }
//
//    override fun scatter(ray: Ray, hit: HitInfo): MatResult? {
//        val reflected = (ray.dir.reflect(hit.normal, Vector3f()) + randRoughnessVec()).normalize()
//
//        return if(reflected.dot(hit.normal) > 0)
//            MatResult(reflected, albedo)
//        else null
//    }
//
//}
//
//class Dielectric(val albedo: Vector3fc, val roughness: Float, val refIndex: Float) : Material {
//
//    fun shlick(cos: Float, refIndex: Float): Float {
//        var r0 = (1 - refIndex) / (1 + refIndex)
//        r0 *= r0
//        return r0 + (1 - r0) * pow((1.0 - cos), 5.0).toFloat()
//    }
//
//    override fun scatter(ray: Ray, hit: HitInfo): MatResult? {
//
//        val reflected = Vector3f(ray.dir).reflect(hit.normal)
//        val attenuation = albedo
//
//        val outNormal: Vector3f
//        val niOverNt: Float
//        val cosine: Float
//
//        if(ray.dir.dot(hit.normal) > 0) {
//            outNormal = -hit.normal
//            niOverNt = refIndex
//            cosine = refIndex * ray.dir.dot(hit.normal)
//        } else {
//            outNormal = Vector3f(hit.normal)
//            niOverNt = 1 / refIndex
//            cosine = -ray.dir.dot(hit.normal)
//        }
//
//        val reflectProb: Float
//
//        val refracted = ray.dir.refract(outNormal, niOverNt)
//        reflectProb = if(refracted != null) {
//            shlick(cosine, refIndex)
//        } else {
//            1f
//        }
//
//        return if(randf() < reflectProb) {
//            MatResult((reflected + randInUnitSphere() * roughness).normalize(), attenuation)
//        } else {
//            MatResult((refracted!! + randInUnitSphere() * roughness).normalize(), attenuation)
//        }
//
//    }
//
//}
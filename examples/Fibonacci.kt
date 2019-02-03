#!/bin/bash
@file:Suppress("unused")

external fun println(x: Any)
external fun <T> listOf(vararg value: T): List<T>

fun main(args: Array<String>) {
    println(fibonacci(12))
//    listOf(1,2,3).map{}


//    val a: collections.BooleanIterator? = null
//    val b: BooleanIterator? = null
}

fun fibonacci(x: Int): Int = when (x) {
    0, 1 -> 1
    else -> fibonacci(x - 1) + fibonacci(x - 2)
}

val a: Int
    get() = a

fun test() {
    val a: Int by 0
    var out = 0

    out = 0

    when {
        3 > a -> {

        }
    }

    inline fun <reified T> T.test() = T::class

    0.test()
}

class A {

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

    sealed class Result<T> {
        object Err : Result<Nothing>()
        class Ok<T>(val value: T) : Result<T>()
    }
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
    init {

    }
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
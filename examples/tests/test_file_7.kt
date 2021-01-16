fun test() {
    a++
    a--
    a!!
    a[0]

    a()
    a(1)
    a(1, 2)
    a<Int>()
    a<Int, Float>(1, 2.5)
    a{}
    a(){}
    a({}, 1, 2)
    a()[3]

    a.value
    a?.value
    a.value()
    a?.value(1,2,3)
    a?.value(1,2,3).other_value
    a?.value(1,2,3).other_func()
    a?.value(1,2,3)?.other_func()[0]
}
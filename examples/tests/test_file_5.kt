fun test() {
    1
    1.5
    1L
    1.5F
    'a'
    "a"
    "a$now"
    "a${now + 1}"
    super
    this
    null
    true
    false
    my_var
    (true)
    throw null
    continue
    break
    return
    return 1

    1 is Int
    1 in 0..10

    if(true) {}
    if(true) {} else {}
    try {} catch(e: Exception) {}
    try {} catch(e: Exception) {} catch(e: Throwable) {}
    try {} catch(e: Exception) {} catch(e: Throwable) {} finally {}

    {}
    { i -> }
    { i,j,k -> }
    { i: Int -> }
    { i: Int, j: Float -> true }

    object {}
    object : Number() {}
    object : Number(), Comparable<Int>, Iterable by 1 {}
    object : Number({}) {}
}
//pub mod semantic_rules;
pub mod typechecker;
pub mod ast;
pub mod tree_to_ast;
mod modifiers;
mod mutable_tree_visitor;

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_success;
    use crate::test_utils::assert_fails;

    #[test]
    fn check_properties() {
        assert_success("val a: Int");
        assert_success("val a: Int = 0");
        assert_success("val a = 0");
        assert_success("var a: Int");
        assert_success("var a: Int = 0");
        assert_success("var a = 0");
    }

    #[test]
    fn check_external_fun() {
        assert_success("external fun test0()");
        assert_success("external fun test1(arg1: Int)");
        assert_success("external fun test2(arg1: Int = 0)");
        assert_success("external fun test3(arg1: Int, arg2: Int)");
        assert_success("external fun test4(): Int");
        assert_success("external fun test5(): Unit");
        assert_fails("external fun test6(val arg1: Int)");
        assert_fails("external fun test7(var arg1: Int)");
        assert_fails("external fun test8(arg1)");
        assert_fails("external fun test9(arg1): Int");
        assert_fails("external fun test10(arg1 = 0)");
        assert_success("external fun Int.test11()");
        assert_success("external fun Int.test12(arg1: Int)");
        assert_success("external fun Int.test13(arg1: Int): Int");
    }

    #[test]
    fn check_modifiers() {
        assert_success("public fun test0() = 0");
        assert_success("private fun test1() = 0");
        assert_success("protected fun test2() = 0");
        assert_success("internal fun test3() = 0");
        assert_fails("public private fun test4() = 0");
        assert_fails("private public fun test5() = 0");
        assert_fails("protected internal fun test6() = 0");
        assert_fails("internal protected fun test7() = 0");
        assert_success("public inline operator fun Int.plus(other: Int): Int = this + other");
        assert_fails("actual expect fun test8() = 0");
        assert_fails("final fun test9() = 0");
        assert_fails("abstract final fun test10() = 0");
    }
}
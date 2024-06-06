use crate::assert_erl;

#[test]
fn named_args_count_once() {
    assert_erl!(
        "
        pub fn foo() -> Result(a, a) {
            todo
        }
        "
    );
}

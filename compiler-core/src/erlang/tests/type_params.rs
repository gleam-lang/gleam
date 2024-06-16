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

#[test]
fn nested_named_args_count_once() {
    assert_erl!(
        "
        pub fn foo() -> Result(a, Result(a, b)) {
            todo
        }
        "
    );
}

#[test]
fn tuple_type_params_count_twice() {
    assert_erl!(
        "
        pub fn foo() -> #(a, Result(a, b)) {
            todo
        }
        "
    );
}

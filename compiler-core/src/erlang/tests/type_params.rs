use crate::assert_erl;

#[test]
fn named_args_count_once() {
    assert_erl!(
        "
        pub fn wibble() -> Result(a, a) {
            todo
        }
        "
    );
}

#[test]
fn nested_named_args_count_once() {
    assert_erl!(
        "
        pub fn wibble() -> Result(a, Result(a, b)) {
            todo
        }
        "
    );
}

#[test]
fn tuple_type_params_count_twice() {
    assert_erl!(
        "
        pub fn wibble() -> #(a, Result(a, b)) {
            todo
        }
        "
    );
}

#[test]
fn custom_type_named_args_count_once() {
    assert_erl!(
        "
        pub type Wibble(a, b) {
            Wibble(a, b)
        }

        pub fn wibble() -> Wibble(a, a) {
            todo
        }
        "
    );
}

#[test]
fn custom_type_nested_named_args_count_once() {
    assert_erl!(
        "
        pub type Wibble(a, b) {
            Wibble(a, b)
        }

        pub fn wibble() -> Wibble(a, Wibble(a, b)) {
            todo
        }
        "
    );
}

#[test]
fn custom_type_tuple_type_params_count_twice() {
    assert_erl!(
        "
        pub type Wibble(a, b) {
            Wibble(a, b)
        }

        pub fn wibble() -> #(a, Wibble(a, b)) {
            todo
        }
        "
    );
}

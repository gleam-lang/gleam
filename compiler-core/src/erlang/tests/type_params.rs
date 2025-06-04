use crate::assert_erl;

#[test]
fn result_type_inferred_count_once() {
    assert_erl!(
        "
        pub fn wibble() {
            let assert Ok(_) = wobble()
        }

        pub type Wobble(a) {
            Wobble
        }

        pub fn wobble() -> Result(a, Wobble(a)) {
            todo
        }
        "
    );
}

#[test]
fn result_type_count_once() {
    assert_erl!(
        "
        pub fn wibble() -> Result(a, a) {
            todo
        }
        "
    );
}

#[test]
fn nested_result_type_count_once() {
    assert_erl!(
        "
        pub fn wibble() -> Result(a, Result(a, b)) {
            todo
        }
        "
    );
}

#[test]
fn custom_type_nested_result_type_count_once() {
    assert_erl!(
        "
        pub type Wibble(a) {
            Oops
        }

        pub fn wibble() -> Result(a, Wibble(a)) {
            todo
        }
        "
    );
}

#[test]
fn tuple_type_params_count_twice() {
    assert_erl!(
        "
        pub fn wibble() -> #(a, b) {
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

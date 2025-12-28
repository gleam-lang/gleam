use crate::assert_erl;

#[test]
fn phantom() {
    assert_erl!("pub type Map(k, v)");
}

#[test]
fn annotated_external_type() {
    assert_erl!(
        r#"
@external(erlang, "gleam_stdlib", "dict")
pub type Dict(key, value)
"#
    );
}

#[test]
fn annotated_external_type_used_in_function() {
    assert_erl!(
        r#"
@external(erlang, "gleam_stdlib", "dict")
pub type Dict(key, value)

@external(erlang, "maps", "get")
pub fn get(dict: Dict(key, value), key: key) -> Result(value, Nil)
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/5127
#[test]
fn unused_opaque_constructor_is_generated_correctly() {
    assert_erl!(
        "
type Wibble {
  Wibble
}

pub opaque type Wobble {
  Wobble(Wibble)
}
"
    );
}

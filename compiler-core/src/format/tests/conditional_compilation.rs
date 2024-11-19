use crate::{assert_format, assert_format_rewrite};

#[test]
fn multiple() {
    assert_format!(
        "type X

@target(erlang)
type Y {
  Y
}

@target(javascript)
type Z {
  Z
}
"
    );
}

#[test]
fn formatter_removes_target_shorthand_erlang() {
    assert_format_rewrite!(
        "@target(erl)
fn wibble() {
  todo
}",
        "@target(erlang)
fn wibble() {
  todo
}
"
    );
}

#[test]
fn formatter_removes_target_shorthand_javascript() {
    assert_format_rewrite!(
        "@target(js)
fn wibble() {
  todo
}",
        "@target(javascript)
fn wibble() {
  todo
}
"
    );
}

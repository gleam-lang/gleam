use crate::assert_format;

#[test]
fn no_body_erlang() {
    assert_format!(
        r#"@external(erlang, "one", "one")
fn one(x: Int) -> Int
"#
    );
}

#[test]
fn no_body_javascript() {
    assert_format!(
        r#"@external(javascript, "one", "one")
fn one(x: Int) -> Int
"#
    );
}

#[test]
fn no_body_body() {
    assert_format!(
        r#"@external(erlang, "two", "two")
@external(javascript, "one", "one")
fn one(x: Int) -> Int
"#
    );
}

#[test]
fn erlang() {
    assert_format!(
        r#"@external(erlang, "one", "one")
fn one(x: Int) -> Int {
  todo
}
"#
    );
}

#[test]
fn javascript() {
    assert_format!(
        r#"@external(javascript, "one", "one")
fn one(x: Int) -> Int {
  todo
}
"#
    );
}

#[test]
fn body() {
    assert_format!(
        r#"@external(erlang, "two", "two")
@external(javascript, "one", "one")
fn one(x: Int) -> Int {
  todo
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2259
#[test]
fn break_external_fn_arguments() {
    assert_format!(
        r#"@external(erlang, "ffi", "improper_list_append")
fn improper_list_append(
  a: item_a,
  b: item_b,
  c: improper_tail,
) -> List(anything)
"#
    );
}

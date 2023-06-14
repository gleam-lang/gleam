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

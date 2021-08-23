use crate::assert_js;

#[test]
fn ok() {
    assert_js!(r#"pub fn main() { Ok(1) }"#);
}

#[test]
fn error() {
    assert_js!(r#"pub fn main() { Error(1) }"#);
}

#[test]
fn ok_fn() {
    assert_js!(r#"pub fn main() { Ok }"#);
}

#[test]
fn error_fn() {
    assert_js!(r#"pub fn main() { Error }"#);
}

#[test]
fn qualified_ok() {
    assert_js!(
        r#"import gleam
pub fn main() { gleam.Ok(1) }"#
    );
}

#[test]
fn qualified_error() {
    assert_js!(
        r#"import gleam
pub fn main() { gleam.Error(1) }"#
    );
}

#[test]
fn qualified_ok_fn() {
    assert_js!(
        r#"import gleam
pub fn main() { gleam.Ok }"#
    );
}

#[test]
fn qualified_error_fn() {
    assert_js!(
        r#"import gleam
pub fn main() { gleam.Error }"#
    );
}

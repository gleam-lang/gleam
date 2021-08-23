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

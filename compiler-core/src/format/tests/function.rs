use crate::assert_format;

#[test]
fn deprecated() {
    assert_format!(
        r#"@deprecated("use something else instead")
pub fn main() -> Nil {
  Nil
}
"#
    );
}

#[test]
fn deprecated_external() {
    assert_format!(
        r#"@deprecated("use something else instead")
@external(erlang, "thing", "main")
pub fn main() -> Nil
"#
    );
}

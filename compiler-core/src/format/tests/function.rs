use crate::assert_format;

#[test]
fn imports() {
    assert_format!(
        r#"@deprecated("use something else instead")
pub fn main() {
  Nil
}
"#
    );
}
